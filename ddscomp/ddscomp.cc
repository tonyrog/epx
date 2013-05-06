// ddscomp.cc
// (C) 2004 Magden LLC. Magnus Feuer
//
// This compiler reads one or more files of various formats and generates
// a pre-compiled cache that can be used by the m1 executable to form
// an instrument animation. Each file will become a single frame within the
// pre-compiled cache.
//
// The cache file format (for now) is:
//  (Machine independent integer formats are for sissies. :-) )
//
// typedef struct {
//    u_int32_t  version;
//    u_int32_t  image_count;
//    u_int32_t  height;
//    u_int32_t  width;
//    u_int32_t  pixel_format;
// } epx_animation_header_t;
//  int imageOffset[image_count]; // An offset table containing the start of each image.
//                                // First image always is offseted at 0. Thus
//                                // imageOffset[0] == 0.
//
//  After the header, there are image_count images of varying sizes.
//  Each bimage has one or more of the following image blocks. Each block
//  describes how the next given number of pixels are to be drawn.
//  A block never wraps onto a new line. At the end of the current
//  line the current block ends, and a new one starts for the next line.
//  If a line in an image is blank, a EPX_ANIM_SKIP block is given
//  with the pixel_count set to mWidth. When the accumulated pixel_count
//  for all blocks processed so far is equal to mWidth, a new line is started.
//
//  Each line has one or or more of sections, each one with the following
//  format:
//
//  EPX_ANIM_SKIP
//
//        Skip the next pix_count pixels on the current line.
//        Activated by Alpha=0.
//
//  EPX_ANIM_RGBA
//  EPX_ANIM_BGRA
//      Paint the next pix_count pixels according to the following pixel data
//      The pixel data, with each bixel being 4 bytes, in RGBA format, will be alpha
//      blended with existing pixels. Total pixel data size is 4*pix_count.
//
//  EPX_ANIM_COPY
//     Paint the next pix_count pixels according to the following pixel data.
//     The pixel data, with each pixel is stored in the target frame buffer's
//     native frame buffer format.
//     Total pixel data size is mBytesPerPixel*pix_count bytes.
//
//  EPX_ANIM_FILL
//     Fill the next pix_count with the pixel value (ARGB) found in the
//     bytes following the header (4 bytes).
//
//  EPX_ANIM_SHADOW
//     [Comment was old]
//
//
//
//
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "lhash.h"
#include "ddsimg.hh"
#include "ddspng.hh"
#include "ddsgif.hh"
// #include "ddsmpg.hh" // Disabled when moved to exosense
extern "C" {
#include <libswscale/swscale.h>
}
#define DEBUG 1
#define DDS_FORMAT_VERSION 0x00000001

int skip_block_count = 0;
int skip_pixels = 0;
int skip_byte_count = 0;

int shadow_block_count = 0;
int shadow_pixels = 0;
int shadow_byte_count = 0;

int copy_block_count = 0;
int copy_pixels = 0;
int copy_byte_count = 0;

int alpha_block_count = 0;
int alpha_pixels = 0;
int alpha_byte_count = 0;

int fill_block_count = 0;
int fill_pixels = 0;
int fill_byte_count = 0;

int ind_block_count = 0;   /* number of indirect blocks */
int ind_pixels = 0;        /* pixels "saved" */
int ind_byte_count = 0;    /* bytes "saved" */

int scale = 0;
float scale_factor = 1.0;

/* I think it should be at lease 8 on most value here ! */
#define BLOCK_THRESHOLD    10
#define SKIP_THRESHOLD     4
#define SHADOW_THRESHOLD   4
#define INDIRECT_THRESHOLD 4
#define COPY_THRESHOLD     4
#define FILL_THRESHOLD     4


void dbo_emit(const char* fmt, ...);

#define DBO(...) do { \
	if (debug_output) dbo_emit(__VA_ARGS__); \
    } while(0)

/* global options */
bool debug_output         = false;
int brightness_adjustment = 0;
int color_tolerance       = 1;
int use_indirect          = 0;  /* generate indirect blocks */
int use_lines             = 0;  /* only use complete ALPHA rows */
int pixelType             = 0;  /* the copy pixel data pixels used */
int alphaBlockType        = 0;  /* BGRA | RGBA, the type of alpha data used */
int bytesPerPixel         = 0;

/* header id:
   ?   unknown
   _   skip
   A   BGRA
   C   copy
   S   shadow
   I   indirect
   a   RGBA
   F   fill
*/
char block_id[] = { '?', '_', 'A', 'C', 'S', 'I', 'a', 'F' };

int dds_compress(int frame, u_int32_t offset,
		 DDSImage* aImage, unsigned char *aDestination);
int dds_compress_row(epx_pixel_t* aRow, DDSImage* aImage, int frame, int y,
		     int aWidth,
		     u_int8_t* target,
		     u_int32_t offset);

int dds_block(epx_pixel_t* aRow, int x, u_int8_t* target, epx_anim_pixels_t* hdr);

inline int min(int a, int b)
{
    return (a < b) ? a : b;
}

inline int max(int a, int b)
{
    return (a < b) ? b : a;
}

void dbo_emit(const char* fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);

    vfprintf(stdout, fmt, ap);
    va_end(ap);
}

int analyze_pixel_data(epx_pixel_t* aRow, int aX, int aWidth, epx_anim_pixels_t* header);


bool tolerated(int aVal1, int aVal2, int aTolerance)
{
    return (aVal1 < aVal2 + aTolerance) && (aVal1 > aVal2 - aTolerance);
}

inline int adjust(int component)
{
    if (brightness_adjustment != 0) {
	component += brightness_adjustment;
	if (component < 0)
	    return 0;
	else if (component > 255)
	    return 255;
    }
    return component;
}

/* add a block and check among previous blocks */

typedef struct _frame_block
{
    dds_lhash_bucket_t bucket;
    u_int32_t offset;          /* file offset from start (after header) */
    u_int32_t npixels;         /* total count of pixels (over nblocks) */
    u_int32_t nblocks;         /* number of linked */
    u_int16_t frame;           /* frame number first found in */
    u_int16_t hits;            /* number of cache hits */
    u_int8_t  type;            /* first indirect block type */
    u_int32_t size;            /* block size */
    epx_anim_pixels_t* hdr;          /* Either ALPHA (line mode) or COPY/SHADOW */
} FrameBlock;

dds_lhash_t* hblock = NULL;


void dump_block_header(epx_anim_pixels_t* b)
{
    u_int8_t* ptr = ((u_int8_t*)b)+sizeof(epx_anim_pixels_t);
    int count = b->count;
    int i = 1;
    switch(b->type) {
    case EPX_ANIM_RGBA:
	printf("\nRGBA:%d\n  ", count);
	while(count--) {
	    printf("%02X%02X%02X%02X,",
		   ptr[0],ptr[1],ptr[2],ptr[3]);
	    if (i >= 8) {
		printf("\n  ");
		i = 0;
	    }
	    i++;
	    ptr += 4;
	}
	break;
    case EPX_ANIM_BGRA:
	printf("\nBGRA:%d\n  ", count);
	while(count--) {
	    printf("%02X%02X%02X%02X,",
		   ptr[0],ptr[1],ptr[2],ptr[3]);
	    if (i >= 8) {
		printf("\n  ");
		i = 0;
	    }
	    i++;
	    ptr += 4;
	}
	break;
    case EPX_ANIM_COPY:
	printf("\nCOPY:%d\n", count);
	break;
    case EPX_ANIM_FILL:
	printf("\nFILL: %d\n  ", count);
	printf("%02X%02X%02X%02X\n",
	       ptr[0],ptr[1],ptr[2],ptr[3]);
	break;

    case EPX_ANIM_SHADOW:
	printf("\nSHADOW:%d\n  ", count);
	while(count--) {
	    printf("%02X,", ptr[0]);
	    if (i >= 24) {
		printf("\n  ");
		i = 0;
	    }
	    i++;
	    ptr++;
	}
	break;
    case EPX_ANIM_SKIP:
	printf("\nSKIP:%d\n", count);
	break;
    }
}


u_int32_t size_block_header(epx_anim_pixels_t* b)
{
    int sz = 0;

    switch(b->type) {
    case EPX_ANIM_BGRA:
    case EPX_ANIM_RGBA:
	sz = 4*b->count;
	break;
    case EPX_ANIM_COPY:
	sz = bytesPerPixel*b->count;
	break;
    case EPX_ANIM_SHADOW:
	sz = b->count;
	break;
    case EPX_ANIM_FILL:
	sz = 4;
	break;
    case EPX_ANIM_SKIP:
	break;
    }
    return sz + sizeof(epx_anim_pixels_t);
}

/* EXACT versions of compare */

inline int memcmp_rgba(const u_int8_t* ptr1, const u_int8_t* ptr2, int n)
{
    return memcmp(ptr1, ptr2, 4*n);
}

inline int memcmp_copy(const u_int8_t* ptr1, const u_int8_t* ptr2, int n)
{
    return memcmp(ptr1, ptr2, bytesPerPixel*n);
}

inline int memcmp_shadow(const u_int8_t* ptr1, const u_int8_t* ptr2, int n)
{
    return memcmp(ptr1, ptr2, n);
}

/* PIXEL version of compare */

inline int px_equal(u_int8_t a, u_int8_t b)
{
    return (a >= b) ?
	((a - b) <= color_tolerance) :
	((b - a) <= color_tolerance);
}

inline int pxcmp_rgba(const u_int8_t* ptr1, const u_int8_t* ptr2, int n)
{
    while(n--) {
	if (!px_equal(ptr1[0], ptr2[0]) ||
	    !px_equal(ptr1[1], ptr2[1]) ||
	    !px_equal(ptr1[2], ptr2[2]) ||
	    !px_equal(ptr1[3], ptr2[3]))
	    return 1;
	ptr1 += 4;
	ptr2 += 4;
    }
    return 0;
}

inline int pxcmp_fill(const u_int8_t* ptr1, const u_int8_t* ptr2, int n)
{
    if (!px_equal(ptr1[0], ptr2[0]) ||
	!px_equal(ptr1[1], ptr2[1]) ||
	!px_equal(ptr1[2], ptr2[2]) ||
	!px_equal(ptr1[3], ptr2[3]))
	return 1;
    return 0;
}

inline int pxcmp_copy(const u_int8_t* ptr1, const u_int8_t* ptr2, int n)
{
    while(n--) {
	switch(bytesPerPixel) {
	case 4: if (!px_equal(ptr1[3], ptr2[3])) return 1;
	case 3: if (!px_equal(ptr1[2], ptr2[2])) return 1;
	case 2: if (!px_equal(ptr1[1], ptr2[1])) return 1;
	case 1: if (!px_equal(ptr1[0], ptr2[0])) return 1;
	default: return 1;
	}
	ptr1 += bytesPerPixel;
	ptr2 += bytesPerPixel;
    }
    return 0;
}

inline int pxcmp_shadow(const u_int8_t* ptr1, const u_int8_t* ptr2, int n)
{
    while(n--) {
	if (!px_equal(ptr1[0], ptr2[0])) return 1;
	ptr1++;
	ptr2++;
    }
    return 0;
}

#define CMP_BGRA    pxcmp_rgba  // this is the same function !!!
#define CMP_RGBA    pxcmp_rgba
#define CMP_COPY    pxcmp_copy
#define CMP_FILL    pxcmp_fill
#define CMP_SHADOW  pxcmp_shadow


int cmp_block_header(epx_anim_pixels_t* b1, epx_anim_pixels_t* b2)
{
    u_int8_t* b1_ptr;
    u_int8_t* b2_ptr;

    if (b1->type != b2->type)
	return b1->type - b2->type;
    if (b1->count != b2->count)
	return b1->count - b2->count;
    b1_ptr = ((u_int8_t*) b1) + sizeof(epx_anim_pixels_t);
    b2_ptr = ((u_int8_t*) b2) + sizeof(epx_anim_pixels_t);
    switch(b1->type) {
    case EPX_ANIM_RGBA:
	return CMP_RGBA(b1_ptr, b2_ptr, b1->count);
    case EPX_ANIM_BGRA:
	return CMP_BGRA(b1_ptr, b2_ptr, b1->count);
    case EPX_ANIM_COPY:
	return CMP_COPY(b1_ptr, b2_ptr,  b1->count);
    case EPX_ANIM_FILL:
	return CMP_FILL(b1_ptr, b2_ptr,  b1->count);
    case EPX_ANIM_SHADOW:
	return CMP_SHADOW(b1_ptr, b2_ptr, b1->count);
    case EPX_ANIM_SKIP:
	return 0;
    }
    return -1;
}

epx_anim_pixels_t* copy_block_header(epx_anim_pixels_t* b)
{
    epx_anim_pixels_t* b_copy = NULL;

    switch(b->type) {
    case EPX_ANIM_BGRA:
    case EPX_ANIM_RGBA:
	b_copy = (epx_anim_pixels_t*) malloc(sizeof(epx_anim_pixels_t) + 4*b->count);
	memcpy(b_copy, b, sizeof(epx_anim_pixels_t) + 4*b->count);

    case EPX_ANIM_COPY:
	b_copy = (epx_anim_pixels_t*) malloc(sizeof(epx_anim_pixels_t) +
				       bytesPerPixel*b->count);
	memcpy(b_copy, b, sizeof(epx_anim_pixels_t) + bytesPerPixel*b->count);
	break;
    case EPX_ANIM_FILL:
	b_copy = (epx_anim_pixels_t*) malloc(sizeof(epx_anim_pixels_t) + 4);
	memcpy(b_copy, b, sizeof(epx_anim_pixels_t) + 4);
	break;
    case EPX_ANIM_SHADOW:
	b_copy = (epx_anim_pixels_t*) malloc(sizeof(epx_anim_pixels_t) + b->count);
	memcpy(b_copy, b, sizeof(epx_anim_pixels_t) + b->count);
	break;
    case EPX_ANIM_SKIP:
	b_copy = (epx_anim_pixels_t*) malloc(sizeof(epx_anim_pixels_t));
	memcpy(b_copy, b, sizeof(epx_anim_pixels_t));
    }
    return b_copy;
}


/* calculate log2 */
int ilog2(unsigned long v)
{
    int n = 0;
    if (v > 0xffff) { n += 16; v >>= 16; };
    if (v > 0xff)   { n += 8;  v >>= 8; };
    if (v > 0xf)    { n += 4;  v >>= 4; };
    if (v > 0x3)    { n += 2;  v >>= 2; };
    if (v > 0x1)    { n += 1;  v >>= 1; };
    return n;
}

/* Adler32 function */
#define BASE 65521L /* largest prime smaller than 65536 */
#define NMAX 5552

/* NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */
hash_value_t  adler32(hash_value_t h,
		      const u_int8_t* buf,
		      unsigned int len,
		      unsigned char mask)
{
    unsigned long s1 = h & 0xffff;
    unsigned long s2 = (h >> 16) & 0xffff;
    int k;

    if (buf == NULL)
	return 1L;

    while (len > 0) {
        k = len < NMAX ? len : NMAX;
        len -= k;
	while (k > 0) {
	    unsigned char ts = *buf++;
            s1 += (ts & mask);  // use non masked bits
	    s2 += s1;
	    k--;
        }
        s1 %= BASE;
        s2 %= BASE;
    }
    return (s2 << 16) | s1;
}

inline hash_value_t adler_alpha(hash_value_t h,
				const u_int8_t* buf,
				unsigned int n)
{
    unsigned char mask =  (color_tolerance <= 0) ? 0xff :
	(0xff << (ilog2(color_tolerance)+2));
    return adler32(h, buf, n*4, mask);
}

inline hash_value_t adler_copy(hash_value_t h,
			       const u_int8_t* buf,
			       unsigned int n)
{
    unsigned char mask =  (color_tolerance <= 0) ? 0xff :
	(0xff << (ilog2(color_tolerance)+2));
    return adler32(h, buf, n*bytesPerPixel, mask);
}

inline hash_value_t adler_fill(hash_value_t h,
			       const u_int8_t* buf,
			       unsigned int n)
{
    unsigned char mask =  (color_tolerance <= 0) ? 0xff :
	(0xff << (ilog2(color_tolerance)+2));
    return adler32(h, buf, 4, mask);
}

inline hash_value_t adler_shadow(hash_value_t h,
				 const u_int8_t* buf,
				 unsigned int n)
{
    unsigned char mask =  (color_tolerance <= 0) ? 0xff :
	(0xff << (ilog2(color_tolerance)+2));
    return adler32(h, buf, n, mask);
}


/* calculate straight sum of all byte values */
hash_value_t  sum32(hash_value_t h,
		    const u_int8_t* buf,
		    unsigned int len)
{
    while(len > 0) {
	h += *buf++;
	len--;
    }
    return h;
}

inline hash_value_t sum_alpha(hash_value_t h,
			      const u_int8_t* buf,
			      unsigned int n)
{
    hash_value_t v = sum32(h, buf, n*4);
    int e = n*4*color_tolerance;         /* error tolerance */
    int mask = (1 << (ilog2(e)+1)) - 1;  /* bits to strip */
    return v & ~mask;
}

inline hash_value_t sum_copy(hash_value_t h,
			     const u_int8_t* buf,
			     unsigned int n)
{
    hash_value_t v = sum32(h, buf, n*bytesPerPixel);
    int e = n*bytesPerPixel*color_tolerance;   /* error tolerance */
    int mask = (1 << (ilog2(e)+1)) - 1;        /* bits to strip */
    return v & ~mask;
}

inline hash_value_t sum_fill(hash_value_t h,
			     const u_int8_t* buf,
			     unsigned int n)
{
    hash_value_t v = sum32(h, buf, 4);
    int e = color_tolerance;               /* error tolerance */
    int mask = (1 << (ilog2(e)+1)) - 1;    /* bits to strip */
    return v & ~mask;
}

inline hash_value_t sum_shadow(hash_value_t h,
			       const u_int8_t* buf,
			       unsigned int n)
{
    hash_value_t v = sum32(h, buf, n);
    int e = n*color_tolerance;                 /* error tolerance */
    int mask = (1 << (ilog2(e)+1)) - 1;        /* bits to strip */
    return v & ~mask;
}


#define HASH_ALPHA  adler_alpha
#define HASH_COPY   adler_copy
#define HASH_SHADOW adler_shadow
#define HASH_FILL   adler_fill

hash_value_t hash_block_header(epx_anim_pixels_t* b)
{
    hash_value_t h = 0;
    u_int8_t* ptr;

    /* offset hash value with adler32 */
    h = adler32(h, (u_int8_t*) &b->type, sizeof(b->type), 0xff);
    h = adler32(h, (u_int8_t*) &b->count, sizeof(b->count), 0xff);

    /* then select method */
    ptr = ((u_int8_t*) b)+sizeof(epx_anim_pixels_t);
    switch(b->type) {
    case EPX_ANIM_BGRA:
    case EPX_ANIM_RGBA:
	h = HASH_ALPHA(h, ptr, b->count);
	break;
    case EPX_ANIM_COPY:
	h = HASH_COPY(h, ptr, b->count);
	break;
    case EPX_ANIM_FILL:
	h = HASH_FILL(h, ptr, b->count);
	break;
    case EPX_ANIM_SHADOW:
	h = HASH_SHADOW(h, ptr, b->count);
	break;
    case EPX_ANIM_SKIP:
	break;
    default:
	fprintf(stderr, "bad block type %d\n", b->type);
	exit(1);
    }
    return h;
}

hash_value_t hash_block(void* key)
{
    FrameBlock* b = (FrameBlock*) key;
    return hash_block_header(b->hdr);
}

int cmp_block(void* arg1, void* arg2)
{
    FrameBlock* a = (FrameBlock*) arg1;
    FrameBlock* b = (FrameBlock*) arg2;
    return cmp_block_header(a->hdr, b->hdr);
}

void release_block(void* a)
{
    free(((FrameBlock*)a)->hdr);
    free(a);
}

void* copy_block(void* arg)
{
    FrameBlock* a = (FrameBlock*) arg;
    FrameBlock* b = (FrameBlock*) malloc(sizeof(FrameBlock));
    *b = *a;
    b->hdr = copy_block_header(a->hdr);
    return b;
}

/* insert block:
 * return 1: if fresh (e.g not found)
 * return 0: if found in cashe
 */
FrameBlock* insert_block(int frame, int y, int offset, epx_anim_pixels_t* hdr)
{
    FrameBlock bt;
    FrameBlock* b;
    /* create template */
    bt.offset = offset;
    bt.npixels = 0;
    bt.nblocks = 0;
    bt.frame   = frame;
    bt.hits    = 0;
    bt.type    = 0;

    bt.size   = size_block_header(hdr);
    bt.hdr    = hdr;

    b = (FrameBlock*) dds_lhash_insert_new(hblock, &bt, &bt);
#ifdef DEBUG
    if (b != NULL) {
	if (debug_output)
	    printf("INSERT:%8p:f=%d,y=%d,hash=%8lx\n",
		   b, frame, y, b->bucket.hvalue);
	if (debug_output && (y == 13)) {
	    dump_block_header(hdr);
	}
    }
#endif
    return b;
}

FrameBlock* find_block(int frame, epx_anim_pixels_t* hdr)
{
    FrameBlock bt;
    FrameBlock* b;

    /* create template */
    bt.frame  = 0;
    bt.offset = 0;
    bt.hdr    = hdr;

    if ((b = (FrameBlock*) dds_lhash_lookup(hblock, &bt)) != NULL) {
	b->hits++;
#ifdef DEBUG
	if (debug_output)
	    printf("HIT:%8p:f=%d,hits=%d from frame %d\n",
		   b, b->frame,b->hits, frame);
#endif
    }
    return b;
}

enum PixelFormat pixelType2PixelFormat(int aPixelType)
{
    switch(aPixelType) {
    case EPX_FORMAT_RGB:      return PIX_FMT_RGB24;
    case EPX_FORMAT_BGR:      return PIX_FMT_BGR24;
    case EPX_FORMAT_RGBA:     return PIX_FMT_RGBA;
    case EPX_FORMAT_ARGB:     return PIX_FMT_ARGB;
    case EPX_FORMAT_BGRA:     return PIX_FMT_BGRA;
    case EPX_FORMAT_ABGR:     return PIX_FMT_ABGR;
    case EPX_FORMAT_565_LE:
#if BYTE_ORDER == BIG_ENDIAN
	return PIX_FMT_BGR565;
#else
	return PIX_FMT_RGB565;
#endif
    case EPX_FORMAT_565_BE:
#if BYTE_ORDER == BIG_ENDIAN
	return PIX_FMT_RGB565;
#else
	return PIX_FMT_BGR565;
#endif
    case EPX_FORMAT_A1R5G5B5:
    case EPX_FORMAT_X1R5G5B5:
#if BYTE_ORDER == BIG_ENDIAN
	return PIX_FMT_RGB555;
#else
	return PIX_FMT_BGR555;
#endif
	/* no R5G5B5A1 version in ffmpeg */
    default: return PIX_FMT_NONE;
    }
}


epx_pixmap_t* scale_pixmap(epx_pixmap_t* aSrcImage, int aWidth, int aHeight, int aAlpha)
{
    epx_pixmap_t*  dstImage;
    PixelFormat       srcFmt = pixelType2PixelFormat(aSrcImage->pixel_format);
    PixelFormat       dstFmt = srcFmt;
    u_int8_t* srcData[4];
    int       srcLineSize[4];
    u_int8_t* dstData[4];
    int       dstLineSize[4];
    SwsContext* ctx1 = NULL;

    dstImage = epx_pixmap_create(aWidth, aHeight, aSrcImage->pixel_format);
    if (!dstImage)
	return NULL;

    // scale srcImage into dstImage
    ctx1 = sws_getCachedContext(NULL,
				aSrcImage->width,
				aSrcImage->height,
				srcFmt,
				dstImage->width,
				dstImage->height,
				dstFmt,
				SWS_BICUBIC, NULL, NULL, NULL);
    srcData[0]     = aSrcImage->data;
    srcLineSize[0] = aSrcImage->bytes_per_row;

    dstData[0]     = dstImage->data;
    dstLineSize[0] = dstImage->bytes_per_row;

    sws_scale(ctx1, srcData, srcLineSize, 0,
	      aSrcImage->height, dstData, dstLineSize);
    sws_freeContext(ctx1);


    if (aAlpha) {
	epx_pixmap_t*  srcAlphaImage = NULL;
	epx_pixmap_t*  dstAlphaImage = NULL;
	SwsContext* ctx2 = NULL;

	// If alpha scaling is needed then produce an alpha image
	ctx2 = sws_getCachedContext(NULL,
				    aSrcImage->width,
				    aSrcImage->height,
				    PIX_FMT_GRAY8,
				    dstImage->width,
				    dstImage->height,
				    PIX_FMT_GRAY8,
				    SWS_BICUBIC, NULL, NULL, NULL);

	if ((srcAlphaImage = epx_pixmap_create(aSrcImage->width,
					   aSrcImage->height,
					   EPX_FORMAT_ALPHA)) == 0) {
	    sws_freeContext(ctx2);
	    goto error;
	}
	if ((dstAlphaImage = epx_pixmap_create(dstImage->width,
					   dstImage->height,
					   EPX_FORMAT_ALPHA)) == 0) {
	    sws_freeContext(ctx2);
	    epx_pixmap_destroy(srcAlphaImage);
	    goto error;
	}
	// copy the alpha channel (treat it as PIX_FMT_GRAY8)
	epx_pixmap_copy_to(aSrcImage, srcAlphaImage);
	srcData[0]     = srcAlphaImage->data;
	srcLineSize[0] = srcAlphaImage->bytes_per_row;
	dstData[0]     = dstAlphaImage->data;
	dstLineSize[0] = dstAlphaImage->bytes_per_row;

	sws_scale(ctx2, srcData, srcLineSize, 0,
		  srcAlphaImage->height, dstData, dstLineSize);
	sws_freeContext(ctx2);

	// add the alpha channel to dstImage!
	epx_pixmap_copy_area(dstAlphaImage, dstImage,
			     0, 0, 0, 0,
			     dstAlphaImage->width,
			     dstAlphaImage->height,
			     EPX_FLAG_SUM);
	epx_pixmap_destroy(srcAlphaImage);
	epx_pixmap_destroy(dstAlphaImage);

    }
    return dstImage;
error:
    if (dstImage)
	epx_pixmap_destroy(dstImage);
    return NULL;
}


int scale_img(DDSImage* aImg, int aWidth, int aHeight)
{
    int i;

    for (i = 0; i < aImg->frames(); i++) {
	epx_pixmap_t* pixmap   = aImg->getPixmap(i);
	int      useAlpha = aImg->useAlpha(i);
	pixmap = scale_pixmap(pixmap, aWidth, aHeight, useAlpha);
	if (!pixmap)
	    return -1;
	aImg->setPixmap(i, pixmap);
    }
    return 0;
}

/* Verify blocks within one frame */
int dds_verify(int frame, u_int8_t *target, int size)
{
    int count = 0;

    while(size > 0) {
	epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) target;

	count++;
	target += sizeof(epx_anim_pixels_t);
	size -= sizeof(epx_anim_pixels_t);

	switch(hdr->type) {
	case EPX_ANIM_SKIP:
	    if (hdr->count == 0) {
		printf("EPX_ANIM_SKIP: Count=0\n");
		return 0;
	    }
	    break;
	case EPX_ANIM_SHADOW:
	    target += hdr->count;
	    size   -= hdr->count;
	    break;
	case EPX_ANIM_COPY:
	    target += hdr->count*bytesPerPixel;
	    size   -= hdr->count*bytesPerPixel;
	    break;
	case EPX_ANIM_FILL:
	    target += 4;
	    size   -= 4;
	    break;

	case EPX_ANIM_BGRA:
	case EPX_ANIM_RGBA:
	    target += hdr->count*4;
	    size   -= hdr->count*4;
	    break;
	case EPX_ANIM_INDIRECT:
	    if (use_indirect)
		/* FIXME: Verify idirect block when enabled */
		break;
	    else {
		printf("BAD mType=%d\n", hdr->type);
		return 0;
	    }
	    break;
	default:
	    printf("BAD mType=%d\n", hdr->type);
		return 0;
	}
    }
    if (size < 0) {
	printf("SIZE mismatch %d\n", size);
	return 0;
    }
    return count;
}

int dds_compress(int frame, u_int32_t offset,
		 DDSImage* aImage, u_int8_t *aDestination)
{
    u_int8_t* base   = aDestination;
    u_int8_t* target = aDestination;
    int aWidth       = aImage->width();
    int aHeight      = aImage->height();
    epx_pixel_t aRow[aWidth];
    int y;


    for (y = 0; y < aHeight; y++) {
	int rsize;
	int i;
	DBO("LINE %04d [%d]: ", y, aWidth);
	for (i = 0; i < aWidth; i++) {
	    aRow[i].r = adjust(aImage->red(frame,i,y));
	    aRow[i].g = adjust(aImage->green(frame,i,y));
	    aRow[i].b = adjust(aImage->blue(frame,i,y));
	    aRow[i].a = aImage->alpha(frame,i,y);
	}
	rsize = dds_compress_row(aRow, aImage, frame, y, aWidth, target, offset);
	DBO("\n");
	target += rsize;
	offset += rsize;
    }
    return (target - base);
}

/* compress one row of data, return size inserted */
int dds_compress_row(epx_pixel_t* aRow, DDSImage* aImage, int frame, int y,
		     int aWidth,
		     u_int8_t* target,
		     u_int32_t offset)
{
    u_int8_t* base = target;
    FrameBlock* b = NULL;
    epx_anim_pixels_t *hdr;
    int x = 0;
    int bsize;
    int indirect = use_indirect;

    if (indirect && use_lines) {
	hdr = (epx_anim_pixels_t *) target;
	hdr->type   = alphaBlockType;
	hdr->count  = aWidth;

	/* build alpha block */
	bsize = dds_block(aRow, x, target+sizeof(epx_anim_pixels_t), hdr);

	if ((b=find_block(frame, hdr)) != NULL) {
	    hdr->type   = EPX_ANIM_INDIRECT;
	    hdr->itype  = b->type;
	    hdr->nblocks = b->nblocks;
	    hdr->count = b->offset;  // NOTE! offset

	    ind_block_count++;
	    ind_pixels += b->npixels;
	    ind_byte_count += sizeof(epx_anim_pixels_t);
	    DBO("I%c[%d] ", block_id[b->type], b->npixels);
	    return sizeof(epx_anim_pixels_t);
	}
	/* insert complete ALPHA row */
	b = insert_block(frame, y, offset, hdr);
	/* continue and write "real" blocks */
	indirect = 0;
    }

    while(x < aWidth) {
	u_int8_t* save_target = target;
	u_int32_t save_offset = offset;

	hdr = (epx_anim_pixels_t *) target;

	analyze_pixel_data(aRow, x, aWidth, hdr);

	target += sizeof(epx_anim_pixels_t);
	offset += sizeof(epx_anim_pixels_t);
	bsize = dds_block(aRow, x, target, hdr);
	offset += bsize;
	target += bsize;
	x += hdr->count;

	if (indirect && (hdr->count > INDIRECT_THRESHOLD) &&
	    ((b=find_block(frame, hdr)) != NULL)) {
	    int i = hdr->count;

	    DBO("I%c[%d] ", block_id[hdr->type], hdr->count);

	    ind_block_count++;
	    ind_pixels += i;
	    ind_byte_count += sizeof(epx_anim_pixels_t);

	    switch(hdr->type) {
	    case EPX_ANIM_BGRA:
	    case EPX_ANIM_RGBA:
		hdr->itype = hdr->type;
		hdr->type = EPX_ANIM_INDIRECT;
		hdr->nblocks = 1;
		hdr->count = b->offset;
		break;
	    case EPX_ANIM_COPY:
		hdr->itype = hdr->type;
		hdr->type = EPX_ANIM_INDIRECT;
		hdr->nblocks = 1;
		hdr->count = b->offset;
		break;
	    case EPX_ANIM_SHADOW:
		hdr->itype = hdr->type;
		hdr->type = EPX_ANIM_INDIRECT;
		hdr->nblocks = 1;
		hdr->count = b->offset;
		break;
	    case EPX_ANIM_FILL:
		hdr->itype = hdr->type;
		hdr->type = EPX_ANIM_INDIRECT;
		hdr->nblocks = 1;
		hdr->count = b->offset;
		break;
	    case EPX_ANIM_SKIP:
		fprintf(stderr, "EPX_ANIM_SKIP: found in cache\n");
		exit(1);
	    }
	    /* update target and offset */
	    target = save_target + sizeof(epx_anim_pixels_t);
	    offset = save_offset + sizeof(epx_anim_pixels_t);
	}
	else {
	    if (!indirect && (b != NULL)) {
		if (b->type == 0)
		    b->type = hdr->type;
		b->npixels += hdr->count;
		b->nblocks++;
	    }

	    if (indirect && (hdr->type != EPX_ANIM_SKIP))
		b = insert_block(frame, y, save_offset, hdr);
	    /* update statitics */
	    DBO("%c[%d] ", block_id[hdr->type], hdr->count);
	    switch(hdr->type) {
	    case  EPX_ANIM_BGRA:
	    case  EPX_ANIM_RGBA:
		alpha_block_count++;
		alpha_pixels += hdr->count;
		alpha_byte_count += (hdr->count*4 +
				     sizeof(epx_anim_pixels_t));
		break;
	    case EPX_ANIM_COPY:
		copy_block_count++;
		copy_pixels += hdr->count;
		copy_byte_count += (hdr->count*bytesPerPixel +
				    sizeof(epx_anim_pixels_t));
		break;
	    case EPX_ANIM_SHADOW:
		shadow_block_count++;
		shadow_pixels +=  hdr->count;
		shadow_byte_count += (sizeof(epx_anim_pixels_t) +
				      hdr->count);
		break;
	    case EPX_ANIM_FILL:
		fill_block_count++;
		fill_pixels += 1;
		fill_byte_count += (4 + sizeof(epx_anim_pixels_t));
		break;
	    case EPX_ANIM_SKIP:
		skip_block_count++;
		skip_pixels += hdr->count;
		skip_byte_count += sizeof(epx_anim_pixels_t);
		break;
	    }
	}
    }
    return (target - base);
}

/* Build block data and return size added to the block */
int dds_block(epx_pixel_t* aRow, int x, u_int8_t* target, epx_anim_pixels_t* hdr)
{
    int i;

    switch(hdr->type) {
    case  EPX_ANIM_RGBA:
	i = hdr->count;
	while(i--) {
	    *target++ = aRow[x].r;
	    *target++ = aRow[x].g;
	    *target++ = aRow[x].b;
	    *target++ = aRow[x].a;
	    x++;
	}
	return hdr->count*4;
    case  EPX_ANIM_BGRA:
	i = hdr->count;
	while(i--) {
	    *target++ = aRow[x].b;
	    *target++ = aRow[x].g;
	    *target++ = aRow[x].r;
	    *target++ = aRow[x].a;
	    x++;
	}
	return hdr->count*4;
    case EPX_ANIM_FILL:
	*target++ = aRow[x].a;
	*target++ = aRow[x].r;
	*target++ = aRow[x].g;
	*target++ = aRow[x].b;
	return 4;

    case EPX_ANIM_COPY:
	i = hdr->count;
	while(i--) { // Write copy data in PixelType order
	    switch(pixelType) {
	    case EPX_FORMAT_RGB:
		*target++ = aRow[x].r;
		*target++ = aRow[x].g;
		*target++ = aRow[x].b;
		break;
	    case EPX_FORMAT_ARGB:
		*target++ = aRow[x].a;
		*target++ = aRow[x].r;
		*target++ = aRow[x].g;
		*target++ = aRow[x].b;
		break;
	    case EPX_FORMAT_RGBA:
		*target++ = aRow[x].r;
		*target++ = aRow[x].g;
		*target++ = aRow[x].b;
		*target++ = aRow[x].a;
		break;
	    case EPX_FORMAT_BGR:
		*target++ = aRow[x].b;
		*target++ = aRow[x].g;
		*target++ = aRow[x].r;
		break;
	    case EPX_FORMAT_BGRA:
		*target++ = aRow[x].b;
		*target++ = aRow[x].g;
		*target++ = aRow[x].r;
		*target++ = aRow[x].a;
		break;
	    case EPX_FORMAT_ABGR:
		*target++ = aRow[x].a;
		*target++ = aRow[x].b;
		*target++ = aRow[x].g;
		*target++ = aRow[x].r;
		break;
	    }
	    x++;
	}
	return hdr->count*bytesPerPixel;

    case EPX_ANIM_SHADOW:
	i = hdr->count;
	while(i--) {
	    *target++ = aRow[x].a;
	    x++;
	}
	return hdr->count;

    case EPX_ANIM_SKIP:
	return 0;

    default:
	return -1;
    }
}

#define is_transparent(a)   ((a) <= color_tolerance)

#define is_opaque(a)        (((a)+color_tolerance) >= 255)

#define is_alpha(a)         !(is_transparent(a) || is_opaque(a))

#define is_shadow(r,g,b,a)  (((r) < color_tolerance) &&		\
			     ((g) < color_tolerance) &&		\
			     ((b) < color_tolerance) &&		\
			     is_alpha(a))
// Compare pixels
#define is_equal(p0,p1)			   \
    (px_equal(p0.r,p1.r) && px_equal(p0.g,p1.g) && \
     px_equal(p0.b,p1.b) && px_equal(p0.a,p1.a))


//
// Count number of consecutive fully transparent pixels (alpha = 0)
//
int count_transparent_pixels(epx_pixel_t* aRow,int aX,int aWidth)
{
    int count = 0;

    while(aX < aWidth) {
	if (!is_transparent(aRow[aX].a))
	    return count;
	count++;
	aX++;
    }
    return count;
}

//
// Count number of "fill" pixels, same pixels consequtive values
//
int count_equal_pixels(epx_pixel_t* aRow, epx_pixel_t p0, int aX, int aWidth)
{
    int count = 0;
    while(aX < aWidth) {
	epx_pixel_t p1 = aRow[aX];
	if (!is_equal(p0, p1))
	    return count;
	count++;
	aX++;
    }
    return count;
}

int count_fill_pixels(epx_pixel_t* aRow,int aX,int aWidth)
{
    if (aX < aWidth)
	return 1 + count_equal_pixels(aRow,aRow[aX],aX+1,aWidth);
    return 0;
}

//
// Count number of fully opaque pixels (alpha channel == 255)
//
int count_opaque_pixels(epx_pixel_t* aRow,int aX,int aWidth)
{
    int count = 0;

    while(aX < aWidth) {
	if (!is_opaque(aRow[aX].a))
	    return count;
	if (count_fill_pixels(aRow,aX,min(aX+FILL_THRESHOLD,aWidth)) >=
	    FILL_THRESHOLD)
	    return count;
	count++;
	aX++;
    }
    return count;
}

//
// Count number of "shadow" pixels (r=0,g=0,b=0 and (0 < a < 255))
//
int count_shadow_pixels(epx_pixel_t* aRow, int aX, int aWidth)
{
    int count = 0;

    while(aX < aWidth) {
	epx_pixel_t p = aRow[aX];
	if (!is_shadow(p.r,p.g,p.b,p.a))
	    return count;
	count++;
	aX++;
    }
    return count;
}



int analyze_pixel_data(epx_pixel_t* aRow, int aX, int aWidth, epx_anim_pixels_t* header)
{
    int x;
    int i;
    int en;
    int ex;
    int extend;
    int skip_count = 0;
    int shadow_count = 0;
    int copy_count = 0;
    int fill_count = 0;
    int alpha_count = 0;

    // If there are less than BLOCK_THRESHOLD pixels left to the end of
    // the line, just install the data.

    if ((aWidth - aX) < BLOCK_THRESHOLD) {
	header->type       = alphaBlockType;
	header->count = aWidth - aX;
	return 0;
    }

    if ((skip_count = count_transparent_pixels(aRow,aX,aWidth)) >
	SKIP_THRESHOLD) {
	header->type = EPX_ANIM_SKIP;
	header->count = skip_count;
	return 0;
    }

    if ((shadow_count = count_shadow_pixels(aRow,aX,aWidth)) >
	SHADOW_THRESHOLD) {
	header->type = EPX_ANIM_SHADOW;
	header->count = shadow_count;
	return 0;
    }

    if ((fill_count = count_fill_pixels(aRow,aX,aWidth)) >
	FILL_THRESHOLD) {
	header->type = EPX_ANIM_FILL;
	header->count = fill_count;
	return 0;
    }

    // Check for fully opaque pixels, but only if we have not found
    // any shadow of transaparent pixels
    if ((skip_count == 0) && (shadow_count == 0)) {
	if ((copy_count = count_opaque_pixels(aRow,aX,aWidth)) >
	    COPY_THRESHOLD) {
	    header->type = EPX_ANIM_COPY;
	    header->count = copy_count;
	    return 0;
	}
    }

    // Check number of alpha pixels to be emited.
    // take max of skip_count/shadow_count and copy_count
    i = skip_count;
    if (shadow_count > i) i = shadow_count;
    if (copy_count > i)   i = copy_count;
    x = aX + i;       // skip past known data
    alpha_count = i;  // number of known pixels
    while(x < aWidth) {
	epx_pixel_t p = aRow[x];

	if (is_transparent(p.a)||is_shadow(p.r,p.g,p.b,p.a)||is_opaque(p.a))
	    break;
	if (count_equal_pixels(aRow,p, x+1, min(x+1+FILL_THRESHOLD,aWidth)) >=
	    FILL_THRESHOLD)
	    break;
	alpha_count++;
	x++;
    }

    ex = x;
    extend = 0;
    /* check for small skip & shadow blocks to extend alpha block */
    do {
	en = count_shadow_pixels(aRow,ex,aWidth);
	if (en > SHADOW_THRESHOLD)
	    break;
	if (!en) {
	    en = count_transparent_pixels(aRow,ex,aWidth);
	    if (en > SKIP_THRESHOLD)
		break;
	    if (!en) {
		en = count_fill_pixels(aRow,ex,aWidth);
		if (en > FILL_THRESHOLD)
		    break;
		if (!en) {
		    en = count_opaque_pixels(aRow,ex,aWidth);
		    if (en > COPY_THRESHOLD)
			break;
		}
	    }
	}
	extend += en;
	ex += en;
    } while(en);

    alpha_count += extend;
    x           += extend;
    header->type = alphaBlockType;
    if ((aWidth-x) < BLOCK_THRESHOLD) {
	extend += (aWidth-x);
	header->count = alpha_count+(aWidth-x);
    }
    else
	header->count = alpha_count;
    return extend;
}

void usage(char *aProgName)
{
  printf("Usage: %s -C cfmt -c outfile -f infile-pattern [opts]\n", aProgName);
  puts(" -C rgb|rgba|argb     Specifies both pixel size and format.\n");
  puts("    gbr|gbra|agbr\n");
  puts(" -b 0-255  Default 0. Specifies neighboring pixel color and alpha variations allowed while still classifying");
  puts("                      two pixels as identical. Not used right now");
  puts(" -B bright Default 0. Specifies a brightness adjustment (positive or negative) to be applied");
  puts("                      to each image.");
  puts(" -c outfile           Specifies the name of the DDS file to generate.\n");
  puts(" -f infile-pattern    Specifies the printf format string to use when scanning for input files.");
  puts("                      For example a format_string of bitmap%.4d.png will look for files");
  puts("                      bitmap0001.png, bitmap0002.png, bitmap0003.png etc.");
  puts("                      input files will be loaded until the next-in-sequence file cannot be opened.");
  puts("                      Input files are often generated by flash MX's export movie function.");
  puts(" -s step              Image step value. May be used to get event numbered file patterns etc.");
  puts(" -v                   Verify generated dds file before writing it.");
  puts(" -i                   Generate indirect blocks when possible.");
  puts(" -l                   only process complete image lines.");
  puts(" -D                   Print out a shitload of information on how the frames are compiled.");
  puts(" -S <factor>          Scale images with 0.01 <= factor <= 100.0");
  exit(1);
}

#include <sys/types.h>


int main(int argc, char *argv[])
{
    int out;
    int ind;
    char buf[1024];
    char obuf[1024];
    int height = 0, width = 0;
    unsigned char * image_data = new unsigned char[10240000];
    int image_size;
    char format_string[512];
    char cache_name[512];
    int stepping = 1;
    char c;
    int image_count = 0;
    int start_image_ind = 0;
    int image_ind = 0;
    u_int32_t offset = 0;
    epx_animation_header_t cache_res;
    extern char *optarg;
    char *dummy;
    format_string[0]=0;
    cache_name[0]=0;
    char* dds_name = 0;
    int des;
    int tot_byte_count;
    int tot_pixels;
    int tot_block_count;
    int verify = false;
    const char* alpha_block_type = "????";

    pixelType = -1;

    printf("DDS compiler version %s. (C) 2006 Magden LLC - GPLv2\n", VERSION);
    //
    // Do the option thing
    //
    while((c = getopt(argc, argv, "B:b:f:c:s:C:S:Dilv")) != -1) {
	switch (c) {
	case 'D':
	    debug_output = true;
	    break;

	case 'v':
	    verify = true;
	    break;

	case 'C':
	    if (strcmp(optarg, "rgba") == 0) {
		pixelType = EPX_FORMAT_RGBA;
		alphaBlockType = EPX_ANIM_RGBA;
	    }
	    else if (strcmp(optarg, "argb") == 0) {
		pixelType = EPX_FORMAT_ARGB;
		alphaBlockType = EPX_ANIM_RGBA;
	    }
	    else if (strcmp(optarg, "rgb") == 0) {
		pixelType = EPX_FORMAT_RGB;
		alphaBlockType = EPX_ANIM_RGBA;
	    }
	    else if (strcmp(optarg, "bgra") == 0) {
		pixelType = EPX_FORMAT_BGRA;
		alphaBlockType = EPX_ANIM_BGRA;
	    }
	    else if (strcmp(optarg, "abgr") == 0) {
		pixelType = EPX_FORMAT_ABGR;
		alphaBlockType = EPX_ANIM_BGRA;
	    }
	    else if (strcmp(optarg, "bgr") == 0) {
		pixelType = EPX_FORMAT_BGR;
		alphaBlockType = EPX_ANIM_BGRA;
	    }
	    break;
	case 'S':
	    // scale factor
	    scale = 1;
	    scale_factor = atof(optarg);
	    if ((scale_factor < 0.01) || (scale_factor > 100.0))
		usage(argv[0]);
	    break;

	case 'i':
	    use_indirect = 1;
	    break;

	case 'l':
	    use_lines = 1;
	    break;

	case 'B':
	    brightness_adjustment = atoi(optarg);
	    break;

	case 'b':
	    color_tolerance = atoi(optarg);
	    break;

	case 'f':
	    strcpy(format_string, optarg);
	    break;

	case 'c':
	    strcpy(cache_name, optarg);
	    if ((dds_name = strrchr(cache_name, '/')) == NULL)
		dds_name = cache_name;
	    else
		dds_name++;
	    break;

	case 's':
	    stepping = atoi(optarg);
	    break;

	case '?':
	    usage(argv[0]);
	}
    }
    if (!cache_name[0] || !format_string[0] || (pixelType==-1)) {
	usage(argv[0]);
    }

    bytesPerPixel = EPX_PIXEL_SIZE(pixelType);

    if (use_indirect) {
	hblock = dds_lhash_new((char*) "idirect", 2, hash_block, cmp_block,
			       release_block, copy_block);
    }

    //
    // First check what the index number is of the
    // first image. 0 or 1.
    //
    sprintf(buf, format_string, 0);
    des = open(buf, O_RDONLY);
    if (des == -1) {
	sprintf(buf, format_string, 1);
	if ((des = open(buf, O_RDONLY)) == -1) {
	    printf("No image matches pattern [%s] and starts with index 0 or 1. Giving up.\n",
		   format_string);
	    exit(255);
	}
	start_image_ind = 1;
    } else
	start_image_ind = 0;

    close(des);

    //
    // First check how many files we can open.
    //
    image_count = 0;
    image_ind = start_image_ind;
    obuf[0] = 0;
    while(1) {
	char* suffix;
	sprintf(buf, format_string, image_ind);
	//
	// Avoid opening a single file specified without %d an infinite number of times.
	//
	if (!strcmp(buf, obuf))
	    break;
	strcpy(obuf, buf);
	des = open(buf, O_RDONLY);

	//
	// Allow for first image to start with nr 0 or 1.
	//
	if (des == -1)
	    break;

	close(des);
	suffix = strrchr(buf, '.');
	if (suffix) {
	    if (strcmp(suffix, ".gif") == 0) {
		DDSGif* img = new DDSGif();
		img->load(buf, 0, 0);
		image_count += img->frames();
		delete img;
	    }
	    // else if ((strcmp(suffix, ".mpg") == 0)||
	    // 	     (strcmp(suffix, ".mpeg") == 0)) {
	    // 	DDSMpg* img = new DDSMpg();
	    // 	img->load(buf, 0, 0);
	    // 	image_count += img->frames();
	    // 	delete img;
	    // }
	    else if (strcmp(suffix, ".png") == 0) {
		image_count++;
	    }
	    else
		image_count++;
	}
	else
	    image_count++;
	image_ind++;
    }


    printf("%s: %d images available for compiling.\n",
	   dds_name, image_count);

    //
    // Write out a "blank" header.
    //
    cache_res.version = DDS_FORMAT_VERSION;
    cache_res.image_count = 0;
    cache_res.height = 0;
    cache_res.width = 0;
    cache_res.pixel_format = 0;
    out = creat(cache_name, 0666);

    if (out == -1) {
	perror(cache_name);
	exit(-1);
    }

    if (write(out, (char *) &cache_res, sizeof(cache_res)) != sizeof(cache_res)) {
	perror("write(header)");
	exit(-1);
    }


    //
    // Write out a blank offset table.
    //
    dummy = (char *) malloc(sizeof(int) * (image_count/stepping));
    if (write(out, dummy, sizeof(int) * (image_count/stepping) != sizeof(int) * (image_count/stepping))) {
	perror("write(offset)");
	exit(-1);
    }

    ind = 0;
    image_ind = start_image_ind;
    offset = 0;

    //
    // Step through each image file and add it to the cache.
    //
    while(ind * stepping < image_count) {
	DDSImage* img;
	char* suffix;
	int frame;

	sprintf(buf, format_string, image_ind * stepping);

	suffix = strrchr(buf, '.');

	if (suffix) {
	    if (strcmp(suffix, ".gif") == 0)
		img = new DDSGif();
	    // else if ((strcmp(suffix, ".mpg") == 0) ||
	    // 	     (strcmp(suffix, ".mpeg") == 0))
	    // 	img = new DDSMpg();
	    else if (strcmp(suffix, ".png") == 0)
		img = new DDSPng();
	    else
		img = new DDSPng();
	}
	else
	    img = new DDSPng();

	//
	// Ensure that we can load the file.
	//
	if (img->load(buf, 0, 0) < 0) {
	    break;
	}
	//
	// Validate that the height and width are correct.
	// First image will determine the h/w for the rest
	// of the loaded files.
	//
	if (scale && (scale_factor != 1.0)) {
	    int h = int(img->height()*scale_factor);
	    int w = int(img->width()*scale_factor);
	    if ((h != 0)  && (w != 0))
		scale_img(img, w, h);
	}

	if (((height!= 0) && (width != 0)) &&
	    ((img->height() != height) || (img->width() != width))) {
	    printf("\nImage has a height [%d] width[%d] that differs from earlier image(s) height[%d] and width[%d]\n",
		   img->height(), img->width(), height, width);
	    close(out);
	    unlink(cache_name);
	    exit(-1);
	}

	height = img->height();
	width  = img->width();

	if (color_tolerance < 1)
	    color_tolerance = 1;

	printf("%s:%d compressing\n", dds_name, ind);
	if (debug_output && use_indirect) {
	    printf("HASH INFO DUMP\n");
	    dds_lhash_info(hblock);
	}

	for (frame = 0; frame < img->frames(); frame++) {
	    image_size = dds_compress(frame, offset, img, image_data);

	    if (verify) {
		if (!dds_verify(frame, image_data, image_size)) {
		    exit(1);
		}
	    }

	    if (debug_output)
		printf("%s:%d compressed %d\n", dds_name, ind, image_size);

	    //
	    // Install the offset entry.
	    //
	    lseek(out, sizeof(epx_animation_header_t) + (sizeof(int)*ind), SEEK_SET);
	    if (write(out, (char *) &offset, sizeof(int)) != sizeof(int)) {
		perror("write(offset)");
		exit(-1);
	    }
	    lseek(out, 0, SEEK_END);
	    //
	    // Dump the image data.
	    //
	    if (write(out, image_data, image_size) != image_size) {
		perror("write(image_data)");
		exit(-1);
	    }
	    if (debug_output)
		printf("Added [%s] to [%s]. Offset[%d] Size[%d] OffsetTableEntry[%d] Height[%d] Width[%d]\n",
		       buf,
		       cache_name,
		       offset,
		       image_size,
		       ind,
		       height,
		       width
		    );
	    offset += image_size;
	    ind++;
	    image_ind++;
	}
	delete img;
    }

    //
    // We'll add the cache resource at the beginning of the file.
    //
    lseek(out, 0, SEEK_SET);
    cache_res.version = DDS_FORMAT_VERSION; // Version of the format used in this DDS file.
    cache_res.height = height;
    cache_res.width = width;
    cache_res.image_count = image_count;
    cache_res.pixel_format = pixelType;
    if (write(out, (char *) &cache_res, sizeof(cache_res)) != sizeof(cache_res)) {
	perror("write(header2)");
	exit(-1);
    }
    close(out);

    //
    // Dump some stats
    //
    tot_block_count = (ind_block_count +
		       skip_block_count +
		       shadow_block_count +
		       copy_block_count +
		       fill_block_count +
		       alpha_block_count);
    tot_pixels = (skip_pixels +
		  ind_pixels +
		  shadow_pixels +
		  copy_pixels +
		  fill_pixels +
		  alpha_pixels);
    tot_byte_count = (ind_byte_count +
		      skip_byte_count +
		      shadow_byte_count +
		      copy_byte_count +
		      fill_byte_count +
		      alpha_byte_count);

    printf("TYPE    BLOCKS   %%      PIXELS     %%      BYTES      %% \n");
    printf("Skip    %.7d %.3d     %.9d %.3d     %.9d %.3d\n",
	   skip_block_count,
	   (int) (100.0 * (float) skip_block_count / (float) tot_block_count),
	   skip_pixels,
	   (int) (100.0 * (float) skip_pixels / (float) tot_pixels),
	   skip_byte_count,
	   (int) (100.0 * (float) skip_byte_count / (float) tot_byte_count)
	);

    printf("Shadow  %.7d %.3d     %.9d %.3d     %.9d %.3d\n",
	   shadow_block_count,
	   (int) (100.0 * (float) shadow_block_count / (float) tot_block_count),
	   shadow_pixels,
	   (int) (100.0 * (float) shadow_pixels / (float) tot_pixels),
	   shadow_byte_count,
	   (int) (100.0 * (float) shadow_byte_count / (float) tot_byte_count)
	);

    printf("Copy    %.7d %.3d     %.9d %.3d     %.9d %.3d\n",
	   copy_block_count,
	   (int) (100.0 * (float) copy_block_count / (float) tot_block_count),
	   copy_pixels,
	   (int) (100.0 * (float) copy_pixels / (float) tot_pixels),
	   copy_byte_count,
	   (int) (100.0 * (float) copy_byte_count / (float) tot_byte_count)
	);

    printf("Fill    %.7d %.3d     %.9d %.3d     %.9d %.3d\n",
	   fill_block_count,
	   (int) (100.0 * (float) fill_block_count / (float) tot_block_count),
	   fill_pixels,
	   (int) (100.0 * (float) fill_pixels / (float) tot_pixels),
	   fill_byte_count,
	   (int) (100.0 * (float) fill_byte_count / (float) tot_byte_count)
	);

    if (alphaBlockType==EPX_ANIM_RGBA)
	alpha_block_type = "RGBA";
    else if (alphaBlockType==EPX_ANIM_BGRA)
	alpha_block_type = "BGRA";


    printf("%s    %.7d %.3d     %.9d %.3d     %.9d %.3d\n",
	   alpha_block_type,
	   alpha_block_count,
	   (int) (100.0 * (float) alpha_block_count / (float) tot_block_count),
	   alpha_pixels,
	   (int) (100.0 * (float) alpha_pixels / (float) tot_pixels),
	   alpha_byte_count,
	   (int) (100.0 * (float) alpha_byte_count / (float) tot_byte_count)
	);

    printf("Indir   %.7d %.3d     %.9d %.3d     %.9d %.3d\n",
	   ind_block_count,
	   (int) (100.0 * (float) ind_block_count / (float)
		  (tot_block_count - skip_block_count)),
	   ind_pixels,
	   (int) (100.0 * (float) ind_pixels / (float)
		  (tot_pixels - skip_pixels)),
	   ind_byte_count,
	   (int) (100.0 * (float) ind_byte_count / (float) tot_byte_count)
	);

    printf("TOTAL   %.7d         %.9d         %.9d\n",
	   tot_block_count,
	   tot_pixels,
	   tot_byte_count);

    puts("\nDone.");
    exit(0);
}
