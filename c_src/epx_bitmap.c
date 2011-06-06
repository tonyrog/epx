/*
 * EPIC Bitmap functions
 *
 */
#include <math.h>
#include "epic.h"

#define BYTE_OFFSET(ofs)        ((unsigned) (ofs) >> 3)
#define BIT_OFFSET(ofs)         ((ofs) & 7)
#define MAKE_MASK(n)            ((1 << (n))-1)
#define MASK_BITS(src,dst,mask) (((src) & (mask)) | ((dst) & ~(mask)))

#define ROW_BYTES(w) BYTE_OFFSET((w)+7)

/* address of bit */
#define EBIT_ADDR(bmp,x,y)						\
    ((bmp)->data+((y)*(bmp)->bytesPerRow)+BYTE_OFFSET(x))

/* bit number */
#define EBIT_NUMBER(x) (7-BIT_OFFSET(x))
/* bit mask */
#define EBIT_MASK(x)   (1 << EBIT_NUMBER(x))


static int ellipse(EBitmap* bmp, int x0, int y0, 
		   unsigned int a, unsigned int b, 
		   int fill, u_int8_t bit);

/*
 * The basic bit copy operation. Copies n bits from the source buffer to
 * the destination buffer. Depending on the directions, it can reverse the
 * copied bits (bytes actually)
 */

static void 
copy_bits(u_int8_t* src,		/* Base pointer to source. */
	  size_t soffs,		/* Bit offset for source relative to src. */
	  int sdir,		/* Direction: 1 (forward) or -1 (backward). */
	  u_int8_t* dst,		/* Base pointer to destination. */
	  size_t doffs,		/* Bit offset for destination relative to dst. */
	  int ddir,		/* Direction: 1 (forward) or -1 (backward). */
	  size_t n)		/* Number of bits to copy. */
{
    u_int32_t lmask;
    u_int32_t rmask;
    u_int32_t count;
    u_int32_t deoffs;

    if (n == 0) {
	return;
    }

    src += sdir*BYTE_OFFSET(soffs);
    dst += ddir*BYTE_OFFSET(doffs);
    soffs = BIT_OFFSET(soffs);
    doffs = BIT_OFFSET(doffs);
    deoffs = BIT_OFFSET(doffs+n);
    lmask = (doffs) ? MAKE_MASK(8-doffs) : 0;
    rmask = (deoffs) ? (MAKE_MASK(deoffs)<<(8-deoffs)) : 0;

    /*
     * Take care of the case that all bits are in the same byte.
     */

    if (doffs+n < 8) {		/* All bits are in the same byte */
	lmask = (lmask & rmask) ? (lmask & rmask) : (lmask | rmask);

	if (soffs == doffs) {
	    *dst = MASK_BITS(*src,*dst,lmask);
	} else if (soffs > doffs) {
	    u_int32_t bits = (*src << (soffs-doffs));
	    if (soffs+n > 8) {
		src += sdir;
		bits |= (*src >> (8-(soffs-doffs)));
	    }
	    *dst = MASK_BITS(bits,*dst,lmask);
	} else {
	    *dst = MASK_BITS((*src >> (doffs-soffs)),*dst,lmask);
	}
	return;			/* We are done! */
    }

    /*
     * At this point, we know that the bits are in 2 or more bytes.
     */

    count = ((lmask) ? (n - (8 - doffs)) : n) >> 3;

    if (soffs == doffs) {
	/*
	 * The bits are aligned in the same way. We can just copy the bytes
	 * (except for the first and last bytes). Note that the directions
	 * might be different, so we can't just use memcpy().
	 */

	if (lmask) {
	    *dst = MASK_BITS(*src, *dst, lmask);
	    dst += ddir;
	    src += sdir;
	}

	while (count--) {
	    *dst = *src;
	    dst += ddir;
	    src += sdir;
	}

	if (rmask) {
	    *dst = MASK_BITS(*src,*dst,rmask);
	}
    } else {
	u_int32_t bits;
	u_int32_t bits1;
	u_int32_t rshift;
	u_int32_t lshift;

	/*
	 * The tricky case. The bits must be shifted into position.
	 */
	
	if (soffs > doffs) {
	    lshift = (soffs - doffs);
	    rshift = 8 - lshift;
	    bits = *src;
	    if (soffs + n > 8) {
		src += sdir;
	    }
	} else {
	    rshift = (doffs - soffs);
	    lshift = 8 - rshift;
	    bits = 0;
	}
	    
	if (lmask) {
	    bits1 = bits << lshift;
	    bits = *src;
	    src += sdir;
	    bits1 |= (bits >> rshift);
	    *dst = MASK_BITS(bits1,*dst,lmask);
	    dst += ddir;
	}

	while (count--) {
	    bits1 = bits << lshift;
	    bits = *src;
	    src += sdir;
	    *dst = bits1 | (bits >> rshift);
	    dst += ddir;
	}
	
	if (rmask) {
	    bits1 = bits << lshift;
	    if ((rmask << rshift) & 0xff) {
		bits = *src;
		bits1 |= (bits >> rshift);
	    }
	    *dst = MASK_BITS(bits1,*dst,rmask);
	}
    }
}


static void 
set_bits(u_int8_t pattern,      /* 8bit bit pattern */
	 u_int8_t* dst,		/* Base pointer to destination. */
	 size_t doffs,		/* Bit offset for destination relative to dst. */
	 int ddir,		/* Direction: 1 (forward) or -1 (backward). */
	 size_t n)		/* Number of bits to set. */
{
    u_int32_t lmask;
    u_int32_t rmask;
    u_int32_t count;
    u_int32_t deoffs;

    if (n == 0) {
	return;
    }

    dst += ddir*BYTE_OFFSET(doffs);
    doffs = BIT_OFFSET(doffs);
    deoffs = BIT_OFFSET(doffs+n);
    lmask = (doffs) ? MAKE_MASK(8-doffs) : 0;
    rmask = (deoffs) ? (MAKE_MASK(deoffs)<<(8-deoffs)) : 0;

    /*
     * Take care of the case that all bits are in the same byte.
     */

    if (doffs+n < 8) {		/* All bits are in the same byte */
	lmask = (lmask & rmask) ? (lmask & rmask) : (lmask | rmask);

	*dst = MASK_BITS(pattern,*dst,lmask);
	return;
    }

    /*
     * At this point, we know that the bits are in 2 or more bytes.
     */

    count = ((lmask) ? (n - (8 - doffs)) : n) >> 3;

    if (lmask) {
	*dst = MASK_BITS(pattern, *dst, lmask);
	dst += ddir;
    }

    while (count--) {
	*dst = pattern;
	dst += ddir;
    }

    if (rmask) {
	*dst = MASK_BITS(pattern,*dst,rmask);
    }
}

static inline int clip_range(int a, int low, int high)
{
    if (a < low) return low;
    if (a > high) return high;
    return a;
}

static inline int in_range(int a, int low, int high)
{
    if (a < low) return 0;
    if (a > high) return 0;
    return 1;
}

    
EBitmap* EBitmapCreate(unsigned int width, 
		       unsigned int height)
{
    u_int8_t* data;
    EBitmap* bmp;
    unsigned int bytesPerRow = ROW_BYTES(width);

    if ((data = (u_int8_t*) malloc(bytesPerRow*height)) == NULL)
	return NULL;
    if ((bmp = (EBitmap*) malloc(sizeof(EBitmap))) == NULL) {
	free(data);
	return NULL;
    }
    EOBJECT_INIT(bmp, EBITMAP_TYPE);    
    bmp->on_heap = 1;
    bmp->refc = 1;

    ERectSet(&bmp->clip, 0, 0, width, height);
    bmp->width = width;
    bmp->bytesPerRow = bytesPerRow;
    bmp->height = height;
    bmp->d  = 1;
    bmp->sz = bytesPerRow*height;  /* total number of bytes */
    bmp->data = data;
    return bmp;
}

void EBITMAP_TYPE_RELEASE(void *arg)
{
    EBitmap* bmp = (EBitmap*) arg;

    EDBGFMT("EBITMAP_TYPE_RELEASE: %p", arg);
    if (bmp->data != NULL) {
	free(bmp->data);
	bmp->data = NULL;
    }
    if (bmp->on_heap)
	free(bmp);
}

/* REQUIRE bit to be exactly on bit (e.g either 1 or 0) */
static inline void ebitmap_put_abit(u_int8_t* ptr, int x, u_int8_t bit)
{
    unsigned int n = EBIT_NUMBER(x);
    u_int8_t mask = (1<<n);
    *ptr = (*ptr & ~mask) | (bit << n);
}

/* write a bit at (x,y) */
void EBitmapPutBit(EBitmap* bmp, int x, int y, u_int8_t bit)
{
    u_int8_t* dst;
    /* First check clip */
    if (!EPointXYInRect(x,y,&bmp->clip))
	return;
    dst = EBIT_ADDR(bmp,x,y);
    if ((dst < bmp->data) || (dst >= bmp->data+bmp->sz))
	return;
    ebitmap_put_abit(dst, x, (bit&1));
}

/* put_bits:
 *   copy bits  R(0,0,width,height)
 *   into destination R(x_dst,y_dst,width,height)
 *   bits in source must be byte aligned for each row!
 */

void EBitmapPutBits(EBitmap* dst, int x_dst, int y_dst,
		    unsigned int width, unsigned int height,
		    void* data, unsigned int len)
{
    u_int8_t* src_ptr;
    int       src_offs;
    u_int8_t* src_end;
    unsigned int src_wb = ROW_BYTES(width);
    u_int8_t* dst_ptr;
    int       dst_offs;
    int x1, y1;
    int x2, y2;
    int x3, y3;
    int x4, y4;
    int rx1, ry1, rx2, ry2;
    int dx, dy, dh, dw;

    /* setup source */
    x1 = 0;
    y1 = 0;
    x2 = width-1;
    y2 = height-1;

    /* clip destination with updates */
    rx1 = dst->clip.xy.x; ry1 = dst->clip.xy.y;
    rx2 = rx1+dst->clip.wh.width-1; ry2 = ry1+dst->clip.wh.height-1;
    x3 = clip_range(x_dst, rx1, rx2);
    y3 = clip_range(y_dst, ry1, ry2);
    x4 = clip_range(x_dst+width-1, rx1, rx2);
    y4 = clip_range(y_dst+height-1, ry1, ry2);

    /* clip against destination */
    x3 = clip_range(x3, 0, dst->width-1);
    y3 = clip_range(y3, 0, dst->height-1);
    x4 = clip_range(x4, 0, dst->width-1);
    y4 = clip_range(y4, 0, dst->height-1);

    /* calculate relative change in x, y, w, h */
    dx = x3 - x_dst;
    dy = y3 - y_dst;
    dw = (x4-x3+1)-width;
    dh = (y4-y3+1)-height;

    /* update the the source again to reflect the changes */
    x1 += dx;
    y1 += dy;
    x2 += dw;
    y2 += dh;
    
    /* now start moving bits from (x1,y1,x2,y2) -> (x3,y3) */
    src_ptr   = ((u_int8_t*)data)+(y1*src_wb) + BYTE_OFFSET(x1);
    src_offs  = BIT_OFFSET(x1);
    src_end   = ((u_int8_t*)data) + len;
    dst_ptr   = EBIT_ADDR(dst,x3,y3);
    dst_offs  = BIT_OFFSET(x3);

    height = (y2-y1)+1;
    width  = (x2-x1)+1;
    while(width && height-- && (src_ptr+src_wb <= src_end)) {
	copy_bits(src_ptr, src_offs, 1, dst_ptr, dst_offs, 1, width);
	src_ptr += src_wb;
	dst_ptr += dst->bytesPerRow;
    }
}


/* read a pixel */
u_int8_t EBitmapGetBit(EBitmap* bmp, int x, int y)
{
    u_int8_t* dst;
    u_int8_t  mask;
    if (!EPointXYInRect(x,y,&bmp->clip))
	return 0;
    dst = EBIT_ADDR(bmp,x,y);
    if ((dst < bmp->data) || (dst >= bmp->data+bmp->sz))
	return 0;
    mask = EBIT_MASK(x);
    return (*dst & mask) != 0;
}

/* Fill Bitmap with byte pattern */
void EBitmapFill(EBitmap* bmp, u_int8_t pat)
{
    memset(bmp->data, pat, bmp->sz);
}

/* fill pixel area */
static inline void fill_area(u_int8_t* dst, int dst_width,
			     int width, int height, u_int8_t pat)
{
    int dst_offs = 0;  /* FIXME set argument */

    while(height--) {
	set_bits(pat, dst, dst_offs, 1, width);
	dst += dst_width;
    }
}

/* FIXME make this buffer thread safe */
static unsigned int bit_buffer_size = 0;
static u_int8_t* bit_buffer = NULL;

static u_int8_t* bit_buffer_alloc(unsigned int n)
{
    if (bit_buffer_size  < n) {
	bit_buffer = (u_int8_t*) realloc(bit_buffer, ROW_BYTES(n));
	bit_buffer_size = n;
    }
    return bit_buffer;
}

/* copy pixel area */
static inline void copy_area(u_int8_t* src, int src_width, 
			     u_int8_t* dst, int dst_width,
			     int width, int height)
{
    int n = width;

    if (dst < src) {
	while(height--) {
	    memmove(dst, src, n);
	    src += src_width;
	    dst += dst_width;
	}
    }
    else {
	src += (src_width*height);
	dst += (dst_width*height);
	
	while(height--) {
	    src -= src_width;
	    dst -= dst_width;
	    memmove(dst, src, n);
	}
    }
}

/* copy area and shift lines left or right */
static inline void shift_area(u_int8_t* src, int src_width,
			      u_int8_t* dst, int dst_width,
			      unsigned int width, unsigned int height, 
			      int amount)
{
    int n = width;

    if (amount > 0) {
	src += amount;
	n -= amount;
    }
    else {
	dst += (-amount);
	n   -= (-amount);
    }
    while(height--) {
	memmove(dst, src, n);
	src += src_width;
	dst += dst_width;
    }
}


/* copy area and shift lines left or right */
static inline void rotate_area(u_int8_t* src, int src_width,
			       u_int8_t* dst, int dst_width,
			       unsigned int width, unsigned int height, 
			       int amount)
{
    int a = (amount < 0) ? -amount : amount;
    int n = width;
    int is_inline = (src == dst);
    u_int8_t* src_from;
    u_int8_t* dst_to;

    if (amount == 0)
	return;
    else if (amount > 0) {
	src_from = src;
	src += a;
	n -= a;
	dst_to = dst + n;
    }
    else {
	n  -= a;
	src_from = src+n;
	dst_to = dst;
	dst += a;
    }

    if (is_inline) {
	u_int8_t* save = bit_buffer_alloc(a);

	n *= 1;
	a *= 1;

	while(height--) {
	    memcpy(save, src_from, a);
	    memmove(dst, src, n);
	    memcpy(dst_to, save, a);
	    src += src_width;
	    src_from += src_width;
	    dst += dst_width;
	    dst_to += dst_width;
	}
    }
    else {  /* not inline */
	n *= 1;
	a *= 1;

	while(height--) {
	    memcpy(dst, src, n);
	    memcpy(dst_to, src_from, a);
	    src += src_width;
	    src_from += src_width;
	    dst += dst_width;
	    dst_to += dst_width;
	}
    }
}

/*
 * Copy pixmap data from src to dst, ignore clip region
 * if pixmap size is the same just memcpy
 * otherwise calculate the min area and copy that
 */
void EBitmapCopy(EBitmap* src, EBitmap* dst)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    copy_area(src->data, src->width, dst->data, dst->width, w, h);
}


void ebitmap_scroll_left(EBitmap* src, EBitmap* dst,
		      int rotate, unsigned int amount, u_int8_t fill)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	rotate_area(src->data, src->width, dst->data, dst->width, w, h, a);
    else {
	shift_area(src->data, src->width, dst->data, dst->width, w, h, a);
	fill_area(dst->data+(w-amount), dst->width, amount, h, fill);
    }
}

void ebitmap_scroll_right(EBitmap* src, EBitmap* dst, 
		       int rotate, unsigned int amount, u_int8_t fill)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	rotate_area(src->data, src->width, dst->data, dst->width, w, h, -a);
    else {
	shift_area(src->data, src->width, dst->data, dst->width, w, h, -a);
	fill_area(dst->data, dst->bytesPerRow, amount, h, fill);
    }
}

void ebitmap_scroll_up(EBitmap* src, EBitmap* dst, 
		    int rotate, unsigned int amount, u_int8_t fill)
{
    if ((amount >= src->height) && !rotate)
	EBitmapFill(dst, fill);
    else {
	u_int8_t* dst_ptr;
	u_int8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EBIT_ADDR(src,0,amount);
	dst_ptr = EBIT_ADDR(dst,0,0);

	if (rotate) {
	    if (src == dst) {
		u_int8_t* save = bit_buffer_alloc(amount*src->width);
		u_int8_t* dst_save = EBIT_ADDR(dst,0,0);

		copy_area(dst_save, dst->width, save, dst->width, dst->width, amount);
		copy_area(src_ptr, src->width, dst_ptr, dst->width, w, h);
		dst_ptr = EBIT_ADDR(dst,0,h);
		copy_area(save, dst->width, dst_ptr, dst->width, w, amount);
	    }
	    else {
		copy_area(src_ptr, src->width, dst_ptr, dst->width, w, h);
		dst_ptr = EBIT_ADDR(dst,0,h);
		src_ptr = EBIT_ADDR(src,0,0);
		copy_area(src_ptr, src->width, dst_ptr, dst->width, w, amount);
	    }
	}
	else {
	    copy_area(src_ptr, src->width, dst_ptr, dst->width, w, h);
	    dst_ptr = EBIT_ADDR(dst,0,h);
	    fill_area(dst_ptr, dst->bytesPerRow, dst->width, amount, fill);
	}
    }
}

void ebitmap_scroll_down(EBitmap* src, EBitmap* dst, 
		      int rotate, unsigned int amount, u_int8_t fill)
{
    if ((amount >= src->height) && !rotate)
	EBitmapFill(dst, fill);
    else {
	u_int8_t* dst_ptr;
	u_int8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EBIT_ADDR(src,0,0);
	dst_ptr = EBIT_ADDR(dst,0,amount);

	if (rotate) {
	    if (src == dst) {
		u_int8_t* save = bit_buffer_alloc(amount*src->width);
		u_int8_t* dst_save = EBIT_ADDR(dst,0,h);

		copy_area(dst_save, dst->width, save, dst->width, dst->width, amount);
		copy_area(src_ptr, src->width, dst_ptr, dst->width, w, h);
		dst_ptr = EBIT_ADDR(dst,0,0);
		copy_area(save, dst->width, dst_ptr, dst->width, w, amount);
	    }
	    else {
		copy_area(src_ptr, src->width, dst_ptr, dst->width, w, h);
		src_ptr = EBIT_ADDR(src,0,h);
		dst_ptr = EBIT_ADDR(dst,0,0);
		copy_area(src_ptr, src->width, dst_ptr, dst->width, w, amount);
	    }
	}
	else {
	    copy_area(src_ptr, src->width, dst_ptr, dst->width, w, h);
	    dst_ptr = EBIT_ADDR(dst,0,0);
	    fill_area(dst_ptr, dst->bytesPerRow, dst->width, amount, fill);
	}
    }
}

/* scroll pixmap up/dow  left/right rotate/fill */
void ebitmap_scroll(EBitmap* src, EBitmap* dst, 
		    int horizontal, int vertical, 
		    int rotate, u_int8_t fill)
{
    if (vertical>0)
	ebitmap_scroll_up(src, dst, rotate, vertical, fill);
    else if (vertical < 0)
	ebitmap_scroll_down(src, dst, rotate, -vertical, fill);
    if (horizontal>0)
	ebitmap_scroll_right(src, dst, rotate, horizontal, fill);
    else if (horizontal < 0)
	ebitmap_scroll_left(src, dst, rotate, -horizontal, fill);
}


/* Draw rectangle (x,y,w,h) with pixel p */
int ebitmap_draw_rectangle(EBitmap* bmp,
			   int x0, int y0,
			   unsigned int width, 
			   unsigned int height, u_int8_t bit)
{
    int x1 = x0+width-1;
    int y1 = y0+height-1;
    
    ebitmap_draw_line(bmp,x0,y0,x1,y0,EPIC_LINE_STYLE_NFIRST,bit);
    ebitmap_draw_line(bmp,x1,y0,x1,y1,EPIC_LINE_STYLE_NFIRST,bit);
    ebitmap_draw_line(bmp,x1,y1,x0,y1,EPIC_LINE_STYLE_NFIRST,bit);
    ebitmap_draw_line(bmp,x0,y1,x0,y0,EPIC_LINE_STYLE_NFIRST,bit);
    return 0;
}

/* Fill rectangle (x,y,w,h) with pixel p */
int ebitmap_fill_rectangle(EBitmap* bmp,
			   int x, int y,
			   unsigned int width, 
			   unsigned int height, u_int8_t pat)
{
    u_int8_t* ptr;
    int x1, y1;
    int x2, y2;
    int rx1, ry1, rx2, ry2;

    /* clip against clip rect */
    rx1 = bmp->clip.xy.x; ry1 = bmp->clip.xy.y;
    rx2 = rx1+bmp->clip.wh.width-1; ry2 = ry1+bmp->clip.wh.height-1;
    x1 = clip_range(x, rx1, rx2);
    y1 = clip_range(y, ry1, ry2);
    x2 = clip_range(x+width-1, rx1, rx2);
    y2 = clip_range(y+height-1, ry1, ry2);

    /* clip physical limits */
    x1 = clip_range(x1, 0, bmp->width-1);
    y1 = clip_range(y1, 0, bmp->height-1);
    x2 = clip_range(x2, 0, bmp->width-1);
    y2 = clip_range(y2, 0, bmp->height-1);

    ptr = EBIT_ADDR(bmp,x1,y1);
    width  = (x2-x1)+1;
    height = (y2-y1)+1;

    fill_area(ptr, bmp->bytesPerRow, width, height, pat);
    return 0;
}

/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 */
int EBitmapCopyArea(EBitmap* src, EBitmap* dst,
		      int x_src, int y_src, int x_dst, int y_dst,
		      unsigned int width, unsigned int height)
{
    u_int8_t* src_ptr;
    u_int8_t* src_ptr1;
    u_int8_t* src_ptr2;
    u_int8_t* dst_ptr;
    u_int8_t* dst_ptr1;
    u_int8_t* dst_ptr2;
    int x1, y1;
    int x2, y2;
    int x3, y3;
    int x4, y4;
    int rx1, ry1, rx2, ry2;
    int dx, dy, dh, dw;

    /* clip source */
    rx1 = src->clip.xy.x; ry1 = src->clip.xy.y;
    rx2 = rx1+src->clip.wh.width-1; ry2 = ry1+src->clip.wh.height-1;
    x1 = clip_range(x_src, rx1, rx2);
    y1 = clip_range(y_src, ry1, ry2);
    x2 = clip_range(x_src+width-1, rx1, rx2);
    y2 = clip_range(y_src+height-1, ry1, ry2);

    /* clip physical limits */
    x1 = clip_range(x1, 0, src->width-1);
    y1 = clip_range(y1, 0, src->height-1);
    x2 = clip_range(x2, 0, src->width-1);
    y2 = clip_range(y2, 0, src->height-1);

    /* calculate relative change in x, y, w, h */
    dx = x1 - x_src;
    dy = y1 - y_src;
    dw = (x2-x1+1)-width;
    dh = (y2-y1+1)-height;    

    /* clip destination with updates */
    rx1 = dst->clip.xy.x; ry1 = dst->clip.xy.y;
    rx2 = rx1+dst->clip.wh.width-1; ry2 = ry1+dst->clip.wh.height-1;
    x_dst += dx;
    y_dst += dy;
    x3 = clip_range(x_dst, rx1, rx2);
    y3 = clip_range(y_dst, ry1, ry2);
    x4 = clip_range(x_dst+(width+dw)-1, rx1, rx2);
    y4 = clip_range(y_dst+(height+dh)-1, ry1, ry2);

    /* clip against destination */
    x3 = clip_range(x3, 0, dst->width-1);
    y3 = clip_range(y3, 0, dst->height-1);
    x4 = clip_range(x4, 0, dst->width-1);
    y4 = clip_range(y4, 0, dst->height-1);

    /* calculate relative change in x, y, w, h */
    dx = x3 - x_dst;
    dy = y3 - y_dst;
    dw = (x4-x3+1)-(width+dw);
    dh = (y4-y3+1)-(height+dh);

    /* update the the source again to reflect the changes */
    x1 += dx;
    y1 += dy;
    x2 += dw;
    y2 += dh;
    
    /* now start moving pixels from (x1,y1,x2,y2) -> (x3,y3) */
    src_ptr1 = EBIT_ADDR(src,x1,y1);
    dst_ptr1 = EBIT_ADDR(dst,x3,y3);

    src_ptr2 = src_ptr1;
    dst_ptr2 = dst_ptr1;
    width  = (x2-x1)+1;
    height = (y2-y1)+1;

    while(height--) {
	src_ptr = src_ptr2;
	dst_ptr = dst_ptr2;
	x_src = width;
	while(x_src--) {
	    *dst_ptr = *src_ptr; /* FIXME */
	    dst_ptr++;
	    src_ptr++;
	}
	src_ptr2 += src->width;
	dst_ptr2 += dst->width;
    }
    return 0;
}


/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 */
int EBitmapDraw(EBitmap* src, EPixmap* dst,
		int x_src, int y_src, int x_dst, int y_dst,
		unsigned int width, unsigned int height,
		EPixel_t fg, EPixel_t bg)
{
    u_int8_t* src_ptr;
    u_int8_t* src_ptr1;
    u_int8_t* src_ptr2;
    u_int8_t* dst_ptr;
    u_int8_t* dst_ptr1;
    u_int8_t* dst_ptr2;
    int x1, y1;
    int x2, y2;
    int x3, y3;
    int x4, y4;
    int rx1, ry1, rx2, ry2;
    int dx, dy, dh, dw;

    /* clip source */
    rx1 = src->clip.xy.x; ry1 = src->clip.xy.y;
    rx2 = rx1+src->clip.wh.width-1; ry2 = ry1+src->clip.wh.height-1;
    x1 = clip_range(x_src, rx1, rx2);
    y1 = clip_range(y_src, ry1, ry2);
    x2 = clip_range(x_src+width-1, rx1, rx2);
    y2 = clip_range(y_src+height-1, ry1, ry2);

    /* clip physical limits */
    x1 = clip_range(x1, 0, src->width-1);
    y1 = clip_range(y1, 0, src->height-1);
    x2 = clip_range(x2, 0, src->width-1);
    y2 = clip_range(y2, 0, src->height-1);

    /* calculate relative change in x, y, w, h */
    dx = x1 - x_src;
    dy = y1 - y_src;
    dw = (x2-x1+1)-width;
    dh = (y2-y1+1)-height;    

    /* clip destination with updates */
    rx1 = dst->clip.xy.x; ry1 = dst->clip.xy.y;
    rx2 = rx1+dst->clip.wh.width-1; ry2 = ry1+dst->clip.wh.height-1;
    x_dst += dx;
    y_dst += dy;
    x3 = clip_range(x_dst, rx1, rx2);
    y3 = clip_range(y_dst, ry1, ry2);
    x4 = clip_range(x_dst+(width+dw)-1, rx1, rx2);
    y4 = clip_range(y_dst+(height+dh)-1, ry1, ry2);

    /* clip against destination */
    x3 = clip_range(x3, 0, dst->width-1);
    y3 = clip_range(y3, 0, dst->height-1);
    x4 = clip_range(x4, 0, dst->width-1);
    y4 = clip_range(y4, 0, dst->height-1);

    /* calculate relative change in x, y, w, h */
    dx = x3 - x_dst;
    dy = y3 - y_dst;
    dw = (x4-x3+1)-(width+dw);
    dh = (y4-y3+1)-(height+dh);

    /* update the the source again to reflect the changes */
    x1 += dx;
    y1 += dy;
    x2 += dw;
    y2 += dh;
    
    /* now start moving pixels from (x1,y1,x2,y2) -> (x3,y3) */
    src_ptr1 = EBIT_ADDR(src,x1,y1);
    dst_ptr1 = EPIXEL_ADDR(dst,x3,y3);

    src_ptr2 = src_ptr1;
    dst_ptr2 = dst_ptr1;
    width  = (x2-x1)+1;
    height = (y2-y1)+1;

    while(width && height--) {
	int x = x1;
	u_int8_t mask = EBIT_MASK(x);
	u_int8_t src_bits;

	src_ptr  = src_ptr2;
	dst_ptr  = dst_ptr2;
	x_src    = width;
	src_bits = *src_ptr++;
	while(x_src--) {
	    EPixel_t s = (src_bits & mask) ? fg : bg;
	    EPixel_t d = EPixelUnpack(dst->pixelType, dst_ptr);

	    d = EPixelBlend(s.a, s, d);
	    EPixelPack(dst->pixelType, d, dst_ptr);
	    dst_ptr += dst->bytesPerPixel;
	    x++;
	    mask >>= 1;
	    if (x_src && BIT_OFFSET(x) == 0) {
		mask = 0x80;
		src_bits = *src_ptr++;
	    }
	}
	src_ptr2 += src->bytesPerRow;
	dst_ptr2 += dst->bytesPerRow;
    }
    return 0;
}

/*
 * draw_line:
 *
 *  FIXME: draw line width > 1
 *
 */
int ebitmap_draw_line(EBitmap* bmp,
		      int x0, int y0, 
		      int x1, int y1, int skip,
		      u_int8_t bit)
{
    (void) skip;
    int dy = y1 - y0;
    int dx = x1 - x0;
    int sx, sy, syw;
    int x2, x3, y2, y3; /* used for clipping */
    int xt;
    u_int8_t* ptr;

    x2 = bmp->clip.xy.x;
    y2 = bmp->clip.xy.y;
    x3 = x2 + (bmp->clip.wh.width-1);
    y3 = y2 + (bmp->clip.wh.height-1);

    x2 = clip_range(x2, 0, bmp->width-1);
    y2 = clip_range(y2, 0, bmp->height-1);
    x3 = clip_range(x3, 0, bmp->width-1);
    y3 = clip_range(y3, 0, bmp->height-1);

    if (dy < 0) { dy=-dy; sy=-1; syw=-bmp->bytesPerRow; } else { sy=1;syw=bmp->bytesPerRow; }
    if (dx < 0) { dx=-dx; sx=-1; } else { sx=1; }
    dy <<= 1;
    dx <<= 1;

    ptr = EBIT_ADDR(bmp,x0,y0);
    xt  = BYTE_OFFSET(x0);       /* save the byte number */
    bit = bit&1;                 /* make sure ONE bit */

    if (in_range(x0, x2, x3) && in_range(y0, y2, y3))
	ebitmap_put_abit(ptr, x0, bit);

    if (dx > dy) {
	int f = dy - (dx >> 1);
	int xtt;
	while (x0 != x1) {
	    if (f >= 0) {
		ptr += syw;
		y0 += sy;
		f -= dx;
	    }
	    x0 += sx;
	    if ((xtt=BYTE_OFFSET(x0)) != xt) {
		ptr += sx;
		xt = xtt;
	    }
	    f += dy;
	    if (in_range(x0, x2, x3) && in_range(y0, y2, y3))
		ebitmap_put_abit(ptr, x0, bit);
	}
    }
    else {
	int f = dx - (dy >> 1);
	int xtt;
	while (y0 != y1) {
	    if (f >= 0) {
		x0 += sx;
		if ((xtt=BYTE_OFFSET(x0)) != xt) {
		    ptr += sx;
		    xt = xtt;
		}
		f -= dy;
	    }
	    ptr += syw;
	    y0 += sy;
	    f += dx;
	    if (in_range(x0, x2, x3) && in_range(y0, y2, y3))
		ebitmap_put_abit(ptr,x0,bit);
	}
    }
    return 0;
}

int ebitmap_draw_ellipse(EBitmap* bmp, int x0, int y0,
			 unsigned int a, unsigned int b, u_int8_t bit)
{
    return ellipse(bmp, x0, y0, a, b, 0, bit);
}

int ebitmap_fill_ellipse(EBitmap* bmp, int x0, int y0,
			 unsigned int a, unsigned int b, u_int8_t bit)
{
    return ellipse(bmp, x0, y0, a, b, 1, bit);
}

static int ellipse(EBitmap* bmp, int x0, int y0, 
	     unsigned int a, unsigned int b, int fill, u_int8_t bit)
{
    int x  = a;
    int y  = 0;
    unsigned long a2 = a*a;
    unsigned long b2 = b*b;
    long pya = a2;
    long pxb = (2*a-1)*b2;
    long f = 0;
    long fx, fxy, fy;
    unsigned long ax, axy, ay;

    while(x >= 0) {
	if (fill) {
	    if ((x == 0) && (y == 0))
		EBitmapPutBit(bmp, x0, y0, bit);
	    else if (x == 0) {
		EBitmapPutBit(bmp,x0,y0+y,bit);
		EBitmapPutBit(bmp,x0,y0-y,bit);
	    }
	    else if (y == 0)
		ebitmap_draw_line(bmp,x0-x,y0,x0+x,y0,EPIC_LINE_STYLE_NFIRST,bit);
	    else {
		ebitmap_draw_line(bmp,x0-x,y0+y,x0+x,y0+y,EPIC_LINE_STYLE_NFIRST,bit);
		ebitmap_draw_line(bmp,x0-x,y0-y,x0+x,y0-y,EPIC_LINE_STYLE_NFIRST,bit);
	    }
	}
	else {
	    if ((x == 0) && (y == 0))
		EBitmapPutBit(bmp, x0, y0, bit);
	    else if (x == 0) {
		EBitmapPutBit(bmp, x0, y0+y, bit);
		EBitmapPutBit(bmp, x0, y0-y, bit);
	    }
	    else if (y == 0) {
		EBitmapPutBit(bmp, x0+x, y0, bit);
		EBitmapPutBit(bmp, x0-x, y0, bit);
	    }
	    else {
		EBitmapPutBit(bmp, x0+x, y0+y, bit);
		EBitmapPutBit(bmp, x0-x, y0-y, bit);
		EBitmapPutBit(bmp, x0-x, y0+y, bit);
		EBitmapPutBit(bmp, x0+x, y0-y, bit);
	    }
	}
	fx  = f - pxb;
	fxy = f - pxb + pya;
	fy  = f + pya;
	ax  = abs(fx);
	axy = abs(fxy);
	ay  = abs(fy);

	if ((ax < axy) && (ax < ay)) {
	    x--; f = fx; pxb -= (b2 + b2);
	}
	else if (axy < ay) {
	    x--; y++; f=fxy; pya += (a2 + a2); pxb -= (b2 + b2);
	}
	else {
	    y++; f=fy; pya += (a2 + a2);
	}
    }
    return 0;
}
	

