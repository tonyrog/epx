/***************************************************************************
 *
 * Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
 *
 * This software is licensed as described in the file COPYRIGHT, which
 * you should have received as part of this distribution. The terms
 * are also available at http://www.rogvall.se/docs/copyright.txt.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYRIGHT file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 ***************************************************************************/
/*
 * EPX Bitmap functions
 *
 */
#include <math.h>
#include <memory.h>
#include "../include/epx_pixmap.h"
#include "../include/epx_bitmap.h"

void epx_bitmap_set_clip(epx_bitmap_t* bitmap, epx_rect_t* clip)
{
    epx_rect_t physRect;

    epx_rect_set(&physRect, 0, 0, bitmap->width, bitmap->height);
    epx_rect_intersect(clip, &physRect, &bitmap->clip);
}

/*
 * The basic bit copy operation. Copies n bits from the source buffer to
 * the destination buffer. Depending on the directions, it can reverse the
 * copied bits (bytes actually)
 */

void copy_bits(
    uint8_t* src,    // Base pointer to source.
    size_t soffs,    // Bit offset for source relative to src.
    int sdir,	     // Direction: 1 (forward) or -1 (backward).
    uint8_t* dst,    // Base pointer to destination.
    size_t doffs,    // Bit offset for destination relative to dst.
    int ddir,        // Direction: 1 (forward) or -1 (backward). */
    size_t n)	     // Number of bits to copy.
{
    uint32_t lmask;
    uint32_t rmask;
    uint32_t count;
    uint32_t deoffs;

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
	    uint32_t bits = (*src << (soffs-doffs));
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
	uint32_t bits;
	uint32_t bits1;
	uint32_t rshift;
	uint32_t lshift;

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


void set_bits(
    uint8_t pattern,   // 8bit bit pattern
    uint8_t* dst,      // Base pointer to destination.
    size_t doffs,      // Bit offset for destination relative to dst.
    int ddir,	       // Direction: 1 (forward) or -1 (backward).
    size_t n)	       // Number of bits to set.
{
    uint32_t lmask;
    uint32_t rmask;
    uint32_t count;
    uint32_t deoffs;

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

int epx_bitmap_init(epx_bitmap_t* dst, unsigned int width, unsigned int height)
{
    uint8_t* data0;
    unsigned int bytes_per_row = ROW_BYTES(width);
    size_t sz;

    EPX_OBJECT_INIT(dst, EPX_BITMAP_TYPE);
    dst->data0   = NULL;
    dst->data    = NULL;    
    dst->parent  = NULL;
    
    bytes_per_row += EPX_ALIGN_OFFS(bytes_per_row, EPX_BITMAP_ALIGNMENT);
    sz = bytes_per_row*height;

    // align? maybe align 16 byte to make good use off SSE
    if ((data0 = (uint8_t*) malloc(sz+EPX_BITMAP_ALIGNMENT-1)) == NULL)
	return -1;
    epx_rect_set(&dst->clip, 0, 0, width, height);
    dst->width = width;
    dst->bytes_per_row = bytes_per_row;
    dst->height = height;
    dst->d  = 1;
    dst->sz = sz;  // total number of bytes
    dst->data0          = data0;
    dst->data           = data0 + EPX_ALIGN_OFFS(data0, EPX_BITMAP_ALIGNMENT);
    return 0;
}

epx_bitmap_t* epx_bitmap_create(unsigned int width, unsigned int height)
{
    epx_bitmap_t* bmp;

    if ((bmp = (epx_bitmap_t*) malloc(sizeof(epx_bitmap_t))) == NULL)
	return NULL;
    if (epx_bitmap_init(bmp, width, height) < 0) {
	free(bmp);
	return NULL;
    }
    bmp->on_heap = 1;
    bmp->refc = 1;    
    return bmp;
}

void EPX_BITMAP_TYPE_RELEASE(void *arg)
{
    epx_bitmap_t* bmp = (epx_bitmap_t*) arg;

    DEBUGF("EPX_BITMAP_TYPE_RELEASE: %p", arg);
    if (bmp->data0 != NULL) {
	free(bmp->data0);
	bmp->data0 = NULL;
	bmp->data  = NULL;
    }
    if (bmp->on_heap)
	free(bmp);
}

static inline void put_abit(uint8_t* ptr, int x, int val)
{
    unsigned int n = EPX_BIT_NUMBER(x);
    uint8_t bit = (1 << n);
    uint8_t d = *ptr & ~bit;
    *ptr = val ? (d | bit) : d;
}

/* write a bit at (x,y) */
void epx_bitmap_put_bit(epx_bitmap_t* bmp, int x, int y, int val)
{
    uint8_t* dst;
    /* First check clip */
    if (!epx_point_xy_in_rect(x,y,&bmp->clip))
	return;
    dst = EPX_BIT_ADDR(bmp,x,y);
    if ((dst < bmp->data) || (dst >= bmp->data+bmp->sz))
	return;
    put_abit(dst, x, val);
}

/* put_bits:
 *   copy bits  R(0,0,width,height)
 *   into destination R(x_dst,y_dst,width,height)
 *   bits in source must be byte aligned for each row!
 */

void epx_bitmap_put_bits(epx_bitmap_t* dst, int x_dst, int y_dst,
			 unsigned int width, unsigned int height,
			 void* data, unsigned int len)
{
    uint8_t* src_ptr;
    int       src_offs;
    uint8_t* src_end;
    unsigned int src_wb = ROW_BYTES(width);
    uint8_t* dst_ptr;
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
    src_ptr   = ((uint8_t*)data)+(y1*src_wb) + BYTE_OFFSET(x1);
    src_offs  = BIT_OFFSET(x1);
    src_end   = ((uint8_t*)data) + len;
    dst_ptr   = EPX_BIT_ADDR(dst,x3,y3);
    dst_offs  = BIT_OFFSET(x3);

    height = (y2-y1)+1;
    width  = (x2-x1)+1;
    while(width && height-- && (src_ptr+src_wb <= src_end)) {
	copy_bits(src_ptr, src_offs, 1, dst_ptr, dst_offs, 1, width);
	src_ptr += src_wb;
	dst_ptr += dst->bytes_per_row;
    }
}


/* read a pixel */
int epx_bitmap_get_bit(epx_bitmap_t* bmp, int x, int y)
{
    uint8_t* dst;
    uint8_t  mask;
    if (!epx_point_xy_in_rect(x,y,&bmp->clip))
	return 0;
    dst = EPX_BIT_ADDR(bmp,x,y);
    // ASSERT ((dst >= bmp->data) && (dst < bmp->data+bmp->sz))
    mask = EPX_BIT_MASK(x);
    return (*dst & mask) != 0;
}

/* Fill Bitmap with byte pattern */
void epx_bitmap_fill(epx_bitmap_t* bmp, uint8_t pat)
{
    memset(bmp->data, pat, bmp->sz);
}

/* fill pixel area */
static inline void fill_area(uint8_t* dst, int dst_offs, int dst_wb,
			     int width, int height, uint8_t pat)
{
    while(height--) {
	set_bits(pat, dst, dst_offs, 1, width);
	dst += dst_wb;
    }
}

// copy bitmap area (bytes)
static inline void copy_area(uint8_t* src, int src_wb,
			     uint8_t* dst, int dst_wb,
			     int width, int height)
{
    int n = width;

    if (dst < src) {
	while(height--) {
	    memmove(dst, src, n);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	src += (src_wb*height);
	dst += (dst_wb*height);
	
	while(height--) {
	    src -= src_wb;
	    dst -= dst_wb;
	    memmove(dst, src, n);
	}
    }
}

// copy area and shift lines left or right
static inline void shift_area(uint8_t* src, int src_wb,
			      uint8_t* dst, int dst_wb,
			      unsigned int width, unsigned int height, 
			      int amount)
{
    int n = width;
    int soffs, doffs;
    
    if (amount > 0) {
	soffs = amount;
	doffs = 0;
	n -= amount;
    }
    else {
	soffs = 0;
	doffs = -amount;
	n   -= (-amount);
    }
    while(height--) {
	copy_bits(src, soffs, 1, dst, doffs, 1, n);
	src += src_wb;
	dst += dst_wb;
    }
}


/* copy area and shift lines left or right */
static inline void rotate_area(uint8_t* src, int src_wb,
			       uint8_t* dst, int dst_wb,
			       unsigned int width, unsigned int height, 
			       int amount)
{
    int a = (amount < 0) ? -amount : amount;
    int n = width;
    int is_inline = (src == dst);
    int soffs, soffs1;
    int doffs, doffs1;

    if (amount == 0)
	return;
    else if (amount > 0) {
	soffs = a;
	soffs1 = 0;
	n -= a;
	doffs = 0;
	doffs1 = n;
    }
    else {
	n  -= a;
	soffs = 0;
	soffs1 = n;
	doffs = a;
	doffs1 = 0;
    }

    if (is_inline) {
	uint8_t save[ROW_BYTES(a)];
	while(height--) {
	    copy_bits(src, soffs1, 1, save, 0, 1, a);
	    copy_bits(src, soffs, 1, dst, doffs, 1, n);	    
	    copy_bits(save, 0, 1, dst, doffs1, 1, a);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {  /* not inline */
	while(height--) {
	    copy_bits(src, soffs, 1, dst, doffs, 1, n);
	    copy_bits(src, soffs1, 1, dst, doffs1, 1, a);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
}

/*
 * Copy pixmap data from src to dst, ignore clip region
 * if pixmap size is the same just memcpy
 * otherwise calculate the min area and copy that
 */
int epx_bitmap_copy_to(epx_bitmap_t* src, epx_bitmap_t* dst)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    copy_area(src->data, src->bytes_per_row,
	      dst->data, dst->bytes_per_row, w, h);
    return 0;
}

epx_bitmap_t* epx_bitmap_copy(epx_bitmap_t* src)
{
    epx_bitmap_t* dst = epx_bitmap_create(src->width, src->height);

    memcpy(dst->data, src->data, src->sz);
    return dst;
}

void epx_bitmap_scroll_left(epx_bitmap_t* src, epx_bitmap_t* dst,
			    int rotate, unsigned int amount, uint8_t pat)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	rotate_area(src->data, src->bytes_per_row,
		    dst->data, dst->bytes_per_row,
		    w, h, a);
    else {
	shift_area(src->data, src->bytes_per_row,
		   dst->data, dst->bytes_per_row, w, h, a);
	fill_area(dst->data, (w-amount), dst->bytes_per_row, amount, h, pat);
    }
}

void epx_bitmap_scroll_right(epx_bitmap_t* src, epx_bitmap_t* dst, 
			     int rotate, unsigned int amount, uint8_t pat)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	rotate_area(src->data, src->bytes_per_row,
		    dst->data, dst->bytes_per_row,
		    w, h, -a);
    else {
	shift_area(src->data, src->bytes_per_row,
		   dst->data, dst->bytes_per_row,
		   w, h, -a);
	fill_area(dst->data, 0, dst->bytes_per_row, amount, h, pat);
    }
}

void epx_bitmap_scroll_up(epx_bitmap_t* src, epx_bitmap_t* dst, 
			  int rotate, unsigned int amount, uint8_t pat)
{
    if ((amount >= src->height) && !rotate)
	epx_bitmap_fill(dst, pat);
    else {
	uint8_t* dst_ptr;
	uint8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EPX_BIT_ADDR(src,0,amount);
	dst_ptr = EPX_BIT_ADDR(dst,0,0);

	if (rotate) {
	    if (src == dst) {
		uint8_t save[(amount*src->bytes_per_row)];
		uint8_t* dst_save = EPX_BIT_ADDR(dst,0,0);

		copy_area(dst_save, dst->bytes_per_row,
			  save, dst->bytes_per_row, dst->width, amount);
		copy_area(src_ptr, src->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, h);
		dst_ptr = EPX_BIT_ADDR(dst,0,h);
		copy_area(save, dst->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, amount);
	    }
	    else {
		copy_area(src_ptr, src->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, h);
		dst_ptr = EPX_BIT_ADDR(dst,0,h);
		src_ptr = EPX_BIT_ADDR(src,0,0);
		copy_area(src_ptr, src->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, amount);
	    }
	}
	else {
	    copy_area(src_ptr, src->bytes_per_row,
		      dst_ptr, dst->bytes_per_row, w, h);
	    dst_ptr = EPX_BIT_ADDR(dst,0,h);
	    fill_area(dst_ptr, 0, dst->bytes_per_row, dst->width, amount, pat);
	}
    }
}

void epx_bitmap_scroll_down(epx_bitmap_t* src, epx_bitmap_t* dst, 
			    int rotate, unsigned int amount, uint8_t pat)
{
    if ((amount >= src->height) && !rotate)
	epx_bitmap_fill(dst, pat);
    else {
	uint8_t* dst_ptr;
	uint8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EPX_BIT_ADDR(src,0,0);
	dst_ptr = EPX_BIT_ADDR(dst,0,amount);

	if (rotate) {
	    if (src == dst) {
		uint8_t save[amount*src->bytes_per_row];
		uint8_t* dst_save = EPX_BIT_ADDR(dst,0,h);

		copy_area(dst_save, dst->bytes_per_row,
			  save, dst->bytes_per_row, dst->width, amount);
		copy_area(src_ptr, src->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, h);
		dst_ptr = EPX_BIT_ADDR(dst,0,0);
		copy_area(save, dst->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, amount);
	    }
	    else {
		copy_area(src_ptr, src->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, h);
		src_ptr = EPX_BIT_ADDR(src,0,h);
		dst_ptr = EPX_BIT_ADDR(dst,0,0);
		copy_area(src_ptr, src->bytes_per_row,
			  dst_ptr, dst->bytes_per_row, w, amount);
	    }
	}
	else {
	    copy_area(src_ptr, src->bytes_per_row,
		      dst_ptr, dst->bytes_per_row, w, h);
	    dst_ptr = EPX_BIT_ADDR(dst,0,0);
	    fill_area(dst_ptr, 0, dst->bytes_per_row, dst->width, amount, pat);
	}
    }
}

// draw clipped horizontal line
static void hline_(epx_bitmap_t* bitmap, int x1, int x2, int y, uint8_t pat)
{
    uint8_t* ptr;
    ptr = EPX_BIT_ADDR(bitmap,0,y);
    set_bits(pat, ptr, x1, 1, (x2-x1)+1);
}

// clip and draw horizontal line
static void hline(epx_bitmap_t* bitmap, int x1, int x2, int y, uint8_t pat)
{
    int xl, xr;
    uint8_t* ptr;

    if (y < epx_rect_top(&bitmap->clip)) return;
    if (y > epx_rect_bottom(&bitmap->clip)) return;
    if (x1 > x2) epx_swap_int(x1,x2);

    if (x2 < (xl = epx_rect_left(&bitmap->clip))) return;
    if (x1 > (xr = epx_rect_right(&bitmap->clip))) return;
    
    x1 = epx_clip_range(x1, xl, xr);
    x2 = epx_clip_range(x2, xl, xr);
    
    ptr = EPX_BIT_ADDR(bitmap,0,y);
    set_bits(pat, ptr, x1, 1, (x2-x1)+1);
}

// clip and draw vertical line
static void vline(epx_bitmap_t* bitmap, int x, int y1, int y2, uint8_t pat)
{
    int yt, yb;
    uint8_t* ptr;

    if (x < epx_rect_left(&bitmap->clip)) return;
    if (x > epx_rect_right(&bitmap->clip)) return;
    if (y1 > y2) epx_swap_int(y1,y2);

    if (y2 < (yt = epx_rect_top(&bitmap->clip))) return;
    if (y1 > (yb = epx_rect_bottom(&bitmap->clip))) return;
    
    y1 = epx_clip_range(y1, yt, yb);
    y2 = epx_clip_range(y2, yt, yb);
    
    ptr = EPX_BIT_ADDR(bitmap,0,y1);
    while(y1 < y2) {
	set_bits(pat, ptr, x, 1, 1);
	ptr += bitmap->bytes_per_row;
	y1++;
	pat = (pat << 1) | (pat >> 7);  // rotate pattern left
    }
}

/* scroll pixmap up/dow  left/right rotate/fill */
void epx_bitmap_scroll(epx_bitmap_t* src, epx_bitmap_t* dst, 
		       int horizontal, int vertical, 
		       int rotate, uint8_t pat)
{
    if (vertical>0)
	epx_bitmap_scroll_up(src, dst, rotate, vertical, pat);
    else if (vertical < 0)
	epx_bitmap_scroll_down(src, dst, rotate, -vertical, pat);
    if (horizontal>0)
	epx_bitmap_scroll_right(src, dst, rotate, horizontal, pat);
    else if (horizontal < 0)
	epx_bitmap_scroll_left(src, dst, rotate, -horizontal, pat);
}


/* Draw rectangle (x,y,w,h) with pixel p */
int epx_bitmap_draw_rectangle(epx_bitmap_t* bmp,
			      int x0, int y0,
			      unsigned int width, 
			      unsigned int height, uint8_t pat)
{
    int x1 = x0+width-1;
    int y1 = y0+height-1;
    
    hline(bmp,x0,x1-1,y0,pat);
    vline(bmp,x1,y0,y1-1,pat);
    hline(bmp,x1,x0+1,y1,pat);
    vline(bmp,x0,y1,y0+1,pat);
    return 0;
}

/* Fill rectangle (x,y,w,h) with pixel p */
int epx_bitmap_fill_rectangle(epx_bitmap_t* bmp,
			      int x, int y,
			      unsigned int width, 
			      unsigned int height, uint8_t pat)
{
    uint8_t* ptr;
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

    /* clip "physical" limits */
    x1 = clip_range(x1, 0, bmp->width-1);
    y1 = clip_range(y1, 0, bmp->height-1);
    x2 = clip_range(x2, 0, bmp->width-1);
    y2 = clip_range(y2, 0, bmp->height-1);

    ptr = EPX_BIT_ADDR(bmp,0,y1);
    width  = (x2-x1)+1;
    height = (y2-y1)+1;

    fill_area(ptr,x1,bmp->bytes_per_row, width, height, pat);
    return 0;
}

/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 */
int epx_bitmap_copy_area(epx_bitmap_t* src, epx_bitmap_t* dst,
			 int x_src, int y_src, int x_dst, int y_dst,
			 unsigned int width, unsigned int height)
{
    uint8_t* src_ptr;
    uint8_t* dst_ptr;
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

    /* clip "physical" limits */
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
    
    /* now start moving bits from (x1,y1,x2,y2) -> (x3,y3) */
    src_ptr = EPX_BIT_ADDR(src,0,y1);
    dst_ptr = EPX_BIT_ADDR(dst,0,y3);
    width  = (x2-x1)+1;
    height = (y2-y1)+1;

    while(height--) {
	copy_bits(src_ptr, x1, 1, dst_ptr, x3, 1, width);
	src_ptr += src->bytes_per_row;
	dst_ptr += dst->bytes_per_row;
    }
    return 0;
}

/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h)  */
int epx_bitmap_draw(epx_bitmap_t* src, epx_pixmap_t* dst,
		    int x_src, int y_src, int x_dst, int y_dst,
		    unsigned int width, unsigned int height,
		    epx_pixel_t fg, epx_pixel_t bg)
{
    uint8_t* src_ptr;
    uint8_t* dst_ptr;    
    uint8_t* src_ptr0;
    uint8_t* dst_ptr0;
    int x1, y1;
    int x2, y2;
    int x3, y3;
    int x4, y4;
    int rx1, ry1, rx2, ry2;
    int dx, dy, dh, dw;
    epx_pixel_unpack_t unpack;
    epx_pixel_pack_t pack;

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
    src_ptr0 = EPX_BIT_ADDR(src,x1,y1);
    dst_ptr0 = EPX_PIXEL_ADDR(dst,x3,y3);

    width  = (x2-x1)+1;
    height = (y2-y1)+1;

    unpack = dst->func.unpack;
    pack   = dst->func.pack;

    while(width && height--) {
	int x = x1;
	uint8_t mask = EPX_BIT_MASK(x);
	uint8_t src_bits;

	src_ptr  = src_ptr0;
	dst_ptr  = dst_ptr0;
	x_src    = width;
	src_bits = *src_ptr++;
	while(x_src--) {
	    epx_pixel_t s = (src_bits & mask) ? fg : bg;
	    epx_pixel_t d = unpack(dst_ptr);

	    d = epx_pixel_blend(s.a, s, d);
	    pack(d, dst_ptr);
	    dst_ptr += dst->bytes_per_pixel;
	    x++;
	    mask >>= 1;
	    if (x_src && (BIT_OFFSET(x) == 0)) {
		mask = 0x80;
		src_bits = *src_ptr++;
	    }
	}
	src_ptr0 += src->bytes_per_row;
	dst_ptr0 += dst->bytes_per_row;
    }
    return 0;
}


/*
 * draw_line:
 *
 *  FIXME: draw line width > 1
 *
 */
int epx_bitmap_draw_line(epx_bitmap_t* bmp,
			 int x0, int y0, 
			 int x1, int y1, int skip,
			 int val)
{
    (void) skip;
    int dy = y1 - y0;
    int dx = x1 - x0;
    int sx, sy, syw;
    int x2, x3, y2, y3; /* used for clipping */
    int xt;
    uint8_t* ptr;

    x2 = bmp->clip.xy.x;
    y2 = bmp->clip.xy.y;
    x3 = x2 + (bmp->clip.wh.width-1);
    y3 = y2 + (bmp->clip.wh.height-1);

    x2 = clip_range(x2, 0, bmp->width-1);
    y2 = clip_range(y2, 0, bmp->height-1);
    x3 = clip_range(x3, 0, bmp->width-1);
    y3 = clip_range(y3, 0, bmp->height-1);

    if (dy < 0) {
	dy=-dy; sy=-1; syw=-bmp->bytes_per_row;
    }
    else {
	sy=1; syw=bmp->bytes_per_row;
    }
    if (dx < 0) {
	dx=-dx; sx=-1;
    }
    else {
	sx=1;
    }
    dy <<= 1;
    dx <<= 1;

    ptr = EPX_BIT_ADDR(bmp,x0,y0);
    xt  = BYTE_OFFSET(x0);       /* save the byte number */

    if (in_range(x0, x2, x3) && in_range(y0, y2, y3))
	put_abit(ptr, x0, val);

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
		put_abit(ptr, x0, val);
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
		put_abit(ptr, x0, val);
	}
    }
    return 0;
}

static int ellipse(epx_bitmap_t* bmp, int x, int y, 
		   unsigned int width, unsigned int height, int fill,
		   uint8_t pat)
{
    unsigned int a = width  >> 1;
    unsigned int b = height >> 1;
    int xc = x + a;
    int yc = y + b;
    unsigned long a2 = a*a;
    unsigned long b2 = b*b;
    long pya = a2;
    long pxb = (2*a-1)*b2;
    long f = 0;
    long fx, fxy, fy;
    unsigned long ax, axy, ay;
    int xo = a;
    int yo = 0;

    if ((a==0) || (b==0))
	return 0;

    while((xo >= 0) && (yo <= (int)b)) {
	int bit = (pat>>7);
	if (fill) {
	    if ((xo == 0) && (yo == 0))
		epx_bitmap_put_bit(bmp,xc,yc,bit);
	    else if (xo == 0) {
		epx_bitmap_put_bit(bmp,xc,yc+yo,bit);
		epx_bitmap_put_bit(bmp,xc,yc-yo,bit);
	    }
	    else if (yo == 0) {
		hline(bmp,xc-xo,xc+xo,yc,pat);
	    }
	    else {
		hline(bmp,xc-xo,xc+xo,yc+yo,pat);
		hline(bmp,xc-xo,xc+xo,yc-yo,pat);
	    }
	}
	else {
	    if ((xo == 0) && (yo == 0))
		epx_bitmap_put_bit(bmp, xc, yc, bit);
	    else if (xo == 0) {
		epx_bitmap_put_bit(bmp, xc, yc+yo, bit);
		epx_bitmap_put_bit(bmp, xc, yc-yo, bit);
	    }
	    else if (yo == 0) {
		epx_bitmap_put_bit(bmp, xc+xo, yc, bit);
		epx_bitmap_put_bit(bmp, xc-xo, yc, bit);
	    }
	    else {
		epx_bitmap_put_bit(bmp, xc+xo, yc+yo, bit);
		epx_bitmap_put_bit(bmp, xc-xo, yc-yo, bit);
		epx_bitmap_put_bit(bmp, xc-xo, yc+yo, bit);
		epx_bitmap_put_bit(bmp, xc+xo, yc-yo, bit);
	    }
	}
	pat <<= 1; pat |= bit;   // rotate pattern left	
	fx  = f - pxb;
	fxy = f - pxb + pya;
	fy  = f + pya;
	ax  = labs(fx);
	axy = labs(fxy);
	ay  = labs(fy);

	if ((ax < axy) && (ax < ay)) {
	    xo--; f = fx; pxb -= (b2 + b2);
	}
	else if (axy < ay) {
	    xo--; yo++; f=fxy; pya += (a2 + a2); pxb -= (b2 + b2);
	}
	else {
	    yo++; f=fy; pya += (a2 + a2);
	}
    }
    return 0;
}
	
int epx_bitmap_draw_ellipse(epx_bitmap_t* bmp, int x, int y,
			    unsigned int width, unsigned int height,
			    uint8_t pat)
{
    return ellipse(bmp, x, y, width, height, 0, pat);
}

int epx_bitmap_fill_ellipse(epx_bitmap_t* bmp, int x, int y,
			    unsigned int width, unsigned int height,
			    uint8_t pat)
{
    return ellipse(bmp, x, y, width, height, 1, pat);
}
