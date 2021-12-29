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
//
// epx_bitmap.h
//
//   EPX bitmap api
//
#ifndef __EPX_BITMAP_H__
#define __EPX_BITMAP_H__

#include <stdint.h>
#include "epx_geometry.h"
#include "epx_object.h"
#include "epx_pixel.h"
#include "epx_pixmap.h"

typedef struct _epx_bitmap_t {
    EPX_OBJECT_MEMBERS(struct _epx_bitmap_t);
    /*! Clip rectangle restrict pixels drawn within boundary */
    struct _epx_bitmap_t* parent;         // parent bitmap (for sub_bitmap)    
    epx_rect_t clip;               
    /*! total width in pixels  */
    unsigned int width;         
    /*! total width in bytes  */
    unsigned int bytes_per_row;   
    /*! total height in pixels */
    unsigned int height;
    /*! 1 */
    unsigned int d;
    /*! size of bitmap area in bytes */
    size_t      sz;
    /*! data, original data (free this! not data) */
    uint8_t* data0;
    /*! data, each row is padded to alignment */
    uint8_t* data;    
} epx_bitmap_t;

#define EPX_BITMAP_ALIGNMENT  4     // 8/16?

#define BYTE_OFFSET(ofs)        ((unsigned) (ofs) >> 3)
#define BIT_OFFSET(ofs)         ((ofs) & 7)
#define MAKE_MASK(n)            ((1 << (n))-1)
#define MASK_BITS(src,dst,mask) (((src) & (mask)) | ((dst) & ~(mask)))
#define ROW_BYTES(w) BYTE_OFFSET((w)+7)

/* address of bit */
#define EPX_BIT_ADDR(bmp,x,y)					\
    ((bmp)->data+((y)*(bmp)->bytes_per_row)+BYTE_OFFSET(x))

/* bit number */
#define EPX_BIT_NUMBER(x) (7-BIT_OFFSET(x))
/* bit mask */
#define EPX_BIT_MASK(x)   (1 << EPX_BIT_NUMBER(x))


extern epx_bitmap_t* epx_bitmap_create(unsigned int width, unsigned int height);
extern epx_bitmap_t* epx_bitmap_copy(epx_bitmap_t* src);
extern int epx_bitmap_copy_to(epx_bitmap_t* src, epx_bitmap_t* dst);
static inline void epx_bitmap_destroy(epx_bitmap_t* bmp) { epx_object_unref(bmp); }
extern void epx_bitmap_set_clip(epx_bitmap_t* pic, epx_rect_t* clip);
extern void epx_bitmap_fill(epx_bitmap_t* bmp, uint8_t pat);
extern void epx_bitmap_put_bit(epx_bitmap_t* bmp, int x, int y, int val);
extern void epx_bitmap_put_bits(epx_bitmap_t* bmp, int x, int y,
				unsigned int width,  unsigned int height,
				void* data, unsigned int len);
extern int epx_bitmap_get_bit(epx_bitmap_t* bmp, int x, int y);

extern int epx_bitmap_draw(epx_bitmap_t* src, epx_pixmap_t* dst,
			   int x_src, int y_src, int x_dst, int y_dst,
			   unsigned int width, unsigned int height,
			   epx_pixel_t fg, epx_pixel_t bg);

/* Copy, this will copy src to dest with blending */
extern int epx_bitmap_copy_area(epx_bitmap_t* src, epx_bitmap_t* dst,
				int x_src, int y_src, int x_dst, int y_dst,
				unsigned int width, unsigned int height);

extern void epx_bitmap_scroll_left(epx_bitmap_t* src, epx_bitmap_t* dst,
				   int rotate, unsigned int amount, 
				   uint8_t pat);
extern void epx_bitmap_scroll_right(epx_bitmap_t* src, epx_bitmap_t* dst, 
				    int rotate, unsigned int amount, 
				    uint8_t pat);
extern void epx_bitmap_scroll_up(epx_bitmap_t* src, epx_bitmap_t* dst, 
				 int rotate, unsigned int amount, 
				 uint8_t pat);
extern void epx_bitmap_scroll_down(epx_bitmap_t* src, epx_bitmap_t* dst, 
				   int rotate, unsigned int amount, 
				   uint8_t pat);

extern void epx_bitmap_scroll(epx_bitmap_t* src, epx_bitmap_t* dst, 
			      int horizontal, int vertical, 
			      int rotate, uint8_t pat);

// rectangles
extern int epx_bitmap_draw_rectangle(epx_bitmap_t* bmp, int x, int y,
				     unsigned int width, 
				     unsigned int height, uint8_t pat);
extern int epx_bitmap_fill_rectangle(epx_bitmap_t* bmp, int x, int y,
				     unsigned int width, 
				     unsigned int height, uint8_t pat);

// line 
extern int epx_bitmap_draw_line(epx_bitmap_t* bmp,
				int x0, int y0, int x1, int y1, 
				int flags, int val);

// ellipse 
extern int epx_bitmap_draw_ellipse(epx_bitmap_t* bmp, int x, int y,
				   unsigned int width, unsigned int height,
				   uint8_t pat);
extern int epx_bitmap_fill_ellipse(epx_bitmap_t* bmp, int x, int y,
				   unsigned int width, unsigned int height,
				   uint8_t pat);

#endif
