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

typedef struct epx_bitmap_t {
    EPX_OBJECT_MEMBERS(struct _epx_bitmap_t);
    /*! Clip rectangle restrict pixels drawn within boundary */
    epx_rect_t clip;               
    /*! total width in pixels  */
    unsigned int width;         
    /*! total width in bytes  */
    unsigned int bytesPerRow;   
    /*! total height in pixels */
    unsigned int height;
    /*! 1 */
    unsigned int d;
    /*! size of bitmap area in bytes */
    size_t      sz;
    /*! data, each row is padded to byte size */
    uint8_t* data;    
} epx_bitmap_t;

extern epx_bitmap_t* epx_bitmap_create(unsigned int width, unsigned int height);
extern void epx_bitmap_copy(epx_bitmap_t* src, epx_bitmap_t* dst);
static inline void epx_bitmap_destroy(epx_bitmap_t* bmp) { EObjectUnRef(bmp); }

extern void epx_bitmap_fill(epx_bitmap_t* bmp, uint8_t bit);
extern void epx_bitmap_put_bit(epx_bitmap_t* bmp, int x, int y, uint8_t bit);
extern void epx_bitmap_put_bits(epx_bitmap_t* bmp, int x, int y,
				unsigned int width,  unsigned int height,
				void* data, unsigned int len);
extern uint8_t epx_bitmap_get_bit(epx_bitmap_t* bmp, int x, int y);

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
				   uint8_t fill);
extern void epx_bitmap_scroll_right(epx_bitmap_t* src, epx_bitmap_t* dst, 
				    int rotate, unsigned int amount, 
				    uint8_t fill);
extern void epx_bitmap_scroll_up(epx_bitmap_t* src, epx_bitmap_t* dst, 
			      int rotate, unsigned int amount, 
			      uint8_t fill);
extern void epx_bitmap_scroll_down(epx_bitmap_t* src, epx_bitmap_t* dst, 
				int rotate, unsigned int amount, 
				uint8_t fill);

extern void epx_bitmap_scroll(epx_bitmap_t* src, epx_bitmap_t* dst, 
			   int horizontal, int vertical, 
			   int rotate, uint8_t fill);

/* Rectangles */
extern int epx_bitmap_draw_rectangle(epx_bitmap_t* bmp, int x, int y,
				  unsigned int width, 
				  unsigned int height, uint8_t bit);
extern int epx_bitmap_fill_rectangle(epx_bitmap_t* bmp, int x, int y,
				  unsigned int width, 
				  unsigned int height, uint8_t bit);

/* Line */
extern int epx_bitmap_draw_line(epx_bitmap_t* bmp,
			     int x0, int y0, 
			     int x1, int y1, 
			     int flags,
			     uint8_t bit);

/* Ellipse */
extern int epx_bitmap_draw_ellipse(epx_bitmap_t* bmp, int x0, int y0,
				   unsigned int a, unsigned int b, uint8_t bit);
extern int epx_bitmap_fill_ellipse(epx_bitmap_t* bmp, int x0, int y0,
				   unsigned int a, unsigned int b, uint8_t bit);


#endif
