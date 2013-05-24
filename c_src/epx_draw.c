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
// Shape drawing api
//
#include "../include/epx_pixel.h"
#include "../include/epx_pixmap.h"
#include "../include/epx_gc.h"
#include "../include/epx_draw.h"

extern void epx_draw_line_horizontal(epx_pixmap_t* pic, int x1, int x2, int y, 
				     int flags, epx_pixel_t fg);
extern void epx_draw_line_vertical(epx_pixmap_t* pic, int x, int y1, int y2,
				   int flags, epx_pixel_t fg);
extern void epx_draw_line_plain(epx_pixmap_t* pic, int x1, int y1,int x2,int y2,
				int flags, epx_pixel_t fg);
extern void epx_draw_line(epx_pixmap_t* pic, int x0, int y0, int x1, int y1, 
			  unsigned int line_width, int flags, 
			  epx_pixel_t p);
extern void epx_draw_line_thick(epx_pixmap_t* pic,
				int x0, int y0, 
				int x1, int y1, int line_width,
				int flags, epx_pixel_t fg);
extern void epx_draw_ellipse(epx_pixmap_t* pic, epx_gc_t* gc, 
			     int x, int y,
			     unsigned int width, unsigned int height);
extern void epx_draw_ellipse_border(epx_pixmap_t* pic, epx_gc_t* gc,
				    int x, int y,
				    unsigned int width, unsigned int height);



/* put pixel given address */
static inline void put_apixel(uint8_t* addr, 
			      epx_pixel_unpack_t unpack,
			      epx_pixel_pack_t pack,
			      epx_flags_t flags, epx_pixel_t s)
{
    if (((flags & EPX_FLAG_BLEND)==0) || (s.a == EPX_ALPHA_OPAQUE))
	pack(s, addr);
    else if (s.a != EPX_ALPHA_TRANSPARENT) {
	epx_pixel_t d = unpack(addr);
	d = epx_pixel_blend(s.a, s, d);
	pack(d, addr);
    }
}

//
// draw a point at (x,y) in foreground color (using line style)
//
void epx_pixmap_draw_point(epx_pixmap_t* pic, epx_gc_t* gc, int x, int y)
{
    uint8_t* dst;

    /* First check clip */
    if (!epx_point_xy_in_rect(x, y, &pic->clip))
	return;
    dst = EPX_PIXEL_ADDR(pic,x,y);
    if (!gc) gc = &epx_default_gc;
    put_apixel(dst,pic->unpack,pic->pack,gc->line_style,gc->foreground_color);
}


// Draw rectangle (x,y,w,h) 
void epx_pixmap_draw_rectangle(epx_pixmap_t* pic, epx_gc_t* gc,
			       int x, int y,
			       unsigned int width, 
			       unsigned int height)
{
    epx_rect_t r = {{x, y}, {width, height}};
    epx_flags_t ff;

    if (!gc) gc = &epx_default_gc;

    if (!epx_rect_intersect(&r, &pic->clip, &r))  // FIXME add border
	return;

    x = epx_rect_left(&r);
    y = epx_rect_top(&r);
    ff = gc->fill_style;
    if (ff == EPX_FILL_STYLE_NONE) {
	epx_flags_t bf     = gc->border_style | EPX_LINE_STYLE_NFIRST;
	epx_pixel_t bc     = gc->border_color;
	unsigned int bw = gc->border_width;
	int x1 = epx_rect_right(&r);
	int y1 = epx_rect_bottom(&r);
	epx_draw_line(pic,x,y,x1,y,bw,bf,bc);
	epx_draw_line(pic,x1,y,x1,y1,bw,bf,bc);
	epx_draw_line(pic,x1,y1,x,y1,bw,bf,bc);
	epx_draw_line(pic,x,y1,x,y,bw,bf,bc);
	return;
    }
    else {
	uint8_t* ptr;
	int x1, y1;
	epx_flags_t  bf = gc->border_style;
	epx_pixel_t fc  = gc->fill_color;
	int bw          = gc->border_width;
	x1 = epx_rect_right(&r);
	y1 = epx_rect_bottom(&r);

	if ((bw > 0) && 
	    ((bf & EPX_BORDER_STYLE_NBORDER) != EPX_BORDER_STYLE_NBORDER)) {
	    epx_pixel_t bc = gc->border_color;
	    if (!(bf & EPX_BORDER_STYLE_NBOTTOM))
		epx_draw_line(pic,x,y-1,x1,y-1,bw,bf,bc);
	    if (!(bf & EPX_BORDER_STYLE_NLEFT))
		epx_draw_line(pic,x1+1,y,x1+1,y1,bw,bf,bc);
	    if (!(bf & EPX_BORDER_STYLE_NTOP))
		epx_draw_line(pic,x1,y1+1,x,y1+1,bw,bf,bc);
	    if (!(bf & EPX_BORDER_STYLE_NRIGHT))
		epx_draw_line(pic,x-1,y1,x-1,y,bw,bf,bc);
	}
	ptr = EPX_PIXEL_ADDR(pic,x,y);
	width  = epx_rect_width(&r);
	height = epx_rect_height(&r);

	if (((ff & EPX_FLAG_BLEND)==0) || (fc.a == EPX_ALPHA_OPAQUE))
	    epx_fill_area(ptr,pic->bytes_per_row,pic->pixel_format,fc,
			  width,height);
	else if (fc.a != EPX_ALPHA_TRANSPARENT) {
	    epx_fill_area_blend(ptr,pic->bytes_per_row,pic->pixel_format,fc,
				width,height);
	}
    }
}

void epx_pixmap_draw_line(epx_pixmap_t* pic, epx_gc_t* gc,
			  int x0, int y0,
			  int x1, int y1)
{
    if (!gc) gc = &epx_default_gc;

    if (gc->line_width == 1) {
	if (y1 == y0)
	    epx_draw_line_horizontal(pic,x0,x1,y0,
				     gc->line_style,
				     gc->foreground_color);
	else if (x1 == x0) {
	    epx_draw_line_vertical(pic,x0,y0,y1,
				   gc->line_style,
				   gc->foreground_color);
	}
	else
	    epx_draw_line_plain(pic,x0,y0,x1,y1,
				gc->line_style,
				gc->foreground_color);
    }
    else if (gc->line_width > 1)
	epx_draw_line_thick(pic,x0,y0,x1,y1,gc->line_width,gc->line_style,
			    gc->foreground_color);
}

void epx_pixmap_draw_ellipse(epx_pixmap_t* pic, epx_gc_t* gc, 
			     int x, int y,
			     unsigned int width, unsigned int height)
{
    if (gc == NULL) gc = &epx_default_gc;    

    if (((gc->border_style & EPX_BORDER_STYLE_NBORDER) == 
	 EPX_BORDER_STYLE_NBORDER) || (gc->border_width == 0)) {
	epx_draw_ellipse(pic, gc, x, y, width, height);
    }
    else {
	epx_draw_ellipse_border(pic, gc, x, y, width, height);
    }
}
	
