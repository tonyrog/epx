/***************************************************************************
 *
 * Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
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
    put_apixel(dst,pic->func.unpack,pic->func.pack,
	       gc->line_style,gc->foreground_color);
}

void epx_draw_rectangle(epx_pixmap_t* pixmap,
			int x, int y, unsigned int width, unsigned int height,
			epx_flags_t ff, epx_pixel_t fc,
			unsigned int bw, epx_flags_t bf, epx_pixel_t bc)
{
    epx_rect_t r = {{x, y}, {width, height}};
    int x1, y1;
    if (!epx_rect_intersect(&r, &pixmap->clip, &r))
	return;
    x = epx_rect_left(&r);
    y = epx_rect_top(&r);
    x1 = epx_rect_right(&r);
    y1 = epx_rect_bottom(&r);
    if (ff == EPX_FILL_STYLE_NONE) {

	bf |= EPX_LINE_STYLE_NFIRST;
	epx_draw_line(pixmap,x,y,x1,y,bw,bf,bc);
	epx_draw_line(pixmap,x1,y,x1,y1,bw,bf,bc);
	epx_draw_line(pixmap,x1,y1,x,y1,bw,bf,bc);
	epx_draw_line(pixmap,x,y1,x,y,bw,bf,bc);
    }
    else {
	uint8_t* ptr;
	if ((bw > 0) &&
	    ((bf & EPX_BORDER_STYLE_NBORDER) != EPX_BORDER_STYLE_NBORDER)) {
	    if (!(bf & EPX_BORDER_STYLE_NBOTTOM))
		epx_draw_line(pixmap,x,y-1,x1,y-1,bw,bf,bc);
	    if (!(bf & EPX_BORDER_STYLE_NLEFT))
		epx_draw_line(pixmap,x1+1,y,x1+1,y1,bw,bf,bc);
	    if (!(bf & EPX_BORDER_STYLE_NTOP))
		epx_draw_line(pixmap,x1,y1+1,x,y1+1,bw,bf,bc);
	    if (!(bf & EPX_BORDER_STYLE_NRIGHT))
		epx_draw_line(pixmap,x-1,y1,x-1,y,bw,bf,bc);
	}
	ptr = EPX_PIXEL_ADDR(pixmap,x,y);
	width  = epx_rect_width(&r);
	height = epx_rect_height(&r);

	if (((ff & EPX_FLAG_BLEND)==0) || (fc.a == EPX_ALPHA_OPAQUE))
	    epx_fill_area(ptr,pixmap->bytes_per_row,pixmap->pixel_format,fc,
			  width,height);
	else if (fc.a != EPX_ALPHA_TRANSPARENT) {
	    epx_fill_area_blend(ptr,pixmap->bytes_per_row,
				pixmap->pixel_format,fc,
				width,height);
	}
    }
}


// Draw rectangle (x,y,w,h)
void epx_pixmap_draw_rectangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
			       int x, int y,
			       unsigned int width,
			       unsigned int height)
{
    if (!gc) gc = &epx_default_gc;

    epx_draw_rectangle(pixmap, x, y, width, height,
		       gc->fill_style, gc->fill_color,
		       gc->border_width, gc->border_style, gc->border_color);
}

void epx_pixmap_draw_line(epx_pixmap_t* pixmap, epx_gc_t* gc,
			  int x0, int y0,
			  int x1, int y1)
{
    if (!gc) gc = &epx_default_gc;

    if (gc->line_width == 1) {
	if (y1 == y0)
	    epx_draw_line_horizontal(pixmap,x0,x1,y0,
				     gc->line_style,
				     gc->foreground_color);
	else if (x1 == x0) {
	    epx_draw_line_vertical(pixmap,x0,y0,y1,
				   gc->line_style,
				   gc->foreground_color);
	}
	else
	    epx_draw_line_plain(pixmap,x0,y0,x1,y1,
				gc->line_style,
				gc->foreground_color);
    }
    else if (gc->line_width > 1)
	epx_draw_line_thick(pixmap,x0,y0,x1,y1,gc->line_width,gc->line_style,
			    gc->foreground_color);
}

void epx_pixmap_draw_ellipse(epx_pixmap_t* pixmap, epx_gc_t* gc,
			     int x, int y,
			     unsigned int width, unsigned int height)
{
    if (gc == NULL) gc = &epx_default_gc;

    if (((gc->border_style & EPX_BORDER_STYLE_NBORDER) ==
	 EPX_BORDER_STYLE_NBORDER) || (gc->border_width == 0)) {
	epx_draw_ellipse(pixmap, gc, x, y, width, height, 0, 0);
    }
    else {
	epx_draw_ellipse_border(pixmap, gc, x, y, width, height, 0, 0);
    }
}

void epx_pixmap_draw_roundrect(epx_pixmap_t* pixmap, epx_gc_t* gc,
			       int x, int y,
			       unsigned int width, unsigned int height,
			       unsigned int rw, unsigned int rh)
{
    if (gc == NULL) gc = &epx_default_gc;
    unsigned int ww;
    unsigned int hh;

    ww = (width < 2*rw) ? 0  : width - 2*rw;
    hh = (height < 2*rh) ? 0 : height - 2*rh;

    if (((gc->border_style & EPX_BORDER_STYLE_NBORDER) ==
	 EPX_BORDER_STYLE_NBORDER) || (gc->border_width == 0)) {
	epx_draw_ellipse(pixmap, gc, x, y, width-ww, height-hh, ww, hh);
	if (gc->fill_style == EPX_FILL_STYLE_NONE) {
	    epx_flags_t bf = gc->border_style | EPX_LINE_STYLE_NFIRST;
	    epx_pixel_t bc = gc->border_color;
	    unsigned int bw = gc->border_width;
	    int x0 = x+rw;
	    int y0 = y+rh;
	    int x1 = x+width-rh;
	    int y1 = y+height-rh;
	    epx_draw_line(pixmap, x0, y, x1, y, bw, bf, bc);
	    epx_draw_line(pixmap, x0, y+height, x1, y+height, bw, bf, bc);
	    epx_draw_line(pixmap, x, y0, x, y1, bw, bf, bc);
	    epx_draw_line(pixmap, x+width, y0, x+width, y1, bw, bf, bc);
	}
	else {
	    epx_pixmap_draw_rectangle(pixmap, gc, x, y+rh+1, width+1, hh);
	}
    }
    else {
	epx_flags_t bf = gc->border_style | EPX_LINE_STYLE_NFIRST;
	epx_pixel_t bc = gc->border_color;
	unsigned int bw = gc->border_width;
	int x0 = x+rw;
	int y0 = y+rh;
	int x1 = x+width-rh;
	int y1 = y+height-rh;
	epx_draw_ellipse_border(pixmap, gc, x, y, width-ww, height-hh, ww, hh);
	epx_draw_rectangle(pixmap, x, y+rh+1, width+1, hh, gc->fill_style, 
			   gc->fill_color, 0, 0, bc);
	epx_draw_line(pixmap, x0, y, x1, y, bw, bf, bc);
	epx_draw_line(pixmap, x0, y+height, x1, y+height, bw, bf, bc);
	epx_draw_line(pixmap, x, y0, x, y1, bw, bf, bc);
	epx_draw_line(pixmap, x+width, y0, x+width, y1, bw, bf, bc);
    }
}

void epx_pixmap_draw_triangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
			      int x0, int y0,
			      int x1, int y1,
			      int x2, int y2)
{
    if (gc->fill_style != EPX_FILL_STYLE_NONE)
	epx_fill_triangle(pixmap, x0,y0,x1,y1,x2,y2, gc->fill_style,
			 gc->fill_color);
    else {
	epx_draw_line(pixmap, x0,y0,x1,y1, gc->line_width,
		      gc->line_style, gc->foreground_color);
	epx_draw_line(pixmap, x1,y1,x2,y2, gc->line_width,
		      gc->line_style, gc->foreground_color);
	epx_draw_line(pixmap, x2,y2,x0,y0, gc->line_width,
		      gc->line_style, gc->foreground_color);
    }
}

