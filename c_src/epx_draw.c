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
#include "../include/epx_colors.h"


#define no_border(ff) \
    (((ff) & EPX_BORDER_STYLE_NBORDER) == EPX_BORDER_STYLE_NBORDER)

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

static inline void fill_rect(epx_pixmap_t* pixmap,
			     int x, int y,
			     unsigned int width, unsigned int height,
			     epx_flags_t ff, epx_pixel_t fc)
{
    epx_rect_t r, r0;
    uint8_t* ptr;
    
    epx_rect_set(&r,x,y,width,height);
    if (!epx_rect_intersect(&r, &pixmap->clip, &r0))
	return;
    ptr = EPX_PIXEL_ADDR(pixmap,r0.xy.x,r0.xy.y);
    if (((ff & EPX_FLAG_BLEND)==0) || (fc.a == EPX_ALPHA_OPAQUE))
	epx_fill_area(ptr,pixmap->bytes_per_row,pixmap->pixel_format,fc,
		      r0.wh.width,r0.wh.height);
    else if (fc.a != EPX_ALPHA_TRANSPARENT)
	epx_fill_area_blend(ptr,pixmap->bytes_per_row,
			    pixmap->pixel_format,fc,
			    r0.wh.width,r0.wh.height);
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
    epx_rect_t r;

    epx_rect_set(&r,x,y,width,height);

    if ((ff == EPX_FILL_STYLE_NONE) && (bw <= 1))
	;
    else if (bw > 0) {
	switch(bf & EPX_BORDER_LOCATION_MASK) {
	case EPX_BORDER_LOCATION_INSIDE:
	    break;
	case EPX_BORDER_LOCATION_CENTER:
	    r.xy.x -= (bw/2-1);
	    r.xy.y -= (bw/2-1);
	    r.wh.width  += bw;
	    r.wh.height += bw;
	    break;
	case EPX_BORDER_LOCATION_OUTSIDE:
	default:
	    r.xy.x -= (bw-1);
	    r.xy.y -= (bw-1);
	    r.wh.width += 2*bw;
	    r.wh.height += 2*bw;
	    break;
	}
    }
	
    if (!epx_rect_intersect(&r, &pixmap->clip, NULL))
	return;

    x = r.xy.x;
    y = r.xy.y;
    width = r.wh.width;
    height = r.wh.height;

    if ((ff == EPX_FILL_STYLE_NONE) && (bw <= 1)) {
	bf |= EPX_LINE_STYLE_NFIRST;
	if (!(bf & EPX_BORDER_STYLE_NTOP))
	    epx_draw_line_horizontal(pixmap,x,x+width-1,y,bf,bc);
	if (!(bf & EPX_BORDER_STYLE_NBOTTOM))
	    epx_draw_line_horizontal(pixmap,x,x+width-1,y+height-1,bf,bc);
	if (!(bf & EPX_BORDER_STYLE_NLEFT))
	    epx_draw_line_vertical(pixmap,x,y+1,y+height-3,bf,bc);
	if (!(bf & EPX_BORDER_STYLE_NRIGHT))
	    epx_draw_line_vertical(pixmap,x+width-1,y+1,y+height-3,bf,bc);
    }
    else if (bw > 0) {
	if (!(bf & EPX_BORDER_STYLE_NTOP))
	    fill_rect(pixmap, x, y,         width,  bw, bf, bc);
	if (!(bf & EPX_BORDER_STYLE_NBOTTOM))
	    fill_rect(pixmap, x, y+height-bw,    width,  bw, bf, bc);	
	if (!(bf & EPX_BORDER_STYLE_NLEFT))
	    fill_rect(pixmap, x,      y+bw, bw, height-2*bw, bf, bc);
	if (!(bf & EPX_BORDER_STYLE_NRIGHT))
	    fill_rect(pixmap, x+width-bw, y+bw, bw, height-2*bw, bf, bc);
    }
    if (ff != EPX_FILL_STYLE_NONE) {
	if (bw > 0)
	    fill_rect(pixmap, x+bw, y+bw, width-2*bw, height-2*bw, ff, fc);
	else
	    fill_rect(pixmap, x, y, width, height, ff, fc);
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

    if (no_border(gc->border_style) || (gc->border_width == 0)) {
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
    unsigned int ww;
    unsigned int hh;
    epx_flags_t ff, bf;
    epx_pixel_t fc, bc;
    unsigned int bw;
    epx_rect_t r;
    
    if (gc == NULL) gc = &epx_default_gc;

    epx_rect_set(&r,x,y,width,height);

    bf = gc->border_style;
    bw = gc->border_width;
    ff = gc->fill_style;

    if ((ff == EPX_FILL_STYLE_NONE) && (bw <= 1))
	;
    else if (bw > 0) {
	switch(bf & EPX_BORDER_LOCATION_MASK) {
	case EPX_BORDER_LOCATION_INSIDE:
	    break;
	case EPX_BORDER_LOCATION_CENTER:
	    r.xy.x -= bw/2;
	    r.xy.y -= bw/2;
	    r.wh.width  += bw;
	    r.wh.height += bw;
	    break;
	case EPX_BORDER_LOCATION_OUTSIDE:
	default:
	    r.xy.x -= bw;
	    r.xy.y -= bw;
	    r.wh.width += 2*bw;
	    r.wh.height += 2*bw;
	    break;
	}
    }

    if (!epx_rect_intersect(&r, &pixmap->clip, NULL))
	return;

    x = r.xy.x;
    y = r.xy.y;
    width  = r.wh.width;
    height = r.wh.height;

    ww = (width < 2*(rw+bw)) ? 0 : width - 2*(rw+bw) - 1;
    hh = (height < 2*(rh+bw)) ? 0 : height - 2*(rh+bw) - 1;

    if (bw == 0)
	epx_draw_ellipse(pixmap, gc, x, y, 2*rw, 2*rh, ww, hh-1);
    else
	epx_draw_ellipse_border(pixmap, gc, x, y, 2*(rw+bw), 2*(rh+bw), ww, hh-1);
    
    bc = gc->border_color;
    fc = gc->fill_color;

    if ((ff == EPX_FILL_STYLE_NONE) && (bw <= 1)) {
	// top
	epx_draw_line_horizontal(pixmap,x+rw,x+width-rw-1,y,bf,bc);
	// bottom
	epx_draw_line_horizontal(pixmap,x+rw,x+width-rw-1,y+height-hh-1,bf,bc);
	// left
	epx_draw_line_vertical(pixmap,x,y+rh,y+height-hh-1,bf,bc);
	// right
	epx_draw_line_vertical(pixmap,x+width-rw-1,y+rh,y+height-hh-1,bf,bc);
    }
    else {
	if (bw > 0) {
	    // top border
	    fill_rect(pixmap, x+bw+rw, y, ww, bw, bf, bc);
	    // bottom border
	    fill_rect(pixmap, x+bw+rw, y+height-bw, ww, bw, bf, bc);

	    // left border
	    fill_rect(pixmap, x, y+bw+rh, bw, hh, bf, bc);
	    // right border
	    fill_rect(pixmap, x+width-bw, y+bw+rh, bw, hh, bf, bc);
	}
	if (ff != EPX_FILL_STYLE_NONE) {
	    // fill top
	    fill_rect(pixmap, x+bw+rw, y+bw, ww, rh, ff, fc);

	    // fill bottom
	    fill_rect(pixmap, x+bw+rw, y+height-bw-rh, ww, rh, ff, fc);
	    
	    // fill interior
	    fill_rect(pixmap, x+bw, y+bw+rh, width-2*bw, hh, ff, fc);
	}
    }
}

void epx_pixmap_draw_triangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
			      int x0, int y0,
			      int x1, int y1,
			      int x2, int y2)
{
    if (gc == NULL) gc = &epx_default_gc;
    
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

