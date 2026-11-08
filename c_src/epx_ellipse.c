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
 * Ellipse drawing algorithms
 *
 */
#include <math.h>
#include <stdint.h>

#include "../include/epx.h"

// A rounded rectangle in continuous coordinates: the box [x,x+w)x[y,y+h)
// with elliptic corners of radii rx,ry (an ellipse is the case with
// rx = w/2, ry = h/2). Pixel (px,py) is inside when its center
// (px+0.5,py+0.5) is.

typedef struct {
    double x, y, w, h;
    double rx, ry;
} rrect_t;

static void rrect_set(rrect_t* r, double x, double y, double w, double h,
		      double rx, double ry)
{
    r->x = x; r->y = y; r->w = w; r->h = h;
    // radii can not be more than half the size
    r->rx = (rx < 0) ? 0 : ((rx > w/2) ? w/2 : rx);
    r->ry = (ry < 0) ? 0 : ((ry > h/2) ? h/2 : ry);
}

// The left and right edge of the shape at height yc (continuous).
// Return 0 when yc is outside the shape.
static int rrect_edges(const rrect_t* r, double yc, double* lp, double* rp)
{
    double dy, inset;

    if ((r->w <= 0) || (r->h <= 0) || (yc <= r->y) || (yc >= r->y + r->h))
	return 0;
    if (yc < r->y + r->ry)
	dy = r->y + r->ry - yc;
    else if (yc > r->y + r->h - r->ry)
	dy = yc - (r->y + r->h - r->ry);
    else
	dy = 0;
    if ((dy <= 0) || (r->ry <= 0))
	inset = 0;
    else {
	double t = dy / r->ry;
	inset = r->rx - r->rx*sqrt((t >= 1.0) ? 0.0 : 1.0 - t*t);
    }
    *lp = r->x + inset;
    *rp = r->x + r->w - inset;
    return (*lp < *rp);
}

// The pixels of row py inside the shape, [*l,*r]. Return 0 if none.
static int rrect_span(const rrect_t* r, int py, int64_t* l, int64_t* rr)
{
    double le, re;
    if (!rrect_edges(r, py + 0.5, &le, &re))
	return 0;
    *l  = (int64_t) ceil(le - 0.5);
    *rr = (int64_t) floor(re - 0.5);
    return (*l <= *rr);
}

static void hline(epx_pixmap_t* pixmap, int64_t x0, int64_t x1, int y,
		  epx_flags_t flags, epx_pixel_t color)
{
    int64_t cl = epx_rect_left(&pixmap->clip);
    int64_t cr = epx_rect_right(&pixmap->clip);
    if (x0 < cl) x0 = cl;
    if (x1 > cr) x1 = cr;
    if (x0 <= x1)
	epx_draw_line_horizontal(pixmap, (int)x0, (int)x1, y, flags, color);
}

// a pixel with coverage cov (0..1), blended
static void cover_pixel(epx_pixmap_t* pixmap, int64_t x, int y,
			epx_flags_t flags, epx_pixel_t color, double cov)
{
    if ((cov <= 0) || (x < epx_rect_left(&pixmap->clip)) ||
	(x > epx_rect_right(&pixmap->clip)))
	return;
    color.a = (uint8_t) (color.a*((cov > 1.0) ? 1.0 : cov) + 0.5);
    epx_draw_line_horizontal(pixmap, (int)x, (int)x, y,
			     flags | EPX_FLAG_BLEND, color);
}

//
// Draw a rounded rectangle [x,x+w)x[y,y+h) with corner radii rx,ry: a
// ring bw pixels wide (border, bf/bc) around the interior (fill, ff/fc,
// not drawn when ff is none). The inner shape is the outer one inset by
// bw on every side, so the ring is bw pixels at the top, bottom and
// sides alike. Each pixel is drawn once (blending works) and only rows
// in the clip are scanned. With bw = 0 and EPX_FLAG_AALIAS in ff the
// edges are anti aliased.
//
void epx_draw_rrect_ring(epx_pixmap_t* pixmap,
			 int x, int y, unsigned int width, unsigned int height,
			 double rx, double ry,
			 unsigned int bw, epx_flags_t bf, epx_pixel_t bc,
			 epx_flags_t ff, epx_pixel_t fc)
{
    int do_fill = (ff != EPX_FILL_STYLE_NONE);
    int do_aalias = (bw == 0) && do_fill && (ff & EPX_FLAG_AALIAS);
    rrect_t outer, inner;
    int64_t ytop, ybot;
    int py;

    if ((width == 0) || (height == 0))
	return;
    rrect_set(&outer, x, y, width, height, rx, ry);
    if (((int64_t)2*bw >= width) || ((int64_t)2*bw >= height))
	rrect_set(&inner, 0, 0, 0, 0, 0, 0);  // all ring
    else
	rrect_set(&inner, (double)x + bw, (double)y + bw,
		  (double)width - 2.0*bw, (double)height - 2.0*bw,
		  outer.rx - bw, outer.ry - bw);

    ytop = y;
    ybot = (int64_t)y + height - 1;
    if (ytop < epx_rect_top(&pixmap->clip))    ytop = epx_rect_top(&pixmap->clip);
    if (ybot > epx_rect_bottom(&pixmap->clip)) ybot = epx_rect_bottom(&pixmap->clip);

    for (py = (int)ytop; py <= ybot; py++) {
	int64_t ol, orr, il, ir;

	if (do_aalias) {
	    double le, re;
	    int64_t fl, fr;
	    if (!rrect_edges(&outer, py + 0.5, &le, &re))
		continue;
	    fl = (int64_t) floor(le);
	    fr = (int64_t) floor(re);
	    if (fl == fr) {  // one pixel wide
		cover_pixel(pixmap, fl, py, ff, fc, re - le);
		continue;
	    }
	    cover_pixel(pixmap, fl, py, ff, fc, (double)(fl+1) - le);
	    hline(pixmap, fl+1, fr-1, py, ff, fc);
	    cover_pixel(pixmap, fr, py, ff, fc, re - (double)fr);
	    continue;
	}
	if (!rrect_span(&outer, py, &ol, &orr))
	    continue;
	if ((bw == 0) || !rrect_span(&inner, py, &il, &ir)) {
	    if (bw > 0)
		hline(pixmap, ol, orr, py, bf, bc);
	    else if (do_fill)
		hline(pixmap, ol, orr, py, ff, fc);
	    continue;
	}
	if (il < ol)  il = ol;
	if (ir > orr) ir = orr;
	if (il > ol)
	    hline(pixmap, ol, il-1, py, bf, bc);
	if (ir < orr)
	    hline(pixmap, ir+1, orr, py, bf, bc);
	if (do_fill)
	    hline(pixmap, il, ir, py, ff, fc);
    }
}

// Ellipse with a border inside the box [x,x+width)x[y,y+height),
// ww and hh stretch the ellipse in to a rounded rectangle.
void epx_draw_ellipse_border(epx_pixmap_t* pixmap, epx_gc_t* gc,
			     int x, int y,
			     unsigned int width, unsigned int height,
			     unsigned int ww, unsigned int hh)
{
    epx_draw_rrect_ring(pixmap, x, y, width + ww, height + hh,
			width/2.0, height/2.0, gc->border_width,
			gc->border_style, gc->border_color,
			gc->fill_style, gc->fill_color);
}

// Ellipse without border in the box [x,x+width)x[y,y+height): filled,
// or a one pixel outline in the border style when not filled.
void epx_draw_ellipse(epx_pixmap_t* pixmap, epx_gc_t* gc,
		      int x, int y, unsigned int width, unsigned int height,
		      unsigned int ww, unsigned int hh)
{
    if (gc->fill_style != EPX_FILL_STYLE_NONE)
	epx_draw_rrect_ring(pixmap, x, y, width + ww, height + hh,
			    width/2.0, height/2.0, 0,
			    gc->border_style, gc->border_color,
			    gc->fill_style, gc->fill_color);
    else if (gc->border_style & EPX_PAINT_STYLE_MASK)
	epx_draw_rrect_ring(pixmap, x, y, width + ww, height + hh,
			    width/2.0, height/2.0, 1,
			    gc->border_style, gc->border_color,
			    EPX_FILL_STYLE_NONE, gc->fill_color);
}
