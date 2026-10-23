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

/* plot 4 pixels - one in each quadrant */
static inline void plot_ellipse4(epx_pixmap_t* pixmap,
				 int xc, int yc,
				 int x, int y, int flags,
				 epx_pixel_t color,
				 unsigned int ww, unsigned int hh)
{
    epx_pixmap_put_pixel(pixmap,xc-x,hh+yc+y,flags,color);
    epx_pixmap_put_pixel(pixmap,ww+xc+x,hh+yc+y,flags,color);
    epx_pixmap_put_pixel(pixmap,xc-x,yc-y,flags,color);
    epx_pixmap_put_pixel(pixmap,ww+xc+x,yc-y,flags,color);
}

/* draw upper and lower ellipse fill line */
static inline void line_ellipse2(epx_pixmap_t* pixmap,
				 int xc, int yc,
				 int x, int y, int flags,
				 epx_pixel_t color,
				 unsigned int ww, unsigned int hh)
{
    epx_draw_line_horizontal(pixmap,xc-x,ww+xc+x,yc-y,flags,color);
    epx_draw_line_horizontal(pixmap,xc-x,ww+xc+x,hh+yc+y,flags,color);
}


// Horizontal span of a rounded rectangle [x0,x1]x[y0,y1] with corner
// radii rx,ry (corner centers at x0+rx,y0+ry etc) on row y.
// Return 0 if the row is outside the shape.
static int rrect_span(int x0, int x1, int y0, int y1,
		      int64_t rx, int64_t ry, int y, int* l, int* r)
{
    int64_t cyt, cyb, dy, inset;

    if ((y < y0) || (y > y1) || (x0 > x1))
	return 0;
    cyt = (int64_t)y0 + ry;
    cyb = (int64_t)y1 - ry;
    if (y < cyt)      dy = cyt - y;
    else if (y > cyb) dy = y - cyb;
    else              dy = 0;
    if ((dy == 0) || (ry == 0))
	inset = 0;
    else {
	double t = (double)dy / (double)ry;
	double c = (t >= 1.0) ? 0.0 : (double)rx*sqrt(1.0 - t*t);
	inset = rx - (int64_t) floor(c + 0.5);
    }
    if ((int64_t)x0 + inset > (int64_t)x1 - inset)
	return 0;
    *l = (int)(x0 + inset);
    *r = (int)(x1 - inset);
    return 1;
}

//
// Draw a rounded rectangle ring, the outer shape is [x0,x1]x[y0,y1]
// (inclusive) with corner radii rx,ry. The ring is bw pixels wide
// drawn with bf/bc, the interior is filled with ff/fc unless ff is none.
// An ellipse is the case where the corners meet. Each pixel is drawn
// once (so blending works) and only rows within the clip are scanned.
//
void epx_draw_rrect_ring(epx_pixmap_t* pixmap,
			 int x0, int y0, int x1, int y1,
			 unsigned int rx, unsigned int ry,
			 unsigned int bw, epx_flags_t bf, epx_pixel_t bc,
			 epx_flags_t ff, epx_pixel_t fc)
{
    int do_fill = (ff != EPX_FILL_STYLE_NONE);
    int64_t w = (int64_t)x1 - x0 + 1;
    int64_t h = (int64_t)y1 - y0 + 1;
    int64_t irx, iry;
    int ix0, ix1, iy0, iy1;
    int ytop, ybot, y;
    int ym;

    if ((w <= 0) || (h <= 0))
	return;
    // radii can not be more than half the size
    if ((int64_t)rx > (w-1)/2) rx = (unsigned int)((w-1)/2);
    if ((int64_t)ry > (h-1)/2) ry = (unsigned int)((h-1)/2);
    // inner shape
    if ((int64_t)bw*2 >= w || (int64_t)bw*2 >= h) {
	ix0 = 1; ix1 = 0; iy0 = 1; iy1 = 0;  // empty
    }
    else {
	ix0 = x0 + bw; ix1 = x1 - bw;
	iy0 = y0 + bw; iy1 = y1 - bw;
    }
    irx = ((int64_t)rx > (int64_t)bw) ? (int64_t)rx - bw : 0;
    iry = ((int64_t)ry > (int64_t)bw) ? (int64_t)ry - bw : 0;

    ytop = (y0 > epx_rect_top(&pixmap->clip)) ? y0 :
	epx_rect_top(&pixmap->clip);
    ybot = (y1 < epx_rect_bottom(&pixmap->clip)) ? y1 :
	epx_rect_bottom(&pixmap->clip);
    ym = (int)(((int64_t)y0 + y1) / 2);

    for (y = ytop; y <= ybot; y++) {
	int oll, orr, il, ir;
	int nl, nr;
	int inner;

	if (!rrect_span(x0, x1, y0, y1, rx, ry, y, &oll, &orr))
	    continue;
	inner = (bw == 0) ? (il = oll, ir = orr, 1) :
	    rrect_span(ix0, ix1, iy0, iy1, irx, iry, y, &il, &ir);
	// close gaps: use the inner span of the row nearer the edge
	if (inner && (bw > 0)) {
	    int yn = (y <= ym) ? y-1 : y+1;
	    if (!rrect_span(ix0, ix1, iy0, iy1, irx, iry, yn, &nl, &nr))
		inner = 0;
	    else {
		if (nl > il) il = nl;
		if (nr < ir) ir = nr;
		if (il > ir) inner = 0;
	    }
	}
	if (inner) {
	    if (il < oll) il = oll;
	    if (ir > orr) ir = orr;
	}
	if (!inner) {
	    if (bw > 0)
		epx_draw_line_horizontal(pixmap, oll, orr, y, bf, bc);
	}
	else {
	    if (il > oll)
		epx_draw_line_horizontal(pixmap, oll, il-1, y, bf, bc);
	    if (ir < orr)
		epx_draw_line_horizontal(pixmap, ir+1, orr, y, bf, bc);
	    if (do_fill)
		epx_draw_line_horizontal(pixmap, il, ir, y, ff, fc);
	}
    }
}

// Ellipse with border, the ellipse is centered at (x+a, y+b)
// where a=width/2, b=height/2 and covers x..x+2a, y..y+2b
// ww and hh stretch the ellipse in to a rounded rectangle.
void epx_draw_ellipse_border(epx_pixmap_t* pixmap, epx_gc_t* gc,
			     int x, int y,
			     unsigned int width, unsigned int height,
			     unsigned int ww, unsigned int hh)
{
    unsigned int a    = width >> 1;
    unsigned int b    = height >> 1;

    if ((a==0) || (b==0))
	return;
    epx_draw_rrect_ring(pixmap, x, y,
			(int)((int64_t)x + 2*(int64_t)a + ww),
			(int)((int64_t)y + 2*(int64_t)b + hh),
			a, b, gc->border_width,
			gc->border_style, gc->border_color,
			gc->fill_style, gc->fill_color);
}


/*
 * Draw ellipse when:
 *   border_width = 0   (or no border)
 * and
 *   no fill & line_width=1
 * or fill
 *  x^2/a^2 + y^2/b^2 = 1
 * ==
 *  x^2*b^2 + y^2*a^2 = a^2*b^2
 *  x = a*sqrt(1 - y^2/b^2)
 *
 */


void epx_draw_ellipse(epx_pixmap_t* pixmap, epx_gc_t* gc,
		      int x, int y, unsigned int width, unsigned int height,
		      unsigned int ww, unsigned int hh)
{
    unsigned int a = width  >> 1;
    unsigned int b = height >> 1;
    int xc = x + a;
    int yc = y + b;
    int do_y = 1;
    int do_fill;
    int do_aalias;
    int64_t a2 = (int64_t)a*a;
    int64_t b2 = (int64_t)b*b;
    int64_t pya = a2;
    int64_t pxb = (2*(int64_t)a-1)*b2;
    int64_t f = 0;
    int64_t fx, fxy, fy;
    int64_t ax, axy, ay;
    epx_flags_t ff = gc->fill_style;
    epx_pixel_t fc = gc->fill_color;
    epx_flags_t lf = gc->border_style;   // gc->line_style;
    epx_pixel_t lc = gc->border_color; // foreground_color;
    int xo = a;
    int yo = 0;

    if ((a==0) || (b==0))
	return;

    do_fill = (ff != EPX_FILL_STYLE_NONE);
    do_aalias = (ff & EPX_FILL_STYLE_AALIAS);  // FILL

    while((xo >= 0) && (yo <= (int)b)) {
	if ((xo == 0) && (yo == 0)) {
	    if (do_fill) {
		if (do_y) {
		    epx_pixmap_put_pixel(pixmap, xc, yc, ff, fc);
		    if (ww)
			epx_pixmap_put_pixel(pixmap, ww+xc, yc, ff, fc);
		}
	    }
	    else {
		epx_pixmap_put_pixel(pixmap, xc, yc, lf, lc);
		if (ww)
		    epx_pixmap_put_pixel(pixmap, ww+xc, yc, lf, lc);
	    }
	}
	else if (xo == 0) {
	    if (do_fill) {
		if (do_y) {
		    epx_pixmap_put_pixel(pixmap,xc,hh+yc+yo, ff, fc);
		    epx_pixmap_put_pixel(pixmap,xc,yc-yo, ff, fc);
		}
	    }
	    else {
		epx_pixmap_put_pixel(pixmap,xc,hh+yc+yo, lf, lc);
		epx_pixmap_put_pixel(pixmap,xc,yc-yo, lf, lc);
	    }
	}
	else if (yo == 0) {
	    if (do_fill) {
		if (do_y) {
		    if (do_aalias) {
			epx_pixel_t c = lc;
			double xt = a*sqrt(1-(double)(yo*yo)/(double)b2);
			epx_draw_line_horizontal(pixmap,xc-xo,ww+xc+xo,yc,ff,c);

			c.a = c.a*(1-(xo - xt));
			epx_pixmap_put_pixel(pixmap,xc-xo-1,yc,ff,c);
			epx_pixmap_put_pixel(pixmap,ww+xc+xo+1,yc,ff,c);
		    }
		    else {
			epx_draw_line_horizontal(pixmap,xc-xo,ww+xc+xo,
						 yc,ff,fc);
		    }
		}
	    }
	    else {
		epx_pixmap_put_pixel(pixmap,xc-xo,yc,lf,lc);
		epx_pixmap_put_pixel(pixmap,ww+xc+xo,yc,lf,lc);
	    }
	}
	else {
	    if (do_fill) {
		if (do_y) {
		    if (do_aalias) {
			epx_pixel_t c = fc;
			double xt = a*sqrt(1.0-(double)(yo*yo)/(double)b2);
			line_ellipse2(pixmap, xc, yc, xo, yo, ff, fc, ww, hh);
			c.a = c.a*(1 - (xo - xt));
			plot_ellipse4(pixmap, xc, yc, xo+1, yo, ff, c, ww, hh);
		    }
		    else {
			line_ellipse2(pixmap, xc, yc, xo, yo, ff, fc, ww, hh);
		    }
		}
	    }
	    else {
		if (do_aalias) {
		    if (do_y) {
			epx_pixel_t c = lc;
			double xt = a*sqrt(1.0-(double)(yo*yo)/(double)b2);

			c.a = c.a*(1-(xo-xt));
			plot_ellipse4(pixmap, xc, yc, xo, yo, lf, c, ww, hh);

			c.a = c.a*(1-((xo+1)-xt));
			plot_ellipse4(pixmap, xc, yc, xo+1, yo, lf, c, ww, hh);
		    }
		    else {
			epx_pixel_t c = lc;
			double yt = b*sqrt(1.0-(double)(xo*xo)/(double)a2);

			c.a = c.a*(1-(yo-yt));
			plot_ellipse4(pixmap, xc, yc, xo, yo, lf, c, ww, hh);

			c.a = c.a*(1-((yo+1) - yt));
			plot_ellipse4(pixmap, xc, yc, xo, yo+1, lf, c, ww, hh);
		    }
		}
		else {
		    plot_ellipse4(pixmap, xc, yc, xo, yo, lf, lc, ww, hh);
		}
	    }
	}

	fx  = f - pxb;
	fxy = f - pxb + pya;
	fy  = f + pya;
	ax  = (fx < 0) ? -fx : fx;
	axy = (fxy < 0) ? -fxy : fxy;
	ay  = (fy < 0) ? -fy : fy;

	if ((ax < axy) && (ax < ay)) {
	    do_y=0; xo--; f = fx; pxb -= (b2 + b2);
	}
	else if (axy < ay) {
	    do_y=1; xo--; yo++; f=fxy; pya += (a2 + a2); pxb -= (b2 + b2);
	}
	else {
	    do_y=1; yo++; f=fy; pya += (a2 + a2);
	}
    }
}
