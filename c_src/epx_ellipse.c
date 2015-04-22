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

#include "../include/epx.h"
#include <math.h>

extern void epx_draw_line_horizontal(epx_pixmap_t* pixmap,
				     int x1, int x2, int y,
				     int flags, epx_pixel_t fg);

/* plot 4 pixels - one in each quadrant */
static inline void plot_ellipse4(epx_pixmap_t* pixmap,
				 int xc, int yc,
				 int x, int y, int flags,
				 epx_pixel_t color)
{
    epx_pixmap_put_pixel(pixmap,xc-x,yc+y,flags,color);
    epx_pixmap_put_pixel(pixmap,xc+x,yc+y,flags,color);
    epx_pixmap_put_pixel(pixmap,xc-x,yc-y,flags,color);
    epx_pixmap_put_pixel(pixmap,xc+x,yc-y,flags,color);
}

/* draw upper and lower ellipse fill line */
static inline void line_ellipse2(epx_pixmap_t* pixmap,
				 int xc, int yc,
				 int x, int y, int flags,
				 epx_pixel_t color)
{
    epx_draw_line_horizontal(pixmap,xc-x,xc+x,yc-y,flags,color);
    epx_draw_line_horizontal(pixmap,xc-x,xc+x,yc+y,flags,color);
}


// General idea:
// trace two ellipses and draw border from
// outer to inner x and fill interior from inner x1, to inner x2
//  x1   x2           x3  x4
//   | B  |    FILL   | B |
//   |      BORDER        |   (inner y is done)
//
void epx_draw_ellipse_border(epx_pixmap_t* pixmap, epx_gc_t* gc,
			     int x, int y,
			     unsigned int width, unsigned int height)
{
    unsigned int a = width  >> 1;
    unsigned int b = height >> 1;
    int xc = x + a;
    int yc = y + b;
    unsigned int border_width = gc->border_width;
    epx_pixel_t bc = gc->border_color;
    epx_pixel_t fill   = gc->fill_color;
    epx_flags_t ff     = gc->fill_style;
    epx_flags_t bf     = gc->border_style;
    int do_fill = (ff != EPX_FILL_STYLE_NONE);
    // Inner circle
    int xi  = a;
    int yi  = 0;
    // FIXME: only allow a,b to be in 16 bit range to avoid overflow...
    unsigned long a2_i = a*a;
    unsigned long b2_i = b*b;
    long pya_i = a2_i;
    long pxb_i = (2*a-1)*b2_i;
    long f_i = 0;
    long fx_i, fxy_i, fy_i;
    int do_yi = 1;
    // Outer circle
    int xo  = a+border_width;
    int yo  = 0;
    unsigned long a2_o = (a+border_width)*(a+border_width);
    unsigned long b2_o = (b+border_width)*(b+border_width);
    long pya_o = a2_o;
    long pxb_o = (2*(a+border_width)-1)*b2_o;
    long f_o = 0;
    long fx_o, fxy_o, fy_o;
    int do_yo = 1;

    if ((a==0) || (b==0))
	return;

    while(xi >= 0) {
	if ((xi == 0) && (yi == 0)) {
	    if (do_fill) {
		epx_pixmap_put_pixel(pixmap, xc, yc, bf, bc);
	    }
	    else
		epx_pixmap_put_pixel(pixmap, xc, yc, bf, bc);
	}
	else if (xi == 0) {
	    if (do_fill) {
		epx_pixmap_put_pixel(pixmap,xc,yc+yi, bf, bc);
		epx_pixmap_put_pixel(pixmap,xc,yc-yi, bf, bc);
	    }
	    else {
		epx_pixmap_put_pixel(pixmap,xc,yc+yi, bf, bc);
		epx_pixmap_put_pixel(pixmap,xc,yc-yi, bf, bc);
	    }
	}
	else if (yi == 0) {
	    if (border_width > 1) {
		if (do_yi) {
		    epx_draw_line_horizontal(pixmap,xc-xo,xc-xi,yc,bf,bc);
		    epx_draw_line_horizontal(pixmap,xc+xi,xc+xo,yc,bf,bc);
		}
	    }
	    else {
		epx_pixmap_put_pixel(pixmap,xc-xo,yc,bf,bc);
		epx_pixmap_put_pixel(pixmap,xc+xo,yc,bf,bc);
	    }
	    if (do_fill) {
		if (do_yi)
		    epx_draw_line_horizontal(pixmap,xc-xi+1,xc+xi-1,yc,ff,fill);
	    }
	}
	else {
	    if (border_width > 1) {
		if (do_yi) {
		    epx_draw_line_horizontal(pixmap,xc-xo,xc-xi,yc+yi,bf,bc);
		    epx_draw_line_horizontal(pixmap,xc+xi,xc+xo,yc+yi,bf,bc);
		    epx_draw_line_horizontal(pixmap,xc-xo,xc-xi,yc-yi,bf,bc);
		    epx_draw_line_horizontal(pixmap,xc+xi,xc+xo,yc-yi,bf,bc);
		}
	    }
	    else {
		plot_ellipse4(pixmap, xc, yc, xo, yi, bf, bc);
	    }
	    if (do_fill) {
		if (do_yi) {
		    line_ellipse2(pixmap, xc, yc, xi-1, yi, ff, fill);
		}
	    }
	}

	// Update Inner circle
	fx_i  = f_i - pxb_i;
	fxy_i = f_i - pxb_i + pya_i;
	fy_i  = f_i + pya_i;

	if (epx_abs_less(fx_i,fxy_i) && epx_abs_less(fx_i,fy_i)) {
	    do_yi=0; xi--; f_i = fx_i;
	    pxb_i -= (b2_i + b2_i);
	}
	else if (epx_abs_less(fxy_i,fy_i)) {
	    do_yi=1; xi--; yi++; f_i=fxy_i;
	    pya_i += (a2_i + a2_i); pxb_i -= (b2_i + b2_i);
	}
	else {
	    do_yi=1; yi++; f_i=fy_i; pya_i += (a2_i + a2_i);
	}

	while (yo < yi) {
	    // Update Outer circle
	    fx_o  = f_o - pxb_o;
	    fxy_o = f_o - pxb_o + pya_o;
	    fy_o  = f_o + pya_o;

	    if (epx_abs_less(fx_o,fxy_o) && epx_abs_less(fx_o,fy_o)) {
		do_yo=0; xo--; f_o = fx_o;
		pxb_o -= (b2_o + b2_o);
	    }
	    else if (epx_abs_less(fxy_o,fy_o)) {
		do_yo=1; xo--; yo++; f_o=fxy_o;
		pya_o += (a2_o + a2_o); pxb_o -= (b2_o + b2_o);
	    }
	    else {
		do_yo=1; yo++; f_o=fy_o; pya_o += (a2_o + a2_o);
	    }
	}
    }

    /* Do remaining outer cirlce */
    while(xo >= 0) {
	// fprintf(stderr, "xo=%d, yo=%d, yi=%d\n", xo, yo, yi);
	if ((xo == 0) && (yo == 0)) {
	    epx_pixmap_put_pixel(pixmap, xc, yc, bf, bc);
	}
	else if (xo == 0) {
	    epx_pixmap_put_pixel(pixmap,xc,yc+yo, bf, bc);
	    epx_pixmap_put_pixel(pixmap,xc,yc-yo, bf, bc);
	}
	else if (yo == 0) {
	    if (border_width > 1) {
		if (do_yo) {
		    epx_draw_line_horizontal(pixmap,xc-xo,xc+xo,yc,bf,bc);
		}
	    }
	    else {
		epx_pixmap_put_pixel(pixmap,xc-xo,yc,bf,bc);
		epx_pixmap_put_pixel(pixmap,xc+xo,yc,bf,bc);
	    }
	}
	else {
	    if (border_width > 1) {
		if (do_yo) {
		    line_ellipse2(pixmap, xc, yc, xo, yo, bf, bc);
		}
	    }
	    else {
		plot_ellipse4(pixmap, xc, yc, xo, yo, bf, bc);
	    }
	}

	// Update Outer circle
	fx_o  = f_o - pxb_o;
	fxy_o = f_o - pxb_o + pya_o;
	fy_o  = f_o + pya_o;

	if (epx_abs_less(fx_o,fxy_o) && epx_abs_less(fx_o,fy_o)) {
	    do_yo=0; xo--; f_o = fx_o;
	    pxb_o -= (b2_o + b2_o);
	}
	else if (epx_abs_less(fxy_o,fy_o)) {
	    do_yo=1; xo--; yo++; f_o=fxy_o;
	    pya_o += (a2_o + a2_o); pxb_o -= (b2_o + b2_o);
	}
	else {
	    do_yo=1; yo++; f_o=fy_o; pya_o += (a2_o + a2_o);
	}
    }
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
		      int x, int y, unsigned int width, unsigned int height)
{
    unsigned int a = width  >> 1;
    unsigned int b = height >> 1;
    int xc = x + a;
    int yc = y + b;
    int do_y = 1;
    int do_fill;
    int do_aalias;
    // FIXME: only allow a,b to be in 16 bit range to avoid overflow...
    unsigned long a2 = a*a;
    unsigned long b2 = b*b;
    long pya = a2;
    long pxb = (2*a-1)*b2;
    long f = 0;
    long fx, fxy, fy;
    unsigned long ax, axy, ay;
    epx_flags_t ff = gc->fill_style;
    epx_flags_t lf = gc->line_style;
    epx_pixel_t fc = gc->fill_color;
    epx_pixel_t lc = gc->foreground_color;
    int xo  = a;
    int yo = 0;

    if ((a==0) || (b==0))
	return;

    do_fill = (ff != EPX_FILL_STYLE_NONE);
    do_aalias = (ff & EPX_FILL_STYLE_AALIAS);  // FILL


    while((xo >= 0) && (yo <= (int)b)) {
	if ((xo == 0) && (yo == 0)) {
	    if (do_fill) {
		if (do_y)
		    epx_pixmap_put_pixel(pixmap, xc, yc, ff, fc);
	    }
	    else
		epx_pixmap_put_pixel(pixmap, xc, yc, lf, lc);
	}
	else if (xo == 0) {
	    if (do_fill) {
		if (do_y) {
		    epx_pixmap_put_pixel(pixmap,xc,yc+yo, ff, fc);
		    epx_pixmap_put_pixel(pixmap,xc,yc-yo, ff, fc);
		}
	    }
	    else {
		epx_pixmap_put_pixel(pixmap,xc,yc+yo, lf, lc);
		epx_pixmap_put_pixel(pixmap,xc,yc-yo, lf, lc);
	    }
	}
	else if (yo == 0) {
	    if (do_fill) {
		if (do_y) {
		    if (do_aalias) {
			epx_pixel_t c = lc;
			double xt = a*sqrt(1-(double)(yo*yo)/(double)b2);
			int alpha = c.a*(xo - xt);
			epx_draw_line_horizontal(pixmap,xc-xo,xc+xo,yc,ff,c);

			c.a = (c.a-alpha);
			epx_pixmap_put_pixel(pixmap,xc-xo-1,yc,ff,c);
			epx_pixmap_put_pixel(pixmap,xc+xo+1,yc,ff,c);
		    }
		    else {
			epx_draw_line_horizontal(pixmap,xc-xo,xc+xo,yc,ff,fc);
		    }
		}
	    }
	    else {
		epx_pixmap_put_pixel(pixmap,xc-xo,yc,lf,lc);
		epx_pixmap_put_pixel(pixmap,xc+xo,yc,lf,lc);
	    }
	}
	else {
	    if (do_fill) {
		if (do_y) {
		    if (do_aalias) {
			epx_pixel_t c = fc;
			double xt = a*sqrt(1-(double)(yo*yo)/(double)b2);
			int alpha = fabs(90*(xo-xt)+37); // fc.a*(xo - xt);

			line_ellipse2(pixmap, xc, yc, xo, yo, ff, fc);

			c.a = alpha; // fc.a - alpha;
			plot_ellipse4(pixmap, xc, yc, xo+1, yo, ff, c);
		    }
		    else {
			line_ellipse2(pixmap, xc, yc, xo, yo, ff, fc);
		    }
		}
	    }
	    else {
		if (do_aalias) {
		    if (do_y) {
			epx_pixel_t c = lc;
			double xt = a*sqrt(1.0-(double)(yo*yo)/(double)b2);
			int alpha = c.a*(xo - xt);

			c.a = alpha;
			plot_ellipse4(pixmap, xc, yc, xo, yo, lf, c);

			c.a = (lc.a-alpha);
			plot_ellipse4(pixmap, xc, yc, xo+1, yo, lf, c);
		    }
		    else {
			epx_pixel_t c = lc;
			double yt = b*sqrt(1.0-(double)(xo*xo)/(double)a2);
			int alpha = c.a*(yo - yt);

			c.a = alpha;
			plot_ellipse4(pixmap, xc, yc, xo, yo, lf, c);

			c.a = (lc.a-alpha);
			plot_ellipse4(pixmap, xc, yc, xo, yo+1, lf, c);
		    }
		}
		else {
		    plot_ellipse4(pixmap, xc, yc, xo, yo, lf, lc);
		}
	    }
	}

	fx  = f - pxb;
	fxy = f - pxb + pya;
	fy  = f + pya;
	ax  = labs(fx);
	axy = labs(fxy);
	ay  = labs(fy);

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
