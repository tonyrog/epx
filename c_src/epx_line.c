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
 * Line segment drawing algorithms
 *
 */
#include <memory.h>
#include <math.h>
#include "../include/epx_pixel.h"
#include "../include/epx_pixmap.h"
#include "../include/epx_gc.h"

#define L(name,sfx) L_##name##_##sfx

#define LINE_DECL(i)			      \
    int L(x0,i);			      \
    int L(y0,i);			      \
    int L(x1,i);			      \
    int L(y1,i);			      \
    int L(err,i);			      \
    uint8_t* L(ptr,i)

#define LINE_SWAPIN(line,i) do {			    \
	L(x0,i) = (line)->p0.x;				    \
	L(y0,i) = (line)->p0.y;				    \
	L(x1,i) = (line)->p1.x;				    \
	L(y1,i) = (line)->p1.y;				    \
	L(err,i) = (line)->err;				    \
	L(ptr,i) = (line)->ptr;				    \
    } while(0)

#define LINE_SWAPOUT(line,i) do {		\
    (line)->p0.x = L(x0,i);			\
    (line)->p0.y = L(y0,i);			\
    (line)->err  = L(err,i);			\
    (line)->ptr  = L(ptr,i);			\
    } while(0)

/* step on x axes until it's time to move on */
#define LINE_STEP_AXIS(err,ptr,x,dy,sx,wsx)	\
    do {						\
	ptr += (wsx);					\
	x += (sx);					\
	err += (dy);					\
    } while(0)

/* one line step, run both ways just reverse arguments */
#define LINE_STEP(err,ptr,x,dx,sx,wsx,y,dy,sy,wsy)	\
    do {						\
	if ((err) >= 0) {				\
	    ptr += (wsy);				\
	    y   += (sy);				\
	    err -= (dx);				\
	}						\
	LINE_STEP_AXIS(err,ptr,x,dy,sx,wsx);		\
    } while(0)


typedef struct {
    epx_point_t p0;     /* current/start point */
    epx_point_t p1;     /* Stop  point */
    epx_point_t s;      /* x, y increament +1 -1 or 0 */
    epx_point_t ws;     /* ptr increament bytesPerPixel/bytesPerRow */
    epx_point_t delta;  /* 2*|dx|, 2*|dy| */
    int      err;    /* 2*error distance from line */
    uint8_t* ptr;  /* current pixel address */
} epx_line_t;

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

void epx_draw_line_horizontal(epx_pixmap_t* pixmap, int x1, int x2, int y,
			      epx_flags_t flags, epx_pixel_t fg)
{
    int xl, xr;
    uint8_t* ptr;

    if (y < epx_rect_top(&pixmap->clip)) return;
    if (y > epx_rect_bottom(&pixmap->clip)) return;
    if (x1 > x2) epx_swap_int(x1,x2);

    if (x2 < (xl = epx_rect_left(&pixmap->clip))) return;
    if (x1 > (xr = epx_rect_right(&pixmap->clip))) return;

    x1 = epx_clip_range(x1, xl, xr);
    x2 = epx_clip_range(x2, xl, xr);
    ptr = EPX_PIXEL_ADDR(pixmap,x1,y);
    if (((flags&EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	epx_fill_row(ptr, pixmap->pixel_format, fg, (x2-x1)+1);
    else if (fg.a != EPX_ALPHA_TRANSPARENT)
	epx_fill_row_blend(ptr,pixmap->pixel_format,fg,(x2-x1)+1);
}


void epx_draw_line_vertical(epx_pixmap_t* pixmap, int x, int y1, int y2,
			    epx_flags_t flags, epx_pixel_t fg)
{
    int yt, yb;
    uint8_t* ptr;

    if (x < epx_rect_left(&pixmap->clip)) return;
    if (x > epx_rect_right(&pixmap->clip)) return;
    if (y1 > y2) epx_swap_int(y1,y2);

    if (y2 < (yt = epx_rect_top(&pixmap->clip))) return;
    if (y1 > (yb = epx_rect_bottom(&pixmap->clip))) return;

    y1 = epx_clip_range(y1, yt, yb);
    y2 = epx_clip_range(y2, yt, yb);
    ptr = EPX_PIXEL_ADDR(pixmap,x,y1);
    while(y1 <= y2) {
	put_apixel(ptr, pixmap->func.unpack, pixmap->func.pack, flags, fg);
	y1++;
	ptr += pixmap->bytes_per_row;
    }
}


/* Setup inital point and pixel address */
static void set_p0(epx_pixmap_t* pixmap, epx_line_t* line, int x, int y)
{
    memset(line, 0, sizeof(epx_line_t));
    epx_point_set(&line->p0, x, y);
    line->ptr = EPX_PIXEL_ADDR(pixmap,x,y);
}

/* Setup next point, step and delta parameters */
static void set_p1(epx_pixmap_t* pixmap, epx_line_t* line, int x, int y)
{
    int dy = y - line->p0.y;
    int dx = x - line->p0.x;

    epx_point_set(&line->p1, x, y);

    if (dy < 0) {
	line->delta.y  = (dy = -dy << 1);
	line->s.y      = -1;
	line->ws.y     = -pixmap->bytes_per_row;
    }
    else {
	line->delta.y = (dy = dy << 1);
	line->s.y     = 1;
	line->ws.y    = pixmap->bytes_per_row;
    }

    if (dx < 0) {
	line->delta.x = (dx = -dx << 1);
	line->s.x     = -1;
	line->ws.x    = -pixmap->bytes_per_pixel;
    }
    else {
	line->delta.x = (dx = dx<<1);
	line->s.x     = 1;
	line->ws.x    = pixmap->bytes_per_pixel;
    }
    if (dx > dy)
	line->err = dy - (dx>>1);
    else
	line->err = dx - (dy>>1);
}

/* Trace line with alpha blending and antialiasing
 * The L = luminance
 *     L(fg,1.0) = luminance of the foreground pixel with alpha=1
 *     L(fg,0.5) + L(fg,0.5) = L(fg,1.0) ?
 *
 *
 */
void trace_aalias_line_1(epx_pixmap_t* pixmap, epx_line_t* line, int flags,
			 epx_pixel_t fg)
{
    epx_pixel_t fa = fg;
    int a  = fa.a;
    int ae;

    LINE_DECL(0);
    LINE_SWAPIN(line,0);

    if (line->delta.x > line->delta.y) {
	while(L(x1,0) != L(x0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(x0,0),line->delta.x,line->s.x,line->ws.x,
		      L(y0,0),line->delta.y,line->s.y,line->ws.y);
	    /* FIXME more scaling needed for luminance to be correct ! */
	    ae = (line->delta.x == 0) ? 0 : ((a*L(err,0)) / line->delta.x);

	    if (ae < 0) {
		/* put alias point below */
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0)-line->s.y, &pixmap->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-line->ws.y, pixmap->func.unpack,
			       pixmap->func.pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0),pixmap->func.unpack,pixmap->func.pack,
			       flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point above */
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0)+line->s.y, &pixmap->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+line->ws.y, pixmap->func.unpack,
			       pixmap->func.pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pixmap->func.unpack,pixmap->func.pack,
			       flags, fa);
		}
	    }
	    else if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		put_apixel(L(ptr,0), pixmap->func.unpack,pixmap->func.pack,
			   flags, fg);
	    }
	}
    }
    else {
	while(L(y1,0) != L(y0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(y0,0),line->delta.y,line->s.y,line->ws.y,
		      L(x0,0),line->delta.x,line->s.x,line->ws.x);
	    /* FIXME more scaling needed for luminance to be correct ! */
	    ae = (line->delta.y == 0) ? 0 : ((a*L(err,0)) / line->delta.y);
	    if (ae < 0) {
		/* put alias point left */
		if (epx_point_xy_in_rect(L(x0,0)-line->s.x,L(y0,0), &pixmap->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-line->ws.x,pixmap->func.unpack,
			       pixmap->func.pack,flags,fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0),pixmap->func.unpack,pixmap->func.pack,
			       flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point right */
		if (epx_point_xy_in_rect(L(x0,0)+line->s.x,L(y0,0), &pixmap->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+line->ws.x, pixmap->func.unpack,
			       pixmap->func.pack,flags,fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pixmap->func.unpack,pixmap->func.pack,
			       flags, fa);
		}
	    }
	    else if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		put_apixel(L(ptr,0), pixmap->func.unpack,pixmap->func.pack,
			   flags, fg);
	    }
	}
    }
    LINE_SWAPOUT(line,0);
}

/* Trace a line with no antialiasing */
void trace_line_1(epx_pixmap_t* pixmap, epx_line_t* line,
		  int flags, epx_pixel_t fg)
{
    LINE_DECL(0);
    LINE_SWAPIN(line,0);

    if (line->delta.x > line->delta.y) {
	while(L(x1,0) != L(x0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(x0,0),line->delta.x,line->s.x,line->ws.x,
		      L(y0,0),line->delta.y,line->s.y,line->ws.y);
	    if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		put_apixel(L(ptr,0), pixmap->func.unpack,pixmap->func.pack,
			   flags, fg);
	    }
	}
    }
    else {
	while(L(y1,0) != L(y0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(y0,0),line->delta.y,line->s.y,line->ws.y,
		      L(x0,0),line->delta.x,line->s.x,line->ws.x);
	    if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pixmap->clip)) {
		put_apixel(L(ptr,0), pixmap->func.unpack,pixmap->func.pack,
			   flags, fg);
	    }
	}
    }
    LINE_SWAPOUT(line,0);
}


/*
 *  draw horizontal line between line trace 1 and line trace 2
 *  Line1->p0.y <= Line2->p0.y
 *  Line1->p1.y <= Line2->p1.y
 */
void trace_line_2(epx_pixmap_t* pixmap, epx_line_t* line1, epx_line_t* line2,
		  int flags, epx_pixel_t fg)
{
    int n;
    LINE_DECL(1);
    LINE_DECL(2);
    (void) L_y1_1;
    (void) L_y1_2;

    if ((line1->delta.y == 0) ||
	(line2->delta.y == 0))
	return;

    LINE_SWAPIN(line1,1);
    LINE_SWAPIN(line2,2);

    // step line 1 until it catch up with line 2
    if (line1->delta.x > line1->delta.y) {
	while(L(y0,1) != L(y0,2)) {
	    LINE_STEP(L(err,1),L(ptr,1),
		      L(x0,1),line1->delta.x,line1->s.x,line1->ws.x,
		      L(y0,1),line1->delta.y,line1->s.y,line1->ws.y);
	}
    }
    else {
	while(L(y0,1) != L(y0,2)) {
	    LINE_STEP(L(err,1),L(ptr,1),
		      L(y0,1),line1->delta.y,line1->s.y,line1->ws.y,
		      L(x0,1),line1->delta.x,line1->s.x,line1->ws.x);
	}
    }

    n = (L(y1,1) - L(y0,1)) + 1;

    if (flags & EPX_FLAG_NLAST)
	n--;
    while(n--) {
	// adjust line1 x position
	if ((line1->delta.x != 0) && (line1->delta.x > line1->delta.y) &&
	    (((L(x0,1) < L(x0,2)) && (line1->s.x < 0)) ||
	     ((L(x0,1) > L(x0,2)) && (line1->s.x > 0)))) {
	    /* Move to 'LAST' x position line 1 */
	    while((L(err,1) < 0) && (L(x0,1) != L(x1,1))) {
		LINE_STEP_AXIS(L(err,1),L(ptr,1),
			       L(x0,1),line1->delta.y,line1->s.x,line1->ws.x);
	    }
	}

	// adjust line2 x position
	if ((line2->delta.x != 0) && (line2->delta.x > line2->delta.y) &&
	    (((L(x0,1) < L(x0,2)) && (line2->s.x > 0)) ||
	     ((L(x0,1) > L(x0,2)) && (line2->s.x < 0)))) {
	    /* Move to 'LAST' x position line 2 */
	    while((L(err,2) < 0) && (L(x0,2) != L(x1,2))) {
		LINE_STEP_AXIS(L(err,2),L(ptr,2),
			       L(x0,2),line2->delta.y,line2->s.x,line2->ws.x);
	    }
	}

	epx_draw_line_horizontal(pixmap, L(x0,1), L(x0,2), L(y0,1), flags, fg);

	if (line2->delta.x > line2->delta.y) {  // step line 2 until y is moved
	    while(L(y0,1) == L(y0,2)) {
		LINE_STEP(L(err,2),L(ptr,2),
			  L(x0,2),line2->delta.x,line2->s.x,line2->ws.x,
			  L(y0,2),line2->delta.y,line2->s.y,line2->ws.y);
	    }
	}
	else {
	    while(L(y0,1) == L(y0,2)) {
		LINE_STEP(L(err,2),L(ptr,2),
			  L(y0,2),line2->delta.y,line2->s.y,line2->ws.y,
			  L(x0,2),line2->delta.x,line2->s.x,line2->ws.x);
	    }
	}

	if (line1->delta.x > line1->delta.y) { 	// step line1
	    while(L(y0,1) != L(y0,2)) {
		if (L(x0,1) == L(x1,1)) break;
		LINE_STEP(L(err,1),L(ptr,1),
			  L(x0,1),line1->delta.x,line1->s.x,line1->ws.x,
			  L(y0,1),line1->delta.y,line1->s.y,line1->ws.y);
	    }
	}
	else {
	    while(L(y0,1) != L(y0,2)) {
		LINE_STEP(L(err,1),L(ptr,1),
			  L(y0,1),line1->delta.y,line1->s.y,line1->ws.y,
			  L(x0,1),line1->delta.x,line1->s.x,line1->ws.x);
	    }
	}
    }
    LINE_SWAPOUT(line1,1);
    LINE_SWAPOUT(line2,2);
}


void epx_draw_line_plain(epx_pixmap_t* pixmap, int x1, int y1, int x2, int y2,
			 int flags, epx_pixel_t fg)
{
    epx_line_t line;
    set_p0(pixmap, &line, x1, y1);
    set_p1(pixmap, &line, x2, y2);
    if (flags & EPX_FLAG_AALIAS)
	trace_aalias_line_1(pixmap, &line, flags, fg);
    else
	trace_line_1(pixmap, &line, flags, fg);
}

void draw_line_twin(epx_pixmap_t* pixmap,
		    int x1, int y1, int x2, int y2,
		    int x3, int y3, int x4, int y4,
		    int flags, epx_pixel_t fg)
{
    epx_line_t line1;
    epx_line_t line2;

    if (y2 < y1) { epx_swap_int(x1,x2); epx_swap_int(y1,y2);  }
    set_p0(pixmap, &line1, x1, y1);
    set_p1(pixmap, &line1, x2, y2);

    if (y4 < y3) { epx_swap_int(x3,x4); epx_swap_int(y3,y4);  }
    set_p0(pixmap, &line2, x3, y3);
    set_p1(pixmap, &line2, x4, y4);

    if (y2 < y4)
	trace_line_2(pixmap, &line1, &line2, flags, fg);
    else
	trace_line_2(pixmap, &line2, &line1, flags, fg);
}


void epx_fill_triangle(epx_pixmap_t* pixmap,
		       int x0, int y0,
		       int x1, int y1,
		       int x2, int y2,
		       epx_flags_t flags, epx_pixel_t fg)
{
    epx_line_t line1;
    epx_line_t line2;

    // sort so that y0 <= y1 <= y2
    // fixme: swap edge/vertex flags
    if (y0 > y1) { epx_swap_int(x0,x1); epx_swap_int(y0,y1);  }
    if (y1 > y2) { epx_swap_int(x1,x2); epx_swap_int(y1,y2);  }
    if (y0 > y1) { epx_swap_int(x0,x1); epx_swap_int(y0,y1);  }

    if (y0 == y1) {
	set_p0(pixmap, &line1, x0, y0);
	set_p1(pixmap, &line1, x2, y2);
	set_p0(pixmap, &line2, x1, y1);
	set_p1(pixmap, &line2, x2, y2);
	trace_line_2(pixmap, &line1, &line2, flags, fg);
    }
    else if (y1 == y2) {
	set_p0(pixmap, &line1, x0, y0);
	set_p1(pixmap, &line1, x1, y1);
	set_p0(pixmap, &line2, x0, y0);
	set_p1(pixmap, &line2, x2, y2);
	trace_line_2(pixmap, &line1, &line2, flags, fg);
    }
    else {
	set_p0(pixmap, &line1, x0, y0);
	set_p1(pixmap, &line1, x1, y1);

	set_p0(pixmap, &line2, x0, y0);
	set_p1(pixmap, &line2, x2, y2);

	trace_line_2(pixmap, &line1, &line2, flags, fg);
	set_p1(pixmap, &line1, x2, y2);
	trace_line_2(pixmap, &line1, &line2, flags, fg);
    }
}


// Fill a convex polygon given in pixel center coordinates,
// pixel (x,y) is drawn when its center is inside, using half open
// intervals [min,max) in both x and y. Clipped to the pixmap clip.
static void fill_convex(epx_pixmap_t* pixmap, const double* px,
			const double* py, int n,
			epx_flags_t flags, epx_pixel_t fg)
{
    double ymin = py[0], ymax = py[0];
    double ys, ye;
    int i, y, y_first, y_last;

    for (i = 1; i < n; i++) {
	if (py[i] < ymin) ymin = py[i];
	if (py[i] > ymax) ymax = py[i];
    }
    ys = ceil(ymin);
    ye = ceil(ymax) - 1;
    if (ys < epx_rect_top(&pixmap->clip)) ys = epx_rect_top(&pixmap->clip);
    if (ye > epx_rect_bottom(&pixmap->clip))
	ye = epx_rect_bottom(&pixmap->clip);
    if (ys > ye)
	return;
    y_first = (int) ys;
    y_last  = (int) ye;

    for (y = y_first; y <= y_last; y++) {
	double xl = HUGE_VAL, xr = -HUGE_VAL;
	double xa, xb;
	for (i = 0; i < n; i++) {
	    int j = (i+1) % n;
	    double y0 = py[i], y1 = py[j];
	    double x;
	    if (y0 == y1) {
		if (y == y0) {
		    if (px[i] < xl) xl = px[i];
		    if (px[i] > xr) xr = px[i];
		    if (px[j] < xl) xl = px[j];
		    if (px[j] > xr) xr = px[j];
		}
		continue;
	    }
	    if ((y < ((y0 < y1) ? y0 : y1)) || (y > ((y0 < y1) ? y1 : y0)))
		continue;
	    x = px[i] + (y - y0)*(px[j] - px[i])/(y1 - y0);
	    if (x < xl) xl = x;
	    if (x > xr) xr = x;
	}
	if (xl > xr)
	    continue;
	xa = ceil(xl);
	xb = ceil(xr) - 1;
	if (xa < epx_rect_left(&pixmap->clip))
	    xa = epx_rect_left(&pixmap->clip);
	if (xb > epx_rect_right(&pixmap->clip))
	    xb = epx_rect_right(&pixmap->clip);
	if (xa > xb)
	    continue;
	epx_draw_line_horizontal(pixmap, (int)xa, (int)xb, y, flags, fg);
    }
}

// Draw a line line_width wide, centered on the line (x0,y0)-(x1,y1)
// The ends are extended by half a pixel so that the end points are
// covered (unless nfirst/nlast are given), each pixel is drawn once.
void epx_draw_line_thick(epx_pixmap_t* pixmap,
			 int x0, int y0,
			 int x1, int y1, int line_width,
			 epx_flags_t flags, epx_pixel_t fg)
{
    double dx = (double)x1 - (double)x0;
    double dy = (double)y1 - (double)y0;
    double len = sqrt(dx*dx + dy*dy);
    double ux, uy;  // unit direction
    double nx, ny;  // normal * half width
    double es, ee;  // start/end extension
    double ax, ay, bx, by;
    double px[4], py[4];

    if (line_width <= 0)
	return;
    if (len == 0.0) { ux = 1.0; uy = 0.0; }
    else { ux = dx/len; uy = dy/len; }
    nx = -uy*line_width/2.0;
    ny =  ux*line_width/2.0;
    es = (flags & EPX_LINE_STYLE_NFIRST) ? -0.5 : 0.5;
    ee = (flags & EPX_LINE_STYLE_NLAST)  ? -0.5 : 0.5;

    ax = x0 - ux*es; ay = y0 - uy*es;
    bx = x1 + ux*ee; by = y1 + uy*ee;

    px[0] = ax + nx; py[0] = ay + ny;
    px[1] = bx + nx; py[1] = by + ny;
    px[2] = bx - nx; py[2] = by - ny;
    px[3] = ax - nx; py[3] = ay - ny;
    fill_convex(pixmap, px, py, 4, flags, fg);
}


void epx_draw_line(epx_pixmap_t* pixmap,int x0, int y0, int x1, int y1,
		   unsigned int line_width,epx_flags_t flags, epx_pixel_t p)
{
    if (line_width <= 1) {
	if (y0 == y1)
	    epx_draw_line_horizontal(pixmap, x0, x1, y1, flags, p);
	else if (x0 == x1)
	    epx_draw_line_vertical(pixmap, x0, y0, y1, flags, p);
	else
	    epx_draw_line_plain(pixmap, x0, y0, x1, y1, flags, p);
    }
    else
	epx_draw_line_thick(pixmap, x0, y0, x1, y1, line_width, flags, p);
}
