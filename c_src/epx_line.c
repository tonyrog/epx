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
void trace_aalias_line_1(epx_pixmap_t* pic, epx_line_t* line, int flags,
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
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0)-line->s.y, &pic->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-line->ws.y, pic->func.unpack,
			       pic->func.pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0),pic->func.unpack,pic->func.pack,
			       flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point above */
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0)+line->s.y, &pic->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+line->ws.y, pic->func.unpack,
			       pic->func.pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pic->func.unpack,pic->func.pack,
			       flags, fa);
		}
	    }
	    else if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pic->func.unpack,pic->func.pack,
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
		if (epx_point_xy_in_rect(L(x0,0)-line->s.x,L(y0,0), &pic->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-line->ws.x,pic->func.unpack,
			       pic->func.pack,flags,fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0),pic->func.unpack,pic->func.pack,
			       flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point right */
		if (epx_point_xy_in_rect(L(x0,0)+line->s.x,L(y0,0), &pic->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+line->ws.x, pic->func.unpack,
			       pic->func.pack,flags,fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pic->func.unpack,pic->func.pack,
			       flags, fa);
		}
	    }
	    else if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pic->func.unpack,pic->func.pack,
			   flags, fg);
	    }
	}
    }
    LINE_SWAPOUT(line,0);
}

/* Trace a line with no antialiasing */
void trace_line_1(epx_pixmap_t* pic, epx_line_t* line,
		  int flags, epx_pixel_t fg)
{
    LINE_DECL(0);
    LINE_SWAPIN(line,0);

    if (line->delta.x > line->delta.y) {
	while(L(x1,0) != L(x0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(x0,0),line->delta.x,line->s.x,line->ws.x,
		      L(y0,0),line->delta.y,line->s.y,line->ws.y);
	    if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pic->func.unpack,pic->func.pack,
			   flags, fg);
	    }
	}
    }
    else {
	while(L(y1,0) != L(y0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(y0,0),line->delta.y,line->s.y,line->ws.y,
		      L(x0,0),line->delta.x,line->s.x,line->ws.x);
	    if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pic->func.unpack,pic->func.pack,
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


static inline uint8_t wu_blend(uint8_t w, uint8_t fg, uint8_t bg)
{
    return (bg > fg) ? epx_blend(w, bg, fg) : epx_blend(w, fg, bg);
}


static inline void put_wu_apixel(uint8_t* ptr,
				 epx_pixel_unpack_t unpack,
				 epx_pixel_pack_t pack,
				 uint8_t w,int flags, epx_pixel_t fg,
				 uint8_t lfg)
{
    epx_pixel_t bg;
    uint8_t lbg;

    bg  = unpack(ptr);
    lbg = epx_pixel_luminance(bg);
    if (lfg > lbg)
	w = w ^ 255;
    fg.r = wu_blend(w, fg.r, bg.r);
    fg.g = wu_blend(w, fg.g, bg.g);
    fg.b = wu_blend(w, fg.b, bg.b);

    if (((flags & EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	pack(fg, ptr);
    else if (fg.a != EPX_ALPHA_TRANSPARENT) {
	bg = epx_pixel_blend(fg.a, fg, bg); // maybe do wu_blend & blend ?
	pack(bg, ptr);
    }
}

/*
 * draw_dline:  orthongonal delta line
 *
 */

static void draw_dline(uint8_t* ptr,
		       epx_pixel_unpack_t unpack,
		       epx_pixel_pack_t pack,
		       float tk,
		       int d0, int d1,
		       int kt, int ks, int kd, int ku, int kv,
		       int x0, int y0,
		       int sx, int sy,
		       int sxw, int syw,
		       int x2, int x3,
		       int y2, int y3,
		       int w,
		       epx_flags_t flags, epx_pixel_t p)
{
    (void) w;
    if (ku > kv) {
	while (d0 <= tk) {
	    if (epx_in_range(x0, x2, x3) && epx_in_range(y0, y2, y3))
		put_apixel(ptr,unpack,pack,flags,p);
	    if (d1 < kt) {
		d1 += kv;
		d0 += ku;
	    }
	    else {
		x0 -= sx; ptr -= sxw;
		d1 += kd;
		d0 += ks;
	    }
	    y0 += sy; ptr += syw;
	}
    }
    else {
	while (d0 <= tk) {
	    if (epx_in_range(x0, x2, x3) && epx_in_range(y0, y2, y3))
		put_apixel(ptr,unpack,pack,flags,p);
	    if (d1 < kt) {
		d1 += ku;
		d0 += kv;
	    }
	    else {
		y0 -= sy; ptr -= syw;
		d1 += kd;
		d0 += ks;
	    }
	    x0 += sx; ptr += sxw;
	}
    }
}


/*
 *  Draw line where line_width > 1
 */

void epx_draw_line_thick(epx_pixmap_t* pic,
			 int x0, int y0,
			 int x1, int y1, int line_width,
			 epx_flags_t flags, epx_pixel_t fg)
{
    int dy = y1 - y0;
    int dx = x1 - x0;
    int ku,kv;
    int sx, sy, sxw, syw;
    int x2, x3, y2, y3; /* used for clipping */
    uint8_t* ptr;
    epx_pixel_t ag = fg;
    uint8_t  wl = epx_pixel_luminance(fg);


    x2 = epx_rect_left(&pic->clip);
    y2 = epx_rect_top(&pic->clip);
    x3 = epx_rect_right(&pic->clip);
    y3 = epx_rect_bottom(&pic->clip);

    if (dy < 0) { dy=-dy; sy=-1; syw=-pic->bytes_per_row; }
    else { sy=1; syw=pic->bytes_per_row; }

    if (dx < 0) { dx=-dx; sx=-1; sxw=-pic->bytes_per_pixel; }
    else { sx=1; sxw=pic->bytes_per_pixel; }

    if (dx == 0) {
	int height;

	if (flags&EPX_LINE_STYLE_NFIRST) { y0 += sy; dy--; }
	if (flags&EPX_LINE_STYLE_NLAST)  { y1 -= sy; dy--; }
	x1 = x0 + line_width;
	// FIXME mega fat lines (or tiny clip area)
	if (!epx_in_range(x0,x2,x3) && !epx_in_range(x1,x2,x3))
	    return;
	x0 = epx_clip_range(x0, x2, x3);
	x1 = epx_clip_range(x1, x2, x3);
	line_width = x1-x0;  // new line_width after clip
	y0 = epx_clip_range(y0, y2, y3);
	y1 = epx_clip_range(y1, y2, y3);
	if (y0 <= y1) {
	    height = (y1-y0)+1;
	    ptr = EPX_PIXEL_ADDR(pic,x0,y0);
	}
	else {
	    height = (y0-y1)+1;
	    ptr = EPX_PIXEL_ADDR(pic,x0,y1);
	}

	if (((flags&EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	    epx_fill_area(ptr,pic->bytes_per_row,pic->pixel_format,
			    fg,line_width,height);
	else if (fg.a != EPX_ALPHA_TRANSPARENT)
	    epx_fill_area_blend(ptr,pic->bytes_per_row,pic->pixel_format,
				fg,line_width, height);
	return;
    }

    if (dy == 0) {
	int width;

	if (flags&EPX_LINE_STYLE_NFIRST) { x0 += sx; }
	if (flags&EPX_LINE_STYLE_NLAST)  { x1 -= sx; }
	y1 = y0 + line_width;
	if (!epx_in_range(y0,y2,y3) && !epx_in_range(y1,y2,y3))
	    return;
	y0 = epx_clip_range(y0, y2, y3);
	y1 = epx_clip_range(y1, y2, y3);
	line_width = (y1-y0);
	x0 = epx_clip_range(x0, x2, x3);
	x1 = epx_clip_range(x1, x2, x3);
	if (x0 <= x1) {
	    width = (x1-x0)+1;
	    ptr = EPX_PIXEL_ADDR(pic,x0,y0);
	}
	else {
	    width = (x0-x1)+1;
	    ptr = EPX_PIXEL_ADDR(pic,x1,y0);
	}
	if (sx < 0) ptr -= ((line_width-1)*pic->bytes_per_row);

	if (((flags&EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	    epx_fill_area(ptr,pic->bytes_per_row,pic->pixel_format,
			  fg,width,line_width);
	else if (fg.a != EPX_ALPHA_TRANSPARENT)
	    epx_fill_area_blend(ptr,pic->bytes_per_row,pic->pixel_format,
				fg,width,line_width);
	return;
    }

    ptr = EPX_PIXEL_ADDR(pic,x0,y0);

    ku = dx+dx;
    kv = dy+dy;
    if (dx > dy) {
	uint16_t eacc = 0;
	uint16_t eadj = ((uint32_t) dy << 16) / (uint32_t) dx;
	int kd = kv-ku;  // 2(dy - dx)
	int ks = kv+ku;  // 2(dy + dx)
	int kt = dx-kv;  // dx - 2dy
	int d0=0, d1=0;
	float thickvar = 0.0;
	float tk = 0.0;
	int i=0;

	tk = 2*(line_width+thickvar*i/dx)*sqrt(dx*dx + dy*dy);

	if (flags&EPX_LINE_STYLE_NLAST) dx--;

	while(i < dx) {
	    uint8_t w;
	    eacc += eadj;
	    w = eacc >> 8;
	    if (((i != 0) || !(flags&EPX_LINE_STYLE_NFIRST))) {
		draw_dline(ptr, pic->func.unpack,pic->func.pack, tk,
			   -d0, d1, kt, ks, kd, ku, kv,
			   x0, y0, sx, sy, sxw, syw,
			   x2, x3, y2, y3, line_width,flags,fg);
		if (epx_in_range(y0-sy, y2, y3))
		    put_wu_apixel(ptr-syw,pic->func.unpack,pic->func.pack,
				  w^255,flags,ag,wl);
	    }
	    if (d0 >= kt) {
		d0 -= ku;
		if (d1 < kt) {
		    y0 += sy; ptr += syw;
		    d1 += kv;
		}
		else {
		    y0 += sy; ptr += syw;
		    d1 += kd;
		    draw_dline(ptr, pic->func.unpack,pic->func.pack, tk,
			       -d0, d1, kt, ks, kd, ku, kv,
			       x0, y0, sx, sy, sxw, syw,
			       x2, x3, y2, y3, line_width,flags,fg);
		}
	    }
	    x0 += sx; ptr += sxw;
	    d0 += kv;
	    i++;
	}
    }
    else {
	uint16_t eacc = 0;
	uint16_t eadj = ((uint32_t) dx << 16) / (uint32_t) dy;

	int kd = ku-kv;  // 2(dx - dy)
	int ks = ku+kv;  // 2(dx + dy)
	int kt = dy-ku;  // dy - 2dx
	int d0=0, d1=0;
	float thickvar = 0.0;
	float tk = 0.0;
	int i=0;

	tk = 2*(line_width+thickvar*i/dx)*sqrt(dx*dx + dy*dy);

	if (flags&EPX_LINE_STYLE_NLAST) dy--;

	while(i < dy) {
	    uint8_t w;
	    eacc += eadj;
	    w = eacc >> 8;
	    if (((i != 0) || !(flags&EPX_LINE_STYLE_NFIRST))) {
		draw_dline(ptr, pic->func.unpack,pic->func.pack, tk,
			   -d0, d1, kt, ks, kd, ku, kv,
			   x0, y0, sx, sy, sxw, syw,
			   x2, x3, y2, y3, line_width,flags,fg);
		if (epx_in_range(x0-sx,x2,x3))
		    put_wu_apixel(ptr-sxw,pic->func.unpack,pic->func.pack,
				  w^255,flags,ag,wl);
	    }
	    if (d0 >= kt) {
		d0 -= kv;
		if (d1 < kt) {
		    x0 += sx; ptr += sxw;
		    d1 += ku;
		}
		else {
		    x0 += sx; ptr += sxw;
		    d1 += kd;
		    draw_dline(ptr, pic->func.unpack,pic->func.pack, tk,
			       -d0, d1, kt, ks, kd, ku, kv,
			       x0, y0, sx, sy, sxw, syw,
			       x2, x3, y2, y3,line_width,flags,fg);
		}
	    }
	    y0 += sy; ptr += syw;
	    d0 += ku;
	    i++;
	}
    }
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
    else {
	if (y0 == y1) {
	    if (x0 > x1) epx_swap_int(x1,x0);
	    epx_pixmap_fill_area(pixmap, x0, y0, x1-x0+1, line_width,
				 p, flags);
	}
	else if (x0 == x1) {
	    if (y0 > y1) epx_swap_int(y1,y0);
	    epx_pixmap_fill_area(pixmap, x0, y0, line_width, y1-y0+1,
				 p, flags);
	}
	else
	    epx_draw_line_thick(pixmap, x0, y0, x1, y1, line_width, flags, p);
    }
}
