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

#define epx_swap_int(a,b) do { \
	int _swap_t = (a); a = (b); b = _swap_t; \
    } while(0)

#define epx_swap_int8(a,b) do { \
	uint8_t _swap_t = (a); a = (b); b = _swap_t; \
    } while(0)

#define epx_swap_float(a,b) do { \
    float _swap_t = (a); a = (b); b = _swap_t; \
    } while(0)


#define L(name,sfx) L_##name##_##sfx

#define LINE_DECL(i)			      \
    int L(x0,i);			      \
    int L(y0,i);			      \
    int L(x1,i);			      \
    int L(y1,i);			      \
    int L(dx,i);			      \
    int L(dy,i);			      \
    int L(sx,i);			      \
    int L(sy,i);			      \
    int L(wsx,i);			      \
    int L(wsy,i);			      \
    int L(err,i);			      \
    uint8_t* L(ptr,i)

#define LINE_SWAPIN(line,i) do {		    \
	L(x0,i) = (line)->p0.x;			    \
	L(y0,i) = (line)->p0.y;			    \
	L(x1,i) = (line)->p1.x;			    \
	L(y1,i) = (line)->p1.y;				    \
	L(dx,i) = (line)->delta.x;			    \
	L(dy,i) = (line)->delta.y;			    \
	L(sx,i) = (line)->s.x;				    \
	L(sy,i) = (line)->s.y;				    \
	L(wsx,i) = (line)->ws.x;			    \
	L(wsy,i) = (line)->ws.y;			    \
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
#define LINE_STEP_AXIS(err,ptr,x,dy,sx,wsx) \
    do { \
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

void epx_draw_line_horizontal(epx_pixmap_t* pic, int x1, int x2, int y, 
			  int flags, epx_pixel_t fg)
{
    int xl, xr;
    uint8_t* ptr;

    if (y < epx_rect_top(&pic->clip))
	return;
    if (y > epx_rect_bottom(&pic->clip))
	return;
    if (x1 > x2) epx_swap_int(x1,x2);
    
    xl = epx_rect_left(&pic->clip);
    if (x2 < xl)
	return;
    xr = epx_rect_right(&pic->clip);
    if (x1 > xr)
	return;

    x1 = epx_clip_range(x1, xl, xr);
    x2 = epx_clip_range(x2, xl, xr);
    ptr = EPX_PIXEL_ADDR(pic,x1,y);
    if (((flags&EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	epx_fill_row(ptr, pic->pixel_format, fg, (x2-x1)+1);
    else if (fg.a != EPX_ALPHA_TRANSPARENT)
	epx_fill_row_blend(ptr,pic->pixel_format,fg,(x2-x1)+1);
}


void epx_draw_line_vertical(epx_pixmap_t* pic, int x, int y1, int y2,
			    int flags, epx_pixel_t fg)
{
    int yt, yb;
    uint8_t* ptr;

    if (x < epx_rect_left(&pic->clip))
	return;
    if (x > epx_rect_right(&pic->clip))
	return;
    if (y1 > y2) epx_swap_int(y1,y2);

    yt = epx_rect_top(&pic->clip);
    if (y2 < yt)
	return;
    yb = epx_rect_bottom(&pic->clip);
    if (y1 > yb)
	return;

    y1 = epx_clip_range(y1, yt, yb);
    y2 = epx_clip_range(y2, yt, yb);
    ptr = EPX_PIXEL_ADDR(pic,x,y1);
    while(y1 <= y2) {
	put_apixel(ptr, pic->unpack, pic->pack, flags, fg);
	y1++;
	ptr += pic->bytes_per_row;
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


/*
 * Trace a bresenham line a also obey clipping region
 */
void break_here()
{
    fprintf(stderr, "BREAK HERE\n");
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
    
    if (L(dx,0) > L(dy,0)) {
	while(L(x1,0) != L(x0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0));
	    /* FIXME more scaling needed for luminance to be correct ! */
	    ae = (L(dx,0) == 0) ? 0 : ((a*L(err,0)) / L(dx,0));

	    if (ae < 0) {
		/* put alias point below */
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0)-L(sy,0), &pic->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-L(wsy,0), pic->unpack,pic->pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point abow */
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0)+L(sy,0), &pic->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+L(wsy,0), pic->unpack,pic->pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fa);
		}
	    }
	    else if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fg);
	    }
	}
    }
    else {
	while(L(y1,0) != L(y0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0));
	    /* FIXME more scaling needed for luminance to be correct ! */
	    ae = (L(dy,0) == 0) ? 0 : ((a*L(err,0)) / L(dy,0));
	    if (ae < 0) {
		/* put alias point left */
		if (epx_point_xy_in_rect(L(x0,0)-L(sx,0),L(y0,0), &pic->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-L(wsx,0), pic->unpack,pic->pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point right */
		if (epx_point_xy_in_rect(L(x0,0)+L(sx,0),L(y0,0), &pic->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+L(wsx,0), pic->unpack,pic->pack, flags, fa);
		}
		if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fa);
		}
	    }
	    else if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fg);
	    }
	}
    }
    LINE_SWAPOUT(line,0);
}

/* Trace a line with no antialiasing */
void trace_line_1(epx_pixmap_t* pic, epx_line_t* line, int flags, epx_pixel_t fg)
{
    LINE_DECL(0);
    LINE_SWAPIN(line,0);

//    fprintf(stderr, "LINE_START: (%d,%d,data=%p,ptr=%p)\r\n", 
//	    line->p0.x, line->p0.y, pic->data, line->ptr);

    if (L(dx,0) > L(dy,0)) {
	while(L(x1,0) != L(x0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0));
	    if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
//		fprintf(stderr, "(%d,%d,%p)\r\n", L(x0,0), L(y0,0),L(ptr,0));
		put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fg);
//		fprintf(stderr, "OK\r\n");
	    }
	}
    }
    else {
	while(L(y1,0) != L(y0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0));
	    if (epx_point_xy_in_rect(L(x0,0),L(y0,0), &pic->clip)) {
//		fprintf(stderr, "(%d,%d,%p)\r\n", L(x0,0), L(y0,0), L(ptr,0));
		put_apixel(L(ptr,0), pic->unpack,pic->pack, flags, fg);
//		fprintf(stderr, "OK\r\n");
	    }
	}
    }
//    fprintf(stderr, "LINE_END:\r\n");
    LINE_SWAPOUT(line,0);
}


/*
 *  draw horizontal line between line trace 1 and line trace 2
 *  (if possible)
 */
void trace_line_2(epx_pixmap_t* pic, epx_line_t* line1, epx_line_t* line2, 
		  int flags, epx_pixel_t fg)
{
    int swapped;
    int n;
    LINE_DECL(1);
    LINE_DECL(2);

    /* Lines are assumed to run in the same y direction */
    if ((line1->s.y != line2->s.y) ||
	(line1->delta.y == 0))
	return;

    if ((n = epx_intersect_range_length(line1->p0.y, line1->p1.y,
					line2->p0.y, line2->p1.y)) == 0)
	return;

    /* Swap in lines so that line1 starts first */
    if (((line1->p0.y <= line2->p0.y) && (line1->s.y > 0)) ||
	((line1->p0.y >= line2->p0.y) && (line1->s.y < 0))) {
	swapped = 0;
	LINE_SWAPIN(line1,1);
	LINE_SWAPIN(line2,2);
    }
    else {
	swapped = 1;
	LINE_SWAPIN(line1,2);
	LINE_SWAPIN(line2,1);
    }
#ifdef DEBUG
    printf("n=%d\n", n);
    printf("<= (x1=%d,y1=%d), (x2=%d,y2=%d), (x3=%d,y3=%d), (x4=%d,y4=%d)\n",
	   L(x0,1), L(y0,1), L(x1,1), L(y1,1),
	   L(x0,2), L(y0,2), L(x1,2), L(y1,2));
#endif
    /* step line 1 until it catch up with line 2 */
    if (L(dx,1) > L(dy,1)) {
	while(L(y0,1) != L(y0,2)) {
	    LINE_STEP(L(err,1),L(ptr,1),
		      L(x0,1),L(dx,1),L(sx,1),L(wsx,1),
		      L(y0,1),L(dy,1),L(sy,1),L(wsy,1));
	}
    }
    else {
	while(L(y0,1) != L(y0,2)) {
	    LINE_STEP(L(err,1),L(ptr,1),
		      L(y0,1),L(dy,1),L(sy,1),L(wsy,1),
		      L(x0,1),L(dx,1),L(sx,1),L(wsx,1));
	}
    }

    if (flags & EPX_FLAG_NLAST)
	n--;
#ifdef DEBUG
    printf("n=%d\n", n);
    printf("<> (x1=%d,y1=%d), (x2=%d,y2=%d), (x3=%d,y3=%d), (x4=%d,y4=%d)\n",
	   L(x0,1), L(y0,1), L(x1,1), L(y1,1),
	   L(x0,2), L(y0,2), L(x1,2), L(y1,2));
#endif
    while(n--) {
	// adjust line1 x position
	if ((L(dx,1) != 0) && (L(dx,1) > L(dy,1)) && 
	    (((L(x0,1) < L(x0,2)) && (L(sx,1) < 0)) ||
	     ((L(x0,1) > L(x0,2)) && (L(sx,1) > 0)))) {
	    /* Move to 'LAST' x position line 1 */
	    while((L(err,1) < 0) && (L(x0,1) != L(x1,1))) {
		LINE_STEP_AXIS(L(err,1),L(ptr,1),
			       L(x0,1),L(dy,1),L(sx,1),L(wsx,1));
	    }
	}

	// adjust line2 x position
	if ((L(dx,2) != 0) && (L(dx,2) > L(dy,2)) && 
	    (((L(x0,1) < L(x0,2)) && (L(sx,2) > 0)) ||
	     ((L(x0,1) > L(x0,2)) && (L(sx,2) < 0)))) {
	    /* Move to 'LAST' x position line 2 */
	    while((L(err,2) < 0) && (L(x0,2) != L(x1,2))) {
		LINE_STEP_AXIS(L(err,2),L(ptr,2),
			       L(x0,2),L(dy,2),L(sx,2),L(wsx,2));
	    }
	}

	epx_draw_line_horizontal(pic, L(x0,1), L(x0,2), L(y0,1), flags, fg);

	/* step line 2 until y is moved (it wont move if dy=0) */
	if (L(dy,2) != 0) {
	    if (L(dx,2) > L(dy,2)) {
		while(L(y0,1) == L(y0,2)) {
		    if (L(x0,1) == L(x1,1))
			break;
		    LINE_STEP(L(err,2),L(ptr,2),
			      L(x0,2),L(dx,2),L(sx,2),L(wsx,2),
			      L(y0,2),L(dy,2),L(sy,2),L(wsy,2));
		}
	    }
	    else {
		while(L(y0,1) == L(y0,2)) {
		    LINE_STEP(L(err,2),L(ptr,2),
			      L(y0,2),L(dy,2),L(sy,2),L(wsy,2),
			      L(x0,2),L(dx,2),L(sx,2),L(wsx,2));
		}
	    }
	}

	// step line1
	if (L(dx,1) > L(dy,1)) {
	    while(L(y0,1) != L(y0,2)) {
		if (L(x0,1) == L(x1,1))
		    break;
		LINE_STEP(L(err,1),L(ptr,1),
			  L(x0,1),L(dx,1),L(sx,1),L(wsx,1),
			  L(y0,1),L(dy,1),L(sy,1),L(wsy,1));
	    }
	}
	else {
	    while(L(y0,1) != L(y0,2)) {
		LINE_STEP(L(err,1),L(ptr,1),
			  L(y0,1),L(dy,1),L(sy,1),L(wsy,1),
			  L(x0,1),L(dx,1),L(sx,1),L(wsx,1));
	    }
	}
    }
#ifdef DEBUG
    printf("=> (x1=%d,y1=%d), (x2=%d,y2=%d), (x3=%d,y3=%d), (x4=%d,y4=%d)\n",
	   L(x0,1), L(y0,1), L(x1,1), L(y1,1),
	   L(x0,2), L(y0,2), L(x1,2), L(y1,2));
#endif
    if (swapped) {
	LINE_SWAPOUT(line1,2);
	LINE_SWAPOUT(line2,1);
    }
    else {
	LINE_SWAPOUT(line1,1);
	LINE_SWAPOUT(line2,2);
    }
}


void epx_draw_line_plain(epx_pixmap_t* pic, int x1, int y1, int x2, int y2,
			 int flags, epx_pixel_t fg)
{
    epx_line_t line;
    set_p0(pic, &line, x1, y1);
    set_p1(pic, &line, x2, y2);
    if (flags & EPX_FLAG_AALIAS)
	trace_aalias_line_1(pic, &line, flags, fg);
    else
	trace_line_1(pic, &line, flags, fg);
}

void draw_line_twin(epx_pixmap_t* pic, 
		    int x1, int y1, int x2, int y2,
		    int x3, int y3, int x4, int y4,
		    int flags, epx_pixel_t fg)
{
    epx_line_t line1;
    epx_line_t line2;

    if (y2 < y1) { epx_swap_int(x1,x2); epx_swap_int(y1,y2);  }
    set_p0(pic, &line1, x1, y1);
    set_p1(pic, &line1, x2, y2);

    if (y4 < y3) { epx_swap_int(x3,x4); epx_swap_int(y3,y4);  }
    set_p0(pic, &line2, x3, y3);
    set_p1(pic, &line2, x4, y4);
    trace_line_2(pic, &line1, &line2, flags, fg);
}


void fill_triangle(epx_pixmap_t* pic, 
		   int x0, int y0,
		   int x1, int y1,
		   int x2, int y2,
		   int flags, epx_pixel_t fg)
{
    epx_line_t line1;
    epx_line_t line2;

    if (y2 < y1) { epx_swap_int(x1,x2); epx_swap_int(y1,y2);  }
    if (y1 < y0) { epx_swap_int(x0,x1); epx_swap_int(y0,y1);  }
    if (x2 < x1) { epx_swap_int(x1,x2); epx_swap_int(y1,y2);  }

    set_p0(pic, &line1, x0, y0);
    set_p1(pic, &line1, x1, y1);

    set_p0(pic, &line2, x0, y0);
    set_p1(pic, &line2, x2, y2);

    trace_line_2(pic, &line1, &line2, flags|EPX_FLAG_NLAST, fg);

    if (epx_point_is_equal(&line1.p0, &line1.p1)) {
	if (epx_point_is_equal(&line2.p0, &line2.p1))
	    return;
	set_p1(pic, &line1, x2, y2);
	trace_line_2(pic, &line1, &line2, flags, fg);
    }
    else if (epx_point_is_equal(&line2.p0, &line2.p1)) {
	set_p1(pic, &line2, x1, y1);
	trace_line_2(pic, &line1, &line2, flags, fg);
    }
    else {
	// fprintf(stderr, "NO LINE DONE\n");
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
		       int flags, epx_pixel_t p)
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
			 int flags, epx_pixel_t fg)
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
		draw_dline(ptr, pic->unpack,pic->pack, tk,
			   -d0, d1, kt, ks, kd, ku, kv,
			   x0, y0, sx, sy, sxw, syw,
			   x2, x3, y2, y3, line_width,flags,fg);
		if (epx_in_range(y0-sy, y2, y3))
		    put_wu_apixel(ptr-syw,pic->unpack,pic->pack,w^255,flags,ag,wl);
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
		    draw_dline(ptr, pic->unpack,pic->pack, tk,
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
		draw_dline(ptr, pic->unpack,pic->pack, tk,
			   -d0, d1, kt, ks, kd, ku, kv,
			   x0, y0, sx, sy, sxw, syw,
			   x2, x3, y2, y3, line_width,flags,fg);
		if (epx_in_range(x0-sx,x2,x3))
		    put_wu_apixel(ptr-sxw,pic->unpack,pic->pack,w^255,flags,ag,wl);
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
		    draw_dline(ptr, pic->unpack,pic->pack, tk,
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


void epx_draw_line(epx_pixmap_t* pic,int x0, int y0, int x1, int y1, 
		   unsigned int line_width,int flags, epx_pixel_t p)
{
    if (line_width <= 1) {
/*	if (flags & EFLAG_AALIAS)
	    draw_line_aalias(pic, x0, y0, x1, y1, flags, p); 
	 else */
	    epx_draw_line_plain(pic, x0, y0, x1, y1, flags, p);
    }
    else
	epx_draw_line_thick(pic, x0, y0, x1, y1, line_width, flags, p);
}
