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
// Basic data structures for size and form
//
#ifndef __EPX_GEOMETRY_H__
#define __EPX_GEOMETRY_H__

#ifdef debug
#include <assert.h>
#define EPX_ASSERT_RECTANGLE(r, x, y, w, h) \
    assert(epx_point_xy_in_rect((x), (y), r) &&	\
	   epx_point_xy_in_rect((x+w-1), (y+h-1), r));
#else
#define EPX_ASSERT_RECTANGLE(r, x, y, w, h)
#endif


typedef struct _epx_point_t {
    int x;
    int y;
} epx_point_t;

typedef struct _epx_dimension_t {
    unsigned int width;
    unsigned int height;
} epx_dimension_t;

typedef struct epx_rect_t {
    epx_point_t xy;
    epx_dimension_t wh;
} epx_rect_t;


static inline int epx_max_int(int a, int b)
{
    return (a>b) ? a : b;
}

static inline int epx_min_int(int a, int b)
{
    return (a<b) ? a : b;
}

static inline float epx_max_float(float a, float b)
{
    return (a>b) ? a : b;
}

static inline float epx_min_float(float a, float b)
{
    return (a<b) ? a : b;
}

/* check if |a|<|b| */
static inline int epx_abs_less(int a, int b)
{
    return (((a<0) ? -a : a) < ((b<0) ? -b : b));
}

static inline int epx_clip_range(int a, int low, int high)
{
    if (a < low) return low;
    if (a > high) return high;
    return a;
}

// Return length of the Intersection of range [a1,a2] with [b1,b2]
// Assertion a1 <= a2, b1 <= b2 !!!
static inline int epx_intersect_range_length(int a1, int a2, int b1, int b2)
{
    a1 = epx_max_int(a1,b1);
    a2 = epx_min_int(a2,b2);
    if (a1 > a2) return 0;
    return (a2-a1)+1;
}

// Intersect range [a1,a2] with [b1,b2] and put the result in [c1,c2]
// and return the length of the result. Zero is returned if there
// is not intersection
// Assertion a1 <= a2, b1 <= b2 !!!
static inline int epx_intersect_range(int a1, int a2, int b1, int b2,
				      int* c1, int* c2)
{
    a1 = epx_max_int(a1,b1);
    a2 = epx_min_int(a2,b2);
    if (c1) *c1 = a1;
    if (c2) *c2 = a2;
    if (a1 > a2)
	return 0;
    return (a2-a1)+1;
}

// Intersect two segments [a,a+al-1] and [b,b+bl-1]
// put the result the intersection start point in c and 
// return its length. 
//
static inline int epx_intersect_segments(int a, int al, int b, int bl, int* c)
{
    int ct = epx_max_int(a,b);
    b = epx_min_int(a+al-1,b+bl-1);
    if (c) *c = ct;
    if (ct > b) return 0;
    return (b-ct)+1;
}

static inline int epx_in_range(int a, int low, int high)
{
    return ((a >= low) && (a <= high));
}

static inline void epx_point_set(epx_point_t* p, int x, int y)
{
    p->x = x;
    p->y = y;
}

static inline int epx_point_is_equal(epx_point_t* p, epx_point_t* q)
{
    return ((p->x == q->x) && (p->y == q->y));
}


static inline void epx_dimension_set(epx_dimension_t* d, 
				     unsigned int width, unsigned int height)
{
    d->width  = width;
    d->height = height;
}

static inline int epx_dimension_is_equal(epx_dimension_t* d, epx_dimension_t* e)
{
    return ((d->width == e->width) && (d->height == e->height));
}


static inline void epx_rect_set(epx_rect_t* r, int x, int y, 
				unsigned int width, unsigned int height)
{
    epx_point_set(&r->xy, x, y);
    epx_dimension_set(&r->wh, width, height);
}

static inline int epx_rect_is_equal(epx_rect_t* a, epx_rect_t* b)
{
    return epx_point_is_equal(&a->xy, &b->xy) && 
	epx_dimension_is_equal(&a->wh, &b->wh);
}

static inline int epx_rect_is_empty(epx_rect_t* r)
{
    return !r->wh.height || !r->wh.width;
}

static inline void epx_rect_set_xy(epx_rect_t* r, int x1, int y1,
				   int x2, int y2)
{
    if (x1 < x2) { 
	r->xy.x = x1; 
	r->wh.width = (x2-x1)+1;
    }
    else {
	r->xy.x = x2; 
	r->wh.width = (x1-x2)+1;
    }
    if (y1 < y2) { 
	r->xy.y = y1; 
	r->wh.height = (y2-y1)+1;
    }
    else {
	r->xy.y = y2; 
	r->wh.height = (y1-y2)+1;
    }
}

static inline void epx_rect_empty(epx_rect_t* r)
{
    epx_rect_set(r, 0, 0, 0, 0);
}

static inline int epx_rect_left(const epx_rect_t* r)
{
    return r->xy.x;
}

static inline int epx_rect_right(const epx_rect_t* r)
{
    return r->xy.x+r->wh.width-1;
}

static inline int epx_rect_top(const epx_rect_t* r)
{
    return r->xy.y;
}

static inline int epx_rect_bottom(const epx_rect_t* r)
{
    return r->xy.y+r->wh.height-1;
}

static inline unsigned int epx_rect_width(const epx_rect_t* r)
{
    return r->wh.width;
}

static inline unsigned int epx_rect_height(const epx_rect_t* r)
{
    return r->wh.height;
}

// Intersect rect's a and b put the result in c return 0 if c is empty
static inline int epx_rect_intersect(const epx_rect_t* a, const epx_rect_t* b,
				     epx_rect_t* r)
{
    int left, right, top, bottom;

    left  = epx_max_int(epx_rect_left(a), epx_rect_left(b));
    right = epx_min_int(epx_rect_right(a), epx_rect_right(b));
    if (left > right) goto empty;

    top    = epx_max_int(epx_rect_top(a), epx_rect_top(b));
    bottom = epx_min_int(epx_rect_bottom(a), epx_rect_bottom(b));
    if (top > bottom) goto empty;

    epx_point_set(&r->xy, left, top);
    epx_dimension_set(&r->wh, (right-left)+1, (bottom-top)+1);
    return 1;
empty:
    epx_rect_empty(r);
    return 0;
}

static inline void epx_rect_union(const epx_rect_t* a, const epx_rect_t* b, 
				  epx_rect_t* r)
{
    int left, right, top, bottom;

    left   = epx_min_int(epx_rect_left(a), epx_rect_left(b));
    right  = epx_max_int(epx_rect_right(a), epx_rect_right(b));
    top    = epx_min_int(epx_rect_top(a), epx_rect_top(b));
    bottom = epx_max_int(epx_rect_bottom(a), epx_rect_bottom(b));

    epx_point_set(&r->xy, left, top);
    epx_dimension_set(&r->wh, (right-left)+1, (bottom-top)+1);
}

// Check if a is a proper sub-rect of b (e.g the intersection(a,b) = a)
static inline int epx_rect_is_subrect(const epx_rect_t* a, const epx_rect_t* b)
{
    return (epx_rect_left(a) >= epx_rect_left(b)) &&
	(epx_rect_right(a) <= epx_rect_right(b)) &&
	(epx_rect_top(a) >= epx_rect_top(b)) &&
	(epx_rect_bottom(a) <= epx_rect_bottom(b));
}

// Form a union of epx_rect and A epx_point
static inline void epx_rect_add_point(const epx_rect_t* a,const epx_point_t* p,
				      epx_rect_t* r)
{
    int left, right, top, bottom;

    left   = epx_min_int(epx_rect_left(a),  p->x);
    right  = epx_max_int(epx_rect_right(a), p->x+1);
    top    = epx_min_int(epx_rect_top(a),   p->y);
    bottom = epx_max_int(epx_rect_bottom(a), p->y+1);

    epx_point_set(&r->xy, left, top);
    epx_dimension_set(&r->wh, (right-left)+1, (bottom-top)+1);
}

static inline int epx_point_xy_in_rect(int x, int y, epx_rect_t* r)
{
    return ((x >= epx_rect_left(r)) && (x <= epx_rect_right(r)) &&
	    (y >= epx_rect_top(r)) && (y <= epx_rect_bottom(r)));
}

static inline int epx_point_in_rect(const epx_point_t* p, epx_rect_t* r)
{
    return epx_point_xy_in_rect(p->x, p->y, r);
}


#endif
