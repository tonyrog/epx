/***************************************************************************
 *
 * Copyright (C) 2007 - 2021, <tony@rogvall.se>
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

#define _GNU_SOURCE
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "../include/epx_object.h"
#include "../include/epx_poly.h"
#include "../include/epx_draw.h"
#include "../include/epx_colors.h"

#define FZERO (10E-7)

#define epx_swap_int(a,b) do {			 \
	int _swap_t = (a); a = (b); b = _swap_t; \
    } while(0)

#define epx_swap_ptr(a,b) do {			   \
	void* _swap_t = (a); a = (b); b = _swap_t; \
    } while(0)


static inline int is_fzero(float a)
{
    return (fabs(a) < FZERO);
}

static inline int get_edge_orig_v0(epx_edge_t* ep)
{
    return (ep->flags & EDGE_FLAG_SWAP) ? ep->v1 : ep->v0;
}

static inline int get_edge_orig_v1(epx_edge_t* ep)
{
    return (ep->flags & EDGE_FLAG_SWAP) ? ep->v0 : ep->v1;
}

static inline int get_edge_orig_x0(epx_edge_t* ep, epx_pos_t* pos_table)
{
    return pos_table[get_edge_orig_v0(ep)].x;
}

static inline int get_edge_orig_y0(epx_edge_t* ep, epx_pos_t* pos_table)
{
    return pos_table[get_edge_orig_v0(ep)].x;
}

static void set_edge(epx_edge_t* ep, epx_vertex_t* vs, int v0, int v1)
{
    float dx = vs[v1].x - vs[v0].x;
    float dy = vs[v1].y - vs[v0].y;
    int flags = 0;

    if (is_fzero(dy))
	flags |= EDGE_FLAG_HORIZONTAL;
    ep->v0 = v0;
    ep->v1 = v1;
    ep->k  = is_fzero(dy) ? 0.0 : dx/dy;
    ep->flags = flags;
}

static inline int icmp(int a, int b)
{
    if (a < b) return -1;
    else if (a > b) return 1;
    else return 0;
}

static void map_edge(epx_edge_t* ep, epx_pos_t* pt, int* v0_ptr, int* v1_ptr)
{
    int v0, v1;
    int dx, dy;
    int flags = 0;

    if (ep->flags & EDGE_FLAG_SWAP) {
	v0 = ep->v1;
	v1 = ep->v0;
    }
    else {
	v0 = ep->v0;
	v1 = ep->v1;
    }
    // pass original v0,v1 to caller
    *v0_ptr = v0;
    *v1_ptr = v1;
    
    switch(icmp(pt[v0].y, pt[v1].y)) {
    case 1:
	epx_swap_int(v0,v1);
	flags |= EDGE_FLAG_SWAP;  // keep track on orignal v0,v1!
	break;
    case 0:
	flags |= EDGE_FLAG_HORIZONTAL;
	break;
    default:
	break;
    }
    dy  = pt[v1].y - pt[v0].y; // >= 0
    dx  = pt[v1].x - pt[v0].x;
    ep->v0 = v0;
    ep->v1 = v1;
    ep->k  = (dy==0) ? 0.0 : (float)dx/(float)dy;
    ep->y = pt[v0].y;  // restart y
    ep->flags = flags;
}

static inline int ydir(int y0, int y1)
{
    if (y0 < y1) return EDGE_FLAG_DOWN;
    else if (y0 > y1) return EDGE_FLAG_UP;
    else return 0;
}

// transform poly vertices/slope
// FIXME: check if CTM is changed!
static void transform_edges(epx_poly_t* poly, epx_t2d_t* t)
{
    int i;
    epx_edge_t* ep;
    epx_vertex_t* vp;
    epx_pos_t* pt;
    int n = (int) poly->nes;
    int dir, d0;
    int v0, v1;
    
    // transform all vertices
    vp = poly->vertex_table;
    pt = poly->pos_table;
    for (i = 0; i < (int)poly->nvs; i++) {
	float xf = vp[i].x;
	float yf = vp[i].y;
	epx_t2d_transform_xy(t, &xf, &yf);
	pt[i].x  = (int)roundf(xf);
	pt[i].y  = (int)roundf(yf);
    }
    // remap all edges
    ep = poly->edge_table;
    map_edge(ep, pt, &v0, &v1);
    ep++;
    d0 = dir = ydir(pt[v0].y, pt[v1].y);
    for (i = 1; i < n; i++) {
	int d;
	map_edge(ep, pt, &v0, &v1);
	d = ydir(pt[v0].y, pt[v1].y);
	if (d != dir)
	    ep->flags |= dir;
	dir = d;
	ep++;
    }
    ep = poly->edge_table;
    if (d0 != dir)
	ep->flags |= dir;
}

void epx_poly_init(epx_poly_t* poly)
{
    EPX_OBJECT_INIT(poly, EPX_POLY_TYPE);
    poly->nes = 0;
    poly->nvs = 0;
    poly->xmin = 0.0;
    poly->xmax = 0.0;
    poly->ymin = 0.0;
    poly->ymax = 0.0;    
    poly->vertex_table = NULL;
    poly->pos_table    = NULL;    
    poly->edge_table   = NULL;
    poly->es           = NULL;
    poly->as           = NULL;
    poly->last_ctm     = NULL;
    poly->last_vsn     = 0;
}

void epx_poly_destroy(epx_poly_t* poly)
{
    epx_object_unref(poly);
}

void EPX_POLY_TYPE_RELEASE(void* arg)
{
    epx_poly_t* poly = (epx_poly_t*) arg;

    EPX_DBGFMT_MEM("EPX_POLY_TYPE_RELEASE: %p", arg);
    free(poly->as);
    free(poly->es);
    free(poly->edge_table);
    free(poly->pos_table);
    free(poly->vertex_table);
    if (poly->on_heap) free(poly);
}

epx_poly_t* epx_poly_create()
{
    epx_poly_t* poly;

    if (!(poly = (epx_poly_t*) malloc(sizeof(epx_poly_t))))
	return 0;
    epx_poly_init(poly);
    poly->on_heap = 1;
    poly->refc = 1;
    return poly;
}

// setup all edges  (once)
// vp must be allocated with malloc!!!
// fixme: add/delete vertex!
int epx_poly_set(epx_poly_t* poly, epx_vertex_t* vp, size_t n)
{
    float ymin = 0.0, ymax = 0.0;
    float xmin = 0.0, xmax = 0.0;

    DEBUGF("epx_poly_set n=%lu", n);

    poly->nvs = n;
    poly->nes = n;
    poly->pos_table = realloc(poly->pos_table, n*sizeof(epx_pos_t));
    if (poly->vertex_table) free(poly->vertex_table);
    poly->vertex_table = vp;
    poly->edge_table = realloc(poly->edge_table, n*sizeof(epx_edge_t));
    poly->es = realloc(poly->es, n*sizeof(epx_edge_t*));
    poly->as = realloc(poly->as, n*sizeof(epx_edge_t*));

    if (n > 0) {
	epx_edge_t*  et;
	epx_edge_t** es;
	int i;
	
	et = poly->edge_table;
	es = poly->es;

	xmax = xmin = vp[0].x;
	ymax = ymin = vp[0].y;

	for (i = 1; i < (int)n; i++) {
	    float x = vp[i].x;
	    float y = vp[i].y;
	    if (y < ymin) ymin = y;
	    if (y > ymax) ymax = y;
	    if (x < xmin) xmin = x;
	    if (x > xmax) xmax = x;
	}
	for (i = 1; i < (int)n; i++) {
	    set_edge(et, vp, i-1, i);
	    *es++ = et++;
	}
	set_edge(et, vp, n-1, 0);
	*es = et;
    }
    poly->ymin = ymin;
    poly->ymax = ymax;
    poly->xmin = xmin;
    poly->xmax = xmax;    
    return 0;
}

// an inclusive pixel span [x0,x1] on a scan line
typedef struct {
    int x0;
    int x1;
} poly_span_t;

static int span_cmp(const void* a, const void* b)
{
    const poly_span_t* sa = (const poly_span_t*) a;
    const poly_span_t* sb = (const poly_span_t*) b;
    return (sa->x0 < sb->x0) ? -1 : ((sa->x0 > sb->x0) ? 1 : 0);
}

static int float_cmp(const void* a, const void* b)
{
    float fa = *((const float*) a);
    float fb = *((const float*) b);
    return (fa < fb) ? -1 : ((fa > fb) ? 1 : 0);
}

// x position on edge v0-v1 at y (y0 != y1), y is clamped to the edge
static inline float edge_x(epx_pos_t* p0, epx_pos_t* p1, float y)
{
    float ylo = (p0->y < p1->y) ? p0->y : p1->y;
    float yhi = (p0->y < p1->y) ? p1->y : p0->y;
    if (y < ylo) y = ylo;
    if (y > yhi) y = yhi;
    return p0->x + (y - p0->y)*(float)(p1->x - p0->x)/(float)(p1->y - p0->y);
}

// sort and merge spans and draw each pixel once
static void fill_spans(epx_pixmap_t* pixmap, poly_span_t* sp, int n, int y,
		       epx_flags_t flags, epx_pixel_t color)
{
    int i;
    poly_span_t cur;

    if (n == 0)
	return;
    qsort(sp, n, sizeof(poly_span_t), span_cmp);
    cur = sp[0];
    for (i = 1; i < n; i++) {
	if (sp[i].x0 <= cur.x1+1) {
	    if (sp[i].x1 > cur.x1)
		cur.x1 = sp[i].x1;
	}
	else {
	    epx_draw_line_horizontal(pixmap, cur.x0, cur.x1, y, flags, color);
	    cur = sp[i];
	}
    }
    epx_draw_line_horizontal(pixmap, cur.x0, cur.x1, y, flags, color);
}

// Scan line polynomial algorithm
void epx_poly_draw(epx_poly_t* poly,
		   epx_pixmap_t* pixmap, epx_gc_t* gc,
		   int x_offs, int y_offs)
{
    int ymin, ymax;
    
    if (gc == NULL)
	gc = &epx_default_gc;

    if (poly->nes == 0)
	return;

    if ((pixmap->ctm != poly->last_ctm) ||
	(pixmap->ctm->version != poly->last_vsn)) {
	transform_edges(poly, pixmap->ctm);
	poly->last_ctm = pixmap->ctm;
	poly->last_vsn = pixmap->ctm->version;
    }

    {
	size_t i;
	ymin = ymax = poly->pos_table[0].y;
	for (i = 1; i < poly->nvs; i++) {
	    if (poly->pos_table[i].y < ymin) ymin = poly->pos_table[i].y;
	    if (poly->pos_table[i].y > ymax) ymax = poly->pos_table[i].y;
	}
    }
    DEBUGF("draw ymin=%d, ymax=%d", ymin, ymax);

    if (gc->fill_style != EPX_FILL_STYLE_NONE) {
	// Even-odd scan line fill, the interior is sampled on integer
	// scan lines using half open edges [ylo,yhi) and the edges them
	// self are added to the spans so the boundary is always covered.
	// Spans are merged so every pixel is drawn once (blending).
	int n = (int) poly->nes;
	epx_pos_t* pt = poly->pos_table;
	float* xs;
	poly_span_t* sp;
	int y;

	xs = (float*) malloc(n*sizeof(float));
	sp = (poly_span_t*) malloc(2*n*sizeof(poly_span_t));
	if ((xs == NULL) || (sp == NULL)) {
	    free(xs);
	    free(sp);
	    return;
	}
	for (y = ymin; y <= ymax; y++) {
	    int i;
	    int nx = 0;
	    int ns = 0;

	    for (i = 0; i < n; i++) {
		epx_edge_t* ep = &poly->edge_table[i];
		epx_pos_t* p0 = &pt[ep->v0];
		epx_pos_t* p1 = &pt[ep->v1];
		int ylo = (p0->y < p1->y) ? p0->y : p1->y;
		int yhi = (p0->y < p1->y) ? p1->y : p0->y;

		if ((y < ylo) || (y > yhi))
		    continue;
		if (ylo == yhi) { // horizontal edge
		    sp[ns].x0 = (p0->x < p1->x) ? p0->x : p1->x;
		    sp[ns].x1 = (p0->x < p1->x) ? p1->x : p0->x;
		    ns++;
		}
		else {
		    float xa = edge_x(p0, p1, y-0.5f);
		    float xb = edge_x(p0, p1, y+0.5f);
		    if (xa > xb) { float t = xa; xa = xb; xb = t; }
		    // pixels with center on the edge segment in this row,
		    // at least the pixel nearest to the edge at y
		    sp[ns].x0 = (int) ceilf(xa);
		    sp[ns].x1 = (int) floorf(xb);
		    if (sp[ns].x0 > sp[ns].x1)
			sp[ns].x0 = sp[ns].x1 = (int) roundf(edge_x(p0,p1,y));
		    ns++;
		    if (y < yhi)
			xs[nx++] = edge_x(p0, p1, y);
		}
	    }
	    qsort(xs, nx, sizeof(float), float_cmp);
	    for (i = 0; i+1 < nx; i += 2) {
		int x0 = (int) ceilf(xs[i]);
		int x1 = (int) floorf(xs[i+1]);
		if (x0 <= x1) {
		    sp[ns].x0 = x0;
		    sp[ns].x1 = x1;
		    ns++;
		}
	    }
	    for (i = 0; i < ns; i++) {
		sp[i].x0 += x_offs;
		sp[i].x1 += x_offs;
	    }
	    fill_spans(pixmap, sp, ns, y+y_offs,
		       gc->fill_style, gc->fill_color);
	}
	free(xs);
	free(sp);
    }
    else if ((gc->line_style & (EPX_FLAG_SOLID|EPX_FLAG_BLEND|EPX_FLAG_SUM|
				 EPX_FLAG_AALIAS|EPX_FLAG_TEXTURED|
				 EPX_LINE_STYLE_DASHED)) != 0) {
	int i;
	epx_pos_t* pt = poly->pos_table;
	// FIXME: do not draw pixel twise!
	for (i = 0; i < (int) poly->nes; i++) {
	    epx_edge_t* ep = &poly->edge_table[i];
	    epx_draw_line(pixmap,
			  pt[ep->v0].x + x_offs,
			  pt[ep->v0].y + y_offs,
			  pt[ep->v1].x + x_offs,
			  pt[ep->v1].y + y_offs,
			  gc->line_width,
			  gc->line_style, gc->foreground_color);
	}
    }
}
