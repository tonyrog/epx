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

// sort array containing epx_edge_t*. a and b will point
// to such elements
static int sort_y(const void* a, const void* b)
{
    float a0 = (*((const epx_edge_t**)a))->y;
    float b0 = (*((const epx_edge_t**)b))->y;
    return a0 - b0;
}

static void sort_edges(epx_edge_t** es, size_t n)
{
    qsort(es, n, sizeof(epx_edge_t*), sort_y);
}

// add edge e into list sorted by x values
static void add_edge(epx_edge_t* e, epx_edge_t** as, int asn, epx_pos_t* pt)
{
    int i = asn;
    float x0 = (float) pt[e->v0].x;
    DEBUGF("add_edge: active table size = %d, x0=%.2f", asn, x0);
    as[i] = NULL; // debug
    while ((i>0) && (x0 < as[i-1]->x)) { // swap until sorted
	epx_swap_ptr(as[i], as[i-1]);
	// as[i] = as[i-1];
	i--;
    }
    DEBUGF("insert at pos = %d",  i);
    e->x = x0;
    assert(as[i] == NULL);
    as[i] = e;
}

static int activate_edges(int y,
			  epx_edge_t** es, int esn,
			  epx_edge_t** as, int asn,
			  epx_pos_t* pt)
{
    int added = 0;
    while(esn && (y >= pt[(*es)->v0].y)) { // activate edge
	DEBUGF("activate_edge (%d,%d) - (%d,%d)",
	      pt[(*es)->v0].x, pt[(*es)->v0].y,
	      pt[(*es)->v1].x, pt[(*es)->v1].y);
	(*es)->y = y;  // mark current y
	add_edge(*es, as, asn+added, pt);
	added++;
	esn--;
	es++;
    }
    return added;
}

// remove edge pointer as[i] with n elements
// FIXME: maybe just NULL mark the deleted edges?
static void remove_edge(int i, epx_edge_t** as, int n)
{
    DEBUGF("remove_edge: %d",  i);
    n--;
    while(i < n) {
	as[i] = as[i+1];
	i++;
    }
}

// update remove and recalc, return number of deleted edges
static int update_edges(int y, epx_edge_t** as, int asn, epx_pos_t* pt)
{
    int i = 0;
    int deleted = 0;
    while(i < asn) {
	if (y == pt[as[i]->v1].y)  { // remove the edge (mark only?)
	    remove_edge(i, as, asn);
	    asn--;
	    deleted++;
	}
	else if (y == as[i]->y) { // not processed	    
	    as[i]->x += as[i]->k;
	    DEBUGF("update_edge %d : (%d,%d)", i, as[i]->x, as[i]->y);
	    as[i]->y = (y+1);  // mark as processed
	    if ((i+1<asn) && (as[i]->x > as[i+1]->x)) { // move up
		int j = i;
		do {
		    epx_swap_ptr(as[j], as[j+1]);
		    j++;
		} while((j+1<asn) && (as[j]->x > as[j+1]->x));
	    }
	    else if ((i>0) && (as[i-1]->x > as[i]->x)) { // move down
		int j = i;
		do {
		    epx_swap_ptr(as[j], as[j-1]);
		    j--;
		} while((j>0) && (as[j-1]->x > as[j]->x));
		i++;  // since we already sort as[i]
	    }
	}
	else
	    i++;
    }
    return deleted;
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

static void poly_point(epx_pixmap_t* pixmap, int x, int y,
		       epx_flags_t flags, epx_pixel_t fg)
{
    uint8_t* dst;

    if (!epx_point_xy_in_rect(x, y, &pixmap->clip))
	return;
    dst = EPX_PIXEL_ADDR(pixmap,x,y);
    put_apixel(dst,pixmap->func.unpack,pixmap->func.pack,
	       flags, fg);
}

/* scan all active edge:
** a) horizontal edge (x0,y0) --- (x1,y0)
**
**                        (x0,y0)
**                        / \				\
** b) non monotonic edge /   \				\
**                           (x1,y1)
** c) normal edge (x0,y0)
**                  \				\
**                   \				\
**                  (x1,y1)
*/

static inline void scan_edge(epx_pixmap_t* pixmap, epx_edge_t* ep,
			     epx_pos_t* pt,
			     int x_offs, int y_offs, int y,
			     int* x_ptr, int* p_ptr,
			     epx_flags_t fill_style, epx_pixel_t fill_color)
			     
{
    if (ep->flags & EDGE_FLAG_HORIZONTAL) {
	int x0 = pt[ep->v0].x;
	int x1 = pt[ep->v1].x;
	if (x1 < x0)
	    epx_swap_int(x0,x1);
	epx_draw_line_horizontal(pixmap,
				 x0+x_offs,
				 x1+x_offs,
				 y+y_offs,
				 fill_style, epx_pixel_white);
	*p_ptr  = 0;
	*x_ptr = x1;	    
    }
    else if ((ep->flags & EDGE_FLAG_DOWN) &&
	     (y == pt[get_edge_orig_v0(ep)].y)) {
	int x = pt[get_edge_orig_v0(ep)].x;
	poly_point(pixmap,x+x_offs,y+y_offs,fill_style, epx_pixel_green);
	*x_ptr = x;
    }
    else if ((ep->flags & EDGE_FLAG_UP) &&
	     (y == pt[get_edge_orig_v1(ep)].y)) {
	int x = pt[get_edge_orig_v1(ep)].x;
	poly_point(pixmap,x+x_offs,y+y_offs, fill_style, epx_pixel_yellow);
	*x_ptr = x;
    }
    else if (*p_ptr) {
	int x0 = *x_ptr;
	int x1 = ep->x;
	epx_draw_line_horizontal(pixmap,
				 x0+x_offs,
				 x1+x_offs,
				 y+y_offs,
				 fill_style, fill_color); // epx_pixel_dimGray);
	*p_ptr = 0;
	*x_ptr = x1;
    }
    else {
	*p_ptr = 1;
	*x_ptr = ep->x;
    }
}

			     

// Scan line polynomial algorithm
void epx_poly_draw(epx_poly_t* poly,
		   epx_pixmap_t* pixmap, epx_gc_t* gc,
		   int x_offs, int y_offs)
{
    int ymin, ymax;
    float fymin, fymax;
    float fxmin, fxmax;
    
    if (gc == NULL)
	gc = &epx_default_gc;

    if ((pixmap->ctm != poly->last_ctm) ||
	(pixmap->ctm->version != poly->last_vsn)) {

	
	transform_edges(poly, pixmap->ctm);
	sort_edges(poly->es, poly->nes);
	poly->last_ctm = pixmap->ctm;
	poly->last_vsn = pixmap->ctm->version;
    }
    
    fymin = poly->ymin;
    fymax = poly->ymax;
    fxmin = poly->xmin;
    fxmax = poly->xmax;
    
    epx_t2d_transform_xy(pixmap->ctm, &fxmin, &fymin);
    epx_t2d_transform_xy(pixmap->ctm, &fxmax, &fymax);

    ymin = (int)roundf(fymin);
    ymax = (int)roundf(fymax);

    DEBUGF("draw ymin=%d, ymax=%d", ymin, ymax);
    
    if (gc->fill_style != EPX_FILL_STYLE_NONE) {
	int esn = poly->nes;  // number of remaining edges in es
	int asn = 0;          // number of active edges
	epx_edge_t** as = poly->as;
	epx_edge_t** es = poly->es;
	epx_pos_t* pt = poly->pos_table;    	
	int y;

	for (y = ymin; y <= ymax; y++) {
	    int i;
	    int k;
	    int p = 0;
	    int x = 0;
	    
	    DEBUGF("render y=%d, asn=%d, esn=%d", y, asn, esn);
	    k = activate_edges(y, es, esn, as, asn, pt);
	    DEBUGF("  activate %d edges", k);
	    asn += k;  // activated k edges in as
	    es  += k;  // skip "removed" edges
	    esn -= k;  // number of edges in es left

	    DEBUGF("  draw asn=%d esn=%d", asn, esn);

	    scan_edge(pixmap, as[0], pt, x_offs, y_offs, y,
		      &x, &p, gc->fill_style, gc->fill_color);
	    
	    for (i = 1; i < asn; i++) {
		scan_edge(pixmap, as[i], pt, x_offs, y_offs, y,
			  &x, &p, gc->fill_style, gc->fill_color);
	    }
	    DEBUGF("  update asn=%d", asn);
	    k = update_edges(y, as, asn, poly->pos_table);
	    DEBUGF("remove %d edges after update", k);
	    asn -= k;  // remove k edges from as
	}
    }
    else {
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
