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

#ifndef __EPX_POLY_H__
#define __EPX_POLY_H__

#include "epx_pixmap.h"
#include "epx_gc.h"
#include "epx_t2d.h"

typedef struct _epx_vertex_t
{
    float x;
    float y;
} epx_vertex_t;

typedef struct _epx_pos_t
{
    int x;
    int y;
} epx_pos_t;

#define EDGE_FLAG_UP         1   // non-monotonic edge
#define EDGE_FLAG_DOWN       2   // non-monotonic edge
#define EDGE_FLAG_SWAP       4   // vertices swapped
#define EDGE_FLAG_HORIZONTAL 8   // v0->y == v1->y

typedef struct _epx_edge_t
{
    float x;      // current x value (inital = x0)
    float k;      // dx/dy (dy>0, =0 otherwise)
    int   flags;  // various flags
    int y;        // current scan y, used as update flag
    int v0, v1;   // index in vertex/pos tables
} epx_edge_t;

typedef struct _epx_poly_t {
    EPX_OBJECT_MEMBERS(struct _epx_canvas_t);
    size_t nes;
    size_t nvs;
    float  xmin, xmax;
    float  ymin, ymax;
    epx_vertex_t* vertex_table;  // [nvs]    
    epx_pos_t*    pos_table;     // [nvs]
    epx_edge_t*   edge_table;    // [nes]
    epx_edge_t**  es;            // y0 sorted edges [nes]
    epx_edge_t**  as;            // active edges    [nes]
    epx_t2d_t*    last_ctm;
    unsigned long last_vsn;
} epx_poly_t;

extern void epx_poly_init(epx_poly_t* poly);

extern epx_poly_t* epx_poly_create(void);
extern int epx_poly_set(epx_poly_t* poly, epx_vertex_t* vs, size_t n);
extern void epx_poly_draw(epx_poly_t* poly,
			  epx_pixmap_t* pixmap,
			  epx_gc_t* gc,
			  int x_offs, int y_offs);

#endif
