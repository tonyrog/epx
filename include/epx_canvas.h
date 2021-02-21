/***************************************************************************
 *
 * Copyright (C) 2007 - 2020, Rogvall Invest AB, <tony@rogvall.se>
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

#ifndef __EPX_CANVAS_H__
#define __EPX_CANVAS_H__

#include "epx_pixmap.h"
#include "epx_geometry.h"

typedef enum {
    CANVAS_OP_LINE,
    CANVAS_OP_QUAD,
    CANVAS_OP_AND,
    CANVAS_OP_OR,
    CANVAS_OP_OVER,
    CANVAS_OP_NOT
} epx_canvas_op_t;

typedef struct {
    double A,B,C,D,E,F;
} epx_canvas_param_t;

typedef struct {
    double S,T,U;
} epx_canvas_state_t;

typedef struct {
    epx_canvas_op_t op;
    epx_pixel_operation_t pixop;
    epx_pixel_t color;  // param
    int u;              // use counter
    int i;              // OP=LINE|QUAD then i is index to param and state
    int j;              // OP!=LINE|QUAD then i,j are index into elem
} epx_canvas_elem_t;

typedef struct {
    epx_rect_t bound;   // bounding box
    int i;              // index to element
} epx_canvas_object_t;

typedef struct _epx_canvas_t {
    EPX_OBJECT_MEMBERS(struct _epx_canvas_t);
    epx_pixel_operation_t topop;  // top level mixing
    size_t nparams;
    size_t nparams_allocated;
    epx_canvas_param_t* param; // [nparams]
    epx_canvas_state_t* state;  // [nparams]
    size_t nelems;
    size_t nelems_allocated;
    epx_canvas_elem_t*  elem;  // [nelems]
} epx_canvas_t;

extern void epx_canvas_init(epx_canvas_t* canvas);

extern int epx_canvas_line(epx_canvas_t* canvas,
			   double D, double E, double F);
extern int epx_canvas_quad(epx_canvas_t* canvas,
			   double A, double B, double C,
			   double D, double E, double F);
extern int epx_canvas_and(epx_canvas_t* canvas, int a, int b);
extern int epx_canvas_or(epx_canvas_t* canvas, int a, int b);
extern int epx_canvas_over(epx_canvas_t* canvas, int a, int b);
extern int epx_canvas_not(epx_canvas_t* canvas, int a);
extern int epx_canvas_set_color(epx_canvas_t* canvas, int a,
				epx_pixel_t color);
extern int epx_canvas_set_operation(epx_canvas_t* canvas, int a,
				    epx_pixel_operation_t pixop);
extern int epx_canvas_set_param(epx_canvas_t* canvas, int i, int k,
				double param);
extern int epx_canvas_draw(epx_canvas_t* canvas, epx_pixmap_t* pixmap);

#endif
