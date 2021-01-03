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

#include <stdlib.h>
#include "../include/epx_object.h"
#include "../include/epx_canvas.h"

#define EXPAND_SIZE 1024

void epx_canvas_init(epx_canvas_t* canvas)
{
    EPX_OBJECT_INIT(canvas, EPX_CANVAS_TYPE);
    canvas->nparams = 0;
    canvas->nparams_allocated = 0;
    canvas->param = NULL;
    canvas->state = NULL;    
    canvas->nelems = 0;
    canvas->nelems_allocated = 0;
    canvas->elem = NULL;
}

void epx_canvas_destroy(epx_canvas_t* canvas)
{
    epx_object_unref(canvas);
}

void EPX_CANVAS_TYPE_RELEASE(void* arg)
{
    epx_canvas_t* canvas = (epx_canvas_t*) arg;

    EPX_DBGFMT_MEM("EPX_CANVAS_TYPE_RELEASE: %p", arg);
    free(canvas->param);
    free(canvas->elem);
    free(canvas->state);
    if (canvas->on_heap) free(canvas);
}

epx_canvas_t* epx_canvas_create()
{
    epx_canvas_t* canvas;

    if (!(canvas = (epx_canvas_t*) malloc(sizeof(epx_canvas_t))))
	return 0;
    epx_canvas_init(canvas);
    canvas->on_heap = 1;
    canvas->refc = 1;
    return canvas; 
}

int epx_canvas_elem(epx_canvas_t* canvas, epx_canvas_op_t op, int i, int j)
{
    int k = (int)canvas->nelems++;

    // printf("elem %d,  op=%d, i=%d, j=%d\r\n", k, op, i, j);
    if ((op == CANVAS_OP_LINE) || (op == CANVAS_OP_QUAD)) {
	if (i >= (int)canvas->nparams)
	    return -1;
    }
    else {
	if (i >= k) return -1;
	if (j >= k) return -1;
    }

    if (k >= (int)canvas->nelems_allocated) {
	size_t n = canvas->nelems_allocated + EXPAND_SIZE;
	epx_canvas_elem_t* elem;
	elem  = realloc(canvas->elem, n*sizeof(epx_canvas_elem_t));
	if (!elem)
	    return -1;
	canvas->elem = elem;
	canvas->nelems_allocated = n;
    }
    canvas->elem[k].op = op;
    if ((op == CANVAS_OP_AND) || (op == CANVAS_OP_OR))
	canvas->elem[k].pixop = EPX_PIXEL_OP_SRC_BLEND;
    else
	canvas->elem[k].pixop = EPX_PIXEL_OP_ADD;
    canvas->elem[k].color = (epx_pixel_t) EPX_PIXEL_TRANSPARENT;
    canvas->elem[k].pixel_is_set = 0;
    canvas->elem[k].i = i;
    canvas->elem[k].j = j;
    return k;
}

int epx_canvas_param(epx_canvas_t* canvas,
		     double A, double B, double C,
		     double D, double E, double F)
{
    int i = canvas->nparams++;

    // printf("param i=%d,A=%f,B=%f,C=%f,D=%f,E=%f,F=%f\r\n",i,A,B,C,D,E,F);
    
    if (i >= (int) canvas->nparams_allocated) {
	size_t n = canvas->nparams_allocated + EXPAND_SIZE;
	epx_canvas_param_t* param;
	epx_canvas_state_t* state;
	param = realloc(canvas->param, n*sizeof(epx_canvas_param_t));
	state = realloc(canvas->state, n*sizeof(epx_canvas_state_t));
	if (!param || !state)
	    return -1;
	canvas->param = param;
	canvas->state = state;
	canvas->nparams_allocated = n;
    }
    canvas->param[i].A = A;
    canvas->param[i].B = B;
    canvas->param[i].C = C;
    canvas->param[i].D = D;
    canvas->param[i].E = E;
    canvas->param[i].F = F;
    
    canvas->state[i].S = 0.0;
    canvas->state[i].T = 0.0;
    canvas->state[i].U = 0.0;

    return i;
}

int epx_canvas_line(epx_canvas_t* canvas, double D, double E, double F)
{
    int i;
    if ((i = epx_canvas_param(canvas, 0.0, 0.0, 0.0, D, E, F)) < 0)
	return -1;
    return epx_canvas_elem(canvas, CANVAS_OP_LINE, i, -1);
}

int epx_canvas_quad(epx_canvas_t* canvas,
		    double A, double B, double C,
		    double D, double E, double F)
{
    int i;
    
    if ((i = epx_canvas_param(canvas,A,B,C,D,E,F)) < 0)
	return -1;
    return epx_canvas_elem(canvas, CANVAS_OP_QUAD, i, -1);
}

int epx_canvas_and(epx_canvas_t* canvas, int a, int b)
{
    return epx_canvas_elem(canvas, CANVAS_OP_AND, a, b);
}

int epx_canvas_or(epx_canvas_t* canvas, int a, int b)
{
    return epx_canvas_elem(canvas, CANVAS_OP_OR, a, b);
}

int epx_canvas_over(epx_canvas_t* canvas, int a, int b)
{
    return epx_canvas_elem(canvas, CANVAS_OP_OVER, a, b);
}

int epx_canvas_not(epx_canvas_t* canvas, int a)
{
    return epx_canvas_elem(canvas, CANVAS_OP_NOT, a, -1);
}

int epx_canvas_set_color(epx_canvas_t* canvas, int a, epx_pixel_t color)
{
    if (a >= (int)canvas->nelems)
	return 0;
    canvas->elem[a].color = color;
    return 1;
}

int epx_canvas_set_operation(epx_canvas_t* canvas, int a,
			     epx_pixel_operation_t pixop)
{
    if (a >= (int)canvas->nelems)
	return 0;
    canvas->elem[a].pixop = pixop;
    return 1;
}

// Fixme rectangle and flags!
int epx_canvas_draw(epx_canvas_t* canvas, epx_pixmap_t* pixmap)
{
    int y;
    epx_flags_t flags = EPX_FLAG_BLEND;
    
    for (y = 0; y < (int)pixmap->height; y++) {
	int x = 0;
	int k;
	
	// start_x, FIXME: make a step function for y!
	// in this case x=0 so compiler should optimise that!
	for (k = 0; k < (int)canvas->nparams; k++) {
	    double Ax, By, S0, T0, U0;
	    Ax = canvas->param[k].A*x;
	    By = canvas->param[k].B*y;
	    S0 = x*(Ax+By+canvas->param[k].D) +
		y*(canvas->param[k].C*y + canvas->param[k].E) +
		canvas->param[k].F;
	    T0 = (canvas->param[k].A+canvas->param[k].D+By+2*Ax);
	    U0 = 2*canvas->param[k].A;
	    canvas->state[k].S = S0;
	    canvas->state[k].T = T0;
	    canvas->state[k].U = U0;
	}
	
	for (x = 0; x < (int)pixmap->width; x++) {
	    // evaluate the tree structure from the leaves to the root
	    // FIXME eval this only for pixels in object bounding box!
	    for (k = 0; k < (int)canvas->nelems; k++) {
		epx_pixel_t c = canvas->elem[k].color;
		epx_pixel_t p = (epx_pixel_t) EPX_PIXEL_TRANSPARENT;
		int i = canvas->elem[k].i;
		int j = canvas->elem[k].j;
		canvas->elem[k].pixel_is_set = 0;
		switch(canvas->elem[k].op) {
		case CANVAS_OP_LINE:
		    p = c;
		    canvas->elem[k].pixel_is_set = (canvas->state[i].S <= 0);
		    break;
		case CANVAS_OP_QUAD:
		    p = c;
		    canvas->elem[k].pixel_is_set = (canvas->state[i].S <= 0);
		    break;
		case CANVAS_OP_AND:
		    if (canvas->elem[i].pixel_is_set &&
			canvas->elem[j].pixel_is_set) {
			p = epx_pixel_operation(canvas->elem[k].pixop,
						canvas->elem[i].pixel,
						canvas->elem[j].pixel);
			p = epx_pixel_operation(canvas->elem[k].pixop, c, p);
			canvas->elem[k].pixel_is_set = 1;
		    }
		    break;
		case CANVAS_OP_OR:
		    if (canvas->elem[i].pixel_is_set &&
			canvas->elem[j].pixel_is_set) {
			p = epx_pixel_operation(canvas->elem[k].pixop,
						canvas->elem[i].pixel,
						canvas->elem[j].pixel);
			p = epx_pixel_operation(canvas->elem[k].pixop, c, p);
			canvas->elem[k].pixel_is_set = 1;
			break;
		    }
		    // fall through
		case CANVAS_OP_OVER:
		    if (canvas->elem[i].pixel_is_set) {
			p = canvas->elem[i].pixel;
			p = epx_pixel_operation(canvas->elem[k].pixop,c,p);
			canvas->elem[k].pixel_is_set = 1;
		    }
		    else if (canvas->elem[j].pixel_is_set) {
			p = canvas->elem[j].pixel;
			p = epx_pixel_operation(canvas->elem[k].pixop,c,p);
			canvas->elem[k].pixel_is_set = 1;
		    }
		    break;    
		case CANVAS_OP_NOT:
		    if (!canvas->elem[i].pixel_is_set) {
			p = canvas->elem[i].pixel;
			p = epx_pixel_operation(canvas->elem[k].pixop,c,p);
			canvas->elem[k].pixel_is_set = 1;
		    }
		    break;
		default:
		    break;
		}
		canvas->elem[k].pixel = p;
	    }
	    // the top node containes the pixel we render
	    epx_pixmap_put_pixel(pixmap, x, y, flags, canvas->elem[k-1].pixel);
	    
	    // step_x, advance all states
	    for (k = 0; k < (int)canvas->nparams; k++) {
		double t = canvas->state[k].T + canvas->state[k].U;
		canvas->state[k].S += t;
		canvas->state[k].T = t;
	    }
	}
    }
    return 0;
}
