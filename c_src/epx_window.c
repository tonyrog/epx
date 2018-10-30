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
// EPX window  managent
//

#include "../include/epx_object.h"
#include "../include/epx_dict.h"
#include "../include/epx_window.h"
#include "../include/epx_backend.h"

void EPX_WINDOW_TYPE_RELEASE(void* arg)
{
    epx_window_t* window = (epx_window_t*) arg;
    epx_backend_t* be;
    EPX_DBGFMT_MEM("EWINDOW_TYPE_RELEASE: %p", arg);
    if ((be = window->backend) != NULL)
	be->cb->win_detach(be, window);
    if (window->on_heap) free(window);
}

void epx_window_destroy(epx_window_t* win)
{
    epx_object_unref(win);
}

int epx_window_init(epx_window_t* win, int x, int y, 
		    unsigned int width, unsigned int height)
{
    EPX_OBJECT_INIT(win, EPX_WINDOW_TYPE);
    win->backend = 0;
    win->user    = 0;
    win->owner   = 0;
    win->opengl  = 0;
    win->mask    = 0;
    epx_rect_set(&win->area, x, y, width, height);
    epx_rect_set(&win->rarea, x, y, width, height);
    return 0;
}

epx_window_t* epx_window_create(int x, int y, 
				unsigned int width, unsigned int height)
{
    epx_window_t* win;

    if (!(win = (epx_window_t*) malloc(sizeof(epx_window_t))))
	return 0;
    epx_window_init(win, x, y, width, height);
    win->on_heap = 1;
    win->refc = 1;
    return win;
}

