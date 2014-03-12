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
// EPX - Backend managment
//
//
#include <string.h>

#include "../include/epx_dict.h"
#include "../include/epx_pixmap.h"
#include "../include/epx_window.h"
#include "../include/epx_backend.h"

typedef epx_backend_t* (*backend_init_t)(epx_dict_t* param);

extern epx_backend_t* none_init(epx_dict_t* param);

#if defined(FB)
extern epx_backend_t* fb_init(epx_dict_t* param);
#endif

#if defined(__APPLE__) && !defined(__x86_64__)
extern epx_backend_t* carbon_init(epx_dict_t* param);
#endif

// #if defined(__APPLE__)
// extern epx_backend_t* cocoa_init(epx_dict_t* param);
// #endif


#if defined(WIN32)
extern epx_backend_t* win32_init(epx_dict_t* param);
#endif

#if defined(X11)
extern epx_backend_t* x11_init(epx_dict_t* param);
#endif

static struct _backend_item {
    char* name;
    backend_init_t init;
} backend_item [] =
{
#if defined(__APPLE__) && !defined(__x86_64__)
    {"macos", (backend_init_t) carbon_init},
#endif

// #if defined(__APPLE__)
//    {"cocoa", (backend_init_t) cocoa_init},
//#endif


#ifdef WIN32
    {"win32", (backend_init_t) win32_init},
#endif

#ifdef X11
    {"x11", (backend_init_t) x11_init},
#endif

#ifdef FB
    {"fb", (backend_init_t) fb_init},
#endif
    {"none", (backend_init_t) none_init},
    {NULL, NULL}
};

char* epx_backend_name(int i)
{
    int sz = sizeof(backend_item)/(sizeof(struct _backend_item));

    if (i >= sz)
	return 0;
    return backend_item[i].name;
}

void EPX_BACKEND_TYPE_RELEASE(void* arg)
{
    epx_backend_t* be = (epx_backend_t*) arg;
    EPX_DBGFMT_MEM("EBACKEND_TYPE_RELEASE: %p", arg);    
    epx_backend_finish(be);
}

void epx_backend_destroy(epx_backend_t* be)
{
    epx_object_unref(be);
}

epx_backend_t* epx_backend_create(char* name, epx_dict_t* param)
{
    if (!name) {
	if (backend_item[0].init)
	    return (backend_item[0].init)(param);
	return 0;
    }
    else {
	int i = 0;
	while(backend_item[i].name) {
	    if (strcmp(backend_item[i].name, name) == 0)
		return (backend_item[i].init)(param);
	    i++;
	}
	return 0;
    }
}

int epx_backend_adjust(epx_backend_t* be, epx_dict_t* param)
{
    return be->cb->adjust(be, param);
}

static int pixmap_backend_detach(epx_pixmap_t* pixmap)
{
    epx_backend_t* be = pixmap->backend;
    if (be)
	return be->cb->pix_detach(be, pixmap);
    return -1;
}

int epx_backend_pixmap_attach(epx_backend_t* be, epx_pixmap_t* pixmap)
{
    if (pixmap->backend)
	return -1;
    if (be->cb->pix_attach(be, pixmap) < 0)
	return -1;
    pixmap->detach = pixmap_backend_detach;
    return 0;
}

static int window_backend_detach(epx_window_t* window)
{
    epx_backend_t* be = window->backend;
    if (be)
	return be->cb->win_detach(be, window);
    return -1;
}

int epx_backend_window_attach(epx_backend_t* be, epx_window_t* window)
{
    if (window->backend)
	return -1;
    if (be->cb->win_attach(be, window) < 0)
	return -1;
    window->detach = window_backend_detach;
    return 0;
}

int epx_window_detach(epx_window_t* window)
{
    window_backend_detach(window);
    window->backend = 0;
    return 0;
}

int epx_pixmap_detach(epx_pixmap_t* pixmap)
{
    pixmap_backend_detach(pixmap);
    pixmap->backend = 0;
    return 0;
}

int epx_window_swap(epx_window_t* win)
{
    if (!win->backend)
	return -1;
    return epx_backend_window_swap(win->backend, win);
}

int epx_window_adjust(epx_window_t* win, epx_dict_t* param)
{
    if (!win->backend)
	return -1;
    return epx_backend_window_adjust(win->backend, win, param);
}
