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
typedef int (*backend_upgrade_t)(epx_backend_t*);

extern epx_backend_t* none_init(epx_dict_t* param);
extern int none_upgrade(epx_backend_t* backend);

#if defined(FB)
extern epx_backend_t* fb_init(epx_dict_t* param);
extern int fb_upgrade(epx_backend_t* backend);
#endif

// #if defined(__APPLE__) && !defined(__x86_64__)
// extern epx_backend_t* carbon_init(epx_dict_t* param);
// extern int carbon_upgrade(epx_backend_t* backend);
// #endif

#if defined(__APPLE__)
extern epx_backend_t* cocoa_init(epx_dict_t* param);
extern int cocoa_upgrade(epx_backend_t* backend);
#endif

#if defined(WIN32)
extern epx_backend_t* win32_init(epx_dict_t* param);
extern int win32_upgrade(epx_backend_t* backend);
#endif

#if defined(X11)
extern epx_backend_t* x11_init(epx_dict_t* param);
extern int x11_upgrade(epx_backend_t* backend);
#endif

#if defined(XCB)
extern epx_backend_t* xcb_init(epx_dict_t* param);
extern int xcb_upgrade(epx_backend_t* backend);
#endif

static struct _backend_item {
    char* name;
    backend_init_t init;
    backend_upgrade_t upgrade;
} backend_item [] =
{
#ifdef X11
    { .name = "x11",
      .init = x11_init,
      .upgrade = x11_upgrade
    },
#endif

#ifdef XCB
    { .name = "xcb",
      .init = xcb_init,
      .upgrade = xcb_upgrade
    },
#endif    

// #if defined(__APPLE__) && !defined(__x86_64__)
//    { .name = "macos",
//      .init = carbon_init,
//      .upgrade = carbon_upgrade
//    },
// #elif defined(__APPLE__) && defined(__x86_64__)
#if defined(__APPLE__) //  && defined(__x86_64__)
    { .name = "macos",
      .init = cocoa_init,
      .upgrade = cocoa_upgrade
    },
#endif

#ifdef WIN32
    { .name = "win32",
      .init = win32_init,
      .upgrade = win32_upgrade
    },
#endif

#ifdef FB
    { .name = "fb",
      .init = fb_init,
      .upgrade = fb_upgrade
    },
#endif
    { .name = "none",
      .init = none_init,
      .upgrade = none_upgrade
    },
    { .name = NULL,
      .init = NULL,
      .upgrade = NULL
    }
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

int epx_backend_upgrade(epx_backend_t* be)
{
    int i = 0;
    while(backend_item[i].name) {
	if (strcmp(backend_item[i].name, be->name) == 0)
	    return (backend_item[i].upgrade)(be);
	i++;
    }
    return -1;
}

int epx_backend_adjust(epx_backend_t* be, epx_dict_t* param)
{
    return be->cb->adjust(be, param);
}

int epx_backend_pixmap_attach(epx_backend_t* be, epx_pixmap_t* pixmap)
{
    if (pixmap->backend != NULL)
	return -1;
    if (be->cb->pix_attach(be, pixmap) < 0)
	return -1;
    return 0;
}

int epx_pixmap_detach(epx_pixmap_t* pixmap)
{
    epx_backend_t* be;

    if ((be = pixmap->backend) != NULL) {
	be->cb->pix_detach(be, pixmap);
	pixmap->backend = 0;
    }
    return 0;
}

int epx_backend_window_attach(epx_backend_t* be, epx_window_t* window)
{
    if (window->backend != NULL)
	return -1;
    if (be->cb->win_attach(be, window) < 0)
	return -1;
    return 0;
}

int epx_window_detach(epx_window_t* window)
{
    epx_backend_t* be;
    if ((be = window->backend) != NULL) {
	be->cb->win_detach(be, window);
	window->backend = 0;
    }
    return 0;
}

int epx_window_swap(epx_window_t* win)
{
    if (win->backend == NULL)
	return -1;
    return epx_backend_window_swap(win->backend, win);
}

int epx_window_adjust(epx_window_t* win, epx_dict_t* param)
{
    if (win->backend == NULL)
	return -1;
    return epx_backend_window_adjust(win->backend, win, param);
}

int epx_window_info(epx_window_t* win, epx_dict_t* param)
{
    if (win->backend == NULL)
	return -1;
    return epx_backend_window_info(win->backend, win, param);
}
