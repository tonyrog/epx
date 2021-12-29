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
// EPX - backend binding
//
#ifndef __EPX_BACKEND_H__
#define __EPX_BACKEND_H__

#include "epx_dict.h"
#include "epx_pixmap.h"
#include "epx_event.h"
#include "epx_window.h"

#ifndef EPX_HANDLE_T
#define EPX_HANDLE_T void*
#endif

#define EPX_INVALID_HANDLE ((EPX_HANDLE_T)-1)
#define EPX_BACKEND_MAX_FORMATS 64
// backend interface
typedef struct _epx_backend_t {
    EPX_OBJECT_MEMBERS(struct _epx_backend_t);
    void* owner;                          // user field (creator pid)
    void* user;                           // extra user data    
    /* !Name of backed type */
    char* name;
    /*! Number of pending events */
    volatile int pending;
    /*! OpenGL supported? */
    int opengl;
    /*! Use OpenGL ? */
    int use_opengl;
    /*! Display width */
    int width;
    /*! Display height */
    int height;
    /*! Number of used display formats */
    int nformats;
    /*! Display formats */
    epx_format_t formats[EPX_BACKEND_MAX_FORMATS];
    /*! List of attached windows */
    epx_object_list_t window_list;
    /*! List of attached pixmaps */
    epx_object_list_t pixmap_list;
    /*! Event handle             */
    EPX_HANDLE_T event;
    /*! Backend callbacks        */
    struct _epx_callbacks_t* cb;
} epx_backend_t;

// backend methods
typedef struct _epx_callbacks_t {
    int (*finish)(epx_backend_t*);
    int (*pix_attach)(epx_backend_t*, epx_pixmap_t*);
    int (*pix_detach)(epx_backend_t*, epx_pixmap_t*);
    int (*pix_draw)(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
		    int, int, int, int, unsigned int, unsigned int);
    int (*pix_sync)(epx_backend_t*, epx_pixmap_t*, epx_window_t*);
    int (*win_attach)(epx_backend_t*, epx_window_t*);
    int (*win_detach)(epx_backend_t*, epx_window_t*);
    EPX_HANDLE_T (*evt_attach)(epx_backend_t*);
    int (*evt_detach)(epx_backend_t*);
    int (*evt_read)(epx_backend_t*, epx_event_t*);
    int (*adjust)(epx_backend_t*, epx_dict_t *);
    int (*win_swap)(epx_backend_t*, epx_window_t*);
    int (*begin)(epx_window_t*);
    int (*end)(epx_window_t*, int);
    int (*win_adjust)(epx_window_t*, epx_dict_t*);
    int (*info)(epx_backend_t*, epx_dict_t*);
    int (*win_info)(epx_window_t*, epx_dict_t*);
} epx_callbacks_t;

extern char*          epx_backend_name(int i);
extern epx_backend_t* epx_backend_create(char* name, epx_dict_t* param);
extern int  epx_backend_upgrade(epx_backend_t* be);
extern void epx_backend_destroy(epx_backend_t* be);

#define epx_backend_finish(be) ((be)->cb->finish((be)))

#define epx_backend_draw_begin(be,win)   ((be)->cb->begin((win)))
#define epx_backend_draw_end(be,win,os)  ((be)->cb->end((win),(os)))
#define epx_backend_pixmap_draw(be,pix,win,xs,ys,xd,yd,w,h)		\
    ((be)->cb->pix_draw((be),(pix),(win),(xs),(ys),(xd),(yd),(w),(h)))
#define epx_backend_pixmap_sync(be,pix,win) \
    ((be)->cb->pix_sync((be),(pix),(win)))

#define epx_backend_window_swap(be,win)   ((be)->cb->win_swap((be),(win)))
#define epx_backend_event_attach(be)    ((be)->cb->evt_attach((be)))
#define epx_backend_event_detach(be)    ((be)->cb->evt_detach((be)))
#define epx_backend_event_read(be,e)    ((be)->cb->evt_read((be),(e)))
#define epx_backend_window_adjust(be,win,param) ((be)->cb->win_adjust((win),(param)))
#define epx_backend_info(be,param) ((be)->cb->info((be),(param)))
#define epx_backend_window_info(be,win,param) ((be)->cb->win_info((win),(param)))

extern int epx_backend_adjust(epx_backend_t *be, epx_dict_t *param);

extern int  epx_window_swap(epx_window_t* win);
extern int  epx_window_adjust(epx_window_t* win, epx_dict_t *param);
extern int  epx_window_info(epx_window_t* win, epx_dict_t *param);

extern int epx_backend_window_attach(epx_backend_t* be, epx_window_t* win);
extern int epx_window_detach(epx_window_t* win);

extern int epx_backend_pixmap_attach(epx_backend_t* be, epx_pixmap_t* pix);
extern int epx_pixmap_detach(epx_pixmap_t* pix);

#endif

