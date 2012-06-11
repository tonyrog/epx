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

// backend interface
typedef struct _epx_backend_t {
    EPX_OBJECT_MEMBERS(struct epx_backend_t);
    /*! Number of pending events */
    int pending;
    /*! OpenGL supported? */
    int opengl;
    /*! Use OpenGL ? */
    int use_opengl;
    /*! List of attached windows */
    epx_window_t*  window_list;
    /*! List of attached pixmaps */
    epx_pixmap_t*  pixmap_list;
    /*! Event handle             */
    EPX_HANDLE_T event;
    /*! Backend callbacks        */
    struct _epx_callbacks_t* cb;
} epx_backend_t;

// backend methods
typedef struct _epx_callbacks_t {
    int (*finish)(epx_backend_t*);
    int (*pic_attach)(epx_backend_t*, epx_pixmap_t*);
    int (*pic_detach)(epx_backend_t*, epx_pixmap_t*);
    int (*pic_draw)(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
		    int, int, int, int, unsigned int, unsigned int);
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
} epx_callbacks_t;

extern char*          epx_backend_name(int i);
extern epx_backend_t* epx_backend_create(char* name, epx_dict_t* param);
extern void epx_backend_destroy(epx_backend_t* be);

#define epx_backend_finish(be) ((be)->cb->finish((be)))

#define epx_backend_draw_begin(be,win)   ((be)->cb->begin((win)))
#define epx_backend_draw_end(be,win,os)  ((be)->cb->end((win),(os)))
#define epx_backend_pixmap_draw(be,pic,win,xs,ys,xd,yd,w,h)		\
    ((be)->cb->pic_draw((be),(pic),(win),(xs),(ys),(xd),(yd),(w),(h)))

#define epx_backend_window_swap(be,win)   ((be)->cb->win_swap((be),(win)))
#define epx_backend_event_attach(be)    ((be)->cb->evt_attach((be)))
#define epx_backend_event_detach(be)    ((be)->cb->evt_detach((be)))
#define epx_backend_event_read(be,e)    ((be)->cb->evt_read((be),(e)))
#define epx_backend_window_adjust(be,win,param) ((be)->cb->win_adjust((win),(param)))

extern int epx_backend_adjust(epx_backend_t *be, epx_dict_t *param);

extern int  epx_window_swap(epx_window_t* win);
extern int  epx_window_adjust(epx_window_t* win, epx_dict_t *param);

extern int epx_backend_window_attach(epx_backend_t* be, epx_window_t* win);
extern int epx_window_detach(epx_window_t* win);

// Render (attached) pixmap on attached win 
extern int epx_pixmap_draw_window(epx_pixmap_t* pic, epx_window_t* win, 
				  int x_src, int y_src, int x_dst, int y_dst, 
				  unsigned int width, unsigned int height);

extern int epx_backend_pixmap_attach(epx_backend_t* be, epx_pixmap_t* pic);
extern int epx_pixmap_detach(epx_pixmap_t* pic);



#endif

