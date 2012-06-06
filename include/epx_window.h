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
#ifndef __EPX_WINDOW_H__
#define __EPX_WINDOW_H__

struct _epx_backend_t;

// window interface
typedef struct _epx_window_t {
    EPX_OBJECT_MEMBERS(struct _epx_window_t);
    struct _epx_backend_t* backend;       // backend pointer if attached
    int (*detach)(struct _epx_window_t*); // detach function
    void* owner;                          // user field (for erlang pid)
    void* user;                           // extra user data
    uint32_t mask;                        // event mask, FIXME lock!
    int opengl;                           // window supportes OpenGL
    int x;
    int y;
    unsigned int width;
    unsigned int height;
} epx_window_t;

//
//  Window inteface 
//
#define epx_window_draw_begin(win)   (((win)->backend)->cb->begin((win)))
#define epx_window_draw_end(win,os)  (((win)->backend)->cb->end((win),(os)))
// FIXME!!! protect event mask operation with lock when threaded!
#define epx_window_set_event_mask(win,ev) ((win)->mask = (ev))
#define epx_window_get_event_mask(win)    ((win)->mask)
#define epx_window_enable_events(win,ev)  ((win)->mask |= (ev))
#define epx_window_disable_events(win,ev) ((win)->mask &= ~(ev))

extern epx_window_t* epx_window_create(int x, int y,
				       unsigned int width,
				       unsigned int height);
extern int epx_window_init(epx_window_t* win, int x, int y, 
			   unsigned int width, unsigned int height);
extern void epx_window_destroy(epx_window_t* win);

#endif
