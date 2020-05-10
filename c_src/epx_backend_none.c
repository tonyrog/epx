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
//  Template display driver 
//

#include "../include/epx_backend.h"

/* This structure is stored in epx_window_t opaque field */
typedef struct {
    int wstate;
    unsigned long dcount;
} none_window_t;

typedef struct {
    epx_backend_t b;                /* DO NOT MOVE !!! */
} none_backend_t;


epx_backend_t* none_init(epx_dict_t* param);
int none_upgrade(epx_backend_t* be);

static int none_finish(epx_backend_t*);
static int none_pix_attach(epx_backend_t*, epx_pixmap_t*);
static int none_pix_detach(epx_backend_t*, epx_pixmap_t*);
static int none_begin(epx_window_t*);
static int none_end(epx_window_t*, int);
static int none_pix_draw(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
			 int src_x, int src_y, int dst_x, int dst_y,
			 unsigned int width,
			 unsigned int height);
static int none_pix_sync(epx_backend_t*, epx_pixmap_t*, epx_window_t*);
static int none_win_attach(epx_backend_t*, epx_window_t*);
static int none_win_detach(epx_backend_t*, epx_window_t*);
static int none_win_swap(epx_backend_t*, epx_window_t*);
static EPX_HANDLE_T none_evt_attach(epx_backend_t*);
static int none_evt_detach(epx_backend_t*);
static int none_evt_read(epx_backend_t*, epx_event_t*);
static int none_adjust(epx_backend_t *backend, epx_dict_t* param);
static int none_win_adjust(epx_window_t *window, epx_dict_t* param);
static int none_info(epx_backend_t *backend, epx_dict_t* param);
static int none_win_info(epx_window_t *window, epx_dict_t* param);


static epx_callbacks_t none_callbacks =
{
    .finish       = none_finish,
    .pix_attach   = none_pix_attach,
    .pix_detach   = none_pix_detach,
    .pix_draw     = none_pix_draw,
    .pix_sync     = none_pix_sync,
    .win_attach   = none_win_attach,
    .win_detach   = none_win_detach,
    .evt_attach   = none_evt_attach,
    .evt_detach   = none_evt_detach,
    .evt_read     = none_evt_read,
    .adjust       = none_adjust,
    .win_swap     = none_win_swap,
    .begin        = none_begin,
    .end          = none_end,
    .win_adjust   = none_win_adjust,
    .info         = none_info,
    .win_info     = none_win_info,
};


epx_backend_t* none_init(epx_dict_t* param)
{
    none_backend_t* be;

    (void) param;

    if (!(be = (none_backend_t*) malloc(sizeof(none_backend_t))))
	return 0;
    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);
    be->b.name = "none";
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;
    be->b.width = 0;
    be->b.height = 0;
    be->b.nformats = 0;
    be->b.cb = &none_callbacks;
    epx_object_list_init(&be->b.pixmap_list);
    epx_object_list_init(&be->b.window_list);
    be->b.event = EPX_INVALID_HANDLE;

    return (epx_backend_t*) &(be->b);
}

int none_upgrade(epx_backend_t* backend)
{
    backend->cb = &none_callbacks;
    return 0;
}


/* return the backend event handle */
static EPX_HANDLE_T none_evt_attach(epx_backend_t* backend)
{
    (void) backend;
    return EPX_INVALID_HANDLE;
}

static int none_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

static int none_finish(epx_backend_t* backend)
{
    none_backend_t* none = (none_backend_t*) backend;

    free(none);
    return 0;
}

static int none_pix_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    none_backend_t* none = (none_backend_t*) backend;

    if (pixmap->opaque != NULL)
	return -1;
    epx_object_link(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) 1;
    pixmap->backend = (epx_backend_t*) none;
    return 0;
}

static int none_pix_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    epx_object_unlink(&backend->pixmap_list, pixmap);
    pixmap->opaque = NULL;
    pixmap->backend = NULL;
    return 0;
}

static int none_begin(epx_window_t* ewin)
{
    (void) ewin;
    return 0;
}

static int none_end(epx_window_t* ewin, int off_screen)
{
    (void) ewin;
    (void) off_screen;
    return 0;
}

static int none_pix_draw(epx_backend_t* backend, epx_pixmap_t* pixmap, epx_window_t* ewin,
			 int src_x, int src_y, int dst_x, int dst_y,
			 unsigned int width,
			 unsigned int height)
{
    none_window_t*  nwin = (none_window_t*) ewin->opaque;
    (void) backend;
    (void) pixmap;
    (void) src_x;
    (void) src_y;
    (void) dst_x;
    (void) dst_y;
    (void) width;
    (void) height;

    if (!nwin)
	return -1;
    nwin->dcount++;
    return 0;
}

static int none_pix_sync(epx_backend_t* backend, epx_pixmap_t* pixmap,
			 epx_window_t* ewin)
{
    (void) backend;
    (void) pixmap;
    (void) ewin;
    return 0;
}

static int none_win_swap(epx_backend_t* backend, epx_window_t* window)
{
    (void) backend;
    (void) window;
    return 0;
}

static int none_win_attach(epx_backend_t* backend, epx_window_t* ewin)
{
    none_backend_t* none = (none_backend_t*) backend;
    none_window_t*  nwin;

    if (ewin->opaque != NULL)
	return -1;
    if ((nwin = (none_window_t*) malloc(sizeof(none_window_t))) == NULL)
	return -1;
    nwin->wstate = 1;
    nwin->dcount = 0;
    epx_object_link(&backend->window_list, ewin);
    ewin->opaque  = (void*) nwin;
    ewin->backend = (epx_backend_t*) none;
    return 0;
}

static int none_win_detach(epx_backend_t* backend, epx_window_t* ewin)
{
    none_backend_t* none = (none_backend_t*) backend;
    none_window_t*  win  = (none_window_t*)  ewin->opaque;
    
    if ((none != NULL) && (win->wstate != 0)) {
	free(win);
	epx_object_unlink(&backend->window_list, ewin);
	ewin->opaque  = NULL;
	ewin->backend = NULL;
	return 0;
    }
    return -1;
}

/* Process mouse and keybord events, called from driver_select. 
 * return -1: error in event processing
 *         0: no event returned  & no pending
 *         1: one event returned & no pending
 *         2: one event returned & 1 pending
 *         and so on
 */

static int none_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    (void) backend;
    (void) e;

    return -1;
}

static int none_adjust(epx_backend_t *be, epx_dict_t* param)
{
    (void) be;
    (void) param;
    return 1;
}

static int none_win_adjust(epx_window_t *win, epx_dict_t* param)
{
    (void) win;
    (void) param;
    return 1;
}

static int none_info(epx_backend_t *be, epx_dict_t* param)
{
    (void) be;
    (void) param;
    return 0;
}

static int none_win_info(epx_window_t *window, epx_dict_t* param)
{
    (void) window;
    (void) param;
    return 0;
}
