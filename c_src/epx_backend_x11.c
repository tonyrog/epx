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
/*
 *  X display driver 
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>
#if defined(__APPLE__)
#include <machine/endian.h>
#else
#include <endian.h>
#endif
#include <memory.h>

#define EPX_HANDLT_T int
#include "../include/epx_backend.h"
#ifdef HAVE_OPENGL
#include <GL/glx.h>

extern int epx_gl_load_texture(epx_pixmap_t* pic, GLuint* textureName,
			       int useAlpha, int useClient, GLuint wrap,
			       int src_x, int src_y,
			       unsigned int width,unsigned int height);
#endif


/* This structure is stored in epx_window_t opaque field */
typedef struct {
    Window window;
    Pixmap pm;     /* off-screen pixmap */
    int pm_height;
    int pm_width;
    GC gc;
    int x;
    int y;
    int height;
    int width;
    int grabbed;
    int accel_num;
    int accel_den;
    int thres;
    epx_rect_t dirty;
#ifdef HAVE_OPENGL
    GLXContext context;
    GLuint textureName;
#endif
} X11Window;

typedef struct {
    epx_backend_t b;                /* DO NOT MOVE !!! */
    Display* display;
    int      screen;
    Visual*  visual;
    Atom     wm_delete_window; /* atom WM_DELETE_WINDOW */
    unsigned short modstate;   /* modifier state */
    unsigned short grab_key;   /* grab key */
    int      use_off_screen;   /* use off screen pixmap */
    int      use_exposure;     /* use backing store or not */
    unsigned long black_pixel;    
    unsigned long white_pixel;    
#ifdef HAVE_OPENGL
    GLXFBConfig* fbconfigs;    // Fixme XFree!
    int          fbnumconfigs;
#endif
} X11Backend;


epx_backend_t* x11_init(epx_dict_t* param);
int x11_upgrade(epx_backend_t* be);

static int x11_finish(epx_backend_t*);
static int x11_pixmap_attach(epx_backend_t*, epx_pixmap_t*);
static int x11_pixmap_detach(epx_backend_t*, epx_pixmap_t*);
static int x11_draw_begin(epx_window_t*);
static int x11_draw_end(epx_window_t*,int off_screen);
static int x11_draw(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
		    int src_x, int src_y, int dst_x, int dst_y,
		    unsigned int width,
		    unsigned int height);
static int x11_pix_sync(epx_backend_t*, epx_pixmap_t*, epx_window_t*);
static int x11_window_attach(epx_backend_t*, epx_window_t*);
static int x11_window_detach(epx_backend_t*, epx_window_t*);
static int x11_win_swap(epx_backend_t*, epx_window_t*);
static EPX_HANDLE_T x11_evt_attach(epx_backend_t*);
static int x11_evt_detach(epx_backend_t*);
static int x11_evt_read(epx_backend_t*, epx_event_t*);
static int x11_adjust(epx_backend_t *backend, epx_dict_t* param);
static int x11_window_adjust(epx_window_t*, epx_dict_t* param);
static int x11_info(epx_backend_t*, epx_dict_t* param);
static int x11_window_info(epx_window_t*, epx_dict_t* param);
/* util */
static void x11_grab(epx_backend_t* backend, epx_window_t* window, int toggle);
static int  x11_error(Display * dpy, XErrorEvent * ev);


static epx_callbacks_t x11_callbacks =
{
    .finish     = x11_finish,
    .pix_attach = x11_pixmap_attach,
    .pix_detach = x11_pixmap_detach,
    .pix_draw   = x11_draw,
    .pix_sync   = x11_pix_sync,
    .win_attach = x11_window_attach,
    .win_detach = x11_window_detach,
    .evt_attach = x11_evt_attach,
    .evt_detach = x11_evt_detach,
    .evt_read   = x11_evt_read,
    .adjust     = x11_adjust,
    .win_swap   = x11_win_swap,
    .begin      = x11_draw_begin,
    .end        = x11_draw_end,
    .win_adjust = x11_window_adjust,
    .info       = x11_info,
    .win_info   = x11_window_info
};

#define NUM_LOCK_MASK    0x00000002
#define CAPS_LOCK_MASK   0x00000001
#define SCROLL_LOCK_MASK 0x00000004

#ifdef HAVE_OPENGL

/* FIXME mixin to epx_dict_t parameters! */
int glxAttributes[] = {
    GLX_RENDER_TYPE, GLX_RGBA_BIT,
    GLX_DOUBLEBUFFER,  1,
    GLX_DEPTH_SIZE,    16, 
    GLX_RED_SIZE,      8, 
    GLX_GREEN_SIZE,    8, 
    GLX_BLUE_SIZE,     8,
    None
};
#endif

static int WaitForNotify(Display *dpy, XEvent *event, XPointer arg)
{
    (void) dpy;
    return (event->type == MapNotify) && (event->xmap.window == (Window) arg);
}

static epx_window_t* find_event_window(X11Backend* x11, Window w)
{
    epx_window_t* window;
    
    epx_lock_lock(x11->b.window_list.lock);
    window = (epx_window_t*) x11->b.window_list.first;

    while(window) {
	X11Window* w11 = (X11Window*) window->opaque;
	if (w11->window == w) {
	    epx_lock_unlock(x11->b.window_list.lock);
	    return window;
	}
	window = window->next;
    }
    epx_lock_unlock(x11->b.window_list.lock);
    return NULL;
}

#if 0
static void clip_window(X11Backend* b, X11Window* w,
			int x, int y, unsigned int width, unsigned int height)
{
    XRectangle clip;
    clip.x = x;
    clip.y = y;
    clip.width = width;
    clip.height = height;
    XSetClipRectangles(b->display, w->gc, 0, 0, &clip, 1, Unsorted);
}
#endif

// number of bits set in mask
static int bit_count(unsigned long mask)
{
    int i = 0;
    while(mask) {
	if (mask & 1)
	    i++;
	mask >>= 1;
    }
    return i;
}
// first high bit set
static int bit_pos(unsigned long mask)
{
    int i = 31;
    while(mask) {
	if (mask >> 31)
	    return i;
	mask <<= 1;
	i--;
    }
    return -1;
}

/* scan trough a screen and locate a "nice" visual  */
static int make_screen_formats(Screen* screen, epx_format_t* efmt,
			       size_t efmt_size)
{
    int i;
    size_t efmt_loaded = 0;

    for (i = 0; i < screen->ndepths; i++) {
	int j;
	Depth* d = &screen->depths[i];

	for (j = 0; j < d->nvisuals; j++) {
	    Visual* v = &d->visuals[j];
	    int r_size = bit_count(v->red_mask);
	    int g_size = bit_count(v->green_mask);
	    int b_size = bit_count(v->blue_mask);
	    int r_offs = bit_pos(v->red_mask);
	    int g_offs = bit_pos(v->green_mask);
	    int b_offs = bit_pos(v->blue_mask);
	    int fmt = 0;
	    int bgr = 0;
	    int alpha = 0;
	    int alpha_first = 0;
	    int little_endian = 0;
	    int bits_per_pixel = 0;
	    epx_format_t f;
	    int c;
	    int k;

#if defined(__cplusplus) || defined(c_plusplus)
	    c = v->c_class;
#else
	    c = v->class;
#endif
	    if (((c != TrueColor) && (c != DirectColor))) 
		continue;
#if BYTE_ORDER == LITTLE_ENDIAN
	    little_endian = 1;  // stored in native format
#endif
	    bgr = (b_offs > g_offs);
	    if ((v->bits_per_rgb == 5) && (d->depth == 15)) {
		fmt = EPX_FMT_RGB5;
		bits_per_pixel = 16;
	    }
	    else if ((v->bits_per_rgb == 5) && (d->depth == 16)) {
		if ((r_size == 5) && (g_size == 5) && (b_size == 5)) {
		    fmt = EPX_FMT_RGB5;
		    alpha = 1;
		    alpha_first = (r_offs == 14);
		}
		else if ((r_size == 5) && (g_size == 6) && (b_size == 5))
		    fmt = EPX_FMT_RGB565;
		bits_per_pixel = 16;
	    }
	    else if ((v->bits_per_rgb == 8) && (d->depth == 24)) {
		fmt = EPX_FMT_RGB8;
		little_endian = 0;  // ignore endian in this case?
		bits_per_pixel = 24;
	    }
	    else if ((v->bits_per_rgb == 8) && (d->depth == 32)) {
		fmt = EPX_FMT_RGB8;
		alpha = 1;
		alpha_first = (r_offs == 23);
		bits_per_pixel = 32;
	    }

	    f = EPX_FMT(fmt,bgr,alpha,alpha_first,little_endian,bits_per_pixel);
	    for (k = 0; k < (int)efmt_loaded; k++) {
		if (efmt[k] == f)
		    break;
	    }
	    if (k == (int)efmt_loaded) {
		if (efmt_loaded < efmt_size) {
		    efmt[k] = f;
		    efmt_loaded++;
		}
	    }
	}
    }
    return (int) efmt_loaded;
}
// #endif

static void init_off_screen(X11Window* w)
{
    w->pm = 0;
    w->pm_width  = 0;
    w->pm_height = 0;
}

static int create_off_screen(X11Backend* b, X11Window* w)
{
    if (b->use_off_screen) {
	w->pm = XCreatePixmap(b->display, w->window,
			      w->width, w->height,
			      DefaultDepth(b->display, b->screen));
	if (w->pm == 0) return -1;
	w->pm_width  = w->width;
	w->pm_height = w->height;
    }
    return 0;
}

static int destroy_off_screen(X11Backend* b, X11Window* w)
{
    if (w->pm == 0) return -1;
    XFreePixmap(b->display, w->pm);
    init_off_screen(w);
    return 0;
}

static int resize_off_screen(X11Backend* b, X11Window* w)
{
    Pixmap pm;
    int width = w->width;
    int height = w->height;
    int ww, hh;

    if (w->pm == 0) return -1;
    if ((w->pm_width >= width) && (w->pm_height >= height))
	return 0; // ok do never shrink (maybe later)
    ww = (width > w->pm_width) ? width+(width/2) : w->pm_width;
    hh = (height > w->pm_height) ? height+(height/2) : w->pm_height;
    // printf("pm: w=%d,h=%d\r\n", ww, hh);
    pm = XCreatePixmap(b->display, w->window, ww, hh, 
		       DefaultDepth(b->display, b->screen));
    XCopyArea(b->display, w->pm, pm, w->gc, 0, 0, w->pm_width, w->pm_height,
	      0, 0);
    XFreePixmap(b->display, w->pm);
    w->pm = pm;
    w->pm_width  = ww;
    w->pm_height = hh;
    return 0;
}

epx_backend_t* x11_init(epx_dict_t* param)
{
    X11Backend* be;
    unsigned int state;
    // Screen* screen;
    char* display_name = NULL;
    size_t len;

    // Must be the first XLib call.
    XInitThreads();

    if ((be = (X11Backend*) malloc(sizeof(X11Backend))) == NULL)
	return NULL;
    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.owner = NULL;
    be->b.user = NULL;
    be->b.name = "x11";
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;    
    be->b.width = 0;
    be->b.height = 0;
    be->b.nformats = 0;
    epx_object_list_init(&be->b.window_list);
    epx_object_list_init(&be->b.pixmap_list);
    be->b.event = EPX_INVALID_HANDLE;
    be->b.cb = &x11_callbacks;    
#ifdef HAVE_OPENGL
    be->fbconfigs = NULL;
#endif

    epx_dict_lookup_string(param, "display", &display_name, &len);

    if ((be->display = XOpenDisplay(display_name)) == NULL) {
	free(be);
	return NULL;
    }

    be->b.use_opengl = 0;
    be->use_off_screen = 1;
    be->use_exposure = 0;

    be->screen   = DefaultScreen(be->display);
    be->b.width  = DisplayWidth(be->display, be->screen);
    be->b.height = DisplayHeight(be->display, be->screen);
    be->white_pixel = XWhitePixel(be->display,be->screen);
    be->black_pixel = XBlackPixel(be->display,be->screen);
    be->b.nformats = 
	make_screen_formats(ScreenOfDisplay(be->display,be->screen),
			    be->b.formats, EPX_BACKEND_MAX_FORMATS);

    x11_adjust((epx_backend_t*)be, param);

    XSetErrorHandler(x11_error);
    // screen = ScreenOfDisplay(be->display, be->screen);

    be->wm_delete_window = XInternAtom(be->display,"WM_DELETE_WINDOW",False);

    /* Fixme locate a proper visual */
    /* x11->visual = find_visual_of_screen(screen); */
    be->visual = XDefaultVisual(be->display, be->screen);
    be->modstate = 0;
    be->grab_key = 0;

    if(XkbGetIndicatorState (be->display,XkbUseCoreKbd,&state) == Success) {
	EPX_DBGFMT("initial modstate=%x", state);
	if (state & NUM_LOCK_MASK)
	    be->modstate |= EPX_KBD_MOD_NUM;
	if (state & CAPS_LOCK_MASK)
	    be->modstate |= EPX_KBD_MOD_CAPS;
	if (state & SCROLL_LOCK_MASK)
	    be->modstate |= EPX_KBD_MOD_SCR;
	EPX_DBGFMT("x11 modstate=%x\n", be->modstate);
    }
    return (epx_backend_t*) &(be->b);
}

int x11_upgrade(epx_backend_t* backend)
{
    backend->cb = &x11_callbacks;
    return 0;
}


/* return the backend event handle */
static EPX_HANDLE_T x11_evt_attach(epx_backend_t* backend)
{
    X11Backend* b = (X11Backend*) backend;
    int fd = ConnectionNumber(b->display);

    return (EPX_HANDLE_T)((long)fd);
}

static int x11_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

static int x11_finish(epx_backend_t* backend)
{
    X11Backend* b = (X11Backend*) backend;
    // FIXME: close all windows & detach all pixmaps
    XCloseDisplay(b->display);
    free(b);
    return 0;
}

static int x11_pixmap_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    X11Backend* b = (X11Backend*) backend;
    XImage* ximg;
    unsigned int bytes_per_row   = pixmap->bytes_per_row;
    unsigned int bits_per_pixel  = pixmap->bits_per_pixel;

    if (pixmap->opaque != NULL)
	return -1;

    if ((ximg = (XImage*) calloc(1,sizeof(XImage))) == NULL)
	return -1;

    ximg->width  = pixmap->width;
    ximg->height = pixmap->height;
    ximg->xoffset = 0;
    ximg->format = ZPixmap;
    ximg->data   = (char*) pixmap->data;
    ximg->bitmap_bit_order = MSBFirst;
    switch(pixmap->pixel_format) {
    case EPX_FORMAT_RGB:
	ximg->byte_order  = MSBFirst;
	ximg->bitmap_unit = 8;
	ximg->bitmap_pad  = 8;
	break;
    case EPX_FORMAT_BGR:
	ximg->byte_order  = LSBFirst;
	ximg->bitmap_unit = 8;
	ximg->bitmap_pad  = 8;
	break;
    case EPX_FORMAT_RGBA:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = LSBFirst;
#else
	ximg->byte_order  = MSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    case EPX_FORMAT_ARGB:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = LSBFirst;
#else
	ximg->byte_order  = MSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    case EPX_FORMAT_BGRA:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = MSBFirst;
#else
	ximg->byte_order  = LSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    case EPX_FORMAT_ABGR:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = LSBFirst;
#else
	ximg->byte_order  = MSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    default:
	break;
    }
    ximg->depth = 24;                     /* ...*/
    ximg->bytes_per_line = bytes_per_row;
    ximg->bits_per_pixel = bits_per_pixel;
    ximg->red_mask   = 0xFF << 16;
    ximg->green_mask = 0xFF <<  8;
    ximg->blue_mask  = 0xFF <<  0;

    XInitImage(ximg);

    EPX_DBGFMT( "        width=%d",   ximg->width);
    EPX_DBGFMT( "       height=%d",  ximg->height);
    EPX_DBGFMT( "      xoffset=%d", ximg->xoffset);
    EPX_DBGFMT( "       format=%s", 
	    ((ximg->format==XYBitmap)?"XYBitmap":
	     ((ximg->format==XYPixmap)?"XYPixmap":
	      ((ximg->format==ZPixmap)?"ZPixmap":"?"))));
    EPX_DBGFMT(      "byte_order=%s", 
	    ( (ximg->byte_order==LSBFirst) ? "LSBFirst" :
	      ((ximg->byte_order==MSBFirst) ?  "MSBFirst" : "?")));
    EPX_DBGFMT( "     bitmap_unit=%d", ximg->bitmap_unit);
    EPX_DBGFMT( "bitmap_bit_order=%s", 
	    ( (ximg->bitmap_bit_order==LSBFirst) ? "LSBFirst" :
	      ((ximg->bitmap_bit_order==MSBFirst) ? "MSBFirst" : "?")));
    EPX_DBGFMT( "     bitmap_pad=%d", ximg->bitmap_pad);
    EPX_DBGFMT( "          depth=%d", ximg->depth);
    EPX_DBGFMT( " bytes_per_line=%d", ximg->bytes_per_line);
    EPX_DBGFMT( " bits_per_pixel=%d", ximg->bits_per_pixel);
    EPX_DBGFMT( "       red_mask=%lX", ximg->red_mask);
    EPX_DBGFMT( "     green_mask=%lX", ximg->green_mask);
    EPX_DBGFMT( "      blue_mask=%lX", ximg->blue_mask);
    EPX_DBGFMT( "         obdata=%p", ximg->obdata);

    epx_object_link(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) ximg;
    pixmap->backend = (epx_backend_t*) b;
    return 0;
}

static int x11_pixmap_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    XImage* ximg = (XImage*) pixmap->opaque;

    if (ximg != NULL) {
	free(ximg);
	epx_object_unlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}

static int x11_draw_begin(epx_window_t* window)
{
    if (window->opengl) {
#ifdef HAVE_OPENGL
	X11Backend* b  = (X11Backend*) window->backend;
	X11Window*  w  = (X11Window*) window->opaque;
	
	glXMakeCurrent(b->display, w->window, w->context);
#endif
    }
    return 0;
}

static int x11_draw_end(epx_window_t* window, int off_screen)
{
    // X11Window*  w = (X11Window*) window->opaque;
    X11Backend* b  = (X11Backend*) window->backend;

    if (window->opengl) {
#ifdef HAVE_OPENGL
	glFlush();
	if (!off_screen)
	    glXSwapBuffers(b->display, w11->window);
#endif
    }
    else if (off_screen)
	return 0;
    else if (!b->use_off_screen) {
	XSync(b->display, False);
	b->b.pending = XEventsQueued(b->display, QueuedAlready);
    }
    return 0;
}

static int x11_draw(epx_backend_t* backend, epx_pixmap_t* pixmap,
		    epx_window_t* window,
		    int src_x, int src_y, int dst_x, int dst_y,
		    unsigned int width,
		    unsigned int height)
{
    X11Backend* b = (X11Backend*) backend;
    X11Window*  w = (X11Window*) window->opaque;
    XImage* ximg = (XImage*) pixmap->opaque;

    if (w == NULL)
	return -1;
    if (window->opengl) {
#ifdef HAVE_OPENGL
	// Fixme check for errors
	epx_gl_load_texture(pixmap, &w->textureName, 0, 1, GL_REPEAT,
			    src_x, src_y, width, height);
	glEnable(GL_TEXTURE_RECTANGLE_ARB); // _EXT enable texturing
	glBindTexture (GL_TEXTURE_RECTANGLE_ARB, w->textureName);
	glBegin(GL_QUADS); {
	    float x0 = 0.0f;
	    float y0 = 0.0f;
	    float x1 = width -  1;
	    float y1 = height - 1;
	    glTexCoord2f(x0,y0); glVertex2f(x0,y0);
	    glTexCoord2f(x0,y1); glVertex2f(x0,y1);
	    glTexCoord2f(x1,y1); glVertex2f(x1,y1);
	    glTexCoord2f(x1,y0); glVertex2f(x1,y0);
	}
	glEnd();
	glDisable(GL_TEXTURE_RECTANGLE_ARB); // EXT);
#endif
    }
    else {
	Drawable d = (b->use_off_screen && w->pm) ? w->pm : w->window;
	XPutImage(b->display, d, 
		  w->gc, ximg, src_x, src_y, dst_x, dst_y,
		  width, height);
    }
    return 0;
}

// make sure all draw command have been synced
static int x11_pix_sync(epx_backend_t* backend, epx_pixmap_t* pixmap,
			epx_window_t* window)
{
    X11Backend* xbackend = (X11Backend*) backend;
    (void) pixmap;
    (void) window;

    return XSync(xbackend->display, False);
}

static int x11_win_swap(epx_backend_t* backend, epx_window_t* window)
{
    X11Backend* b = (X11Backend*) backend;
    X11Window*  w = (X11Window*) window->opaque;

    if (window->opengl) {
#ifdef HAVE_OPENGL
	glXMakeCurrent(be->display, w->window, w->context);
	glXSwapBuffers(be->display, w->window);
#endif
    }
    else if (w && w->pm && b->use_off_screen) {
	// printf("XCopyArea: w=%d,h=%d\r\n", w->width, w->height);
	XCopyArea(b->display, w->pm, w->window, w->gc,
		  0, 0, w->width, w->height, 0, 0);
	XSync(b->display, False);
    }
    else {
	XSync(b->display, False);
    }
    backend->pending = XEventsQueued(b->display, QueuedAlready);
    return 0;
}

static int x11_window_attach(epx_backend_t* backend, epx_window_t* window)
{
    X11Backend* be = (X11Backend*) backend;
    X11Window* xwin = NULL;
    Window win = 0;
    XSetWindowAttributes attr;
    unsigned long valuemask;

    if (window->opaque != NULL)
	return -1;
    if ((xwin = (X11Window*) malloc(sizeof(X11Window))) == NULL)
	return -1;
    memset(xwin, 0, sizeof(X11Window));
	
    attr.event_mask =
	  KeyPressMask
	| KeyReleaseMask
	| ButtonPressMask
	| ButtonReleaseMask
	| EnterWindowMask
	| LeaveWindowMask
	| PointerMotionMask
	// | PointerMotionHintMask | Button1MotionMask
	// | Button2MotionMask | Button3MotionMaskCWEventMask
	// | Button4MotionMask | Button5MotionMask
	// | ButtonMotionMask
	| KeymapStateMask
	| VisibilityChangeMask
	| StructureNotifyMask
	// | ResizeRedirectMask
	| SubstructureNotifyMask
	| SubstructureRedirectMask
	| FocusChangeMask
	| PropertyChangeMask
	| ColormapChangeMask
	| OwnerGrabButtonMask
	| ExposureMask
	;
    attr.backing_store = Always;        /* auto expose */
    attr.save_under = True;		/* popups ... */
    attr.background_pixel = be->white_pixel;
    attr.border_pixel = be->black_pixel;
    attr.override_redirect = False;

    valuemask =
	CWBackPixel |
	CWBorderPixel |
	CWSaveUnder | 
	CWOverrideRedirect |
	CWEventMask;

    if (!be->use_exposure)
	valuemask |= CWBackingStore;

    if (be->b.opengl && be->b.use_opengl) {
#ifdef HAVE_OPENGL
	XVisualInfo *vinfo;

	vinfo = glXGetVisualFromFBConfig(be->display, be->fbconfigs[0]);
	attr.border_pixel = 0;
	// FIXME save in be - need to release colormap / resuse!?
	attr.colormap = XCreateColormap(be->display,
					RootWindow(be->display, vinfo->screen),
					vinfo->visual, AllocNone);
	valuemask |= CWColormap;
	win = XCreateWindow(be->display, 
			    RootWindow(be->display, vinfo->screen),
			    window->area.xy.x, /* x */
			    window->area.xy.y,	/* y */
			    window->area.wh.width,	/* width */
			    window->area.wh.height,	/* height */
			    2,		/* border */
			    vinfo->depth,	/* depth */
			    InputOutput,	/* class */
			    vinfo->visual,	/* Visual */
			    valuemask,	        /* valuemask */
			    &attr		/* attributes */
	    );
	if (win) {
	    /* xwin->context = glXCreateNewContext(be->display,
						be->fbconfigs[0], 
						GLX_RGBA_TYPE,
						NULL, True ); */
	    xwin->context = glXCreateContext(be->display, vinfo, NULL, True);
	    if (xwin->context)
		ewin->opengl = 1;
	}
#endif
    }

    if (!win) {
	win = XCreateWindow(be->display, 
			    XDefaultRootWindow(be->display), 
			    window->area.xy.x,
			    window->area.xy.y,
			    window->area.wh.width,
			    window->area.wh.height,
			    2,
			    CopyFromParent,
			    InputOutput,
			    be->visual,
			    valuemask,
			    &attr);
    }

    if (win) {
	XEvent event;
	// XGCValues values;
	
	xwin->grabbed = 0;
	xwin->accel_num = 0;
	xwin->accel_den = 0;
	xwin->thres = 0;
	xwin->window = win;
	xwin->x = window->area.xy.x;
	xwin->y = window->area.xy.y;	
	xwin->width = window->area.wh.width;
	xwin->height = window->area.wh.height;
	// Create a copy of the default screen GC set clip=None
	xwin->gc = XCreateGC(be->display, win, 0, NULL);
	// XCopyGC(be->display, XDefaultGC(be->display, be->screen),
	//	(unsigned long)-1, xwin->gc);
	XSetBackground(be->display, xwin->gc, be->white_pixel);
	XSetForeground(be->display, xwin->gc, be->black_pixel);
	// values.clip_mask = None;
	// XChangeGC(be->display, xwin->gc, GCClipMask, &values);
#if 0
	XSelectInput(be->display, win,
		     KeyPressMask
		     | KeyReleaseMask
		     | ButtonPressMask
		     | ButtonReleaseMask
		     | EnterWindowMask
		     | LeaveWindowMask
		     | PointerMotionMask
		     // | PointerMotionHintMask | Button1MotionMask
		     // | Button2MotionMask | Button3MotionMask
		     // | Button4MotionMask | Button5MotionMask
		     // | ButtonMotionMask
		     | KeymapStateMask
		     | VisibilityChangeMask
		     | StructureNotifyMask
		     // | ResizeRedirectMask
		     | SubstructureNotifyMask
		     | SubstructureRedirectMask
		     | FocusChangeMask
		     | PropertyChangeMask
		     | ColormapChangeMask
		     | OwnerGrabButtonMask
		     | ExposureMask
	    );
#endif
	init_off_screen(xwin);
	create_off_screen(be, xwin);
	
	XSetWMProtocols(be->display, win, &be->wm_delete_window, 1);

	XMapWindow(be->display, win);
	XIfEvent(be->display, &event, WaitForNotify, (XPointer) win );
	// XSync(x11->display, False);
	epx_object_link(&backend->window_list, window);
	window->opaque  = (void*) xwin;
	window->backend = (epx_backend_t*) be;
	return 0;
    }
    free(xwin);
    return -1;
}

static int x11_window_detach(epx_backend_t* backend, epx_window_t* window)
{
    X11Backend* be = (X11Backend*) backend;
    X11Window*  xwin = (X11Window*) window->opaque;
    
    if ((xwin != NULL) && (xwin->window != 0)) {
	EPX_DBGFMT("XUnmapWindow");
	XUnmapWindow(be->display, xwin->window);
	EPX_DBGFMT("XDestroyWindow");
	destroy_off_screen(be, xwin);
	if (window->opengl) {
#ifdef HAVE_OPENGL
	    glXDestroyContext(be->display, xwin->context);
	    if (be->fbconfigs != NULL)
		XFree(be->fbconfigs);
#endif
	}
	XFreeGC(be->display, xwin->gc);
	XDestroyWindow(be->display, xwin->window);
	XFlush(be->display);
	/* Ungrabb? */
	free(xwin);
	epx_object_unlink(&backend->window_list, window);
	window->opaque  = NULL;
	window->backend = NULL;
	return 0;
    }
    return -1;
}

// We could filter here, but then we will leave events in the queue
// I guess we prefere to save unprocessed events elsewhere?
static Bool CheckEvent (Display* dpy, XEvent* ev, XPointer arg)
{
    // u_int32_t mask = (u_int32_t) arg;
    (void) arg;
    (void) dpy;
    (void) ev;
    return True;
}

// Button4
#ifndef WheelUpMask
#define WheelUpMask  (1 << 11)
#endif

// Button5
#ifndef WheelDownMask
#define WheelDownMask  (1 << 12)
#endif

// Button6
#ifndef WheelLeftMask
#define WheelLeftMask  (1 << 13)
#endif

// Button7
#ifndef WheelRightMask
#define WheelRightMask  (1 << 14)
#endif

// Map button state to epx_event_t button
u_int32_t Buttons(unsigned int state)
{
    u_int32_t button = 0;
    if (state & Button1Mask) button |= EPX_BUTTON_LEFT;
    if (state & Button2Mask) button |= EPX_BUTTON_MIDDLE;
    if (state & Button3Mask) button |= EPX_BUTTON_RIGHT;
    if (state & WheelUpMask) button |= EPX_WHEEL_UP;
    if (state & WheelDownMask) button |= EPX_WHEEL_DOWN;
    if (state & WheelLeftMask) button |= EPX_WHEEL_LEFT;
    if (state & WheelRightMask) button |= EPX_WHEEL_RIGHT;
    return button;
}

static int FilterEvent(epx_event_t* e)
{
    u_int32_t mask;

    if (!e->window)
	return 1;
    if (e->type == EPX_EVENT_CLOSE)
	return 0;
    mask = e->window->mask;
    if ((e->type & mask) == 0)
	return 1;
    switch(e->type) {
    case EPX_EVENT_BUTTON_PRESS:
    case EPX_EVENT_BUTTON_RELEASE:
    case EPX_EVENT_POINTER_MOTION:
	mask &= EPX_EVENT_BUTTON_MASK; // only button events
	if (mask == EPX_EVENT_BUTTON_ANY)
	    return 0;
	if ((e->pointer.button & (mask >> 16)) == 0)
	    return 1;
	break;
    default:
	break;
    }
    return 0;
}

/* Process mouse and keybord events, called from driver_select. 
 * return -1: error in event processing
 *         0: no event returned  & no pending
 *         1: one event returned & no pending
 *         2: one event returned & 1 pending
 *         and so on
 *
 */

static int x11_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    X11Backend* b = (X11Backend*) backend;
    XEvent ev;

    backend->pending = 0;

next:
    if (!XCheckIfEvent(b->display, &ev, CheckEvent, (XPointer)0))
	return -1;
    switch (ev.type) {
    case CreateNotify:
	EPX_DBGFMT("Event: CreateNotify");
	break;
    case DestroyNotify:
	EPX_DBGFMT("Event: DestroyNotify");
	e->window = find_event_window(b, ev.xdestroywindow.window);
	e->type = EPX_EVENT_DESTROYED;
	goto got_event;

    case ClientMessage:
	EPX_DBGFMT("Event: ClientMessage");
	e->window = find_event_window(b, ev.xclient.window);
	if ((ev.xclient.data.l[0] != (int) b->wm_delete_window))
	    break;
	EPX_DBGFMT("Event: WM_DELETE_WINDOW");
	e->type = EPX_EVENT_CLOSE;
	goto got_event;

    case ConfigureNotify: {
	epx_window_t* w;
	EPX_DBGFMT("Event: ConfigureNotity x=%d, y=%d, w=%d, h=%d",
		ev.xconfigure.x,ev.xconfigure.y,
		ev.xconfigure.width, ev.xconfigure.height);
	if ((w = find_event_window(b, ev.xconfigure.window)) != NULL) {
	    X11Window*  win  = (X11Window*) w->opaque;
	    EPX_DBGFMT("found window %p, win=%p", w, win);
	    e->window = w;
	    e->type = EPX_EVENT_CONFIGURE;
	    e->area.x = ev.xconfigure.x;
	    e->area.y = ev.xconfigure.y;
	    e->area.w = ev.xconfigure.width;
	    e->area.h = ev.xconfigure.height;
	    // update reported values
	    w->rarea.xy.y = e->area.x;
	    w->rarea.xy.y = e->area.y;
	    w->rarea.wh.width = e->area.w;
	    w->rarea.wh.height = e->area.h;

	    // update using
	    win->width  = ev.xconfigure.width;
	    win->height = ev.xconfigure.height;
	    resize_off_screen(b, win);
	    goto got_event;
	}
	else {
	    EPX_DBGFMT("no window found");
	}
	break;
    }

    case ResizeRequest: {
	epx_window_t* w;
	EPX_DBGFMT("Event: ResizeRequest w=%d, h=%d",
		ev.xresizerequest.width,ev.xresizerequest.height);
	w = find_event_window(b, ev.xresizerequest.window);
	if (w != NULL) {
	    X11Window*  win  = (X11Window*) w->opaque;
	    e->window = w;
	    e->type = EPX_EVENT_RESIZE;
	    e->dimension.w = ev.xresizerequest.width;
	    e->dimension.h = ev.xresizerequest.height;
	    e->dimension.d = 0;
	    // update reported values
	    w->rarea.wh.width = e->dimension.w;
	    w->rarea.wh.height = e->dimension.h;
	    // update using
	    win->width  = ev.xresizerequest.width;
	    win->height = ev.xresizerequest.height;
	    resize_off_screen(b, win);
	    goto got_event;
	}
	break;
    }

    case KeymapNotify:
	EPX_DBGFMT("Event: KeymapNotify");
	break;
	
    case Expose:  {
	epx_window_t* w;
	EPX_DBGFMT("Event: Expose x=%d, y=%d, w=%d, h=%d cnt=%d",
		   ev.xexpose.x,ev.xexpose.y,
		   ev.xexpose.width, ev.xexpose.height,
		   ev.xexpose.count);
	if ((w = find_event_window(b, ev.xexpose.window)) != NULL) {
	    X11Window* win  = (X11Window*) w->opaque;
	    epx_rect_t dirty;
	    
	    epx_rect_set(&dirty,
			 ev.xexpose.x, ev.xexpose.y,
			 ev.xexpose.width, ev.xexpose.height);
	    epx_rect_union(&win->dirty, &dirty, &win->dirty);

	    if (ev.xexpose.count > 0)
		break;
	    if (b->use_off_screen && win->pm && !b->use_exposure) {
		XCopyArea(b->display, win->pm, win->window, win->gc,
			  win->dirty.xy.x, win->dirty.xy.y,
			  win->dirty.wh.width, win->dirty.wh.height,
			  win->dirty.xy.x, win->dirty.xy.y);
	    }
	    e->window = w;
	    e->type = EPX_EVENT_EXPOSE;
	    e->area.x = win->dirty.xy.x;
	    e->area.y = win->dirty.xy.y;
	    e->area.w = win->dirty.wh.width;
	    e->area.h = win->dirty.wh.height;
	    epx_rect_empty(&win->dirty);
	    goto got_event;
	}
	break;
    }

    case GraphicsExpose:
	EPX_DBGFMT("Event: GraphicsExpose");
	break;
    case NoExpose:
	// EPX_DBGFMT("Event: NoExpose");
	break;
    case VisibilityNotify:
	EPX_DBGFMT("Event: VisibilityNotify");
	break;
    case UnmapNotify:
	EPX_DBGFMT("Event: UnmapNotify");
	break;
    case MapNotify:
	EPX_DBGFMT("Event: MapNotify");
	break;
    case MapRequest:
	EPX_DBGFMT("Event: MapRequest");
	break;
    case ReparentNotify:
	EPX_DBGFMT("Event: ReparentNotify");
	break;
    case ConfigureRequest:
	EPX_DBGFMT("Event: ConfigureRequest");
	break;
    case GravityNotify:
	EPX_DBGFMT("Event: GravityNotify");
	break;

    case CirculateNotify:
	EPX_DBGFMT("Event: CirculateNotify");
	break;
    case CirculateRequest:
	EPX_DBGFMT("Event: CirculateRequest");
	break;
    case PropertyNotify:
	EPX_DBGFMT("Event: PropertyNotify %s",
		XGetAtomName(b->display, ev.xproperty.atom));
	break;
    case SelectionClear:
	EPX_DBGFMT("Event: SelectionClear");
	break;
    case SelectionRequest:
	EPX_DBGFMT("Event: SelectionRequest");
	break;
    case SelectionNotify:
	EPX_DBGFMT("Event: SelectionNotify");
	break;
    case ColormapNotify:
	EPX_DBGFMT("Event: ColormapNotify");
	break;
    case MappingNotify:
	EPX_DBGFMT("Event: MappingNotify");
	break;
    case GenericEvent:
	EPX_DBGFMT("Event: GenericEvent");
	break;

    case EnterNotify:
	EPX_DBGFMT("Event: EnterNotity");
	e->window = find_event_window(b, ev.xcrossing.window);
	e->type = EPX_EVENT_ENTER;
	e->pointer.x = ev.xcrossing.x;
	e->pointer.y = ev.xcrossing.y;
	e->pointer.z = 0;
	goto got_event;
	
    case LeaveNotify:
	EPX_DBGFMT("Event: LeaveNotity");
	e->window = find_event_window(b, ev.xcrossing.window);
	e->type = EPX_EVENT_LEAVE;
	e->pointer.x = ev.xcrossing.x;
	e->pointer.y = ev.xcrossing.y;
	e->pointer.z = 0;
	goto got_event;

    case FocusIn:
	EPX_DBGFMT("Event: FocusIn");
	e->window = find_event_window(b, ev.xfocus.window);
	e->type = EPX_EVENT_FOCUS_IN;
	goto got_event;

    case FocusOut:
	EPX_DBGFMT("Event: FocusOut");
	e->window = find_event_window(b, ev.xfocus.window);
	e->type = EPX_EVENT_FOCUS_OUT;
	goto got_event;

    case MotionNotify:
	EPX_DBGFMT("Event: MotionNotify");
	e->window = find_event_window(b, ev.xmotion.window);
	e->pointer.button = Buttons(ev.xmotion.state);
	e->type = EPX_EVENT_POINTER_MOTION;
	e->pointer.x = ev.xmotion.x;
	e->pointer.y = ev.xmotion.y;
	e->pointer.z = 0;
	goto got_event;

    case ButtonPress:
	e->window = find_event_window(b, ev.xbutton.window);
	e->pointer.button = Buttons(1 << (ev.xbutton.button + 7));
	e->type = EPX_EVENT_BUTTON_PRESS;
	e->pointer.x = ev.xbutton.x;
	e->pointer.y = ev.xbutton.y;
	e->pointer.z = 0;
	goto got_event;
	
    case ButtonRelease:
	e->window = find_event_window(b, ev.xbutton.window);
	e->pointer.button = Buttons(1 << (ev.xbutton.button + 7));
	e->type = EPX_EVENT_BUTTON_RELEASE;
	e->pointer.x = ev.xbutton.x;
	e->pointer.y = ev.xbutton.y;
	e->pointer.z = 0;
	goto got_event;

    case KeyPress:
    case KeyRelease: {
	char ignored_char;
	//KeySym sym = XKeycodeToKeysym(b->display, ev.xkey.keycode, 0);
	KeySym sym = XkbKeycodeToKeysym(b->display, ev.xkey.keycode, 0, 0);
	
	if (sym == NoSymbol) {
	    EPX_DBGFMT("event sym=NoSymbol");
	    goto ignore_event;
	}
	e->window = find_event_window(b, ev.xkey.window);

	// remove retriggered events
	if ((ev.type == KeyRelease) && e->window &&
	    (e->window->mask & EPX_EVENT_NO_AUTO_REPEAT)) {
	    if (XEventsQueued(b->display, QueuedAfterReading)) {
		XEvent nev;
		XPeekEvent(b->display, &nev);
		if ((nev.type == KeyPress) &&
		    (nev.xkey.time == ev.xkey.time) &&
		    (nev.xkey.keycode == ev.xkey.keycode)) {
		    // delete retriggered KeyPress event
		    XNextEvent (b->display, &ev);
		    goto ignore_event;
		}
	    }
	}
	
	if (ev.type == KeyPress)
	    e->type = EPX_EVENT_KEY_PRESS;
	else if (ev.type == KeyRelease)
	    e->type = EPX_EVENT_KEY_RELEASE;
	else
	    break;

	/* calculate kbd modifiers*/
	if (b->modstate) {
	    EPX_DBGFMT("1:modstate=%x", b->modstate);
	}
	b->modstate &= (EPX_KBD_MOD_NUM|EPX_KBD_MOD_CAPS|EPX_KBD_MOD_SCR);
	if (ev.xkey.state) {
	    EPX_DBGFMT("key.state=%x", ev.xkey.state);
	}
//	if (ev.xkey.state & ShiftMask) {
//	    EPX_DBGFMT("shiftmask");
//	    b->modstate |= EPX_KBD_MOD_SHIFT;
//	}
	if (ev.xkey.state & LockMask) {
	    EPX_DBGFMT("lockmask (caps)");
	    b->modstate |= EPX_KBD_MOD_CAPS;
	}
//	if (ev.xkey.state & ControlMask) {
//	    EPX_DBGFMT("controlmask");
//	    b->modstate |= EPX_KBD_MOD_CTRL;
//	}
//	if (ev.xkey.state & Mod1Mask) {
//	    EPX_DBGFMT("mod1mask (alt)");
//	    b->modstate |= EPX_KBD_MOD_ALT;
//	}
	if (ev.xkey.state & Mod2Mask) {
	    EPX_DBGFMT("mod2mask (num)");
	    b->modstate |= EPX_KBD_MOD_NUM;
	}
	if (ev.xkey.state & Mod3Mask) {
	    EPX_DBGFMT("mod3mask");
	}
	if (ev.xkey.state & Mod4Mask) {
	    EPX_DBGFMT("mod4mask");
	}
	if (ev.xkey.state & Mod5Mask) {
	    EPX_DBGFMT("mod5mask (scr)");
	    // b->modstate |= EPX_KBD_MOD_SCR;
	}
	if (b->modstate) {
	    EPX_DBGFMT("2:modstate=%x", b->modstate);
	}
	switch (sym) {
	case XK_Escape: e->key.sym = EPX_KBD_KEY_ESCAPE; break;
	case XK_Delete: e->key.sym = EPX_KBD_KEY_DELETE; break;
	case XK_Home:   e->key.sym = EPX_KBD_KEY_HOME;   break;
	case XK_Left:   e->key.sym = EPX_KBD_KEY_LEFT;   break;
	case XK_Up:     e->key.sym = EPX_KBD_KEY_UP;     break;
	case XK_Right:  e->key.sym = EPX_KBD_KEY_RIGHT;  break;
	case XK_Down:   e->key.sym = EPX_KBD_KEY_DOWN;   break;
	case XK_Page_Up: e->key.sym = EPX_KBD_KEY_PAGEUP; break;
	case XK_Page_Down: e->key.sym = EPX_KBD_KEY_PAGEDOWN; break;
	case XK_End: e->key.sym = EPX_KBD_KEY_END;  break;
	case XK_Insert: e->key.sym = EPX_KBD_KEY_INSERT; break;
	case XK_Pause:
	case XK_Break: e->key.sym = EPX_KBD_KEY_QUIT; break;
	case XK_Print:
	case XK_Sys_Req: e->key.sym = EPX_KBD_KEY_PRINT; break;
	case XK_Menu: e->key.sym = EPX_KBD_KEY_MENU; break;
	case XK_Cancel: e->key.sym = EPX_KBD_KEY_CANCEL; break;
	case XK_KP_Enter: e->key.sym = EPX_KBD_KEY_KP_ENTER; break;
	case XK_KP_Home: e->key.sym = EPX_KBD_KEY_KP7; break;
	case XK_KP_Left: e->key.sym = EPX_KBD_KEY_KP4; break;
	case XK_KP_Up:   e->key.sym = EPX_KBD_KEY_KP8; break;
	case XK_KP_Right: e->key.sym = EPX_KBD_KEY_KP6; break;
	case XK_KP_Down:  e->key.sym = EPX_KBD_KEY_KP2; break;
	case XK_KP_Page_Up: e->key.sym = EPX_KBD_KEY_KP9; break;
	case XK_KP_Page_Down: e->key.sym = EPX_KBD_KEY_KP3; break;
	case XK_KP_End: e->key.sym = EPX_KBD_KEY_KP1; break;
	case XK_KP_Insert: e->key.sym = EPX_KBD_KEY_KP0; break;
	case XK_KP_Delete: e->key.sym = EPX_KBD_KEY_KP_PERIOD; break;
	case XK_KP_Equal:  e->key.sym = EPX_KBD_KEY_KP_EQUALS; break;
	case XK_KP_Multiply: e->key.sym = EPX_KBD_KEY_KP_MULTIPLY; break;
	case XK_KP_Add: e->key.sym = EPX_KBD_KEY_KP_PLUS; break;
	case XK_KP_Subtract: e->key.sym = EPX_KBD_KEY_KP_MINUS; break;
	case XK_KP_Decimal: e->key.sym = EPX_KBD_KEY_KP_PERIOD; break;
	case XK_KP_Divide: e->key.sym = EPX_KBD_KEY_KP_DIVIDE; break;
	case XK_KP_5:
	case XK_KP_Begin: e->key.sym = EPX_KBD_KEY_KP5; break;
	case XK_F1:  e->key.sym = EPX_KBD_KEY_F1;  break;
	case XK_F2:  e->key.sym = EPX_KBD_KEY_F2;  break;
	case XK_F3:  e->key.sym = EPX_KBD_KEY_F3;  break;
	case XK_F4:  e->key.sym = EPX_KBD_KEY_F4;  break;
	case XK_F5:  e->key.sym = EPX_KBD_KEY_F5;  break;
	case XK_F6:  e->key.sym = EPX_KBD_KEY_F6;  break;
	case XK_F7:  e->key.sym = EPX_KBD_KEY_F7;  break;
	case XK_F8:  e->key.sym = EPX_KBD_KEY_F8;  break;
	case XK_F9:  e->key.sym = EPX_KBD_KEY_F9;  break;
	case XK_F10: e->key.sym = EPX_KBD_KEY_F10; break;
	case XK_F11: e->key.sym = EPX_KBD_KEY_F11; break;
	case XK_F12: e->key.sym = EPX_KBD_KEY_F12;  break;
	    /* state modifiers*/
	case XK_Num_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		b->modstate ^= EPX_KBD_MOD_NUM;
	    goto no_key;
	case XK_Shift_Lock:
	case XK_Caps_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		b->modstate ^= EPX_KBD_MOD_CAPS;
	    goto no_key;
	case XK_Scroll_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		b->modstate ^= EPX_KBD_MOD_SCR;
	    goto no_key;
	case XK_Shift_L:
	    EPX_DBGFMT("shift left");
	    e->key.sym = EPX_KBD_KEY_LSHIFT;
	    b->modstate |= EPX_KBD_MOD_LSHIFT;
	    break;
	case XK_Shift_R: 
	    EPX_DBGFMT("shift right");
	    e->key.sym = EPX_KBD_KEY_RSHIFT;
	    b->modstate |= EPX_KBD_MOD_RSHIFT;
	    break;
	case XK_Control_L: 
	    EPX_DBGFMT("control left");
	    e->key.sym = EPX_KBD_KEY_LCTRL;
	    b->modstate |= EPX_KBD_MOD_LCTRL;
	    break;
	case XK_Control_R: 
	    EPX_DBGFMT("control right");
	    e->key.sym = EPX_KBD_KEY_RCTRL;
	    b->modstate |= EPX_KBD_MOD_RCTRL;
	    break;
	case XK_Alt_L:
	    e->key.sym = EPX_KBD_KEY_LALT;
	    b->modstate |= EPX_KBD_MOD_LALT;
	    break;
	case XK_Alt_R:
	    e->key.sym = EPX_KBD_KEY_RALT;
	    b->modstate |= EPX_KBD_MOD_RALT;
	    break;
	case XK_Meta_L:
	case XK_Super_L:
	case XK_Hyper_L: e->key.sym = EPX_KBD_KEY_LMETA; break;
	case XK_Meta_R:
	case XK_Super_R:
	case XK_Hyper_R: e->key.sym = EPX_KBD_KEY_RMETA; break;
	default:
	    switch (sym) {
	    case XK_BackSpace:
	    case XK_Tab:
	    case XK_Return:
		break;
	    default:
		if (sym & 0xFF00) {
		    EPX_DBGFMT("Unhandled X11 keysym: %04x\n", (int)sym);
		}
	    }
	    XLookupString(&ev.xkey, &ignored_char, 1, &sym, NULL );
	    if (b->modstate & EPX_KBD_MOD_CTRL)
		e->key.sym = sym & 0x1f;	/* Control code */ 
	    else
		e->key.sym = sym & 0xff;	/* ASCII*/
	    break;
	}

	if (b->modstate & EPX_KBD_MOD_NUM) {
	    switch (e->key.sym) {
	    case EPX_KBD_KEY_KP0:
	    case EPX_KBD_KEY_KP1:
	    case EPX_KBD_KEY_KP2:
	    case EPX_KBD_KEY_KP3:
	    case EPX_KBD_KEY_KP4:
	    case EPX_KBD_KEY_KP5:
	    case EPX_KBD_KEY_KP6:
	    case EPX_KBD_KEY_KP7:
	    case EPX_KBD_KEY_KP8:
	    case EPX_KBD_KEY_KP9:
		e->key.sym = (e->key.sym - EPX_KBD_KEY_KP0) + '0';
		break;
	    case EPX_KBD_KEY_KP_PERIOD: e->key.sym = '.'; break;
	    case EPX_KBD_KEY_KP_DIVIDE: e->key.sym = '/'; break;
	    case EPX_KBD_KEY_KP_MULTIPLY: e->key.sym = '*'; break;
	    case EPX_KBD_KEY_KP_MINUS: e->key.sym = '-'; break;
	    case EPX_KBD_KEY_KP_PLUS:  e->key.sym = '+'; break;
	    case EPX_KBD_KEY_KP_ENTER:
		e->key.sym = EPX_KBD_KEY_ENTER;
		break;
	    case EPX_KBD_KEY_KP_EQUALS: e->key.sym = '-'; break;
		default: break;

	    }
	}
	if (b->grab_key && (b->grab_key == e->key.sym))
	    x11_grab(backend, e->window, (ev.xkey.state & ControlMask));
	e->key.code = ev.xkey.keycode;
	e->key.mod  = b->modstate;
	goto got_event;
    no_key:
	EPX_DBGFMT("3:modstate=%x", b->modstate);
	goto ignore_event;
    }

    default:
	EPX_DBGFMT("Event: Igore event=%d", ev.type);
	goto ignore_event;
    }

ignore_event:
    if (XEventsQueued(b->display, QueuedAlready))
	goto next;
    return 0;

got_event:
    if (FilterEvent(e))
	goto ignore_event;
    backend->pending = XEventsQueued(b->display, QueuedAlready);
    return 1+backend->pending;
}


static void x11_grab(epx_backend_t* backend, epx_window_t* wptr, int toggle)
{
    X11Backend* b = (X11Backend*) backend;
    X11Window* w = (X11Window*) wptr;

    if (w == NULL)
	return;

    if (toggle) {
	/* toggle grab control */
	if (w->grabbed) {
	    XUngrabPointer(b->display, CurrentTime);
	    XUngrabKeyboard(b->display, CurrentTime);
	    XChangePointerControl(b->display,
				  True, False,
				  w->accel_num, 
				  w->accel_den, 0);
	    w->grabbed = 0;
	}
	else {
	    /* save pointer config */
	    XGetPointerControl(b->display, 
			       &w->accel_num, 
			       &w->accel_den,
			       &w->thres);
	    XChangePointerControl(b->display, True, False, 1, 1, 0);
	    XGrabKeyboard(b->display, w->window,
			  True,  /* only to this window */
			  GrabModeAsync, GrabModeAsync, CurrentTime);
	    XGrabPointer(b->display, w->window, False,
			 PointerMotionMask | ButtonPressMask,
			 GrabModeAsync, GrabModeAsync, None, None,
			 CurrentTime);
	    w->grabbed = 1;
	}
    }
    else if (w->grabbed)
	XChangePointerControl(b->display, True, False,
			      w->accel_num, 
			      w->accel_den, 0);
}

static int x11_error(Display * dpy, XErrorEvent * ev)
{
    char err_string[256];

    XGetErrorText(dpy, ev->error_code, err_string, 256);
    fprintf(stderr, "X11 error: %s\r\n", err_string);
    return 0;
}


static int x11_adjust(epx_backend_t* backend, epx_dict_t* param)
{
    X11Backend* b = (X11Backend*) backend;    
    int int_param;

    if (epx_dict_lookup_boolean(param, "use_opengl", &int_param) != -1) {
	b->b.use_opengl = int_param;
#ifdef HAVE_OPENGL
	if ((b->b.use_opengl = int_param) && (b->fbconfigs == NULL)) {
	    int   i;
	    int   nscreens = XScreenCount(b->display);
	    for (i = 0; i < nscreens; i++) {
		GLXFBConfig* config;
		config = glXChooseFBConfig(be->display, i,
					   glxAttributes,
					   &be->fbnumconfigs);
		if (config) {
		    if (be->fbnumconfigs > 0) {
			be->fbconfigs = config;
			be->b.opengl = 1;
			be->screen = i;
			break;
		    }
		    XFree(config);
		}
	    }
	}
#endif
    }

    if (epx_dict_lookup_boolean(param, "use_off_screen", &int_param) != -1) {
	b->use_off_screen = int_param;
    }
    if (epx_dict_lookup_boolean(param, "use_exposure", &int_param) != -1) {
	b->use_exposure = int_param;
    }
    return 1;
}

static int x11_info(epx_backend_t* backend, epx_dict_t*param)
{
    X11Backend* b  = (X11Backend*) backend;
    int bval;
    
    if (epx_dict_lookup_boolean(param, "display", &bval) >= 0) {
	// return the low-level Window
	epx_dict_set_binary(param, "display", &b->display, sizeof(Display*));
    }
    if (epx_dict_lookup_boolean(param, "visual", &bval) >= 0) {
	VisualID vid;
	vid = XVisualIDFromVisual(b->visual);
	epx_dict_set_binary(param, "visual", &vid, sizeof(VisualID));
    }
    return 1;    
}

static int x11_window_adjust(epx_window_t *win, epx_dict_t*param)
{
    int bool_val;
    char* window_name = NULL;
    size_t name_len;
    X11Window*  w  = (X11Window*) win->opaque;
    X11Backend* b  = (X11Backend*) win->backend;
    XWindowChanges value;
    unsigned int mask = 0;
    unsigned int sz_hint = 0;
    int min_h = -1, min_w = -1;
    int max_h = -1, max_w = -1;

    EPX_DBGFMT("x11: Window adjust\n");

    if (epx_dict_lookup_string(param, "name", &window_name, &name_len) >= 0) {
	XStoreName(b->display, w->window, window_name);
    }

    if (epx_dict_lookup_integer(param, "min_width", &min_w) >= 0) {
	EPX_DBGFMT("x11: min_width=%d", min_w);
	sz_hint |= PMinSize;
    }
    if (epx_dict_lookup_integer(param, "min_height", &min_h) >= 0) {
	EPX_DBGFMT("x11: min_height=%d", min_h);
	sz_hint |= PMinSize;
    }
    if (epx_dict_lookup_integer(param, "max_width", &max_w) >= 0) {
	EPX_DBGFMT("x11: max_width=%d", max_w);
	sz_hint |= PMaxSize;
    }
    if (epx_dict_lookup_integer(param, "max_height", &max_h) >= 0) {
	EPX_DBGFMT("x11: max_height=%d", max_h);
	sz_hint |= PMaxSize;
    }

    if (sz_hint) {
	XSizeHints *sizehints;
	sizehints = XAllocSizeHints();
	if (sizehints != NULL) {
	    sizehints->flags = sz_hint;
	    sizehints->min_width  = (min_w < 0) ? win->area.wh.width : (unsigned int) min_w;
	    sizehints->min_height = (min_h < 0) ? win->area.wh.height :  (unsigned int) min_h;
	    sizehints->max_width  = (max_w < 0) ? win->area.wh.width :  (unsigned int) max_w;
	    sizehints->max_height = (max_h < 0) ? win->area.wh.height :  (unsigned int) max_h;
	    XSetWMNormalHints(b->display, w->window, sizehints);
	    XFree(sizehints);
	}
    }    

    if (epx_dict_lookup_integer(param, "x", &value.x) >= 0) {
	EPX_DBGFMT("x11: x=%d", value.x);
	mask |= CWX;
    }
    if (epx_dict_lookup_integer(param, "y", &value.y) >= 0) {
	EPX_DBGFMT("x11: y=%d", value.y);
	mask |= CWY;
    }
    if (epx_dict_lookup_integer(param, "width", &value.width) >= 0) {
	EPX_DBGFMT("x11: width=%d", value.width);
	mask |= CWWidth;
    }
    if (epx_dict_lookup_integer(param, "height", &value.height) >= 0) {
	EPX_DBGFMT("x11: height=%d", value.height);
	mask |= CWHeight;
    }
    if (epx_dict_lookup_integer(param, "border_width", &value.border_width)
	>= 0) {
	EPX_DBGFMT("x11: border_width=%d", value.border_width);
	mask |= CWBorderWidth;
    }
    if (mask) {
	unsigned int mask0 = mask;
	// check if values already are set/reported
	if (value.x == win->rarea.xy.x) mask &= ~CWX;
	if (value.y == win->rarea.xy.y) mask &= ~CWY;
	if (value.width == (int)win->rarea.wh.width) mask &= ~CWWidth;
	if (value.height == (int)win->rarea.wh.height) mask &= ~CWHeight;

	if (mask0 & CWX)      win->area.xy.x = w->x = value.x;
	if (mask0 & CWY)      win->area.xy.y = w->y = value.y;
	if (mask0 & CWWidth)  win->area.wh.width  = w->width = value.width;
	if (mask0 & CWHeight) win->area.wh.height = w->height = value.height;

	XConfigureWindow(b->display, w->window, mask, &value);
    }

    // XFlush(b->display);

    if (epx_dict_lookup_boolean(param, "show", &bool_val) >= 0) {
	EPX_DBGFMT("x11: show=%d", bool_val);
	if (bool_val)
	    XMapWindow(b->display, w->window);
	else
	    XUnmapWindow(b->display, w->window);
	XFlush(b->display);	
    }

    if (epx_dict_lookup_boolean(param, "select", &bool_val) >= 0) {
	EPX_DBGFMT("x11: select=%d", bool_val);	
	if (bool_val) {
	    XMapRaised(b->display, w->window);
	    XFlush(b->display);
	}
    }
    //  "focus", "modal" ...
    return 1;    
}

// Query interface, send boolean entries and return the
// queried for value

static int x11_window_info(epx_window_t* win, epx_dict_t* param)
{
    int bval;
    X11Window*  w  = (X11Window*) win->opaque;
    X11Backend* b  = (X11Backend*) win->backend;
    
    if (epx_dict_lookup_boolean(param, "name", &bval) >= 0) {
	char* window_name;
	if (XFetchName(b->display, w->window, &window_name) == Success) {
	    epx_dict_set_string(param, "name", window_name);
	    XFree(window_name);
	}
	else {
	    epx_dict_set_string(param, "name", "");
	}
    }

    if (epx_dict_lookup_boolean(param, "display", &bval) >= 0) {
	// return the low-level Window
	epx_dict_set_binary(param, "display", &b->display, sizeof(Display*));
    }    

    if (epx_dict_lookup_boolean(param, "window", &bval) >= 0) {
	// return the low-level Window
	epx_dict_set_binary(param, "window", &w->window, sizeof(Window));
    }
    return 1;
}
