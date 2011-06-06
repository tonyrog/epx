/*
 *  X display driver 
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>
#ifdef MAC_OS_X
#include <machine/endian.h>
#else
#include <endian.h>
#endif
#include <memory.h>

#define EPX_HANDLT_T int
#include "epx_backend.h"
#ifdef HAVE_OPENGL
#include <GL/glx.h>

extern int epx_gl_load_texture(epx_pixmap_t* pic, GLuint* textureName,
			       int useAlpha, int useClient, GLuint wrap,
			       int src_x, int src_y,
			       unsigned int width,unsigned int height);
#endif


/* This structure is stored in EWindow opaque field */
typedef struct {
    Window window;
    Pixmap pm;     /* off-screen pixmap */
    GC gc;
    int height;
    int width;
    int grabbed;
    int accel_num;
    int accel_den;
    int thres;
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
#ifdef HAVE_OPENGL
    GLXFBConfig* fbconfigs;    // Fixme XFree!
    int          fbnumconfigs;
#endif
} X11Backend;


epx_backend_t* x11_init(epx_dict_t* param);

static int x11_finish(epx_backend_t*);
static int x11_pic_attach(epx_backend_t*, epx_pixmap_t*);
static int x11_pic_detach(epx_backend_t*, epx_pixmap_t*);
static int x11_begin(epx_window_t*);
static int x11_end(epx_window_t*,int off_screen);
static int x11_pic_draw(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
			int src_x, int src_y, int dst_x, int dst_y,
			unsigned int width,
			unsigned int height);
static int x11_win_attach(epx_backend_t*, epx_window_t*);
static int x11_win_detach(epx_backend_t*, epx_window_t*);
static int x11_win_swap(epx_backend_t*, epx_window_t*);
static EPX_HANDLE_T x11_evt_attach(epx_backend_t*);
static int x11_evt_detach(epx_backend_t*);
static int x11_evt_read(epx_backend_t*, epx_event_t*);
static int x11_adjust(epx_backend_t *backend, epx_dict_t* param);
static int x11_win_adjust(epx_window_t*, epx_dict_t* param);
/* util */
static void x11_grab(epx_backend_t* backend, epx_window_t* window, int toggle);
static int  x11_error(Display * dpy, XErrorEvent * ev);


static epx_callbacks_t x11_callbacks =
{
    x11_finish,
    x11_pic_attach,
    x11_pic_detach,
    x11_pic_draw,
    x11_win_attach,
    x11_win_detach,
    x11_evt_attach,
    x11_evt_detach,
    x11_evt_read,
    x11_adjust,
    x11_win_swap,
    x11_begin,
    x11_end,
    x11_win_adjust
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
    epx_window_t* window = x11->b.window_list;  /* list of attached windows */

    while(window) {
	X11Window* w11 = (X11Window*) window->opaque;
	if (w11->window == w)
	    return window;
	window = window->next;
    }
    return NULL;
}

#if 0 
// Not used right now but it's a fixme

/* scan trough a screen and locate a "nice" visual  */
static Visual* find_visual_of_screen(Screen* screen)
{
    int i;

    for (i = 0; i < screen->ndepths; i++) {
	int j;
	Depth* d = &screen->depths[i];

	for (j = 0; j < d->nvisuals; j++) {
	    Visual* v = &d->visuals[j];

	    (void) v; // Inhibits warning when DBG is not active.

	    EDBG("Visual: screen=%d, visual=%d", i, j);
	    EDBG("          depth=%d", d->depth);
	    EDBG("             id=%X", (unsigned int)v->visualid);
	    EDBG("       red_mask=%lX", v->red_mask);
	    EDBG("     green_mask=%lX", v->green_mask);
	    EDBG("      blue_mask=%lX", v->blue_mask);
	    EDBG("   bits_per_rgb=%d", v->bits_per_rgb);
	    EDBG("%s", "");
	}
    }
    return NULL;
}
#endif





epx_backend_t* x11_init(epx_dict_t* param)
{
    X11Backend* be;
    unsigned int state;
    // Screen* screen;
    int int_param;
    char* display_name = NULL;
    size_t len;

    if ((be = (X11Backend*) malloc(sizeof(X11Backend))) == NULL)
	return NULL;
    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.cb = &x11_callbacks;
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;
    be->b.pixmap_list = 0;
    be->b.window_list = 0;
    be->b.event = EPX_INVALID_HANDLE;

    epx_dict_lookup_string(param, "x11_display", &display_name, &len);

    if ((be->display = XOpenDisplay(display_name)) == NULL) {
	free(be);
	return NULL;
    }

    if (epx_dict_lookup_integer(param, "use_opengl", &int_param) != -1)
	be->b.use_opengl = int_param;
    
    be->screen = DefaultScreen(be->display);
    if (be->b.use_opengl) {
#ifdef HAVE_OPENGL
	int   nscreens;
	int   i;

	nscreens = XScreenCount(be->display);
	for (i = 0; i < nscreens; i++) {
	    be->fbconfigs = glXChooseFBConfig(be->display, i,
					      glxAttributes,
					      &be->fbnumconfigs);
	    if (be->fbconfigs && (be->fbnumconfigs > 0)) {
		be->b.opengl = 1;
		be->screen = i;
		break;
	    }
	}
#endif
    }
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

/* return the backend event handle */
static EPX_HANDLE_T x11_evt_attach(epx_backend_t* backend)
{
    X11Backend* x11 = (X11Backend*) backend;
    int fd = ConnectionNumber(x11->display);

    return (EPX_HANDLE_T)((long)fd);
}

static int x11_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

static int x11_finish(epx_backend_t* backend)
{
    X11Backend* x11 = (X11Backend*) backend;
    // FIXME: close all windows & detach all pixmaps
    XCloseDisplay(x11->display);
    free(x11);
    return 0;
}

static int x11_pic_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    X11Backend* x11 = (X11Backend*) backend;
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
    pixmap->backend = (epx_backend_t*) x11;
    return 0;
}

static int x11_pic_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
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

static int x11_begin(epx_window_t* ewin)
{
    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	X11Backend* be  = (X11Backend*) ewin->backend;
	X11Window*  w11 = (X11Window*) ewin->opaque;
	glXMakeCurrent(be->display, w11->window, w11->context);
#endif
    }
    return 0;
}

static int x11_end(epx_window_t* ewin, int off_screen)
{
    X11Window*  w11 = (X11Window*) ewin->opaque;
    X11Backend* be  = (X11Backend*) ewin->backend;

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	glFlush();
	if (!off_screen)
	    glXSwapBuffers(be->display, w11->window);
#endif
    }
    else {
	if (!(off_screen && w11->pm)) {
	    XSync(be->display, False);
	    be->b.pending = XEventsQueued(be->display, QueuedAlready);
	}
    }
    return 0;
}

static int x11_pic_draw(epx_backend_t* backend, epx_pixmap_t* pic, epx_window_t* ewin,
			int src_x, int src_y, int dst_x, int dst_y,
			unsigned int width,
			unsigned int height)
{
    X11Backend* x11 = (X11Backend*) backend;
    X11Window*  w11 = (X11Window*) ewin->opaque;
    XImage* ximg = (XImage*) pic->opaque;

    if (w11 == NULL)
	return -1;
    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	// Fixme check for errors
	epx_gl_load_texture(pic, &w11->textureName, 0, 1, GL_REPEAT,
			    src_x, src_y, width, height);
	glEnable(GL_TEXTURE_RECTANGLE_ARB); // _EXT enable texturing
	glBindTexture (GL_TEXTURE_RECTANGLE_ARB, w11->textureName);
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
	Drawable d = w11->pm ? w11->pm : w11->window;
	XPutImage(x11->display, d,
		  w11->gc, ximg, src_x, src_y, dst_x, dst_y,
		  width, height);
    }
    return 0;
}

static int x11_win_swap(epx_backend_t* backend, epx_window_t* ewin)
{
    X11Backend* be = (X11Backend*) backend;
    X11Window*  w11 = (X11Window*) ewin->opaque;

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	glXMakeCurrent(be->display, w11->window, w11->context);
	glXSwapBuffers(be->display, w11->window);
	backend->pending = XEventsQueued(be->display, QueuedAlready);
#endif
    }
    else if (w11 && w11->pm) {
	XCopyArea(be->display, w11->pm, w11->window, w11->gc,
		  0, 0, w11->width, w11->height, 0, 0);
	XSync(be->display, False);
	backend->pending = XEventsQueued(be->display, QueuedAlready);
    }
    return 0;
}

static int x11_win_attach(epx_backend_t* backend, epx_window_t* ewin)
{
    X11Backend* be = (X11Backend*) backend;
    X11Window* xwin = NULL;
    Window win = 0;
    XSetWindowAttributes attr;
    unsigned long valuemask;

    if (ewin->opaque != NULL)
	return -1;
    if ((xwin = (X11Window*) malloc(sizeof(X11Window))) == NULL)
	return -1;
    memset(xwin, 0, sizeof(X11Window));

    attr.backing_store = Always;        /* auto expose */
    attr.save_under = True;		/* popups ... */
    attr.event_mask = 
	KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask |
	EnterWindowMask | LeaveWindowMask |
	PointerMotionMask |
	// The following may be used to optimize button motions ...!
	// PointerMotionHintMask | Button1MotionMask |
	// Button2MotionMask | Button3MotionMask | Button4MotionMask |
	// Button5MotionMask | ButtonMotionMask	|
	KeymapStateMask | ExposureMask | VisibilityChangeMask |
	StructureNotifyMask | ResizeRedirectMask | SubstructureNotifyMask |
	SubstructureRedirectMask | FocusChangeMask | PropertyChangeMask	|
	ColormapChangeMask | OwnerGrabButtonMask;

    valuemask = CWSaveUnder | CWEventMask | CWBackingStore;

    if (be->b.opengl && be->b.use_opengl) {
#ifdef HAVE_OPENGL
	XVisualInfo *vinfo;

	vinfo = glXGetVisualFromFBConfig(be->display, be->fbconfigs[0]);
	attr.border_pixel = 0;
	// FIXME save in be - need to release colormap / resuse!?
	attr.colormap = XCreateColormap(be->display,
					RootWindow(be->display, vinfo->screen),
					vinfo->visual, AllocNone);
	valuemask |= (CWBorderPixel | CWColormap);
	win = XCreateWindow(be->display, 
			    RootWindow(be->display, vinfo->screen),
			    ewin->x, /* x */
			    ewin->y,	/* y */
			    ewin->width,	/* width */
			    ewin->height,	/* height */
			    0,		/* border */
			    vinfo->depth,	/* depth */
			    InputOutput,	/* class */
			    vinfo->visual,	/* Visual */
			    valuemask,	/* valuemask */
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

    if (!win)
	win = XCreateWindow(be->display, 
			    XDefaultRootWindow(be->display), 
			    ewin->x, /* x */
			    ewin->y,	/* y */
			    ewin->width,	/* width */
			    ewin->height,	/* height */
			    2,		/* border */
			    CopyFromParent,	/* depth */
			    InputOutput,	/* class */
			    be->visual,	/* Visual */
			    valuemask,	/* valuemask */
			    &attr		/* attributes */
	    );

    if (win) {
	XEvent event;
	xwin->grabbed = 0;
	xwin->accel_num = 0;
	xwin->accel_den = 0;
	xwin->thres = 0;
	xwin->window = win;
	xwin->width = ewin->width;
	xwin->height = ewin->height;
	/* FIXME: allocate this ? */
	xwin->gc = XDefaultGC(be->display, be->screen);
	/* FIXME: configurable */
	xwin->pm = XCreatePixmap(be->display,
				 win,  /* used to specify screen */
				 ewin->width,
				 ewin->height,
				 DefaultDepth(be->display, be->screen));
	if (!xwin->pm)
	    fprintf(stderr, "epic_x11: Failed create of Pixmap\n");

	XSetWMProtocols(be->display, win, &be->wm_delete_window, 1);

	XMapWindow(be->display, win);
	XIfEvent(be->display, &event, WaitForNotify, (XPointer) win );
	// XSync(x11->display, False);
	epx_object_link(&backend->window_list, ewin);
	ewin->opaque  = (void*) xwin;
	ewin->backend = (epx_backend_t*) be;
	return 0;
    }
    free(xwin);
    return -1;
}

static int x11_win_detach(epx_backend_t* backend, epx_window_t* ewin)
{
    X11Backend* be = (X11Backend*) backend;
    X11Window*  xwin = (X11Window*) ewin->opaque;
    
    if ((xwin != NULL) && (xwin->window != 0)) {
	EPX_DBGFMT("XUnmapWindow");
	XUnmapWindow(be->display, xwin->window);
	EPX_DBGFMT("XDestroyWindow");
	if (xwin->pm)
	    XFreePixmap(be->display, xwin->pm);
	if (ewin->opengl) {
#ifdef HAVE_OPENGL
	    glXDestroyContext(be->display, xwin->context);
#endif
	}
	XDestroyWindow(be->display, xwin->window);
	XFlush(be->display);
	/* Ungrabb? */
	free(xwin);
	epx_object_unlink(&backend->window_list, ewin);
	ewin->opaque  = NULL;
	ewin->backend = NULL;
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

    if (state & Button1Mask)
	button |= EPX_BUTTON_LEFT;
    if (state & Button2Mask)
	button |= EPX_BUTTON_MIDDLE;
    if (state & Button3Mask)
	button |= EPX_BUTTON_RIGHT;
    if (state & WheelUpMask)
	button |= EPX_WHEEL_UP;
    if (state & WheelDownMask)
	button |= EPX_WHEEL_DOWN;
    if (state & WheelLeftMask)
	button |= EPX_WHEEL_LEFT;
    if (state & WheelRightMask)
	button |= EPX_WHEEL_RIGHT;
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
    X11Backend* x11 = (X11Backend*) backend;
    XEvent ev;

    backend->pending = 0;

next:
    if (!XCheckIfEvent(x11->display, &ev, CheckEvent, (XPointer)0))
	return -1;
    switch (ev.type) {
    case CreateNotify:
	EPX_DBGFMT("Event: CreateNotify");
	break;
    case DestroyNotify:
	EPX_DBGFMT("Event: DestroyNotify");
	e->window = find_event_window(x11, ev.xdestroywindow.window);
	e->type = EPX_EVENT_DESTROYED;
	goto got_event;

    case ClientMessage:
	EPX_DBGFMT("Event: ClientMessage");
	e->window = find_event_window(x11, ev.xclient.window);
	if ((ev.xclient.data.l[0] != (int) x11->wm_delete_window))
	    break;
	EPX_DBGFMT("Event: WM_DELETE_WINDOW");
	e->type = EPX_EVENT_CLOSE;
	goto got_event;

    case ConfigureNotify:
	EPX_DBGFMT("Event: ConfigureNotity x=%d, y=%d, w=%d, h=%d",
		ev.xconfigure.x,ev.xconfigure.y,
		ev.xconfigure.width, ev.xconfigure.height);
	e->window = find_event_window(x11, ev.xconfigure.window);
	e->type = EPX_EVENT_CONFIGURE;
	e->area.x = ev.xconfigure.x;
	e->area.y = ev.xconfigure.y;
	e->area.w = ev.xconfigure.width;
	e->area.h = ev.xconfigure.height;
	goto got_event;

    case ResizeRequest:
	EPX_DBGFMT("Event: ResizeRequest w=%d, h=%d",
		ev.xresizerequest.width,ev.xresizerequest.height);
	e->window = find_event_window(x11, ev.xresizerequest.window);
	e->type = EPX_EVENT_RESIZE;
	e->dimension.w = ev.xresizerequest.width;
	e->dimension.h = ev.xresizerequest.height;
	e->dimension.d = 0;
	goto got_event;
	break;

    case KeymapNotify:
	EPX_DBGFMT("Event: KeymapNotify");
	break;
    case Expose:
	EPX_DBGFMT("Event: Expose");
	break;
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
	EPX_DBGFMT("Event: GravityRequest");
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
		XGetAtomName(x11->display, ev.xproperty.atom));
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
	e->window = find_event_window(x11, ev.xcrossing.window);
	e->type = EPX_EVENT_ENTER;
	e->pointer.x = ev.xcrossing.x;
	e->pointer.y = ev.xcrossing.y;
	e->pointer.z = 0;
	goto got_event;
	
    case LeaveNotify:
	EPX_DBGFMT("Event: LeaveNotity");
	e->window = find_event_window(x11, ev.xcrossing.window);
	e->type = EPX_EVENT_LEAVE;
	e->pointer.x = ev.xcrossing.x;
	e->pointer.y = ev.xcrossing.y;
	e->pointer.z = 0;
	goto got_event;

    case FocusIn:
	EPX_DBGFMT("Event: FocusIn");
	e->window = find_event_window(x11, ev.xfocus.window);
	e->type = EPX_EVENT_FOCUS_IN;
	goto got_event;

    case FocusOut:
	EPX_DBGFMT("Event: FocusOut");
	e->window = find_event_window(x11, ev.xfocus.window);
	e->type = EPX_EVENT_FOCUS_OUT;
	goto got_event;

    case MotionNotify:
	EPX_DBGFMT("Event: MotionNotify");
	e->window = find_event_window(x11, ev.xmotion.window);
	e->pointer.button = Buttons(ev.xmotion.state);
	e->type = EPX_EVENT_POINTER_MOTION;
	e->pointer.x = ev.xmotion.x;
	e->pointer.y = ev.xmotion.y;
	e->pointer.z = 0;
	goto got_event;

    case ButtonPress:
	e->window = find_event_window(x11, ev.xbutton.window);
	e->pointer.button = Buttons(1 << (ev.xbutton.button + 7));
	e->type = EPX_EVENT_BUTTON_PRESS;
	e->pointer.x = ev.xbutton.x;
	e->pointer.y = ev.xbutton.y;
	e->pointer.z = 0;
	goto got_event;
	
    case ButtonRelease:
	e->window = find_event_window(x11, ev.xbutton.window);
	e->pointer.button = Buttons(1 << (ev.xbutton.button + 7));
	e->type = EPX_EVENT_BUTTON_RELEASE;
	e->pointer.x = ev.xbutton.x;
	e->pointer.y = ev.xbutton.y;
	e->pointer.z = 0;
	goto got_event;

    case KeyPress:
    case KeyRelease: {
	char ignored_char;
	KeySym sym = XKeycodeToKeysym(x11->display, ev.xkey.keycode, 0);
	
	if (sym == NoSymbol) {
	    EPX_DBGFMT("event sym=NoSymbol");
	    goto ignore_event;
	}
	e->window = find_event_window(x11, ev.xkey.window);

	if (ev.type == KeyPress)
	    e->type = EPX_EVENT_KEY_PRESS;
	else if (ev.type == KeyRelease)
	    e->type = EPX_EVENT_KEY_RELEASE;
	else
	    break;

	/* calculate kbd modifiers*/
	if (x11->modstate) {
	    EPX_DBGFMT("1:modstate=%x", x11->modstate);
	}
	x11->modstate &= (EPX_KBD_MOD_NUM|EPX_KBD_MOD_CAPS|EPX_KBD_MOD_SCR);
	if (ev.xkey.state) {
	    EPX_DBGFMT("key.state=%x", ev.xkey.state);
	}
	if (ev.xkey.state & ShiftMask) {
	    EPX_DBGFMT("shiftmask");
	    x11->modstate |= EPX_KBD_MOD_SHIFT;
	}
	if (ev.xkey.state & LockMask) {
	    EPX_DBGFMT("lockmask (caps)");
	    x11->modstate |= EPX_KBD_MOD_CAPS;
	}
	if (ev.xkey.state & ControlMask) {
	    EPX_DBGFMT("controlmask");
	    x11->modstate |= EPX_KBD_MOD_CTRL;
	}
	if (ev.xkey.state & Mod1Mask) {
	    EPX_DBGFMT("mod1mask (alt)");
	    x11->modstate |= EPX_KBD_MOD_ALT;
	}
	if (ev.xkey.state & Mod2Mask) {
	    EPX_DBGFMT("mod2mask (num)");
	    x11->modstate |= EPX_KBD_MOD_NUM;
	}
	if (ev.xkey.state & Mod3Mask) {
	    EPX_DBGFMT("mod3mask");
	}
	if (ev.xkey.state & Mod4Mask) {
	    EPX_DBGFMT("mod4mask");
	}
	if (ev.xkey.state & Mod5Mask) {
	    EPX_DBGFMT("mod5mask (scr)");
	    x11->modstate |= EPX_KBD_MOD_SCR;
	}
	if (x11->modstate) {
	    EPX_DBGFMT("2:modstate=%x", x11->modstate);
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
		x11->modstate ^= EPX_KBD_MOD_NUM;
	    goto no_key;
	case XK_Shift_Lock:
	case XK_Caps_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		x11->modstate ^= EPX_KBD_MOD_CAPS;
	    goto no_key;
	case XK_Scroll_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		x11->modstate ^= EPX_KBD_MOD_SCR;
	    goto no_key;
	case XK_Shift_L:
	    EPX_DBGFMT("shift left");
	    e->key.sym = EPX_KBD_KEY_LSHIFT; 
	    break;
	case XK_Shift_R: 
	    EPX_DBGFMT("shift right");
	    e->key.sym = EPX_KBD_KEY_RSHIFT; 
	    break;
	case XK_Control_L: 
	    EPX_DBGFMT("control left");
	    e->key.sym = EPX_KBD_KEY_LCTRL; 
	    break;
	case XK_Control_R: 
	    EPX_DBGFMT("control right");
	    e->key.sym = EPX_KBD_KEY_RCTRL;
	    break;
	case XK_Alt_L: e->key.sym = EPX_KBD_KEY_LALT; break;
	case XK_Alt_R: e->key.sym = EPX_KBD_KEY_RALT; break;
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
	    if (x11->modstate & EPX_KBD_MOD_CTRL)
		e->key.sym = sym & 0x1f;	/* Control code */ 
	    else
		e->key.sym = sym & 0xff;	/* ASCII*/
	    break;
	}

	if (x11->modstate & EPX_KBD_MOD_NUM) {
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
	if (x11->grab_key == e->key.sym)
	    x11_grab(backend, NULL, (ev.xkey.state & ControlMask));
	e->key.code = ev.xkey.keycode;
	e->key.mod  = x11->modstate;
	goto got_event;
    no_key:
	EPX_DBGFMT("3:modstate=%x", x11->modstate);
	goto ignore_event;
    }

    default:
	EPX_DBGFMT("Event: Igore event=%d", ev.type);
	goto ignore_event;
    }

ignore_event:
    if (XEventsQueued(x11->display, QueuedAlready))
	goto next;
    return 0;

got_event:
    if (FilterEvent(e))
	goto ignore_event;
    backend->pending = XEventsQueued(x11->display, QueuedAlready);
    return 1+backend->pending;
}


static void x11_grab(epx_backend_t* backend, epx_window_t* wptr, int toggle)
{
    X11Backend* b = (X11Backend*) backend;
    X11Window* w = (X11Window*) wptr;

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


int x11_adjust(epx_backend_t* backend, epx_dict_t* param)
{
    (void) backend;
    (void) param;
    return 1;
}

int x11_win_adjust(epx_window_t *win, epx_dict_t*dict)
{
    (void) win;
    (void) dict;
    return 1;
}

