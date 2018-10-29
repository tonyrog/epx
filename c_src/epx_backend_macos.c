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
 *  Mac Os X display driver
 *
 * Where:
 *  /System/Library/Frameworks/ApplicationServices.framework/Frameworks/
 *    CoreGraphics.framework/Headers
 */

#if !defined(__x86_64__)

// This code does not build under 64bit How do you do it?

#include <Carbon/Carbon.h>
#include <QuickTime/QuickTime.h>
#include <machine/endian.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>


#include "../include/epx_backend.h"
#ifdef HAVE_OPENGL
#include <AGL/agl.h>
#include <GL/glu.h>
extern int epx_gl_load_texture(epx_pixmap_t* pic, GLuint* textureName,
			       int useAlpha, int useClient, GLuint wrap,
			       int src_x, int src_y,
			       unsigned int width,unsigned int height);
#endif

#include <objc/objc-runtime.h>
#include <Cocoa/Cocoa.h>


#define kEpxEventNew 1
#define kEpxEventDel 2

typedef struct {
    epx_backend_t b;
    int             ofd;         // pipe output signal
    MenuRef         menu;
    unsigned short  modstate;    // modifier state
    unsigned short  grab_key;    // grab key
    pthread_t       carbon_thr;  // thread id 
    pthread_mutex_t carbon_lock; // lock for context ops
} CarbonBackend;

typedef struct {
    epx_window_t*         ewin;      /* reference to exp_window representaion */
    HIRect           winBounds;
    WindowClass      winClass;
    int              winAttrs[10];
    WindowRef        winRef;
    CGContextRef     ctx;
    CGrafPtr         port;
    int              winIsSetup; // 1 when window setup is done
    OSStatus         winErr;    // indicate window setup error (!=noErr)
#ifdef HAVE_OPENGL
    AGLContext       saveContext;
    AGLContext       aglContext;
    AGLPixelFormat   aglFormat;
    GLuint textureName;
#endif    
} CarbonWindow;

typedef struct {
    CGBitmapInfo bitmapinfo;
    CGColorSpaceRef colorspace;
    CGDataProviderRef provider;
} CarbonPixels;


epx_backend_t* carbon_init(epx_dict_t* param);
int carbon_upgrade(epx_backend_t* be);

static int carbon_finish(epx_backend_t*);
static int carbon_pix_attach(epx_backend_t*, epx_pixmap_t* pic);
static int carbon_pix_detach(epx_backend_t*, epx_pixmap_t* pic);
static int carbon_begin(epx_window_t* ewin);
static int carbon_end(epx_window_t* ewin, int off_screen);
static int carbon_pix_draw(epx_backend_t*, epx_pixmap_t* pic, epx_window_t* win,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height);
static int carbon_win_attach(epx_backend_t*, epx_window_t* win);
static int carbon_win_detach(epx_backend_t*, epx_window_t* win);
static int carbon_win_swap(epx_backend_t*, epx_window_t* win);
static EPX_HANDLE_T carbon_evt_attach(epx_backend_t*);
static int carbon_evt_detach(epx_backend_t*);
static int carbon_evt_read(epx_backend_t*, epx_event_t*);
static int carbon_adjust(epx_backend_t* backend, epx_dict_t* param);
static int carbon_win_adjust(epx_window_t*, epx_dict_t* param);
static int carbon_info(epx_backend_t* backend, epx_dict_t* param);

static epx_callbacks_t carbon_callbacks =
{
    .finish = carbon_finish,
    .pix_attach = carbon_pix_attach,
    .pix_detach = carbon_pix_detach,
    .pix_draw   = carbon_pix_draw,
    .win_attach = carbon_win_attach,
    .win_detach = carbon_win_detach,
    .evt_attach = carbon_evt_attach,
    .evt_detach = carbon_evt_detach,
    .evt_read   = carbon_evt_read,
    .adjust     = carbon_adjust,
    .win_swap   = carbon_win_swap,
    .begin      = carbon_begin,
    .end        = carbon_end,
    .win_adjust = carbon_win_adjust,
    .info       = carbon_info
};

static pascal OSStatus EPxAppEventHandler(
    EventHandlerCallRef	inHandlerCallRef,
    EventRef		inEvent,
    void*		inUserData);

static pascal OSStatus EPxWindowEventHandler(
    EventHandlerCallRef	inHandlerCallRef,
    EventRef		inEvent,
    void*		inUserData);
//----------------------------------------------------------------------------
DEFINE_ONE_SHOT_HANDLER_GETTER( EPxWindowEventHandler )

DEFINE_ONE_SHOT_HANDLER_GETTER( EPxAppEventHandler )

//----------------------------------------------------------------------------
#ifdef debug
char* id_to_string(UInt32 w, char* str, char** end_ptr)
{
    int i = sizeof(w);

    str[i] = '\0';
    while(i--) {
	str[i] = w;
	w >>= 8;
    }
    if (end_ptr != NULL)
	*end_ptr = str+sizeof(w)-1;
    return str;
}

void dbg_print_event(char* label, EventRef theEvent)
{
    UInt32	 eventClass;
    UInt32	 eventKind;		
    char e_class[5];

    eventClass = GetEventClass(theEvent);
    eventKind  = GetEventKind(theEvent);
    id_to_string(eventClass, e_class, NULL);

    EPX_DBGFMT("%s: class='%s', kind=%lu", label, e_class, eventKind);
}
#else
#define dbg_print_event(label,inEvent) do {} while(0)
#endif

#ifdef HAVE_OPENGL

// This code is used from example but...
void ReportError (char * strError)
{
    char errMsgCStr [256];

    sprintf (errMsgCStr, "%s\n", strError);

    // out as debug string
#ifdef kVerboseErrors
    // ensure we are faded in
    if (gDSpStarted)
	DSpContext_CustomFadeGammaIn (NULL, NULL, 0);
    CStrToPStr (strErr, errMsgCStr);
    DebugStr (strErr);
#endif // kVerboseErrors
}

OSStatus glReportError (void)
{
    GLenum err = glGetError();
    switch (err) {
    case GL_NO_ERROR:
	break;
    case GL_INVALID_ENUM:
	ReportError ("GL Error: Invalid enumeration");
	break;
    case GL_INVALID_VALUE:
	ReportError ("GL Error: Invalid value");
	break;
    case GL_INVALID_OPERATION:
	ReportError ("GL Error: Invalid operation");
	break;
    case GL_STACK_OVERFLOW:
	ReportError ("GL Error: Stack overflow");
	break;
    case GL_STACK_UNDERFLOW:
	ReportError ("GL Error: Stack underflow");
	break;
    case GL_OUT_OF_MEMORY:
	ReportError ("GL Error: Out of memory");
	break;
    default:
	ReportError ("GL Error: Unknown error");
	break;
    }
    // ensure we are returning an OSStatus noErr if no error condition
    if (err == GL_NO_ERROR)
	return noErr;
    else {
	fprintf(stderr, "GL error at %s:%d: %s\n",__FILE__,__LINE__,
		(char*)gluErrorString(err));
	return (OSStatus) err;
    }
}


OSStatus aglReportError (void)
{
    GLenum err = aglGetError();
    if (AGL_NO_ERROR != err)
	ReportError ((char *)aglErrorString(err));
    // ensure we are returning an OSStatus noErr if no error condition
    if (err == AGL_NO_ERROR)
	return noErr;
    else
	return (OSStatus) err;
}

/* Set up GL context */
static OSStatus carbon_gl_setup(CarbonWindow* cwin)
{
    GLint attributes[] = {
	AGL_RGBA,
	AGL_DOUBLEBUFFER,
	AGL_DEPTH_SIZE,	32,
	AGL_ACCELERATED, 
	AGL_NO_RECOVERY,
	AGL_NONE,
	AGL_NONE
    };
    AGLContext save;
    GLint swap = 0;  // =1 Looks a lot nicer but slow things up a bit
    
/***
    if ((Ptr) kUnresolvedCFragSymbolAddress == (Ptr) aglChoosePixelFormat)
	return paramErr;
**/

    if (!(cwin->aglFormat = aglChoosePixelFormat(NULL,0,attributes)))
	return paramErr;
    if (!(cwin->aglContext = aglCreateContext(cwin->aglFormat, 0))) {
	aglDestroyPixelFormat(cwin->aglFormat);
	return paramErr;
    }
    aglSetWindowRef(cwin->aglContext, cwin->winRef);

    save = aglGetCurrentContext();
    aglSetCurrentContext(cwin->aglContext);
    aglUpdateContext (cwin->aglContext);
    // FIXME: setup from EDict
    aglSetInteger (cwin->aglContext, AGL_SWAP_INTERVAL, &swap); 
    aglSetCurrentContext(save);

    return noErr;
}

static OSStatus carbon_gl_cleanup(CarbonWindow* cwin)
{
    OSStatus err;
	
    glFinish ();
    aglSetCurrentContext (0);
    err = aglReportError ();
    aglDestroyContext(cwin->aglContext);
    err = aglReportError ();
    cwin->aglContext = 0;
    
    if (cwin->aglFormat)  {
	aglDestroyPixelFormat(cwin->aglFormat);
	err = aglReportError ();
    }
    cwin->aglFormat = 0;
    return err;
}

#endif

void* carbon_event_thread(void* arg)
{
    CarbonBackend* be = (CarbonBackend*) arg;
    OSStatus err;
    EventTypeSpec appEventList[] =
	{ { 'EPX ',  kEpxEventNew},
	  { 'EPX ',  kEpxEventDel},
	  { kEventClassCommand, kEventProcessCommand } };
    // EventRef theEvent;
    // EventTargetRef theTarget;

    EPX_DBGFMT("carbon_event_thread: running ofd=%d", be->ofd);

    {
	ProcessSerialNumber psn;
	void *rel_pool;

	rel_pool = [[NSAutoreleasePool alloc] init];
	[NSApplication sharedApplication];

	GetCurrentProcess(&psn);
	TransformProcessType(&psn, kProcessTransformToForegroundApplication);
	SetFrontProcess(&psn);
    }

    InitCursor();
    SetEventMask( everyEvent ) ;
    SetThemeCursor(kThemeArrowCursor);

    CreateStandardWindowMenu(0, &be->menu);
    InsertMenu(be->menu, 0);
    DrawMenuBar();

    err = InstallApplicationEventHandler( GetEPxAppEventHandlerUPP(),
					  GetEventTypeCount( appEventList ), 
					  appEventList, be, NULL );

    pthread_mutex_unlock(&be->carbon_lock);

    EPX_DBGFMT("carbon_event_thread: app loop start err=%d", err);
    RunApplicationEventLoop();

    close(be->ofd);  // signal termination!
    return 0;
}


epx_backend_t* carbon_init(epx_dict_t* param)
{
#pragma unused(param)
    CarbonBackend* be;
    int int_param;
    int carbon_pipe[2];

    EPX_DBGFMT("carbon_init");

    if ((be = (CarbonBackend*) malloc(sizeof(CarbonBackend))) == 0)
	return 0;
    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.cb = &carbon_callbacks;
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;
    epx_object_list_init(&be->b.pixmap_list);
    epx_object_list_init(&be->b.window_list);
    be->b.event = EPX_INVALID_HANDLE;

    if (epx_dict_lookup_integer(param, "use_opengl", &int_param) != -1)
	be->b.use_opengl = int_param;
#ifdef HAVE_OPENGL
    be->b.opengl = 1;
#endif

    pipe(carbon_pipe);
    be->b.event = (EPX_HANDLE_T) carbon_pipe[0];
    be->ofd     = carbon_pipe[1];
    pthread_mutex_init(&be->carbon_lock, 0);
    pthread_mutex_lock(&be->carbon_lock);  // Lock until thread is initialised

    pthread_create(&be->carbon_thr, 0, carbon_event_thread,
		   (void*) be);
    pthread_mutex_lock(&be->carbon_lock);    // Wait for init to complete
    pthread_mutex_unlock(&be->carbon_lock);

    return (epx_backend_t*) &(be->b);
}

int carbon_upgrade(epx_backend_t* backend)
{
    backend->cb = &carbon_callbacks;
    return 0;
}


/* return the backend event handle */
static EPX_HANDLE_T carbon_evt_attach(epx_backend_t* backend)
{
    CarbonBackend* be = (CarbonBackend*) backend;
    return (EPX_HANDLE_T) be->b.event;
}

static int carbon_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

static int carbon_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    int r = 0;
    
    if ((r=backend->pending) == 0) // LOCK atomic check
	return 0;
    if (read((int)backend->event, (void*) e, sizeof(epx_event_t)) < 0)
	return -1;
    backend->pending--;  // LOCK atomic! check
    return r;
}

static int carbon_finish(epx_backend_t* backend)
{
    CarbonBackend* be = (CarbonBackend*) backend;
    EPX_DBGFMT("carbon_finish");
    
    // post a Quit to the event handler
    //  MUST close all attached windows and detach all pixmaps

    // join the thread

    // close file descriptor

    free(be);
    return 0;
}

static void pixel_ReleaseInfo(void *info)
{
    (void) info;
    EPX_DBGFMT_MEM("ReleaseInfo");
    /* NOOP */
}

static const void* pixel_GetBytePointer(void* info)
{
    epx_pixmap_t* pic = (epx_pixmap_t*) info;
    return (void*) pic->data;
}

static size_t pixel_GetBytesAtOffset(void *info, void *buffer, 
				     off_t offset, size_t count)
{
    epx_pixmap_t* pic = (epx_pixmap_t*) info;
    memcpy(buffer, pic->data+offset, count);
    return count;
}

static void pixel_ReleaseBytePointer(void *info, const void *pointer)
{
    (void) info;
    (void) pointer;
    /* Noop */
}

static CGDataProviderDirectCallbacks dacallbacks =
{
    0,
    pixel_GetBytePointer,
    pixel_ReleaseBytePointer,
    pixel_GetBytesAtOffset,
    pixel_ReleaseInfo
};

static int carbon_pix_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    CarbonBackend* be = (CarbonBackend*) backend;
    CarbonPixels* pe;

    if (pixmap->opaque != 0)
	return -1;
    if ((pe = (CarbonPixels*) malloc(sizeof(CarbonPixels))) == 0)
	return -1;

    pe->colorspace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    pe->bitmapinfo = 0;

    // Note! We do not use Alpha channel on output bitmap this is
    // handled in EPX so we use AlphaNone/AlphaSkip where needed
    switch(pixmap->pixel_format) {
    case EPX_FORMAT_RGB:
	pe->bitmapinfo |= kCGImageAlphaNone;         /* RGB format */
	break;
    case EPX_FORMAT_RGBA:
	pe->bitmapinfo |= kCGBitmapByteOrder32Big;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipLast; /* RGBA format */
	break;
    case EPX_FORMAT_ARGB:
	pe->bitmapinfo |= kCGBitmapByteOrder32Big;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst; // XRGB
	break;
    case EPX_FORMAT_BGR:
	pe->bitmapinfo |= kCGImageAlphaNone;          // BGR format
	break;
    case EPX_FORMAT_BGRA: /* ARGB */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst;
	break;
    case EPX_FORMAT_ABGR: /* RGBA */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipLast;   // RGBX 
	break;
    default:
	fprintf(stderr, "carbon: unhandled pixel_format = %d\n", pixmap->pixel_format);
	free(pe);
	return -1;
    }

    pe->provider = CGDataProviderCreateDirect(
	(void*) pixmap, pixmap->sz, &dacallbacks);
    epx_object_link(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) pe;
    pixmap->backend = (epx_backend_t*) be;
    return 0;
}

						
static int carbon_pix_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    CarbonPixels* pe = (CarbonPixels*) pixmap->opaque;

    if (pe != 0) {
	CGColorSpaceRelease(pe->colorspace);
	CGDataProviderRelease(pe->provider);
	epx_object_unlink(&backend->pixmap_list, pixmap);
	// FIXME: free(pe) ? 
	pixmap->opaque = 0;
	pixmap->backend = 0;
    }
    return 0;
}

/* There must be a better way */
static void draw_image(CGContextRef ctx, CGRect srcRect, CGRect dstRect,
		       epx_pixmap_t* pic)
{
    CarbonPixels* pe = (CarbonPixels*) pic->opaque;
    CGImageRef img;
    CGImageRef srcImg;
    unsigned int bytes_per_row   = pic->bytes_per_row;
    unsigned int bits_per_pixel  = pic->bits_per_pixel;

    // Boring to create this every time!
    img = CGImageCreate(pic->width,               /* width in pixels */
			pic->height,              /* height in pixels */
			8,                        /* bitsPerComponent */
			bits_per_pixel,           /* bitsPerPixel */
			bytes_per_row,            /* bytes_per_Row */
			pe->colorspace,           /* colorspace */
			pe->bitmapinfo,           /* bitmapinfo, */
			pe->provider,
			0,
			false,
			kCGRenderingIntentSaturation);
    if (!img) {
	fprintf(stderr, "epx_macos: error CGImageCreate\n");
	return;
    }

    if ((srcRect.origin.x == (CGFloat)0) && 
	(srcRect.origin.y == (CGFloat)0) &&
	(srcRect.size.width == (CGFloat) pic->width) && 
	(srcRect.size.height == (CGFloat) pic->height)) {
	// Complete image
	srcImg = img;
    }
    else {
	// Sub image
	srcImg = CGImageCreateWithImageInRect(img, srcRect);
    }

    if (srcImg != 0) {
	dstRect = CGContextConvertRectToUserSpace(ctx, dstRect);
	CGContextDrawImage(ctx, dstRect, srcImg);
    }
    if (srcImg && (srcImg != img))
	CGImageRelease(srcImg);
    CGImageRelease(img);
}

static int carbon_begin(epx_window_t* ewin)
{
    CarbonBackend* be  = (CarbonBackend*) ewin->backend;
    CarbonWindow* cwin = (CarbonWindow*) ewin->opaque;
    // WindowRef     wref;

    if ((cwin == 0) || (cwin->winRef == 0))
	return -1;

    cwin->port = GetWindowPort(cwin->winRef);
    pthread_mutex_lock(&be->carbon_lock);

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	cwin->saveContext = aglGetCurrentContext();
	aglSetCurrentContext(cwin->aglContext);
	aglUpdateContext(cwin->aglContext);
#endif
    }
    else {
	QDBeginCGContext(cwin->port, &cwin->ctx);
	CGContextSaveGState(cwin->ctx);
    }
    return 0;
}

static int carbon_end(epx_window_t* ewin, int off_screen)
{
    (void) off_screen;
    CarbonWindow* cwin = (CarbonWindow*) ewin->opaque;
    CarbonBackend* be  = (CarbonBackend*) ewin->backend;

    if (cwin == 0)
	return -1;
    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	if (!cwin->aglContext)
	    return -1;
	glFlush();
	if (!off_screen)
	    aglSwapBuffers (cwin->aglContext);
	aglSetCurrentContext(cwin->saveContext);
#endif
    }
    else {
	CGContextFlush(cwin->ctx);
	// CGContextSynchronize(cwin->ctx); 
	CGContextRestoreGState(cwin->ctx);
	QDEndCGContext(cwin->port, &cwin->ctx);
    }
    pthread_mutex_unlock(&be->carbon_lock);

    return 0;
}



static int carbon_pix_draw(epx_backend_t* backend, epx_pixmap_t* pic, 
			   epx_window_t* ewin,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height)

{
    (void) backend;
    CarbonWindow* cwin = (CarbonWindow*) ewin->opaque;

    if (cwin == 0) 
	return -1;
    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	// Fixme check for errors
	epx_gl_load_texture(pic, &cwin->textureName, 0, 1,  GL_REPEAT,
			    src_x, src_y, width, height);
	glEnable(GL_TEXTURE_RECTANGLE_EXT); // enable texturing
	glBindTexture (GL_TEXTURE_RECTANGLE_ARB, cwin->textureName);
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
	glDisable(GL_TEXTURE_RECTANGLE_EXT);
#endif
    }
    else {
	CGRect srcRect;
	CGRect dstRect;

	srcRect = CGRectMake((float)src_x, (float)src_y,
			     (float)width, (float)height);
	dstRect = CGRectMake((float)dst_x, (float)dst_y+21, 
			     (float)width, (float)height);
	draw_image(cwin->ctx, srcRect, dstRect, pic);
    }
    return 0;
}

static int carbon_win_swap(epx_backend_t* backend, epx_window_t* ewin)
{
    (void) backend;
    // CarbonBackend* be = (CarbonBackend*) backend;

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	CarbonWindow* cwin = (CarbonWindow*) ewin->opaque;
	AGLContext save;
	if (!cwin || !cwin->winRef || !cwin->aglContext)
	    return -1;
	save = aglGetCurrentContext();
	aglSetCurrentContext(cwin->aglContext);
	aglUpdateContext (cwin->aglContext);
	aglSwapBuffers (cwin->aglContext);
	aglSetCurrentContext(save);
#endif
    }
    return 0;
}

static int carbon_win_attach(epx_backend_t* backend, epx_window_t* ewin)
{
    CarbonBackend* be = (CarbonBackend*) backend;
    CarbonWindow* cwin;
    EventRef      newEvent;
    int i;

    if (ewin->opaque != 0)
	return -1;

    if ((cwin = (CarbonWindow*) malloc(sizeof(CarbonWindow))) == 0) 
	return -1;
    memset(cwin, 0, sizeof(CarbonWindow));
    cwin->ewin   = ewin;
    cwin->winErr = noErr;
    cwin->winIsSetup = 0;
    ewin->opaque = (void*) cwin;

    epx_object_link(&backend->window_list, ewin);
    ewin->backend = (epx_backend_t*) be;
    cwin->winClass    = kDocumentWindowClass;
    i = 0; // Max 10 FIXME
    cwin->winAttrs[i++] = kHIWindowBitCloseBox;
    cwin->winAttrs[i++] = kHIWindowBitZoomBox;
    cwin->winAttrs[i++] = kHIWindowBitCollapseBox;
    cwin->winAttrs[i++] = kHIWindowBitResizable;
    cwin->winAttrs[i] = 0;

    cwin->winBounds.origin.x = ewin->x;
    cwin->winBounds.origin.y = ewin->y;
    cwin->winBounds.size.height = ewin->height;
    cwin->winBounds.size.width  = ewin->width;

    EPX_DBGFMT("carbon_win_attach: cwin=%p", cwin);
    MacCreateEvent(nil, 'EPX ',  kEpxEventNew, GetCurrentEventTime(),
		   kEventAttributeNone, &newEvent);
    SetEventParameter(newEvent, 'EPX ', 'CWIN', sizeof(CarbonWindow*), &cwin);
    PostEventToQueue(GetMainEventQueue(), newEvent, kEventPriorityHigh);
    ReleaseEvent(newEvent);
    EPX_DBGFMT("carbon_win_attach: post done");

    while(cwin->winIsSetup == 0)
	usleep(1000);
    if (cwin->winErr != noErr) {
	free(cwin);
	return -1;
    }
    return 0;
}

static int carbon_win_detach(epx_backend_t* backend, epx_window_t* ewin)
{
    CarbonWindow* cwin = (CarbonWindow*) ewin->opaque;

    if (cwin != 0) {
	EventRef  delEvent;

	epx_object_unlink(&backend->window_list, ewin);
	ewin->opaque  = 0;
	ewin->backend = 0;

	MacCreateEvent(nil, 'EPX ',  kEpxEventDel, GetCurrentEventTime(),
		       kEventAttributeNone, &delEvent);
	SetEventParameter(delEvent, 'EPX ', 'CWIN', 
			  sizeof(CarbonWindow*), &cwin);
	PostEventToQueue(GetMainEventQueue(), delEvent, kEventPriorityHigh);
	ReleaseEvent(delEvent);
    }
    return 0;
}


static pascal OSStatus EPxAppEventHandler(
    EventHandlerCallRef nextHandler, 
    EventRef		inEvent,
    void*		inUserData )
{
    (void) nextHandler;
    OSStatus	 result = eventNotHandledErr;
    OSStatus	 err = eventNotHandledErr;
    UInt32	 eventClass = GetEventClass(inEvent);
    UInt32	 eventKind = GetEventKind(inEvent);
    CarbonBackend* be = (CarbonBackend*) inUserData;

    dbg_print_event("EPxAppEventHandler", inEvent);

    if (eventClass == 'EPX ') {
	CarbonWindow* cwin;
	GetEventParameter( inEvent, 'EPX ', 'CWIN', 
			   0, sizeof(CarbonWindow*), 0, &cwin);
	EPX_DBGFMT("EPxAppEventHandler:cwin = %p", cwin);
	if (cwin == NULL)
	    return eventNotHandledErr;
	if (eventKind == kEpxEventNew) {
	    static const EventTypeSpec    kWindowEvents[] =
		{
  		    { kEventClassCommand,  kEventCommandProcess },
  		    { kEventClassCommand,  kEventProcessCommand },
		    { kEventClassCommand,  kEventCommandUpdateStatus },
		    { kEventClassKeyboard, kEventRawKeyDown },
		    { kEventClassKeyboard, kEventRawKeyUp },
		    { kEventClassMouse,    kEventMouseDown },
		    { kEventClassMouse,    kEventMouseUp },
		    { kEventClassMouse,    kEventMouseDragged },
		    { kEventClassMouse,    kEventMouseWheelMoved},
		    // {kEventClassMouse,  kEventMouseEntered}
		    // {kEventClassMouse,  kEventMouseExited}
		    // { kEventClassWindow,   kEventWindowClickContentRgn },
		    // { kEventClassWindow,   kEventWindowDrawContent },
		    { kEventClassWindow,   kEventWindowBoundsChanged },
		    { kEventClassWindow,   kEventWindowFocusAcquired },
		    { kEventClassWindow,   kEventWindowFocusRelinquish },
		    { kEventClassWindow,   kEventWindowClose } 
		};

	    if ((err = HIWindowCreate(cwin->winClass,
				      cwin->winAttrs,
				      0, // inDefSpec
				      kHICoordSpaceScreenPixel,
				      &(cwin->winBounds),
				      &(cwin->winRef))) != noErr) {
		EPX_DBGFMT("EPxAppEventHandler: could not create window %d",
			(int) err);
		cwin->winErr = err;
		cwin->winIsSetup = 1;
		return err;
	    }

	    // FIXME: allow erlang apps to change this
	    SetWindowTitleWithCFString (cwin->winRef, CFSTR("EPx"));
	    SetWRefCon(cwin->winRef, (long) cwin); // link to CarbonWindow

	    if ((err = InstallStandardEventHandler(GetWindowEventTarget(cwin->winRef))) != noErr) {
		EPX_DBGFMT("EPxAppEventHandler: could not install standard event handler %d",
			(int) err);
		cwin->winErr = err;
		cwin->winIsSetup = 1;
		return err;
	    }
	    err = InstallWindowEventHandler(cwin->winRef,
					    GetEPxWindowEventHandlerUPP(),
					    GetEventTypeCount( kWindowEvents ), 
					    kWindowEvents,
					    cwin, NULL );
	    cwin->ewin->opengl = 0;
	    if (be->b.opengl && be->b.use_opengl) {
#ifdef HAVE_OPENGL
		if ((carbon_gl_setup(cwin) == noErr) && cwin->aglContext)
		    cwin->ewin->opengl = 1;
#endif
	    }
	    ShowWindow(cwin->winRef);
	    SelectWindow(cwin->winRef);
	    BringToFront(cwin->winRef);
	    SetUserFocusWindow(cwin->winRef); //New
	    cwin->winErr = noErr;
	    cwin->winIsSetup = 1;
	    result = noErr;
	}
	else if (eventKind == kEpxEventDel) {
	    EPX_DBGFMT("EPxAppEventHandler: kEpxEventDel, cwin=%p", cwin);
	    DisposeWindow(cwin->winRef);
#ifdef HAVE_OPENGL
	    if (cwin->aglContext)
		carbon_gl_cleanup(cwin);
#endif
	    free(cwin);
	    result = noErr;
	}
    }
    return result;
}

// FilterEvent: return 1 to discard, return 0 to keep
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
	if ((e->pointer.x < 0) || (e->pointer.y < 0))
	    return 1;
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


static pascal OSStatus EPxWindowEventHandler( 
    EventHandlerCallRef nextHandler, 
    EventRef inEvent, 
    void* inRefcon )
{
    (void) nextHandler;
    OSStatus    result = eventNotHandledErr;
    OSStatus    err;
    UInt32	eventClass = GetEventClass(inEvent);
    UInt32	eventKind = GetEventKind(inEvent);
    CarbonWindow*  cwin = (CarbonWindow*) inRefcon;
    epx_window_t*  ewin;
    CarbonBackend* be;
    WindowRef      window;
    epx_event_t e;

    dbg_print_event("EPxWindowEventHandler", inEvent);

    if (!cwin)
	return result;

    window = cwin->winRef;
    ewin = cwin->ewin;
    be   = ewin ? (CarbonBackend*)ewin->backend : 0;

    e.window = ewin;

    switch (eventClass) {
    case kEventClassKeyboard: {
	char ch;
	UInt32 keyCode;
	UInt32 keyMod;

	err = GetEventParameter(inEvent, kEventParamKeyMacCharCodes, 
				typeChar, NULL, sizeof(ch), NULL, &ch);
	err = GetEventParameter(inEvent, kEventParamKeyCode,
				typeUInt32, NULL, sizeof(keyCode),
				NULL, &keyCode);
	err = GetEventParameter(inEvent, kEventParamKeyModifiers,
				typeUInt32, NULL, sizeof(keyMod),
				NULL, &keyMod);
	if (eventKind == kEventRawKeyDown)
	    e.type = EPX_EVENT_KEY_PRESS;
	else if (eventKind == kEventRawKeyUp)
	    e.type = EPX_EVENT_KEY_RELEASE;
	e.key.mod  = 0;
	if (keyMod & shiftKey)
	    e.key.mod |= EPX_KBD_MOD_LSHIFT;
	if (keyMod & controlKey)
	    e.key.mod |= EPX_KBD_MOD_LCTRL;
	if (keyMod & optionKey)
	    e.key.mod |= EPX_KBD_MOD_LALT;
	if (keyMod & alphaLock)
	    e.key.mod |= EPX_KBD_MOD_CAPS;
	if (keyMod & kEventKeyModifierNumLockMask)
	    e.key.mod |= EPX_KBD_MOD_NUM;
	// keyMod & kEventKeyModifierFnMask

	switch(ch) {
	case kBackspaceCharCode:
	    e.key.sym = EPX_KBD_KEY_BACKSPACE; break;
	case kTabCharCode:
	    e.key.sym = EPX_KBD_KEY_TAB; break;	    
	case kReturnCharCode:
	    e.key.sym = EPX_KBD_KEY_ENTER; break;
	case kLeftArrowCharCode:
	    e.key.sym = EPX_KBD_KEY_LEFT; break;
	case kRightArrowCharCode:
	    e.key.sym = EPX_KBD_KEY_RIGHT; break;
	case kUpArrowCharCode:
	    e.key.sym = EPX_KBD_KEY_UP; break;
	case kDownArrowCharCode:
	    e.key.sym = EPX_KBD_KEY_DOWN; break;
	case kPageUpCharCode:
	    e.key.sym = EPX_KBD_KEY_PAGEUP; break;
	case kPageDownCharCode:
	    e.key.sym = EPX_KBD_KEY_PAGEDOWN; break;
	case kDeleteCharCode:
	    e.key.sym = EPX_KBD_KEY_DELETE; break;	    
	default:
	    e.key.sym = (ch & 0x7f);
	    break;
	}
//  kVerticalTabCharCode          = 11,
//  kFunctionKeyCharCode          = 16,
//  kCommandCharCode              = 17,
//  kAppleLogoCharCode            = 20,
//  kEscapeCharCode               = 27,
//  kClearCharCode                = 27,
//  kSpaceCharCode                = 32,
//  kDeleteCharCode               = 127,
	e.key.code = keyCode;
	EPX_DBGFMT("Keyboard event: fd=%d", be ? be->ofd : -1);
	goto notHandled_event;
    }

    case kEventClassMouse: {
	HIPoint where;
	HIRect  bounds;
	UInt32  modifiers;
	EventMouseButton button = 0;
	EventMouseWheelAxis axis = 0;
	SInt32 delta;

	GetEventParameter(inEvent, kEventParamMouseLocation, 
			  typeHIPoint, NULL, sizeof(HIPoint), 
			  NULL, &where);

	GetEventParameter(inEvent, kEventParamKeyModifiers,
			  typeUInt32, NULL, sizeof(UInt32), 
			  NULL, &modifiers);

	if (eventKind == kEventMouseWheelMoved) {
	    e.type = EPX_EVENT_BUTTON_PRESS;
	    GetEventParameter(inEvent, kEventParamMouseWheelAxis,
			      typeMouseWheelAxis, NULL, sizeof(axis), 
			      NULL, &axis);
	    GetEventParameter(inEvent, kEventParamMouseWheelDelta,
			      typeSInt32, NULL, sizeof(SInt32),
			      NULL, &delta);
	}
	else {
	    if (eventKind == kEventMouseDown)
		e.type = EPX_EVENT_BUTTON_PRESS;
	    else if (eventKind == kEventMouseUp)
		e.type = EPX_EVENT_BUTTON_RELEASE;
	    else if (eventKind == kEventMouseDragged)
		e.type = EPX_EVENT_POINTER_MOTION;
	    else
		break;
	    GetEventParameter(inEvent, kEventParamMouseButton,
			      typeMouseButton, NULL, sizeof(button), 
			      NULL, &button);
	}


	if (button == kEventMouseButtonPrimary)
	    e.pointer.button = EPX_BUTTON_LEFT;
	else if (button == kEventMouseButtonTertiary)
	    e.pointer.button = EPX_BUTTON_MIDDLE;
	else if (button == kEventMouseButtonSecondary)
	    e.pointer.button = EPX_BUTTON_RIGHT;
	else if (axis == kEventMouseWheelAxisX) {
	    if (delta > 0)
		e.pointer.button = EPX_WHEEL_LEFT;
	    else if (delta < 0)
		e.pointer.button = EPX_WHEEL_RIGHT;
	}
	else if (axis == kEventMouseWheelAxisY) {
	    if (delta > 0)
		e.pointer.button = EPX_WHEEL_UP;
	    else if (delta < 0)
		e.pointer.button = EPX_WHEEL_DOWN;
	}
	else
	    break;

	HIWindowGetBounds(window, kWindowContentRgn,
			  kHICoordSpaceScreenPixel,
			  &bounds);
	e.pointer.x = where.x - bounds.origin.x;
	e.pointer.y = where.y - bounds.origin.y;;
	e.pointer.z = 0;
	EPX_DBGFMT("Mouse event(%d): %d (%d,%d)", 
		e.type,button,where.x,where.y);
	goto notHandled_event;
    }

    case kEventClassWindow:
        /* if ( eventKind == kEventWindowDrawContent ) {
	    SetUserFocusWindow(cwin->winRef);
	    EPX_DBGFMT("WindowDrawContent");
        }
        else */
	if ( eventKind == kEventWindowClose ) {
	    EPX_DBGFMT("WindowClose");
	    e.type = EPX_EVENT_CLOSE;
	    goto handled_event;
        }
        else if (eventKind == kEventWindowClickContentRgn) {
            // Point   where;
	    HIPoint where;
            UInt32  modifiers;
	    EPX_DBGFMT("kEventWindowClickContentRgn");
/*
            GetEventParameter(inEvent, kEventParamMouseLocation, 
			      typeQDPoint, NULL, sizeof(Point), 
			      NULL, &where);
*/
            GetEventParameter(inEvent, kEventParamMouseLocation, 
			      typeHIPoint, NULL, sizeof(HIPoint), 
			      NULL, &where);

            GetEventParameter(inEvent, kEventParamKeyModifiers, 
			      typeUInt32, NULL, sizeof(UInt32), 
			      NULL, &modifiers);
	    // What should we use?
            // QDGlobalToLocalPoint(GetWindowPort(window), &where);

	    e.type = EPX_EVENT_BUTTON_PRESS;
	    e.pointer.x = where.x; // where.h;
	    e.pointer.y = where.y; // where.v;
	    e.pointer.z = 0;
	    EPX_DBGFMT("WindowClickContentRgn %d, %d", where.x, where.y);
	    goto handled_event;
        }
        else if ( eventKind == kEventWindowFocusAcquired) {
	    EPX_DBGFMT("kEventWindowFocusAcquired");
	    e.type = EPX_EVENT_FOCUS_IN;
	    goto handled_event;
	}
        else if ( eventKind == kEventWindowFocusRelinquish) {
	    EPX_DBGFMT("kEventWindowFocusRelinquish");
	    e.type = EPX_EVENT_FOCUS_OUT;
	    goto handled_event;
	}
        else if ( eventKind == kEventWindowBoundsChanged ) {
            Rect r;
	    EPX_DBGFMT("kEventWindowBoundsChanged");
            GetEventParameter( inEvent, kEventParamCurrentBounds, 
			       typeQDRectangle, NULL, sizeof(Rect),
			       NULL, &r);
            // We get here also when the window gets moved and doesn't 
	    // need to be redrawn.
	    EPX_DBGFMT("WindowBoundsChanged %d,%d,%d,%d",
		    r.top, r.left, r.bottom, r.right);
	    e.type = EPX_EVENT_RESIZE;
	    e.dimension.w = (r.right-r.left)+1;
	    e.dimension.h = (r.bottom-r.top)+1;
	    e.dimension.d = 0;
	    goto handled_event;
        }
	else {
	    EPX_DBGFMT("Other event");
	}
        break;
	

    case kEventClassCommand: {
	HICommandExtended cmd;

	if (eventKind == kEventCommandProcess) {
	    GetEventParameter( inEvent, kEventParamDirectObject, typeHICommand,
			       NULL, sizeof( cmd ), NULL, &cmd);
	    switch ( cmd.commandID ) {
		// Add your own command-handling cases here
	    default:
		break;
	    }
	    break;
	}
	break;
    }

    default:
	break;
    }
    return result;

handled_event:
    if (!be || FilterEvent(&e))
	return noErr;
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
    return noErr;

notHandled_event:
    if (!be || FilterEvent(&e)) 
	return eventNotHandledErr;	
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
    return eventNotHandledErr;
}

int carbon_adjust(epx_backend_t *backend, epx_dict_t* param)
{
    (void) backend;
    (void) param;
    EPX_DBGFMT("carbon: Backend adjust");
    return 1;
}

int carbon_win_adjust(epx_window_t* win, epx_dict_t* param)
{
    int width, wi;
    int height, hi;
    int bool_val;
    CarbonWindow* cwin = (CarbonWindow*)win->opaque;

    EPX_DBGFMT("carbon: Window adjust");
    wi=epx_dict_lookup_integer(param, "width", &width);
    hi=epx_dict_lookup_integer(param, "height", &height);

    EPX_DBGFMT("carbon:%d width=%d", wi, width);
    EPX_DBGFMT("carbon:%d height=%d", hi, height);

    if ((wi >= 0)||(hi >= 0)) {
	if (wi >= 0) {
	    cwin->winBounds.size.width = width;
	}
	else
	    width = cwin->winBounds.size.width;
	if (hi >= 0) {
	    cwin->winBounds.size.height = height;
	}
	else
	    height = cwin->winBounds.size.height;
	SizeWindow(cwin->winRef, width, height, false);
    }
    
    if (epx_dict_lookup_boolean(param, "show", &bool_val) >= 0) {
	if (bool_val)
	    ShowWindow(cwin->winRef);
	else
	    HideWindow(cwin->winRef);
    }

    if (epx_dict_lookup_boolean(param, "select", &bool_val) >= 0) {
	if (bool_val)
	    SelectWindow(cwin->winRef);
    }
    //  "focus", "modal" ...
    return 1;
}

int carbon_info(epx_backend_t *backend, epx_dict_t* param)
{
    (void) backend;
    (void) param;
    EPX_DBGFMT("carbon: info");
    return 0;
}

#endif
