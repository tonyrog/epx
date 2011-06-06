/*
 *  Mac Os X display driver
 *
 * Where:
 *  /System/Library/Frameworks/ApplicationServices.framework/Frameworks/
 *    CoreGraphics.framework/Headers
 */
#include <Carbon/Carbon.h>
#include <Cocoa/Cocoa.h>
#include <objc/objc-runtime.h>

#include <machine/endian.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

#undef HAVE_OPENGL   // not yet

#include "epic.h"
#ifdef HAVE_OPENGL
#include <AGL/agl.h>
#endif


#define kEpicEventNew 1
#define kEpicEventDel 2

typedef struct {
    EBackend b;
    int     ofd;                /* pipe output signal */
    MenuRef menu;
    unsigned short modstate;    /* modifier state */
    unsigned short grab_key;    /* grab key */
} CocoaBackend;

typedef struct {
    EWindow*         ewin;       // reference to EPic representaion
    NSRect           winBounds;
    NSUInteger       winAttrs;
    NSWindow*        winNS;       // Cocoa window
    WindowRef        winRef;      // window ref
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
} CocoaWindow;

typedef struct {
    CGBitmapInfo bitmapinfo;
    CGColorSpaceRef colorspace;
    CGDataProviderRef provider;
} CocoaPixels;


EBackend* cocoa_init(EDict* param);

static int cocoa_finish(EBackend*);
static int cocoa_pic_attach(EBackend*, EPixmap* pic);
static int cocoa_pic_detach(EBackend*, EPixmap* pic);
static int cocoa_begin(EWindow* ewin);
static int cocoa_end(EWindow* ewin, int off_screen);
static int cocoa_pic_draw(EBackend*, EPixmap* pic, EWindow* win,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height);
static int cocoa_win_attach(EBackend*, EWindow* win);
static int cocoa_win_detach(EBackend*, EWindow* win);
static int cocoa_win_swap(EBackend*, EWindow* win);
static EHANDLE_T cocoa_evt_attach(EBackend*);
static int cocoa_evt_detach(EBackend*);
static int cocoa_evt_read(EBackend*, EEvent*);
static int cocoa_adjust(EBackend *backend, EDict* param);
static int cocoa_win_adjust(EWindow*, EDict* param);

static EPicCallbacks cocoa_callbacks =
{
    cocoa_finish,
    cocoa_pic_attach,
    cocoa_pic_detach,
    cocoa_pic_draw,
    cocoa_win_attach,
    cocoa_win_detach,
    cocoa_evt_attach,
    cocoa_evt_detach,
    cocoa_evt_read,
    cocoa_adjust,
    cocoa_win_swap,
    cocoa_begin,
    cocoa_end,
    cocoa_win_adjust
};


/* util */

static pthread_t cocoa_thr;
static pthread_mutex_t cocoa_lock;

static pascal OSStatus EPicAppEventHandler(
    EventHandlerCallRef	inHandlerCallRef,
    EventRef		inEvent,
    void*		inUserData);

static pascal OSStatus EPicWindowEventHandler(
    EventHandlerCallRef	inHandlerCallRef,
    EventRef		inEvent,
    void*		inUserData);
//----------------------------------------------------------------------------
DEFINE_ONE_SHOT_HANDLER_GETTER( EPicWindowEventHandler )

DEFINE_ONE_SHOT_HANDLER_GETTER( EPicAppEventHandler )

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

    EDBGFMT("%s: class='%s', kind=%lu", label, e_class, eventKind);
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
static OSStatus cocoa_gl_setup(CocoaWindow* cwin)
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

static OSStatus cocoa_gl_cleanup(CocoaWindow* cwin)
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

#if 0
static int32_t sUMASystemVersion = 0 ;

long UMAGetSystemVersion() { return sUMASystemVersion ; }
#endif

void* cocoa_event_thread(void* arg)
{
    CocoaBackend* be = (CocoaBackend*) arg;
    OSStatus err;
    EventTypeSpec appEventList[] =
	{ { 'EPIC',  kEpicEventNew},
	  { 'EPIC',  kEpicEventDel},
	  { kEventClassCommand, kEventProcessCommand } };
    // EventRef theEvent;
    // EventTargetRef theTarget;

    EDBGFMT("cocoa_event_thread: running ofd=%d", be->ofd);

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

    err = InstallApplicationEventHandler( GetEPicAppEventHandlerUPP(),
					  GetEventTypeCount( appEventList ), 
					  appEventList, be, NULL );

    pthread_mutex_unlock(&cocoa_lock);

    RunApplicationEventLoop();

    close(be->ofd);  // signal termination!
    return NULL;
}


EBackend* cocoa_init(EDict* param)
{
#pragma unused(param)
    CocoaBackend* be;
    int int_param;
    int cocoa_pipe[2];

    if ((be = (CocoaBackend*) malloc(sizeof(CocoaBackend))) == NULL)
	return NULL;
    EOBJECT_INIT((EBackend*)be, EBACKEND_TYPE);

    be->b.cb = &cocoa_callbacks;
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;
    be->b.pixmap_list = NULL;
    be->b.window_list = NULL;
    be->b.event = INVALID_HANDLE;

    if (EDictLookupInteger(param, "use_opengl", &int_param) != -1)
	be->b.use_opengl = int_param;
#ifdef HAVE_OPENGL
    be->b.opengl = 1;
#endif

    pipe(cocoa_pipe);
    be->b.event = (EHANDLE_T) cocoa_pipe[0];
    be->ofd     = cocoa_pipe[1];
    pthread_mutex_init(&cocoa_lock, NULL);
    pthread_mutex_lock(&cocoa_lock);  // Lock until thread is initialised

    pthread_create(&cocoa_thr, NULL, cocoa_event_thread, 
		   (void*) be);
    pthread_mutex_lock(&cocoa_lock);    // Wait for init to complete
    pthread_mutex_unlock(&cocoa_lock);  // Wait for init to complete

    return (EBackend*) &(be->b);
}

/* return the backend event handle */
static EHANDLE_T cocoa_evt_attach(EBackend* backend)
{
    CocoaBackend* be = (CocoaBackend*) backend;
    return (EHANDLE_T) be->b.event;
}

static int cocoa_evt_detach(EBackend* backend)
{
    (void) backend;
    return 0;
}

static int cocoa_evt_read(EBackend* backend, EEvent* e)
{
    int r = 0;
    
    if ((r=backend->pending) == 0) // LOCK atomic check
	return 0;
    if (read((int)backend->event, (void*) e, sizeof(EEvent)) < 0)
	return -1;
    backend->pending--;  // LOCK atomic! check
    return r;
}

static int cocoa_finish(EBackend* backend)
{
    CocoaBackend* be = (CocoaBackend*) backend;

    free(be);
    return 0;
}

static void pixel_ReleaseInfo(void *info)
{
    (void) info;
    EDBGFMT_MEM("ReleaseInfo");
    /* NOOP */
}

static const void* pixel_GetBytePointer(void* info)
{
    EPixmap* pic = (EPixmap*) info;
    return (void*) pic->data;
}

static size_t pixel_GetBytesAtOffset(void *info, void *buffer, 
				     off_t offset, size_t count)
{
    EPixmap* pic = (EPixmap*) info;
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

static int cocoa_pic_attach(EBackend* backend, EPixmap* pixmap)
{
    CocoaBackend* be = (CocoaBackend*) backend;
    CocoaPixels* pe;

    if (pixmap->opaque != NULL)
	return -1;
    if ((pe = (CocoaPixels*) malloc(sizeof(CocoaPixels))) == NULL)
	return -1;

    pe->colorspace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    pe->bitmapinfo = 0;

    switch(pixmap->pixelType) {
    case EPIXEL_TYPE_RGB:
	pe->bitmapinfo |= kCGImageAlphaNone;         /* RGB format */
	break;
    case EPIXEL_TYPE_RGBA:
	pe->bitmapinfo |= kCGBitmapByteOrder32Big;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipLast; /* RGBA format */
	break;
    case EPIXEL_TYPE_ARGB:
	pe->bitmapinfo |= kCGBitmapByteOrder32Big;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst; /* ARGB format */
	break;
    case EPIXEL_TYPE_BGR: /* ??? */
	pe->bitmapinfo |= kCGImageAlphaNone;         /* BGR format */
	break;
    case EPIXEL_TYPE_BGRA: /* ARGB */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst;
	break;
    case EPIXEL_TYPE_ABGR: /* RGBA */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipLast;
	break;
    default:
	fprintf(stderr, "macos: unhandled pixelType = %d\n", pixmap->pixelType);
	free(pe);
	return -1;
    }

    pe->provider = CGDataProviderCreateDirect(
	(void*) pixmap, pixmap->sz, &dacallbacks);
    EObjectLink(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) pe;
    pixmap->backend = (EBackend*) be;
    return 0;
}

						
static int cocoa_pic_detach(EBackend* backend, EPixmap* pixmap)
{
    CocoaPixels* pe = (CocoaPixels*) pixmap->opaque;

    if (pe != NULL) {
	CGColorSpaceRelease(pe->colorspace);
	CGDataProviderRelease(pe->provider);
	EObjectUnlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}

/* There must be a better way */
static void draw_image(CGContextRef ctx, CGRect srcRect, CGRect dstRect,
		       EPixmap* pic)
{
    CocoaPixels* pe = (CocoaPixels*) pic->opaque;
    CGImageRef img;
    CGImageRef srcImg;
    unsigned int bytesPerRow   = pic->bytesPerRow;
    unsigned int bitsPerPixel  = pic->bitsPerPixel;

    // Boring to create this every time!
    img = CGImageCreate(pic->width,               /* width in pixels */
			pic->height,              /* height in pixels */
			8,                        /* bitsPerComponent */
			bitsPerPixel,             /* bitsPerPixel */
			bytesPerRow,              /* bytesPerRow */
			pe->colorspace,           /* colorspace */
			pe->bitmapinfo,           /* bitmapinfo, */
			pe->provider,
			NULL,
			false,
			kCGRenderingIntentSaturation);
    if (!img) {
	fprintf(stderr, "epic_macos: error CGImageCreate\n");
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

    if (srcImg != NULL) {
	dstRect = CGContextConvertRectToUserSpace(ctx, dstRect);
	CGContextDrawImage(ctx, dstRect, srcImg);
    }
    if (srcImg && (srcImg != img))
	CGImageRelease(srcImg);
    CGImageRelease(img);
}

static int cocoa_begin(EWindow* ewin)
{
    CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;

    if ((cwin == NULL) || (cwin->winRef == 0))
	return -1;

    cwin->port = GetWindowPort(cwin->winRef);
    pthread_mutex_lock(&cocoa_lock);

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

static int cocoa_end(EWindow* ewin, int off_screen)
{
    (void) off_screen;
    CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;

    if (cwin == NULL)
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
	/* CGContextSynchronize(Ctx); */
	CGContextRestoreGState(cwin->ctx);
	QDEndCGContext(cwin->port, &cwin->ctx);
    }
    pthread_mutex_unlock(&cocoa_lock);

    return 0;
}



static int cocoa_pic_draw(EBackend* backend, EPixmap* pic, EWindow* ewin,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height)

{
    (void) backend;
    CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;

    if (cwin == NULL) 
	return -1;
    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	// Fixme check for errors
	EGlLoadTexture(pic, &cwin->textureName, 0, 1,  GL_REPEAT,
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

static int cocoa_win_swap(EBackend* backend, EWindow* ewin)
{
    (void) backend;

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;
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

static int cocoa_win_attach(EBackend* backend, EWindow* ewin)
{
    CocoaBackend* be = (CocoaBackend*) backend;
    CocoaWindow* cwin;
    EventRef      newEvent;

    if (ewin->opaque != NULL)
	return -1;

    if ((cwin = (CocoaWindow*) malloc(sizeof(CocoaWindow))) == NULL) 
	return -1;
    memset(cwin, 0, sizeof(CocoaWindow));
    cwin->ewin   = ewin;
    cwin->winErr = noErr;
    cwin->winIsSetup = 0;
    ewin->opaque = (void*) cwin;

    EObjectLink(&backend->window_list, ewin);
    ewin->backend = (EBackend*) be;

    cwin->winAttrs = 
	NSTitledWindowMask |
	NSClosableWindowMask |
	NSMiniaturizableWindowMask |
	NSResizableWindowMask;
	// NSTexturedBackgroundWindowMask	

    cwin->winBounds.origin.x = ewin->x;
    cwin->winBounds.origin.y = ewin->y;
    cwin->winBounds.size.height = ewin->height;
    cwin->winBounds.size.width  = ewin->width;

    EDBGFMT("cocoa_win_attach: cwin=%p", cwin);
    MacCreateEvent(nil, 'EPIC',  kEpicEventNew, GetCurrentEventTime(),
		   kEventAttributeNone, &newEvent);
    SetEventParameter(newEvent, 'EPIC', 'CWIN', sizeof(CocoaWindow*), &cwin);
    PostEventToQueue(GetMainEventQueue(), newEvent, kEventPriorityHigh);
    ReleaseEvent(newEvent);
    EDBGFMT("cocoa_win_attach: post done");

    while(cwin->winIsSetup == 0)
	usleep(1000);
    if (cwin->winErr != noErr) {
	free(cwin);
	return -1;
    }
    return 0;
}

static int cocoa_win_detach(EBackend* backend, EWindow* ewin)
{
    CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;

    if (cwin != NULL) {
	EventRef  delEvent;

	EObjectUnlink(&backend->window_list, ewin);
	ewin->opaque = NULL;
	ewin->backend = NULL;

	MacCreateEvent(nil, 'EPIC',  kEpicEventDel, GetCurrentEventTime(),
		       kEventAttributeNone, &delEvent);
	SetEventParameter(delEvent, 'EPIC', 'CWIN', 
			  sizeof(CocoaWindow*), &cwin);
	PostEventToQueue(GetMainEventQueue(), delEvent, kEventPriorityHigh);
	ReleaseEvent(delEvent);
    }
    return 0;
}


static pascal OSStatus EPicAppEventHandler(
    EventHandlerCallRef nextHandler, 
    EventRef		inEvent,
    void*		inUserData )
{
    (void) nextHandler;
    OSStatus	 result = eventNotHandledErr;
    OSStatus	 err = eventNotHandledErr;
    UInt32	 eventClass = GetEventClass(inEvent);
    UInt32	 eventKind = GetEventKind(inEvent);
    CocoaBackend* be = (CocoaBackend*) inUserData;

    dbg_print_event("EPicAppEventHandler", inEvent);

    if (eventClass == 'EPIC') {
	CocoaWindow* cwin;
	GetEventParameter( inEvent, 'EPIC', 'CWIN', 
			   NULL, sizeof(CocoaWindow*), NULL, &cwin);
	EDBGFMT("EPicAppEventHandler:cwin = %p", cwin);
	if (cwin == NULL)
	    return eventNotHandledErr;
	if (eventKind == kEpicEventNew) {
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
	    cwin->winNS = [[NSWindow alloc]
			   initWithContentRect:cwin->winBounds
			   styleMask:cwin->winAttrs
			   backing:NSBackingStoreBuffered
			   defer:NO];
	    if (!cwin->winNS) {
		EDBGFMT("EPicAppEventHandler: could not create window %d\n",
			(int) err);
		cwin->winErr = paramErr;  // error code?
		cwin->winIsSetup = 1;
		return err;
	    }

	    [cwin->winNS setTitle:@"Epic"];

	    // SetWRefCon(cwin->winRef, (long) cwin); // link to CocoaWindow
	    [cwin->winNS display];
	    // [cwin->winNS setNeedsDisplay:true];
	    // [cwin->winNS showWindow];
	    [cwin->winNS makeKeyAndOrderFront: nil];
	    
	    cwin->winRef = [cwin->winNS windowRef];


	    if ((err = InstallStandardEventHandler(GetWindowEventTarget(cwin->winRef))) != noErr) {
		EDBGFMT("EPicAppEventHandler: could not install standard event handler %d\n",
			(int) err);
		cwin->winErr = err;
		cwin->winIsSetup = 1;
		return err;
	    }
	    err = InstallWindowEventHandler(cwin->winRef,
					    GetEPicWindowEventHandlerUPP(),
					    GetEventTypeCount( kWindowEvents ), 
					    kWindowEvents,
					    cwin, NULL );
	    cwin->ewin->opengl = 0;
	    if (be->b.opengl && be->b.use_opengl) {
#ifdef HAVE_OPENGL
		if ((cocoa_gl_setup(cwin) == noErr) && cwin->aglContext)
		    cwin->ewin->opengl = 1;
#endif
	    }
	    // ShowWindow(cwin->winRef);
	    // SelectWindow(cwin->winRef);
	    // BringToFront(cwin->winRef);
	    // SetUserFocusWindow(cwin->winRef); //New
	    cwin->winErr = noErr;
	    cwin->winIsSetup = 1;
	    result = noErr;
	}
	else if (eventKind == kEpicEventDel) {
#ifdef HAVE_OPENGL
	    if (cwin->aglContext)
		cocoa_gl_cleanup(cwin);
#endif
	    DisposeWindow(cwin->winRef);
	    free(cwin);
	    result = noErr;
	}
    }
    return result;
}

// FilterEvent: return 1 to discard, return 0 to keep
static int FilterEvent(EEvent* e)
{
    u_int32_t mask;

    if (!e->window)
	return 1;
    if (e->type == EEVENT_CLOSE)
	return 0;
    mask = e->window->mask;
    if ((e->type & mask) == 0)
	return 1;
    switch(e->type) {
    case EEVENT_BUTTON_PRESS:
    case EEVENT_BUTTON_RELEASE:
    case EEVENT_POINTER_MOTION:
	if ((e->pointer.x < 0) || (e->pointer.y < 0))
	    return 1;
	mask &= EEVENT_BUTTON_MASK; // only button events
	if (mask == EEVENT_BUTTON_ANY)
	    return 0;
	if ((e->pointer.button & (mask >> 16)) == 0)
	    return 1;
	break;
    default:
	break;
    }
    return 0;
}


static pascal OSStatus EPicWindowEventHandler( 
    EventHandlerCallRef nextHandler, 
    EventRef inEvent, 
    void* inRefcon )
{
    (void) nextHandler;
    OSStatus    result = eventNotHandledErr;
    OSStatus    err;
    UInt32	eventClass = GetEventClass(inEvent);
    UInt32	eventKind = GetEventKind(inEvent);
    CocoaWindow* cwin = (CocoaWindow*) inRefcon;
    EWindow*      ewin;
    CocoaBackend* be;
    WindowRef     window;
    EEvent e;

    if (cwin == NULL)
	return result;

    window = cwin->winRef;
    ewin = cwin->ewin;
    be   = ewin ? (CocoaBackend*)ewin->backend : NULL;
    
    dbg_print_event("EPicWindowEventHandler", inEvent);

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
	    e.type = EEVENT_KEY_PRESS;
	else if (eventKind == kEventRawKeyUp)
	    e.type = EEVENT_KEY_RELEASE;
	e.key.mod  = 0;
	if (keyMod & shiftKey)
	    e.key.mod |= EKBD_MOD_LSHIFT;
	if (keyMod & controlKey)
	    e.key.mod |= EKBD_MOD_LCTRL;
	if (keyMod & optionKey)
	    e.key.mod |= EKBD_MOD_LALT;
	if (keyMod & alphaLock)
	    e.key.mod |= EKBD_MOD_CAPS;
	if (keyMod & kEventKeyModifierNumLockMask)
	    e.key.mod |= EKBD_MOD_NUM;
	// keyMod & kEventKeyModifierFnMask

	switch(ch) {
	case kBackspaceCharCode:
	    e.key.sym = EKBD_KEY_BACKSPACE; break;
	case kTabCharCode:
	    e.key.sym = EKBD_KEY_TAB; break;	    
	case kReturnCharCode:
	    e.key.sym = EKBD_KEY_ENTER; break;
	case kLeftArrowCharCode:
	    e.key.sym = EKBD_KEY_LEFT; break;
	case kRightArrowCharCode:
	    e.key.sym = EKBD_KEY_RIGHT; break;
	case kUpArrowCharCode:
	    e.key.sym = EKBD_KEY_UP; break;
	case kDownArrowCharCode:
	    e.key.sym = EKBD_KEY_DOWN; break;
	case kPageUpCharCode:
	    e.key.sym = EKBD_KEY_PAGEUP; break;
	case kPageDownCharCode:
	    e.key.sym = EKBD_KEY_PAGEDOWN; break;
	case kDeleteCharCode:
	    e.key.sym = EKBD_KEY_DELETE; break;	    
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
	EDBGFMT("Keyboard event: fd=%d\n", be->ofd);
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
	    e.type = EEVENT_BUTTON_PRESS;
	    GetEventParameter(inEvent, kEventParamMouseWheelAxis,
			      typeMouseWheelAxis, NULL, sizeof(axis), 
			      NULL, &axis);
	    GetEventParameter(inEvent, kEventParamMouseWheelDelta,
			      typeSInt32, NULL, sizeof(SInt32),
			      NULL, &delta);
	}
	else {
	    if (eventKind == kEventMouseDown)
		e.type = EEVENT_BUTTON_PRESS;
	    else if (eventKind == kEventMouseUp)
		e.type = EEVENT_BUTTON_RELEASE;
	    else if (eventKind == kEventMouseDragged)
		e.type = EEVENT_POINTER_MOTION;
	    else
		break;
	    GetEventParameter(inEvent, kEventParamMouseButton,
			      typeMouseButton, NULL, sizeof(button), 
			      NULL, &button);
	}


	if (button == kEventMouseButtonPrimary)
	    e.pointer.button = EBUTTON_LEFT;
	else if (button == kEventMouseButtonTertiary)
	    e.pointer.button = EBUTTON_MIDDLE;
	else if (button == kEventMouseButtonSecondary)
	    e.pointer.button = EBUTTON_RIGHT;
	else if (axis == kEventMouseWheelAxisX) {
	    if (delta > 0)
		e.pointer.button = EWHEEL_LEFT;
	    else if (delta < 0)
		e.pointer.button = EWHEEL_RIGHT;
	}
	else if (axis == kEventMouseWheelAxisY) {
	    if (delta > 0)
		e.pointer.button = EWHEEL_UP;
	    else if (delta < 0)
		e.pointer.button = EWHEEL_DOWN;
	}
	else
	    break;

	HIWindowGetBounds(window, kWindowContentRgn,
			  kHICoordSpaceScreenPixel,
			  &bounds);
	e.pointer.x = where.x - bounds.origin.x;
	e.pointer.y = where.y - bounds.origin.y;;
	e.pointer.z = 0;
	EDBGFMT("Mouse event(%d): %d (%d,%d)\n", 
		e.type,button,where.x,where.y);
	goto notHandled_event;
    }

    case kEventClassWindow:
        /* if ( eventKind == kEventWindowDrawContent ) {
	    SetUserFocusWindow(cwin->winRef);
	    EDBGFMT("WindowDrawContent\n");
        }
        else */ if ( eventKind == kEventWindowClose ) {
	    EDBGFMT("WindowClose\n");
	    e.type = EEVENT_CLOSE;
	    goto handled_event;
        }
        else if (eventKind == kEventWindowClickContentRgn) {
            // Point   where;
	    HIPoint where;
            UInt32  modifiers;
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

	    e.type = EEVENT_BUTTON_PRESS;
	    e.pointer.x = where.x; // where.h;
	    e.pointer.y = where.y; // where.v;
	    e.pointer.z = 0;
	    EDBGFMT("WindowClickContentRgn %d, %d\n", where.x, where.y);
	    goto handled_event;
        }
        else if ( eventKind == kEventWindowFocusAcquired) {
	    e.type = EEVENT_FOCUS_IN;
	    goto handled_event;
	}
        else if ( eventKind == kEventWindowFocusRelinquish) {
	    e.type = EEVENT_FOCUS_OUT;
	    goto handled_event;
	}
        else if ( eventKind == kEventWindowBoundsChanged ) {
            Rect r;

            GetEventParameter( inEvent, kEventParamCurrentBounds, 
			       typeQDRectangle, NULL, sizeof(Rect),
			       NULL, &r);
            // We get here also when the window gets moved and doesn't 
	    // need to be redrawn.
	    EDBGFMT("WindowBoundsChanged %d,%d,%d,%d\n",
		    r.top, r.left, r.bottom, r.right);
	    e.type = EEVENT_RESIZE;
	    e.dimension.w = (r.right-r.left)+1;
	    e.dimension.h = (r.bottom-r.top)+1;
	    e.dimension.d = 0;
	    goto handled_event;
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
    if (FilterEvent(&e))
	return noErr;
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
    return noErr;

notHandled_event:
    if (FilterEvent(&e)) 
	return eventNotHandledErr;	
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
    return eventNotHandledErr;
}

int cocoa_adjust(EBackend *backend, EDict* param)
{
    (void) backend;
    (void) param;
    printf("cocoa: Backend adjust\n");
    return 1;
}

int cocoa_win_adjust(EWindow *win, EDict* param)
{
    int width, wi;
    int height, hi;
    int bool_val;
    CocoaWindow* cwin = (CocoaWindow*)win->opaque;

    printf("cocoa: Window adjust\n");
    wi=EDictLookupInteger(param, "width", &width);
    hi=EDictLookupInteger(param, "height", &height);

    printf("cocoa:%d width=%d\n", wi, width);
    printf("cocoa:%d height=%d\n", hi, height);

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
    
    if (EDictLookupBoolean(param, "show", &bool_val) >= 0) {
	if (bool_val)
	    ShowWindow(cwin->winRef);
	else
	    HideWindow(cwin->winRef);
    }

    if (EDictLookupBoolean(param, "select", &bool_val) >= 0) {
	if (bool_val)
	    SelectWindow(cwin->winRef);
    }
    //  "focus", "modal" ...
    return 1;
}



