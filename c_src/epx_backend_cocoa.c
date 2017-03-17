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
#include <Carbon/Carbon.h>
#include <Cocoa/Cocoa.h>
#include <objc/objc-runtime.h>

#include <machine/endian.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

#import <AppKit/AppKit.h>
#import <Foundation/NSThread.h>
#import <Foundation/Foundation.h>

#undef HAVE_OPENGL   // not yet

#include "../include/epx_backend.h"
#ifdef HAVE_OPENGL
#include <AGL/agl.h>
#endif

#define debug

#define kEpxEventNew 1
#define kEpxEventDel 2

struct CocoaWindow;
struct CocoaBackend;

@interface AppWindow : NSWindow <NSWindowDelegate> {
    struct CocoaWindow_* cwindow;
}
@property struct CocoaWindow_* cwindow;
- (void)windowDidResize:(NSNotification *)notification;
- (void)windowDidMove:(NSNotification *)notification;
- (BOOL)windowShouldClose:(id)sender;
@end

@interface AppApplication : NSApplication {
struct CocoaBackend_* backend;
}
@property struct CocoaBackend_* backend;
- (void)create_window:(NSEvent*) theEvent;
- (void)close_window:(NSEvent*) theEvent;
@end

typedef struct CocoaBackend_ {
    epx_backend_t b;
    int     ofd;                // pipe output signal
    NSMenu* menu;
    unsigned short modstate;    // modifier state
    unsigned short grab_key;    // grab key
} CocoaBackend;

typedef struct CocoaWindow_ {
    epx_window_t*    ewin;        // reference to EPx representaion
    NSRect           winBounds;
    NSUInteger       winAttrs;
    AppWindow*       winNS;       // application window
    CGContextRef     ctx;         // graphics port
    int              winIsSetup;  // 1 when window setup is done
    OSStatus         winErr;      // indicate window setup error (!=noErr)
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

epx_backend_t* cocoa_init(epx_dict_t* param);
int cocoa_upgrade(epx_backend_t* be);

static int cocoa_finish(epx_backend_t*);
static int cocoa_pix_attach(epx_backend_t*, epx_pixmap_t* pic);
static int cocoa_pix_detach(epx_backend_t*, epx_pixmap_t* pic);
static int cocoa_begin(epx_window_t* ewin);
static int cocoa_end(epx_window_t* ewin, int off_screen);
static int cocoa_pix_draw(epx_backend_t*, epx_pixmap_t* pic, epx_window_t* win,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height);
static int cocoa_win_attach(epx_backend_t*, epx_window_t* win);
static int cocoa_win_detach(epx_backend_t*, epx_window_t* win);
static int cocoa_win_swap(epx_backend_t*, epx_window_t* win);
static EPX_HANDLE_T cocoa_evt_attach(epx_backend_t*);
static int cocoa_evt_detach(epx_backend_t*);
static int cocoa_evt_read(epx_backend_t*, epx_event_t*);
static int cocoa_adjust(epx_backend_t *backend, epx_dict_t* param);
static int cocoa_win_adjust(epx_window_t*, epx_dict_t* param);
static int cocoa_info(epx_backend_t *backend, epx_dict_t* param);

static epx_callbacks_t cocoa_callbacks =
{
    .finish = cocoa_finish,
    .pix_attach = cocoa_pix_attach,
    .pix_detach = cocoa_pix_detach,
    .pix_draw   = cocoa_pix_draw,
    .win_attach = cocoa_win_attach,
    .win_detach = cocoa_win_detach,
    .evt_attach = cocoa_evt_attach,
    .evt_detach = cocoa_evt_detach,
    .evt_read   = cocoa_evt_read,
    .adjust     = cocoa_adjust,
    .win_swap   = cocoa_win_swap,
    .begin      = cocoa_begin,
    .end        = cocoa_end,
    .win_adjust = cocoa_win_adjust,
    .info       = cocoa_info
};

// #define DBG(...) NSLog(__VA_ARGS__)
#define DBG(...) 
/* util */

static pthread_t cocoa_thr;
static pthread_mutex_t cocoa_lock;

static int FilterEvent(epx_event_t* e);

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
    aglSetWindowRef(cwin->aglContext, [cwin->winNS windowRef]);

    save = aglGetCurrentContext();
    aglSetCurrentContext(cwin->aglContext);
    aglUpdateContext (cwin->aglContext);
    // FIXME: setup from epx_dict_t
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

@interface CocoaMultithreading : NSObject
+ (void)beginMultithreading;
@end

@implementation CocoaMultithreading
+ (void)dummyThread:(id)unused
{
    (void)unused;
}

+ (void)beginMultithreading
{
    [NSThread detachNewThreadSelector:@selector(dummyThread:)
            toTarget:self withObject:nil];
}
@end

@interface WinView : NSView {
    CocoaBackend* backend;
}
@property CocoaBackend* backend;
- (id)init;
- (void)mouseDown:(NSEvent*) theEvent;
- (void)mouseUp:(NSEvent*) theEvent;
- (void)mouseDragged:(NSEvent*) theEvent;
- (void)rightMouseDown:(NSEvent*) theEvent;
- (void)rightMouseUp:(NSEvent*) theEvent;
- (void)rightMouseDragged:(NSEvent*) theEvent;
- (void)keyDown:(NSEvent*) theEvent;
- (void)keyUp:(NSEvent*) theEvent;
- (void)mouseEntered:(NSEvent*) theEvent;
- (void)mouseExited:(NSEvent*) theEvent;
- (void)flagsChanged:(NSEvent*) theEvent;

- (void)crossing_event:(NSEvent*) theEvent andType:(int)type;
- (void)pointer_event:(NSEvent*) theEvent andType:(int)type;
- (void)key_event:(NSEvent*) theEvent andType:(int)type;

@end

@implementation WinView
@synthesize backend;

- (id)init
{
    if (self = [super init]) {
    }
    return self;
}

- (void)mouseDown:(NSEvent*) theEvent
{
    DBG(@"mouseDown\r\n");
    [self pointer_event: theEvent andType:EPX_EVENT_BUTTON_PRESS];
}

- (void)mouseUp:(NSEvent*) theEvent
{
    DBG(@"mouseUp\r\n");
    [self pointer_event: theEvent andType:EPX_EVENT_BUTTON_RELEASE];
}

- (void)mouseDragged:(NSEvent*) theEvent
{
    DBG(@"mouseDragged\r\n");
    [self pointer_event: theEvent andType:EPX_EVENT_POINTER_MOTION];
}

- (void)rightMouseDown:(NSEvent*) theEvent
{
    [self pointer_event: theEvent andType:EPX_EVENT_POINTER_MOTION];
}

- (void)rightMouseUp:(NSEvent*) theEvent
{
    [self pointer_event: theEvent andType:EPX_EVENT_BUTTON_RELEASE];
}

- (void)rightMouseDragged:(NSEvent*) theEvent
{
    [self pointer_event: theEvent andType:EPX_EVENT_POINTER_MOTION];
}

- (void)keyDown:(NSEvent*) theEvent
{
    [self key_event: theEvent andType:EPX_EVENT_KEY_PRESS];
}

- (void)keyUp:(NSEvent*) theEvent
{
    [self key_event: theEvent andType:EPX_EVENT_KEY_RELEASE];
}

- (void)mouseEntered:(NSEvent*) theEvent
{
    [self crossing_event: theEvent andType:EPX_EVENT_ENTER];
}

- (void)mouseExited:(NSEvent*) theEvent
{
    [self crossing_event: theEvent andType:EPX_EVENT_LEAVE];
}

- (void)flagsChanged:(NSEvent*) theEvent
{
    NSEventModifierFlags keyMod = [theEvent modifierFlags];
    epx_event_t e;

    if (keyMod & NSShiftKeyMask)
	e.key.mod |= EPX_KBD_MOD_LSHIFT;
    if (keyMod & NSControlKeyMask)
	e.key.mod |= EPX_KBD_MOD_LCTRL;
    if (keyMod & NSAlternateKeyMask)
	e.key.mod |= EPX_KBD_MOD_LALT;
    if (keyMod & NSAlphaShiftKeyMask)
	e.key.mod |= EPX_KBD_MOD_CAPS;
    if (keyMod & NSNumericPadKeyMask)
	e.key.mod |= EPX_KBD_MOD_NUM;
    if (keyMod & NSCommandKeyMask)
	e.key.mod |= EPX_KBD_MOD_META;
    // Fixme add FunctionKey to epx: NSFunctionKeyMask
    // DBG(@"flagsChanged: %d\r", e.key.mod);
}


- (void) crossing_event:(NSEvent*) theEvent andType:(int)type
{
    epx_window_t* ewin;
    CocoaBackend* be;
    CocoaWindow* cwin;
    epx_event_t e;
    AppWindow* appwin = (AppWindow*) [theEvent window];
    NSPoint where;
    NSRect bounds;

    if (!appwin)
	return;
    cwin = [appwin cwindow];  // get window proxy
    if (!cwin)
	return;

    ewin = cwin->ewin;
    be   = ewin ? (CocoaBackend*) (ewin->backend) : 0;
    
    where = theEvent.locationInWindow;
    // convert into 

    e.window = ewin;
    e.type = type;
    e.pointer.button = 0;

    bounds = appwin.contentLayoutRect;

    e.pointer.x = where.x - bounds.origin.x;
    e.pointer.y = (bounds.size.height - where.y) - bounds.origin.y;
    e.pointer.z = 0;

    DBG(@"crossing_event: x=%d, y=%d\r", e.pointer.x, e.pointer.y);

    if (!be)
	return;
    if (FilterEvent(&e)) {
	DBG(@"crossing_event: filtered\r");
	return;
    }
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
}

- (void) pointer_event:(NSEvent*) theEvent andType:(int)type
{
    epx_window_t* ewin;
    CocoaBackend* be;
    CocoaWindow* cwin;
    epx_event_t e;
    AppWindow* appwin = (AppWindow*) [theEvent window];
    NSPoint where;
    NSRect bounds;
    NSEventType nstype;

    if (!appwin)
	return;
    cwin = [appwin cwindow];  // get window proxy
    if (!cwin)
	return;

    ewin = cwin->ewin;
    be   = ewin ? (CocoaBackend*) (ewin->backend) : 0;
    
    where = theEvent.locationInWindow;
    // convert into 

    e.window = ewin;
    e.type = type;
    e.pointer.button = 0;
    nstype = [theEvent type];
    if ((nstype == NSLeftMouseDown) || 
	(nstype == NSLeftMouseUp) ||
	(nstype == NSLeftMouseDragged))
	e.pointer.button |= EPX_BUTTON_LEFT;
    else if ((nstype == NSRightMouseDown) || 
	     (nstype == NSRightMouseUp) ||
	     (nstype == NSRightMouseDragged))
	e.pointer.button |= EPX_BUTTON_RIGHT;
    bounds = appwin.contentLayoutRect;

    e.pointer.x = where.x - bounds.origin.x;
    e.pointer.y = (bounds.size.height - where.y) - bounds.origin.y;
    e.pointer.z = 0;

    DBG(@"pointer_event: x=%d, y=%d, b=%d\r",
	  e.pointer.x, e.pointer.y, e.pointer.button);

    if (!be)
	return;
    if (FilterEvent(&e)) {
	DBG(@"pointer_event: filtered\r");
	return;
    }
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
}

- (void) key_event:(NSEvent*) theEvent andType:(int)type
{
    epx_window_t* ewin;
    CocoaBackend* be;
    CocoaWindow* cwin;
    epx_event_t e;
    AppWindow* appwin = (AppWindow*) [theEvent window];
    NSString* keys;
    unichar ch;
    UInt32 keyCode;
    UInt32 keyMod;

    if (!appwin)
	return;
    cwin = [appwin cwindow];  // get window proxy
    // DBG(@"key_event: win=%p, cwin=%p\r", win, cwin);
    if (!cwin)
	return;

    ewin = cwin->ewin;
    be   = ewin ? (CocoaBackend*) (ewin->backend) : 0;

    keyCode = [theEvent keyCode];
    keyMod  = [theEvent modifierFlags];
    keys    = [theEvent characters];
    if ([keys length] == 0) {
	DBG(@"key_event: empty\r");
	return;
    }
    e.type  = type;
    e.window = ewin;

    e.key.mod = 0;
    if (keyMod & NSShiftKeyMask)
	e.key.mod |= EPX_KBD_MOD_LSHIFT;
    if (keyMod & NSControlKeyMask)
	e.key.mod |= EPX_KBD_MOD_LCTRL;
    if (keyMod & NSAlternateKeyMask)
	e.key.mod |= EPX_KBD_MOD_LALT;
    if (keyMod & NSAlphaShiftKeyMask)
	e.key.mod |= EPX_KBD_MOD_CAPS;
    if (keyMod & NSNumericPadKeyMask)
	e.key.mod |= EPX_KBD_MOD_NUM;
    if (keyMod & NSCommandKeyMask)
	e.key.mod |= EPX_KBD_MOD_META;

    // keyMod & kEventKeyModifierFnMask
    ch = [keys characterAtIndex:0];
    switch(ch) {
    case '\b': e.key.sym = EPX_KBD_KEY_BACKSPACE; break;
    case '\t': e.key.sym = EPX_KBD_KEY_TAB; break;
    case '\r': e.key.sym = EPX_KBD_KEY_ENTER; break;
    case NSLeftArrowFunctionKey: e.key.sym = EPX_KBD_KEY_LEFT; break;
    case NSRightArrowFunctionKey: e.key.sym = EPX_KBD_KEY_RIGHT; break;
    case NSUpArrowFunctionKey: e.key.sym = EPX_KBD_KEY_UP; break;
    case NSDownArrowFunctionKey: e.key.sym = EPX_KBD_KEY_DOWN; break;
    case NSPrintFunctionKey: e.key.sym = EPX_KBD_KEY_PRINT; break;
    case NSSysReqFunctionKey: e.key.sym = EPX_KBD_KEY_SYSREQ; break;
    case NSPauseFunctionKey: e.key.sym = EPX_KBD_KEY_PAUSE; break;
    case NSBreakFunctionKey: e.key.sym = EPX_KBD_KEY_BREAK; break;
    case NSInsertFunctionKey: e.key.sym = EPX_KBD_KEY_INSERT; break;
    case NSDeleteFunctionKey: e.key.sym = EPX_KBD_KEY_DELETE; break;
    case NSHomeFunctionKey: e.key.sym = EPX_KBD_KEY_HOME; break;
    case NSEndFunctionKey: e.key.sym = EPX_KBD_KEY_END; break;
    case NSPageUpFunctionKey: e.key.sym = EPX_KBD_KEY_PAGEUP; break;
    case NSPageDownFunctionKey: e.key.sym = EPX_KBD_KEY_PAGEDOWN; break;
	// NSBeginFunctionKey:
    case NSScrollLockFunctionKey: e.key.sym = EPX_KBD_KEY_SCROLLOCK; break;
	// NSResetFunctionKey:
	// NSStopFunctionKey:
    case NSMenuFunctionKey: e.key.sym = EPX_KBD_KEY_MENU; break;
	// NSUserFunctionKey:
	// NSSystemFunctionKey:
	// NSClearLineFunctionKey:
	// NSClearDisplayFunctionKey:
	// NSInsertLineFunctionKey:
	// NSDeleteLineFunctionKey:
	// NSInsertCharFunctionKey:
	// NSDeleteCharFunctionKey:
	// NSPrevFunctionKey:
	// NSNextFunctionKey:
	// NSSelectFunctionKey:
	// NSExecuteFunctionKey:
	// NSUndoFunctionKey:
	// NSRedoFunctionKey:
	// NSFindFunctionKey:
	// NSHelpFunctionKey:
	// NSModeSwitchFunctionKey:
    case NSF1FunctionKey: e.key.sym = EPX_KBD_KEY_F1; break;
    case NSF2FunctionKey: e.key.sym = EPX_KBD_KEY_F2; break;
    case NSF3FunctionKey: e.key.sym = EPX_KBD_KEY_F3; break;
    case NSF4FunctionKey: e.key.sym = EPX_KBD_KEY_F4; break;
    case NSF5FunctionKey: e.key.sym = EPX_KBD_KEY_F5; break;
    case NSF6FunctionKey: e.key.sym = EPX_KBD_KEY_F6; break;
    case NSF7FunctionKey: e.key.sym = EPX_KBD_KEY_F7; break;
    case NSF8FunctionKey: e.key.sym = EPX_KBD_KEY_F8; break;
    case NSF9FunctionKey: e.key.sym = EPX_KBD_KEY_F9; break;
    case NSF10FunctionKey: e.key.sym = EPX_KBD_KEY_F10; break;
    case NSF11FunctionKey: e.key.sym = EPX_KBD_KEY_F11; break;
    case NSF12FunctionKey: e.key.sym = EPX_KBD_KEY_F12; break;
	// case NSF13FunctionKey:
	// case NSF14FunctionKey:
	// case NSF15FunctionKey:
	// case NSF16FunctionKey:
	// case NSF17FunctionKey:
	// case NSF18FunctionKey:
	// case NSF19FunctionKey:
	// case NSF20FunctionKey:
	// case NSF21FunctionKey:
	// case NSF22FunctionKey:
	// case NSF23FunctionKey:
	// case NSF24FunctionKey:
	// case NSF25FunctionKey:
	// case NSF26FunctionKey:
	// case NSF27FunctionKey:
	// case NSF28FunctionKey:
        // case NSF29FunctionKey:
	// case NSF30FunctionKey:
	// case NSF31FunctionKey:
	// case NSF32FunctionKey:
	// case NSF33FunctionKey:
	// case NSF34FunctionKey:
	// case NSF35FunctionKey:
    default:
	e.key.sym = (ch & 0x7f);
	break;
    }
    e.key.code = keyCode;
    
//    DBG(@"key_event: keyMod=%x, keyCode=%x, KeySym=%x\r",
//	  e.key.mod, e.key.code, e.key.sym);

    if (!be)
	return;
    if (FilterEvent(&e)) {
	DBG(@"key_event: filtered\r");
	return;
    }
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
}




@end

//
// Wrapper to store cwin somewhere
//

@implementation AppWindow
@synthesize cwindow;

- (void)windowDidResize:(NSNotification *)notification
{
    (void)notification;
    epx_window_t* ewin;
    CocoaBackend* be;
    CocoaWindow* cwin;
    epx_event_t e;
    AppWindow* appwin = (AppWindow*) self;
    NSRect bounds;

    if (!appwin)
	return;
    cwin = [appwin cwindow];  // get window proxy
    if (!cwin)
	return;

    ewin = cwin->ewin;
    be   = ewin ? (CocoaBackend*) (ewin->backend) : 0;
    
    e.window = ewin;
    e.type = EPX_EVENT_RESIZE;
    e.pointer.button = 0;
    bounds = appwin.contentLayoutRect;

    e.dimension.w = bounds.size.width;
    e.dimension.h = bounds.size.height;
    e.dimension.d = 0;

    DBG(@"windowDidReszie: w=%d, h=%d, d=%d\r",
	  e.dimension.w, e.dimension.h, e.dimension.d);

    if (!be)
	return;
    if (FilterEvent(&e)) {
	DBG(@"pointer_event: filtered\r");
	return;
    }
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));    
}

- (void)windowDidMove:(NSNotification *)notification
{
    (void)notification;
    epx_window_t* ewin;
    CocoaBackend* be;
    epx_event_t e;
    NSRect bounds;

    if (!cwindow)
	return;

    ewin = cwindow->ewin;
    be   = ewin ? (CocoaBackend*) (ewin->backend) : 0;
    
    e.window = ewin;
    e.type = EPX_EVENT_CONFIGURE;
    e.pointer.button = 0;
    bounds = self.frame;

    e.area.x = bounds.origin.x;
    e.area.y = bounds.origin.y;
    e.area.w = bounds.size.width;
    e.area.h = bounds.size.height;

    DBG(@"windowDidMove: x=%d, y=%d, w=%d, h=%d\r",
	  e.area.x, e.area.y, e.area.w, e.area.h);

    if (!be)
	return;
    if (FilterEvent(&e)) {
	DBG(@"windowDidMove: filtered\r");
	return;
    }
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));    
}

- (BOOL)windowShouldClose:(id)sender
{
    (void) sender;
    epx_window_t* ewin;
    CocoaBackend* be;
    epx_event_t e;

    if (!cwindow) return FALSE;

    ewin = cwindow->ewin;
    be   = ewin ? (CocoaBackend*) (ewin->backend) : 0;
    
    e.window = ewin;
    e.type = EPX_EVENT_CLOSE;

    if (!be)
	return FALSE;
    if (FilterEvent(&e)) {
	DBG(@"pointer_event: filtered\r");
	return FALSE;
    }
    be->b.pending++;
    write(be->ofd,(void*) &e, sizeof(e));
    return FALSE;
}



@end

@implementation AppApplication
@synthesize backend;

-(void)sendEvent:(NSEvent *)theEvent
{
    if ([theEvent type] == NSApplicationDefined) {
	if ([theEvent data1] == kEpxEventNew)
	    [self create_window: theEvent];
	else if ([theEvent data1] == kEpxEventDel)
	    [self close_window: theEvent];
    }
    else {
	[super sendEvent:theEvent];
    }
} 

- (void)create_window:(NSEvent*) theEvent
{
    CocoaWindow* cwin = (CocoaWindow*) [theEvent data2];
    AppWindow* appwin;
    NSRect viewRect = NSMakeRect(0,0,cwin->winBounds.size.width,
				 cwin->winBounds.size.height);
    WinView* view = [[WinView alloc] initWithFrame:viewRect];
    CocoaBackend* be;
    epx_window_t* ewin;
    NSTrackingArea *area = [[NSTrackingArea alloc]initWithRect:viewRect 
			    options:NSTrackingMouseEnteredAndExited |
			    NSTrackingActiveInActiveApp
			    // NSTrackingMouseMoved
			    owner:view userInfo:nil];
    // NSGraphicsContext* ctx;

    ewin = cwin->ewin;
    be   = ewin ? (CocoaBackend*) (ewin->backend) : 0;
    [view setBackend:be];
    
    EPX_DBGFMT("create_window\n");
    
    appwin = [[AppWindow alloc]
	      initWithContentRect:cwin->winBounds
	      styleMask:cwin->winAttrs
	      backing:NSBackingStoreBuffered
	      defer:NO];
    cwin->winNS = appwin;

    // ctx = [cwin->winNS graphicsContext];
    // cwin->ctx = [ctx graphicsPort];
    // CGContextTranslateCTM(cwin->ctx, 0, cwin->winBounds.size.height);
    // CGContextScaleCTM(cwin->ctx, 1.0, -1.0);

    if (!appwin) {
	EPX_DBGFMT("EPxAppEventHandler: could not create window\n");
	cwin->winErr = paramErr;  // error code?
	cwin->winIsSetup = 1;
	return;
    }

    appwin.cwindow = cwin;   // circular ref!

    [[appwin contentView] addSubview:view];  // for mouse events
    [view addTrackingArea:area];  // for Enter/Exit callbacks
    [appwin setDelegate:appwin];  // Resize and Moved callbacks

    [[appwin contentView] setAutoresizesSubviews:YES];

    [appwin makeFirstResponder: view];
    // [appwin makeFirstResponder: responder];
    [appwin setTitle: @"EPX"];
    [appwin makeKeyAndOrderFront:self];
    [appwin setBackgroundColor:[NSColor blueColor]];
    [NSApp activateIgnoringOtherApps:YES];
    [appwin display];
    // fixme: callback when window event flags are changed!!!
    // [appwin setAcceptsMouseMovedEvents: YES];
    // [cwin->winNS makeKeyAndOrderFront:NSApp];

    cwin->ewin->opengl = 0;
    if (self.backend->b.opengl && self.backend->b.use_opengl) {
#ifdef HAVE_OPENGL
	if ((cocoa_gl_setup(cwin) == noErr) && cwin->aglContext)
	    cwin->ewin->opengl = 1;
#endif
    }
    cwin->winErr = noErr;
    cwin->winIsSetup = 1;
}

- (void)close_window:(NSEvent*) theEvent
{
    CocoaWindow* cwin = (CocoaWindow*) [theEvent data2];

    DBG(@"close_window\r\n");

#ifdef HAVE_OPENGL
    if (cwin->aglContext)
	cocoa_gl_cleanup(cwin);
#endif
    [cwin->winNS close];
    free(cwin);
}

@end


@interface AppDelegate : NSObject <NSApplicationDelegate> {
CocoaBackend* backend;
}
@property CocoaBackend* backend;
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification;
@end

@implementation AppDelegate
@synthesize backend;
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    (void) aNotification;
    DBG(@"applicationDidFinishLaunching\r\n");
}
@end


void* cocoa_event_thread(void* arg)
{
    CocoaBackend* be = (CocoaBackend*) arg;
    AppApplication* application;
    // OSStatus err;
    // ProcessSerialNumber psn;

    EPX_DBGFMT("cocoa_event_thread: running ofd=%d", be->ofd);

    [CocoaMultithreading beginMultithreading];

    [NSAutoreleasePool new];
    application = (AppApplication*) [AppApplication sharedApplication];
    [application setBackend:be];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

    be->menu = [[NSMenu new] autorelease];
    id menubar = be->menu;
    id appMenuItem = [[NSMenuItem new] autorelease];

    [menubar addItem:appMenuItem];
    [NSApp setMainMenu:menubar];

    id appMenu = [[NSMenu new] autorelease];
    id appName = [[NSProcessInfo processInfo] processName];
    id quitTitle = [@"Quit " stringByAppendingString:appName];

    id quitMenuItem = [[[NSMenuItem alloc] initWithTitle:quitTitle
        action:@selector(terminate:) keyEquivalent:@"q"] autorelease];
    [appMenu addItem:quitMenuItem];
    [appMenuItem setSubmenu:appMenu];

    [NSApp activateIgnoringOtherApps:YES];
    AppDelegate *delegate = [[AppDelegate alloc] init];
    [application setDelegate:delegate];
    [delegate autorelease];
    [delegate setBackend: be];

    pthread_mutex_unlock(&cocoa_lock);

    [NSApp run];

    close(be->ofd);  // signal termination!
    return NULL;
}


epx_backend_t* cocoa_init(epx_dict_t* param)
{
#pragma unused(param)
    CocoaBackend* be;
    int int_param;
    int cocoa_pipe[2];
    NSScreen* screen;

    EPX_DBGFMT("cocoa_init");

    if ((be = (CocoaBackend*) malloc(sizeof(CocoaBackend))) == NULL)
	return NULL;
    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);

    be->b.name = "macos";
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.cb = &cocoa_callbacks;
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;
    be->b.width = 0;
    be->b.height = 0;
    epx_object_list_init(&be->b.pixmap_list);
    epx_object_list_init(&be->b.window_list);
    be->b.nformats = 0;
    be->b.event = EPX_INVALID_HANDLE;

    if (epx_dict_lookup_integer(param, "use_opengl", &int_param) != -1)
	be->b.use_opengl = int_param;
#ifdef HAVE_OPENGL
    be->b.opengl = 1;
#endif
    screen = [NSScreen deepestScreen];

    be->b.width  = screen.frame.size.width;
    be->b.height = screen.frame.size.height;

    pipe(cocoa_pipe);
    be->b.event = (EPX_HANDLE_T) (long) cocoa_pipe[0];
    be->ofd     = cocoa_pipe[1];
    pthread_mutex_init(&cocoa_lock, NULL);
    pthread_mutex_lock(&cocoa_lock);  // Lock until thread is initialised

    if (param->opaque != NULL) {
	int (*thread_create)(pthread_t*, pthread_attr_t* attr,
			     void* (*func)(void* arg), void* arg);
	thread_create = param->opaque;
	(*thread_create)(&cocoa_thr, NULL, cocoa_event_thread, (void*) be);
    }
    else {
	pthread_create(&cocoa_thr, NULL, cocoa_event_thread,
		       (void*) be);
    }
    pthread_mutex_lock(&cocoa_lock);    // Wait for init to complete
    pthread_mutex_unlock(&cocoa_lock);  // Wait for init to complete

    return (epx_backend_t*) &(be->b);
}

int cocoa_upgrade(epx_backend_t* backend)
{
    backend->cb = &cocoa_callbacks;
    return 0;
}


/* return the backend event handle */
static EPX_HANDLE_T cocoa_evt_attach(epx_backend_t* backend)
{
    CocoaBackend* be = (CocoaBackend*) backend;

    EPX_DBGFMT("cocoa_evt_attach");

    return (EPX_HANDLE_T) be->b.event;
}

static int cocoa_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

static int cocoa_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    int r = 0;
    
    if ((r=backend->pending) == 0) // LOCK atomic check
	return 0;
    if (read((int)backend->event, (void*) e, sizeof(epx_event_t)) < 0)
	return -1;
    backend->pending--;  // LOCK atomic! check
    return r;
}

static int cocoa_finish(epx_backend_t* backend)
{
    CocoaBackend* be = (CocoaBackend*) backend;

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

static int cocoa_pix_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    CocoaBackend* be = (CocoaBackend*) backend;
    CocoaPixels* pe;

    if (pixmap->opaque != NULL)
	return -1;
    if ((pe = (CocoaPixels*) malloc(sizeof(CocoaPixels))) == NULL)
	return -1;

    pe->colorspace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    pe->bitmapinfo = 0;

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
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst; /* ARGB format */
	break;
    case EPX_FORMAT_BGR: /* ??? */
	pe->bitmapinfo |= kCGImageAlphaNone;         /* BGR format */
	break;
    case EPX_FORMAT_BGRA: /* ARGB */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst;
	break;
    case EPX_FORMAT_ABGR: /* RGBA */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipLast;
	break;
    default:
	fprintf(stderr, "macos: unhandled pixelType = %d\n", pixmap->pixel_format);
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

						
static int cocoa_pix_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    CocoaPixels* pe = (CocoaPixels*) pixmap->opaque;

    if (pe != NULL) {
	CGColorSpaceRelease(pe->colorspace);
	CGDataProviderRelease(pe->provider);
	epx_object_unlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}

/* There must be a better way */
static void draw_image(CocoaWindow* cwin, CGRect srcRect, CGRect dstRect,
		       epx_pixmap_t* pic)
{
    CocoaPixels* pe = (CocoaPixels*) pic->opaque;
    CGImageRef img;
    unsigned int bytesPerRow   = pic->bytes_per_row;
    unsigned int bitsPerPixel  = pic->bits_per_pixel;
    CGContextRef ctx = cwin->ctx;

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
	fprintf(stderr, "epix_cocoa: error CGImageCreate\n");
	return;
    }

    if ((srcRect.origin.x == (CGFloat)0) && 
	(srcRect.origin.y == (CGFloat)0) &&
	(srcRect.size.width == (CGFloat) pic->width) && 
	(srcRect.size.height == (CGFloat) pic->height)) {

	CGContextDrawImage(ctx, dstRect, img);
    }
    else { // Sub image
	CGImageRef srcImg;
	srcImg = CGImageCreateWithImageInRect(img, srcRect);

	CGContextSaveGState(ctx);
	// CGContextTranslateCTM(ctx, 0, srcRect.size.height);
	// CGContextScaleCTM(ctx, 1.0, -1.0);
	CGContextDrawImage(ctx, dstRect, srcImg);
	CGContextRestoreGState(ctx);
	CGImageRelease(srcImg);
    }
    CGImageRelease(img);
}

static int cocoa_begin(epx_window_t* ewin)
{
    CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;
    NSGraphicsContext* ctx;

    if (cwin == NULL)
	return -1;

    ctx = [cwin->winNS graphicsContext];
    cwin->ctx = [ctx graphicsPort];

    pthread_mutex_lock(&cocoa_lock);

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	cwin->saveContext = aglGetCurrentContext();
	aglSetCurrentContext(cwin->aglContext);
	aglUpdateContext(cwin->aglContext);
#endif
    }
    else {
	[NSGraphicsContext saveGraphicsState];
	[NSGraphicsContext setCurrentContext:ctx];
    }
    return 0;
}

static int cocoa_end(epx_window_t* ewin, int off_screen)
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
	[[cwin->winNS graphicsContext] flushGraphics];
	[NSGraphicsContext restoreGraphicsState];
    }
    pthread_mutex_unlock(&cocoa_lock);

    return 0;
}



static int cocoa_pix_draw(epx_backend_t* backend, 
			  epx_pixmap_t* pic, epx_window_t* ewin,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height)
{
    (void) backend;
    CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;

    DBG(@"cocoa_pix_draw: cwin=%p\r\n", cwin);

    if (cwin == NULL) 
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
	dstRect = CGRectMake((float)dst_x, (float)dst_y,   // +21?
			     (float)width, (float)height);
	draw_image(cwin, srcRect, dstRect, pic);
    }
    return 0;
}

static int cocoa_win_swap(epx_backend_t* backend, epx_window_t* ewin)
{
    (void) backend;

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;
	AGLContext save;
	if (!cwin || !cwin->aglContext)
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

static int cocoa_win_attach(epx_backend_t* backend, epx_window_t* ewin)
{
    CocoaBackend* be = (CocoaBackend*) backend;
    CocoaWindow* cwin;

    if (ewin->opaque != NULL)
	return -1;

    if ((cwin = (CocoaWindow*) malloc(sizeof(CocoaWindow))) == NULL) 
	return -1;
    memset(cwin, 0, sizeof(CocoaWindow));
    cwin->ewin   = ewin;
    cwin->winErr = noErr;
    cwin->winIsSetup = 0;
    ewin->opaque = (void*) cwin;

    epx_object_link(&backend->window_list, ewin);
    ewin->backend = (epx_backend_t*) be;

    cwin->winAttrs = 
	NSBorderlessWindowMask |
	NSTitledWindowMask |
	NSClosableWindowMask |
	NSMiniaturizableWindowMask |
	NSResizableWindowMask;
	// NSTexturedBackgroundWindowMask	

    cwin->winBounds.origin.x = ewin->x;
    cwin->winBounds.origin.y = ewin->y;
    cwin->winBounds.size.height = ewin->height;
    cwin->winBounds.size.width  = ewin->width;

    DBG(@"cocoa_win_attach: cwin=%p\r\n", cwin);
    // Relase pool

    NSEvent* event = [NSEvent otherEventWithType: NSApplicationDefined
		      location: NSMakePoint(0,0)
		      modifierFlags: 0
		      timestamp: 0.0
		      windowNumber: 0
		      context: nil
		      subtype: 0
		      data1: kEpxEventNew
		      data2: (NSInteger) cwin
	];

    [NSApp sendEvent: event];  //  atStart: YES

    DBG(@"cocoa_win_attach: post done\r\n");

    while(cwin->winIsSetup == 0)
	usleep(1000);
    DBG(@"cocoa_win_attach: is setup\r\n");
    if (cwin->winErr != noErr) {
	free(cwin);
	return -1;
    }
    return 0;
}

static int cocoa_win_detach(epx_backend_t* backend, epx_window_t* ewin)
{
    CocoaWindow* cwin = (CocoaWindow*) ewin->opaque;

    DBG(@"cocoa_win_detach: cwin=%p\r\n", cwin);

    if (cwin != NULL) {
	epx_object_unlink(&backend->window_list, ewin);
	ewin->opaque = NULL;
	ewin->backend = NULL;

	// Relase pool
	NSEvent* event = [NSEvent otherEventWithType: NSApplicationDefined
			  location: NSMakePoint(0,0)
			  modifierFlags: 0
			  timestamp: 0.0
			  windowNumber: 0
			  context: nil
			  subtype: 0
			  data1: kEpxEventDel
			  data2: (NSInteger) cwin
	    ];
	[NSApp sendEvent: event];  //  atStart: YES

	DBG(@"cocoa_win_detach: post done\r\n");
	// ReleaseEvent(delEvent);
    }
    return 0;
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

static int cocoa_adjust(epx_backend_t *backend, epx_dict_t* param)
{
    (void) backend;
    (void) param;
    DBG(@"cocoa: Backend adjust\n");
    return 1;
}

static int cocoa_win_adjust(epx_window_t *win, epx_dict_t* param)
{
    int width, wi;
    int height, hi;
    int bool_val;
    CocoaWindow* cwin = (CocoaWindow*)win->opaque;

    DBG(@"cocoa: Window adjust\n");
    wi=epx_dict_lookup_integer(param, "width", &width);
    hi=epx_dict_lookup_integer(param, "height", &height);

    DBG(@"cocoa:%d width=%d\n", wi, width);
    DBG(@"cocoa:%d height=%d\n", hi, height);

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
	[cwin->winNS setMinSize:cwin->winBounds.size];
    }
    
    if (epx_dict_lookup_boolean(param, "show", &bool_val) >= 0) {
	if (bool_val) {
	    if (![cwin->winNS isVisible])
		[cwin->winNS orderOut:(cwin->winNS)];
	}
	else {
	    if ([cwin->winNS isVisible]) {
		[cwin->winNS makeKeyAndOrderFront:(cwin->winNS)];
		[NSApp activateIgnoringOtherApps:YES];
	    }
	}
    }

    if (epx_dict_lookup_boolean(param, "select", &bool_val) >= 0) {
	if (bool_val) {
	    [cwin->winNS makeKeyAndOrderFront:(cwin->winNS)];
	}
    }
    //  "focus", "modal" ...
    return 1;
}

static int cocoa_info(epx_backend_t *backend, epx_dict_t* param)
{
    (void) backend;
    (void) param;
    EPX_DBGFMT("cocoa: info");
    return 0;
}

