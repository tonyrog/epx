/***************************************************************************
 *
 * Copyright (C) 2007 - 2026, Rogvall Invest AB, <tony@rogvall.se>
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
 *  Win32 (plain GDI) display driver
 *
 *  ********************************************************************
 *  *  UNTESTED FIRST DRAFT.  This file has never been compiled with a  *
 *  *  Windows compiler nor run on Windows. It was written against the *
 *  *  documented Win32 API and only syntax checked on Linux with stub *
 *  *  headers. Expect compile errors and bugs. See doc/WINDOWS.md.    *
 *  ********************************************************************
 *
 *  Design
 *  ------
 *  - A dedicated GUI thread is started by win32_init. It owns ALL HWNDs
 *    (Win32 windows are bound to the creating thread, only that thread
 *    receives their messages and may DestroyWindow them). The GUI thread
 *    runs a plain GetMessage/DispatchMessage loop.
 *
 *  - The epx backend callbacks are called from the NIF backend_main
 *    thread. Operations that must run on the GUI thread (create, destroy,
 *    adjust, sync) are marshalled with SendMessage to a hidden
 *    message-only window (HWND_MESSAGE) owned by the GUI thread.
 *    SendMessage is synchronous, so the backend_main thread gets the
 *    result back. Never hold a backend/window lock while calling
 *    SendMessage (dead lock).
 *
 *  - Each window has an off-screen back buffer: a 32 bit top-down DIB
 *    section (memory layout B,G,R,X == EPX_FORMAT_BGRA) selected into a
 *    memory DC. pix_draw (backend_main thread) only converts/copies
 *    pixels into the DIB memory with epx_copy_area (no GDI calls), under a
 *    per window CRITICAL_SECTION. win_swap posts WM_EPX_FLUSH to the
 *    window and the GUI thread BitBlt's the dirty part to the screen.
 *    WM_PAINT (system expose) is served from the back buffer and also
 *    reported as EPX_EVENT_EXPOSE (as x11 does with backing store).
 *
 *  - Events are translated in the window procedure (GUI thread) and put
 *    in a ring buffer protected by a CRITICAL_SECTION. A manual-reset
 *    Win32 event object is signalled while the ring is non-empty. That
 *    HANDLE is what evt_attach returns (instead of a unix fd). The NIF
 *    backend_poll thread must wait on it with WaitForMultipleObjects
 *    instead of poll() - see doc/WINDOWS.md.
 *
 *  - evt_read pops from the ring on the backend_main thread, applies the
 *    window event mask filter and returns 1+pending as the x11 backend.
 *
 *  Not done: OpenGL (WGL), grab/grab_key, cursors, DPI scaling (only an
 *  opt-in SetProcessDPIAware), NIF upgrade of a running backend (the GUI
 *  thread and the registered window procedure live in the old DLL).
 */

#include <string.h>
#include <stdlib.h>

/* Include epx headers BEFORE windows.h, so that windows macros
 * (RGB, OPAQUE, TRANSPARENT, min, max ...) do not clobber epx headers */
#include "../include/epx_backend.h"

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601      /* Windows 7 */
#endif
#ifndef WINVER
#define WINVER _WIN32_WINNT
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <windowsx.h>    /* GET_X_LPARAM / GET_Y_LPARAM */
#include <process.h>     /* _beginthreadex */

#ifndef WM_MOUSEHWHEEL
#define WM_MOUSEHWHEEL 0x020E
#endif
#ifndef MAPVK_VSC_TO_VK_EX
#define MAPVK_VSC_TO_VK_EX 3
#endif
#ifndef WHEEL_DELTA
#define WHEEL_DELTA 120
#endif

#define EPX_WIN_CLASS    L"epx_window"
#define EPX_MSG_CLASS    L"epx_message"

/* private messages, sent to the message-only window */
#define WM_EPX_CREATE   (WM_APP + 1)   /* lParam = Win32Window*  */
#define WM_EPX_DESTROY  (WM_APP + 2)   /* lParam = Win32Window*  */
#define WM_EPX_ADJUST   (WM_APP + 3)   /* lParam = adjust_req_t* */
#define WM_EPX_SYNC     (WM_APP + 4)   /* lParam = Win32Window*  */
#define WM_EPX_QUIT     (WM_APP + 5)
/* private message, posted to an epx window */
#define WM_EPX_FLUSH    (WM_APP + 6)

#define EVENT_QUEUE_SIZE 1024          /* must fit in unsigned */
#define BACK_BUFFER_FORMAT EPX_FORMAT_BGRA  /* == 32 bit BI_RGB DIB layout */

struct _Win32Backend;

/* This structure is stored in epx_window_t opaque field */
typedef struct {
    struct _Win32Backend* be;
    epx_window_t* ewin;
    HWND     hwnd;
    DWORD    style;
    DWORD    ex_style;
    /* back buffer, protected by lock */
    CRITICAL_SECTION lock;
    HDC      mem_dc;
    HBITMAP  dib;
    HBITMAP  old_bm;
    uint8_t* bits;
    int      bm_width;
    int      bm_height;
    int      bm_bytes_per_row;
    int      dirty;              /* dirty rectangle is valid */
    int      dx0, dy0, dx1, dy1; /* dirty rect [dx0,dx1) x [dy0,dy1) */
    volatile LONG flush_posted;  /* WM_EPX_FLUSH in the queue */
    /* GUI thread state */
    int      width;              /* client area size */
    int      height;
    int      min_w, min_h;       /* client size limits, -1 = not set */
    int      max_w, max_h;
    int      mouse_inside;
    int      wheel_acc;
    int      hwheel_acc;
    int      configured;         /* configure event reported at least once */
    uint16_t key_sym[512];       /* sym reported at key press, by code */
} Win32Window;

typedef struct _Win32Backend {
    epx_backend_t b;                /* DO NOT MOVE !!! */
    HINSTANCE hinst;
    HANDLE    gui_thread;
    unsigned  gui_tid;
    HWND      msg_hwnd;             /* message only window (GUI thread) */
    HANDLE    ready;                /* GUI thread init done */
    int       init_ok;
    /* event queue GUI thread -> backend_main thread */
    CRITICAL_SECTION qlock;
    HANDLE    qevent;               /* manual reset, set while qlen > 0 */
    unsigned  qhead;
    unsigned  qlen;
    unsigned  qdropped;
    epx_event_t q[EVENT_QUEUE_SIZE];
} Win32Backend;

typedef struct {
    Win32Window* ww;
    epx_dict_t*  param;
} adjust_req_t;

epx_backend_t* win32_init(epx_dict_t* param);
int win32_upgrade(epx_backend_t* be);

static int win32_finish(epx_backend_t*);
static int win32_pix_attach(epx_backend_t*, epx_pixmap_t*);
static int win32_pix_detach(epx_backend_t*, epx_pixmap_t*);
static int win32_begin(epx_window_t*);
static int win32_end(epx_window_t*, int);
static int win32_pix_draw(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
			  int src_x, int src_y, int dst_x, int dst_y,
			  unsigned int width,
			  unsigned int height);
static int win32_pix_sync(epx_backend_t*, epx_pixmap_t*, epx_window_t*);
static int win32_win_attach(epx_backend_t*, epx_window_t*);
static int win32_win_detach(epx_backend_t*, epx_window_t*);
static int win32_win_swap(epx_backend_t*, epx_window_t*);
static EPX_HANDLE_T win32_evt_attach(epx_backend_t*);
static int win32_evt_detach(epx_backend_t*);
static int win32_evt_read(epx_backend_t*, epx_event_t*);
static int win32_adjust(epx_backend_t *backend, epx_dict_t* param);
static int win32_win_adjust(epx_window_t *window, epx_dict_t* param);
static int win32_info(epx_backend_t *backend, epx_dict_t* param);
static int win32_win_info(epx_window_t *window, epx_dict_t* param);

static epx_callbacks_t win32_callbacks =
{
    .finish       = win32_finish,
    .pix_attach   = win32_pix_attach,
    .pix_detach   = win32_pix_detach,
    .pix_draw     = win32_pix_draw,
    .pix_sync     = win32_pix_sync,
    .win_attach   = win32_win_attach,
    .win_detach   = win32_win_detach,
    .evt_attach   = win32_evt_attach,
    .evt_detach   = win32_evt_detach,
    .evt_read     = win32_evt_read,
    .adjust       = win32_adjust,
    .win_swap     = win32_win_swap,
    .begin        = win32_begin,
    .end          = win32_end,
    .win_adjust   = win32_win_adjust,
    .info         = win32_info,
    .win_info     = win32_win_info,
};

/* window classes are process global, count users */
static volatile LONG class_refc = 0;

static LRESULT CALLBACK epx_wndproc(HWND, UINT, WPARAM, LPARAM);
static LRESULT CALLBACK msg_wndproc(HWND, UINT, WPARAM, LPARAM);

/******************************************************************************
 *  Utilities
 *****************************************************************************/

/* Convert a dict string (UTF-8, or Latin-1 if not valid UTF-8) to UTF-16 */
static WCHAR* to_wide(const char* s, size_t len)
{
    WCHAR* w;
    int n;

    if (len > 0x7fffffff)
	return NULL;
    n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
			    s, (int) len, NULL, 0);
    if (n > 0) {
	if ((w = (WCHAR*) malloc((n+1)*sizeof(WCHAR))) == NULL)
	    return NULL;
	MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, s, (int) len, w, n);
	w[n] = 0;
    }
    else {  /* empty or not UTF-8: treat as Latin-1 */
	size_t i;
	if ((w = (WCHAR*) malloc((len+1)*sizeof(WCHAR))) == NULL)
	    return NULL;
	for (i = 0; i < len; i++)
	    w[i] = (WCHAR) (unsigned char) s[i];
	w[len] = 0;
    }
    return w;
}

/* Convert UTF-16 to a malloc'ed UTF-8 string */
static char* from_wide(const WCHAR* w)
{
    int n = WideCharToMultiByte(CP_UTF8, 0, w, -1, NULL, 0, NULL, NULL);
    char* s;
    if (n <= 0)
	return NULL;
    if ((s = (char*) malloc(n)) == NULL)
	return NULL;
    WideCharToMultiByte(CP_UTF8, 0, w, -1, s, n, NULL, NULL);
    return s;
}

/* Size of window frame (non client area) for a given style */
static void frame_size(DWORD style, DWORD ex_style, int* fw, int* fh)
{
    RECT r = { 0, 0, 100, 100 };
    AdjustWindowRectEx(&r, style, FALSE, ex_style);
    *fw = (r.right - r.left) - 100;
    *fh = (r.bottom - r.top) - 100;
}

/******************************************************************************
 *  Event queue (GUI thread produce, backend_main thread consume)
 *****************************************************************************/

static void push_event(Win32Backend* be, epx_event_t* e)
{
    EnterCriticalSection(&be->qlock);
    if (be->qlen > 0) {
	/* compress consecutive motion events */
	unsigned last = (be->qhead + be->qlen - 1) % EVENT_QUEUE_SIZE;
	epx_event_t* le = &be->q[last];
	if ((e->type == EPX_EVENT_POINTER_MOTION) &&
	    (le->type == EPX_EVENT_POINTER_MOTION) &&
	    (le->window == e->window) &&
	    (le->pointer.button == e->pointer.button)) {
	    *le = *e;
	    LeaveCriticalSection(&be->qlock);
	    return;
	}
    }
    if (be->qlen >= EVENT_QUEUE_SIZE) {
	be->qdropped++;  /* reader is not keeping up, drop */
    }
    else {
	unsigned pos = (be->qhead + be->qlen) % EVENT_QUEUE_SIZE;
	be->q[pos] = *e;
	be->qlen++;
	SetEvent(be->qevent);
    }
    LeaveCriticalSection(&be->qlock);
}

/* remove all queued events for a window (called before it is freed) */
static void purge_events(Win32Backend* be, epx_window_t* window)
{
    unsigned i, n = 0;
    EnterCriticalSection(&be->qlock);
    for (i = 0; i < be->qlen; i++) {
	epx_event_t* e = &be->q[(be->qhead + i) % EVENT_QUEUE_SIZE];
	if (e->window != window) {
	    if (n != i)
		be->q[(be->qhead + n) % EVENT_QUEUE_SIZE] = *e;
	    n++;
	}
    }
    be->qlen = n;
    if (n == 0)
	ResetEvent(be->qevent);
    LeaveCriticalSection(&be->qlock);
}

static void push_simple(Win32Window* ww, uint32_t type)
{
    epx_event_t e;
    memset(&e, 0, sizeof(e));
    e.type = type;
    e.window = ww->ewin;
    push_event(ww->be, &e);
}

static void push_pointer(Win32Window* ww, uint32_t type, uint32_t button,
			 int x, int y)
{
    epx_event_t e;
    memset(&e, 0, sizeof(e));
    e.type = type;
    e.window = ww->ewin;
    e.pointer.button = button;
    e.pointer.x = x;
    e.pointer.y = y;
    e.pointer.z = 0;
    push_event(ww->be, &e);
}

static void push_area(Win32Window* ww, uint32_t type,
		      int x, int y, int w, int h)
{
    epx_event_t e;
    memset(&e, 0, sizeof(e));
    e.type = type;
    e.window = ww->ewin;
    e.area.x = x;
    e.area.y = y;
    e.area.w = w;
    e.area.h = h;
    push_event(ww->be, &e);
}

/* Same filter as the x11 backend */
static int FilterEvent(epx_event_t* e)
{
    uint32_t mask;

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

/******************************************************************************
 *  Back buffer (DIB section)
 *****************************************************************************/

static HBITMAP create_dib(int width, int height, uint8_t** bits)
{
    BITMAPINFO bmi;

    memset(&bmi, 0, sizeof(bmi));
    bmi.bmiHeader.biSize        = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth       = width;
    bmi.bmiHeader.biHeight      = -height;   /* negative = top-down */
    bmi.bmiHeader.biPlanes      = 1;
    bmi.bmiHeader.biBitCount    = 32;
    bmi.bmiHeader.biCompression = BI_RGB;    /* B,G,R,X in memory */
    *bits = NULL;
    return CreateDIBSection(NULL, &bmi, DIB_RGB_COLORS, (void**) bits,
			    NULL, 0);
}

static void destroy_back_buffer(Win32Window* ww)
{
    if (ww->mem_dc) {
	SelectObject(ww->mem_dc, ww->old_bm);
	DeleteDC(ww->mem_dc);
    }
    if (ww->dib)
	DeleteObject(ww->dib);
    ww->mem_dc = NULL;
    ww->dib = NULL;
    ww->old_bm = NULL;
    ww->bits = NULL;
    ww->bm_width = ww->bm_height = ww->bm_bytes_per_row = 0;
}

/* GUI thread: make sure back buffer is at least width x height.
 * Never shrinks, grows with 50% extra (like the x11 backend). */
static int resize_back_buffer(Win32Window* ww, int width, int height)
{
    int nw, nh;
    HDC dc;
    HBITMAP dib, old;
    uint8_t* bits;

    if (width < 1) width = 1;
    if (height < 1) height = 1;
    if (ww->mem_dc && (width <= ww->bm_width) && (height <= ww->bm_height))
	return 0;
    nw = (width > ww->bm_width) ? width + width/2 : ww->bm_width;
    nh = (height > ww->bm_height) ? height + height/2 : ww->bm_height;

    if ((dib = create_dib(nw, nh, &bits)) == NULL)
	return -1;
    if ((dc = CreateCompatibleDC(NULL)) == NULL) {
	DeleteObject(dib);
	return -1;
    }
    old = (HBITMAP) SelectObject(dc, dib);
    memset(bits, 0xff, (size_t) nw * 4 * (size_t) nh);  /* white */

    EnterCriticalSection(&ww->lock);
    if (ww->mem_dc) {
	BitBlt(dc, 0, 0, ww->bm_width, ww->bm_height,
	       ww->mem_dc, 0, 0, SRCCOPY);
	GdiFlush();  /* bits may be touched directly by other thread */
    }
    destroy_back_buffer(ww);
    ww->mem_dc = dc;
    ww->dib = dib;
    ww->old_bm = old;
    ww->bits = bits;
    ww->bm_width = nw;
    ww->bm_height = nh;
    ww->bm_bytes_per_row = nw * 4;
    LeaveCriticalSection(&ww->lock);
    return 0;
}

/* GUI thread: copy dirty part of back buffer to screen */
static void flush_window(Win32Window* ww)
{
    InterlockedExchange(&ww->flush_posted, 0);
    EnterCriticalSection(&ww->lock);
    if (ww->dirty && ww->mem_dc) {
	HDC hdc = GetDC(ww->hwnd);
	if (hdc) {
	    BitBlt(hdc, ww->dx0, ww->dy0,
		   ww->dx1 - ww->dx0, ww->dy1 - ww->dy0,
		   ww->mem_dc, ww->dx0, ww->dy0, SRCCOPY);
	    ReleaseDC(ww->hwnd, hdc);
	}
	GdiFlush();
	ww->dirty = 0;
    }
    LeaveCriticalSection(&ww->lock);
}

/******************************************************************************
 *  Keyboard
 *****************************************************************************/

static uint16_t get_modstate(void)
{
    uint16_t m = 0;
    if (GetKeyState(VK_LSHIFT)   & 0x8000) m |= EPX_KBD_MOD_LSHIFT;
    if (GetKeyState(VK_RSHIFT)   & 0x8000) m |= EPX_KBD_MOD_RSHIFT;
    if (GetKeyState(VK_LCONTROL) & 0x8000) m |= EPX_KBD_MOD_LCTRL;
    if (GetKeyState(VK_RCONTROL) & 0x8000) m |= EPX_KBD_MOD_RCTRL;
    if (GetKeyState(VK_LMENU)    & 0x8000) m |= EPX_KBD_MOD_LALT;
    if (GetKeyState(VK_RMENU)    & 0x8000) m |= EPX_KBD_MOD_RALT;
    if (GetKeyState(VK_LWIN)     & 0x8000) m |= EPX_KBD_MOD_LMETA;
    if (GetKeyState(VK_RWIN)     & 0x8000) m |= EPX_KBD_MOD_RMETA;
    if (GetKeyState(VK_NUMLOCK)  & 0x0001) m |= EPX_KBD_MOD_NUM;
    if (GetKeyState(VK_CAPITAL)  & 0x0001) m |= EPX_KBD_MOD_CAPS;
    if (GetKeyState(VK_SCROLL)   & 0x0001) m |= EPX_KBD_MOD_SCR;
    /* AltGr is reported by windows as LCtrl + RAlt */
    if ((m & EPX_KBD_MOD_RALT) && (m & EPX_KBD_MOD_LCTRL))
	m |= EPX_KBD_MOD_ALTGR;
    return m;
}

/* Map virtual key to an epx key symbol, 0 = not a "special" key */
static uint16_t vk_to_sym(UINT vk, UINT scan, int ext)
{
    switch(vk) {
    case VK_BACK:    return EPX_KBD_KEY_BACKSPACE;
    case VK_TAB:     return EPX_KBD_KEY_TAB;
    case VK_RETURN:  return ext ? EPX_KBD_KEY_KP_ENTER : EPX_KBD_KEY_ENTER;
    case VK_ESCAPE:  return EPX_KBD_KEY_ESCAPE;
    /* navigation keys, non extended ones are the keypad (numlock off) */
    case VK_LEFT:    return ext ? EPX_KBD_KEY_LEFT     : EPX_KBD_KEY_KP4;
    case VK_RIGHT:   return ext ? EPX_KBD_KEY_RIGHT    : EPX_KBD_KEY_KP6;
    case VK_UP:      return ext ? EPX_KBD_KEY_UP       : EPX_KBD_KEY_KP8;
    case VK_DOWN:    return ext ? EPX_KBD_KEY_DOWN     : EPX_KBD_KEY_KP2;
    case VK_INSERT:  return ext ? EPX_KBD_KEY_INSERT   : EPX_KBD_KEY_KP0;
    case VK_DELETE:  return ext ? EPX_KBD_KEY_DELETE   : EPX_KBD_KEY_KP_PERIOD;
    case VK_HOME:    return ext ? EPX_KBD_KEY_HOME     : EPX_KBD_KEY_KP7;
    case VK_END:     return ext ? EPX_KBD_KEY_END      : EPX_KBD_KEY_KP1;
    case VK_PRIOR:   return ext ? EPX_KBD_KEY_PAGEUP   : EPX_KBD_KEY_KP9;
    case VK_NEXT:    return ext ? EPX_KBD_KEY_PAGEDOWN : EPX_KBD_KEY_KP3;
    case VK_CLEAR:   return EPX_KBD_KEY_KP5;
    /* keypad, numlock on */
    case VK_NUMPAD0: case VK_NUMPAD1: case VK_NUMPAD2: case VK_NUMPAD3:
    case VK_NUMPAD4: case VK_NUMPAD5: case VK_NUMPAD6: case VK_NUMPAD7:
    case VK_NUMPAD8: case VK_NUMPAD9:
	return (uint16_t)(EPX_KBD_KEY_KP0 + (vk - VK_NUMPAD0));
    case VK_DECIMAL:  return EPX_KBD_KEY_KP_PERIOD;
    case VK_MULTIPLY: return EPX_KBD_KEY_KP_MULTIPLY;
    case VK_ADD:      return EPX_KBD_KEY_KP_PLUS;
    case VK_SUBTRACT: return EPX_KBD_KEY_KP_MINUS;
    case VK_DIVIDE:   return EPX_KBD_KEY_KP_DIVIDE;
    /* function keys (note F12 is not F11+1 in epx_event.h) */
    case VK_F1: case VK_F2: case VK_F3: case VK_F4: case VK_F5:
    case VK_F6: case VK_F7: case VK_F8: case VK_F9: case VK_F10:
    case VK_F11:
	return (uint16_t)(EPX_KBD_KEY_F1 + (vk - VK_F1));
    case VK_F12:     return EPX_KBD_KEY_F12;
    /* misc */
    case VK_PAUSE:
    case VK_CANCEL:  return EPX_KBD_KEY_QUIT;   /* as x11: Pause/Break */
    case VK_SNAPSHOT: return EPX_KBD_KEY_PRINT;
    case VK_APPS:    return EPX_KBD_KEY_MENU;
    /* modifiers */
    case VK_SHIFT:
	if (MapVirtualKeyW(scan, MAPVK_VSC_TO_VK_EX) == VK_RSHIFT)
	    return EPX_KBD_KEY_RSHIFT;
	return EPX_KBD_KEY_LSHIFT;
    case VK_LSHIFT:   return EPX_KBD_KEY_LSHIFT;
    case VK_RSHIFT:   return EPX_KBD_KEY_RSHIFT;
    case VK_CONTROL:  return ext ? EPX_KBD_KEY_RCTRL : EPX_KBD_KEY_LCTRL;
    case VK_LCONTROL: return EPX_KBD_KEY_LCTRL;
    case VK_RCONTROL: return EPX_KBD_KEY_RCTRL;
    case VK_MENU:     return ext ? EPX_KBD_KEY_RALT : EPX_KBD_KEY_LALT;
    case VK_LMENU:    return EPX_KBD_KEY_LALT;
    case VK_RMENU:    return EPX_KBD_KEY_RALT;
    case VK_LWIN:     return EPX_KBD_KEY_LMETA;
    case VK_RWIN:     return EPX_KBD_KEY_RMETA;
    default:
	return 0;
    }
}

/* keypad translation when numlock is on (same as x11 backend) */
static uint16_t numlock_sym(uint16_t sym)
{
    switch(sym) {
    case EPX_KBD_KEY_KP0: case EPX_KBD_KEY_KP1: case EPX_KBD_KEY_KP2:
    case EPX_KBD_KEY_KP3: case EPX_KBD_KEY_KP4: case EPX_KBD_KEY_KP5:
    case EPX_KBD_KEY_KP6: case EPX_KBD_KEY_KP7: case EPX_KBD_KEY_KP8:
    case EPX_KBD_KEY_KP9:
	return (uint16_t)((sym - EPX_KBD_KEY_KP0) + '0');
    case EPX_KBD_KEY_KP_PERIOD:   return '.';
    case EPX_KBD_KEY_KP_DIVIDE:   return '/';
    case EPX_KBD_KEY_KP_MULTIPLY: return '*';
    case EPX_KBD_KEY_KP_MINUS:    return '-';
    case EPX_KBD_KEY_KP_PLUS:     return '+';
    case EPX_KBD_KEY_KP_ENTER:    return EPX_KBD_KEY_ENTER;
    case EPX_KBD_KEY_KP_EQUALS:   return '=';
    default: return sym;
    }
}

/* GUI thread: WM_(SYS)KEYDOWN / WM_(SYS)KEYUP
 * NOTE: the message loop does NOT call TranslateMessage, the character
 * is computed here with ToUnicode on key down (once per key down, which
 * keeps dead key composition working) and remembered for the key up.
 */
static void key_event(Win32Window* ww, UINT vk, LPARAM lp, int down)
{
    epx_event_t e;
    UINT scan = (UINT)((lp >> 16) & 0xff);
    int  ext  = (int)((lp >> 24) & 1);
    uint16_t code = (uint16_t)(scan | (ext << 8));  /* 0..511 */
    uint16_t mod;
    uint16_t sym;

    /* auto repeat: bit 30 = previous key state (down) */
    if (down && (lp & (1L << 30)) &&
	(ww->ewin->mask & EPX_EVENT_NO_AUTO_REPEAT))
	return;
    /* lock keys only update state (as x11) */
    if ((vk == VK_NUMLOCK) || (vk == VK_CAPITAL) || (vk == VK_SCROLL))
	return;

    mod = get_modstate();

    if (!down && ww->key_sym[code]) {
	sym = ww->key_sym[code];  /* report same sym as on press */
	ww->key_sym[code] = 0;
    }
    else if ((sym = vk_to_sym(vk, scan, ext)) != 0) {
	if (mod & EPX_KBD_MOD_NUM)
	    sym = numlock_sym(sym);
    }
    else if (down) {
	BYTE  ks[256];
	WCHAR buf[8];
	int   n = 0;

	if (GetKeyboardState(ks))
	    n = ToUnicode(vk, scan, ks, buf, 8, 0);
	if (n < 0)        /* dead key, wait for the composed character */
	    return;
	if (n > 0)
	    sym = (uint16_t) buf[0];
	else if ((vk >= 'A') && (vk <= 'Z'))
	    sym = (mod & EPX_KBD_MOD_CTRL) ? (uint16_t)(vk & 0x1f) :
		(uint16_t)(vk + ('a'-'A'));
	else if ((vk >= '0') && (vk <= '9'))
	    sym = (uint16_t) vk;
	if (sym >= EPX_KBD_KEY_FIRST)  /* collides with epx special keys */
	    sym = 0;
    }
    if (sym == 0)
	return;
    if (down)
	ww->key_sym[code] = sym;

    memset(&e, 0, sizeof(e));
    e.type     = down ? EPX_EVENT_KEY_PRESS : EPX_EVENT_KEY_RELEASE;
    e.window   = ww->ewin;
    e.key.sym  = sym;
    e.key.code = code;
    e.key.mod  = mod;
    push_event(ww->be, &e);
}

/******************************************************************************
 *  GUI thread window procedures
 *****************************************************************************/

static uint32_t mk_buttons(WPARAM wp)
{
    uint32_t b = 0;
    if (wp & MK_LBUTTON) b |= EPX_BUTTON_LEFT;
    if (wp & MK_MBUTTON) b |= EPX_BUTTON_MIDDLE;
    if (wp & MK_RBUTTON) b |= EPX_BUTTON_RIGHT;
    return b;
}

static void report_configure(Win32Window* ww)
{
    RECT  r;
    POINT p = { 0, 0 };
    epx_window_t* win = ww->ewin;
    int w, h;

    if (IsIconic(ww->hwnd))
	return;
    GetClientRect(ww->hwnd, &r);
    ClientToScreen(ww->hwnd, &p);
    w = r.right - r.left;
    h = r.bottom - r.top;
    if (ww->configured &&
	(win->rarea.xy.x == p.x) && (win->rarea.xy.y == p.y) &&
	((int)win->rarea.wh.width == w) && ((int)win->rarea.wh.height == h))
	return;
    ww->configured = 1;
    win->rarea.xy.x = p.x;
    win->rarea.xy.y = p.y;
    win->rarea.wh.width = w;
    win->rarea.wh.height = h;
    push_area(ww, EPX_EVENT_CONFIGURE, p.x, p.y, w, h);
}

static void wheel_event(Win32Window* ww, int vertical, WPARAM wParam,
			LPARAM lParam)
{
    POINT p;
    int* acc = vertical ? &ww->wheel_acc : &ww->hwheel_acc;
    uint32_t pos = vertical ? EPX_WHEEL_UP : EPX_WHEEL_RIGHT;
    uint32_t neg = vertical ? EPX_WHEEL_DOWN : EPX_WHEEL_LEFT;

    p.x = GET_X_LPARAM(lParam);   /* screen coordinates */
    p.y = GET_Y_LPARAM(lParam);
    ScreenToClient(ww->hwnd, &p);
    *acc += GET_WHEEL_DELTA_WPARAM(wParam);
    /* x11 reports wheel as button press + release */
    while (*acc >= WHEEL_DELTA) {
	*acc -= WHEEL_DELTA;
	push_pointer(ww, EPX_EVENT_BUTTON_PRESS, pos, p.x, p.y);
	push_pointer(ww, EPX_EVENT_BUTTON_RELEASE, pos, p.x, p.y);
    }
    while (*acc <= -WHEEL_DELTA) {
	*acc += WHEEL_DELTA;
	push_pointer(ww, EPX_EVENT_BUTTON_PRESS, neg, p.x, p.y);
	push_pointer(ww, EPX_EVENT_BUTTON_RELEASE, neg, p.x, p.y);
    }
}

static LRESULT CALLBACK epx_wndproc(HWND hwnd, UINT msg,
				    WPARAM wParam, LPARAM lParam)
{
    Win32Window* ww;

    if (msg == WM_NCCREATE) {
	CREATESTRUCTW* cs = (CREATESTRUCTW*) lParam;
	ww = (Win32Window*) cs->lpCreateParams;
	ww->hwnd = hwnd;
	SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR) ww);
	return DefWindowProcW(hwnd, msg, wParam, lParam);
    }
    ww = (Win32Window*) GetWindowLongPtrW(hwnd, GWLP_USERDATA);
    if (ww == NULL)  /* before WM_NCCREATE or during destroy */
	return DefWindowProcW(hwnd, msg, wParam, lParam);

    switch(msg) {
    case WM_CLOSE:
	/* do not destroy, let the application decide */
	push_simple(ww, EPX_EVENT_CLOSE);
	return 0;

    case WM_ERASEBKGND:
	return 1;  /* everything is painted from the back buffer */

    case WM_PAINT: {
	PAINTSTRUCT ps;
	HDC hdc = BeginPaint(hwnd, &ps);
	int x = ps.rcPaint.left;
	int y = ps.rcPaint.top;
	int w = ps.rcPaint.right - ps.rcPaint.left;
	int h = ps.rcPaint.bottom - ps.rcPaint.top;
	if (hdc) {
	    EnterCriticalSection(&ww->lock);
	    if (ww->mem_dc)
		BitBlt(hdc, x, y, w, h, ww->mem_dc, x, y, SRCCOPY);
	    LeaveCriticalSection(&ww->lock);
	}
	EndPaint(hwnd, &ps);
	if ((w > 0) && (h > 0))
	    push_area(ww, EPX_EVENT_EXPOSE, x, y, w, h);
	return 0;
    }

    case WM_EPX_FLUSH:
	flush_window(ww);
	return 0;

    case WM_SIZE:
	if (wParam == SIZE_MINIMIZED)
	    break;
	ww->width  = LOWORD(lParam);
	ww->height = HIWORD(lParam);
	resize_back_buffer(ww, ww->width, ww->height);
	report_configure(ww);
	return 0;

    case WM_MOVE:
	report_configure(ww);
	return 0;

    case WM_GETMINMAXINFO: {
	MINMAXINFO* mmi = (MINMAXINFO*) lParam;
	int fw, fh;
	frame_size(ww->style, ww->ex_style, &fw, &fh);
	if (ww->min_w >= 0) mmi->ptMinTrackSize.x = ww->min_w + fw;
	if (ww->min_h >= 0) mmi->ptMinTrackSize.y = ww->min_h + fh;
	if (ww->max_w >= 0) {
	    mmi->ptMaxTrackSize.x = ww->max_w + fw;
	    mmi->ptMaxSize.x = ww->max_w + fw;
	}
	if (ww->max_h >= 0) {
	    mmi->ptMaxTrackSize.y = ww->max_h + fh;
	    mmi->ptMaxSize.y = ww->max_h + fh;
	}
	return 0;
    }

    case WM_SETFOCUS:
	push_simple(ww, EPX_EVENT_FOCUS_IN);
	return 0;

    case WM_KILLFOCUS:
	push_simple(ww, EPX_EVENT_FOCUS_OUT);
	return 0;

    case WM_MOUSEMOVE: {
	int x = GET_X_LPARAM(lParam);
	int y = GET_Y_LPARAM(lParam);
	if (!ww->mouse_inside) {
	    TRACKMOUSEEVENT tme;
	    tme.cbSize = sizeof(tme);
	    tme.dwFlags = TME_LEAVE;
	    tme.hwndTrack = hwnd;
	    tme.dwHoverTime = 0;
	    TrackMouseEvent(&tme);
	    ww->mouse_inside = 1;
	    push_pointer(ww, EPX_EVENT_ENTER, 0, x, y);
	}
	push_pointer(ww, EPX_EVENT_POINTER_MOTION, mk_buttons(wParam), x, y);
	return 0;
    }

    case WM_MOUSELEAVE: {
	POINT p;
	ww->mouse_inside = 0;
	GetCursorPos(&p);
	ScreenToClient(hwnd, &p);
	push_pointer(ww, EPX_EVENT_LEAVE, 0, p.x, p.y);
	return 0;
    }

    case WM_LBUTTONDOWN:
    case WM_MBUTTONDOWN:
    case WM_RBUTTONDOWN: {
	uint32_t b = (msg == WM_LBUTTONDOWN) ? EPX_BUTTON_LEFT :
	    ((msg == WM_MBUTTONDOWN) ? EPX_BUTTON_MIDDLE : EPX_BUTTON_RIGHT);
	SetCapture(hwnd);  /* get the release even outside the window */
	push_pointer(ww, EPX_EVENT_BUTTON_PRESS, b,
		     GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
	return 0;
    }

    case WM_LBUTTONUP:
    case WM_MBUTTONUP:
    case WM_RBUTTONUP: {
	uint32_t b = (msg == WM_LBUTTONUP) ? EPX_BUTTON_LEFT :
	    ((msg == WM_MBUTTONUP) ? EPX_BUTTON_MIDDLE : EPX_BUTTON_RIGHT);
	if ((wParam & (MK_LBUTTON|MK_MBUTTON|MK_RBUTTON)) == 0)
	    ReleaseCapture();
	push_pointer(ww, EPX_EVENT_BUTTON_RELEASE, b,
		     GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
	return 0;
    }

    case WM_MOUSEWHEEL:
	wheel_event(ww, 1, wParam, lParam);
	return 0;

    case WM_MOUSEHWHEEL:
	wheel_event(ww, 0, wParam, lParam);
	return 0;

    case WM_KEYDOWN:
	key_event(ww, (UINT) wParam, lParam, 1);
	return 0;
    case WM_KEYUP:
	key_event(ww, (UINT) wParam, lParam, 0);
	return 0;
    case WM_SYSKEYDOWN:  /* Alt+key, F10. Pass on so Alt+F4 still works */
	key_event(ww, (UINT) wParam, lParam, 1);
	break;
    case WM_SYSKEYUP:
	key_event(ww, (UINT) wParam, lParam, 0);
	break;

    case WM_SYSCOMMAND:
	/* Alt/F10 would enter (non existing) menu mode and eat keys */
	if ((wParam & 0xFFF0) == SC_KEYMENU)
	    return 0;
	break;

    default:
	break;
    }
    return DefWindowProcW(hwnd, msg, wParam, lParam);
}

/* GUI thread: create the HWND for ww */
static LRESULT gui_create_window(Win32Backend* be, Win32Window* ww)
{
    epx_window_t* win = ww->ewin;
    RECT r;
    HWND hwnd;

    r.left   = win->area.xy.x;
    r.top    = win->area.xy.y;
    r.right  = r.left + ww->width;
    r.bottom = r.top + ww->height;
    AdjustWindowRectEx(&r, ww->style, FALSE, ww->ex_style);

    if (resize_back_buffer(ww, ww->width, ww->height) < 0)
	return -1;

    hwnd = CreateWindowExW(ww->ex_style, EPX_WIN_CLASS, L"epx", ww->style,
			   r.left, r.top, r.right - r.left, r.bottom - r.top,
			   NULL, NULL, be->hinst, ww);
    if (hwnd == NULL) {
	EPX_DBGFMT("win32: CreateWindowEx failed error=%lu",
		   (unsigned long) GetLastError());
	destroy_back_buffer(ww);
	return -1;
    }
    /* as x11: window is mapped on attach */
    ShowWindow(hwnd, SW_SHOWNORMAL);
    UpdateWindow(hwnd);
    return 0;
}

static void gui_destroy_window(Win32Window* ww)
{
    if (ww->hwnd) {
	if (GetCapture() == ww->hwnd)
	    ReleaseCapture();
	/* detach ww first, no more events from this window */
	SetWindowLongPtrW(ww->hwnd, GWLP_USERDATA, 0);
	DestroyWindow(ww->hwnd);
	ww->hwnd = NULL;
    }
    EnterCriticalSection(&ww->lock);
    destroy_back_buffer(ww);
    LeaveCriticalSection(&ww->lock);
}

static void set_window_name(HWND hwnd, const char* name, size_t len)
{
    WCHAR* wname = to_wide(name, len);
    if (wname) {
	SetWindowTextW(hwnd, wname);
	free(wname);
    }
}

/* GUI thread: window_adjust, see README.md (x11 window parameters) */
static LRESULT gui_window_adjust(Win32Window* ww, epx_dict_t* param)
{
    epx_window_t* win = ww->ewin;
    HWND  hwnd = ww->hwnd;
    char* name = NULL;
    size_t name_len;
    int   ival, bval;
    int   set_pos = 0, set_size = 0;
    int   x, y, w, h;
    RECT  cr;
    POINT p = { 0, 0 };

    if (hwnd == NULL)
	return -1;

    if (epx_dict_lookup_string(param, "name", &name, &name_len) >= 0)
	set_window_name(hwnd, name, name_len);

    if (epx_dict_lookup_integer(param, "min_width", &ival) >= 0)
	ww->min_w = ival;
    if (epx_dict_lookup_integer(param, "min_height", &ival) >= 0)
	ww->min_h = ival;
    if (epx_dict_lookup_integer(param, "max_width", &ival) >= 0)
	ww->max_w = ival;
    if (epx_dict_lookup_integer(param, "max_height", &ival) >= 0)
	ww->max_h = ival;

    /* current client area in screen coordinates */
    GetClientRect(hwnd, &cr);
    ClientToScreen(hwnd, &p);
    x = p.x;
    y = p.y;
    w = cr.right - cr.left;
    h = cr.bottom - cr.top;

    if (epx_dict_lookup_integer(param, "x", &ival) >= 0) {
	win->area.xy.x = ival;
	if (ival != win->rarea.xy.x) { x = ival; set_pos = 1; }
    }
    if (epx_dict_lookup_integer(param, "y", &ival) >= 0) {
	win->area.xy.y = ival;
	if (ival != win->rarea.xy.y) { y = ival; set_pos = 1; }
    }
    if (epx_dict_lookup_integer(param, "width", &ival) >= 0) {
	win->area.wh.width = ival;
	if (ival != (int) win->rarea.wh.width) { w = ival; set_size = 1; }
    }
    if (epx_dict_lookup_integer(param, "height", &ival) >= 0) {
	win->area.wh.height = ival;
	if (ival != (int) win->rarea.wh.height) { h = ival; set_size = 1; }
    }
    /* "border_width" is accepted but ignored (window frame is the wm's) */

    if (set_pos || set_size) {
	RECT r;
	UINT flags = SWP_NOZORDER | SWP_NOACTIVATE;
	DWORD style = (DWORD) GetWindowLongW(hwnd, GWL_STYLE);
	DWORD ex_style = (DWORD) GetWindowLongW(hwnd, GWL_EXSTYLE);
	r.left = x;
	r.top = y;
	r.right = x + w;
	r.bottom = y + h;
	AdjustWindowRectEx(&r, style, FALSE, ex_style);
	if (!set_pos)  flags |= SWP_NOMOVE;
	if (!set_size) flags |= SWP_NOSIZE;
	SetWindowPos(hwnd, NULL, r.left, r.top,
		     r.right - r.left, r.bottom - r.top, flags);
    }

    if (epx_dict_lookup_boolean(param, "show", &bval) >= 0)
	ShowWindow(hwnd, bval ? SW_SHOW : SW_HIDE);

    if (epx_dict_lookup_boolean(param, "select", &bval) >= 0) {
	if (bval) {
	    if (IsIconic(hwnd))
		ShowWindow(hwnd, SW_RESTORE);
	    else if (!IsWindowVisible(hwnd))
		ShowWindow(hwnd, SW_SHOW);
	    BringWindowToTop(hwnd);
	    /* may only flash the task bar, foreground lock rules */
	    SetForegroundWindow(hwnd);
	}
    }
    return 1;
}

static LRESULT CALLBACK msg_wndproc(HWND hwnd, UINT msg,
				    WPARAM wParam, LPARAM lParam)
{
    Win32Backend* be;

    if (msg == WM_NCCREATE) {
	CREATESTRUCTW* cs = (CREATESTRUCTW*) lParam;
	SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR) cs->lpCreateParams);
	return DefWindowProcW(hwnd, msg, wParam, lParam);
    }
    be = (Win32Backend*) GetWindowLongPtrW(hwnd, GWLP_USERDATA);
    if (be == NULL)
	return DefWindowProcW(hwnd, msg, wParam, lParam);

    switch(msg) {
    case WM_EPX_CREATE:
	return gui_create_window(be, (Win32Window*) lParam);
    case WM_EPX_DESTROY:
	gui_destroy_window((Win32Window*) lParam);
	return 0;
    case WM_EPX_ADJUST: {
	adjust_req_t* req = (adjust_req_t*) lParam;
	return gui_window_adjust(req->ww, req->param);
    }
    case WM_EPX_SYNC: {
	Win32Window* ww = (Win32Window*) lParam;
	flush_window(ww);
	if (ww->hwnd)
	    UpdateWindow(ww->hwnd);  /* process pending WM_PAINT now */
	GdiFlush();
	return 0;
    }
    case WM_EPX_QUIT:
	DestroyWindow(hwnd);
	return 0;
    case WM_DESTROY:
	SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0);
	PostQuitMessage(0);
	return 0;
    default:
	break;
    }
    return DefWindowProcW(hwnd, msg, wParam, lParam);
}

static unsigned __stdcall gui_thread_main(void* arg)
{
    Win32Backend* be = (Win32Backend*) arg;
    MSG msg;

    /* make sure the thread has a message queue */
    PeekMessageW(&msg, NULL, WM_USER, WM_USER, PM_NOREMOVE);

    be->msg_hwnd = CreateWindowExW(0, EPX_MSG_CLASS, L"", 0, 0, 0, 0, 0,
				   HWND_MESSAGE, NULL, be->hinst, be);
    be->init_ok = (be->msg_hwnd != NULL);
    SetEvent(be->ready);
    if (!be->init_ok)
	return 1;

    while (GetMessageW(&msg, NULL, 0, 0) > 0) {
	/* No TranslateMessage: characters are computed in key_event */
	DispatchMessageW(&msg);
    }
    return 0;
}

/******************************************************************************
 *  Window classes
 *****************************************************************************/

static int register_classes(HINSTANCE hinst)
{
    WNDCLASSEXW wc;

    if (InterlockedIncrement(&class_refc) > 1)
	return 0;

    memset(&wc, 0, sizeof(wc));
    wc.cbSize        = sizeof(wc);
    wc.style         = CS_HREDRAW | CS_VREDRAW;  /* no CS_DBLCLKS */
    wc.lpfnWndProc   = epx_wndproc;
    wc.hInstance     = hinst;
    wc.hIcon         = LoadIconW(NULL, (LPCWSTR) IDI_APPLICATION);
    wc.hCursor       = LoadCursorW(NULL, (LPCWSTR) IDC_ARROW);
    wc.hbrBackground = NULL;
    wc.lpszClassName = EPX_WIN_CLASS;
    if (!RegisterClassExW(&wc) &&
	(GetLastError() != ERROR_CLASS_ALREADY_EXISTS))
	goto error;

    memset(&wc, 0, sizeof(wc));
    wc.cbSize        = sizeof(wc);
    wc.lpfnWndProc   = msg_wndproc;
    wc.hInstance     = hinst;
    wc.lpszClassName = EPX_MSG_CLASS;
    if (!RegisterClassExW(&wc) &&
	(GetLastError() != ERROR_CLASS_ALREADY_EXISTS)) {
	UnregisterClassW(EPX_WIN_CLASS, hinst);
	goto error;
    }
    return 0;
error:
    InterlockedDecrement(&class_refc);
    return -1;
}

static void unregister_classes(HINSTANCE hinst)
{
    if (InterlockedDecrement(&class_refc) == 0) {
	UnregisterClassW(EPX_WIN_CLASS, hinst);
	UnregisterClassW(EPX_MSG_CLASS, hinst);
    }
}

/******************************************************************************
 *  Backend API (called from the NIF backend_main thread)
 *****************************************************************************/

epx_backend_t* win32_init(epx_dict_t* param)
{
    Win32Backend* be;
    HMODULE hmod = NULL;
    int bval;

    if ((be = (Win32Backend*) calloc(1, sizeof(Win32Backend))) == NULL)
	return NULL;
    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.owner = NULL;
    be->b.user = NULL;
    be->b.name = "win32";
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;
    be->b.width = GetSystemMetrics(SM_CXSCREEN);
    be->b.height = GetSystemMetrics(SM_CYSCREEN);
    be->b.nformats = 1;
    be->b.formats[0] = BACK_BUFFER_FORMAT;
    epx_object_list_init(&be->b.window_list);
    epx_object_list_init(&be->b.pixmap_list);
    be->b.event = EPX_INVALID_HANDLE;
    be->b.cb = &win32_callbacks;

    /* window classes must be registered with the DLL instance */
    if (!GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
			    GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
			    (LPCWSTR) (void*) &win32_init, &hmod))
	hmod = GetModuleHandleW(NULL);
    be->hinst = (HINSTANCE) hmod;

    /* opt-in, affects the whole erl process */
    if ((epx_dict_lookup_boolean(param, "dpi_aware", &bval) >= 0) && bval)
	SetProcessDPIAware();

    if (register_classes(be->hinst) < 0) {
	free(be);
	return NULL;
    }
    InitializeCriticalSection(&be->qlock);
    be->qevent = CreateEventW(NULL, TRUE, FALSE, NULL);
    be->ready  = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (!be->qevent || !be->ready)
	goto error;

    be->gui_thread = (HANDLE) _beginthreadex(NULL, 0, gui_thread_main, be,
					     0, &be->gui_tid);
    if (be->gui_thread == NULL)
	goto error;
    WaitForSingleObject(be->ready, INFINITE);
    if (!be->init_ok) {
	WaitForSingleObject(be->gui_thread, INFINITE);
	CloseHandle(be->gui_thread);
	goto error;
    }
    be->b.event = (EPX_HANDLE_T) be->qevent;
    win32_adjust((epx_backend_t*) be, param);
    return (epx_backend_t*) &(be->b);

error:
    if (be->qevent) CloseHandle(be->qevent);
    if (be->ready) CloseHandle(be->ready);
    DeleteCriticalSection(&be->qlock);
    unregister_classes(be->hinst);
    free(be);
    return NULL;
}

int win32_upgrade(epx_backend_t* backend)
{
    /* FIXME: the GUI thread and the window procedures still run code
     * from the old DLL, a real upgrade must restart the GUI thread. */
    backend->cb = &win32_callbacks;
    return 0;
}

static int win32_finish(epx_backend_t* backend)
{
    Win32Backend* be = (Win32Backend*) backend;

    // FIXME: close all windows & detach all pixmaps (x11 does not either)
    SendMessageW(be->msg_hwnd, WM_EPX_QUIT, 0, 0);
    WaitForSingleObject(be->gui_thread, INFINITE);
    CloseHandle(be->gui_thread);
    CloseHandle(be->ready);
    CloseHandle(be->qevent);
    DeleteCriticalSection(&be->qlock);
    unregister_classes(be->hinst);
    free(be);
    return 0;
}

/* return the backend event handle (a Win32 event HANDLE, not an fd!) */
static EPX_HANDLE_T win32_evt_attach(epx_backend_t* backend)
{
    Win32Backend* be = (Win32Backend*) backend;
    return (EPX_HANDLE_T) be->qevent;
}

static int win32_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

/* return  0: no event returned
 *         1: one event returned & no pending
 *         2: one event returned & 1 pending ...
 */
static int win32_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    Win32Backend* be = (Win32Backend*) backend;
    int r = 0;

    EnterCriticalSection(&be->qlock);
    while (be->qlen > 0) {
	*e = be->q[be->qhead];
	be->qhead = (be->qhead + 1) % EVENT_QUEUE_SIZE;
	be->qlen--;
	if (!FilterEvent(e)) {
	    r = 1;
	    break;
	}
    }
    backend->pending = (int) be->qlen;
    if (be->qlen == 0)
	ResetEvent(be->qevent);
    LeaveCriticalSection(&be->qlock);
    if (r == 0)
	return 0;
    return 1 + backend->pending;
}

static int win32_pix_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    if (pixmap->opaque != NULL)
	return -1;
    /* nothing to allocate, conversion is done in pix_draw */
    epx_object_link(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) 1;
    pixmap->backend = backend;
    return 0;
}

static int win32_pix_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    if (pixmap->opaque != NULL) {
	epx_object_unlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}

static int win32_begin(epx_window_t* window)
{
    (void) window;
    return 0;
}

static int win32_end(epx_window_t* window, int off_screen)
{
    (void) window;
    (void) off_screen;
    return 0;
}

/* Copy (and convert) pixmap area into the window back buffer */
static int win32_pix_draw(epx_backend_t* backend, epx_pixmap_t* pixmap,
			  epx_window_t* window,
			  int src_x, int src_y, int dst_x, int dst_y,
			  unsigned int width,
			  unsigned int height)
{
    Win32Window* ww = (Win32Window*) window->opaque;
    int w, h;
    (void) backend;

    if ((ww == NULL) || (pixmap->data == NULL))
	return -1;
    w = (width > 0x7fffffff) ? 0x7fffffff : (int) width;
    h = (height > 0x7fffffff) ? 0x7fffffff : (int) height;

    /* clip against source */
    if (src_x < 0) { dst_x -= src_x; w += src_x; src_x = 0; }
    if (src_y < 0) { dst_y -= src_y; h += src_y; src_y = 0; }
    if (dst_x < 0) { src_x -= dst_x; w += dst_x; dst_x = 0; }
    if (dst_y < 0) { src_y -= dst_y; h += dst_y; dst_y = 0; }
    if (src_x + w > (int) pixmap->width)  w = (int) pixmap->width - src_x;
    if (src_y + h > (int) pixmap->height) h = (int) pixmap->height - src_y;

    EnterCriticalSection(&ww->lock);
    if (ww->bits != NULL) {
	/* clip against back buffer */
	if (dst_x + w > ww->bm_width)  w = ww->bm_width - dst_x;
	if (dst_y + h > ww->bm_height) h = ww->bm_height - dst_y;
	if ((w > 0) && (h > 0)) {
	    uint8_t* src = EPX_PIXEL_ADDR(pixmap, src_x, src_y);
	    uint8_t* dst = ww->bits + dst_y*ww->bm_bytes_per_row + dst_x*4;
	    epx_copy_area(src, (int) pixmap->bytes_per_row,
			  pixmap->pixel_format,
			  dst, ww->bm_bytes_per_row, BACK_BUFFER_FORMAT,
			  (unsigned) w, (unsigned) h);
	    if (!ww->dirty) {
		ww->dx0 = dst_x;     ww->dy0 = dst_y;
		ww->dx1 = dst_x + w; ww->dy1 = dst_y + h;
		ww->dirty = 1;
	    }
	    else {
		if (dst_x < ww->dx0) ww->dx0 = dst_x;
		if (dst_y < ww->dy0) ww->dy0 = dst_y;
		if (dst_x + w > ww->dx1) ww->dx1 = dst_x + w;
		if (dst_y + h > ww->dy1) ww->dy1 = dst_y + h;
	    }
	}
    }
    LeaveCriticalSection(&ww->lock);
    return 0;
}

/* make sure everything drawn is on screen */
static int win32_pix_sync(epx_backend_t* backend, epx_pixmap_t* pixmap,
			  epx_window_t* window)
{
    Win32Backend* be = (Win32Backend*) backend;
    (void) pixmap;
    if ((window == NULL) || (window->opaque == NULL))
	return 0;
    SendMessageW(be->msg_hwnd, WM_EPX_SYNC, 0, (LPARAM) window->opaque);
    return 0;
}

/* show the back buffer, asynchronous (coalesced) */
static int win32_win_swap(epx_backend_t* backend, epx_window_t* window)
{
    Win32Window* ww = (Win32Window*) window->opaque;
    int dirty;
    (void) backend;

    if ((ww == NULL) || (ww->hwnd == NULL))
	return -1;
    EnterCriticalSection(&ww->lock);
    dirty = ww->dirty;
    LeaveCriticalSection(&ww->lock);
    if (dirty && (InterlockedExchange(&ww->flush_posted, 1) == 0)) {
	if (!PostMessageW(ww->hwnd, WM_EPX_FLUSH, 0, 0))
	    InterlockedExchange(&ww->flush_posted, 0);
    }
    return 0;
}

static int win32_win_attach(epx_backend_t* backend, epx_window_t* window)
{
    Win32Backend* be = (Win32Backend*) backend;
    Win32Window* ww;

    if (window->opaque != NULL)
	return -1;
    if ((ww = (Win32Window*) calloc(1, sizeof(Win32Window))) == NULL)
	return -1;
    InitializeCriticalSection(&ww->lock);
    ww->be = be;
    ww->ewin = window;
    ww->style = WS_OVERLAPPEDWINDOW;
    ww->ex_style = WS_EX_APPWINDOW;
    ww->width = (window->area.wh.width > 0) ? (int) window->area.wh.width : 1;
    ww->height = (window->area.wh.height > 0) ?
	(int) window->area.wh.height : 1;
    ww->min_w = ww->min_h = ww->max_w = ww->max_h = -1;

    if (SendMessageW(be->msg_hwnd, WM_EPX_CREATE, 0, (LPARAM) ww) != 0) {
	DeleteCriticalSection(&ww->lock);
	free(ww);
	return -1;
    }
    epx_object_link(&backend->window_list, window);
    window->opaque  = (void*) ww;
    window->backend = backend;
    return 0;
}

static int win32_win_detach(epx_backend_t* backend, epx_window_t* window)
{
    Win32Backend* be = (Win32Backend*) backend;
    Win32Window* ww = (Win32Window*) window->opaque;

    if (ww == NULL)
	return -1;
    SendMessageW(be->msg_hwnd, WM_EPX_DESTROY, 0, (LPARAM) ww);
    purge_events(be, window);
    DeleteCriticalSection(&ww->lock);
    free(ww);
    epx_object_unlink(&backend->window_list, window);
    window->opaque  = NULL;
    window->backend = NULL;
    return 0;
}

static int win32_adjust(epx_backend_t* backend, epx_dict_t* param)
{
    int bval;
    if (epx_dict_lookup_boolean(param, "use_opengl", &bval) >= 0)
	backend->use_opengl = 0;  /* FIXME: WGL not implemented */
    return 1;
}

static int win32_info(epx_backend_t* backend, epx_dict_t* param)
{
    Win32Backend* be = (Win32Backend*) backend;
    int bval;

    if (epx_dict_lookup_boolean(param, "display", &bval) >= 0) {
	// closest thing to a display: the module instance
	epx_dict_set_binary(param, "display", &be->hinst, sizeof(HINSTANCE));
    }
    if (epx_dict_lookup_boolean(param, "double_buffer", &bval) >= 0)
	epx_dict_set_boolean(param, "double_buffer", 1);
    if (epx_dict_lookup_boolean(param, "use_exposure", &bval) >= 0)
	epx_dict_set_boolean(param, "use_exposure", 0);
    if (epx_dict_lookup_boolean(param, "use_opengl", &bval) >= 0)
	epx_dict_set_boolean(param, "use_opengl", backend->use_opengl);
    return 1;
}

static int win32_win_adjust(epx_window_t* window, epx_dict_t* param)
{
    Win32Backend* be = (Win32Backend*) window->backend;
    adjust_req_t req;

    if ((be == NULL) || (window->opaque == NULL))
	return -1;
    req.ww = (Win32Window*) window->opaque;
    req.param = param;
    /* must run on the GUI thread (window owner) */
    return (int) SendMessageW(be->msg_hwnd, WM_EPX_ADJUST, 0, (LPARAM) &req);
}

static int win32_win_info(epx_window_t* window, epx_dict_t* param)
{
    Win32Window* ww = (Win32Window*) window->opaque;
    int bval;

    if (ww == NULL)
	return -1;
    if (epx_dict_lookup_boolean(param, "name", &bval) >= 0) {
	WCHAR wname[256];
	char* name;
	/* cross thread GetWindowText sends WM_GETTEXT to the GUI thread */
	wname[0] = 0;
	GetWindowTextW(ww->hwnd, wname, 256);
	if ((name = from_wide(wname)) != NULL) {
	    epx_dict_set_string(param, "name", name);
	    free(name);
	}
	else
	    epx_dict_set_string(param, "name", "");
    }
    if (epx_dict_lookup_boolean(param, "window", &bval) >= 0) {
	// return the low-level HWND
	epx_dict_set_binary(param, "window", &ww->hwnd, sizeof(HWND));
    }
    return 1;
}
