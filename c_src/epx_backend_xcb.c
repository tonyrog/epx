/***************************************************************************
 *
 * Copyright (C) 2020, Rogvall Invest AB, <tony@rogvall.se>
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
 *  xcb display driver 
 */

#include <xcb/xcb.h>

#define XK_MISCELLANY
#define XK_XKB_KEYS
#define XK_LATIN1
#define XK_LATIN2
#define XK_LATIN3
#define XK_LATIN4
#define XK_CYRILLIC
#define XK_GREEK
#define XK_ARMENIAN
#include <X11/keysymdef.h>
// Need XInitThreads?
#include <X11/Xlib.h>


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
    xcb_window_t window;
    xcb_pixmap_t pm;     // off-screen pixmap
    int pm_height;
    int pm_width;
    xcb_gcontext_t gc;
    int x;
    int y;
    int height;
    int width;
    int grabbed;
    int accel_num;
    int accel_den;
    int thres;
    epx_rect_t dirty;
    xcb_timestamp_t last_key_time;
    xcb_keycode_t  last_key_code;
#ifdef HAVE_OPENGL
    GLXContext context;
    GLuint textureName;
#endif
} XCBWindow;

typedef struct {
    epx_backend_t b;                /* DO NOT MOVE !!! */
    xcb_connection_t* display; // rename later
    int      screen_nbr;
    xcb_screen_t* screen;
    xcb_visualid_t  visual;
    xcb_atom_t     wm_delete_window; /* atom WM_DELETE_WINDOW */
    unsigned short modstate;   /* modifier state */
    unsigned short grab_key;   /* grab key */
    int      use_off_screen;   /* use off screen pixmap */
    int      use_exposure;     /* use backing store or not */
    unsigned long black_pixel;    
    unsigned long white_pixel;
    xcb_generic_event_t* pending_event;
    xcb_keycode_t min_keycode;
    xcb_keycode_t max_keycode;
    int           nkeysyms;
    xcb_get_keyboard_mapping_reply_t *keymap;
    uint32_t      max_request_length;
#ifdef HAVE_OPENGL
    GLXFBConfig* fbconfigs;    // Fixme XFree!
    int          fbnumconfigs;
#endif
} XCBBackend;


// image description epx_pixmap_t used by put_image
typedef struct {
    int width;                  // width of image
    int height;		        // height of image
    int xoffset;		// number of pixels offset in X direction
    int format;			// XY_BITMAP, XY_PIXMAP, ZPIXMAP
    uint8_t *data;		// pointer to image data
    int byte_order;		// XCB_IMAGE_ORDER_LSB/MSB_FIRST
    int bitmap_unit;		// quant. of scanline 8, 16, 32
    int bitmap_bit_order;	// XCB_IMAGE_ORDER_MSB/LSB_FIRST
    int bitmap_pad;		// 8, 16, 32 either XY or ZPixmap
    int depth;			// depth of image
    int bytes_per_line;		// accelerator to next scanline
    int bits_per_pixel;		// bits per pixel (ZPixmap)
    unsigned long red_mask;	// bits in z arrangement
    unsigned long green_mask;
    unsigned long blue_mask;   
} XCBPixmap;

epx_backend_t* xcb_init(epx_dict_t* param);
int xcb_upgrade(epx_backend_t* be);

static int xcb_finish(epx_backend_t*);
static int xcb_pix_attach(epx_backend_t*, epx_pixmap_t*);
static int xcb_pix_detach(epx_backend_t*, epx_pixmap_t*);
static int xcb_draw_begin(epx_window_t*);
static int xcb_draw_end(epx_window_t*,int off_screen);
static int xcb_draw(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
		    int src_x, int src_y, int dst_x, int dst_y,
		    unsigned int width,
		    unsigned int height);
static int xcb_pix_sync(epx_backend_t*, epx_pixmap_t*, epx_window_t*);
static int xcb_win_attach(epx_backend_t*, epx_window_t*);
static int xcb_win_detach(epx_backend_t*, epx_window_t*);
static int xcb_win_swap(epx_backend_t*, epx_window_t*);
static EPX_HANDLE_T xcb_evt_attach(epx_backend_t*);
static int xcb_evt_detach(epx_backend_t*);
static int xcb_evt_read(epx_backend_t*, epx_event_t*);
static int xcb_adjust(epx_backend_t *backend, epx_dict_t* param);
static int xcb_win_adjust(epx_window_t*, epx_dict_t* param);
static int xcb_info(epx_backend_t*, epx_dict_t* param);
static int xcb_win_info(epx_window_t*, epx_dict_t* param);

static void xcb_grab(epx_backend_t* backend, epx_window_t* window, int toggle);


static epx_callbacks_t xcb_callbacks =
{
    .finish     = xcb_finish,
    .pix_attach = xcb_pix_attach,
    .pix_detach = xcb_pix_detach,
    .pix_draw   = xcb_draw,
    .pix_sync   = xcb_pix_sync,
    .win_attach = xcb_win_attach,
    .win_detach = xcb_win_detach,
    .evt_attach = xcb_evt_attach,
    .evt_detach = xcb_evt_detach,
    .evt_read   = xcb_evt_read,
    .adjust     = xcb_adjust,
    .win_swap   = xcb_win_swap,
    .begin      = xcb_draw_begin,
    .end        = xcb_draw_end,
    .win_adjust = xcb_win_adjust,
    .info       = xcb_info,
    .win_info   = xcb_win_info
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


static epx_window_t* find_event_window(XCBBackend* xcb, xcb_window_t w)
{
    epx_window_t* window;
    
    epx_lock_lock(xcb->b.window_list.lock);
    window = (epx_window_t*) xcb->b.window_list.first;

    while(window) {
	XCBWindow* w11 = (XCBWindow*) window->opaque;
	if (w11->window == w) {
	    epx_lock_unlock(xcb->b.window_list.lock);
	    return window;
	}
	window = window->next;
    }
    epx_lock_unlock(xcb->b.window_list.lock);
    return NULL;
}

#if 0
static void clip_window(XCBBackend* b, XCBWindow* w,
			int x, int y, unsigned int width, unsigned int height)
{
    xcb_rectangle_t clip;
    clip.x = x;
    clip.y = y;
    clip.width = width;
    clip.height = height;
    xcb_set_clip_rectangles(b->display, XCB_CLIP_ORDERING_UNSORTED,
			    w->gc, 0, 0, 1, &clip);
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

static int init_keysyms(XCBBackend* b)
{
    const xcb_setup_t* setup = xcb_get_setup(b->display);
    xcb_keycode_t min_keycode = setup->min_keycode;
    xcb_keycode_t max_keycode = setup->max_keycode;
    uint8_t count = max_keycode - min_keycode + 1;
    xcb_get_keyboard_mapping_cookie_t req;    
    xcb_generic_error_t* err;
    
    req = xcb_get_keyboard_mapping(b->display,min_keycode,count);
    if ((b->keymap = xcb_get_keyboard_mapping_reply(b->display, req, &err)) == NULL) {
	fprintf(stderr, "xcb_get_keyboard_mapping_reply %d\r\n",
		err->error_code);
	return -1;
    }
    b->min_keycode = min_keycode;
    b->max_keycode = max_keycode;
    b->nkeysyms = xcb_get_keyboard_mapping_keysyms_length(b->keymap);
    return b->nkeysyms;
}

static void convert_case(xcb_keysym_t sym,
			 xcb_keysym_t* lower,xcb_keysym_t* upper)
{
    *lower = sym;
    *upper = sym;

    switch(sym >> 8) {
    case 0: /* Latin 1 */
	if ((sym >= XK_A) && (sym <= XK_Z))
	    *lower += (XK_a - XK_A);
	else if ((sym >= XK_a) && (sym <= XK_z))
	    *upper -= (XK_a - XK_A);
	else if ((sym >= XK_Agrave) && (sym <= XK_Odiaeresis))
	    *lower += (XK_agrave - XK_Agrave);
	else if ((sym >= XK_agrave) && (sym <= XK_odiaeresis))
	    *upper -= (XK_agrave - XK_Agrave);
	else if ((sym >= XK_Ooblique) && (sym <= XK_Thorn))
	    *lower += (XK_oslash - XK_Ooblique);
	else if ((sym >= XK_oslash) && (sym <= XK_thorn))
	    *upper -= (XK_oslash - XK_Ooblique);
	break;
    case 1: /* Latin 2 */
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym == XK_Aogonek)
	    *lower = XK_aogonek;
	else if (sym >= XK_Lstroke && sym <= XK_Sacute)
	    *lower += (XK_lstroke - XK_Lstroke);
	else if (sym >= XK_Scaron && sym <= XK_Zacute)
	    *lower += (XK_scaron - XK_Scaron);
	else if (sym >= XK_Zcaron && sym <= XK_Zabovedot)
	    *lower += (XK_zcaron - XK_Zcaron);
	else if (sym == XK_aogonek)
	    *upper = XK_Aogonek;
	else if (sym >= XK_lstroke && sym <= XK_sacute)
	    *upper -= (XK_lstroke - XK_Lstroke);
	else if (sym >= XK_scaron && sym <= XK_zacute)
	    *upper -= (XK_scaron - XK_Scaron);
	else if (sym >= XK_zcaron && sym <= XK_zabovedot)
	    *upper -= (XK_zcaron - XK_Zcaron);
	else if (sym >= XK_Racute && sym <= XK_Tcedilla)
	    *lower += (XK_racute - XK_Racute);
	else if (sym >= XK_racute && sym <= XK_tcedilla)
	    *upper -= (XK_racute - XK_Racute);
	break;
    case 2: /* Latin 3 */
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym >= XK_Hstroke && sym <= XK_Hcircumflex)
	    *lower += (XK_hstroke - XK_Hstroke);
	else if (sym >= XK_Gbreve && sym <= XK_Jcircumflex)
	    *lower += (XK_gbreve - XK_Gbreve);
	else if (sym >= XK_hstroke && sym <= XK_hcircumflex)
	    *upper -= (XK_hstroke - XK_Hstroke);
	else if (sym >= XK_gbreve && sym <= XK_jcircumflex)
	    *upper -= (XK_gbreve - XK_Gbreve);
	else if (sym >= XK_Cabovedot && sym <= XK_Scircumflex)
	    *lower += (XK_cabovedot - XK_Cabovedot);
	else if (sym >= XK_cabovedot && sym <= XK_scircumflex)
	    *upper -= (XK_cabovedot - XK_Cabovedot);
	break;
    case 3: /* Latin 4 */
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym >= XK_Rcedilla && sym <= XK_Tslash)
	    *lower += (XK_rcedilla - XK_Rcedilla);
	else if (sym >= XK_rcedilla && sym <= XK_tslash)
	    *upper -= (XK_rcedilla - XK_Rcedilla);
	else if (sym == XK_ENG)
	    *lower = XK_eng;
	else if (sym == XK_eng)
	    *upper = XK_ENG;
	else if (sym >= XK_Amacron && sym <= XK_Umacron)
	    *lower += (XK_amacron - XK_Amacron);
	else if (sym >= XK_amacron && sym <= XK_umacron)
	    *upper -= (XK_amacron - XK_Amacron);
	break;
    case 6: /* Cyrillic */
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym >= XK_Serbian_DJE && sym <= XK_Serbian_DZE)
	    *lower -= (XK_Serbian_DJE - XK_Serbian_dje);
	else if (sym >= XK_Serbian_dje && sym <= XK_Serbian_dze)
	    *upper += (XK_Serbian_DJE - XK_Serbian_dje);
	else if (sym >= XK_Cyrillic_YU && sym <= XK_Cyrillic_HARDSIGN)
	    *lower -= (XK_Cyrillic_YU - XK_Cyrillic_yu);
	else if (sym >= XK_Cyrillic_yu && sym <= XK_Cyrillic_hardsign)
	    *upper += (XK_Cyrillic_YU - XK_Cyrillic_yu);
	break;
    case 7: /* Greek */
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym >= XK_Greek_ALPHAaccent && sym <= XK_Greek_OMEGAaccent)
	    *lower += (XK_Greek_alphaaccent - XK_Greek_ALPHAaccent);
	else if (sym >= XK_Greek_alphaaccent && sym <= XK_Greek_omegaaccent &&
		 sym != XK_Greek_iotaaccentdieresis &&
		 sym != XK_Greek_upsilonaccentdieresis)
	    *upper -= (XK_Greek_alphaaccent - XK_Greek_ALPHAaccent);
	else if (sym >= XK_Greek_ALPHA && sym <= XK_Greek_OMEGA)
	    *lower += (XK_Greek_alpha - XK_Greek_ALPHA);
	else if (sym >= XK_Greek_alpha && sym <= XK_Greek_omega &&
		 sym != XK_Greek_finalsmallsigma)
	    *upper -= (XK_Greek_alpha - XK_Greek_ALPHA);
	break;
    case 0x14: /* Armenian */
	if (sym >= XK_Armenian_AYB && sym <= XK_Armenian_fe) {
	    *lower = sym | 1;
	    *upper = sym & ~1;
	}
	break;
    default:
	break;
    }
}

static xcb_keysym_t keycode_to_keysym(XCBBackend* b, xcb_keycode_t code, int col)
{
    xcb_keysym_t* keysyms = xcb_get_keyboard_mapping_keysyms(b->keymap);
    int k = b->keymap->keysyms_per_keycode;

    if ((col < 0) || ((col >= k) && (col > 3)))
	return XCB_NO_SYMBOL;
    if (code < b->min_keycode)
	return XCB_NO_SYMBOL;
    if (code > b->max_keycode)
	return XCB_NO_SYMBOL;
    
    keysyms = &keysyms[(code - b->min_keycode) * k];
    
    if (col < 4) {
      if (col > 1) {
	  while ((k > 2) && (keysyms[k - 1] == XCB_NO_SYMBOL))
	      k--;
	  if (k < 3)
	      col -= 2;
      }
      if ((k <= (col|1)) || (keysyms[col|1] == XCB_NO_SYMBOL)) {
	  xcb_keysym_t lsym, usym;
	  convert_case(keysyms[col&~1], &lsym, &usym);
	  if (!(col & 1))
	      return lsym;
	  else if (usym == lsym)
	      return XCB_NO_SYMBOL;
	  else
	      return usym;
      }
    }
    return keysyms[col]; 
}

    
// scan trough a screen and locate a "nice" visual 
static int make_image_formats(const xcb_setup_t* setup,
			      epx_format_t* efmt, size_t efmt_size)
{
    xcb_format_iterator_t iter = xcb_setup_pixmap_formats_iterator (setup);
    uint8_t image_byte_order = setup->image_byte_order;
    int n = 0;
    //uint8_t bitmap_format_bit_order = setup->bitmap_format_bit_order;
    //uint8_t bitmap_format_scanline_unit = setup->bitmap_format_scanline_unit;
    //uint8_t bitmap_format_scanline_pad  = setup->bitmap_format_scanline_pad;
    
    while(iter.rem && efmt_size) {
	switch(iter.data->bits_per_pixel) {
	case 8:
	    *efmt++ = EPX_FMT_GRAY; // RGB232?
	    efmt_size--;
	    n++;
	    break;
	case 16:
	    if (image_byte_order == XCB_IMAGE_ORDER_LSB_FIRST)
		*efmt++ = EPX_FORMAT_R5G6B5_LE;
	    else
		*efmt++ = EPX_FORMAT_R5G6B5_BE;
	    efmt_size--;
	    n++;	    
	    break;
	case 24:
	    if (image_byte_order == XCB_IMAGE_ORDER_LSB_FIRST)
		*efmt++ = EPX_FORMAT_B8G8R8;
	    else
		*efmt++ = EPX_FORMAT_R8G8B8;
	    efmt_size--;
	    n++;
	    break;
	case 32:
	    if (image_byte_order == XCB_IMAGE_ORDER_LSB_FIRST)
		*efmt++ = EPX_FORMAT_A8R8G8B8_LE;
	    else
		*efmt++ = EPX_FORMAT_A8R8G8B8_BE;
	    efmt_size--;
	    n++;
	    break;
	default:
	    break;
	}
	xcb_format_next(&iter);
    }
    return n;
}

// scan trough a screen and locate a "nice" visual 
static int make_screen_formats(xcb_screen_t* screen, epx_format_t* efmt,
			size_t efmt_size)
{
    size_t efmt_loaded = 0;
    xcb_depth_iterator_t iter;

    iter = xcb_screen_allowed_depths_iterator (screen);
    while(iter.rem) {
	xcb_visualtype_iterator_t jter;
	jter = xcb_depth_visuals_iterator (iter.data);
	while(jter.rem) {
	    int r_size = bit_count(jter.data->red_mask);
	    int g_size = bit_count(jter.data->green_mask);
	    int b_size = bit_count(jter.data->blue_mask);
	    int r_offs = bit_pos(jter.data->red_mask);
	    int g_offs = bit_pos(jter.data->green_mask);
	    int b_offs = bit_pos(jter.data->blue_mask);
	    int fmt = 0;
	    int bgr = 0;
	    int alpha = 0;
	    int alpha_first = 0;
	    int little_endian = 0;
	    int bits_per_pixel = 0;
	    epx_format_t f;
	    int c = jter.data->_class;
	    int k;

	    if (((c != XCB_VISUAL_CLASS_TRUE_COLOR) &&
		 (c != XCB_VISUAL_CLASS_DIRECT_COLOR)))
		continue;
#if BYTE_ORDER == LITTLE_ENDIAN
	    little_endian = 1;  // stored in native format
#endif
	    bgr = (b_offs > g_offs);
	    if ((jter.data->bits_per_rgb_value == 5) &&
		(iter.data->depth == 15)) {
		fmt = EPX_FMT_RGB5;
		bits_per_pixel = 16;
	    }
	    else if ((jter.data->bits_per_rgb_value == 5) &&
		     (iter.data->depth == 16)) {
		if ((r_size == 5) && (g_size == 5) && (b_size == 5)) {
		    fmt = EPX_FMT_RGB5;
		    alpha = 1;
		    alpha_first = (r_offs == 14);
		}
		else if ((r_size == 5) && (g_size == 6) && (b_size == 5))
		    fmt = EPX_FMT_RGB565;
		bits_per_pixel = 16;
	    }
	    else if ((jter.data->bits_per_rgb_value == 8) &&
		     (iter.data->depth == 24)) {
		fmt = EPX_FMT_RGB8;
		little_endian = 0;  // ignore endian in this case?
		bits_per_pixel = 24;
	    }
	    else if ((jter.data->bits_per_rgb_value == 8) &&
		     (iter.data->depth == 32)) {
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
	    xcb_visualtype_next(&jter);
	}
	xcb_depth_next(&iter);
    }
    return (int) efmt_loaded;
}
// #endif

static void init_off_screen(XCBWindow* w)
{
    w->pm = 0;
    w->pm_width  = 0;
    w->pm_height = 0;
}

static int create_off_screen(XCBBackend* b, XCBWindow* w)
{
    if (b->use_off_screen) {
	xcb_pixmap_t pm = xcb_generate_id(b->display);
	xcb_void_cookie_t req;
	xcb_generic_error_t* err;
	req = xcb_create_pixmap(b->display, b->screen->root_depth,
				pm,
				w->window,
				w->width, w->height);
	if ((err = xcb_request_check(b->display, req)) != NULL) {
	    fprintf(stderr, "create_pixmap error %d\r\n", err->error_code);
	    return -1;
	}
	w->pm = pm;
	if (w->pm == 0) return -1;
	w->pm_width  = w->width;
	w->pm_height = w->height;
    }
    return 0;
}

static int destroy_off_screen(XCBBackend* b, XCBWindow* w)
{
    if (w->pm == 0) return -1;
    xcb_free_pixmap(b->display, w->pm);
    init_off_screen(w);
    return 0;
}

static int resize_off_screen(XCBBackend* b, XCBWindow* w)
{
    int width = w->width;
    int height = w->height;
    int ww, hh;
    xcb_void_cookie_t req;
    xcb_generic_error_t* err;    
    xcb_pixmap_t pm; 
    
    if (w->pm == 0) return -1;
    if ((w->pm_width >= width) && (w->pm_height >= height))
	return 0; // ok do never shrink (maybe later)
    ww = (width > w->pm_width) ? width+(width/2) : w->pm_width;
    hh = (height > w->pm_height) ? height+(height/2) : w->pm_height;
    // printf("pm: w=%d,h=%d\r\n", ww, hh);
    pm = xcb_generate_id(b->display);
    req = xcb_create_pixmap(b->display, b->screen->root_depth,
			    pm,
			    w->window,
			    ww, hh);
    if ((err = xcb_request_check(b->display, req)) != NULL) {
	fprintf(stderr, "create_pixmap error %d\r\n", err->error_code);
	return -1;
    }    
    xcb_copy_area(b->display,w->pm,pm,w->gc,0,0,0,0,w->pm_width,w->pm_height);
    xcb_free_pixmap(b->display, w->pm);
    w->pm = pm;
    w->pm_width  = ww;
    w->pm_height = hh;
    return 0;
}

epx_backend_t* xcb_init(epx_dict_t* param)
{
    XCBBackend* b;
    char* display_name = NULL;
    size_t len;
    const xcb_setup_t* setup;
    xcb_screen_iterator_t iter;
    xcb_intern_atom_cookie_t req;
    xcb_intern_atom_reply_t* rep;
    int n;
    char* name;
    
    // Must be the first XLib call.
    XInitThreads();

    if ((b = (XCBBackend*) malloc(sizeof(XCBBackend))) == NULL)
	return NULL;
    EPX_OBJECT_INIT((epx_backend_t*)b, EPX_BACKEND_TYPE);
    b->b.name = "xcb";
    b->b.on_heap = 1;
    b->b.refc = 1;
    b->b.cb = &xcb_callbacks;
    b->b.pending = 0;
    b->b.opengl = 0;
    b->b.width = 0;
    b->b.height = 0;
    epx_object_list_init(&b->b.pixmap_list);
    epx_object_list_init(&b->b.window_list);
    b->b.nformats = 0;
    b->b.event = EPX_INVALID_HANDLE;
#ifdef HAVE_OPENGL
    b->fbconfigs = NULL;
#endif

    epx_dict_lookup_string(param, "display", &display_name, &len);

    if ((b->display = xcb_connect(display_name, &b->screen_nbr)) == NULL) {
	free(b);
	return NULL;
    }

    setup = xcb_get_setup(b->display);

    // Get the screen #screen_nbr
    b->screen = NULL;
    n = b->screen_nbr;
    iter = xcb_setup_roots_iterator (setup);
    while(n && iter.rem) {
	xcb_screen_next (&iter);
	n--;
    }
    if (n == 0)
	b->screen = iter.data;
    else {
	fprintf(stderr, "unable to find screen %d\r\n", b->screen_nbr);
	return NULL;
    }

    init_keysyms(b);  // load keysyms
    
    
    b->b.use_opengl = 0;
    b->use_off_screen = 1;
    b->use_exposure = 0;

    b->b.width  = b->screen->width_in_pixels;
    b->b.height = b->screen->height_in_pixels;
    b->white_pixel = b->screen->white_pixel;
    b->black_pixel = b->screen->black_pixel;
    b->pending_event = NULL;
    b->max_request_length = xcb_get_maximum_request_length(b->display);    
    b->b.nformats = 
	make_screen_formats(b->screen, b->b.formats, EPX_BACKEND_MAX_FORMATS);

    xcb_adjust((epx_backend_t*)b, param);

    // wrap in a function?
    name = "WM_DELETE_WINDOW";
    req = xcb_intern_atom(b->display,0,strlen(name), name);
    rep = xcb_intern_atom_reply(b->display, req, NULL);
    b->wm_delete_window = rep->atom;
    free(rep);

    /* Fixme locate a proper visual */
    /* xcb->visual = find_visual_of_screen(screen); */
    b->visual = b->screen->root_visual;
    b->modstate = 0;
    b->grab_key = 0;



    /* FIXME
    if(XkbGetIndicatorState (be->display,XkbUseCoreKbd,&state) == Success) {
	EPX_DBGFMT("initial modstate=%x", state);
	if (state & NUM_LOCK_MASK)
	    be->modstate |= EPX_KBD_MOD_NUM;
	if (state & CAPS_LOCK_MASK)
	    be->modstate |= EPX_KBD_MOD_CAPS;
	if (state & SCROLL_LOCK_MASK)
	    be->modstate |= EPX_KBD_MOD_SCR;
	EPX_DBGFMT("xcb modstate=%x\n", be->modstate);
    }
    */
    return (epx_backend_t*) &(b->b);
}

int xcb_upgrade(epx_backend_t* backend)
{
    backend->cb = &xcb_callbacks;
    return 0;
}

/* return the backend event handle */
static EPX_HANDLE_T xcb_evt_attach(epx_backend_t* backend)
{
    XCBBackend* b = (XCBBackend*) backend;
    int fd = xcb_get_file_descriptor(b->display);
    return (EPX_HANDLE_T)((long)fd);
}

static int xcb_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

static int xcb_finish(epx_backend_t* backend)
{
    XCBBackend* b = (XCBBackend*) backend;
    // FIXME: close all windows & detach all pixmaps
    free(b->keymap);
    xcb_disconnect(b->display);
    free(b);
    return 0;
}

static int xcb_pix_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    XCBBackend* b = (XCBBackend*) backend;
    XCBPixmap* p;
    unsigned int bytes_per_row   = pixmap->bytes_per_row;
    unsigned int bits_per_pixel  = pixmap->bits_per_pixel;

    if (pixmap->opaque != NULL)
	return -1;

    if ((p = (XCBPixmap*) calloc(1,sizeof(XCBPixmap))) == NULL)
	return -1;

    p->width  = pixmap->width;
    p->height = pixmap->height;
    p->xoffset = 0;
    p->format = XCB_IMAGE_FORMAT_Z_PIXMAP;
    p->data   = pixmap->data;
    p->bitmap_bit_order = XCB_IMAGE_ORDER_MSB_FIRST;
    switch(pixmap->pixel_format) {
    case EPX_FORMAT_RGB:
	p->byte_order  = XCB_IMAGE_ORDER_MSB_FIRST;
	p->bitmap_unit = 8;
	p->bitmap_pad  = 8;
	break;
    case EPX_FORMAT_BGR:
	p->byte_order  = XCB_IMAGE_ORDER_LSB_FIRST;
	p->bitmap_unit = 8;
	p->bitmap_pad  = 8;
	break;
    case EPX_FORMAT_RGBA:
#if BYTE_ORDER == BIG_ENDIAN
	p->byte_order  = XCB_IMAGE_ORDER_LSB_FIRST;
#else
	p->byte_order  = XCB_IMAGE_ORDER_MSB_FIRST;
#endif
	p->bitmap_unit = 32;
	p->bitmap_pad  = 32;
	break;
    case EPX_FORMAT_ARGB:
#if BYTE_ORDER == BIG_ENDIAN
	p->byte_order  = XCB_IMAGE_ORDER_LSB_FIRST;
#else
	p->byte_order  = XCB_IMAGE_ORDER_MSB_FIRST;
#endif
	p->bitmap_unit = 32;
	p->bitmap_pad  = 32;
	break;
    case EPX_FORMAT_BGRA:
#if BYTE_ORDER == BIG_ENDIAN
	p->byte_order  = XCB_IMAGE_ORDER_MSB_FIRST;
#else
	p->byte_order  = XCB_IMAGE_ORDER_LSB_FIRST;
#endif
	p->bitmap_unit = 32;
	p->bitmap_pad  = 32;
	break;
    case EPX_FORMAT_ABGR:
#if BYTE_ORDER == BIG_ENDIAN
	p->byte_order  = XCB_IMAGE_ORDER_LSB_FIRST;
#else
	p->byte_order  = XCB_IMAGE_ORDER_MSB_FIRST;
#endif
	p->bitmap_unit = 32;
	p->bitmap_pad  = 32;
	break;
    default:
	break;
    }
    p->depth = 24;                     /* ...*/
    p->bytes_per_line = bytes_per_row;
    p->bits_per_pixel = bits_per_pixel;
    p->red_mask   = 0xFF << 16;
    p->green_mask = 0xFF <<  8;
    p->blue_mask  = 0xFF <<  0;

    EPX_DBGFMT("  width=%d",   p->width);
    EPX_DBGFMT("  height=%d",  p->height);
    EPX_DBGFMT("  xoffset=%d", p->xoffset);
    EPX_DBGFMT("  format=%s", 
		((p->format==XCB_IMAGE_FORMAT_XY_BITMAP)?
		 "XCB_IMAGE_FORMAT_XY_BITMAP" :
		 ((p->format==XCB_IMAGE_FORMAT_XY_PIXMAP)?
		  "XCB_IMAGE_FORMAT_XY_PIXMAP" :
		  ((p->format==XCB_IMAGE_FORMAT_Z_PIXMAP)?
		   "XCB_IMAGE_FORMAT_Z_PIXMAP" :"?" ))));
    EPX_DBGFMT("  byte_order=%s", 
	       ( (p->byte_order==XCB_IMAGE_ORDER_LSB_FIRST) ?
		 "XCB_IMAGE_ORDER_LSB_FIRST" :
		 ((p->byte_order==XCB_IMAGE_ORDER_MSB_FIRST) ?
		  "XCB_IMAGE_ORDER_MSB_FIRST" : "?")));
    EPX_DBGFMT("  bitmap_unit=%d", p->bitmap_unit);
    EPX_DBGFMT("  bitmap_bit_order=%s", 
	       ( (p->bitmap_bit_order==XCB_IMAGE_ORDER_LSB_FIRST) ?
		 "XCB_IMAGE_ORDER_LSB_FIRST" :
		 ((p->bitmap_bit_order==XCB_IMAGE_ORDER_MSB_FIRST) ?
		  "XCB_IMAGE_ORDER_MSB_FIRST" : "?")));
    EPX_DBGFMT("  bitmap_pad=%d", p->bitmap_pad);
    EPX_DBGFMT("  depth=%d", p->depth);
    EPX_DBGFMT("  bytes_per_line=%d", p->bytes_per_line);
    EPX_DBGFMT("  bits_per_pixel=%d", p->bits_per_pixel);
    EPX_DBGFMT("  red_mask=%lX", p->red_mask);
    EPX_DBGFMT("  green_mask=%lX", p->green_mask);
    EPX_DBGFMT("  blue_mask=%lX", p->blue_mask);

    // FIXME: refcount pixmap?
    epx_object_link(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) p;
    pixmap->backend = (epx_backend_t*) b;
    return 0;
}

static int xcb_pix_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    XCBPixmap* p = (XCBPixmap*) pixmap->opaque;

    if (p != NULL) {
	free(p);
	epx_object_unlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}

static int xcb_draw_begin(epx_window_t* ewin)
{
    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	XCBBackend* b  = (XCBBackend*) ewin->backend;
	XCBWindow*  w  = (XCBWindow*) ewin->opaque;
	
	glXMakeCurrent(b->display, w->window, w->context);
#endif
    }
    return 0;
}

static int xcb_draw_end(epx_window_t* ewin, int off_screen)
{
    // XCBWindow*  w = (XCBWindow*) ewin->opaque;
    XCBBackend* b  = (XCBBackend*) ewin->backend;

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	glFlush();
	if (!off_screen)
	    glXSwapBuffers(b->display, w11->window);
#endif
    }
    else if (off_screen)
	return 0;
    else if (!b->use_off_screen) {
	xcb_flush(b->display);
	if (b->pending_event == NULL)
	    b->pending_event = xcb_poll_for_queued_event(b->display);
	b->b.pending = (b->pending_event != NULL);
    }
    return 0;
}

static int xcb_draw(epx_backend_t* backend, epx_pixmap_t* pic,
		    epx_window_t* ewin,
		    int src_x, int src_y, int dst_x, int dst_y,
		    unsigned int width,
		    unsigned int height)
{
    XCBBackend* b = (XCBBackend*) backend;
    XCBWindow*  w = (XCBWindow*) ewin->opaque;
    XCBPixmap*  p = (XCBPixmap*) pic->opaque;

    if (w == NULL)
	return -1;
    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	// Fixme check for errors
	epx_gl_load_texture(pic, &w->textureName, 0, 1, GL_REPEAT,
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
	xcb_drawable_t d = (b->use_off_screen && w->pm) ? w->pm : w->window;
	uint8_t left_pad = 0;

	if (src_x != 0) { // write each line
	    uint8_t* data = p->data + p->bytes_per_line*src_y + src_x;
	    uint32_t data_len = (width * p->bits_per_pixel + 7) / 8;
	    unsigned int i;

	    EPX_DBGFMT("src_x=%d,src_y=%d,data_len=%ld*%d, max_req_len=%u",
		       src_x, src_y, data_len, height,
		       b->max_request_length);
	    
	    for (i = 0; i < height; i++) {
		xcb_put_image(b->display,
			      p->format,
			      d,
			      w->gc,
			      width, height,
			      dst_x, dst_y,
			      left_pad,
			      p->bits_per_pixel,
			      data_len,
			      data);
		dst_y++;
		data += p->bytes_per_line;
	    }
	}
	else {
	    // src_x = 0 we should be able to write all at once
	    // but then epx_pixmap alignement must match!
	    uint8_t* data = p->data + p->bytes_per_line*src_y;
	    uint32_t data_len = p->bytes_per_line * height;

	    EPX_DBGFMT("src_x=%d,src_y=%d,data_len=%ld max_req_len=%d",
		       src_x, src_y, data_len, b->max_request_length);
	    
	    xcb_put_image(b->display,
			  p->format,
			  d,
			  w->gc,
			  width, height,
			  dst_x, dst_y,
			  left_pad,
			  p->bits_per_pixel,
			  data_len,
			  data);
	}
    }
    return 0;
}

// make sure all draw command have been synced
static int xcb_pix_sync(epx_backend_t* backend, epx_pixmap_t* pixmap,
			epx_window_t* ewin)
{
    XCBBackend* be = (XCBBackend*) backend;
    (void) pixmap;
    (void) ewin;

    return xcb_flush(be->display);
}

static int xcb_win_swap(epx_backend_t* backend, epx_window_t* ewin)
{
    XCBBackend* b = (XCBBackend*) backend;
    XCBWindow*  w = (XCBWindow*) ewin->opaque;

    if (ewin->opengl) {
#ifdef HAVE_OPENGL
	glXMakeCurrent(be->display, w->window, w->context);
	glXSwapBuffers(be->display, w->window);
#endif
    }
    else if (w && w->pm && b->use_off_screen) {
	xcb_copy_area(b->display, w->pm, w->window, w->gc,
		      0, 0, 0, 0, w->width, w->height);
	xcb_flush(b->display);
    }
    else {
	xcb_flush(b->display);
    }

    if (b->pending_event == NULL)
	b->pending_event = xcb_poll_for_queued_event(b->display);
    b->b.pending = (b->pending_event != NULL);
    return 0;
}

// util set value according to bit mask flag = (1 << pos)
static void set_value(uint32_t mask, uint32_t flag, uint32_t* values, uint32_t value)
{
    if (flag & mask) {
	int i = __builtin_popcount(mask & (flag-1));
	values[i] = value;
    }
}


static int xcb_win_attach(epx_backend_t* backend, epx_window_t* ewin)
{
    XCBBackend* b = (XCBBackend*) backend;
    XCBWindow*  w = NULL;
    xcb_window_t win = 0;
    uint32_t event_mask;
    uint32_t value_mask;
    uint32_t value_list[32];

    if (ewin->opaque != NULL)
	return -1;
    if ((w = (XCBWindow*) malloc(sizeof(XCBWindow))) == NULL)
	return -1;
    memset(w, 0, sizeof(XCBWindow));

    value_mask =
	XCB_CW_BACK_PIXEL |
	XCB_CW_BORDER_PIXEL |
	XCB_CW_SAVE_UNDER | 
	XCB_CW_OVERRIDE_REDIRECT |
	XCB_CW_EVENT_MASK;
    if (!b->use_exposure)
	value_mask |= XCB_CW_BACKING_STORE;

    event_mask =
	XCB_EVENT_MASK_KEY_PRESS
	| XCB_EVENT_MASK_KEY_RELEASE
	| XCB_EVENT_MASK_BUTTON_PRESS
	| XCB_EVENT_MASK_BUTTON_RELEASE
	| XCB_EVENT_MASK_ENTER_WINDOW
	| XCB_EVENT_MASK_LEAVE_WINDOW
	| XCB_EVENT_MASK_POINTER_MOTION
	// | XCB_EVENT_MASK_POINTER_MOTION_HINT
	// | XCB_EVENT_MASK_BUTTON_1_MOTION
	// | XCB_EVENT_MASK_BUTTON_2_MOTION
	// | XCB_EVENT_MASK_BUTTON_3_MOTION
	// | XCB_EVENT_MASK_BUTTON_4_MOTION
	// | XCB_EVENT_MASK_BUTTON_5_MOTION
	// | XCB_EVENT_MASK_BUTTON_MOTION
	| XCB_EVENT_MASK_KEYMAP_STATE
	| XCB_EVENT_MASK_VISIBILITY_CHANGE
	| XCB_EVENT_MASK_STRUCTURE_NOTIFY
	// | XCB_EVENT_MASK_RESIZE_REDIRECT
	| XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY
	| XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
	| XCB_EVENT_MASK_FOCUS_CHANGE
	| XCB_EVENT_MASK_PROPERTY_CHANGE
	| XCB_EVENT_MASK_COLOR_MAP_CHANGE
	| XCB_EVENT_MASK_OWNER_GRAB_BUTTON
	| XCB_EVENT_MASK_EXPOSURE;
	
    set_value(value_mask, XCB_CW_EVENT_MASK, value_list, event_mask);

    // auto expose
    set_value(value_mask, XCB_CW_BACKING_STORE, value_list,
	      XCB_BACKING_STORE_ALWAYS);

    // used for popups
    set_value(value_mask, XCB_CW_SAVE_UNDER, value_list, 1);
    
    set_value(value_mask, XCB_CW_BACK_PIXEL, value_list, b->white_pixel);
    
    set_value(value_mask, XCB_CW_BORDER_PIXEL, value_list, b->black_pixel);
    
    set_value(value_mask, XCB_CW_OVERRIDE_REDIRECT, value_list, 0);

    if (b->b.opengl && b->b.use_opengl) {
#ifdef HAVE_OPENGL
	XVisualInfo *vinfo;

	vinfo = glXGetVisualFromFBConfig(b->display, b->fbconfigs[0]);
	attr.border_pixel = 0;
	// FIXME save in be - need to release colormap / resuse!?
	attr.colormap = XCreateColormap(b->display,
					RootWindow(b->display, vinfo->screen),
					vinfo->visual, AllocNone);
	valuemask |= XCB_CW_COLORMAP;
	
	win = xcb_generate_id(b->display);
	
	xcb_create_window(b->display,
			  XCB_COPY_FROM_PARENT, // vinfo->depth, /* depth */
			  win,
			  b->screen->root,
			  ewin->area.xy.x,      // x
			  ewin->area.xy.y,	// y
			  ewin->area.wh.width,	// width
			  ewin->area.wh.height,	// height
			  2,		        // border
			  XCB_WINDOW_CLASS_INPUT_OUTPUT, // class
			  b->screen->root_visual, // vinfo->visual, // visual
			  value_mask,	        // valuemask 
			  value_list);	        // attributes 
	if (win) {
	    /* w->context = glXCreateNewContext(b->display,
						be->fbconfigs[0], 
						GLX_RGBA_TYPE,
						NULL, True ); */
	    w->context = glXCreateContext(b->display, vinfo, NULL, True);
	    if (w->context)
		ewin->opengl = 1;
	}
#endif
    }

    if (!win) {
	win = xcb_generate_id(b->display);
	
	xcb_create_window(b->display,
			  XCB_COPY_FROM_PARENT, // vinfo->depth, /* depth */
			  win,
			  b->screen->root,
			  ewin->area.xy.x,      // x
			  ewin->area.xy.y,	// y
			  ewin->area.wh.width,	// width
			  ewin->area.wh.height,	// height
			  2,		        // border
			  XCB_WINDOW_CLASS_INPUT_OUTPUT, // class
			  b->screen->root_visual, // be->vinfo, // visual
			  value_mask,
			  value_list);
    }

    if (win) {
	// XEvent event;
	// XGCValues values;
	
	w->grabbed = 0;
	w->accel_num = 0;
	w->accel_den = 0;
	w->thres = 0;
	w->window = win;
	w->x = ewin->area.xy.x;
	w->y = ewin->area.xy.y;	
	w->width = ewin->area.wh.width;
	w->height = ewin->area.wh.height;

	w->gc = xcb_generate_id(b->display);
	xcb_create_gc(b->display, w->gc, win, 0, NULL);

	value_mask = XCB_GC_FOREGROUND|XCB_GC_BACKGROUND;
	set_value(value_mask, XCB_GC_FOREGROUND, value_list,
		  b->black_pixel);
	set_value(value_mask, XCB_GC_BACKGROUND, value_list,
		  b->white_pixel);
	
	xcb_change_gc(b->display, w->gc, value_mask, value_list);
	
	init_off_screen(w);
	create_off_screen(b, w);
	
	// XSetWMProtocols(be->display, win, &be->wm_delete_window, 1);

	xcb_map_window(b->display, win);

	// fixme loop until MAP_NOTIFY on current window
	// XIfEvent(be->display, &event, WaitForNotify, (XPointer) win );
	xcb_flush(b->display);
	epx_object_link(&backend->window_list, ewin);
	ewin->opaque  = (void*) w;
	ewin->backend = (epx_backend_t*) b;
	return 0;
    }
    free(w);
    return -1;
}

static int xcb_win_detach(epx_backend_t* backend, epx_window_t* ewin)
{
    XCBBackend* be = (XCBBackend*) backend;
    XCBWindow*  xwin = (XCBWindow*) ewin->opaque;
    
    if ((xwin != NULL) && (xwin->window != 0)) {
	EPX_DBGFMT("xcb_unmap_window");
	xcb_unmap_window(be->display, xwin->window);
	destroy_off_screen(be, xwin);
	if (ewin->opengl) {
#ifdef HAVE_OPENGL
	    glXDestroyContext(be->display, xwin->context);
	    if (be->fbconfigs != NULL)
		XFree(be->fbconfigs);
#endif
	}
	xcb_free_gc(be->display, xwin->gc);
	xcb_destroy_window(be->display, xwin->window);
	xcb_flush(be->display);
	/* Ungrabb? */
	free(xwin);
	epx_object_unlink(&backend->window_list, ewin);
	ewin->opaque  = NULL;
	ewin->backend = NULL;
	return 0;
    }
    return -1;
}

// Button4
#ifndef XCB_KEY_BUT_MASK_WHEEL_UP
#define XCB_KEY_BUT_MASK_WHEEL_UP XCB_KEY_BUT_MASK_BUTTON_4
#endif

// Button5
#ifndef XCB_KEY_BUT_MASK_WHEEL_DOWN
#define XCB_KEY_BUT_MASK_WHEEL_DOWN XCB_KEY_BUT_MASK_BUTTON_5
#endif

#ifndef XCB_KEY_BUT_MASK_BUTTON_6
#define XCB_KEY_BUT_MASK_BUTTON_6 (1 << 13)
#endif

#ifndef XCB_KEY_BUT_MASK_BUTTON_7
#define XCB_KEY_BUT_MASK_BUTTON_7 (1 << 14)
#endif

#ifndef XCB_KEY_BUT_MASK_WHEEL_LEFT
#define XCB_KEY_BUT_MASK_WHEEL_LEFT XCB_KEY_BUT_MASK_BUTTON_6
#endif

#ifndef XCB_KEY_BUT_MASK_WHEEL_RIGHT
#define XCB_KEY_BUT_MASK_WHEEL_RIGHT XCB_KEY_BUT_MASK_BUTTON_7
#endif

// Map button state to epx_event_t button
static uint32_t to_epx_buttons(uint32_t state)
{
    u_int32_t button = 0;
    if (state & XCB_KEY_BUT_MASK_BUTTON_1)    button |= EPX_BUTTON_LEFT;
    if (state & XCB_KEY_BUT_MASK_BUTTON_2)    button |= EPX_BUTTON_MIDDLE;
    if (state & XCB_KEY_BUT_MASK_BUTTON_3)    button |= EPX_BUTTON_RIGHT;
    if (state & XCB_KEY_BUT_MASK_WHEEL_UP)    button |= EPX_WHEEL_UP;
    if (state & XCB_KEY_BUT_MASK_WHEEL_DOWN)  button |= EPX_WHEEL_DOWN;
    if (state & XCB_KEY_BUT_MASK_WHEEL_LEFT)  button |= EPX_WHEEL_LEFT;
    if (state & XCB_KEY_BUT_MASK_WHEEL_RIGHT) button |= EPX_WHEEL_RIGHT;
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

static int xcb_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    XCBBackend* b = (XCBBackend*) backend;
    xcb_generic_event_t *ev;
    uint8_t ev_type;
    int group;
	
    backend->pending = 0;
    
    if ((ev = b->pending_event) != NULL) {
	b->pending_event = NULL;
	goto again;
    }
    
    if ((ev = xcb_poll_for_event(b->display)) == NULL)
	return -1;
again:
    switch ((ev_type=(ev->response_type & ~0x80))) {
    case XCB_CREATE_NOTIFY:
	EPX_DBGFMT("Event: CreateNotify");
	break;
    case XCB_DESTROY_NOTIFY: {
	xcb_destroy_notify_event_t* xev = (xcb_destroy_notify_event_t*) ev;
	EPX_DBGFMT("Event: DestroyNotify");
	e->window = find_event_window(b, xev->window);
	e->type = EPX_EVENT_DESTROYED;
	goto got_event;
    }

    case XCB_CLIENT_MESSAGE: {
	xcb_client_message_event_t* xev = (xcb_client_message_event_t*) ev;
	EPX_DBGFMT("Event: ClientMessage");
	e->window = find_event_window(b, xev->window);
	if ((xev->data.data32[0] != b->wm_delete_window))
	    break;
	EPX_DBGFMT("Event: WM_DELETE_WINDOW");
	e->type = EPX_EVENT_CLOSE;
	goto got_event;
    }

    case XCB_CONFIGURE_NOTIFY: {
	xcb_configure_notify_event_t* xev = (xcb_configure_notify_event_t*) ev;
	epx_window_t* w;
	EPX_DBGFMT("Event: ConfigureNotity x=%d, y=%d, w=%d, h=%d",
		   xev->x,
		   xev->y,
		   xev->width,
		   xev->height);
	if ((w = find_event_window(b, xev->event)) != NULL) {
	    XCBWindow*  win  = (XCBWindow*) w->opaque;	    
	    e->window = w;
	    e->type = EPX_EVENT_CONFIGURE;
	    e->area.x = xev->x;
	    e->area.y = xev->y;
	    e->area.w = xev->width;
	    e->area.h = xev->height;
	    // update reported values
	    w->rarea.xy.y = e->area.x;
	    w->rarea.xy.y = e->area.y;
	    w->rarea.wh.width = e->area.w;
	    w->rarea.wh.height = e->area.h;

	    // update using
	    win->width  = xev->width;
	    win->height = xev->height;
	    resize_off_screen(b, win);
	    goto got_event;
	}
	break;
    }

    case XCB_RESIZE_REQUEST: {
	xcb_resize_request_event_t* xev = (xcb_resize_request_event_t*) ev;
	epx_window_t* w;
	EPX_DBGFMT("Event: ResizeRequest w=%d, h=%d",
		   xev->width, xev->height);
	w = find_event_window(b, xev->window);
	if (w != NULL) {
	    XCBWindow*  win  = (XCBWindow*) w->opaque;
	    e->window = w;
	    e->type = EPX_EVENT_RESIZE;
	    e->dimension.w = xev->width;
	    e->dimension.h = xev->height;
	    e->dimension.d = 0;
	    // update reported values
	    w->rarea.wh.width = e->dimension.w;
	    w->rarea.wh.height = e->dimension.h;
	    // update using
	    win->width  = xev->width;
	    win->height = xev->height;
	    resize_off_screen(b, win);
	    goto got_event;
	}
	break;
    }

    case XCB_KEYMAP_NOTIFY: {
	// xcb_keymap_notify_event_t* xev = (xcb_keymap_notify_event_t*) ev;
	EPX_DBGFMT("Event: KeymapNotify");
	break;
    }
	
    case XCB_EXPOSE:  {
	xcb_expose_event_t* xev = (xcb_expose_event_t*) ev;
	epx_window_t* w;
	EPX_DBGFMT("Event: Expose x=%d, y=%d, w=%d, h=%d cnt=%d",
		   xev->x, xev->y, xev->width, xev->height,
		   xev->count);
	if ((w = find_event_window(b, xev->window)) != NULL) {
	    XCBWindow* win  = (XCBWindow*) w->opaque;
	    epx_rect_t dirty;
	    
	    epx_rect_set(&dirty, xev->x, xev->y, xev->width, xev->height);
	    epx_rect_union(&win->dirty, &dirty, &win->dirty);

	    if (xev->count > 0)
		break;
	    if (b->use_off_screen && win->pm && !b->use_exposure) {
		xcb_copy_area(b->display, win->pm, win->window, win->gc,
			      win->dirty.xy.x, win->dirty.xy.y,
			      win->dirty.xy.x, win->dirty.xy.y,
			      win->dirty.wh.width, win->dirty.wh.height);
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

    case XCB_GRAPHICS_EXPOSURE: {
	// xcb_graphics_exposure_event_t* xev =
	//  (xcb_graphics_exposure_event_t*)ev;
	EPX_DBGFMT("Event: GraphicsExposure");
	break;
    }
	
    case XCB_NO_EXPOSURE: {
	// xcb_no_exposure_event_t* xev = (xcb_no_exposure_event_t*) ev;
	// EPX_DBGFMT("Event: NoExposure");
	break;
    }

    case XCB_VISIBILITY_NOTIFY: {
	//xcb_visibility_notify_event_t* xev=(xcb_visibility_notify_event_t*)ev
	EPX_DBGFMT("Event: VisibilityNotify");
	break;
    }
	
    case XCB_UNMAP_NOTIFY: {
	// xcb_unmap_notify_event_t* xev = (xcb_unmap_notify_event_t*) ev;
	EPX_DBGFMT("Event: UnmapNotify");
	break;
    }
	
    case XCB_MAP_NOTIFY: {
	// xcb_map_notify_event_t* xev = (xcb_map_notify_event_t*) ev;
	EPX_DBGFMT("Event: MapNotify");
	break;
    }
	
    case XCB_MAP_REQUEST: {
	// xcb_map_request_event_t* xev = (xcb_map_request_event_t*) ev;
	EPX_DBGFMT("Event: MapRequest");
	break;
    }
	
    case XCB_REPARENT_NOTIFY: {
	// xcb_reparent_notify_event_t* xev = (xcb_reparent_notify_event_t*) ev;
	EPX_DBGFMT("Event: ReparentNotify");
	break;
    }
	
    case XCB_CONFIGURE_REQUEST: {
	// xcb_configure_notify_event_t* xev=(xcb_configure_notify_event_t*)ev;
	EPX_DBGFMT("Event: ConfigureRequest");
	break;
    }
	
    case XCB_GRAVITY_NOTIFY: {
	// xcb_gravity_notify_event_t* xev=(xcb_gravity_notify_event_t*)ev;
	EPX_DBGFMT("Event: GravityNotify");
	break;
    }

    case XCB_CIRCULATE_NOTIFY:
	EPX_DBGFMT("Event: CirculateNotify");
	break;
    case XCB_CIRCULATE_REQUEST:
	EPX_DBGFMT("Event: CirculateRequest");
	break;
	
    case XCB_PROPERTY_NOTIFY: {
	xcb_get_atom_name_cookie_t req;	
	xcb_get_atom_name_reply_t *rep;
	xcb_property_notify_event_t* xev=(xcb_property_notify_event_t*)ev;
	
	req = xcb_get_atom_name(b->display, xev->atom);
	rep = xcb_get_atom_name_reply(b->display, req, NULL);
	
	
	EPX_DBGFMT("Event: PropertyNotify %s", xcb_get_atom_name_name(rep));
	free(rep);
	break;
    }
	
    case XCB_SELECTION_CLEAR:
	EPX_DBGFMT("Event: SelectionClear");
	break;
	
    case XCB_SELECTION_REQUEST:
	EPX_DBGFMT("Event: SelectionRequest");
	break;
	
    case XCB_SELECTION_NOTIFY:
	EPX_DBGFMT("Event: SelectionNotify");
	break;
	
    case XCB_COLORMAP_NOTIFY:
	EPX_DBGFMT("Event: ColormapNotify");
	break;
	
    case XCB_MAPPING_NOTIFY:
	EPX_DBGFMT("Event: MappingNotify");
	break;
	
    case XCB_ENTER_NOTIFY: {
	xcb_enter_notify_event_t* xev = (xcb_enter_notify_event_t*) ev;
	EPX_DBGFMT("Event: EnterNotity");
	e->window = find_event_window(b, xev->event);
	e->type = EPX_EVENT_ENTER;
	e->pointer.x = xev->event_x;
	e->pointer.y = xev->event_y;
	e->pointer.z = 0;
	goto got_event;
    }
	
    case XCB_LEAVE_NOTIFY: {
	xcb_leave_notify_event_t* xev = (xcb_leave_notify_event_t*) ev;
	EPX_DBGFMT("Event: XCB_LEAVE_NOTITY");
	e->window = find_event_window(b, xev->event);
	e->type = EPX_EVENT_LEAVE;
	e->pointer.x = xev->event_x;
	e->pointer.y = xev->event_y;
	e->pointer.z = 0;
	goto got_event;
    }

    case XCB_FOCUS_IN: {
	xcb_focus_in_event_t* xev = (xcb_focus_in_event_t*) ev;
	EPX_DBGFMT("Event: FocusIn");
	e->window = find_event_window(b, xev->event);
	e->type = EPX_EVENT_FOCUS_IN;
	goto got_event;
    }

    case XCB_FOCUS_OUT: {
	xcb_focus_out_event_t* xev = (xcb_focus_out_event_t*) ev;
	EPX_DBGFMT("Event: FocusOut");
	e->window = find_event_window(b, xev->event);
	e->type = EPX_EVENT_FOCUS_OUT;
	goto got_event;
    }

    case XCB_MOTION_NOTIFY: {
	xcb_motion_notify_event_t* xev = (xcb_motion_notify_event_t*) ev;
	e->window = find_event_window(b, xev->event);
	e->pointer.button = to_epx_buttons(xev->state);
	e->type = EPX_EVENT_POINTER_MOTION;
	e->pointer.x = xev->event_x;
	e->pointer.y = xev->event_y;
	e->pointer.z = 0;
	EPX_DBGFMT("Event: XCB_MOTION_NOTIFY %d,%d",
		   e->pointer.x, e->pointer.y);
	goto got_event;
    }

    case XCB_BUTTON_PRESS: {
	xcb_button_press_event_t* xev = (xcb_button_press_event_t*) ev;
	e->window = find_event_window(b, xev->event);
	e->pointer.button = to_epx_buttons(1 << (xev->detail + 7));
	e->type = EPX_EVENT_BUTTON_PRESS;
	e->pointer.x = xev->event_x;
	e->pointer.y = xev->event_y;
	e->pointer.z = 0;
	EPX_DBGFMT("Event: XCB_BUTTON_PRESS %d x=%d,y=%d",
		   e->pointer.button, e->pointer.x, e->pointer.y);
	goto got_event;
    }
	
    case XCB_BUTTON_RELEASE: {
	xcb_button_release_event_t* xev = (xcb_button_release_event_t*) ev;
	e->window = find_event_window(b, xev->event);
	e->pointer.button = to_epx_buttons(1 << (xev->detail + 7));
	e->type = EPX_EVENT_BUTTON_RELEASE;
	e->pointer.x = xev->event_x;
	e->pointer.y = xev->event_y;
	e->pointer.z = 0;
	EPX_DBGFMT("Event: XCB_BUTTON_RELEASE %d x=%d,y=%d",
		   e->pointer.button, e->pointer.x, e->pointer.y);	
	goto got_event;
    }

    case XCB_KEY_PRESS:
    case XCB_KEY_RELEASE: {
	// xcb_key_release_event_t and xcb_key_press_event_t are the same
	xcb_key_press_event_t* xev = (xcb_key_press_event_t*) ev;
	xcb_keysym_t sym = keycode_to_keysym(b, xev->detail, 0);

	if (sym == XCB_NO_SYMBOL) {
	    EPX_DBGFMT("event sym=XCB_NO_SYMBOL");
	    goto ignore_event;
	}
	
	e->window = find_event_window(b, xev->event);

	if (e->window) {
	    XCBWindow* win  = (XCBWindow*) e->window->opaque;
	    if (e->window->mask & EPX_EVENT_NO_AUTO_REPEAT) {
		// remove retriggered events
		if ((win->last_key_time == xev->time) &&
		    (win->last_key_code == xev->detail))
		    goto ignore_event;
	    }
	    win->last_key_time = xev->time;
	    win->last_key_code = xev->detail;
	}
	
	if (ev_type == XCB_KEY_PRESS)
	    e->type = EPX_EVENT_KEY_PRESS;
	else if (ev_type == XCB_KEY_RELEASE)
	    e->type = EPX_EVENT_KEY_RELEASE;
	else
	    break;

	/* calculate kbd modifiers*/
	if (b->modstate) {
	    EPX_DBGFMT("1:modstate=%x", b->modstate);
	}
	b->modstate &= (EPX_KBD_MOD_NUM|EPX_KBD_MOD_CAPS|EPX_KBD_MOD_SCR);
	if (xev->state) {
	    EPX_DBGFMT("key.state=%x", xev->state);
	}
	if (xev->state & XCB_KEY_BUT_MASK_LOCK) {
	    EPX_DBGFMT("lockmask (caps)");
	    b->modstate |= EPX_KBD_MOD_CAPS;
	}
	if (xev->state & XCB_KEY_BUT_MASK_MOD_2) {
	    EPX_DBGFMT("mod2mask (num)");
	    b->modstate |= EPX_KBD_MOD_NUM;
	}
	if (xev->state & XCB_KEY_BUT_MASK_MOD_3) {
	    EPX_DBGFMT("mod3mask");
	}
	if (xev->state & XCB_KEY_BUT_MASK_MOD_4) {
	    EPX_DBGFMT("mod4mask");
	}
	if (xev->state & XCB_KEY_BUT_MASK_MOD_5) {
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
	    if (ev_type == XCB_KEY_RELEASE)
		b->modstate ^= EPX_KBD_MOD_NUM;
	    goto no_key;
	case XK_Shift_Lock:
	case XK_Caps_Lock:
	    /* not sent, used only for state*/
	    if (ev_type == XCB_KEY_RELEASE)
		b->modstate ^= EPX_KBD_MOD_CAPS;
	    goto no_key;
	case XK_Scroll_Lock:
	    /* not sent, used only for state*/
	    if (ev_type == XCB_KEY_RELEASE)
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
		    EPX_DBGFMT("Unhandled XCB keysym: %04x\n", (int)sym);
		}
	    }
	    group = (xev->state & XCB_MOD_MASK_5) ? 4 : 0;
	    group += (xev->state & XCB_MOD_MASK_CONTROL) ? 2 : 0;
	    group += (((xev->state & XCB_MOD_MASK_LOCK)!=0) !=
		      ((xev->state & XCB_MOD_MASK_SHIFT)!=0));

	    printf("modstate = %x, state = %x group=%d keysyms_per_keycode=%d\r\n",
		   b->modstate, xev->state, group,
		   b->keymap->keysyms_per_keycode);
	    
	    sym = keycode_to_keysym(b, xev->detail, group);

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
	    xcb_grab(backend,e->window,(xev->state&XCB_KEY_BUT_MASK_CONTROL));
	e->key.code = xev->detail;
	e->key.mod  = b->modstate;
	goto got_event;
	no_key:
	EPX_DBGFMT("3:modstate=%x", b->modstate);
	goto ignore_event;
    }

    default:
	EPX_DBGFMT("Event: Ignore event=%d", ev_type);
	goto ignore_event;
    }

ignore_event:
    free(ev);
    if ((ev = xcb_poll_for_queued_event(b->display)) != NULL)
	goto again;
    return 0;

got_event:
    if (FilterEvent(e)) goto ignore_event;
    free(ev);
    
    b->pending_event = xcb_poll_for_queued_event(b->display);
    backend->pending = (b->pending_event != NULL);
    return 1+backend->pending;
}

static void xcb_grab(epx_backend_t* backend, epx_window_t* wptr, int toggle)
{
    XCBBackend* b = (XCBBackend*) backend;
    XCBWindow* w = (XCBWindow*) wptr;

    if (w == NULL)
	return;

    if (toggle) {
	/* toggle grab control */
	if (w->grabbed) {
	    xcb_ungrab_pointer(b->display, XCB_TIME_CURRENT_TIME);
	    xcb_ungrab_keyboard(b->display, XCB_TIME_CURRENT_TIME);
	    xcb_change_pointer_control(b->display,
				       w->accel_num,
				       w->accel_den,
				       0, 
				       True, // do_acceleration
				       False  // do_threshol
		);
	    w->grabbed = 0;
	}
	else {
	    xcb_get_pointer_control_cookie_t req;
	    xcb_get_pointer_control_reply_t* rep;
	    
	    /* save pointer config */
	    req = xcb_get_pointer_control(b->display);
	    rep = xcb_get_pointer_control_reply(b->display, req, NULL);
		
	    w->accel_num = rep->acceleration_numerator;
	    w->accel_den = rep->acceleration_denominator;
	    w->thres = rep->threshold;
	    free(rep);

	    xcb_change_pointer_control(b->display,
				       1, 1, 0,
				       True, False);
	    
	    xcb_grab_keyboard(b->display, w->window,
			      True,  /* only to this window */
			      GrabModeAsync, GrabModeAsync, CurrentTime);
	    xcb_grab_pointer(b->display, w->window, False,
			 PointerMotionMask | ButtonPressMask,
			 GrabModeAsync, GrabModeAsync, None, None,
			 CurrentTime);
	    w->grabbed = 1;
	}
    }
    else if (w->grabbed)
	xcb_change_pointer_control(b->display,
				   w->accel_num, w->accel_den, 0,
				   True, False);
				   
}

static int xcb_adjust(epx_backend_t* backend, epx_dict_t* param)
{
    XCBBackend* b = (XCBBackend*) backend;    
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

static int xcb_info(epx_backend_t* backend, epx_dict_t*param)
{
    XCBBackend* b  = (XCBBackend*) backend;
    int bval;
    
    if (epx_dict_lookup_boolean(param, "display", &bval) >= 0) {
	// return the low-level Window
	epx_dict_set_binary(param, "display",
			    &b->display, sizeof(xcb_connection_t*));
    }
    if (epx_dict_lookup_boolean(param, "visual", &bval) >= 0) {
	epx_dict_set_binary(param, "visual",
			    &b->visual, sizeof(xcb_visualid_t));
    }
    return 1;    
}

typedef enum {
    XCB_SIZE_HINT_US_POSITION = 1 << 0,
    XCB_SIZE_HINT_US_SIZE = 1 << 1,
    XCB_SIZE_HINT_P_POSITION = 1 << 2,
    XCB_SIZE_HINT_P_SIZE = 1 << 3,
    XCB_SIZE_HINT_P_MIN_SIZE = 1 << 4,
    XCB_SIZE_HINT_P_MAX_SIZE = 1 << 5,
    XCB_SIZE_HINT_P_RESIZE_INC = 1 << 6,
    XCB_SIZE_HINT_P_ASPECT = 1 << 7,
    XCB_SIZE_HINT_BASE_SIZE = 1 << 8,
    XCB_SIZE_HINT_P_WIN_GRAVITY = 1 << 9
} xcb_size_hints_flags_t;

typedef struct xcb_size_hints_t {
    uint32_t flags;
    int32_t x, y;
    int32_t width, height;
    int32_t min_width, min_height;
    int32_t max_width, max_height;
    int32_t width_inc, height_inc;
    int32_t min_aspect_num, min_aspect_den;
    int32_t max_aspect_num, max_aspect_den;
    int32_t base_width, base_height;
    uint32_t win_gravity;
} xcb_size_hints_t;

static int xcb_win_adjust(epx_window_t *win, epx_dict_t*param)
{
    int bool_val;
    char* window_name = NULL;
    char* icon_name = NULL;
    size_t name_len;
    XCBWindow*  w  = (XCBWindow*) win->opaque;
    XCBBackend* b  = (XCBBackend*) win->backend;
    int value_x, value_y, value_w, value_h, value_bw;
    uint32_t value_mask = 0;
    uint32_t value_list[32];
    uint32_t sz_hint = 0;
    int min_h = -1, min_w = -1;
    int max_h = -1, max_w = -1;

    EPX_DBGFMT("xcb: Window adjust\n");

    if (epx_dict_lookup_string(param, "name", &window_name, &name_len) >= 0) {

	xcb_change_property(b->display,
			    XCB_PROP_MODE_REPLACE,
			    w->window,
			    XCB_ATOM_WM_NAME,
			    XCB_ATOM_STRING,
			    8,
			    name_len,
			    window_name);
    }

    if (epx_dict_lookup_string(param, "icon_name", &icon_name, &name_len) >= 0) {

	xcb_change_property(b->display,
			    XCB_PROP_MODE_REPLACE,
			    w->window,
			    XCB_ATOM_WM_ICON_NAME,
			    XCB_ATOM_STRING,
			    8,
			    name_len,
			    icon_name);
    }    

    if (epx_dict_lookup_integer(param, "min_width", &min_w) >= 0) {
	EPX_DBGFMT("xcb: min_width=%d", min_w);
	sz_hint |= XCB_SIZE_HINT_P_MIN_SIZE;
    }
    if (epx_dict_lookup_integer(param, "min_height", &min_h) >= 0) {
	EPX_DBGFMT("xcb: min_height=%d", min_h);
	sz_hint |= XCB_SIZE_HINT_P_MIN_SIZE;
    }
    if (epx_dict_lookup_integer(param, "max_width", &max_w) >= 0) {
	EPX_DBGFMT("xcb: max_width=%d", max_w);
	sz_hint |= XCB_SIZE_HINT_P_MAX_SIZE;
    }
    if (epx_dict_lookup_integer(param, "max_height", &max_h) >= 0) {
	EPX_DBGFMT("xcb: max_height=%d", max_h);
	sz_hint |= XCB_SIZE_HINT_P_MAX_SIZE;
    }

    if (sz_hint) {
	xcb_size_hints_t hints;
	memset(&hints, 0, sizeof(hints));
	hints.flags = sz_hint;
	hints.min_width  =
	    (min_w < 0) ? win->area.wh.width : (unsigned int) min_w;
	hints.min_height =
	    (min_h < 0) ? win->area.wh.height : (unsigned int) min_h;
	hints.max_width  =
	    (max_w < 0) ? win->area.wh.width :  (unsigned int) max_w;
	hints.max_height =
	    (max_h < 0) ? win->area.wh.height : (unsigned int) max_h;
	xcb_change_property(b->display,
			    XCB_PROP_MODE_REPLACE,
			    w->window,
			    XCB_ATOM_WM_NORMAL_HINTS,
			    XCB_ATOM_WM_SIZE_HINTS,
			    32,
			    sizeof(hints) >> 2,
			    &hints);
    }

    value_mask = 0;

    if (epx_dict_lookup_integer(param, "x", &value_x) >= 0) {
	EPX_DBGFMT("xcb: x=%d", value_x);
	value_mask |= XCB_CONFIG_WINDOW_X;
    }
    if (epx_dict_lookup_integer(param, "y", &value_y) >= 0) {
	EPX_DBGFMT("xcb: y=%d", value_y);
	value_mask |= XCB_CONFIG_WINDOW_Y;
    }
    if (epx_dict_lookup_integer(param, "width", &value_w) >= 0) {
	EPX_DBGFMT("xcb: width=%d", value_w);
	value_mask |= XCB_CONFIG_WINDOW_WIDTH;
    }
    if (epx_dict_lookup_integer(param, "height", &value_h) >= 0) {
	EPX_DBGFMT("xcb: height=%d", value_h);
	value_mask |= XCB_CONFIG_WINDOW_HEIGHT;
    }
    if (epx_dict_lookup_integer(param, "border_width", &value_bw)
	>= 0) {
	EPX_DBGFMT("xcb: border_width=%d", value_bw);
	value_mask |= XCB_CONFIG_WINDOW_BORDER_WIDTH;
    }
    
    if (value_mask) {
	uint32_t value_mask_orig = value_mask;
	// check if values already are set/reported
	if (value_x == win->rarea.xy.x)
	    value_mask &= ~XCB_CONFIG_WINDOW_X;
	if (value_y == win->rarea.xy.y)
	    value_mask &= ~XCB_CONFIG_WINDOW_Y;
	if (value_w == (int)win->rarea.wh.width)
	    value_mask &= ~XCB_CONFIG_WINDOW_WIDTH;
	if (value_h == (int)win->rarea.wh.height)
	    value_mask &= ~XCB_CONFIG_WINDOW_HEIGHT;
	// cache values
	if (value_mask_orig & XCB_CONFIG_WINDOW_X)
	    win->area.xy.x = w->x = value_x;
	if (value_mask_orig & XCB_CONFIG_WINDOW_Y)
	    win->area.xy.y = w->y = value_y;
	if (value_mask_orig & XCB_CONFIG_WINDOW_WIDTH)
	    win->area.wh.width  = w->width = value_w;
	if (value_mask_orig & XCB_CONFIG_WINDOW_HEIGHT)
	    win->area.wh.height = w->height = value_h;

	set_value(value_mask, XCB_CONFIG_WINDOW_X, value_list, value_x);
	set_value(value_mask, XCB_CONFIG_WINDOW_Y, value_list, value_y);
	set_value(value_mask, XCB_CONFIG_WINDOW_WIDTH, value_list, value_w);
	set_value(value_mask, XCB_CONFIG_WINDOW_HEIGHT, value_list, value_h);
	set_value(value_mask,  XCB_CONFIG_WINDOW_BORDER_WIDTH,
		  value_list, value_bw);

	xcb_configure_window(b->display, w->window, value_mask, value_list);
    }

    if (epx_dict_lookup_boolean(param, "show", &bool_val) >= 0) {
	EPX_DBGFMT("xcb: show=%d", bool_val);
	if (bool_val)
	    xcb_map_window(b->display, w->window);
	else
	    xcb_unmap_window(b->display, w->window);
	xcb_flush(b->display);	
    }

    if (epx_dict_lookup_boolean(param, "select", &bool_val) >= 0) {
	EPX_DBGFMT("xcb: select=%d", bool_val);	
	if (bool_val) {
	    value_mask = XCB_CONFIG_WINDOW_STACK_MODE;
	    value_list[0] = XCB_STACK_MODE_ABOVE;

	    xcb_configure_window(b->display, w->window,
				 value_mask, value_list);
	    xcb_flush(b->display);
	}
    }
    //  "focus", "modal" ...
    return 1;    
}

// Query interface, send boolean entries and return the
// queried for value

static int xcb_win_info(epx_window_t* win, epx_dict_t* param)
{
    int bval;
    XCBWindow*  w  = (XCBWindow*) win->opaque;
    XCBBackend* b  = (XCBBackend*) win->backend;
    
    if (epx_dict_lookup_boolean(param, "name", &bval) >= 0) {
	xcb_get_property_cookie_t cookie;
	xcb_get_property_reply_t *reply;

	cookie = xcb_get_property(b->display, 0,
				  w->window,
				  XCB_ATOM_WM_NAME,
				  XCB_ATOM_STRING,
				  0, 0);
	if ((reply = xcb_get_property_reply(b->display, cookie, NULL))) {
	    int len = xcb_get_property_value_length(reply);

	    if (len == 0)
		epx_dict_set_string(param, "name", "?");
		else
		    epx_dict_set_string(param, "name",
					(char*)xcb_get_property_value(reply));
	    free(reply);
	}
    }

    if (epx_dict_lookup_boolean(param, "display", &bval) >= 0) {
	epx_dict_set_binary(param, "display",
			    &b->display, sizeof(xcb_connection_t*));
    }
    
    if (epx_dict_lookup_boolean(param, "window", &bval) >= 0) {
	epx_dict_set_binary(param, "window",
			    &w->window, sizeof(xcb_window_t*));
    }
    
    if (epx_dict_lookup_boolean(param, "visual", &bval) >= 0) {
	epx_dict_set_binary(param, "visual",
			    &b->visual, sizeof(xcb_visualid_t));
    }    

    return 1;
}
