/***************************************************************************
 *
 * Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
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
// EPX nif interface
//

#include <string.h>
#include <unistd.h>  // WINDOWS fixme
#include <poll.h>    // WINDOWS fixme
#include <errno.h>
#include <math.h>

#define NIF_ERLANG   // use nif logging with %T
#include "erl_nif.h"
#include "../include/epx.h"

// #define NIF_TRACE

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif


#define MAX_PATH 1024

static int epx_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int epx_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
			 ERL_NIF_TERM load_info);
static void epx_unload(ErlNifEnv* env, void* priv_data);

#define NIF_LIST \
    NIF("debug",         1,  debug)				\
    NIF("simd_info_",    1,  simd_info)				\
    NIF("simd_set",      1,  simd_set )				\
    NIF("pixmap_create", 3, pixmap_create)			\
    NIF("pixmap_copy",   1, pixmap_copy)			\
    NIF("pixmap_info_",  2, pixmap_info)			\
    NIF("pixmap_sub_pixmap", 5, pixmap_sub_pixmap)		\
    NIF("pixmap_set_clip", 2, pixmap_set_clip)			\
    NIF("pixmap_fill", 3, pixmap_fill)				\
    NIF("pixmap_fill_area", 7, pixmap_fill_area)		\
    NIF("pixmap_copy_to", 2, pixmap_copy_to)			\
    NIF("pixmap_flip", 1, pixmap_flip)				\
    NIF("pixmap_scale", 4, pixmap_scale)			\
    NIF("pixmap_scale_area", 11, pixmap_scale_area)		\
    NIF("pixmap_put_pixel", 5, pixmap_put_pixel)		\
    NIF("pixmap_put_pixels", 8, pixmap_put_pixels)		\
    NIF("pixmap_get_pixel", 3, pixmap_get_pixel)		\
    NIF("pixmap_interp_pixel", 3, pixmap_interp_pixel)		\
    NIF("pixmap_get_pixels", 5, pixmap_get_pixels)		\
    NIF("pixmap_copy_area", 9, pixmap_copy_area)		\
    NIF("pixmap_alpha_area", 9, pixmap_alpha_area)		\
    NIF("pixmap_fade_area", 9, pixmap_fade_area)		\
    NIF("pixmap_shadow_area", 9, pixmap_shadow_area)		\
    NIF("pixmap_operation_area", 9, pixmap_operation_area)	\
    NIF("pixmap_add_color_area", 11, pixmap_add_color_area)	\
    NIF("pixmap_filter_area", 10, pixmap_filter_area)		\
    NIF("pixmap_rotate_area", 12, pixmap_rotate_area)		\
    NIF("pixmap_scroll", 6, pixmap_scroll)			\
    NIF("pixmap_attach", 2, pixmap_attach)			\
    NIF("pixmap_detach", 1, pixmap_detach)			\
    NIF("pixmap_draw", 8, pixmap_draw)				\
    NIF("pixmap_draw_point", 4, pixmap_draw_point)		\
    NIF("pixmap_draw_line", 6, pixmap_draw_line)		\
    NIF("pixmap_draw_triangle", 8, pixmap_draw_triangle)	\
    NIF("pixmap_draw_triangles", 3, pixmap_draw_triangles)	\
    NIF("pixmap_draw_rectangle", 6, pixmap_draw_rectangle)	\
    NIF("pixmap_draw_fan", 4, pixmap_draw_fan)			\
    NIF("pixmap_draw_strip", 3, pixmap_draw_strip)		\
    NIF("pixmap_draw_ellipse", 6, pixmap_draw_ellipse)		\
    NIF("pixmap_draw_roundrect", 8, pixmap_draw_roundrect)	\
    NIF("pixmap_ltm_translate", 3, pixmap_ltm_translate)	\
    NIF("pixmap_ltm_scale", 3, pixmap_ltm_scale)		\
    NIF("pixmap_ltm_rotate", 2, pixmap_ltm_rotate)		\
    NIF("pixmap_ltm_reset", 1, pixmap_ltm_reset)		\
    NIF("bitmap_create", 2, bitmap_create)			\
    NIF("bitmap_put_bit", 4, bitmap_put_bit)			\
    NIF("bitmap_get_bit", 3, bitmap_get_bit)			\
    NIF("bitmap_get_bits", 5, bitmap_get_bits)			\
    NIF("bitmap_put_bits", 6, bitmap_put_bits)			\
    NIF("bitmap_scroll", 6, bitmap_scroll)			\
    NIF("bitmap_copy", 1, bitmap_copy)				\
    NIF("bitmap_copy_to", 2, bitmap_copy_to)			\
    NIF("bitmap_copy_area", 8, bitmap_copy_area)		\
    NIF("bitmap_info_",  2, bitmap_info)			\
    NIF("bitmap_set_clip", 2, bitmap_set_clip)			\
    NIF("bitmap_fill", 2, bitmap_fill)				\
    NIF("bitmap_fill_rectangle", 6, bitmap_fill_rectangle)	\
    NIF("bitmap_draw_rectangle", 6, bitmap_draw_rectangle)	\
    NIF("bitmap_draw_ellipse", 6, bitmap_draw_ellipse)		\
    NIF("bitmap_fill_ellipse", 6, bitmap_fill_ellipse)		\
    NIF("bitmap_draw", 10, bitmap_draw)				\
    NIF("dict_create", 0, dict_create)				\
    NIF("dict_copy",   1, dict_copy)				\
    NIF("dict_set",    3, dict_set)				\
    NIF("dict_get",    2, dict_get)				\
    NIF("dict_get_boolean", 2, dict_get_boolean)		\
    NIF("dict_get_integer", 2, dict_get_integer)		\
    NIF("dict_get_float", 2, dict_get_float)			\
    NIF("dict_get_string", 2, dict_get_string)			\
    NIF("dict_get_binary", 2, dict_get_binary)			\
    NIF("dict_is_key",     2, dict_is_key)			\
    NIF("dict_first",      1, dict_first)			\
    NIF("dict_next",       2, dict_next)			\
    NIF("dict_info_",      2, dict_info)			\
    NIF("gc_create", 0, gc_create)				\
    NIF("gc_copy", 1, gc_copy)					\
    NIF("gc_default", 0, gc_default)				\
    NIF("gc_set", 3, gc_set)					\
    NIF("gc_get", 2, gc_get)					\
    NIF("font_open", 1, font_open)				\
    NIF("font_load", 1, font_load)				\
    NIF("font_unload", 1, font_unload)				\
    NIF("font_map", 1, font_map)				\
    NIF("font_unmap", 1, font_unmap)				\
    NIF("font_info_", 2, font_info)				\
    NIF("glyph_info_", 3, glyph_info)				\
    NIF("font_draw_glyph", 5, font_draw_glyph)			\
    NIF("font_draw_string", 5, font_draw_string)		\
    NIF("font_draw_utf8", 5, font_draw_utf8)			\
    NIF("backend_list", 0, backend_list)			\
    NIF("backend_open_", 2, backend_open)			\
    NIF("backend_info_", 2, backend_info)			\
    NIF("backend_adjust_", 2, backend_adjust)			\
    NIF("window_create", 4, window_create)			\
    NIF("window_create", 5, window_create)			\
    NIF("window_adjust_", 2, window_adjust)			\
    NIF("window_info_",   2, window_info)			\
    NIF("window_attach", 2, window_attach)			\
    NIF("window_detach", 1, window_detach)			\
    NIF("window_set_event_mask", 2, window_set_event_mask)	\
    NIF("window_enable_events", 2, window_enable_events)	\
    NIF("window_disable_events", 2, window_disable_events)	\
    NIF("window_sync", 1, window_sync)				\
    NIF("animation_info_", 2, animation_info)			\
    NIF("animation_draw", 6, animation_draw)			\
    NIF("animation_copy", 6, animation_copy)			\
    NIF("animation_open", 1, animation_open)			\
    NIF("canvas_create", 0, canvas_create)			\
    NIF("canvas_line", 4, canvas_line)				\
    NIF("canvas_quad", 7, canvas_quad)				\
    NIF("canvas_and",  3, canvas_and)				\
    NIF("canvas_or",   3, canvas_or)				\
    NIF("canvas_over", 3, canvas_over)				\
    NIF("canvas_not",  2, canvas_not)				\
    NIF("canvas_set_color", 3, canvas_set_color)		\
    NIF("canvas_set_operation", 3, canvas_set_operation)	\
    NIF("canvas_set_params", 5, canvas_set_params)		\
    NIF("canvas_set_params", 8, canvas_set_params)		\
    NIF("canvas_draw", 2, canvas_draw)				\
    NIF("poly_create", 0, poly_create)				\
    NIF("poly_set",    2, poly_set)				\
    NIF("poly_set",    3, poly_set)				\
    NIF("poly_draw",   5, poly_draw)				\
    NIF("poly_info",   2, poly_info)				\
    NIF("poly_info",   3, poly_info)

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST


#ifdef __APPLE__
extern int erl_drv_stolen_main_thread_join(ErlNifTid tid, void **respp);
extern int erl_drv_steal_main_thread(char *name,
				     ErlNifTid *dtid,
				     void* (*func)(void*),
				     void* arg,
				     ErlDrvThreadOpts *opts);
#endif

#define DO_STEAL      1
#define DO_NOT_STEAL  0

#define DO_WAKEUP     1
#define DO_NOT_WAKEUP 0

#define STACK_SIZE_AUTO -1
#define STACK_SIZE_IN_KBYTES(k) (k)

typedef enum {
    EPX_MESSAGE_STOP,           // time to die
    EPX_MESSAGE_UPGRADE,        // upgrade thread
    EPX_MESSAGE_SYNC,           // sync thread
    EPX_MESSAGE_SYNC_ACK,       // sync acknowledge
    EPX_MESSAGE_KILL,           // thread to kill
    EPX_MESSAGE_POLL,           // poll a fd
    EPX_MESSAGE_EVENT_READY,    // event is ready to be read
    EPX_MESSAGE_WINDOW_ATTACH,  // backend main - attach a window
    EPX_MESSAGE_WINDOW_DETACH,  // backend main - detach a window
    EPX_MESSAGE_PIXMAP_ATTACH,  // backend main - attach a pixmap
    EPX_MESSAGE_PIXMAP_DETACH,  // backend main - detach a pixmap
    EPX_MESSAGE_PIXMAP_DRAW,    // backend main - draw pixmap (on window)
    EPX_MESSAGE_WINDOW_SWAP,    // backend main - swap window
    EPX_MESSAGE_WINDOW_ADJUST,  // backend main - set window parameters
    EPX_MESSAGE_BACKEND_ADJUST, // backend main - set backend parameters
    EPX_MESSAGE_WINDOW_SYNC,    // backend main - sync window
} epx_message_type_t;

struct _epx_thread_t;
struct _epx_queue_t;

typedef struct epx_message_t
{
    epx_message_type_t type;
    struct _epx_thread_t* sender;
    union {
	epx_window_t* window;  // window attach/detach
	epx_pixmap_t* pixmap;  // pixmap attach/detach
	struct _epx_thread_t* thread;  // thread kill
	epx_dict_t*   param;   // adjust arg
	EPX_HANDLE_T  handle;  // poll arg
	struct {
	    epx_window_t* window;  // window adjust
	    epx_dict_t*   param;   // window param
	} wadjust;
	struct {
	    epx_pixmap_t* pixmap;  // attached pixmap
	    epx_window_t* window;  // attached window
	    int src_x;
	    int src_y;
	    int dst_x;
	    int dst_y;
	    unsigned int width;
	    unsigned int height;
	} wdraw;
	void* (*upgrade)(void* arg);
	struct _epx_queue_t* reply_q;
    };
} epx_message_t;

typedef struct _epx_qlink_t {
    struct _epx_qlink_t* next;
    epx_message_t mesg;
} epx_qlink_t;

#define MAX_QLINK  8  // pre-allocated qlinks

typedef struct _epx_queue_t {
    ErlNifMutex*   mtx;
    ErlNifCond*    cv;
    int len;
    epx_qlink_t*   front;   // pick from front
    epx_qlink_t*   rear;    // insert at rear
    epx_qlink_t*   free;    // free list in ql
    epx_qlink_t  ql[MAX_QLINK];  // "pre" allocated qlinks
} epx_queue_t;

typedef struct _epx_thread_t {
    ErlNifTid   tid;     // thread id
    epx_queue_t q;       // message queue
    void*       arg;     // thread init argument
    int         stolen;  // if stolen main thread
    int         wake[2]; // wakeup pipe (very unix, fixme)
    void*       priv;    // private (upgrade) data
} epx_thread_t;

// Object wrapper
typedef struct _epx_nif_object_t {
    epx_object_t* epx_obj;          // object pointer
} epx_nif_object_t;

// Backend wrapper
typedef struct _epx_nif_backend_t {
    epx_backend_t* backend;           // real backend
    ErlNifPid      own;               // creating erlang process    
    struct _epx_nif_backend_t* next;  // upgrade list
    epx_thread_t*  main;              // main thread
} epx_nif_backend_t;

// Window wrapper
typedef struct _epx_nif_window_t {
    epx_window_t* window;    
    ErlNifPid     own;      // "owner" pid storage
} epx_nif_window_t;

// Pixmap wrapper
typedef struct _epx_nif_pixmap_t {
    epx_pixmap_t* pixmap;    
    ErlNifPid     own;      // "owner" pid storage
} epx_nif_pixmap_t;

// Wrapper to handle reource atom name etc.
typedef struct {
    char* name;
    ERL_NIF_TERM type;       // resource atom name
    ErlNifResourceType* res; // the resource type
} epx_resource_t;

typedef struct {
    int ref_count;
    epx_queue_t q;          // sync ack queue
    epx_thread_t* reaper;   // killer thread
    epx_gc_t* def_gc;
    ErlNifRWLock* backend_list_lock;
    epx_nif_backend_t* backend_list;
    int debug;  // current debug level
    int accel;  // current acceleration type
} epx_ctx_t;

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

ErlNifFunc epx_funcs[] =
{
    NIF_LIST
};

ErlNifResourceType* dict_res;
ErlNifResourceType* gc_res;
ErlNifResourceType* font_res;
ErlNifResourceType* backend_res;
ErlNifResourceType* animation_res;
ErlNifResourceType* canvas_res;
ErlNifResourceType* poly_res;
ErlNifResourceType* bitmap_res;
ErlNifResourceType* pixmap_res;
ErlNifResourceType* window_res;

// Atom macros
#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)			\
    atm_##name = enif_make_atom(env,string)

DECL_ATOM(ok);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(undefined);
DECL_ATOM(error);

DECL_ATOM(debug);
DECL_ATOM(info);
DECL_ATOM(notice);
DECL_ATOM(warning);
//DECL_ATOM(error);
DECL_ATOM(critical);
DECL_ATOM(alert);
DECL_ATOM(emergency);
DECL_ATOM(none);

// epx types
DECL_ATOM(epx_pixmap);
DECL_ATOM(epx_bitmap);
DECL_ATOM(epx_window);
DECL_ATOM(epx_backend);
DECL_ATOM(epx_dict);
DECL_ATOM(epx_gc);
DECL_ATOM(epx_font);
DECL_ATOM(epx_animation);
DECL_ATOM(epx_canvas);
DECL_ATOM(epx_poly);

// simd
DECL_ATOM(emu);
DECL_ATOM(altivec);
DECL_ATOM(mmx);
DECL_ATOM(sse2);
DECL_ATOM(avx2);
DECL_ATOM(neon);
DECL_ATOM(auto);
DECL_ATOM(accel);
DECL_ATOM(functions);
DECL_ATOM(cpu_features);
DECL_ATOM(cpu_vendor_name);
DECL_ATOM(cpu_brand_string);
DECL_ATOM(cpu_serial_number);
DECL_ATOM(cpu_cache_line_size);

// pixmap_info
DECL_ATOM(width);
DECL_ATOM(height);
DECL_ATOM(bytes_per_row);
DECL_ATOM(bits_per_pixel);
DECL_ATOM(bytes_per_pixel);
//DECL_ATOM(pixel_format);
//DECL_ATOM(epx_pixel_format);
DECL_ATOM(parent);
DECL_ATOM(clip);
DECL_ATOM(backend);

// animation_info
//DECL_ATOM(file_name);
//DECL_ATOM(file_size);
DECL_ATOM(count);
//DECL_ATOM(width);
//DECL_ATOM(height);
//DECL_ATOM(pixel_format);
//DECL_ATOM(epx_pixel_format);

// window_info
// DECL_ATOM(width);
// DECL_ATOM(height);
// DECL_ATOM(backend);
DECL_ATOM(x);
DECL_ATOM(y);
DECL_ATOM(dx);
DECL_ATOM(dy);
// DECL_ATOM(backend);
DECL_ATOM(event_mask);

// poly_info
//DECL_ATOM(count);
DECL_ATOM(box);
DECL_ATOM(edge);
DECL_ATOM(sedge);
DECL_ATOM(vertex);
DECL_ATOM(pos);
DECL_ATOM(slope);
// poly flags
DECL_ATOM(absolute);  // default
DECL_ATOM(relative);

// Flags
DECL_ATOM(solid);
DECL_ATOM(blend);
DECL_ATOM(sum);
DECL_ATOM(aalias);
DECL_ATOM(textured);
DECL_ATOM(nfirst);
DECL_ATOM(nlast);

DECL_ATOM(dashed);
DECL_ATOM(ntop);
DECL_ATOM(nright);
DECL_ATOM(nbottom);
DECL_ATOM(nleft);
DECL_ATOM(nborder);
DECL_ATOM(outside);
DECL_ATOM(inside);
DECL_ATOM(center);

// DECL_ATOM(none);
DECL_ATOM(butt);
DECL_ATOM(round);
DECL_ATOM(projecting);

DECL_ATOM(miter);
// DECL_ATOM(round);
DECL_ATOM(bevel);

// Gc items
DECL_ATOM(fill_style);
DECL_ATOM(fill_color);
DECL_ATOM(fill_texture);
DECL_ATOM(line_style);
DECL_ATOM(line_join_style);
DECL_ATOM(line_cap_style);
DECL_ATOM(line_width);
DECL_ATOM(line_texture);
DECL_ATOM(border_style);
DECL_ATOM(border_join_style);
DECL_ATOM(border_cap_style);
DECL_ATOM(border_color);
DECL_ATOM(border_width);
DECL_ATOM(border_texture);
DECL_ATOM(foreground_color);
DECL_ATOM(background_color);
DECL_ATOM(fader_value);
DECL_ATOM(font);
DECL_ATOM(glyph_delta_x);
DECL_ATOM(glyph_delta_y);
DECL_ATOM(glyph_fixed_width);
DECL_ATOM(glyph_dot_kern);

// Backend info
DECL_ATOM(name);
// DECL_ATOM(width);
// DECL_ATOM(height);
DECL_ATOM(pending);
DECL_ATOM(opengl);
DECL_ATOM(use_opengl);
DECL_ATOM(windows);
DECL_ATOM(pixmaps);
DECL_ATOM(pixel_formats);
DECL_ATOM(epx_pixel_formats);

// Dict info
DECL_ATOM(size);
DECL_ATOM(sorted);

// Font info
DECL_ATOM(file_name);
DECL_ATOM(file_size);
DECL_ATOM(foundry_name);
DECL_ATOM(family_name);
DECL_ATOM(weight);
DECL_ATOM(slant);
//DECL_ATOM(width);
DECL_ATOM(style);
DECL_ATOM(spacing);
DECL_ATOM(pixel_format);
DECL_ATOM(epx_pixel_format);
DECL_ATOM(pixel_size);
DECL_ATOM(point_size);
DECL_ATOM(resolution_x);
DECL_ATOM(resolution_y);
DECL_ATOM(descent);
DECL_ATOM(ascent);

// epx_font_spacing_t
// DECL_ATOM(none);
DECL_ATOM(proportional);
DECL_ATOM(monospaced);
DECL_ATOM(char_cell);

// epx_font_style_t
// DECL_ATOM(none);
DECL_ATOM(serif);
DECL_ATOM(sans_serif);
DECL_ATOM(informal);
DECL_ATOM(decorated );

// epx_font_width_t
// DECL_ATOM(none);
DECL_ATOM(normal);
DECL_ATOM(condensed);
DECL_ATOM(narrow);
DECL_ATOM(double_wide);
// epx_font_slant_t
// DECL_ATOM(none);
DECL_ATOM(roman);
DECL_ATOM(italic);
DECL_ATOM(oblique);
DECL_ATOM(reverse_italic);
DECL_ATOM(reverse_oblique);
DECL_ATOM(other);
// epx_font_spacing_t
// DECL_ATOM(none);
DECL_ATOM(medium);
DECL_ATOM(bold);
DECL_ATOM(demibold);

// Events
DECL_ATOM(epx_event);
// Event-keys
DECL_ATOM(left);
DECL_ATOM(right);
DECL_ATOM(up);
DECL_ATOM(down);
DECL_ATOM(insert);
DECL_ATOM(delete);
DECL_ATOM(home);
DECL_ATOM(end);
DECL_ATOM(pageup);
DECL_ATOM(pagedown);
DECL_ATOM(f1);
DECL_ATOM(f2);
DECL_ATOM(f3);
DECL_ATOM(f4);
DECL_ATOM(f5);
DECL_ATOM(f6);
DECL_ATOM(f7);
DECL_ATOM(f8);
DECL_ATOM(f9);
DECL_ATOM(f10);
DECL_ATOM(f11);
DECL_ATOM(f12);
DECL_ATOM(print);
DECL_ATOM(sysreq);
DECL_ATOM(pause);
DECL_ATOM(break);
DECL_ATOM(quit);
DECL_ATOM(menu);
DECL_ATOM(redraw);
DECL_ATOM(ctrl);
DECL_ATOM(ctrl_left);
DECL_ATOM(ctrl_right);
DECL_ATOM(shift);
DECL_ATOM(shift_left);
DECL_ATOM(shift_right);
DECL_ATOM(alt);
DECL_ATOM(alt_left);
DECL_ATOM(alt_right);
DECL_ATOM(meta);
DECL_ATOM(meta_left);
DECL_ATOM(meta_right);
DECL_ATOM(num);
DECL_ATOM(caps);
DECL_ATOM(altgr);
DECL_ATOM(scr);
// Event-types
DECL_ATOM(key_press);
DECL_ATOM(key_release);
DECL_ATOM(no_auto_repeat);
DECL_ATOM(motion);
DECL_ATOM(button_press);
DECL_ATOM(button_release);
DECL_ATOM(close);
DECL_ATOM(destroyed);
DECL_ATOM(focus_in);
DECL_ATOM(focus_out);
DECL_ATOM(focus);
DECL_ATOM(enter);
DECL_ATOM(leave);
DECL_ATOM(configure);
DECL_ATOM(expose);
DECL_ATOM(resize);
DECL_ATOM(crossing);
DECL_ATOM(synced);  // special sync operation
// Event-buttons
// DECL_ATOM(left);
// DECL_ATOM(right);
DECL_ATOM(middle);
DECL_ATOM(wheel);
DECL_ATOM(wheel_up);
DECL_ATOM(wheel_down);
DECL_ATOM(wheel_left);
DECL_ATOM(wheel_right);
DECL_ATOM(button);
DECL_ATOM(all);

// Operations
DECL_ATOM(clear);
DECL_ATOM(src);
DECL_ATOM(dst);
DECL_ATOM(src_over);
DECL_ATOM(dst_over);
DECL_ATOM(src_in);
DECL_ATOM(dst_in);
DECL_ATOM(src_out);
DECL_ATOM(dst_out);
DECL_ATOM(src_atop);
DECL_ATOM(dst_atop);
DECL_ATOM(xor);
DECL_ATOM(copy);
DECL_ATOM(add);
DECL_ATOM(sub);
DECL_ATOM(src_blend);
DECL_ATOM(dst_blend);

// Pixel formats (subset)
DECL_ATOM(argb);
DECL_ATOM(rgba);
DECL_ATOM(abgr);
DECL_ATOM(bgra);
DECL_ATOM(rgb);
DECL_ATOM(bgr);
DECL_ATOM(a8);

// epx_pixel_format.format
DECL_ATOM(rgb4);
DECL_ATOM(rgb5);
DECL_ATOM(rgb8);
DECL_ATOM(rgb10);
DECL_ATOM(rgb12);
DECL_ATOM(rgb16);
DECL_ATOM(rgb332);
DECL_ATOM(rgb232);
DECL_ATOM(rgb565);
DECL_ATOM(yuv8);
DECL_ATOM(alpha);
DECL_ATOM(gray);
DECL_ATOM(red);
DECL_ATOM(green);
DECL_ATOM(blue);
DECL_ATOM(calpha);

static void nif_emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((epx_debug_level >= 0) && (level <= epx_debug_level))) {
	int save_errno = errno;
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	enif_fprintf(stderr, "%s:%d: ", file, line);
	enif_vfprintf(stderr, fmt, ap);
	enif_fprintf(stderr, "\r\n");
	va_end(ap);
	errno = save_errno;
    }
}

// some primitive argument access functions

static int get_number(ErlNifEnv* env, const ERL_NIF_TERM term, double* number)
{
    double x;
    int64_t xi;

    if (enif_get_int64(env, term, &xi)) {
	*number = (double) xi;
	return 1;
    }
    if (!enif_get_double(env, term, &x))
	return 0;
    *number = x;
    return 1;
}

static int get_signed(ErlNifEnv* env, const ERL_NIF_TERM term, int* sp)
{
    if (!enif_get_int(env, term, sp)) {
	double fcoord;
	if (!enif_get_double(env, term, &fcoord))
	    return 0;
	*sp = (int) round(fcoord);
    }
    return 1;
}

static int get_fsigned(ErlNifEnv* env, const ERL_NIF_TERM term, float* sp)
{
    double x;
    int xi;
    
    if (enif_get_int(env, term, &xi)) {
	*sp = (float) xi;
	return 1;
    }
    if (!enif_get_double(env, term, &x))
	return 0;
    *sp = x;
    return 1;
}

static int get_unsigned(ErlNifEnv* env, const ERL_NIF_TERM term,
			unsigned int* up)
{
    double uf;    
    if (enif_get_uint(env, term, up))
	;
    else if (enif_get_double(env, term, &uf) && !(uf < 0))
	*up = (unsigned int) uf;
    else
	return 0;
    return 1;
}

static int get_funsigned(ErlNifEnv* env, const ERL_NIF_TERM term,
			 float* up)
{
    unsigned int u;
    double uf;
    if (enif_get_uint(env, term, &u))
	*up = (float) u;
    else if (enif_get_double(env, term, &uf) && !(uf < 0))
	*up = (float) uf;
    else
	return 0;
    return 1;
}

static int get_coord(ErlNifEnv* env,
		     epx_t2d_t* ctm,
		     const ERL_NIF_TERM x, const ERL_NIF_TERM y,
		     int* xp, int* yp)
{
    if (ctm) {
	float xf, yf;
	if (!get_fsigned(env, x, &xf)) return 0;
	if (!get_fsigned(env, y, &yf)) return 0;
	epx_t2d_transform_xy(ctm, &xf, &yf);
	*xp = (int) roundf(xf);
	*yp = (int) roundf(yf);
    }
    else {
	if (!get_signed(env, x, xp)) return 0;
	if (!get_signed(env, y, yp)) return 0;
    }
    return 1;
}

static int get_dim(ErlNifEnv* env,
		   epx_t2d_t* ctm,
		   const ERL_NIF_TERM w,
		   const ERL_NIF_TERM h,
		   unsigned int* wp, unsigned int* hp)
{
    if (ctm) {
	float wf, hf;
	if (!get_funsigned(env, w, &wf)) return 0;
	if (!get_funsigned(env, h, &hf)) return 0;
	epx_t2d_transform_wh(ctm, &wf, &hf);
	*wp = (unsigned int) round(wf);
	*hp = (unsigned int) round(hf);
    }
    else {
	if (!get_unsigned(env, w, wp)) return 0;
	if (!get_unsigned(env, h, hp)) return 0;
    }
    return 1;    
}

static int get_fcoord(ErlNifEnv* env,
		      epx_t2d_t* ctm,
		      const ERL_NIF_TERM x, const ERL_NIF_TERM y,
		      float* xp, float* yp)
{
    if (!get_fsigned(env, x, xp)) return 0;
    if (!get_fsigned(env, y, yp)) return 0;
    if (ctm)
	epx_t2d_transform_xy(ctm, xp, yp);
    return 1;
}

#ifdef not_used
static int get_fdim(ErlNifEnv* env,
		    epx_t2d_t* ctm,
		    const ERL_NIF_TERM w,
		    const ERL_NIF_TERM h,
		    float* wp, float* hp)
{
    if (!get_funsigned(env, w, wp)) return 0;
    if (!get_funsigned(env, h, hp)) return 0;
    if (ctm)
	epx_t2d_transform_wh(ctm, wp, hp);
    return 1;    
}
#endif

/******************************************************************************
 *
 *   Message queue
 *
 *****************************************************************************/

// Peek at queue front
#ifdef __not_used__
static epx_message_t* epx_queue_peek(epx_queue_t* q)
{
    epx_qlink_t* ql;

    enif_mutex_lock(q->mtx);
    ql = q->front;
    enif_mutex_unlock(q->mtx);
    if (ql)
	return &ql->mesg;
    else
	return 0;
}
#endif

// Get message from queue front
static int epx_queue_get(epx_queue_t* q, epx_message_t* m)
{
    epx_qlink_t* ql;

    enif_mutex_lock(q->mtx);
    while(!(ql = q->front)) {
	enif_cond_wait(q->cv, q->mtx);
    }
    if (!(q->front = ql->next))
	q->rear = 0;
    q->len--;

    *m = ql->mesg;

    if ((ql >= &q->ql[0]) && (ql <= &q->ql[MAX_QLINK-1])) {
	ql->next = q->free;
	q->free = ql;
    }
    else
	enif_free(ql);
    enif_mutex_unlock(q->mtx);
    return 0;
}


// Put message at queue rear
static int epx_queue_put(epx_queue_t* q, epx_message_t* m)
{
    epx_qlink_t* ql;
    epx_qlink_t* qr;
    int res = 0;

    enif_mutex_lock(q->mtx);

    if ((ql = q->free))
	q->free = ql->next;
    else
	ql = enif_alloc(sizeof(epx_qlink_t));
    if (!ql)
	res = -1;
    else {
	ql->mesg = *m;
	q->len++;
	ql->next = 0;
	if (!(qr = q->rear)) {
	    q->front = ql;
	    enif_cond_signal(q->cv);
	}
	else
	    qr->next = ql;
	q->rear = ql;
    }
    enif_mutex_unlock(q->mtx);
    return res;
}

static int epx_queue_init(epx_queue_t* q)
{
    int i;
    if (!(q->cv     = enif_cond_create("queue_cv")))
	return -1;
    if (!(q->mtx    = enif_mutex_create("queue_mtx")))
	return -1;
    q->front  = 0;
    q->rear   = 0;
    q->len    = 0;
    for (i = 0; i < MAX_QLINK-1; i++)
	q->ql[i].next = &q->ql[i+1];
    q->ql[MAX_QLINK-1].next = 0;
    q->free = &q->ql[0];
    return 0;
}

static void epx_queue_destroy(epx_queue_t* q)
{
    epx_qlink_t* ql;

    enif_cond_destroy(q->cv);
    enif_mutex_destroy(q->mtx);

    ql = q->front;
    while(ql) {
	epx_qlink_t* qln = ql->next;
	if ((ql >= &q->ql[0]) && (ql <= &q->ql[MAX_QLINK-1]))
	    ;
	else
	    enif_free(ql);
	ql = qln;
    }
}

/******************************************************************************
 *
 *   Threads
 *
 *****************************************************************************/

static char* format_message(epx_message_t* m)
{
    switch(m->type) {
    case EPX_MESSAGE_STOP: return "stop";
    case EPX_MESSAGE_UPGRADE: return "stop";
    case EPX_MESSAGE_SYNC: return "sync";
    case EPX_MESSAGE_SYNC_ACK: return "sync_ack";
    case EPX_MESSAGE_KILL: return "kill";
    case EPX_MESSAGE_POLL: return "poll";
    case EPX_MESSAGE_EVENT_READY: return "event_ready";
    case EPX_MESSAGE_WINDOW_ATTACH: return "window_attach";
    case EPX_MESSAGE_WINDOW_DETACH: return "window_detach";
    case EPX_MESSAGE_WINDOW_ADJUST: return "window_adjust";
    case EPX_MESSAGE_WINDOW_SWAP: return "window_swap";
    case EPX_MESSAGE_BACKEND_ADJUST: return "backend_adjust";
    case EPX_MESSAGE_PIXMAP_ATTACH: return "pixmap_attach";
    case EPX_MESSAGE_PIXMAP_DETACH: return "pixmap_detach";
    case EPX_MESSAGE_PIXMAP_DRAW: return "pixmap_draw";
    case EPX_MESSAGE_WINDOW_SYNC: return "window_sync";
    default: return "unknown";
    }
}

static int epx_thread_wakeup(epx_thread_t* thr)
{
    int r = 0;
    if (thr->wake[1] >= 0) {
	if ((r=write(thr->wake[1], "W", 1)) == 1)
	    r = 0;
    }
    return r;
}

// eat wakeup tokens
static int epx_thread_wokeup(epx_thread_t* thr)
{
    char buf[1];
    int r;
    if ((thr->wake[0]) >= 0) {
	r = read(thr->wake[0], &buf, 1);
	return (r == 1) ? 0 : -1;
    }
    return 0;
}

static int epx_message_send(epx_thread_t* thr, epx_thread_t* sender,
			    epx_message_t* m)
{
    int r;
    m->sender = sender;
    epx_thread_wakeup(thr);
    r=epx_queue_put(&thr->q, m);
    return r;
}

static int epx_message_recv(epx_thread_t* thr, epx_thread_t** from,
			    epx_message_t* m)
{
    int r;
    if ((r = epx_queue_get(&thr->q, m)) < 0)
	return r;
    if (from)
	*from = m->sender;
    return epx_thread_wokeup(thr);
}

#ifdef __not_used__
static epx_message_t* epx_message_peek(epx_thread_t* thr, epx_thread_t** from)
{
    epx_message_t* m;
    if ((m = epx_queue_peek(&thr->q))) {
	if (from)
	    *from = m->sender;
    }
    return m;
}
#endif

#ifdef __APPLE__
static int epx_posix_steal_pthread(pthread_t* pthr,
				   const pthread_attr_t *attr, 
				   void *(*start_routine)(void *),
				   void *arg)
{
    (void) pthr;
    (void) attr;
    ErlNifTid tid;
    DEBUGF("epx_posix_steal_pthread");
    return erl_drv_steal_main_thread((char *)"epx_thread",
				     &tid,start_routine,arg,NULL);
    return -1;
}
#endif

static epx_thread_t* epx_thread_start(void* (*func)(void* arg),
				      void* arg, int wakeup, int steal, 
				      int stack_size)
{
    ErlNifThreadOpts* opts;
    epx_thread_t* thr;

    if (!(thr = enif_alloc(sizeof(epx_thread_t))))
	return 0;
    thr->wake[0] = thr->wake[1] = -1;
    thr->priv = NULL;
    if (epx_queue_init(&thr->q) < 0)
	goto error;
    if (wakeup) {
	if (pipe(thr->wake) < 0)
	    goto error;
    }
    if (!(opts = enif_thread_opts_create("epx_thread_opts")))
	goto error;
    opts->suggested_stack_size = stack_size;
    thr->arg = arg;

    if (steal) {
#ifdef __APPLE__
	(void) erl_drv_steal_main_thread((char *)"epx_thread",
					 &thr->tid,func, thr, opts);
	thr->stolen = 1;
#else
	enif_thread_create("epx_thread", &thr->tid, func, thr, opts);
#endif
    }
    else {
	enif_thread_create("epx_thread", &thr->tid, func, thr, opts);
    }
    enif_thread_opts_destroy(opts);
    return thr;
error:
    if (thr->wake[0] >= 0) close(thr->wake[0]);
    if (thr->wake[1] >= 0) close(thr->wake[1]);
    enif_free(thr);
    return 0;
}

static int epx_thread_stop(epx_thread_t* thr, epx_thread_t* sender,
			     void** exit_value)
{
    epx_message_t m;
    int r;
    (void) r;

    m.type = EPX_MESSAGE_STOP;
    epx_message_send(thr, sender, &m);

    if (thr->stolen) {
#ifdef __APPLE__
	r=erl_drv_stolen_main_thread_join(thr->tid, exit_value);
#else
	r=enif_thread_join(thr->tid, exit_value);
#endif
    }
    else {
	r=enif_thread_join(thr->tid, exit_value);
    }
    DEBUGF("enif_thread_join: return=%d, exit_value=%p", r, *exit_value);
    epx_queue_destroy(&thr->q);
    if (thr->wake[0] >= 0) close(thr->wake[0]);
    if (thr->wake[1] >= 0) close(thr->wake[1]);
    enif_free(thr);
    return 0;
}

static void epx_thread_exit(void* value)
{
    enif_thread_exit(value);
}


static int backend_send(epx_backend_t* backend, epx_message_t* mp)
{
    epx_nif_backend_t* nif_backend = backend->res;
    return epx_message_send(nif_backend->main, 0, mp);
}

/******************************************************************************
 *
 *   Resource destructors
 *
 *****************************************************************************/


static ERL_NIF_TERM* object_type[] =
{
    [EPX_BACKEND_TYPE] = &ATOM(epx_backend),
    [EPX_WINDOW_TYPE]  = &ATOM(epx_window),
    [EPX_PIXMAP_TYPE]  = &ATOM(epx_pixmap),
    [EPX_BITMAP_TYPE]  = &ATOM(epx_bitmap),
    [EPX_GC_TYPE]      = &ATOM(epx_gc),
    [EPX_DICT_TYPE]    = &ATOM(epx_dict),
    [EPX_FONT_TYPE]    = &ATOM(epx_font),
    [EPX_ANIM_TYPE]    = &ATOM(epx_animation),
    [EPX_CANVAS_TYPE]  = &ATOM(epx_canvas),
    [EPX_POLY_TYPE]    = &ATOM(epx_poly)
};


typedef epx_object_t* (*create_fn_t)(void);
typedef epx_object_t* (*copy_fn_t)(epx_object_t*);
	 
static create_fn_t object_create[] =
{ 
    [EPX_GC_TYPE]      = (create_fn_t) epx_gc_create,
    [EPX_DICT_TYPE]    = (create_fn_t) epx_dict_create,
    [EPX_FONT_TYPE]    = (create_fn_t) epx_font_create,
    [EPX_ANIM_TYPE]    = (create_fn_t) epx_anim_create,
    [EPX_CANVAS_TYPE]  = (create_fn_t) epx_canvas_create,
    [EPX_POLY_TYPE]    = (create_fn_t) epx_poly_create
};

static copy_fn_t object_copy[] =
{
    [EPX_BITMAP_TYPE]  = (copy_fn_t) epx_bitmap_copy,        
    [EPX_PIXMAP_TYPE]  = (copy_fn_t) epx_pixmap_copy,    
    [EPX_GC_TYPE]      = (copy_fn_t) epx_gc_copy,
    [EPX_DICT_TYPE]    = (copy_fn_t) epx_dict_copy,
};

static void object_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    epx_object_t* epx_obj = ((epx_nif_object_t*) obj)->epx_obj;
    DEBUGF("OBJECT_DTOR: obj=%T:%p, refc=%lu",
	   *object_type[epx_obj->type],
	   epx_obj, epx_object_refc(epx_obj));
    epx_object_unref(epx_obj);
}

static void backend_dtor(ErlNifEnv* env, void* obj)
{
    epx_ctx_t* ctx = (epx_ctx_t*) enif_priv_data(env);
    epx_nif_backend_t* nif_backend = (epx_nif_backend_t*) obj;
    epx_backend_t* backend = nif_backend->backend;
    epx_nif_backend_t** pp;
    epx_message_t m;

    DEBUGF("BACKEND_DTOR: obj=%p, refc=%lu", backend, epx_object_refc(backend));

    backend->owner = NULL;
    // unlock backend from context
    enif_rwlock_rwlock(ctx->backend_list_lock);
    pp = &ctx->backend_list;
    while(*pp != nif_backend)
	pp = &(*pp)->next;
    *pp = nif_backend->next;
    enif_rwlock_rwunlock(ctx->backend_list_lock);

    // let reaper kill it, to avoid dead lock
    m.type = EPX_MESSAGE_KILL;
    m.thread = nif_backend->main;
    epx_message_send(ctx->reaper, 0, &m);
}

static void pixmap_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    epx_nif_pixmap_t* pm = (epx_nif_pixmap_t*) obj;
    epx_pixmap_t* pixmap = pm->pixmap;

    DEBUGF("PIXMAP_DTOR: obj=%p, user=%p, onwer=%p, refc=%lu",
	   obj, pixmap->user, pixmap->owner, epx_object_refc(pixmap));
    pm->pixmap = NULL;
    pixmap->owner  = NULL;  // mark dtor 
    if (pixmap->user) {     // mapped
	epx_message_t m;
	epx_backend_t* backend = pixmap->user;
	epx_nif_backend_t* nif_backend = (epx_nif_backend_t*) backend->res;
	m.type = EPX_MESSAGE_PIXMAP_DETACH;
	m.pixmap = pixmap;
	epx_object_ref(m.pixmap);
	epx_message_send(nif_backend->main, 0, &m);
    }
    epx_object_unref(pixmap);
}

static void window_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    epx_nif_window_t* nw = (epx_nif_window_t*) obj;
    epx_window_t* window = nw->window;

    DEBUGF("WINDOW_DTOR: obj=%p, user=%p, owner=%p, refc=%lu",
	   obj, window->user, window->owner, epx_object_refc(window));
    nw->window = NULL;
    window->owner = NULL;   // mark dtor 
    if (window->user) {
	epx_message_t m;
	epx_backend_t* backend = window->user;
	epx_nif_backend_t* nif_backend = (epx_nif_backend_t*) backend->res;
	m.type = EPX_MESSAGE_WINDOW_DETACH;
	m.window = window;
	epx_object_ref(m.window);
	epx_message_send(nif_backend->main, 0, &m);
    }
    epx_object_unref(window);
}

/******************************************************************************
 *
 *   Epx resource
 *
 *****************************************************************************/

static epx_nif_object_t* alloc_object(ErlNifResourceType* res,
				      epx_object_t* epx_obj)
{
    epx_nif_object_t* obj = NULL;
    if (epx_obj != NULL) {
	obj = enif_alloc_resource(res, sizeof(epx_nif_object_t));
	obj->epx_obj = epx_obj;
	epx_obj->res = obj;      // connect to resource object
    }
    return obj;
}

static epx_object_t* create_object(ErlNifResourceType* res,
				   epx_object_type_t type)
{
    epx_object_t* epx_obj = (object_create[type])();
    if ((epx_obj != NULL) && (alloc_object(res, epx_obj) == NULL)) {
	epx_object_unref(epx_obj);
	return NULL;
    }
    return epx_obj;
}

static epx_object_t* copy_object(ErlNifResourceType* res,
				     epx_object_t* epx_obj)
{
    epx_object_t* epx_obj_copy = (object_copy[epx_obj->type])(epx_obj);
    if ((epx_obj_copy != NULL) && (alloc_object(res, epx_obj_copy) == NULL)) {
	epx_object_unref(epx_obj);
	return NULL;
    }
    return epx_obj_copy;
}


void* epx_resource_alloc(epx_resource_t* rp, size_t size)
{
    return enif_alloc_resource(rp->res, size);
}

static ErlNifResourceType* epx_resource_create(ErlNifEnv* env,
					       const char* name,
					       ErlNifResourceDtor* dtor)
{
    ErlNifResourceFlags tried;
    return enif_open_resource_type(env, 0, name, dtor,
				   ERL_NIF_RT_CREATE, &tried);
}

static ErlNifResourceType* epx_resource_takeover(ErlNifEnv* env,
						 const char* name,
						 ErlNifResourceDtor* dtor)
{
    ErlNifResourceFlags tried;
    return enif_open_resource_type(env, 0, name, dtor,
				   ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
				   &tried);
}


/******************************************************************************
 *
 *   make/get
 *
 *****************************************************************************/

// For now, wrap the resource object {type,pointer-val,handle}
static ERL_NIF_TERM make_object(ErlNifEnv* env, epx_object_t* epx_obj)
{
    return enif_make_tuple3(env,
			    *object_type[epx_obj->type],
			    enif_make_ulong(env, (unsigned long) epx_obj->res),
			    enif_make_resource(env, epx_obj->res));
}

static ERL_NIF_TERM make_window(ErlNifEnv* env, epx_window_t* window)
{
    if (window == NULL)
	return ATOM(undefined);
    else
	return enif_make_tuple3(env,
				ATOM(epx_window),
				enif_make_ulong(env,
						(unsigned long) window->res),
				enif_make_resource(env, window->res));
}

static ERL_NIF_TERM make_pixmap(ErlNifEnv* env, epx_pixmap_t* pixmap)
{
    if (pixmap == NULL)
	return ATOM(undefined);
    else
	return enif_make_tuple3(env,
				ATOM(epx_pixmap),
				enif_make_ulong(env,
						(unsigned long) pixmap->res),
				enif_make_resource(env, pixmap->res));
}

// Accept {type,pointer-val,handle}
static int get_object(ErlNifEnv* env, const ERL_NIF_TERM arg,
		      ErlNifResourceType* res,
		      epx_object_t** epx_obj_ptr)
{
    const ERL_NIF_TERM* elem;
    epx_nif_object_t* obj;
    int arity;
    unsigned long handle;

    if (!enif_get_tuple(env, arg, &arity, &elem))
	return 0;
    if (arity != 3)
	return 0;
    if (!enif_is_atom(env, elem[0]))
	return 0;	
    if (!enif_get_ulong(env, elem[1], &handle))
	return 0;
    if (!enif_get_resource(env, elem[2], res, (void**)&obj))
	return 0;
    if ((unsigned long) obj != handle)
	return 0;
    if (elem[0] != *object_type[obj->epx_obj->type])
	return 0;
    *epx_obj_ptr = obj->epx_obj;
    return 1;
}


static int get_font(ErlNifEnv* env, const ERL_NIF_TERM arg,
		    epx_font_t** font)
{
    if (!get_object(env, arg, font_res, (epx_object_t**) font))
	return 0;
    return 1;
}

static int get_gc(ErlNifEnv* env, const ERL_NIF_TERM arg,
		  epx_gc_t** gc)
{
    if (!get_object(env, arg, gc_res, (epx_object_t**) gc))
	return 0;
    return 1;
}

static int get_canvas(ErlNifEnv* env, const ERL_NIF_TERM arg,
		      epx_canvas_t** canvas)
{
    if (!get_object(env, arg, canvas_res, (epx_object_t**) canvas))
	return 0;
    return 1;
}

static int get_poly(ErlNifEnv* env, const ERL_NIF_TERM arg,
		    epx_poly_t** poly)
{
    if (!get_object(env, arg, poly_res, (epx_object_t**) poly))
	return 0;
    return 1;
}

static int get_anim(ErlNifEnv* env, const ERL_NIF_TERM arg,
		    epx_animation_t** anim)
{
    if (!get_object(env, arg, animation_res, (epx_object_t**) anim))
	return 0;
    return 1;
}


static int get_dict(ErlNifEnv* env, const ERL_NIF_TERM arg,
		    epx_dict_t** dict)
{
    if (!get_object(env, arg, dict_res, (epx_object_t**) dict))
	return 0;
    return 1;
}

static int get_bitmap(ErlNifEnv* env, const ERL_NIF_TERM arg,
		      epx_bitmap_t** bitmap)
{
    if (!get_object(env, arg, bitmap_res, (epx_object_t**) bitmap))
	return 0;
    return 1;
}

// Accept {type,pointer-val,handle}
static int get_res(ErlNifEnv* env, const ERL_NIF_TERM arg,
		   ErlNifResourceType* res, ERL_NIF_TERM type, void** obj_ptr)
{
    const ERL_NIF_TERM* elem;
    int arity;
    unsigned long handle;
    void* obj;

    if (!enif_get_tuple(env, arg, &arity, &elem))
	return 0;
    if (arity != 3)
	return 0;
    if (elem[0] != type)
	return 0;
    if (!enif_get_ulong(env, elem[1], &handle))
	return 0;
    if (!enif_get_resource(env, elem[2], res, &obj))
	return 0;
    if ((unsigned long)obj != handle)
	return 0;
    *obj_ptr = obj;
    return 1;
}

static int get_backend(ErlNifEnv* env, const ERL_NIF_TERM arg,
		       epx_backend_t** backend_ptr)
{
    epx_nif_backend_t* ptr;
    if (!get_res(env, arg, backend_res, ATOM(epx_backend), (void**) &ptr))
	return 0;
    *backend_ptr = ptr->backend;
    return 1;
}

static int get_window(ErlNifEnv* env, const ERL_NIF_TERM arg,
		      epx_window_t** window_ptr)
{
    epx_nif_window_t* ptr;
    if (!get_res(env, arg, window_res, ATOM(epx_window), (void**) &ptr))
	return 0;
    *window_ptr = ptr->window;
    return 1;    
}

static int get_pixmap(ErlNifEnv* env, const ERL_NIF_TERM arg,
		      epx_pixmap_t** pixmap_ptr)
{
    epx_nif_pixmap_t* ptr;
    if (!get_res(env, arg, pixmap_res, ATOM(epx_pixmap), (void**) &ptr))
	return 0;
    *pixmap_ptr = ptr->pixmap;
    return 1;    
}

static int get_texture(ErlNifEnv* env, const ERL_NIF_TERM term,
		       epx_pixmap_t** texture)
{
    if (term == ATOM(undefined))
	*texture = 0;
    else if (!get_pixmap(env, term, texture))
	return 0;
    return 1;
}

static int get_flag(ErlNifEnv* env, const ERL_NIF_TERM term,
		    epx_flags_t* flags)
{
    (void) env;
    if (term == ATOM(solid))         *flags = EPX_FLAG_SOLID;
    else if (term == ATOM(blend))    *flags = EPX_FLAG_BLEND;
    else if (term == ATOM(sum))      *flags = EPX_FLAG_SUM;
    else if (term == ATOM(aalias))   *flags = EPX_FLAG_AALIAS;
    else if (term == ATOM(textured)) *flags = EPX_FLAG_TEXTURED;
    else if (term == ATOM(nfirst))   *flags = EPX_FLAG_NFIRST;
    else if (term == ATOM(nlast))    *flags = EPX_FLAG_NLAST;
    else if (term == ATOM(none))     *flags = EPX_FLAG_NONE;
    else return 0;
    return 1;
}

static int get_border_flag(ErlNifEnv* env, const ERL_NIF_TERM term,
			   epx_flags_t* flags)
{
    (void) env;
    if (term == ATOM(solid))        *flags = EPX_BORDER_STYLE_SOLID;
    else if (term == ATOM(blend))   *flags = EPX_BORDER_STYLE_BLEND;
    else if (term == ATOM(sum))     *flags = EPX_BORDER_STYLE_SUM;
    else if (term == ATOM(aalias))  *flags = EPX_BORDER_STYLE_AALIAS;
    else if (term == ATOM(textured)) *flags = EPX_BORDER_STYLE_TEXTURED;
    else if (term == ATOM(dashed))  *flags = EPX_BORDER_STYLE_DASHED;
    else if (term == ATOM(ntop))    *flags = EPX_BORDER_STYLE_NTOP;
    else if (term == ATOM(nright))  *flags = EPX_BORDER_STYLE_NRIGHT;
    else if (term == ATOM(nbottom)) *flags = EPX_BORDER_STYLE_NBOTTOM;
    else if (term == ATOM(nleft))   *flags = EPX_BORDER_STYLE_NLEFT;
    else if (term == ATOM(nborder)) *flags = EPX_BORDER_STYLE_NBORDER;
    else if (term == ATOM(outside)) *flags = EPX_BORDER_LOCATION_OUTSIDE;
    else if (term == ATOM(inside))  *flags = EPX_BORDER_LOCATION_INSIDE;
    else if (term == ATOM(center))  *flags = EPX_BORDER_LOCATION_CENTER;
    else return 0;
    return 1;
}

// Parse pixmap flags
// Input styles:  [flag-name] (atom)
//                flag-name   (atom)
//             :  bits        (integer)
// FIXME: add valid mask!
static int get_flags(ErlNifEnv* env, const ERL_NIF_TERM term,
		     epx_flags_t* flags)
{
    epx_flags_t f;
    if (enif_get_uint(env, term, &f)) {
	*flags = f;
	return 1;
    }
    if (enif_is_atom(env, term)) {
	if (!get_flag(env, term, &f))
	    return 0;
	*flags = f;
	return 1;
    }
    else if (enif_is_empty_list(env, term)) {
	*flags = EPX_FLAG_NONE;
	return 1;
    }
    else if (enif_is_list(env, term)) {
	epx_flags_t fs = EPX_FLAG_NONE;
	ERL_NIF_TERM list = term;
	ERL_NIF_TERM head, tail;

	while(enif_get_list_cell(env, list, &head, &tail)) {
	    if (!get_flag(env, head, &f))
		return 0;
	    fs |= f;
	    list = tail;
	}
	if (!enif_is_empty_list(env, list))
	    return 0;
	*flags = fs;
	return 1;
    }
    return 0;
}

static int get_border_flags(ErlNifEnv* env, const ERL_NIF_TERM term,
		     epx_flags_t* flags)
{
    epx_flags_t f;
    if (enif_get_uint(env, term, &f)) {
	*flags = f;
	return 1;
    }
    if (enif_is_atom(env, term)) {
	if (!get_border_flag(env, term, &f))
	    return 0;
	*flags = f;
	return 1;
    }
    else if (enif_is_empty_list(env, term)) {
	*flags = EPX_FLAG_NONE;
	return 1;
    }
    else if (enif_is_list(env, term)) {
	epx_flags_t fs = EPX_FLAG_NONE;
	ERL_NIF_TERM list = term;
	ERL_NIF_TERM head, tail;

	while(enif_get_list_cell(env, list, &head, &tail)) {
	    if (!get_border_flag(env, head, &f))
		return 0;
	    if (f & EPX_BORDER_LOCATION_MASK)
		fs = (fs & ~EPX_BORDER_LOCATION_MASK) | f;
	    else
		fs |= f;
	    list = tail;
	}
	if (!enif_is_empty_list(env, list))
	    return 0;
	*flags = fs;
	return 1;
    }
    return 0;
}

static ERL_NIF_TERM make_flags(ErlNifEnv* env, epx_flags_t flags)
{
    int i = 0;
    ERL_NIF_TERM fv[16];

    if (flags & EPX_FLAG_SOLID)  fv[i++] = ATOM(solid);
    if (flags & EPX_FLAG_BLEND)  fv[i++] = ATOM(blend);
    if (flags & EPX_FLAG_SUM)    fv[i++] = ATOM(sum);
    if (flags & EPX_FLAG_AALIAS) fv[i++] = ATOM(aalias);
    if (flags & EPX_FLAG_TEXTURED) fv[i++] = ATOM(textured);
    if (flags & EPX_FLAG_NFIRST) fv[i++] = ATOM(nfirst);
    if (flags & EPX_FLAG_NLAST) fv[i++] = ATOM(nlast);
    if (flags & EPX_LINE_STYLE_DASHED) fv[i++] = ATOM(dashed);
    if (flags & EPX_BORDER_STYLE_NTOP) fv[i++] = ATOM(ntop);
    if (flags & EPX_BORDER_STYLE_NRIGHT) fv[i++] = ATOM(nright);
    if (flags & EPX_BORDER_STYLE_NBOTTOM) fv[i++] = ATOM(nbottom);
    if (flags & EPX_BORDER_STYLE_NLEFT) fv[i++] = ATOM(nleft);
    if (flags & EPX_BORDER_LOCATION_OUTSIDE) fv[i++] = ATOM(outside);
    if (flags & EPX_BORDER_LOCATION_INSIDE) fv[i++] = ATOM(inside);
    if (flags & EPX_BORDER_LOCATION_CENTER) fv[i++] = ATOM(center);
    return enif_make_list_from_array(env, fv, i);
}

static int get_operation(ErlNifEnv* env, const ERL_NIF_TERM term,
			 epx_pixel_operation_t* op)
{
    (void) env;
         if (term == ATOM(clear))     *op = EPX_PIXEL_OP_CLEAR;
    else if (term == ATOM(src))       *op = EPX_PIXEL_OP_SRC;
    else if (term == ATOM(dst))       *op = EPX_PIXEL_OP_DST;
    else if (term == ATOM(src_over))  *op = EPX_PIXEL_OP_SRC_OVER;
    else if (term == ATOM(dst_over))  *op = EPX_PIXEL_OP_DST_OVER;
    else if (term == ATOM(src_in))    *op = EPX_PIXEL_OP_SRC_IN;
    else if (term == ATOM(dst_in))    *op = EPX_PIXEL_OP_DST_IN;
    else if (term == ATOM(src_out))   *op = EPX_PIXEL_OP_SRC_OUT;
    else if (term == ATOM(dst_out))   *op = EPX_PIXEL_OP_DST_OUT;
    else if (term == ATOM(src_atop))  *op = EPX_PIXEL_OP_SRC_ATOP;
    else if (term == ATOM(dst_atop))  *op = EPX_PIXEL_OP_DST_ATOP;
    else if (term == ATOM(xor))       *op = EPX_PIXEL_OP_XOR;
    else if (term == ATOM(copy))      *op = EPX_PIXEL_OP_COPY;
    else if (term == ATOM(add))       *op = EPX_PIXEL_OP_ADD;
    else if (term == ATOM(sub))       *op = EPX_PIXEL_OP_SUB;
    else if (term == ATOM(src_blend)) *op = EPX_PIXEL_OP_SRC_BLEND;
    else if (term == ATOM(dst_blend)) *op = EPX_PIXEL_OP_DST_BLEND;
    else return 0;
    return 1;
}

static int get_event_flag(ErlNifEnv* env, const ERL_NIF_TERM term,
		     uint32_t* e)
{
    (void) env;
    if (term == ATOM(key_press))           *e = EPX_EVENT_KEY_PRESS;
    else if (term == ATOM(key_release))	   *e = EPX_EVENT_KEY_RELEASE;
    else if (term == ATOM(motion))	   *e = EPX_EVENT_POINTER_MOTION;
    else if (term == ATOM(button_press))   *e = EPX_EVENT_BUTTON_PRESS;
    else if (term == ATOM(button_release)) *e = EPX_EVENT_BUTTON_RELEASE;
    else if (term == ATOM(focus_in))       *e = EPX_EVENT_FOCUS_IN;
    else if (term == ATOM(focus_out))      *e = EPX_EVENT_FOCUS_OUT;
    else if (term == ATOM(focus))          *e = EPX_EVENT_FOCUS_OUT |
					       EPX_EVENT_FOCUS_IN;
    else if (term == ATOM(enter))          *e = EPX_EVENT_ENTER;
    else if (term == ATOM(leave))          *e = EPX_EVENT_LEAVE;
    else if (term == ATOM(configure))      *e = EPX_EVENT_CONFIGURE;
    else if (term == ATOM(expose))         *e = EPX_EVENT_EXPOSE;
    else if (term == ATOM(resize))         *e = EPX_EVENT_RESIZE;
    else if (term == ATOM(crossing))       *e = EPX_EVENT_ENTER|EPX_EVENT_LEAVE;
    else if (term == ATOM(button))         *e = EPX_EVENT_BUTTON_MASK;
    else if (term == ATOM(left))           *e = EPX_EVENT_BUTTON_LEFT;
    else if (term == ATOM(middle))         *e = EPX_EVENT_BUTTON_MIDDLE;
    else if (term == ATOM(right))          *e = EPX_EVENT_BUTTON_RIGHT;
    else if (term == ATOM(wheel))          *e = EPX_EVENT_WHEEL_MASK;
    else if (term == ATOM(wheel_up))       *e = EPX_EVENT_WHEEL_UP;
    else if (term == ATOM(wheel_down))     *e = EPX_EVENT_WHEEL_DOWN;
    else if (term == ATOM(wheel_left))     *e = EPX_EVENT_WHEEL_LEFT;
    else if (term == ATOM(wheel_right))    *e = EPX_EVENT_WHEEL_RIGHT;
    else if (term == ATOM(close))          *e = EPX_EVENT_CLOSE;
    else if (term == ATOM(destroyed))      *e = EPX_EVENT_DESTROYED;
    else if (term == ATOM(all))            *e = EPX_EVENT_ALL;
    else if (term == ATOM(none))           *e = 0;
    else if (term == ATOM(no_auto_repeat)) *e = EPX_EVENT_NO_AUTO_REPEAT;
    else return 0;
    return 1;
}

static int get_event_flags(ErlNifEnv* env, const ERL_NIF_TERM term,
		      uint32_t* events)
{
    if (enif_is_empty_list(env, term)) {
	*events = 0;
	return 1;
    }
    else if (enif_is_atom(env, term)) {
	uint32_t e;
	if (!get_event_flag(env, term, &e))
	    return 0;
	*events = e;
	return 1;
    }
    else if (enif_is_list(env, term)) {
	uint32_t es = 0;
	uint32_t e;
	ERL_NIF_TERM list = term;
	ERL_NIF_TERM head, tail;

	while(enif_get_list_cell(env, list, &head, &tail)) {
	    if (!get_event_flag(env, head, &e))
		return 0;
	    es |= e;
	    list = tail;
	}
	if (!enif_is_empty_list(env, list))
	    return 0;
	*events = es;
	return 1;
    }
    return 0;
}

static ERL_NIF_TERM make_event_flags(ErlNifEnv* env, uint32_t mask)
{
    ERL_NIF_TERM list;

    list = enif_make_list(env, 0);

    if (mask & EPX_EVENT_KEY_PRESS)
	list = enif_make_list_cell(env, ATOM(key_press), list);
    if (mask & EPX_EVENT_KEY_RELEASE)
	list = enif_make_list_cell(env, ATOM(key_release), list);
    if (mask & EPX_EVENT_NO_AUTO_REPEAT)
	list = enif_make_list_cell(env, ATOM(no_auto_repeat), list);
    if (mask & EPX_EVENT_POINTER_MOTION)
	list = enif_make_list_cell(env, ATOM(motion), list);
    if (mask & EPX_EVENT_BUTTON_PRESS)
	list = enif_make_list_cell(env, ATOM(button_press), list);
    if (mask & EPX_EVENT_BUTTON_RELEASE)
	list = enif_make_list_cell(env, ATOM(button_release), list);
    if ((mask & (EPX_EVENT_FOCUS_IN|EPX_EVENT_FOCUS_OUT)) ==
	(EPX_EVENT_FOCUS_IN|EPX_EVENT_FOCUS_OUT))
	list = enif_make_list_cell(env, ATOM(focus), list);
    else {
	if (mask & EPX_EVENT_FOCUS_IN)
	    list = enif_make_list_cell(env, ATOM(focus_in), list);
	if (mask & EPX_EVENT_FOCUS_OUT)
	    list = enif_make_list_cell(env, ATOM(focus_out), list);
    }
    if ((mask & (EPX_EVENT_ENTER | EPX_EVENT_LEAVE)) ==
	(EPX_EVENT_ENTER | EPX_EVENT_LEAVE))
	list = enif_make_list_cell(env, ATOM(crossing), list);
    else {
	if (mask & EPX_EVENT_ENTER)
	    list = enif_make_list_cell(env, ATOM(enter), list);
	if (mask & EPX_EVENT_LEAVE)
	    list = enif_make_list_cell(env, ATOM(leave), list);
    }
    if (mask & EPX_EVENT_CONFIGURE)
	list = enif_make_list_cell(env, ATOM(configure), list);
    if (mask & EPX_EVENT_RESIZE)
	list = enif_make_list_cell(env, ATOM(resize), list);
    if (mask & EPX_EVENT_EXPOSE)
	list = enif_make_list_cell(env, ATOM(expose), list);    
    if (mask & EPX_EVENT_BUTTON_LEFT)
	list = enif_make_list_cell(env, ATOM(left), list);
    if (mask & EPX_EVENT_BUTTON_MIDDLE)
	list = enif_make_list_cell(env, ATOM(middle), list);
    if (mask & EPX_EVENT_BUTTON_RIGHT)
	list = enif_make_list_cell(env, ATOM(right), list);
    if (mask & EPX_EVENT_WHEEL_UP)
	list = enif_make_list_cell(env, ATOM(wheel_up), list);
    if (mask & EPX_EVENT_WHEEL_DOWN)
	list = enif_make_list_cell(env, ATOM(wheel_down), list);
    if (mask & EPX_EVENT_WHEEL_LEFT)
	list = enif_make_list_cell(env, ATOM(wheel_left), list);
    if (mask & EPX_EVENT_WHEEL_RIGHT)
	list = enif_make_list_cell(env, ATOM(wheel_right), list);
    if (mask & EPX_EVENT_CLOSE)
	list = enif_make_list_cell(env, ATOM(close), list);
    if (mask & EPX_EVENT_DESTROYED)
	list = enif_make_list_cell(env, ATOM(destroyed), list);
    return list;
}

static int get_boolean(ErlNifEnv* env, const ERL_NIF_TERM term,
		       int* value)
{
    (void) env;
    if (term == ATOM(true))
	*value = 1;
    else if (term == ATOM(false))
	*value = 0;
    else
	return 0;
    return 1;
}

static int get_bool(ErlNifEnv* env, const ERL_NIF_TERM term,
		    int* value)
{
    if (get_boolean(env, term, value))
	return 1;
    else if (enif_get_int(env, term, value))
	*value = (*value != 0);
    else
	return 0;
    return 1;
}

static ERL_NIF_TERM make_bool(ErlNifEnv* env, int value)
{
    (void) env;
    return (value) ? ATOM(true) : ATOM(false);
}


// Parse pixel format argument
// Input styles:  format-name (atom|string)
//
static int get_pixel_format(ErlNifEnv* env, const ERL_NIF_TERM term,
			    epx_format_t* fmtp)
{
    char fmtbuf[256];
    const ERL_NIF_TERM* elem;
    int fmt;
    int arity;
    int bgr;
    int alpha_first;
    int alpha;
    int little;
    int bpp;

    *fmtp = EPX_FORMAT_INVALID;
    if (enif_get_string(env, term, fmtbuf, sizeof(fmtbuf), ERL_NIF_LATIN1) ||
	enif_get_atom(env, term, fmtbuf, sizeof(fmtbuf), ERL_NIF_LATIN1)) {
	if ((*fmtp = epx_pixel_format_from_name(fmtbuf)) == EPX_FORMAT_INVALID)
	    return 0;
	return 1;
    }
    //  0                1    2      3   4           5     6      7
    // {epx_pixel_format,name,format,bgr,alpha_first,alpha,little,bpp}

    if (!enif_get_tuple(env, term, &arity, &elem)) return 0;
    if (arity != 8) return 0;
    if (elem[0] != ATOM(epx_pixel_format)) return 0;
    // elem[1]=name is not needed
    if (elem[2] == ATOM(rgb4)) fmt = EPX_FMT_RGB4;
    else if (elem[2] == ATOM(rgb5)) fmt = EPX_FMT_RGB5;
    else if (elem[2] == ATOM(rgb8)) fmt = EPX_FMT_RGB8;
    else if (elem[2] == ATOM(rgb10)) fmt = EPX_FMT_RGB10;
    else if (elem[2] == ATOM(rgb12)) fmt = EPX_FMT_RGB12;
    else if (elem[2] == ATOM(rgb16)) fmt = EPX_FMT_RGB16;
    else if (elem[2] == ATOM(rgb332)) fmt = EPX_FMT_RGB332;
    else if (elem[2] == ATOM(rgb232)) fmt = EPX_FMT_RGB232;
    else if (elem[2] == ATOM(rgb565)) fmt = EPX_FMT_RGB565;
    else if (elem[2] == ATOM(yuv8))   fmt = EPX_FMT_YUV8;
    else if (elem[2] == ATOM(alpha))   fmt = EPX_FMT_ALPHA;
    else if (elem[2] == ATOM(gray))   fmt = EPX_FMT_GRAY;
    else if (elem[2] == ATOM(red))   fmt = EPX_FMT_RED;
    else if (elem[2] == ATOM(green))   fmt = EPX_FMT_GREEN;
    else if (elem[2] == ATOM(blue))   fmt = EPX_FMT_BLUE;
    else if (elem[2] == ATOM(calpha)) fmt = EPX_FMT_CALPHA;
    else return 0;
    if (!get_boolean(env, elem[3], &bgr))  return 0;
    if (!get_boolean(env, elem[4], &alpha_first))  return 0;
    if (!get_boolean(env, elem[5], &alpha))  return 0;
    if (!get_boolean(env, elem[6], &little))  return 0;
    if (!enif_get_int(env, elem[7], &bpp)) return 0;
    if ((bpp < 1) || (bpp > 64)) return 0;
    *fmtp = EPX_FMT(fmt,bgr,alpha,alpha_first,little,bpp);
    return 1;
}

// Return pixel format as an atom
static ERL_NIF_TERM make_pixel_format(ErlNifEnv* env, epx_format_t fmt)
{
    char* name;
    char  namebuf[32];

    if (!(name = epx_pixel_format_to_name(fmt,namebuf,sizeof(namebuf))))
	return ATOM(undefined);
    return enif_make_atom(env, name);
}

// return
// #epx_pixel_format { name, format, bgr, alpha_first, alpha, little, bpp }
//
static ERL_NIF_TERM make_epx_pixel_format(ErlNifEnv* env, epx_format_t fmt)
{
    ERL_NIF_TERM name;
    ERL_NIF_TERM format;
    char namebuf[32];
    char* ptr;

    if (!(ptr = epx_pixel_format_to_name(fmt,namebuf,sizeof(namebuf))))
	name=ATOM(undefined);
    else
	name=enif_make_atom(env, ptr);
    switch((fmt&EPX_M_Fmt)>>12) {
    case EPX_FMT_RGB4: format = ATOM(rgb4); break;
    case EPX_FMT_RGB5: format = ATOM(rgb5); break;
    case EPX_FMT_RGB8: format = ATOM(rgb8); break;
    case EPX_FMT_RGB10:format = ATOM(rgb10); break;
    case EPX_FMT_RGB12:format = ATOM(rgb12); break;
    case EPX_FMT_RGB16:format = ATOM(rgb16); break;
    case EPX_FMT_RGB332: format = ATOM(rgb332); break;
    case EPX_FMT_RGB232:format = ATOM(rgb232); break;
    case EPX_FMT_RGB565:format = ATOM(rgb565); break;
    case EPX_FMT_YUV8:format = ATOM(yuv8); break;
    case EPX_FMT_ALPHA:format = ATOM(alpha); break;
    case EPX_FMT_GRAY:format = ATOM(gray); break;
    case EPX_FMT_RED:format = ATOM(red); break;
    case EPX_FMT_GREEN:format = ATOM(green); break;
    case EPX_FMT_BLUE:format = ATOM(blue); break;
    case EPX_FMT_CALPHA:format = ATOM(calpha); break;
    default: format=ATOM(undefined); break;
    }
    return
	enif_make_tuple(env, 8,
			ATOM(epx_pixel_format),
			name,
			format,
			(fmt & EPX_F_Bgr) ? ATOM(true) : ATOM(false),
			(fmt & EPX_F_AFirst) ? ATOM(true) : ATOM(false),
			(fmt & EPX_F_Alpha) ? ATOM(true) : ATOM(false),
			(fmt & EPX_F_Little) ? ATOM(true) : ATOM(false),
			enif_make_int(env, EPX_PIXEL_BIT_SIZE(fmt)));
}

// Parse color argument
// Input styles:  16#AARRGGBB  (integer)
//             :  color-name   (atom)  FIXME (prepare)
//             :  {A,R,G,B}    (ARGB tuple)
//             :  {R,G,B}      (RGB triple)
//

#define clampu8(x) (((x) < 0) ? 0 : ((x)>255) ? 255 : (x))

static int get_color(ErlNifEnv* env, const ERL_NIF_TERM term,
		     epx_pixel_t* pixel)
{
    uint32_t value;
    char namebuf[256];
    const ERL_NIF_TERM* elem;
    int arity;
    int a, r, g, b;

    if (enif_get_uint(env, term, &value)) {
	pixel->px = U32BE(value);
	return 1;
    }

    if (enif_get_atom(env, term, namebuf, sizeof(namebuf), ERL_NIF_LATIN1)) {
	*pixel = epx_pixel_from_string(namebuf);
	return 1;
    }

    if (!enif_get_tuple(env, term, &arity, &elem))
	return 0;
    if (arity == 3) {
	a = EPX_ALPHA_OPAQUE;
	if (!enif_get_int(env, elem[0], &r)) return 0;
	if (!enif_get_int(env, elem[1], &g)) return 0;
	if (!enif_get_int(env, elem[2], &b)) return 0;
    }
    else if (arity == 4) {
	if (!enif_get_int(env, elem[0], &a)) return 0;
	if (!enif_get_int(env, elem[1], &r)) return 0;
	if (!enif_get_int(env, elem[2], &g)) return 0;
	if (!enif_get_int(env, elem[3], &b)) return 0;
    }
    else
	return 0;
    a = clampu8(a);
    r = clampu8(r);
    g = clampu8(g);
    b = clampu8(b);
    *pixel = epx_pixel_argb(a,r,g,b);
    return 1;
}

static ERL_NIF_TERM make_color(ErlNifEnv* env, epx_pixel_t p)
{
    return enif_make_tuple4(env,
			    enif_make_uint(env, p.a),
			    enif_make_uint(env, p.r),
			    enif_make_uint(env, p.g),
			    enif_make_uint(env, p.b));
}

static ERL_NIF_TERM make_texture(ErlNifEnv* env, epx_pixmap_t* texture)
{
    if (!texture)
	return ATOM(undefined);
    else
	return make_pixmap(env, texture);
}

static ERL_NIF_TERM make_bitmap(ErlNifEnv* env, epx_bitmap_t* bitmap)
{
    if (!bitmap)
	return ATOM(undefined);
    else
	return make_object(env, (epx_object_t*) bitmap);
}

static ERL_NIF_TERM make_font(ErlNifEnv* env, epx_font_t* font)
{
    if (!font)
	return ATOM(undefined);
    else
	return make_object(env,(epx_object_t*)font);
}

// Get a 8 bit fix_point value in range [0..1] mapped to [0..255]
// The value may also be given as [0..255]
static int get_fix_8(ErlNifEnv* env, const ERL_NIF_TERM term, uint8_t* value)
{
    unsigned int u_value;
    double f_value;
    if (enif_get_uint(env, term, &u_value)) {
	if (u_value > 255)
	    *value = 255;
	else
	    *value = u_value;
	return 1;
    }
    else if (enif_get_double(env, term, &f_value)) {
	if (f_value >= 1.0)
	    *value = 255;
	else if (f_value <= 0.0)
	    *value = 0;
	else
	    *value = (uint8_t)(f_value*256);
	return 1;
    }
    return 0;
}

static int get_cap_style(ErlNifEnv* env, const ERL_NIF_TERM term,
			 epx_cap_style_t* style)
{
    (void) env;
    if (term == ATOM(none))
	*style = EPX_CAP_STYLE_NONE;
    else if (term == ATOM(butt))
	*style = EPX_CAP_STYLE_BUTT;
    else if (term == ATOM(round))
	*style = EPX_CAP_STYLE_ROUND;
    else if (term == ATOM(projecting))
	*style = EPX_CAP_STYLE_PROJECTING;
    else {
	*style = 0;
	return 0;
    }
    return 1;
}

static ERL_NIF_TERM make_cap_style(ErlNifEnv* env, epx_cap_style_t style)
{
    (void) env;
    if (style == EPX_CAP_STYLE_NONE)
	return ATOM(none);
    else if (style == EPX_CAP_STYLE_BUTT)
	return ATOM(butt);
    else if (style == EPX_CAP_STYLE_ROUND)
	return ATOM(round);
    else if (style == EPX_CAP_STYLE_PROJECTING)
	return ATOM(projecting);
    else
	return ATOM(undefined);
}


static int get_join_style(ErlNifEnv* env, const ERL_NIF_TERM term,
			  epx_join_style_t* style)
{
    (void) env;
    if (term == ATOM(miter))
	*style = EPX_JOIN_STYLE_MITER;
    else if (term == ATOM(round))
	*style = EPX_JOIN_STYLE_ROUND;
    else if (term == ATOM(bevel))
	*style = EPX_JOIN_STYLE_BEVEL;
    else {
	*style = 0;
	return 0;
    }
    return 1;
}

static ERL_NIF_TERM make_join_style(ErlNifEnv* env, epx_join_style_t style)
{
    (void) env;
    if (style == EPX_JOIN_STYLE_MITER)
	return ATOM(miter);
    else if (style == EPX_JOIN_STYLE_ROUND)
	return ATOM(round);
    else if (style == EPX_JOIN_STYLE_BEVEL)
	return ATOM(bevel);
    else
	return ATOM(undefined);
}

static int make_font_weight(ErlNifEnv* env, epx_font_weight_t weight)
{
    (void) env;
    switch(weight) {
    case EPX_FONT_WEIGHT_NONE: return ATOM(none);
    case EPX_FONT_WEIGHT_MEDIUM: return ATOM(medium);
    case EPX_FONT_WEIGHT_BOLD: return ATOM(bold);
    case EPX_FONT_WEIGHT_DEMIBOLD: return ATOM(demibold);
    default: return ATOM(undefined);
    }
}

static int make_font_slant(ErlNifEnv* env, epx_font_slant_t slant)
{
    (void) env;
    switch(slant) {
    case EPX_FONT_SLANT_NONE: return ATOM(none);
    case EPX_FONT_SLANT_ROMAN: return ATOM(roman);
    case EPX_FONT_SLANT_ITALIC: return ATOM(italic);
    case EPX_FONT_SLANT_OBLIQUE: return ATOM(oblique);
    case EPX_FONT_SLANT_REVERSE_ITALIC: return ATOM(reverse_italic);
    case EPX_FONT_SLANT_REVERSE_OBLIQUE: return ATOM(reverse_oblique);
    case EPX_FONT_SLANT_OTHER:return ATOM(other);
    default: return ATOM(undefined);
    }
}

static int make_font_width(ErlNifEnv* env, epx_font_width_t width)
{
    (void) env;
    switch(width) {
    case EPX_FONT_WIDTH_NONE: return ATOM(none);
    case EPX_FONT_WIDTH_NORMAL: return ATOM(normal);
    case EPX_FONT_WIDTH_CONDENSED: return ATOM(condensed);
    case EPX_FONT_WIDTH_NARROW: return ATOM(narrow);
    case EPX_FONT_WIDTH_DOUBLE_WIDE: return ATOM(double_wide);
    default: return ATOM(undefined);
    }
}


static int make_font_style(ErlNifEnv* env, epx_font_style_t style)
{
    (void) env;
    switch(style) {
    case EPX_FONT_STYLE_NONE: return ATOM(none);
    case EPX_FONT_STYLE_SERIF: return ATOM(serif);
    case EPX_FONT_STYLE_SANS_SERIF: return ATOM(sans_serif);
    case EPX_FONT_STYLE_INFORMAL: return ATOM(informal);
    case EPX_FONT_STYLE_DECORATED: return ATOM(decorated);
    default: return ATOM(undefined);
    }
}

static int make_font_spacing(ErlNifEnv* env, epx_font_spacing_t spacing)
{
    (void) env;
    switch(spacing) {
    case EPX_FONT_SPACING_NONE: return ATOM(none);
    case EPX_FONT_SPACING_PROPORTIONAL: return ATOM(proportional);
    case EPX_FONT_SPACING_MONOSPACED: return ATOM(monospaced);
    case EPX_FONT_SPACING_CHAR_CELL: return ATOM(char_cell);
    default: return ATOM(undefined);
    }
}


// Parse #epx_rect (fixme)
//   {X, Y, Width, Height}
// (WINDOW)
static int get_rect(ErlNifEnv* env, const ERL_NIF_TERM term,
		    epx_rect_t* rect)
{
    const ERL_NIF_TERM* elem;
    int arity;
    epx_rect_t r;

    if (!enif_get_tuple(env, term, &arity, &elem))
	return 0;
    if (arity != 4)
	return 0;
    if (!get_signed(env, elem[0], &r.xy.x))
	return 0;
    if (!get_signed(env, elem[1], &r.xy.y))
	return 0;
    if (!get_unsigned(env, elem[2], &r.wh.width))
	return 0;
    if (!get_unsigned(env, elem[3], &r.wh.height))
	return 0;
    *rect = r;
    return 1;
}

// Load filter
// {Width, Height, <io-list>}
static int get_filter(ErlNifEnv* env, const ERL_NIF_TERM term,
		      epx_filter_t* filter, ErlNifBinary* factors)
{
    const ERL_NIF_TERM* elem;
    int arity;
    int i;
    int n;
    uint32_t sum;

    if (!enif_get_tuple(env, term, &arity, &elem))
	return 0;
    if (arity != 3)
	return 0;
    if (!enif_get_uint(env, elem[0], &filter->wh.width))
	return 0;
    if (!enif_get_uint(env, elem[1], &filter->wh.height))
	return 0;
    // this many filter coefficents are expected
    n = filter->wh.width*filter->wh.height;
    if (!enif_inspect_iolist_as_binary(env, elem[2], factors))
	return 0;
    if ((int)factors->size < n)
	return 0;
    filter->factor = (uint8_t*) factors->data;
    sum = 0;
    for (i = 0; i < n; i++)
	sum += factors->data[i];
    filter->fsum = sum;
    return 1;
}

/* format event
 *
 * {epx_event, Window, {key_press,  Sym, Mod, Code}}
 * {epx_event, Window, {key_release, Sym, Mod, Code}}
 *
 * {epx_event, Window, {button_press,   Button, Where={X, Y, Z}}
 * {epx_event, Window, {button_release, Button, Where={X, Y, Z}}}
 * {epx_event, Window, {motion,         Button, Where={X, Y, Z}}}
 * {epx_event, Window, {enter,          Where={X, Y, Z}}}
 * {epx_event, Window, {leave,          Where={X, Y, Z}}}
 *
 * {epx_event, Window, focus_in}
 * {epx_event, Window, focus_out}
 * {epx_event, Window, closed }
 * {epx_event, Window, destroy }
 *
 *  Button = [left,middle,right,wheel_up,wheel_down,wheel_left,wheel_right]
 *   Mod = [shift_left,shift_right,  (+ shift)
 *          ctrl_left, ctrl_right,   (+ ctrl)
 *          alt_left, alt_right,     (+ alt)
 *          meta_left, meta_right    (+ meta)
 *          num, caps, altgr, scr]
 * APPLE key?
 *
 *   Sym = UNICODE-KEY | symbol name (atom)
 *   Code = integer
 */

/* translate sym speical into atoms */
static ERL_NIF_TERM make_key_sym(ErlNifEnv* env, unsigned short sym)
{
    switch(sym) {
    case EPX_KBD_KEY_LEFT: return ATOM(left);
    case EPX_KBD_KEY_RIGHT: return ATOM(right);
    case EPX_KBD_KEY_UP: return ATOM(up);
    case EPX_KBD_KEY_DOWN: return ATOM(down);
    case EPX_KBD_KEY_INSERT: return ATOM(insert);
    case EPX_KBD_KEY_DELETE: return ATOM(delete);
    case EPX_KBD_KEY_HOME: return ATOM(home);
    case EPX_KBD_KEY_END: return ATOM(end);
    case EPX_KBD_KEY_PAGEUP: return ATOM(pageup);
    case EPX_KBD_KEY_PAGEDOWN: return ATOM(pagedown);
    case EPX_KBD_KEY_F1: return ATOM(f1);
    case EPX_KBD_KEY_F2: return ATOM(f2);
    case EPX_KBD_KEY_F3: return ATOM(f3);
    case EPX_KBD_KEY_F4: return ATOM(f4);
    case EPX_KBD_KEY_F5: return ATOM(f5);
    case EPX_KBD_KEY_F6: return ATOM(f6);
    case EPX_KBD_KEY_F7: return ATOM(f7);
    case EPX_KBD_KEY_F8: return ATOM(f8);
    case EPX_KBD_KEY_F9: return ATOM(f9);
    case EPX_KBD_KEY_F10: return ATOM(f10);
    case EPX_KBD_KEY_F11: return ATOM(f11);
    case EPX_KBD_KEY_F12: return ATOM(f12);
    case EPX_KBD_KEY_PRINT: return ATOM(print);
    case EPX_KBD_KEY_SYSREQ: return ATOM(sysreq);
    case EPX_KBD_KEY_PAUSE: return ATOM(pause);
    case EPX_KBD_KEY_BREAK: return ATOM(break);
    case EPX_KBD_KEY_QUIT: return ATOM(quit);
    case EPX_KBD_KEY_MENU: return ATOM(menu);
    case EPX_KBD_KEY_REDRAW: return ATOM(redraw);
    default: return enif_make_uint(env, sym);
    }
}

static ERL_NIF_TERM make_key_event(ErlNifEnv* env, ERL_NIF_TERM type,
				   epx_event_t*e)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);

    if (e->key.mod) {
	if (e->key.mod & EPX_KBD_MOD_CTRL) {
	    if (e->key.mod & EPX_KBD_MOD_LCTRL)
		list = enif_make_list_cell(env, ATOM(ctrl_left), list);
	    if (e->key.mod & EPX_KBD_MOD_RCTRL)
		list = enif_make_list_cell(env, ATOM(ctrl_right), list);
	    list = enif_make_list_cell(env, ATOM(ctrl), list);
	}
	if (e->key.mod & EPX_KBD_MOD_SHIFT) {
	    if (e->key.mod & EPX_KBD_MOD_LSHIFT)
		list = enif_make_list_cell(env, ATOM(shift_left), list);
	    if (e->key.mod & EPX_KBD_MOD_RSHIFT)
		list = enif_make_list_cell(env, ATOM(shift_right), list);
	    list = enif_make_list_cell(env, ATOM(shift), list);
	}
	if (e->key.mod & EPX_KBD_MOD_ALT) {
	    if (e->key.mod & EPX_KBD_MOD_LALT)
		list = enif_make_list_cell(env, ATOM(alt_left), list);
	    if (e->key.mod & EPX_KBD_MOD_RALT)
		list = enif_make_list_cell(env, ATOM(alt_right), list);
	    list = enif_make_list_cell(env, ATOM(alt), list);
	}
	if (e->key.mod & EPX_KBD_MOD_META) {
	    if (e->key.mod & EPX_KBD_MOD_LMETA)
		list = enif_make_list_cell(env, ATOM(meta_left), list);
	    if (e->key.mod & EPX_KBD_MOD_RMETA)
		list = enif_make_list_cell(env, ATOM(meta_right), list);
	    list = enif_make_list_cell(env, ATOM(meta), list);
	}
	if (e->key.mod & EPX_KBD_MOD_NUM)
	    list = enif_make_list_cell(env, ATOM(num), list);
	if (e->key.mod & EPX_KBD_MOD_CAPS)
	    list = enif_make_list_cell(env, ATOM(caps), list);
	if (e->key.mod & EPX_KBD_MOD_ALTGR)
	    list = enif_make_list_cell(env, ATOM(altgr), list);
	if (e->key.mod & EPX_KBD_MOD_SCR)
	    list = enif_make_list_cell(env, ATOM(scr), list);
    }
    return enif_make_tuple4(env, type,
			    make_key_sym(env, e->key.sym),
			    list,
			    enif_make_uint(env, e->key.code));
}

static ERL_NIF_TERM make_crossing_event(ErlNifEnv* env, ERL_NIF_TERM type,
					epx_event_t* e)
{
    ERL_NIF_TERM where;

    where = enif_make_tuple3(env,
			     enif_make_int(env, e->pointer.x),
			     enif_make_int(env, e->pointer.y),
			     enif_make_int(env, e->pointer.z));
    return enif_make_tuple2(env, type, where);
}

static ERL_NIF_TERM make_area_event(ErlNifEnv* env, ERL_NIF_TERM type,
				    epx_event_t* e)
{
    ERL_NIF_TERM area;

    area = enif_make_tuple4(env,
			    enif_make_int(env, e->area.x),
			    enif_make_int(env, e->area.y),
			    enif_make_int(env, e->area.w),
			    enif_make_int(env, e->area.h));
    return enif_make_tuple2(env, type, area);
}

static ERL_NIF_TERM make_dimension_event(ErlNifEnv* env, ERL_NIF_TERM type,
					 epx_event_t* e)
{
    ERL_NIF_TERM dim;

    dim = enif_make_tuple3(env,
			   enif_make_int(env, e->dimension.w),
			   enif_make_int(env, e->dimension.h),
			   enif_make_int(env, e->dimension.d));
    return enif_make_tuple2(env, type, dim);
}

static ERL_NIF_TERM make_pointer_event(ErlNifEnv* env, ERL_NIF_TERM type,
				       epx_event_t* e)
{
    ERL_NIF_TERM button = enif_make_list(env, 0);
    ERL_NIF_TERM where;

    if (e->pointer.button & EPX_BUTTON_LEFT)
	button = enif_make_list_cell(env, ATOM(left), button);
    if (e->pointer.button & EPX_BUTTON_RIGHT)
	button = enif_make_list_cell(env, ATOM(right), button);
    if (e->pointer.button & EPX_BUTTON_MIDDLE)
	button = enif_make_list_cell(env, ATOM(middle), button);
    if (e->pointer.button & EPX_WHEEL_UP)
	button = enif_make_list_cell(env, ATOM(wheel_up), button);
    if (e->pointer.button & EPX_WHEEL_DOWN)
	button = enif_make_list_cell(env, ATOM(wheel_down), button);
    if (e->pointer.button & EPX_WHEEL_LEFT)
	button = enif_make_list_cell(env, ATOM(wheel_left), button);
    if (e->pointer.button & EPX_WHEEL_RIGHT)
	button = enif_make_list_cell(env, ATOM(wheel_right), button);

    where = enif_make_tuple3(env,
			     enif_make_int(env, e->pointer.x),
			     enif_make_int(env, e->pointer.y),
			     enif_make_int(env, e->pointer.z));

    return enif_make_tuple3(env, type, button, where);
}

static ERL_NIF_TERM make_event(ErlNifEnv* env, epx_event_t* e)
{
    ERL_NIF_TERM data;

    switch(e->type) {
    case EPX_EVENT_KEY_PRESS:
	data = make_key_event(env, ATOM(key_press), e); break;
    case EPX_EVENT_KEY_RELEASE:
	data = make_key_event(env, ATOM(key_release), e); break;
    case EPX_EVENT_POINTER_MOTION:
	data = make_pointer_event(env, ATOM(motion), e); break;
    case EPX_EVENT_BUTTON_PRESS:
	data = make_pointer_event(env, ATOM(button_press), e); break;
    case EPX_EVENT_BUTTON_RELEASE:
	data = make_pointer_event(env, ATOM(button_release), e); break;
    case EPX_EVENT_CLOSE:
	data = ATOM(close); break;
    case EPX_EVENT_DESTROYED:
	data = ATOM(destroyed); break;
    case EPX_EVENT_FOCUS_IN:
	data = ATOM(focus_in); break;
    case EPX_EVENT_FOCUS_OUT:
	data = ATOM(focus_out); break;
    case EPX_EVENT_ENTER:
	data = make_crossing_event(env, ATOM(enter), e); break;
    case EPX_EVENT_LEAVE:
	data = make_crossing_event(env, ATOM(leave), e); break;
    case EPX_EVENT_CONFIGURE:
	data = make_area_event(env, ATOM(configure), e); break;
    case EPX_EVENT_EXPOSE:
	data = make_area_event(env, ATOM(expose), e); break;	
    case EPX_EVENT_RESIZE:
	data = make_dimension_event(env, ATOM(resize), e); break;
    default:
	data = ATOM(undefined);
    }
    return enif_make_tuple3(env, ATOM(epx_event),
			    make_window(env, e->window),
			    data);
}

/******************************************************************************
 *
 *****************************************************************************/

static ERL_NIF_TERM debug(ErlNifEnv* env, int argc,
			  const ERL_NIF_TERM argv[])
{
    epx_ctx_t* ctx = (epx_ctx_t*) enif_priv_data(env);
    (void) argc;
    int d = DLOG_NONE;

    if (argv[0] == ATOM(debug))        d=DLOG_DEBUG;
    else if (argv[0] == ATOM(info))    d=DLOG_INFO;
    else if (argv[0] == ATOM(notice))  d=DLOG_NOTICE;
    else if (argv[0] == ATOM(warning)) d=DLOG_WARNING;
    else if (argv[0] == ATOM(error))   d=DLOG_ERROR;
    else if (argv[0] == ATOM(critical)) d=DLOG_CRITICAL;
    else if (argv[0] == ATOM(alert))    d=DLOG_ALERT;
    else if (argv[0] == ATOM(emergency)) d=DLOG_EMERGENCY;
    else if (argv[0] == ATOM(none))      d=DLOG_NONE;
    else return enif_make_badarg(env);

    epx_set_debug(d);
    ctx->debug = d;    // save for upgrade

    return ATOM(ok);
}

/******************************************************************************
 *
 *  simd
 *  acceltype()  :: emu|altivec|mmx|sse2|avx2|neon
 *  cpu_vendor_name :: string()
 *  cpu_features    :: string()
 *  cpu_cache_line_size :: integer()
 *  accel = {acceltype(), [acceltype()]}
 *  function = [{name,format()}]
 *****************************************************************************/

static ERL_NIF_TERM simd_info(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    if (argv[0] == ATOM(cpu_vendor_name)) {
	char buf[1024];
	int n = epx_cpu_vendor_name(buf, sizeof(buf));
	return enif_make_string_len(env, buf, n, ERL_NIF_LATIN1);
    }
    else if (argv[0] == ATOM(cpu_brand_string)) {
	char buf[1024];
	int n = epx_cpu_brand_string(buf, sizeof(buf));
	return enif_make_string_len(env, buf, n, ERL_NIF_LATIN1);
    }
    else if (argv[0] == ATOM(cpu_features)) {
	char buf[1024];
	int n = epx_cpu_features(buf, sizeof(buf));
	return enif_make_string_len(env, buf, n, ERL_NIF_LATIN1);
    }
    else if (argv[0] == ATOM(cpu_cache_line_size)) {
	return enif_make_int(env, epx_cpu_cache_line_size());
    }
    else if (argv[0] == ATOM(accel)) {
	ERL_NIF_TERM type = ATOM(none);
	ERL_NIF_TERM avail[5];
	int accel;
	int i = 0;

	accel = epx_simd_accel();
	if (epx_simd != NULL) {
	    if (epx_simd->type == EPX_SIMD_EMU) 	 type = ATOM(emu);
	    else if (epx_simd->type == EPX_SIMD_ALTIVEC) type = ATOM(altivec);
	    else if (epx_simd->type == EPX_SIMD_MMX)     type = ATOM(mmx);
	    else if (epx_simd->type == EPX_SIMD_SSE2)    type = ATOM(sse2);
	    else if (epx_simd->type == EPX_SIMD_AVX2)    type = ATOM(avx2);
	    else if (epx_simd->type == EPX_SIMD_NEON)    type = ATOM(neon);
	    accel -= epx_simd->type;
	}
	// list types available
	if (accel & EPX_SIMD_EMU)     avail[i++] = ATOM(emu);
	if (accel & EPX_SIMD_ALTIVEC) avail[i++] = ATOM(altivec);
	if (accel & EPX_SIMD_MMX)     avail[i++] = ATOM(mmx);
	if (accel & EPX_SIMD_SSE2)    avail[i++] = ATOM(sse2);
	if (accel & EPX_SIMD_AVX2)    avail[i++] = ATOM(avx2);
	if (accel & EPX_SIMD_NEON)    avail[i++] = ATOM(neon);
	return
	    enif_make_tuple2(env, type,
			     enif_make_list_from_array(env, avail, i));
    }
    else if (argv[0] == ATOM(functions)) {
	// return functions that are accelerated (fixme)
	// check that we are accelerated, do not forget to update this list!
	return
	    enif_make_list(
		env, 7,
		enif_make_tuple(env, 2, enif_make_atom(env, "copy_to"),
				enif_make_list(env, 0)),
		enif_make_tuple(env, 2, enif_make_atom(env, "fill_area"),
				enif_make_list(env, 0)),
		enif_make_tuple(env, 2, enif_make_atom(env, "fill_area_blend"),
				enif_make_list(env, 6,
					       ATOM(rgb),
					       ATOM(bgr),
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra)
				    )),
		enif_make_tuple(env, 2, enif_make_atom(env, "blend_area"),
				enif_make_list(env, 4,
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra)
				    )),
		enif_make_tuple(env, 2, enif_make_atom(env, "alpha_area"),
				enif_make_list(env, 4,
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra)
				    )),
		enif_make_tuple(env, 2, enif_make_atom(env, "fade_area"),
				enif_make_list(env, 4,
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra)
				    )),
		enif_make_tuple(env, 2, enif_make_atom(env, "color_area"),
				enif_make_list(env, 8,
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra),
					       enif_make_tuple(
						   env,2,
						   ATOM(a8),ATOM(rgba)),
					       enif_make_tuple(
						   env,2,
						   ATOM(a8),ATOM(bgra)),
					       enif_make_tuple(
						   env,2,
						   ATOM(a8),ATOM(argb)),
					       enif_make_tuple(
						   env,2,
						   ATOM(a8),ATOM(abgr))
				    )));
    }
    else
	return enif_make_badarg(env);
}

static ERL_NIF_TERM simd_set(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    epx_ctx_t* ctx = (epx_ctx_t*) enif_priv_data(env);
    (void) argc;
    int accel;
    if (argv[0] == ATOM(none)) accel = EPX_SIMD_NONE;
    else if (argv[0] == ATOM(emu)) accel = EPX_SIMD_EMU;
    else if (argv[0] == ATOM(altivec)) accel = EPX_SIMD_ALTIVEC;
    else if (argv[0] == ATOM(mmx)) accel = EPX_SIMD_MMX;
    else if (argv[0] == ATOM(sse2)) accel = EPX_SIMD_SSE2;
    else if (argv[0] == ATOM(avx2)) accel = EPX_SIMD_AVX2;
    else if (argv[0] == ATOM(neon)) accel = EPX_SIMD_NEON;
    else if (argv[0] == ATOM(auto)) accel = EPX_SIMD_AUTO;
    else return enif_make_badarg(env);
    // We could check for availability here ? and return error
    ctx->accel = accel;   // save for upgrade
    epx_simd_init(accel);
    return ATOM(ok);
}

/******************************************************************************
 *
 *  Pixmaps
 *
 *****************************************************************************/

static ERL_NIF_TERM pixmap_create(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    unsigned int width;
    unsigned int height;
    epx_format_t fmt;
    epx_pixmap_t* pixmap;
    epx_nif_pixmap_t* pm;
    ERL_NIF_TERM t;

    if (!get_unsigned(env, argv[0], &width))
	return enif_make_badarg(env);
    if (!get_unsigned(env, argv[1], &height))
	return enif_make_badarg(env);
    if (!get_pixel_format(env, argv[2], &fmt))
	return enif_make_badarg(env);

    if ((pixmap = epx_pixmap_create(width, height, fmt)) == NULL)
	return enif_make_badarg(env);
    pm = enif_alloc_resource(pixmap_res, sizeof(epx_nif_pixmap_t));
    pm->pixmap = pixmap;
    pixmap->owner = (void*) enif_self(env, &pm->own);
    pixmap->res = pm;
    t = make_pixmap(env, pixmap);
    enif_release_resource(pm);
    return t;
}

static ERL_NIF_TERM pixmap_info(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(width)) {
	return enif_make_uint(env, src->width);
    }
    else if (argv[1] == ATOM(height)) {
	return enif_make_uint(env, src->height);
    }
    else if (argv[1] == ATOM(bytes_per_row)) {
	return enif_make_uint(env, src->bytes_per_row);
    }
    else if (argv[1] == ATOM(bits_per_pixel)) {
	return enif_make_uint(env, src->bits_per_pixel);
    }
    else if (argv[1] == ATOM(bytes_per_pixel)) {
	return enif_make_uint(env, src->bytes_per_pixel);
    }
    else if (argv[1] == ATOM(pixel_format)) {
	return make_pixel_format(env, src->pixel_format);
    }
    else if (argv[1] == ATOM(epx_pixel_format)) {
	return make_epx_pixel_format(env, src->pixel_format);
    }
    else if (argv[1] == ATOM(parent))
	return make_pixmap(env,src->parent);
    else if (argv[1] == ATOM(clip)) {
	return enif_make_tuple4(env,
				enif_make_int(env, src->clip.xy.x),
				enif_make_int(env, src->clip.xy.y),
				enif_make_uint(env, src->clip.wh.width),
				enif_make_uint(env, src->clip.wh.height));
    }
    else if (argv[1] == ATOM(backend)) {
	epx_backend_t* backend = (epx_backend_t*) src->user;
	if (!backend)
	    return ATOM(undefined);
	else
	    return make_object(env,(epx_object_t*)backend);
    }
    else
	return enif_make_badarg(env);
}


static ERL_NIF_TERM pixmap_sub_pixmap(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    int x, y;
    unsigned int width, height;
    epx_pixmap_t* src;
    epx_pixmap_t* subpixmap;
    epx_nif_pixmap_t* pm;
    ERL_NIF_TERM t;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_signed(env, argv[1], &x))
	return enif_make_badarg(env);
    if (!get_signed(env, argv[2], &y))
	return enif_make_badarg(env);
    if (!get_unsigned(env, argv[3], &width))
	return enif_make_badarg(env);
    if (!get_unsigned(env, argv[4], &height))
	return enif_make_badarg(env);

    if ((subpixmap = epx_pixmap_sub_pixmap(src, x, y, width, height)) == NULL)
	return enif_make_badarg(env);
    pm = enif_alloc_resource(pixmap_res, sizeof(epx_nif_pixmap_t));
    pm->pixmap = subpixmap;
    subpixmap->res = pm;
    subpixmap->owner = (void*) enif_self(env, &pm->own);    
    t = make_pixmap(env, subpixmap);
    enif_release_resource(pm);
    return t;
}

static ERL_NIF_TERM pixmap_copy(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* pixmap;
    ERL_NIF_TERM t;
    epx_nif_pixmap_t* pm;
    
    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if ((pixmap = epx_pixmap_copy(src)) == NULL)
	return enif_make_badarg(env);
    pm = enif_alloc_resource(pixmap_res, sizeof(epx_nif_pixmap_t));
    pm->pixmap = pixmap;
    pixmap->res = pm;
    pixmap->owner = (void*) enif_self(env, &pm->own);
    t = make_pixmap(env, pixmap);
    enif_release_resource(pm);
    return t;
}

static ERL_NIF_TERM pixmap_get_pixel(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixel_t p;
    int x, y;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_coord(env, src->ctm, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    p = epx_pixmap_get_pixel(src, x, y);
    return make_color(env, p);
}

static ERL_NIF_TERM pixmap_interp_pixel(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixel_t p;
    float x;
    float y;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_fcoord(env, src->ctm, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    p = epx_interp(src, x, y);
    return make_color(env, p);
}

//
// pixmap_get_pixels(Pixmap, X, Y, W, H)  (VIEW)
//
static ERL_NIF_TERM pixmap_get_pixels(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    int x, y;
    unsigned int w;
    unsigned int h;
    ErlNifBinary bin;
    ERL_NIF_TERM bt;

    if (!get_pixmap(env, argv[0],&src))
	return enif_make_badarg(env);
    if (!get_coord(env, src->ctm, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);

    // special case !parent, x=0, y=0, w=0, h=0 => all pixels (with alignment)
    if (!src->parent && !x && !y && !w && !h)
	bt = enif_make_resource_binary(env, src, src->data, src->sz);
    else {
	epx_rect_t r  = {{x,y},{w,h}};
	epx_rect_t sr = {{0,0},{src->width,src->height}};
	size_t sz;
	size_t szr;

	if (!epx_rect_intersect(&sr, &r, &r))
	    szr = 0;
	else
	    szr = r.wh.width*src->bytes_per_pixel;
	sz = szr*r.wh.height;
	if (sz == src->sz) // must be all pixels (no alignment)
	    bt = enif_make_resource_binary(env, src, src->data, src->sz);
	else {
	    uint8_t* sptr;
	    uint8_t* dptr;

	    if (!enif_alloc_binary(szr*r.wh.height, &bin))
		return enif_make_badarg(env);
	    sptr = EPX_PIXEL_ADDR(src,r.xy.x,r.xy.y);
	    dptr = bin.data;
	    if (sz) {
		h = r.wh.height;
		while(h--) {
		    memcpy(dptr, sptr, szr);
		    dptr += szr;
		    sptr += src->bytes_per_row;
		}
	    }
	    bt = enif_make_binary(env, &bin);
	    enif_release_binary(&bin);
	}
    }
    return bt;
}

// VIEW coordinated
static ERL_NIF_TERM pixmap_put_pixel(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_pixel_t color;
    epx_flags_t flags;
    int x, y;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_coord(env, pixmap->ctm, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_color(env, argv[3], &color))
	return enif_make_badarg(env);    
    if (!get_flags(env, argv[4], &flags))
	return enif_make_badarg(env);
    // flags/color arguments reversed
    epx_pixmap_put_pixel(pixmap, x, y, flags, color);
    return ATOM(ok);
}

// (VIEW)
static ERL_NIF_TERM pixmap_put_pixels(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    int x, y;
    unsigned int w;
    unsigned int h;
    epx_format_t fmt;
    epx_flags_t flags;
    ErlNifBinary bin;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);

    if (!get_coord(env, pixmap->ctm, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, pixmap->ctm, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);

    if (!get_pixel_format(env, argv[5], &fmt))
	return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[6], &bin))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[7], &flags))
	return enif_make_badarg(env);

    epx_pixmap_put_pixels(pixmap, x, y, w, h, fmt, flags,
			  bin.data, bin.size);
    return ATOM(ok);
}

// epx:pixmap_set_clip(Pixmap, #epx_rect{}) -> ok | exception(badarg)
// (WINDOW)!
static ERL_NIF_TERM pixmap_set_clip(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_rect_t rect;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_rect(env, argv[1], &rect))
	return enif_make_badarg(env);
    epx_pixmap_set_clip(pixmap, &rect);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_fill(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_pixel_t color;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_color(env, argv[1], &color))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[2], &flags))
	return enif_make_badarg(env);
    if ((flags & EPX_FLAG_BLEND) && (color.a != EPX_ALPHA_OPAQUE))
	epx_pixmap_fill_blend(pixmap, color);
    else
	epx_pixmap_fill(pixmap, color);
    return ATOM(ok);
}

// (VIEW)
static ERL_NIF_TERM pixmap_fill_area(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    int x, y;
    unsigned int w;
    unsigned int h;    
    epx_pixel_t color;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_coord(env, pixmap->ctm, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, pixmap->ctm, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);
    if (!get_color(env, argv[5], &color))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[6], &flags))
	return enif_make_badarg(env);
    epx_pixmap_fill_area(pixmap, x, y, w, h, color, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_copy_to(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    epx_pixmap_copy_to(src, dst);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_flip(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    epx_pixmap_flip(pixmap);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_scale(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    unsigned int width;
    unsigned int height;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!get_unsigned(env, argv[2], &width))
	return enif_make_badarg(env);
    if (!get_unsigned(env, argv[3], &height))
	return enif_make_badarg(env);
    epx_pixmap_scale(src, dst, width, height);
    return ATOM(ok);
}

// (VIEW) -> (VIEW)
static ERL_NIF_TERM pixmap_scale_area(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    unsigned int w_src;
    unsigned int h_src;
    int x_dst;
    int y_dst;
    unsigned int w_dst;
    unsigned int h_dst;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);

    if (!get_coord(env, src->ctm, argv[2], argv[3], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[4], argv[5], &x_dst, &y_dst))
	return enif_make_badarg(env);

    if (!get_dim(env, src->ctm, argv[6], argv[7], &w_src, &h_src))
	return enif_make_badarg(env);
    if (!get_dim(env, dst->ctm, argv[8], argv[9], &w_dst, &h_dst))
	return enif_make_badarg(env);    

    if (!get_flags(env, argv[10], &flags))
	return enif_make_badarg(env);
    epx_pixmap_scale_area(src, dst, x_src, y_src, x_dst, y_dst, 
			  w_src, h_src, w_dst, h_dst, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_copy_area(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    unsigned int w_src, h_src;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);

    if (!get_coord(env, src->ctm, argv[2], argv[3], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[4], argv[5], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[6], argv[7], &w_src, &h_src))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[8], &flags))
	return enif_make_badarg(env);
    epx_pixmap_copy_area(src, dst, x_src, y_src, x_dst, y_dst,
			 w_src, h_src, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_alpha_area(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    uint8_t alpha;
    unsigned int w_src, h_src;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!get_fix_8(env, argv[2], &alpha))
	return enif_make_badarg(env);
    if (!get_coord(env, src->ctm, argv[3], argv[4], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[5], argv[6], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[7], argv[8], &w_src, &h_src))
	return enif_make_badarg(env); 
    epx_pixmap_alpha_area(src, dst, alpha, x_src, y_src, x_dst, y_dst,
			  w_src, h_src);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_fade_area(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    uint8_t fade;
    unsigned int w_src, h_src;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!get_fix_8(env, argv[2], &fade))
	return enif_make_badarg(env);
    if (!get_coord(env, src->ctm, argv[3], argv[4], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[5], argv[6], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[7], argv[8], &w_src, &h_src))
	return enif_make_badarg(env);     
    epx_pixmap_fade_area(src, dst, fade, x_src, y_src, x_dst, y_dst,
			 w_src, h_src);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_shadow_area(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    unsigned int w_src, h_src;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);

    if (!get_coord(env, src->ctm, argv[2], argv[3], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[4], argv[5], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[6], argv[7], &w_src, &h_src))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[8], &flags))
	return enif_make_badarg(env);
    epx_pixmap_shadow_area(src, dst, x_src, y_src, x_dst, y_dst,
			   w_src, h_src, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_operation_area(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    unsigned int w_src, h_src;
    epx_pixel_operation_t op;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!get_operation(env, argv[2], &op))
	return enif_make_badarg(env);

    if (!get_coord(env, src->ctm, argv[3], argv[4], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[5], argv[6], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[7], argv[8], &w_src, &h_src))
	return enif_make_badarg(env);    
    epx_pixmap_operation_area(src, dst, op, x_src, y_src, x_dst, y_dst,
			      w_src, h_src);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_add_color_area(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    epx_pixel_t color;
    uint8_t fade;
    unsigned int w_src, h_src;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!get_fix_8(env, argv[2], &fade))
	return enif_make_badarg(env);
    if (!get_color(env, argv[3], &color))
	return enif_make_badarg(env);

    if (!get_coord(env, src->ctm, argv[4], argv[5], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[6], argv[7], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[8], argv[9], &w_src, &h_src))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[10], &flags))
	return enif_make_badarg(env);
    epx_pixmap_add_color_area(src, dst, fade, color,
			      x_src, y_src, x_dst, y_dst,
			      w_src, h_src, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_filter_area(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    epx_filter_t filter;
    ErlNifBinary factors;
    unsigned int w_src, h_src;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!get_filter(env, argv[2], &filter, &factors))
	return enif_make_badarg(env);
    if (!get_coord(env, src->ctm, argv[3], argv[4], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, dst->ctm, argv[5], argv[6], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, src->ctm, argv[7], argv[8], &w_src, &h_src))
	return enif_make_badarg(env); 
    if (!get_flags(env, argv[9], &flags))
	return enif_make_badarg(env);
    epx_pixmap_filter_area(src, dst, &filter, x_src, y_src, x_dst, y_dst,
			   w_src, h_src, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_rotate_area(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    double angle;
    int x_src;
    int y_src;
    int xc_src;
    int yc_src;
    int xc_dst;
    int yc_dst;
    unsigned int w_src;
    unsigned int h_src;
    epx_flags_t flags;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[2], &angle))
	return enif_make_badarg(env);

    if (!get_coord(env, src->ctm, argv[3], argv[4], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, src->ctm, argv[5], argv[6], &xc_src, &yc_src))
	return enif_make_badarg(env);

    if (!get_coord(env, dst->ctm, argv[7], argv[8], &xc_dst, &yc_dst))
	return enif_make_badarg(env);

    if (!get_dim(env, src->ctm, argv[9], argv[10], &w_src, &h_src))
	return enif_make_badarg(env);    

    if (!get_flags(env, argv[11], &flags))
	return enif_make_badarg(env);
    epx_pixmap_rotate_area(src, dst, angle,
			   x_src, y_src,
			   xc_src, yc_src, xc_dst, yc_dst,
			   w_src, h_src, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_scroll(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int horizontal;
    int vertical;
    int rotate;
    epx_pixel_t fill;

    if (!get_pixmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &horizontal))
    	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &vertical))
    	return enif_make_badarg(env);
    if (!get_bool(env, argv[4], &rotate))
    	return enif_make_badarg(env);
    if (!get_color(env, argv[5], &fill))
    	return enif_make_badarg(env);
    epx_pixmap_scroll(src, dst, horizontal, vertical, rotate, fill);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_attach(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;
    epx_pixmap_t* pixmap;
    epx_message_t m;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_backend(env, argv[1], &backend))
	return enif_make_badarg(env);
    if (pixmap->user) // already attached
	return enif_make_badarg(env);

    // make sure backend survive until EPX_MESSAGE_PIXMAP is processed
    epx_object_ref(backend);  // pixmap is using backend!!!
    pixmap->user = backend;   // pixmap (will be) attached to backend

    m.type = EPX_MESSAGE_PIXMAP_ATTACH;
    m.pixmap = pixmap;
    epx_object_ref(m.pixmap);
    backend_send(backend, &m);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_detach(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;    
    epx_pixmap_t* pixmap;
    epx_message_t m;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!(backend = pixmap->user))
	return enif_make_badarg(env);

    m.type = EPX_MESSAGE_PIXMAP_DETACH;
    m.pixmap = pixmap;
    epx_object_ref(m.pixmap);
    backend_send(backend, &m);
    return ATOM(ok);
}

// Draw pixmap onto window
// epx:pixmap_draw(Pixmap, Window, SrcX, SrcY, DstX, DstY, Width, Height)
//
// Both Pixmap and Window must be attached to the same backend!
//
static ERL_NIF_TERM pixmap_draw(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;
    epx_pixmap_t* pixmap;
    epx_window_t* window;
    epx_message_t m;
    epx_rect_t sr0, sr1;     // source rect
    int dst_x, dst_y;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_window(env, argv[1], &window))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[2], argv[3], &sr0.xy.x, &sr0.xy.y))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[4], argv[5], &dst_x, &dst_y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[6], argv[7], &sr0.wh.width, &sr0.wh.height))
	return enif_make_badarg(env);
    if (!(backend = pixmap->user))
	return enif_make_badarg(env);
    if (backend != window->user)
	return enif_make_badarg(env);

    if (!epx_rect_intersect(&sr0, &pixmap->clip, &sr1)) {
	// printf("update empty rectangle\r\n");
	return ATOM(ok);
    }

    // move dst_x and dst_y according to clipped source rectangel
    dst_x += (sr1.xy.x - sr0.xy.x);
    dst_y += (sr1.xy.y - sr0.xy.y);

    // FIXME: check if dst_x / dst_y is outside window
    
    m.wdraw.src_x = sr1.xy.x;
    m.wdraw.src_y = sr1.xy.y;
    m.wdraw.dst_x = dst_x;
    m.wdraw.dst_y = dst_y;
    m.wdraw.width = sr1.wh.width;
    m.wdraw.height = sr1.wh.height;
    
    m.wdraw.pixmap = pixmap;
    m.wdraw.window = window;
    epx_object_ref(m.wdraw.pixmap);
    epx_object_ref(m.wdraw.window);
    m.type = EPX_MESSAGE_PIXMAP_DRAW;
    backend_send(backend, &m);
    return ATOM(ok);
}


// Send a sync signal to the backend thread,
// the backend thread should responed with a
// synced event.
//
static ERL_NIF_TERM window_sync(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;
    epx_window_t* window;
    epx_message_t m;

    if (!get_window(env, argv[0], &window))
	return enif_make_badarg(env);
    if (!(backend = window->user))
	return enif_make_badarg(env);
    m.window = window;
    epx_object_ref(m.window);
    m.type = EPX_MESSAGE_WINDOW_SYNC;
    backend_send(backend, &m);
    return ATOM(ok);
}


// epx:pixmap_draw_point(Pixmap, Gc, X, Y)
static ERL_NIF_TERM pixmap_draw_point(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_gc_t* gc;
    int x, y;
    
    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, pixmap->ctm, argv[2], argv[3], &x, &y))
	return enif_make_badarg(env);
    epx_pixmap_draw_point(pixmap, gc, x, y);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_draw_line(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_gc_t* gc;
    int x1;
    int y1;
    int x2;
    int y2;

    if (!get_pixmap(env, argv[0], &pixmap))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, pixmap->ctm, argv[2], argv[3], &x1, &y1))
	return enif_make_badarg(env);
    if (!get_coord(env, pixmap->ctm, argv[4], argv[5], &x2, &y2))
	return enif_make_badarg(env);
    epx_pixmap_draw_line(pixmap, gc, x1, y1, x2, y2);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_draw_triangle(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    int x0, y0;
    int x1, y1;
    int x2, y2;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[2], argv[3], &x0, &y0))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[4], argv[5], &x1, &y1))
	return enif_make_badarg(env); 
    if (!get_coord(env, px->ctm, argv[6], argv[7], &x2, &y2))
	return enif_make_badarg(env);
    epx_pixmap_draw_triangle(px, gc, x0, y0, x1, y1, x2, y2);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_draw_triangles(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    size_t npoints;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!enif_is_list(env, argv[2]))
	return enif_make_badarg(env);	

    npoints = 0;
    list = argv[2];
    // triangles are a list of
    // [P1,P2,P3 | {P1,P2,P3} | {X1,Y1,X2,Y2,X3,Y3}]
    while(enif_get_list_cell(env, list, &head, &tail)) {
	const ERL_NIF_TERM* array;
	int arity;

	if (!enif_get_tuple(env, head, &arity, &array))
	    return enif_make_badarg(env);
	if (arity == 2) // point
	    npoints++;
	else if (arity == 3) // tuple of points
	    npoints += 3;
	else if (arity == 6) // tuple of coords
	    npoints += 3;
	else
	    return enif_make_badarg(env);
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);

    {
	int xs[npoints];
	int ys[npoints];
	int i, j;

	list = argv[2];
	i = 0;
	while(enif_get_list_cell(env, list, &head, &tail)) {
	    const ERL_NIF_TERM* array;
	    int arity;

	    enif_get_tuple(env, head, &arity, &array);
	    if (arity == 2) {
		if (!get_coord(env,px->ctm,array[0],array[1],&xs[i],&ys[i]))
		    return enif_make_badarg(env);
		i++;
	    }
	    else if (arity == 3) {
		const ERL_NIF_TERM* pt;

		for (j = 0; j < 3; j++) {
		    if (!enif_get_tuple(env, array[j], &arity, &pt))
			return enif_make_badarg(env);
		    if (!get_coord(env,px->ctm,pt[0],pt[1],&xs[i],&ys[i]))
			return enif_make_badarg(env);
		    i++;
		}
	    }
	    else if (arity == 6) {
		for (j = 0; j < 6; j += 2) {
		    if (!get_coord(env,px->ctm,array[j],array[j+1],
				   &xs[i],&ys[i]))
			return enif_make_badarg(env);
		    i++;
		}
	    }
	    list = tail;
	}
	draw_bary_triangles(px, gc, xs, ys, npoints);
    }
    return ATOM(ok);    
}


static ERL_NIF_TERM pixmap_draw_rectangle(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    int x, y;
    unsigned int w, h;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[2], argv[3], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, px->ctm, argv[4], argv[5], &w, &h))
	return enif_make_badarg(env);
    epx_pixmap_draw_rectangle(px, gc, x, y, w, h);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_draw_fan(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    size_t npoints;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;
    int closed;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!enif_is_list(env, argv[2]))
	return enif_make_badarg(env);
    if (!get_boolean(env, argv[3], &closed))
	return enif_make_badarg(env);

    npoints = 0;
    list = argv[2];
    // poly is given as a list of points
    while(enif_get_list_cell(env, list, &head, &tail)) {
	const ERL_NIF_TERM* pt;
	int arity;

	if (!enif_get_tuple(env, head, &arity, &pt) || (arity != 2))
	    return enif_make_badarg(env);
	npoints += 1;
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);

    {
	int xs[npoints];
	int ys[npoints];
	int i;

	list = argv[2];
	i = 0;
	while(enif_get_list_cell(env, list, &head, &tail)) {
	    const ERL_NIF_TERM* pt;
	    int arity;

	    enif_get_tuple(env, head, &arity, &pt);
	    if (!get_coord(env, px->ctm, pt[0], pt[1], &xs[i], &ys[i]))
		return enif_make_badarg(env);
	    i++;
	    list = tail;
	}
	draw_bary_fan(px, gc, xs, ys, npoints, closed);
    }
    return ATOM(ok);    
}


static ERL_NIF_TERM pixmap_draw_strip(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    size_t npoints;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!enif_is_list(env, argv[2]))
	return enif_make_badarg(env);

    npoints = 0;
    list = argv[2];
    // poly is given as a list of points
    while(enif_get_list_cell(env, list, &head, &tail)) {
	const ERL_NIF_TERM* pt;
	int arity;

	if (!enif_get_tuple(env, head, &arity, &pt) || (arity != 2))
	    return enif_make_badarg(env);
	npoints += 1;
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);

    {
	int xs[npoints];
	int ys[npoints];
	int i;

	list = argv[2];
	i = 0;
	while(enif_get_list_cell(env, list, &head, &tail)) {
	    const ERL_NIF_TERM* pt;
	    int arity;

	    enif_get_tuple(env, head, &arity, &pt);
	    if (!get_coord(env, px->ctm, pt[0], pt[1], &xs[i], &ys[i]))
		return enif_make_badarg(env);
	    i++;
	    list = tail;
	}
	draw_bary_strip(px, gc, xs, ys, npoints);
    }
    return ATOM(ok);    
}


/******************************************************************************
 *
 *  Bitmaps
 *
 *****************************************************************************/

static ERL_NIF_TERM bitmap_create(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    unsigned int width;
    unsigned int height;
    epx_bitmap_t* bitmap;
    epx_nif_object_t* obj;
    ERL_NIF_TERM t;
      
    if (!get_unsigned(env, argv[0], &width))
	return enif_make_badarg(env);
    if (!get_unsigned(env, argv[1], &height))
	return enif_make_badarg(env);

    if ((bitmap = epx_bitmap_create(width, height)) == NULL)
	return enif_make_badarg(env);
    obj = alloc_object(bitmap_res, (epx_object_t*)bitmap);
    t = make_object(env,(epx_object_t*)bitmap);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM bitmap_copy(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* src;
    epx_bitmap_t* dst;
    epx_nif_object_t* obj;
    ERL_NIF_TERM t;
    
    if (!get_bitmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if ((dst = epx_bitmap_copy(src)) == NULL)
	return enif_make_badarg(env);
    obj = alloc_object(bitmap_res, (epx_object_t*)dst);
    t = make_object(env,(epx_object_t*)dst);
    enif_release_resource(obj);
    return t;    
}

static ERL_NIF_TERM bitmap_copy_area(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* src;
    epx_bitmap_t* dst;
    int x_src, y_src;
    int x_dst, y_dst;
    unsigned int w_src, h_src;

    if (!get_bitmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_bitmap(env, argv[1], &dst))
	return enif_make_badarg(env);

    if (!get_coord(env, NULL, argv[2], argv[3], &x_src, &y_src))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[4], argv[5], &x_dst, &y_dst))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[6], argv[7], &w_src, &h_src))
	return enif_make_badarg(env);
    epx_bitmap_copy_area(src, dst, x_src, y_src, x_dst, y_dst,
			 w_src, h_src);
    return ATOM(ok);
}


static ERL_NIF_TERM bitmap_info(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* src;

    if (!get_bitmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(width)) {
	return enif_make_uint(env, src->width);
    }
    else if (argv[1] == ATOM(height)) {
	return enif_make_uint(env, src->height);
    }
    else if (argv[1] == ATOM(bytes_per_row)) {
	return enif_make_uint(env, src->bytes_per_row);
    }
    else if (argv[1] == ATOM(parent))
	return make_bitmap(env,src->parent);
    else if (argv[1] == ATOM(clip)) {
	return enif_make_tuple4(env,
				enif_make_int(env, src->clip.xy.x),
				enif_make_int(env, src->clip.xy.y),
				enif_make_uint(env, src->clip.wh.width),
				enif_make_uint(env, src->clip.wh.height));
    }
    else
	return enif_make_badarg(env);
}

// bitmap_set_clip(Bitmap, #epx_rect{}) -> ok | exception(badarg)
static ERL_NIF_TERM bitmap_set_clip(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    epx_rect_t rect;

    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_rect(env, argv[1], &rect))
	return enif_make_badarg(env);
    epx_bitmap_set_clip(bitmap, &rect);
    return ATOM(ok);
}


static ERL_NIF_TERM bitmap_put_bit(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    int x, y, bit;

    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_boolean(env, argv[3], &bit)) {
	if (!enif_get_int(env, argv[3], &bit))
	    return enif_make_badarg(env);
	if ((bit < 0) || (bit > 1))
	    return enif_make_badarg(env);
    }
    epx_bitmap_put_bit(bitmap, x, y, bit);
    return ATOM(ok);	
}

static ERL_NIF_TERM bitmap_get_bit(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    int x, y, bit;

    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    bit = epx_bitmap_get_bit(bitmap, x, y);
    return enif_make_int(env, bit);
}

// get bitmap bits as a binary
static ERL_NIF_TERM bitmap_get_bits(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    int x, y;
    unsigned int w;
    unsigned int h;
    epx_rect_t r;
    epx_rect_t sr;
    ErlNifBinary bin;    
    ERL_NIF_TERM bt;
    size_t nb;
    uint8_t* src;
    uint8_t* dst;
    size_t doffs;
    
    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);

    epx_rect_set(&r, x, y, w, h);
    epx_rect_set(&sr, 0, 0, bitmap->width, bitmap->height);
    epx_rect_intersect(&sr, &r, &r);
    
    x = r.xy.x;
    y = r.xy.y;
    w = r.wh.width;
    h = r.wh.height;
    nb = (w*h + 7) / 8;
    if (!enif_alloc_binary(nb, &bin))
	return enif_make_badarg(env);
    src = bitmap->data + y*bitmap->bytes_per_row;
    dst = bin.data;
    doffs = 0;
    while(h--) {
	copy_bits(src, x, 1, dst, doffs, 1, w);
	src   += bitmap->bytes_per_row;
	doffs += w;
    }
    bt = enif_make_binary(env, &bin);
    enif_release_binary(&bin);
    return bt;
}

static inline size_t usub(size_t a, size_t b)
{
    return (a >= b) ? a-b : 0;
}

// put bits into a bitmap
// -spec bitmap_put_bits(Dst::epx_pixmap(),X::coord(),Y::coord(),
//		         Width::dim(),Height::dim(),
//		         Bits::iolist()) ->
//	  void().
//
//  FIXME: rethink what data is copied... now we intersect
//  destination and ignore the new width for the source..?
//  maybe more realistic to assume that the original rect
//  {w,h} reflect the source data orientation and layout
//
static ERL_NIF_TERM bitmap_put_bits(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    int x, y;
    unsigned int w;
    unsigned int h;
    epx_rect_t r;
    epx_rect_t dr;
    ErlNifBinary bin;
    ssize_t nb;
    uint8_t* src;
    uint8_t* dst;
    size_t soffs;
    
    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[5], &bin))
	return 0;
    
    epx_rect_set(&r, x, y, w, h);
    epx_rect_set(&dr, 0, 0, bitmap->width, bitmap->height);
    epx_rect_intersect(&dr, &r, &r);
    
    x = r.xy.x;
    y = r.xy.y;
    w = r.wh.width;
    h = r.wh.height;
    nb = (w*h + 7) / 8;
    dst = bitmap->data + y*bitmap->bytes_per_row;
    src = bin.data;
    nb  = bin.size*8;  // #bits avaiable in source
    soffs = 0;
    while(h-- && nb) {
	size_t n = (nb > w) ? w : nb;
	copy_bits(src, soffs, 1, dst, x, 1, n);
	dst   += bitmap->bytes_per_row;
	soffs += w;  // soffs += orig_w ?
	// n = (nb > orig_w) ? orig_w : nb
	nb -= n;
    }
    return ATOM(ok);
}

static ERL_NIF_TERM bitmap_copy_to(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* src;
    epx_bitmap_t* dst;

    if (!get_bitmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_bitmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    epx_bitmap_copy_to(src, dst);
    return ATOM(ok);
}

static ERL_NIF_TERM bitmap_scroll(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{

    (void) argc;
    epx_bitmap_t* src;
    epx_bitmap_t* dst;
    int horizontal;
    int vertical;
    int rotate;
    int pat;

    if (!get_bitmap(env, argv[0], &src))
	return enif_make_badarg(env);
    if (!get_bitmap(env, argv[1], &dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &horizontal))
    	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &vertical))
    	return enif_make_badarg(env);
    if (!get_bool(env, argv[4], &rotate))
    	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &pat))
    	return enif_make_badarg(env);
    epx_bitmap_scroll(src, dst, horizontal, vertical, rotate, (uint8_t) pat);
    return ATOM(ok);    
}

static ERL_NIF_TERM bitmap_fill(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    int pat;

    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &pat))
	return enif_make_badarg(env);
    epx_bitmap_fill(bitmap, (uint8_t) pat);
    return ATOM(ok);	
}

static ERL_NIF_TERM bitmap_fill_rectangle(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    int x, y;
    unsigned int w;
    unsigned int h;    
    epx_bitmap_t* bitmap;
    int pat;    

    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &pat))
	return enif_make_badarg(env);    
    epx_bitmap_fill_rectangle(bitmap, x, y, w, h, (uint8_t) pat);
    return ATOM(ok);	
}

static ERL_NIF_TERM bitmap_draw_rectangle(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    int x, y;
    unsigned int w;
    unsigned int h;    
    epx_bitmap_t* bitmap;
    int pat;

    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &pat))
	return enif_make_badarg(env); 
    epx_bitmap_draw_rectangle(bitmap, x, y, w, h, pat);
    return ATOM(ok);	
}

static ERL_NIF_TERM bitmap_draw_ellipse(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    int x, y;
    unsigned int w, h;
    int pat;
    
    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &pat))
	return enif_make_badarg(env);     
    epx_bitmap_draw_ellipse(bitmap, x, y, w, h, (uint8_t) pat);
    return ATOM(ok);
}

static ERL_NIF_TERM bitmap_fill_ellipse(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    int x, y;
    unsigned int w, h;
    int pat;
    
    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[1], argv[2], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[3], argv[4], &w, &h))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &pat))
	return enif_make_badarg(env);     
    epx_bitmap_fill_ellipse(bitmap, x, y, w, h, (uint8_t) pat);
    return ATOM(ok);
}


static ERL_NIF_TERM bitmap_draw(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_bitmap_t* bitmap;
    epx_pixmap_t* pixmap;
    epx_rect_t sr0, sr1;  // source rect
    int dst_x, dst_y;
    epx_pixel_t fg, bg;
    
    if (!get_bitmap(env, argv[0], &bitmap))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &pixmap))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[2], argv[3], &sr0.xy.x, &sr0.xy.y))
	return enif_make_badarg(env);
    if (!get_coord(env, NULL, argv[4], argv[5], &dst_x, &dst_y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[6], argv[7], &sr0.wh.width, &sr0.wh.height))
	return enif_make_badarg(env);
    if (!get_color(env, argv[8], &fg))
    	return enif_make_badarg(env);
    if (!get_color(env, argv[9], &bg))
    	return enif_make_badarg(env);

    if (!epx_rect_intersect(&sr0, &pixmap->clip, &sr1)) {
	// printf("update empty rectangle\r\n");
	return ATOM(ok);
    }

    // move dst_x and dst_y according to clipped source rectangel
    dst_x += (sr1.xy.x - sr0.xy.x);
    dst_y += (sr1.xy.y - sr0.xy.y);

    epx_bitmap_draw(bitmap, pixmap,
		    sr1.xy.x, sr1.xy.y, dst_x, dst_y, 
		    sr1.wh.width,sr1.wh.height,
		    fg, bg);
    return ATOM(ok);
}


/******************************************************************************
 *
 *  Polygons
 *
 *****************************************************************************/

static ERL_NIF_TERM poly_create(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;    
    epx_poly_t* poly;
    epx_nif_object_t* obj;    
    ERL_NIF_TERM t;

    if ((poly = epx_poly_create()) == NULL)
	return enif_make_badarg(env);
    obj = alloc_object(poly_res, (epx_object_t*)poly);
    t = make_object(env,(epx_object_t*)poly);
    enif_release_resource(obj);
    return t;    
}

#define FZERO (10E-7)

static inline int is_fzero(float a)
{
    return (fabs(a) < FZERO);
}

static inline int is_fequal(float a, float b)
{
    return (fabs(a-b) < FZERO);
}

//
// epx:poly_set(Poly::epx_poly(), Points::[X::number(),Y::number(),..]|
//                                        [X::number(),Y::number()...],
//             [Flags::[relative|absolute]]
//  -> ok

static ERL_NIF_TERM poly_set(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    epx_poly_t* poly;
    size_t npoints;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;
    int is_pair_list = -1;
    int is_absolute = 1;
    
    if (!get_poly(env, argv[0], &poly))
	return enif_make_badarg(env);
    if (!enif_is_list(env, argv[1]))
	return enif_make_badarg(env);
    if (argc >= 3) {
	if (argv[2] == ATOM(relative))
	    is_absolute = 0;
	else if (argv[2] == ATOM(absolute))
	    is_absolute = 1;
	else
	    return enif_make_badarg(env);
    }

    npoints = 0;
    list = argv[1];

    // first determine size of point list either pairs or numbers
    // maybe allow some binary formats?
    if (enif_get_list_cell(env, list, &head, &tail)) {
	const ERL_NIF_TERM* pair;
	int arity;
	float f;
	list = tail;
	if (enif_get_tuple(env, head, &arity, &pair) || (arity == 2))
	    is_pair_list = 1;
	else if (get_fsigned(env, head, &f)) {
	    if (!enif_get_list_cell(env, list, &head, &tail))
		return enif_make_badarg(env);
	    is_pair_list = 0;
	    if (!get_fsigned(env, head, &f))
		return enif_make_badarg(env);
	    list = tail;
	}
	else
	    return enif_make_badarg(env);
	npoints++;
	if (is_pair_list) {  // must be pair list
	    while(enif_get_list_cell(env, list, &head, &tail)) {
		const ERL_NIF_TERM* pt;
		int arity;
		if (!enif_get_tuple(env, head, &arity, &pt) || (arity != 2))
		    return enif_make_badarg(env);
		npoints++;
		list = tail;
	    }
	}
	else {
	    int nnum = 0;
	    while(enif_get_list_cell(env, list, &head, &tail)) {
		float f;
		if (!get_fsigned(env, head, &f))
		    return enif_make_badarg(env);
		nnum++;
		list = tail;
	    }
	    if ((nnum & 1) == 1) // must be even number of numbers
		return enif_make_badarg(env);
	    npoints += (nnum >> 1);
	}
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);  
    
    INFOF("poly_set: npoints=%lu", npoints);
    
    {
	epx_vertex_t* vs;
	int i;

	if ((vs = malloc(npoints*sizeof(epx_vertex_t))) == NULL)
	    return enif_make_badarg(env);  // fixme exception limit!

	list = argv[1];
	i = 0;
	if (is_pair_list) {
	    while(enif_get_list_cell(env, list, &head, &tail)) {
		const ERL_NIF_TERM* coord;
		float x, y;
		int arity;
		if (!enif_get_tuple(env, head, &arity, &coord))
		    goto error;
		if (!get_fcoord(env,NULL,coord[0],coord[1],&x,&y))
		    goto error;
		list = tail;
		if (i == 0) {
		    vs[i].x = x;
		    vs[i].y = y;
		}
		else if (is_absolute) {
		    if (is_fequal(vs[i-1].x, x) &&
			is_fequal(vs[i-1].y, y)) {
			INFOF("poly_set: equal", 0);
			continue;
		    }
		    vs[i].x = x;
		    vs[i].y = y;
		} 
		else { // relative
		    if (is_fzero(x) && is_fzero(y)) {
			INFOF("poly_set: zero", 0);
			continue;
		    }
		    vs[i].x = vs[i-1].x + x;
		    vs[i].y = vs[i-1].y + y;
		}
		INFOF("poly_set: vertex[%d] {x=%f, y=%f}",
		      i, vs[i].x, vs[i].y);
		i++;
	    }
	}
	else {
	    while(enif_get_list_cell(env, list, &head, &tail)) {
		float x, y;		
		if (!get_fsigned(env, head, &x)) goto error;
		list = tail;
		if (!enif_get_list_cell(env, list, &head, &tail)) goto error;
		if (!get_fsigned(env, head, &y))   goto error;
		list = tail;

		if (i == 0) {
		    vs[i].x = x;
		    vs[i].y = y;
		}
		else if (is_absolute) {
		    if (is_fequal(vs[i-1].x, x) &&
			is_fequal(vs[i-1].y, y))
			continue;
		    vs[i].x = x;
		    vs[i].y = y;
		}
		else { // relative
		    if (is_fzero(x) && is_fzero(y))
			continue;
		    vs[i].x = vs[i-1].x + x;
		    vs[i].y = vs[i-1].y + y;
		}		

		INFOF("poly_set: vertex[%d] x=%f, y=%f",
		      i, vs[i].x, vs[i].y);
		i++;
	    }
	}
	epx_poly_set(poly, vs, (size_t) i);
	return ATOM(ok);
    error:
	free(vs);
	return enif_make_badarg(env);	
    }
    return ATOM(ok);
}


static ERL_NIF_TERM poly_draw(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_poly_t* poly;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    int x_offs, y_offs;
    
    if (!get_poly(env, argv[0], &poly))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[2], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[3], argv[4], &x_offs, &y_offs))
	return enif_make_badarg(env);

    epx_poly_draw(poly, px, gc, x_offs, y_offs);

    return ATOM(ok);    
}

static ERL_NIF_TERM poly_info(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argv;
    epx_poly_t* poly;
    
    if (!get_poly(env, argv[0], &poly))
	return enif_make_badarg(env);
    if ((argv[1] == ATOM(count)) && (argc == 2))
	return enif_make_int(env, poly->nes);
    else if ((argv[1] == ATOM(box)) && (argc == 2))
	return enif_make_tuple4(env,
				enif_make_double(env, poly->xmin),
				enif_make_double(env, poly->xmax),
				enif_make_double(env, poly->ymin),
				enif_make_double(env, poly->ymax));
    else if ((argv[1] == ATOM(vertex)) && (argc == 3)) {
	int i;
	epx_vertex_t* vp;
	if (!enif_get_int(env, argv[2], &i) || (i < 1) || (i > (int)poly->nvs))
	    return enif_make_badarg(env);
	vp = &poly->vertex_table[i-1];
	return enif_make_tuple2(env,
				enif_make_double(env, vp->x),
				enif_make_double(env, vp->y));
    }
    else if ((argv[1] == ATOM(pos)) && (argc == 3)) {
	int i;
	epx_pos_t* pp;
	if (!enif_get_int(env, argv[2], &i) || (i < 1) || (i > (int)poly->nvs))
	    return enif_make_badarg(env);
	pp = &poly->pos_table[i-1];
	return enif_make_tuple2(env,
				enif_make_int(env, pp->x),
				enif_make_int(env, pp->y));
    }
    else if ((argv[1] == ATOM(edge)) && (argc == 3)) {
	int i;
	epx_edge_t* ep;
	if (!enif_get_int(env, argv[2], &i) || (i < 1) || (i > (int)poly->nes))
	    return enif_make_badarg(env);
	ep = &poly->edge_table[i-1];
	return enif_make_tuple2(env,
				enif_make_int(env, ep->v0+1),
				enif_make_int(env, ep->v1+1));
    }
    else if ((argv[1] == ATOM(sedge)) && (argc == 3)) {
	int i;
	epx_edge_t* ep;
	if (!enif_get_int(env, argv[2], &i) || (i < 1) || (i > (int)poly->nes))
	    return enif_make_badarg(env);
	ep = poly->es[i-1];
	return enif_make_tuple2(env,
				enif_make_int(env, ep->v0+1),
				enif_make_int(env, ep->v1+1));
    }
    else if ((argv[1] == ATOM(slope)) && (argc == 3)) {
	int i;
	epx_edge_t* ep;
	if (!enif_get_int(env, argv[2], &i) || (i < 1) || (i > (int)poly->nes))
	    return enif_make_badarg(env);
	ep = &poly->edge_table[i-1];
	return enif_make_double(env, ep->k);
    } 
    else
	return enif_make_badarg(env);	
}


static ERL_NIF_TERM pixmap_draw_ellipse(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    int x, y;
    unsigned int w, h;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[2], argv[3], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, px->ctm, argv[4], argv[5], &w, &h))
	return enif_make_badarg(env);    
    epx_pixmap_draw_ellipse(px, gc, x, y, w, h);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_draw_roundrect(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    int x, y;
    unsigned int w, h;
    unsigned int rw, rh;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[2], argv[3], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, px->ctm, argv[4], argv[5], &w, &h))
	return enif_make_badarg(env);
    if (!get_dim(env, px->ctm, argv[6], argv[7], &rw, &rh))
	return enif_make_badarg(env);
    epx_pixmap_draw_roundrect(px, gc, x, y, w, h, rw, rh);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_ltm_translate(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    float tx, ty;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_fsigned(env, argv[1], &tx))
	return enif_make_badarg(env);
    if (!get_fsigned(env, argv[2], &ty))
	return enif_make_badarg(env);
    epx_t2d_translate(&px->ltm, tx, ty, &px->ltm);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_ltm_scale(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    float sx, sy;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_fsigned(env, argv[1], &sx))
	return enif_make_badarg(env);
    if (!get_fsigned(env, argv[2], &sy))
	return enif_make_badarg(env);
    epx_t2d_scale(&px->ltm, sx, sy, &px->ltm);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_ltm_rotate(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    float a;

    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    if (!get_fsigned(env, argv[1], &a))
	return enif_make_badarg(env);
    epx_t2d_rotate(&px->ltm, a, &px->ltm);
    return ATOM(ok);    
}

static ERL_NIF_TERM pixmap_ltm_reset(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    
    if (!get_pixmap(env, argv[0], &px))
	return enif_make_badarg(env);
    epx_t2d_identity(&px->ltm);
    return ATOM(ok);
}



static ERL_NIF_TERM animation_open(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    char path[MAX_PATH];
    int r;
    epx_animation_t* anim;
    epx_nif_object_t* obj;
    ERL_NIF_TERM t;

    if (!(r=enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1))
	|| (r < 0))
	return enif_make_badarg(env);

    if ((anim = epx_anim_create()) == NULL)
	return enif_make_badarg(env);
    obj = alloc_object(animation_res, (epx_object_t*)anim);

    if (epx_anim_open_init(anim, path) < 0) {
	enif_release_resource(obj);
	return enif_make_badarg(env); // reason?
    }
    t = make_object(env,(epx_object_t*)anim);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM animation_copy(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    epx_animation_t* anim;
    int x, y;
    unsigned int index;
    epx_anim_pixels_t* base;
    epx_anim_pixels_t* current;

    if (!get_anim(env, argv[0], &anim))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[2], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[3], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[4], argv[5], &x, &y))
	return enif_make_badarg(env);
    if (!(base = epx_anim_get_pixels(anim, 0)))
	return enif_make_badarg(env);
    if (!(current = epx_anim_get_pixels(anim, (int)index)))
	return enif_make_badarg(env);
    epx_anim_copy_frame(px, gc, x, y,
			anim->hdr.width,
			anim->hdr.height,
			anim->hdr.pixel_format,
			base, current);
    return ATOM(ok);
}

static ERL_NIF_TERM animation_draw(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* px;
    epx_gc_t* gc;
    epx_animation_t* anim;
    int x, y;
    unsigned int index;
    epx_anim_pixels_t* base;
    epx_anim_pixels_t* current;

    if (!get_anim(env, argv[0], &anim))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[2], &px))
	return enif_make_badarg(env);
    if (!get_gc(env, argv[3], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, px->ctm, argv[4], argv[5], &x, &y))
	return enif_make_badarg(env);
    if (!(base = epx_anim_get_pixels(anim, 0)))
	return enif_make_badarg(env);
    if (!(current = epx_anim_get_pixels(anim, (int) index)))
	return enif_make_badarg(env);
    epx_anim_draw_frame(px, gc, x, y,
			anim->hdr.width,
			anim->hdr.height,
			anim->hdr.pixel_format,
			base, current);
    return ATOM(ok);
}

static ERL_NIF_TERM animation_info(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_animation_t* anim;

    if (!get_anim(env, argv[0], &anim))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(file_name)) {
	return enif_make_string(env, anim->file_name, ERL_NIF_LATIN1);
    }
    else if (argv[1] == ATOM(file_size)) {
	return enif_make_uint(env, anim->mapped_size);
    }
    else if (argv[1] == ATOM(count)) {
	return enif_make_uint(env, anim->hdr.image_count);
    }
    if (argv[1] == ATOM(width)) {
	return enif_make_uint(env, anim->hdr.width);
    }
    else if (argv[1] == ATOM(height)) {
	return enif_make_uint(env, anim->hdr.height);
    }
    else if (argv[1] == ATOM(pixel_format)) {
	return make_pixel_format(env, anim->hdr.pixel_format);
    }
    else if (argv[1] == ATOM(epx_pixel_format)) {
	return make_epx_pixel_format(env, anim->hdr.pixel_format);
    }
    else
	return enif_make_badarg(env);
}

/******************************************************************************
 *
 * canvas
 *
 *****************************************************************************/


static ERL_NIF_TERM canvas_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;    
    epx_canvas_t* canvas;
    epx_nif_object_t* obj;    
    ERL_NIF_TERM t;

    if ((canvas = epx_canvas_create()) == NULL)
	return enif_make_badarg(env);
    obj = alloc_object(canvas_res, (epx_object_t*)canvas);
    t = make_object(env,(epx_object_t*)canvas);
    enif_release_resource(obj);
    return t;
}

static ERL_NIF_TERM canvas_line(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    double D, E, F;
    int k;
   
    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &D))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &E))
	return enif_make_badarg(env);
    if (!get_number(env, argv[3], &F))
	return enif_make_badarg(env);
    if ((k = epx_canvas_line(canvas, D, E, F)) < 0)
	return enif_make_badarg(env); // exception alloc ?
    return enif_make_int(env, k);
}

static ERL_NIF_TERM canvas_quad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    double A, B, C, D, E, F;
    int k;
   
    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!get_number(env, argv[1], &A))
	return enif_make_badarg(env);
    if (!get_number(env, argv[2], &B))
	return enif_make_badarg(env);
    if (!get_number(env, argv[3], &C))
	return enif_make_badarg(env);
    if (!get_number(env, argv[4], &D))
	return enif_make_badarg(env);
    if (!get_number(env, argv[5], &E))
	return enif_make_badarg(env);
    if (!get_number(env, argv[6], &F))
	return enif_make_badarg(env);    
    if ((k = epx_canvas_quad(canvas, A,B,C,D,E,F)) < 0)
	return enif_make_badarg(env);
    return enif_make_int(env, k);
}

static ERL_NIF_TERM canvas_and(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    int i, j, k;

    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &i))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &j))
	return enif_make_badarg(env);
    if ((k = epx_canvas_and(canvas, i, j)) < 0)
	return enif_make_badarg(env);
    return enif_make_int(env, k);    
}

static ERL_NIF_TERM canvas_or(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    int i, j, k;

    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &i))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &j))
	return enif_make_badarg(env);
    if ((k = epx_canvas_or(canvas, i, j)) < 0)
	return enif_make_badarg(env);
    return enif_make_int(env, k);    
}

static ERL_NIF_TERM canvas_over(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    int i, j, k;

    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &i))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &j))
	return enif_make_badarg(env);
    if ((k = epx_canvas_over(canvas, i, j)) < 0)
	return enif_make_badarg(env);
    return enif_make_int(env, k);    
}

static ERL_NIF_TERM canvas_not(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    int i, k;

    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &i))
	return enif_make_badarg(env);
    if ((k = epx_canvas_not(canvas, i)) < 0)
	return enif_make_badarg(env);
    return enif_make_int(env, k);    
}


static ERL_NIF_TERM canvas_set_color(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    int i;
    epx_pixel_t color;

    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &i))
	return enif_make_badarg(env);
    if (!get_color(env, argv[2], &color))
	return enif_make_badarg(env);
    if (!epx_canvas_set_color(canvas, i, color))
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM canvas_set_operation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;
    int i;
    epx_pixel_operation_t op;    

    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &i))
	return enif_make_badarg(env);
    if (!get_operation(env, argv[2], &op))
	return enif_make_badarg(env);
    if (!epx_canvas_set_operation(canvas, i, op))
	return enif_make_badarg(env);
    return ATOM(ok);
}

static ERL_NIF_TERM canvas_set_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    epx_canvas_t* canvas;
    int i, j, k;
    double P[6];
    
    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &i))
	return enif_make_badarg(env);
    j = 2;
    k = 0;
    while((j < argc) && (k < 6)) {
	if (!get_number(env, argv[j], &P[k]))
	    return enif_make_badarg(env);
	j++;
	k++;
    }
    if (argc == 5) { // set "linear" params only D,E,F
	for (k = 3; k < 6; k++) {
	    if (!epx_canvas_set_param(canvas, i, k, P[k-3]))
		return enif_make_badarg(env);
	}	
	return ATOM(ok);
    }
    else if (argc == 8) {  // set "all" params A,B,C,D,E,F
	for (k = 0; k < 6; k++) {
	    if (!epx_canvas_set_param(canvas, i, k, P[k]))
		return enif_make_badarg(env);
	}
	return ATOM(ok);
    }
    else
	return enif_make_badarg(env);
}


static ERL_NIF_TERM canvas_draw(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_canvas_t* canvas;    
    epx_pixmap_t* pixmap;

    if (!get_canvas(env, argv[0], &canvas))
	return enif_make_badarg(env);
    if (!get_pixmap(env, argv[1], &pixmap))
	return enif_make_badarg(env);
    epx_canvas_draw(canvas, 0, 0, 0, 0,
		    pixmap->width, pixmap->height,
		    pixmap, EPX_FLAG_BLEND);
    return ATOM(ok);
}

/******************************************************************************
 *
 * Dictionary
 *
 *****************************************************************************/

typedef union dict_data_buf_t
{
    char data[256];
    ErlNifBinary bin;
} dict_data_buf_t;

// Load data into dictionary item
// Handles:
//       integer/float/boolean/string/binary
// TODO:
//       sequence/dict
//
static int get_dict_data(ErlNifEnv* env, const ERL_NIF_TERM t,
			 epx_dict_data_t* data, dict_data_buf_t* buf)
{
    if (enif_get_int(env, t, &data->u.v_integer)) {
	data->type = EPX_DICT_INTEGER;
	return 1;
    }
    else if (enif_get_double(env, t, &data->u.v_float)) {
	data->type = EPX_DICT_FLOAT;
	return 1;
    }
    else if (enif_is_atom(env, t)) {
	int n;
	if (t == ATOM(true)) {
	    data->u.v_boolean = 1;
	    data->type = EPX_DICT_BOOLEAN;
	    return 1;
	}
	else if (t == ATOM(false)) {
	    data->u.v_boolean = 0;
	    data->type = EPX_DICT_BOOLEAN;
	    return 1;
	}
	else if ((n=enif_get_atom(env,t,buf->data,sizeof(buf->data),
				  ERL_NIF_LATIN1))) {
	    data->u.v_string.ptr = buf->data;
	    data->u.v_string.len = n-1;
	    data->type = EPX_DICT_STRING;
	    return 1;
	}
    }
    else if (enif_is_binary(env, t)) {
	if (enif_inspect_binary(env, t, &buf->bin)) {
	    data->u.v_binary.ptr = buf->bin.data;
	    data->u.v_binary.len = buf->bin.size;
	    data->type = EPX_DICT_BINARY;
	    return 1;
	}
    }
    else if (enif_is_list(env, t)) {
	if (enif_inspect_iolist_as_binary(env, t, &buf->bin)) {
	    data->u.v_string.ptr = (char*) buf->bin.data;
	    data->u.v_string.len = buf->bin.size;
	    data->type = EPX_DICT_STRING;
	    return 1;
	}
    }
    return 0;
}

static ERL_NIF_TERM dict_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    epx_object_t* dict;
    ERL_NIF_TERM t;

    if ((dict = create_object(dict_res, EPX_DICT_TYPE)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env, dict);
    enif_release_resource(dict->res);
    return t;    
/*
    if ((dict = epx_dict_create()) == NULL)
	return enif_make_badarg(env);	
    obj = alloc_object(dict_res, (epx_object_t*)dict);
    t = make_object(env,(epx_object_t*)dict);
    enif_release_resource(obj);
    return t;
*/
}

static ERL_NIF_TERM dict_copy(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* src;
    epx_object_t* dst;
    ERL_NIF_TERM t;
    
    if (!get_dict(env, argv[0], &src))
	return enif_make_badarg(env);

    if ((dst = copy_object(dict_res, (epx_object_t*) src)) == NULL)
	return enif_make_badarg(env);
    t = make_object(env, dst);
    enif_release_resource(dst->res);
    return t;        
    /*
    if ((dst = epx_dict_copy(src)) == NULL)
	return enif_make_badarg(env);
    obj = alloc_object(dict_res, (epx_object_t*)dst);
    t = make_object(env,(epx_object_t*)dst);
    enif_release_resource(obj);
    return t;
    */
}

static ERL_NIF_TERM dict_set(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    epx_dict_data_t key;
    dict_data_buf_t key_data;
    epx_dict_data_t value;
    dict_data_buf_t value_data;

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    DEBUGF("get_dict: %p refc=%lu", dict, epx_object_refc(dict));

    if (!get_dict_data(env, argv[1], &key, &key_data))
	return enif_make_badarg(env);

    if (!get_dict_data(env, argv[2], &value, &value_data))
	return enif_make_badarg(env);

    if (epx_dict_set_ent(dict, &key, &value) < 0)
	return enif_make_badarg(env);
    return argv[2];
}

static ERL_NIF_TERM make_entry(ErlNifEnv* env, epx_dict_entry_t* ent)
{
    switch(ent->data.type) {
    case EPX_DICT_NONE:     // 'undefined'
	return ATOM(undefined);
    case EPX_DICT_BOOLEAN: // true|false
	return ent->data.u.v_boolean ? ATOM(true) : ATOM(false);
    case EPX_DICT_INTEGER:
	return enif_make_int(env, ent->data.u.v_integer);
    case EPX_DICT_FLOAT:
	return enif_make_double(env, ent->data.u.v_float);
    case EPX_DICT_STRING:
	return enif_make_string_len(env, ent->data.u.v_string.ptr,
				    ent->data.u.v_string.len,ERL_NIF_LATIN1);
    case EPX_DICT_BINARY: {
	ErlNifBinary bin;
	ERL_NIF_TERM bt;
	size_t n = ent->data.u.v_binary.len;
	if (!enif_alloc_binary(n, &bin))
	    return enif_make_badarg(env);
	memcpy(bin.data, ent->data.u.v_binary.ptr, n);
	bt = enif_make_binary(env, &bin);
	enif_release_binary(&bin);
	return bt;
    }

    case EPX_DICT_SEQUENCE:   // sequence of (epx_dict_data_)
	// not yet
	return enif_make_badarg(env);
    default:
	return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM dict_get(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    epx_dict_data_t key;
    dict_data_buf_t key_data;
    epx_dict_entry_t* ent;

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);

    if (!get_dict_data(env, argv[1], &key, &key_data))
	return enif_make_badarg(env);
    if (!(ent = epx_dict_lookup_ent(dict, &key)))
	return enif_make_badarg(env);
    return make_entry(env, ent);
}

// Specialized getters where key is "string"
// return value is boolean
static ERL_NIF_TERM dict_get_boolean(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    int value;
    char key[256];

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (!enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (epx_dict_lookup_boolean(dict, key, &value) < 0)
	return enif_make_badarg(env);
    // We should create true and false in private data once!
    return value ? ATOM(true) : ATOM(false);
}


// Specialized getters where key is "string"
// return value is integer
static ERL_NIF_TERM dict_get_integer(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    int value;
    char key[256];

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (!enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (epx_dict_lookup_integer(dict, key, &value) < 0)
	return enif_make_badarg(env);
    // We should create true and false in private data once!
    return enif_make_int(env, value);
}

// Specialized getters where key is "string"
// return value is float
static ERL_NIF_TERM dict_get_float(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    double value;
    char key[256];

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (!enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (epx_dict_lookup_float(dict, key, &value) < 0)
	return enif_make_badarg(env);
    // We should create true and false in private data once!
    return enif_make_double(env, value);
}


// Specialized getters where key is "string"
// return value is string
static ERL_NIF_TERM dict_get_string(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    char* value;
    size_t len;
    char key[256];

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (!enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (epx_dict_lookup_string(dict, key, &value, &len) < 0)
	return enif_make_badarg(env);
    // We should create true and false in private data once!
    return enif_make_string_len(env, value, len, ERL_NIF_LATIN1);
}

// Specialized getters where key is "string"
// return value is binary
static ERL_NIF_TERM dict_get_binary(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    void* value;
    size_t len;
    char key[256];
    ErlNifBinary bin;
    ERL_NIF_TERM bt;

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (!enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (epx_dict_lookup_binary(dict, key, &value, &len) < 0)
	return enif_make_badarg(env);
    if (!enif_alloc_binary(len, &bin))
	return enif_make_badarg(env);
    memcpy(bin.data, value, len);
    bt = enif_make_binary(env, &bin);
    enif_release_binary(&bin);
    return bt;
}

static ERL_NIF_TERM dict_is_key(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    char key[256];

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (!enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (epx_dict_is_key(dict, key))
	return ATOM(true);
    return ATOM(false);
}

static ERL_NIF_TERM dict_first(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    size_t len;
    char* value;

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (dict->used == 0)
	return ATOM(false);
    else if (epx_dict_first(dict, &value, &len) < 0)
	return enif_make_badarg(env);
    return enif_make_string_len(env, value, len, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM dict_next(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    char key[256];
    size_t len;
    char* value;

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (!enif_get_string(env, argv[1], key, sizeof(key), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    value = key;
    if (epx_dict_next(dict, &value, &len) < 0)
	return ATOM(false);
    return enif_make_string_len(env, value, len, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM dict_info(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;

    if (!get_dict(env, argv[0], &dict))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(size)) {
	return enif_make_uint(env, dict->used);
    }
    else if (argv[1] == ATOM(sorted)) {
	return dict->is_sorted ? ATOM(true) : ATOM(false);
    }
    else
	return enif_make_badarg(env);
}

/******************************************************************************
 *
 * Graphic context
 *
 *****************************************************************************/

static ERL_NIF_TERM gc_create(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    epx_gc_t* gc;
    epx_nif_object_t* obj;
    ERL_NIF_TERM t;

    if ((gc = epx_gc_create()) == NULL)
	return enif_make_badarg(env);
    obj = alloc_object(gc_res, (epx_object_t*)gc);
    t = make_object(env,(epx_object_t*)gc);
    enif_release_resource(obj);
    return t;    
}

static ERL_NIF_TERM gc_copy(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    epx_gc_t* src;
    epx_gc_t* dst;
    epx_nif_object_t* obj;    
    ERL_NIF_TERM t;

    if (!get_gc(env, argv[0], &src))
	return enif_make_badarg(env);
    if ((dst = epx_gc_copy(src)) == NULL)
	return enif_make_badarg(env);	
    obj = alloc_object(gc_res, (epx_object_t*)dst);
    t = make_object(env,(epx_object_t*)dst);
    enif_release_resource(obj);
    return t;    
}

static ERL_NIF_TERM gc_default(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    epx_ctx_t* ctx = enif_priv_data(env);
    return make_object(env, (epx_object_t*)ctx->def_gc);
}

// FIXME add atom hash!
static ERL_NIF_TERM gc_set(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_gc_t* src;

    if (!get_gc(env, argv[0], &src))
	return enif_make_badarg(env);

    if (argv[1] == ATOM(fill_style)) {
	epx_flags_t style;
	if (!get_flags(env, argv[2], &style))
	    return enif_make_badarg(env);
	epx_gc_set_fill_style(src, style);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(fill_color)) {
	epx_pixel_t color;
	if (!get_color(env, argv[2], &color))
	    return enif_make_badarg(env);
	epx_gc_set_fill_color(src, color);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(fill_texture)) {
	epx_pixmap_t* texture;
	if (!get_texture(env, argv[2], &texture))
	    return enif_make_badarg(env);
	epx_gc_set_fill_texture(src, texture);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(line_style)) {
	epx_flags_t style;
	if (!get_flags(env, argv[2], &style))
	    return enif_make_badarg(env);
	epx_gc_set_line_style(src, style);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(line_join_style)) {
	epx_join_style_t style;
	if (!get_join_style(env, argv[2], &style))
	    return enif_make_badarg(env);
	epx_gc_set_line_join_style(src, style);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(line_cap_style)) {
	epx_cap_style_t style=0;
	if (!get_cap_style(env, argv[2], &style))
	    return enif_make_badarg(env);
	epx_gc_set_line_cap_style(src, style);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(line_width)) {
	unsigned int width;
	if (!get_unsigned(env, argv[2], &width))
	    return enif_make_badarg(env);
	epx_gc_set_line_width(src, width);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(line_texture)) {
	epx_pixmap_t* texture;
	if (!get_texture(env, argv[2], &texture))
	    return enif_make_badarg(env);
	epx_gc_set_line_texture(src, texture);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(border_style)) {
	epx_flags_t style;
	if (!get_border_flags(env, argv[2], &style))
	    return enif_make_badarg(env);
	epx_gc_set_border_style(src, style);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(border_join_style)) {
	epx_join_style_t style;
	if (!get_join_style(env, argv[2], &style))
	    return enif_make_badarg(env);
	epx_gc_set_border_join_style(src, style);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(border_cap_style)) {
	epx_cap_style_t style;
	if (!get_cap_style(env, argv[2], &style))
	    return enif_make_badarg(env);
	epx_gc_set_border_cap_style(src, style);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(border_color)) {
	epx_pixel_t color;
	if (!get_color(env, argv[2], &color))
	    return enif_make_badarg(env);
	epx_gc_set_border_color(src, color);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(border_width)) {
	unsigned int width;
	if (!get_unsigned(env, argv[2], &width))
	    return enif_make_badarg(env);
	epx_gc_set_border_width(src, width);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(border_texture)) {
	epx_pixmap_t* texture;
	if (!get_texture(env, argv[2], &texture))
	    return enif_make_badarg(env);
	epx_gc_set_border_texture(src, texture);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(foreground_color)) {
	epx_pixel_t color;
	if (!get_color(env, argv[2], &color))
	    return enif_make_badarg(env);
	epx_gc_set_foreground_color(src, color);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(background_color)) {
	epx_pixel_t color;
	if (!get_color(env, argv[2], &color))
	    return enif_make_badarg(env);
	epx_gc_set_background_color(src, color);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(fader_value)) {
	uint8_t fade;
	if (!get_fix_8(env, argv[2], &fade))
	    return enif_make_badarg(env);
	epx_gc_set_fader_value(src, fade);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(font)) {
	epx_font_t* font;
	if (argv[2] == ATOM(undefined))
	    font = NULL;
	else if (!get_font(env, argv[2], &font))
	    return enif_make_badarg(env);
	epx_gc_set_font(src, font);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(glyph_delta_x)) {
	int x;
	if (!enif_get_int(env, argv[2], &x))
	    return enif_make_badarg(env);
	epx_gc_set_glyph_delta_x(src, x);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(glyph_delta_y)) {
	int y;
	if (!enif_get_int(env, argv[2], &y))
	    return enif_make_badarg(env);
	epx_gc_set_glyph_delta_y(src, y);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(glyph_fixed_width)) {
	unsigned int width;
	if (!get_unsigned(env, argv[2], &width))
	    return enif_make_badarg(env);
	epx_gc_set_glyph_fixed_width(src, width);
	return ATOM(ok);
    }
    else if (argv[1] == ATOM(glyph_dot_kern)) {
	unsigned int kern;
	if (!enif_get_uint(env, argv[2], &kern))
	    return enif_make_badarg(env);
	epx_gc_set_glyph_dot_kern(src, kern);
	return ATOM(ok);
    }
    else
	return enif_make_badarg(env);
}

static ERL_NIF_TERM gc_get(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_gc_t* src;

    if (!get_gc(env, argv[0], &src))
	return enif_make_badarg(env);

    if (argv[1] == ATOM(fill_style)) {
	return make_flags(env, epx_gc_get_fill_style(src));
    }
    else if (argv[1] == ATOM(fill_color)) {
	return make_color(env, epx_gc_get_fill_color(src));
    }
    else if (argv[1] == ATOM(fill_texture)) {
	return make_texture(env, epx_gc_get_fill_texture(src));
    }
    else if (argv[1] == ATOM(line_style)) {
	return make_flags(env, epx_gc_get_line_style(src));
    }
    else if (argv[1] == ATOM(line_join_style)) {
	return make_join_style(env, epx_gc_get_line_join_style(src));
    }
    else if (argv[1] == ATOM(line_cap_style)) {
	return make_cap_style(env, epx_gc_get_line_cap_style(src));
    }
    else if (argv[1] == ATOM(line_width)) {
	return enif_make_uint(env, epx_gc_get_line_width(src));
    }
    else if (argv[1] == ATOM(line_texture)) {
	return make_texture(env, epx_gc_get_line_texture(src));
    }
    else if (argv[1] == ATOM(border_style)) {
	return make_flags(env, epx_gc_get_border_style(src));
    }
    else if (argv[1] == ATOM(border_join_style)) {
	return make_join_style(env, epx_gc_get_border_join_style(src));
    }
    else if (argv[1] == ATOM(border_cap_style)) {
	return make_cap_style(env, epx_gc_get_border_cap_style(src));
    }
    else if (argv[1] == ATOM(border_color)) {
	return make_color(env, epx_gc_get_border_color(src));
    }
    else if (argv[1] == ATOM(border_width)) {
	return enif_make_uint(env, epx_gc_get_border_width(src));
    }
    else if (argv[1] == ATOM(border_texture)) {
	return make_texture(env, epx_gc_get_border_texture(src));
    }
    else if (argv[1] == ATOM(foreground_color)) {
	return make_color(env, epx_gc_get_foreground_color(src));
    }
    else if (argv[1] == ATOM(background_color)) {
	return make_color(env, epx_gc_get_background_color(src));
    }
    else if (argv[1] == ATOM(fader_value)) {
	// floating point?
	return enif_make_uint(env, epx_gc_get_fader_value(src));
    }
    else if (argv[1] == ATOM(font)) {
	return make_font(env, epx_gc_get_font(src));
    }
    else if (argv[1] == ATOM(glyph_delta_x)) {
	return enif_make_int(env, epx_gc_get_glyph_delta_x(src));
    }
    else if (argv[1] == ATOM(glyph_delta_y)) {
	return enif_make_int(env, epx_gc_get_glyph_delta_y(src));
    }
    else if (argv[1] == ATOM(glyph_fixed_width)) {
	return enif_make_uint(env, epx_gc_get_glyph_fixed_width(src));
    }
    else if (argv[1] == ATOM(glyph_dot_kern)) {
	return enif_make_uint(env, epx_gc_get_glyph_dot_kern(src));
    }
    else
	return enif_make_badarg(env);
}


/******************************************************************************
 *
 * Font
 *
 *****************************************************************************/

static ERL_NIF_TERM font_open(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    char path[MAX_PATH];
    int r;
    epx_font_t* font;
    epx_nif_object_t* obj;
    ERL_NIF_TERM t;

    if (!(r=enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1))
	|| (r < 0))
	return enif_make_badarg(env);    

    if ((font = epx_font_create()) == NULL)
	return enif_make_badarg(env);	

    obj = alloc_object(font_res, (epx_object_t*)font);
    if (epx_font_open_init(font, path) < 0) {
	enif_release_resource(obj);
	return enif_make_badarg(env); // reason?
    }
    t = make_object(env,(epx_object_t*)font);
    enif_release_resource(obj);
    return t;    
}


static ERL_NIF_TERM font_load(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_font(env, argv[0], &font))
	return enif_make_badarg(env);
    if (epx_font_is_loaded(font) || epx_font_is_mapped(font))
	return ATOM(ok);
    if (epx_font_load(font) < 0)
	return ATOM(error); // fixme - reason
    return ATOM(ok);
}

static ERL_NIF_TERM font_unload(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_font(env, argv[0], &font))
	return enif_make_badarg(env);
    epx_font_unload(font);
    return ATOM(ok);
}

static ERL_NIF_TERM font_map(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_font(env, argv[0], &font))
	return enif_make_badarg(env);
    if (epx_font_is_loaded(font) || epx_font_is_mapped(font))
	return ATOM(ok);
    if (epx_font_map(font) < 0)
	return ATOM(error); // fixme - reason
    return ATOM(ok);
}

static ERL_NIF_TERM font_unmap(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_font(env, argv[0], &font))
	return enif_make_badarg(env);
    epx_font_unmap(font);
    return ATOM(ok);
}

static ERL_NIF_TERM font_info(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_font(env, argv[0], &font))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(file_name))
	return enif_make_string(env, font->file_name, ERL_NIF_LATIN1);
    else if (argv[1] == ATOM(file_size))
	return enif_make_uint(env, font->file_size);
    else if (argv[1] == ATOM(foundry_name))
	return enif_make_string(env, font->foundry_name, ERL_NIF_LATIN1);
    else if (argv[1] == ATOM(family_name))
	return enif_make_string(env, font->family_name, ERL_NIF_LATIN1);
    else if (argv[1] == ATOM(weight))
	return make_font_weight(env, font->font_info.weight);
    else if (argv[1] == ATOM(slant))
	return make_font_slant(env, font->font_info.slant);
    else if (argv[1] == ATOM(width))
	return make_font_width(env, font->font_info.width);
    else if (argv[1] == ATOM(style))
	return make_font_style(env, font->font_info.style);
    else if (argv[1] == ATOM(spacing))
	return make_font_spacing(env, font->font_info.spacing);
    else if (argv[1] == ATOM(pixel_format))
	return make_pixel_format(env, font->font_info.pixel_format);
    else if (argv[1] == ATOM(epx_pixel_format))
	return make_epx_pixel_format(env, font->font_info.pixel_format);
    else if (argv[1] == ATOM(pixel_size))
	return enif_make_uint(env, font->font_info.pixel_size);
    else if (argv[1] == ATOM(point_size))
	return enif_make_uint(env, font->font_info.point_size);
    else if (argv[1] == ATOM(resolution_x))
	return enif_make_uint(env, font->font_info.resolution_x);
    else if (argv[1] == ATOM(resolution_y))
	return enif_make_uint(env, font->font_info.resolution_y);
    else if (argv[1] == ATOM(descent))
	return enif_make_uint(env, font->font_info.descent);
    else if (argv[1] == ATOM(ascent))
	return enif_make_uint(env, font->font_info.ascent);
    else
	return enif_make_badarg(env);
}


static ERL_NIF_TERM glyph_info(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;
    epx_font_file_t* ff;
    unsigned encoding;
    epx_glyph_t* glyph;
    
    if (!get_font(env, argv[0], &font))
	return enif_make_badarg(env);
    if ((ff = font->font_file) == NULL) // must be loaded/mapped
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &encoding))
	return enif_make_badarg(env);

    if ((glyph = epx_font_file_glyph(font->font_file, encoding)) == 0)
	return ATOM(undefined);
    if (argv[2] == ATOM(width))
	return enif_make_uint(env, glyph->width);
    if (argv[2] == ATOM(height))
	return enif_make_uint(env, glyph->height);
    if (argv[2] == ATOM(x))
	return enif_make_int(env, glyph->xoffs);
    if (argv[2] == ATOM(y))
	return enif_make_int(env, glyph->yoffs);
    if (argv[2] == ATOM(dx))
	return enif_make_int(env, glyph->dwx);
    if (argv[2] == ATOM(dy))
	return enif_make_int(env, glyph->dwy);
    if (argv[2] == ATOM(name)) {
	const char* str;	
	if (glyph->name_offset == 0)
	    return ATOM(undefined);
	str = epx_font_file_string(font->font_file,
				   glyph->name_offset);
	return enif_make_string(env, str, ERL_NIF_LATIN1);
    }
    return enif_make_badarg(env);    
}

// Draw a glyph C at X,Y return {X',Y'}
static ERL_NIF_TERM font_draw_glyph(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_gc_t* gc;
    epx_pixmap_t* dst;
    epx_t2d_t* ctm;
    int x, y;
    int c;

    if (!get_texture(env, argv[0], &dst))
	return enif_make_badarg(env);
    ctm = dst ? dst->ctm : NULL;
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, ctm, argv[2], argv[3], &x, &y))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &c))
	return enif_make_badarg(env);
    epx_font_draw_glyph(gc, dst, &x, &y, c);
    return enif_make_tuple2(env, enif_make_int(env,x), enif_make_int(env,y));
}

// Draw a "flat" string with (unicode) chars
static ERL_NIF_TERM font_draw_string(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_gc_t* gc;
    epx_pixmap_t* dst;
    epx_t2d_t* ctm;    
    int x, y;
    int n;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;

    if (!get_texture(env, argv[0], &dst))
	return enif_make_badarg(env);
    ctm = dst ? dst->ctm : NULL;
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, ctm, argv[2], argv[3], &x, &y))
	return enif_make_badarg(env);
    list = argv[4];
    n = 0;
    while(enif_get_list_cell(env, list, &head, &tail)) {
	int c;
	if (!enif_get_int(env, head, &c))
	    c = '?';
	if (n && (c == '.') && epx_gc_get_glyph_dot_kern(gc) &&
	    !enif_is_empty_list(env, tail))
	    x += epx_gc_get_glyph_dot_kern(gc);
	epx_font_draw_glyph(gc, dst, &x, &y, c);
	n++;
	list = tail;
    }
    if (n)
	x -= epx_gc_get_glyph_delta_x(gc);
    return enif_make_tuple2(env, enif_make_int(env,x), enif_make_int(env,y));
}

// Draw string with io-list utf8
static ERL_NIF_TERM font_draw_utf8(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_gc_t* gc;
    epx_pixmap_t* dst;
    epx_t2d_t* ctm;    
    int x, y;
    ErlNifBinary bin;

    if (!get_texture(env, argv[0], &dst))
	return enif_make_badarg(env);
    ctm = dst ? dst->ctm : NULL;    
    if (!get_gc(env, argv[1], &gc))
	return enif_make_badarg(env);
    if (!get_coord(env, ctm, argv[2], argv[3], &x, &y))
	return enif_make_badarg(env);    
    if (!enif_inspect_iolist_as_binary(env, argv[4], &bin))
	return enif_make_badarg(env);
    epx_font_draw_utf8(gc, dst, &x, &y, (char*)bin.data, bin.size);
    return enif_make_tuple2(env, enif_make_int(env,x), enif_make_int(env,y));
}

/******************************************************************************
 *
 *   reaper_main
 *      thread for killing other threads
 *
 *****************************************************************************/

static void* reaper_main(void* arg)
{
    epx_thread_t* self = arg;

    DEBUGF("reaper: started (addr=%p, %p)", &self, self);

    while(1) {
	epx_message_t m;
	epx_thread_t* from = 0;
	void* exit_value;

	epx_message_recv(self, &from, &m);

	switch(m.type) {
	case EPX_MESSAGE_KILL:
	    DEBUGF("reaper_main: kill thread=%p", m.thread);
	    epx_thread_stop(m.thread, self, &exit_value);
	    break;

	case EPX_MESSAGE_UPGRADE:
	    DEBUGF("reaper_main: upgrade func=%p", m.upgrade);
	    return (*m.upgrade)(arg);

	case EPX_MESSAGE_SYNC:
	    DEBUGF("reaper_main: sync");
	    m.type = EPX_MESSAGE_SYNC_ACK;
	    epx_queue_put(m.reply_q, &m);
	    break;

	case EPX_MESSAGE_STOP:
	    DEBUGF("reaper: stoppped by command");
	    epx_thread_exit(self);
	    break;
	case EPX_MESSAGE_POLL:
	case EPX_MESSAGE_EVENT_READY:
	case EPX_MESSAGE_WINDOW_ATTACH:
	case EPX_MESSAGE_WINDOW_DETACH:
	case EPX_MESSAGE_WINDOW_ADJUST:
	case EPX_MESSAGE_WINDOW_SWAP:
	case EPX_MESSAGE_BACKEND_ADJUST:
	case EPX_MESSAGE_PIXMAP_ATTACH:
	case EPX_MESSAGE_PIXMAP_DETACH:
	case EPX_MESSAGE_PIXMAP_DRAW:
	case EPX_MESSAGE_WINDOW_SYNC:
	case EPX_MESSAGE_SYNC_ACK:
	default:
	    DEBUGF("reaper: unhandled message %s",
		       format_message(&m));
	    break;
	}
    }
    return 0;
}

/******************************************************************************
 *
 *   backend_poll
 *      thread for polling backends for events
 *****************************************************************************/

static int backend_poll_dispatch(epx_thread_t* self, epx_message_t* mp)
{
    switch(mp->type) {
    case EPX_MESSAGE_POLL:
	return 1;
    case EPX_MESSAGE_UPGRADE:
	return 2;
    case EPX_MESSAGE_SYNC: {
	epx_message_t m = *mp;
	DEBUGF("backend_poll_dispatch: sync");
	m.type = EPX_MESSAGE_SYNC_ACK;
	epx_queue_put(mp->reply_q, &m);
	DEBUGF("backend_poll_dispatch: sync ack sent");
	break;
    }
    case EPX_MESSAGE_STOP:
	DEBUGF("backend_poll_dispatch: stoppped by command");
	epx_thread_exit(self);
	break;
    case EPX_MESSAGE_KILL:
    case EPX_MESSAGE_EVENT_READY:
    case EPX_MESSAGE_WINDOW_ATTACH:
    case EPX_MESSAGE_WINDOW_DETACH:
    case EPX_MESSAGE_WINDOW_ADJUST:
    case EPX_MESSAGE_WINDOW_SWAP:
    case EPX_MESSAGE_BACKEND_ADJUST:
    case EPX_MESSAGE_PIXMAP_ATTACH:
    case EPX_MESSAGE_PIXMAP_DETACH:
    case EPX_MESSAGE_PIXMAP_DRAW:
    case EPX_MESSAGE_WINDOW_SYNC:
    case EPX_MESSAGE_SYNC_ACK:
    default:
	DEBUGF("backend_poll_dispatch: unhandled message %s",
	       format_message(mp));
	break;
    }
    return 0;
}


static void* backend_poll(void* arg)
{
    epx_thread_t* self = arg;

    DEBUGF("backend_poll: started (addr=%p,%p,priv=%p)",
	       &self, self, self->priv);

    while(1) {
	epx_message_t message;
	epx_thread_t* from  = NULL;

	epx_message_recv(self, &from, &message);

	switch(backend_poll_dispatch(self, &message)) {
	case 0: break;
	poll_message:
	case 1: {
	    struct pollfd fds[2];
	    int nfd;
	    int n;
	    int timeout = 1000;

	    fds[0].fd = (int)((long)message.handle);
	    fds[0].events = POLLIN;
	    DEBUGF("backend_poll: poll fds[0]=%ld", fds[0].fd);
	    
	    nfd = 1;
	    if (self->wake[0] >= 0) {
		fds[1].fd = self->wake[0];
		fds[1].events = POLLIN;
		timeout = -1;   // timeout not needed
		nfd++;
		DEBUGF("backend_poll: poll fds[1]=%ld", fds[1].fd);
	    }

	    if ((n = poll(fds, nfd, timeout)) > 0) {
		if (fds[0].revents & POLLIN) {
		    DEBUGF("backend_poll: read fd=%d", fds[0].fd);
		    message.type = EPX_MESSAGE_EVENT_READY;
		    epx_message_send(from, self, &message);
		}
		else if ((nfd>1) && (fds[1].revents & POLLIN)) {
		    // system message!
		    epx_message_t sys_message;
		    epx_thread_t* sys_from;
		    epx_message_recv(self, &sys_from, &sys_message);
		    switch(backend_poll_dispatch(self, &sys_message)) {
		    case 0: goto poll_message;
		    case 1: goto poll_message;
		    case 2: return (*sys_message.upgrade)(arg);
		    default: goto poll_message;
		    }
		}
	    }
	    else if (n == 0) {           // timeout
		goto poll_message;
	    }
	    else if (n < 0) {
		ERRORF("backend_poll: error %s (%d)", errno, strerror(errno));
		// check EAGAIN or real error, loop or die
		goto poll_message;
	    }
	    break;
	}
	case 2: return (*message.upgrade)(arg);
	default: goto poll_message;
	}
    }
    return 0;
}



/******************************************************************************
 *
 * Backend main loop run as a thread.
 *  The main purpose is to dispatch and send messages to window owners
 *
 *****************************************************************************/

// private thread data
typedef struct {
    ErlNifEnv*    env;       // thread environment
    EPX_HANDLE_T  handle;
    epx_thread_t* poll_thr;
} backend_priv_t;

static void* backend_main(void* arg)
{
    epx_thread_t* self = arg;
    epx_backend_t* backend = self->arg;
    backend_priv_t* priv;
    epx_message_t m;

    DEBUGF("backend_main: started (addr=%p,%p,priv=%p)",
	       &self, self, self->priv);

    if ((priv = (backend_priv_t*) self->priv) == NULL) {
	priv = enif_alloc(sizeof(backend_priv_t));
	priv->env = enif_alloc_env();
	priv->handle = epx_backend_event_attach(backend);
	priv->poll_thr = epx_thread_start(backend_poll, 0,
					  DO_WAKEUP, 
					  DO_NOT_STEAL,
					  STACK_SIZE_IN_KBYTES(4));
	self->priv = priv;
    }

    m.type = EPX_MESSAGE_POLL;  // start/restart polling
    m.handle = priv->handle;
    epx_message_send(priv->poll_thr, self, &m);

    DEBUGF("backend_main: send EPX_MESSAGE_POLL handle=%ld",
	       priv->handle);

    epx_object_ref(backend);

    while(1) {
	epx_thread_t* from = 0;

	epx_object_unref(backend);

	epx_message_recv(self, &from, &m);

	if (m.type == EPX_MESSAGE_STOP) {
	    void* poll_thr_exit = 0;
	    DEBUGF("backend_main: stopped by command");
	    enif_free_env(priv->env);
	    epx_thread_stop(priv->poll_thr, self, &poll_thr_exit);
	    DEBUGF("backend_main: unref backend refc=%d", epx_object_refc(backend));
	    epx_object_unref(backend);  // release real backend
	    enif_free(priv);            // release private data
	    self->priv = NULL;
	    epx_thread_exit(self);
	}
	epx_object_ref(backend);

	switch(m.type) {
	case EPX_MESSAGE_UPGRADE: {
	    int r;
	    void* (*upgrade)(void*) = m.upgrade;
	    DEBUGF("backend_main: upgrade func=%p", upgrade);
	    m.upgrade = backend_poll; // relay to poll thread
	    epx_message_send(priv->poll_thr, 0, &m);
	    epx_object_unref(backend);
	    r = epx_backend_upgrade(backend);
	    DEBUGF("backend_main: backend %s upgrade status = %d",
		       backend->name, r);
	    return (*upgrade)(arg);   // tail call?
	}

	case EPX_MESSAGE_SYNC:
	    DEBUGF("backend_main: sync");
	    epx_message_send(priv->poll_thr, 0, &m); // relay to poll thread
	    m.type = EPX_MESSAGE_SYNC_ACK;
	    epx_queue_put(m.reply_q, &m);
	    DEBUGF("backend_main: sync ack sent");
	    break;

	case EPX_MESSAGE_EVENT_READY: {
	    int n;
	    epx_event_t evt;

	    DEBUGF("backend_main: EPX_MESSAGE_EVENT_READY");
	    while ((n = epx_backend_event_read(backend, &evt)) > 0) {
		ErlNifPid* pid = evt.window->owner;
		// send event to window owner
		if (pid) {
		    ERL_NIF_TERM msg = make_event(priv->env, &evt);
		    enif_send(0, pid, priv->env, msg);
		    enif_clear_env(priv->env);
		}
		DEBUGF("backend_main: got event %08X n=%d", evt.type, n);
		if (n == 1)
		    break;
	    }
	    DEBUGF("backend_main: send EPX_MESSAGE_POLL");
	    m.type = EPX_MESSAGE_POLL;  // poll for more events
	    m.handle = priv->handle;
	    epx_message_send(priv->poll_thr, self, &m);
	    break;
	}

	case EPX_MESSAGE_WINDOW_ATTACH:
	    DEBUGF("backend_main: EPX_MESSAGE_WINDOW_ATTACH");
	    DEBUGF("  window=%p, owner=%p", m.window, m.window->owner);
	    if (m.window->owner != NULL)
		epx_backend_window_attach(backend, m.window);
	    DEBUGF("backend_main: window_attach unref: m.window %p", m.window);
	    epx_object_unref(m.window);
	    break;

	case EPX_MESSAGE_WINDOW_DETACH:
	    DEBUGF("backend_main: EPX_MESSAGE_WINDOW_DETACH");
	    DEBUGF("  window=%p, owner=%p", m.window, m.window->owner);
	    if (m.window->owner != NULL)	    
		epx_window_detach(m.window);
	    DEBUGF("   unref m.window->user %p", m.window->user);
	    epx_object_unref(m.window->user);
	    m.window->user = 0;
	    DEBUGF("    unref: m.window %p", m.window);
	    epx_object_unref(m.window);
	    break;

	case EPX_MESSAGE_WINDOW_ADJUST:
	    DEBUGF("backend_main: EPX_MESSAGE_WINDOW_ADJUST");
	    DEBUGF("  window=%p, owner=%p", m.wadjust.window,
		   m.wadjust.window->owner);
	    if (m.wadjust.window->owner != NULL)
		epx_window_adjust(m.wadjust.window, m.wadjust.param);
	    DEBUGF("    unref: m.window %p", m.wadjust.window);
	    epx_object_unref(m.wadjust.window);
	    DEBUGF("    unref: m.wadjust.param %p", m.wadjust.param);
	    epx_object_unref(m.wadjust.param);    // thread safe?
	    break;

	case EPX_MESSAGE_WINDOW_SWAP:
	    DEBUGF("backend_main: EPX_MESSAGE_WINDOW_SWAP");
	    DEBUGF("  window=%p, owner=%p", m.window, m.window->owner);
	    if (m.window->owner != NULL)
		epx_window_swap(m.window);	    
	    DEBUGF("    unref: m.window %p", m.window);
	    epx_window_swap(m.window);	    
	    break;

	case EPX_MESSAGE_BACKEND_ADJUST:
	    DEBUGF("backend_main: EPX_MESSAGE_BACKEND_ADJUST");
	    if (backend->owner != NULL)
		epx_backend_adjust(backend, m.param);
	    DEBUGF("    unref: m.param %p", m.param);
	    epx_object_unref(m.param);    // thread safe?	    
	    break;

	case EPX_MESSAGE_PIXMAP_ATTACH:
	    DEBUGF("backend_main: EPX_MESSAGE_PIXMAP_ATTACH");
	    epx_backend_pixmap_attach(backend, m.pixmap);
	    DEBUGF("    unref: m.pixmap %p", m.pixmap);
	    epx_object_unref(m.pixmap);  // thread safe?
	    break;

	case EPX_MESSAGE_PIXMAP_DETACH:
	    DEBUGF("backend_main: EPX_MESSAGE_PIXMAP_DETACH");
	    DEBUGF("    owner %p", m.pixmap->owner);
	    epx_pixmap_detach(m.pixmap);
	    DEBUGF("    unref: m.pixmap->user %p", m.pixmap->user);
	    epx_object_unref(m.pixmap->user);
	    m.pixmap->user = 0;
	    epx_object_unref(m.pixmap);
	    break;

	case EPX_MESSAGE_PIXMAP_DRAW:
	    DEBUGF("backend_main: EPX_MESSAGE_PIXMAP_DRAW");
	    DEBUGF("  window=%p, owner=%p", m.wdraw.window,
		   m.wdraw.window->owner);
	    if ((m.wdraw.window->owner != NULL) &&
		!epx_backend_draw_begin(backend, m.wdraw.window)) {
		epx_backend_pixmap_draw(backend, m.wdraw.pixmap, m.wdraw.window,
					m.wdraw.src_x, m.wdraw.src_y,
					m.wdraw.dst_x, m.wdraw.dst_y,
					m.wdraw.width, m.wdraw.height);
		epx_backend_draw_end(backend, m.wdraw.window, 0);
		epx_window_swap(m.wdraw.window);
	    }
	    epx_object_unref(m.wdraw.pixmap);
	    epx_object_unref(m.wdraw.window);
	    break;

	case EPX_MESSAGE_WINDOW_SYNC: {
	    ERL_NIF_TERM msg;
	    ErlNifPid* pid;
	    DEBUGF("backend_main: EPX_MESSAGE_WINDOW_SYNC");
	    DEBUGF("  window=%p, owner=%p", m.window, m.window->owner);

	    pid = m.window->owner;
	    if (pid != NULL) {
		epx_backend_pixmap_sync(backend, NULL, m.window);	
		msg = enif_make_tuple3(priv->env,
				       ATOM(epx_event),
				       make_window(priv->env, m.window),
				       ATOM(synced));
		enif_send(0, pid, priv->env, msg);
		enif_clear_env(priv->env);
	    }
	    DEBUGF("    unref: m.window %p", m.window);
	    epx_object_unref(m.window);
	    break;
	}

	case EPX_MESSAGE_POLL:
	case EPX_MESSAGE_KILL:
	case EPX_MESSAGE_STOP:
	case EPX_MESSAGE_SYNC_ACK:
	default:
	    DEBUGF("backend_main: unhandled message %s",
		       format_message(&m));
	    break;
	}
    }

    return 0;
}

static ERL_NIF_TERM backend_list(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    char* name;
    ERL_NIF_TERM list = enif_make_list(env, 0);
    int i = 0;

    // determine number of backends
    while(epx_backend_name(i))
	i++;

    while(i > 0) {
	ERL_NIF_TERM item;
	i--;
	name = epx_backend_name(i);
	item = enif_make_string(env, name, ERL_NIF_LATIN1);
	list = enif_make_list_cell(env, item, list);
    }
    return list;
}

static ERL_NIF_TERM backend_open(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    (void) argc;
    char name[256];
    epx_dict_t* param;
    epx_backend_t* backend;
    epx_nif_backend_t* nif_backend;
    epx_ctx_t* ctx = (epx_ctx_t*) enif_priv_data(env);
    ERL_NIF_TERM res;

    if (!enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (!get_dict(env, argv[1], &param))
	return enif_make_badarg(env);
#ifdef __APPLE__
    param->opaque = epx_posix_steal_pthread;
#endif
    if (!(backend = epx_backend_create(name, param)))
	return enif_make_badarg(env);
    
    nif_backend = enif_alloc_resource(backend_res, sizeof(epx_nif_backend_t));
    if (!nif_backend)
	return enif_make_badarg(env);
    backend->owner = (void*) enif_self(env, &nif_backend->own);
    backend->res = nif_backend;
    nif_backend->backend = backend;
    nif_backend->next = NULL;

    // start the backend thread stack size = default - FIXME handle error
    nif_backend->main = epx_thread_start(backend_main, backend,
					 DO_NOT_WAKEUP, 
					 DO_NOT_STEAL,
					 STACK_SIZE_AUTO);

    // add backend to list
    enif_rwlock_rwlock(ctx->backend_list_lock);
    nif_backend->next = ctx->backend_list;
    ctx->backend_list = nif_backend;
    enif_rwlock_rwunlock(ctx->backend_list_lock);

    res = make_object(env, (epx_object_t*) backend);
    enif_release_resource(nif_backend);
    return res;
}

static ERL_NIF_TERM backend_info(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;

    if (!get_backend(env, argv[0], &backend))
	return enif_make_badarg(env);

    if (argv[1] == ATOM(name)) {  // number of events pending
	return enif_make_string(env, backend->name, ERL_NIF_LATIN1);
    }
    else if (argv[1] == ATOM(pending)) {  // number of events pending
	return enif_make_int(env, backend->pending);
    }
    else if (argv[1] == ATOM(opengl)) { // opengl support
	return make_bool(env, backend->opengl);
    }
    else if (argv[1] == ATOM(use_opengl)) { // opengl used
	return make_bool(env, backend->use_opengl);
    }
    else if (argv[1] == ATOM(windows)) { // windows attached
	epx_window_t* window;
	ERL_NIF_TERM list = enif_make_list(env, 0);

	epx_lock_lock(backend->window_list.lock);
	window = (epx_window_t*) backend->window_list.first;

	while(window) {
	    list =
		enif_make_list_cell(env,
				    make_window(env,window),
				    list);
	    window = window->next;
	}
	epx_lock_unlock(backend->window_list.lock);
	return list;
    }
    else if (argv[1] == ATOM(pixmaps)) { // pixmaps attached
	epx_pixmap_t* pixmap;
	ERL_NIF_TERM list = enif_make_list(env, 0);

	epx_lock_lock(backend->pixmap_list.lock);
	pixmap = (epx_pixmap_t*) backend->pixmap_list.first;

	while(pixmap) {
	    list =
		enif_make_list_cell(env,
				    make_pixmap(env,pixmap),
				    list);
	    pixmap = pixmap->next;
	}
	epx_lock_unlock(backend->pixmap_list.lock);
	return list;
    }
    else if (argv[1] == ATOM(width)) { // width of display
	return enif_make_int(env, backend->width);
    }
    else if (argv[1] == ATOM(height)) { // height of display
	return enif_make_int(env, backend->height);
    }
    else if (argv[1] == ATOM(epx_pixel_formats)) { // pixmap formats supported
	int i;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	for (i = backend->nformats-1; i >= 0; i--) {
	    ERL_NIF_TERM hd = make_epx_pixel_format(env,backend->formats[i]);
	    list = enif_make_list_cell(env,hd,list);
	}
	return list;
    }
    else if (argv[1] == ATOM(pixel_formats)) { // as epx_pixel_formats but names
	int i;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	for (i = backend->nformats-1; i >= 0; i--) {
	    ERL_NIF_TERM hd = make_pixel_format(env,backend->formats[i]);
	    list = enif_make_list_cell(env,hd,list);
	}
	return list;
    }
    else {
	char namebuf[256];
	if (enif_get_atom(env,argv[1],namebuf,sizeof(namebuf),ERL_NIF_LATIN1)) {
	    epx_dict_t d;
	    ERL_NIF_TERM r;
	    
	    epx_dict_init(&d);
	    epx_dict_set_boolean(&d, namebuf, 0);

	    if (epx_backend_info(backend, &d)) {
		epx_dict_data_t k;
		int ix;
		k.type = EPX_DICT_STRING;
		k.u.v_string.ptr = namebuf;
		k.u.v_string.len = strlen(namebuf);
		if ((ix = epx_dict_lookup_ix(&d, &k)) < 0)
		    return enif_make_badarg(env);
		r = make_entry(env, d.entry[ix]);
	    }
	    else
		r = enif_make_badarg(env);
	    epx_dict_destroy(&d);
	    return r;
	}
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM backend_adjust(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;
    epx_dict_t* param;
    epx_message_t m;

    if (!get_backend(env, argv[0], &backend))
	return enif_make_badarg(env);    
    if (!get_dict(env, argv[1], &param))
	return enif_make_badarg(env);
    m.type = EPX_MESSAGE_BACKEND_ADJUST;
    m.param = param;
    epx_object_ref(m.param);
    backend_send(backend, &m);
    return ATOM(ok);    
}


// Windows
static ERL_NIF_TERM window_create(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    int x, y;
    unsigned int width;
    unsigned int height;
    epx_window_t* window = 0;
    ERL_NIF_TERM t;
    uint32_t events = 0;
    epx_nif_window_t* nw;

    if (!get_coord(env, NULL, argv[0], argv[1], &x, &y))
	return enif_make_badarg(env);
    if (!get_dim(env, NULL, argv[2], argv[3], &width, &height))
	return enif_make_badarg(env);
    if (argc == 5) {
	if (!get_event_flags(env, argv[4], &events))
	    return enif_make_badarg(env);
    }
    if ((window = epx_window_create(x, y, width, height)) == NULL)
	return enif_make_badarg(env);
    nw = enif_alloc_resource(window_res, sizeof(epx_nif_window_t));
    nw->window = window;
    window->owner = (void*) enif_self(env, &nw->own);
    window->res = nw;
    epx_window_set_event_mask(window, events);
    t = make_window(env,window);
    enif_release_resource(nw);
    return t;
}

static ERL_NIF_TERM window_adjust(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;
    epx_window_t* window;
    epx_dict_t* param;
    epx_message_t m;

    if (!get_window(env, argv[0], &window))
	return enif_make_badarg(env);
    if (!get_dict(env, argv[1], &param))
	return enif_make_badarg(env);

    if (!(backend = window->user))
	return enif_make_badarg(env);

    m.type = EPX_MESSAGE_WINDOW_ADJUST;
    m.wadjust.window = window;
    m.wadjust.param  = param;
    epx_object_ref(m.wadjust.window);
    epx_object_ref(m.wadjust.param);
    backend_send(backend, &m);
    return ATOM(ok);
}

static ERL_NIF_TERM window_info(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_window_t* win;

    if (!get_window(env, argv[0], &win))        
	return enif_make_badarg(env);
    if (argv[1] == ATOM(x)) {
	return enif_make_int(env, win->rarea.xy.x);
    }
    else if (argv[1] == ATOM(y)) {
	return enif_make_uint(env, win->rarea.xy.y);
    }
    else if (argv[1] == ATOM(width)) {
	return enif_make_uint(env, win->rarea.wh.width);
    }
    else if (argv[1] == ATOM(height)) {
	return enif_make_uint(env, win->rarea.wh.height);
    }
    else if (argv[1] == ATOM(backend)) {
	epx_backend_t* backend = (epx_backend_t*) win->user;
	if (!backend)
	    return ATOM(undefined);
	else
	    return make_object(env,(epx_object_t*) backend);
    }
    else if (argv[1] == ATOM(event_mask)) {
	return make_event_flags(env, win->mask);
    }
    else {
	char namebuf[256];
	if (enif_get_atom(env,argv[1],namebuf,sizeof(namebuf),ERL_NIF_LATIN1)) {
	    epx_dict_t d;
	    ERL_NIF_TERM r;
	    
	    epx_dict_init(&d);
	    epx_dict_set_boolean(&d, namebuf, 0);

	    if (epx_window_info(win, &d)) {
		epx_dict_data_t k;
		int ix;
		k.type = EPX_DICT_STRING;
		k.u.v_string.ptr = namebuf;
		k.u.v_string.len = strlen(namebuf);
		if ((ix = epx_dict_lookup_ix(&d, &k)) < 0)
		    return enif_make_badarg(env);
		r = make_entry(env, d.entry[ix]);
	    }
	    else
		r = enif_make_badarg(env);
	    epx_dict_destroy(&d);
	    return r;
	}
	return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM window_attach(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;    
    epx_window_t* window;
    epx_message_t m;

    if (!get_window(env, argv[0], &window))
	return enif_make_badarg(env);
    if (!get_backend(env, argv[1], &backend))
	return enif_make_badarg(env);

    if (window->user)
	return enif_make_badarg(env);

    epx_object_ref(backend);    // window reference backend
    window->user = backend;
    m.type = EPX_MESSAGE_WINDOW_ATTACH;
    m.window = window;
    epx_object_ref(m.window);
    backend_send(backend, &m);
    return ATOM(ok);
}


static ERL_NIF_TERM window_detach(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;
    epx_window_t* window;
    epx_message_t m;

    if (!get_window(env, argv[0], &window))
	return enif_make_badarg(env);
    if (!(backend = window->user))
	return enif_make_badarg(env);

    m.type = EPX_MESSAGE_WINDOW_DETACH;
    m.window = window;
    epx_object_ref(m.window);
    backend_send(backend, &m);
    return ATOM(ok);
}

static ERL_NIF_TERM window_set_event_mask(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_window_t* window;
    uint32_t events;

    if (!get_window(env, argv[0], &window))
	return enif_make_badarg(env);
    if (!get_event_flags(env, argv[1], &events))
	return enif_make_badarg(env);
    epx_window_set_event_mask(window, events);
    return ATOM(ok);
}

static ERL_NIF_TERM window_enable_events(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_window_t* window;
    uint32_t events;

    if (!get_window(env, argv[0], &window))
	return enif_make_badarg(env);
    if (!get_event_flags(env, argv[1], &events))
	return enif_make_badarg(env);
    epx_window_enable_events(window, events);
    return ATOM(ok);
}

static ERL_NIF_TERM window_disable_events(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_window_t* window;
    uint32_t events;

    if (!get_window(env, argv[0], &window))
	return enif_make_badarg(env);
    if (!get_event_flags(env, argv[1], &events))
	return enif_make_badarg(env);
    epx_window_disable_events(window, events);
    return ATOM(ok);
}

static void load_atoms(ErlNifEnv* env,epx_ctx_t* ctx)
{
    (void) ctx;
    // Load atoms
    LOAD_ATOM(ok);
    LOAD_ATOM(true);
    LOAD_ATOM(false);
    LOAD_ATOM(undefined);
    LOAD_ATOM(error);

    // debug
    LOAD_ATOM(debug);
    LOAD_ATOM(info);
    LOAD_ATOM(notice);
    LOAD_ATOM(warning);
    LOAD_ATOM(error);
    LOAD_ATOM(critical);
    LOAD_ATOM(alert);
    LOAD_ATOM(emergency);
    LOAD_ATOM(none);

    // Type names
    LOAD_ATOM(epx_pixmap);
    LOAD_ATOM(epx_bitmap);    
    LOAD_ATOM(epx_window);
    LOAD_ATOM(epx_backend);
    LOAD_ATOM(epx_dict);
    LOAD_ATOM(epx_gc);
    LOAD_ATOM(epx_font);
    LOAD_ATOM(epx_animation);
    LOAD_ATOM(epx_canvas);
    LOAD_ATOM(epx_poly);

    // Flags
    LOAD_ATOM(solid);
    LOAD_ATOM(blend);
    LOAD_ATOM(sum);
    LOAD_ATOM(aalias);
    LOAD_ATOM(textured);
    LOAD_ATOM(nfirst);
    LOAD_ATOM(nlast);
    LOAD_ATOM(dashed);
    LOAD_ATOM(ntop);
    LOAD_ATOM(nright);
    LOAD_ATOM(nbottom);
    LOAD_ATOM(nleft);
    LOAD_ATOM(nborder);
    LOAD_ATOM(outside);
    LOAD_ATOM(inside);
    LOAD_ATOM(center);    

    // simd_info
    LOAD_ATOM(emu);
    LOAD_ATOM(altivec);
    LOAD_ATOM(mmx);
    LOAD_ATOM(sse2);
    LOAD_ATOM(avx2);
    LOAD_ATOM(neon);
    LOAD_ATOM(auto);
    LOAD_ATOM(accel);
    LOAD_ATOM(functions);
    LOAD_ATOM(cpu_features);
    LOAD_ATOM(cpu_vendor_name);
    LOAD_ATOM(cpu_brand_string);
    LOAD_ATOM(cpu_serial_number);
    LOAD_ATOM(cpu_cache_line_size);

    // pixmap_info
    LOAD_ATOM(width);
    LOAD_ATOM(height);
    LOAD_ATOM(bytes_per_row);
    LOAD_ATOM(bits_per_pixel);
    LOAD_ATOM(bytes_per_pixel);
    LOAD_ATOM(pixel_format);
    LOAD_ATOM(epx_pixel_format);
    LOAD_ATOM(parent);
    LOAD_ATOM(clip);
    LOAD_ATOM(backend);

    // animation_info
    LOAD_ATOM(count);

    // window_info
    LOAD_ATOM(x);
    LOAD_ATOM(y);    
    LOAD_ATOM(event_mask);

    // poly_info
    LOAD_ATOM(box);
    LOAD_ATOM(edge);
    LOAD_ATOM(sedge);
    LOAD_ATOM(vertex);
    LOAD_ATOM(pos);
    LOAD_ATOM(slope);

    // poly flags
    LOAD_ATOM(absolute);  // default
    LOAD_ATOM(relative);

    // epx_cap_style_t
    LOAD_ATOM(none);
    LOAD_ATOM(butt);
    LOAD_ATOM(round);
    LOAD_ATOM(projecting);

    // epx_join_style_t
    LOAD_ATOM(miter);
    LOAD_ATOM(round);
    LOAD_ATOM(bevel);

    // gc_get
    LOAD_ATOM(fill_style);
    LOAD_ATOM(fill_color);
    LOAD_ATOM(fill_texture);
    LOAD_ATOM(line_style);
    LOAD_ATOM(line_join_style);
    LOAD_ATOM(line_cap_style);
    LOAD_ATOM(line_width);
    LOAD_ATOM(line_texture);
    LOAD_ATOM(border_style);
    LOAD_ATOM(border_join_style);
    LOAD_ATOM(border_cap_style);
    LOAD_ATOM(border_color);
    LOAD_ATOM(border_width);
    LOAD_ATOM(border_texture);
    LOAD_ATOM(foreground_color);
    LOAD_ATOM(background_color);
    LOAD_ATOM(fader_value);
    LOAD_ATOM(font);
    LOAD_ATOM(glyph_delta_x);
    LOAD_ATOM(glyph_delta_y);
    LOAD_ATOM(glyph_fixed_width);
    LOAD_ATOM(glyph_dot_kern);

    // backend info
    LOAD_ATOM(name);
    LOAD_ATOM(width);
    LOAD_ATOM(height);
    LOAD_ATOM(pending);
    LOAD_ATOM(opengl);
    LOAD_ATOM(use_opengl);
    LOAD_ATOM(windows);
    LOAD_ATOM(pixmaps);
    LOAD_ATOM(pixel_formats);
    LOAD_ATOM(epx_pixel_formats);

    // dict info
    LOAD_ATOM(size);
    LOAD_ATOM(sorted);

    // font_info atoms
    LOAD_ATOM(file_name);
    LOAD_ATOM(file_size);
    LOAD_ATOM(foundry_name);
    LOAD_ATOM(family_name);
    LOAD_ATOM(weight);
    LOAD_ATOM(slant);
    LOAD_ATOM(width);
    LOAD_ATOM(style);
    LOAD_ATOM(spacing);
    LOAD_ATOM(pixel_format);
    LOAD_ATOM(epx_pixel_format);
    LOAD_ATOM(pixel_size);
    LOAD_ATOM(point_size);
    LOAD_ATOM(resolution_x);
    LOAD_ATOM(resolution_y);
    LOAD_ATOM(descent);
    LOAD_ATOM(ascent);

    // Glyph info
    // LOAD_ATOM(name);    
    // LOAD_ATOM(width);
    // LOAD_ATOM(height);
    // LOAD_ATOM(x);    
    // LOAD_ATOM(y);    
    LOAD_ATOM(dx);
    LOAD_ATOM(dy);        

    // epx_font_spacing_t
    LOAD_ATOM(none);
    LOAD_ATOM(none);
    LOAD_ATOM(proportional);
    LOAD_ATOM(monospaced);
    LOAD_ATOM(char_cell);

    // epx_font_style_t
    LOAD_ATOM(none);
    LOAD_ATOM(serif);
    LOAD_ATOM(sans_serif);
    LOAD_ATOM(informal);
    LOAD_ATOM(decorated );

    // epx_font_width_t
    LOAD_ATOM(none);
    LOAD_ATOM(normal);
    LOAD_ATOM(condensed);
    LOAD_ATOM(narrow);
    LOAD_ATOM(double_wide);

    // epx_font_slant_t
    LOAD_ATOM(none);
    LOAD_ATOM(roman);
    LOAD_ATOM(italic);
    LOAD_ATOM(oblique);
    LOAD_ATOM(reverse_italic);
    LOAD_ATOM(reverse_oblique);
    LOAD_ATOM(other);

    // epx_font_weight_t
    LOAD_ATOM(none);
    LOAD_ATOM(medium);
    LOAD_ATOM(bold);
    LOAD_ATOM(demibold);

    LOAD_ATOM(epx_event);
    // Event-keys
    LOAD_ATOM(left);
    LOAD_ATOM(right);
    LOAD_ATOM(up);
    LOAD_ATOM(down);
    LOAD_ATOM(insert);
    LOAD_ATOM(delete);
    LOAD_ATOM(home);
    LOAD_ATOM(end);
    LOAD_ATOM(pageup);
    LOAD_ATOM(pagedown);
    LOAD_ATOM(f1);
    LOAD_ATOM(f2);
    LOAD_ATOM(f3);
    LOAD_ATOM(f4);
    LOAD_ATOM(f5);
    LOAD_ATOM(f6);
    LOAD_ATOM(f7);
    LOAD_ATOM(f8);
    LOAD_ATOM(f9);
    LOAD_ATOM(f10);
    LOAD_ATOM(f11);
    LOAD_ATOM(f12);
    LOAD_ATOM(print);
    LOAD_ATOM(sysreq);
    LOAD_ATOM(pause);
    LOAD_ATOM(break);
    LOAD_ATOM(quit);
    LOAD_ATOM(menu);
    LOAD_ATOM(redraw);
    LOAD_ATOM(ctrl);
    LOAD_ATOM(ctrl_left);
    LOAD_ATOM(ctrl_right);
    LOAD_ATOM(shift);
    LOAD_ATOM(shift_left);
    LOAD_ATOM(shift_right);
    LOAD_ATOM(alt);
    LOAD_ATOM(alt_left);
    LOAD_ATOM(alt_right);
    LOAD_ATOM(meta);
    LOAD_ATOM(meta_left);
    LOAD_ATOM(meta_right);
    LOAD_ATOM(num);
    LOAD_ATOM(caps);
    LOAD_ATOM(altgr);
    LOAD_ATOM(scr);
    // Event-types
    LOAD_ATOM(key_press);
    LOAD_ATOM(key_release);
    LOAD_ATOM(no_auto_repeat);    
    LOAD_ATOM(motion);
    LOAD_ATOM(button_press);
    LOAD_ATOM(button_release);
    LOAD_ATOM(close);
    LOAD_ATOM(destroyed);
    LOAD_ATOM(focus_in);
    LOAD_ATOM(focus_out);
    LOAD_ATOM(focus);
    LOAD_ATOM(enter);
    LOAD_ATOM(leave);
    LOAD_ATOM(configure);
    LOAD_ATOM(expose);
    LOAD_ATOM(resize);
    LOAD_ATOM(crossing);
    LOAD_ATOM(synced);
    // Event-buttons
    LOAD_ATOM(left);
    LOAD_ATOM(right);
    LOAD_ATOM(middle);
    LOAD_ATOM(button);  // button mask
    LOAD_ATOM(wheel_up);
    LOAD_ATOM(wheel_down);
    LOAD_ATOM(wheel_left);
    LOAD_ATOM(wheel_right);
    LOAD_ATOM(wheel);  // wheel mask
    LOAD_ATOM(all);    // all mask

    // Operations
    LOAD_ATOM(clear);
    LOAD_ATOM(src);
    LOAD_ATOM(dst);
    LOAD_ATOM(src_over);
    LOAD_ATOM(dst_over);
    LOAD_ATOM(src_in);
    LOAD_ATOM(dst_in);
    LOAD_ATOM(src_out);
    LOAD_ATOM(dst_out);
    LOAD_ATOM(src_atop);
    LOAD_ATOM(dst_atop);
    LOAD_ATOM(xor);
    LOAD_ATOM(copy);
    LOAD_ATOM(add);
    LOAD_ATOM(sub);
    LOAD_ATOM(src_blend);
    LOAD_ATOM(dst_blend);

    // Pixel formats (subset)
    LOAD_ATOM(argb);
    LOAD_ATOM(rgba);
    LOAD_ATOM(abgr);
    LOAD_ATOM(bgra);
    LOAD_ATOM(rgb);
    LOAD_ATOM(bgr);
    LOAD_ATOM(a8);

    // epx_pixel_format.format
    LOAD_ATOM(rgb4);
    LOAD_ATOM(rgb5);
    LOAD_ATOM(rgb8);
    LOAD_ATOM(rgb10);
    LOAD_ATOM(rgb12);
    LOAD_ATOM(rgb16);
    LOAD_ATOM(rgb332);
    LOAD_ATOM(rgb232);
    LOAD_ATOM(rgb565);
    LOAD_ATOM(yuv8);
    LOAD_ATOM(alpha);
    LOAD_ATOM(gray);
    LOAD_ATOM(red);
    LOAD_ATOM(green);
    LOAD_ATOM(blue);
    LOAD_ATOM(calpha);
}


// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif


// This is a callback from epx to handle locks
epx_lock_t epx_nif_locker(epx_lock_command_t cmd, epx_lock_t lock)
{
    switch(cmd) {
    case EPX_LOCK_CREATE:
	return (epx_lock_t) enif_rwlock_create("epx_nif_lock");
    case EPX_LOCK_DESTROY:
	enif_rwlock_destroy((ErlNifRWLock*)lock);
	return 0;
    case EPX_LOCK_LOCK:
	enif_rwlock_rwlock((ErlNifRWLock*)lock);
	return 0;
    case EPX_LOCK_UNLOCK:
	enif_rwlock_rwunlock((ErlNifRWLock*)lock);
	return 0;
    default:
	return 0;
    }
}

static int epx_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    epx_ctx_t* ctx;
    (void) env;
    (void) load_info;

    epx_set_debug(DLOG_DEFAULT);

    DEBUGF("epx_load");
    epx_init(EPX_SIMD_AUTO);
    epx_lock_init(epx_nif_locker);

    // Create resource types
    pixmap_res = epx_resource_create(env, "epx_pixmap", pixmap_dtor);
    window_res = epx_resource_create(env, "epx_window", window_dtor);    
    backend_res = epx_resource_create(env,"epx_backend", backend_dtor);
    bitmap_res = epx_resource_create(env, "epx_bitmap", object_dtor);
    dict_res = epx_resource_create(env,"epx_dict", object_dtor);
    gc_res = epx_resource_create(env,"epx_gc", object_dtor);
    font_res = epx_resource_create(env,"epx_font", object_dtor);
    animation_res = epx_resource_create(env,"epx_animation", object_dtor);
    canvas_res = epx_resource_create(env,"epx_canvas", object_dtor);
    poly_res = epx_resource_create(env,"epx_poly", object_dtor);

    if ((ctx = (epx_ctx_t*) enif_alloc(sizeof(epx_ctx_t))) == NULL)
	return -1;
    if (epx_queue_init(&ctx->q) < 0)
	return -1;
    if (!(ctx->backend_list_lock = enif_rwlock_create("backend_list_lock")))
	return -1;
    ctx->backend_list = NULL;
    ctx->ref_count = 1;
    ctx->debug = DLOG_DEFAULT;
    ctx->accel = epx_simd_accel();

    // create the "default" gc
    ctx->def_gc = epx_gc_copy(&epx_default_gc);
    alloc_object(gc_res, (epx_object_t*)ctx->def_gc);
    epx_object_ref(ctx->def_gc);

    load_atoms(env, ctx);

    ctx->reaper = epx_thread_start(reaper_main, 0, 
				   DO_NOT_WAKEUP,
				   DO_NOT_STEAL,
				   STACK_SIZE_IN_KBYTES(4));

    *priv_data = ctx;
    return 0;
}

static int epx_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		       ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    epx_message_t m;
    epx_ctx_t* ctx = (epx_ctx_t*) *old_priv_data;
    epx_nif_backend_t* bp;
    int sync_count;

    epx_set_debug(ctx->debug);
    DEBUGF("epx_upgrade");
    epx_init(ctx->accel);
    epx_lock_init(epx_nif_locker);

    ctx->ref_count++;

    pixmap_res = epx_resource_takeover(env, "epx_pixmap", pixmap_dtor);
    window_res = epx_resource_takeover(env, "epx_window", window_dtor);    
    backend_res = epx_resource_takeover(env,"epx_backend", backend_dtor);
    bitmap_res = epx_resource_create(env, "epx_bitmap", object_dtor);    
    dict_res = epx_resource_takeover(env,"epx_dict", object_dtor);
    gc_res = epx_resource_takeover(env,"epx_gc", object_dtor);
    font_res = epx_resource_takeover(env,"epx_font", object_dtor);
    animation_res = epx_resource_takeover(env,"epx_animation", object_dtor);
    canvas_res = epx_resource_takeover(env,"epx_canvas", object_dtor);
    poly_res = epx_resource_takeover(env,"epx_poly", object_dtor);
    
    load_atoms(env, ctx);

    // upgrade reaper
    m.type   = EPX_MESSAGE_UPGRADE;
    m.upgrade = reaper_main;  // the new main funtion
    epx_message_send(ctx->reaper, 0, &m);

    // & sync
    m.type    = EPX_MESSAGE_SYNC;
    m.reply_q = &ctx->q;
    epx_message_send(ctx->reaper, 0, &m);

    sync_count = 1;

    enif_rwlock_rwlock(ctx->backend_list_lock);
    for (bp = ctx->backend_list; bp != NULL; bp = bp->next) {
	m.type   = EPX_MESSAGE_UPGRADE;
	m.upgrade = backend_main;
	DEBUGF("epx_upgrade: send upgrade func=%p to %p",
		   backend_main, bp->main);
	epx_message_send(bp->main, 0, &m);
	m.type   = EPX_MESSAGE_SYNC;
	m.reply_q = &ctx->q;
	DEBUGF("epx_upgrade: send sync to %p", bp->main);
	epx_message_send(bp->main, 0, &m);
	sync_count += (1+1);  // one extra for backend_poll!
    }
    enif_rwlock_rwunlock(ctx->backend_list_lock);

    DEBUGF("upx_upgrade: wait for %d sync", sync_count);
    while(sync_count) {
	epx_message_t m;
	int r;
	if ((r = epx_queue_get(&ctx->q, &m)) < 0)
	    return -1;
	if (m.type != EPX_MESSAGE_SYNC_ACK)
	    return -1;
	sync_count--;
	DEBUGF("upx_upgrade: got sync, %d remain", sync_count);
    }

    *priv_data = *old_priv_data;
    return 0;
}

static void epx_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    epx_ctx_t* ctx = (epx_ctx_t*) priv_data;

    DEBUGF("epx_unload");
    // FIXME: stop all backends?
    if (--ctx->ref_count == 0) {
	void* exit_value;
	// backend should have been stopped already
	DEBUGF("epx_unload: free");
	epx_thread_stop(ctx->reaper, 0, &exit_value);
	enif_free(ctx);
    }
}


ERL_NIF_INIT(epx, epx_funcs,
	     epx_load, NULL,
	     epx_upgrade, epx_unload)
