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
// EPX nif interface
//

#include <string.h>
#include <unistd.h>  // WINDOWS fixme
#include <poll.h>    // WINDOWS fixme
#include <errno.h>

#include "erl_nif.h"
#include "../include/epx.h"

#define MAX_PATH 1024

static int epx_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int epx_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int epx_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info);
static void epx_unload(ErlNifEnv* env, void* priv_data);

// Pixmaps
static ERL_NIF_TERM pixmap_create(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_copy(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_sub_pixmap(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM pixmap_info(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_set_clip(ErlNifEnv* env, int argc, 
				    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_fill(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_copy_to(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_flip(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_scale(ErlNifEnv* env, int argc, 
				 const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_put_pixel(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_put_pixels(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_get_pixel(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_get_pixels(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_copy_area(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_alpha_area(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_fade_area(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_shadow_area(ErlNifEnv* env, int argc, 
				       const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_operation_area(ErlNifEnv* env, int argc, 
					  const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM pixmap_add_color_area(ErlNifEnv* env, int argc, 
					  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_filter_area(ErlNifEnv* env, int argc, 
				       const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_rotate_area(ErlNifEnv* env, int argc, 
				       const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_scroll(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_attach(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_detach(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_draw(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_draw_point(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_draw_line(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_draw_rectangle(ErlNifEnv* env, int argc, 
					  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pixmap_draw_ellipse(ErlNifEnv* env, int argc, 
					const ERL_NIF_TERM argv[]);
// Pixmap animations
static ERL_NIF_TERM animation_open(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM animation_copy(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM animation_draw(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM animation_info(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[]);

// Dictionaries
static ERL_NIF_TERM dict_create(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_copy(ErlNifEnv* env, int argc, 
			      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_set(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_get(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_get_boolean(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_get_integer(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_get_float(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_get_string(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dict_get_binary(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[]);

// Graphic context
static ERL_NIF_TERM gc_create(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gc_copy(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gc_default(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gc_set(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM gc_get(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[]);

// Fonts
static ERL_NIF_TERM font_open(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_load(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_unload(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_map(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_unmap(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_info(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_draw_glyph(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_draw_string(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM font_draw_utf8(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[]);

typedef enum {
    EPX_MESSAGE_STOP,           // time to die
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
    EPX_MESSAGE_ADJUST,         // backend main - set backend parameters
} epx_message_type_t;

struct _epx_thread_t;

typedef struct epx_message_t
{
    epx_message_type_t type;
    int dtor;   // message is sent from destructor
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
    };
} epx_message_t;

typedef struct _epx_qlink_t {
    struct _epx_qlink_t* next;
    epx_message_t mesg;
} epx_qlink_t;

#define MAX_QLINK  8  // pre-allocated qlinks

typedef struct {
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
    int         wake[2]; // wakeup pipe (very unix, fixme)
} epx_thread_t;

// Backend wrapper
typedef struct {
    epx_thread_t*  main;     // main thread
    ErlNifPid*     owner;    // creating erlang process (not used right now)
    epx_backend_t* backend;  // real backend
} epx_nif_backend_t;

// Wrapper to handle reource atom name etc.
typedef struct {
    char* name;
    ERL_NIF_TERM type;       // resource atom name
    ErlNifResourceType* res; // the resource type
} epx_resource_t;

// one reaper thread to kill them all
static epx_thread_t* reaper;

static ERL_NIF_TERM backend_list(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM backend_open(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM backend_info(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM backend_adjust(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[]);



// Windows
static ERL_NIF_TERM window_create(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM window_adjust(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM window_info(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM window_attach(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM window_detach(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM window_set_event_mask(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM window_enable_events(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM window_disable_events(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM simd_info(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM simd_set(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM debug(ErlNifEnv* env, int argc,
			  const ERL_NIF_TERM argv[]);

ErlNifFunc epx_funcs[] =
{
    // Debug 
    { "debug",         1,  debug },
    // Simd interface
    { "simd_info",     1,  simd_info },
    { "simd_set",      1,  simd_set  },

    // Pixmap interface
    { "pixmap_create", 3, pixmap_create },
    { "pixmap_copy",   1, pixmap_copy },
    { "pixmap_info",    2, pixmap_info },
    { "pixmap_sub_pixmap", 5, pixmap_sub_pixmap },
    { "pixmap_set_clip", 2, pixmap_set_clip },
    { "pixmap_fill", 2, pixmap_fill },
    { "pixmap_copy_to", 2, pixmap_copy_to },
    { "pixmap_flip", 1, pixmap_flip },
    { "pixmap_scale", 4, pixmap_scale },
    { "pixmap_put_pixel", 5, pixmap_put_pixel },
    { "pixmap_put_pixels", 8, pixmap_put_pixels },
    { "pixmap_get_pixel", 3, pixmap_get_pixel },
    { "pixmap_get_pixels", 5, pixmap_get_pixels },
    { "pixmap_copy_area", 9, pixmap_copy_area },
    { "pixmap_alpha_area", 9, pixmap_alpha_area },
    { "pixmap_fade_area", 9, pixmap_fade_area },
    { "pixmap_shadow_area", 9, pixmap_shadow_area },
    { "pixmap_operation_area", 9, pixmap_operation_area },
    { "pixmap_add_color_area", 11, pixmap_add_color_area },
    { "pixmap_filter_area", 10, pixmap_filter_area },
    { "pixmap_rotate_area", 12, pixmap_rotate_area },
    { "pixmap_scroll", 6, pixmap_scroll },
    { "pixmap_attach", 2, pixmap_attach },
    { "pixmap_detach", 1, pixmap_detach },
    { "pixmap_draw", 8, pixmap_draw },
    { "pixmap_draw_point", 4, pixmap_draw_point },
    { "pixmap_draw_line", 6, pixmap_draw_line },
    { "pixmap_draw_rectangle", 6, pixmap_draw_rectangle },
    { "pixmap_draw_ellipse", 6, pixmap_draw_ellipse },

    // Dictionary interface
    { "dict_create", 0, dict_create },
    { "dict_copy",   1, dict_copy },
    { "dict_set",    3, dict_set },
    { "dict_get",    2, dict_get },
    { "dict_get_boolean", 2, dict_get_boolean },
    { "dict_get_integer", 2, dict_get_integer },
    { "dict_get_float", 2, dict_get_float },
    { "dict_get_string", 2, dict_get_string },
    { "dict_get_binary", 2, dict_get_binary },

    // Graphic context
    { "gc_create", 0, gc_create },    
    { "gc_copy", 1, gc_copy },
    { "gc_default", 0, gc_default },
    { "gc_set", 3, gc_set },
    { "gc_get", 2, gc_get },

    // Font
    { "font_open", 1, font_open },
    { "font_load", 1, font_load },
    { "font_unload", 1, font_unload },
    { "font_map", 1, font_map },
    { "font_unmap", 1, font_unmap },
    { "font_info", 2, font_info },
    { "font_draw_glyph", 5, font_draw_glyph },
    { "font_draw_string", 5, font_draw_string },
    { "font_draw_utf8", 5, font_draw_utf8 },
    // Backend
    { "backend_list", 0, backend_list },
    { "backend_open", 2, backend_open },
    { "backend_info", 2, backend_info },
    { "backend_adjust", 2, backend_adjust },

    // Window
    { "window_create", 4, window_create },
    { "window_create", 5, window_create },
    { "window_adjust", 2, window_adjust },
    { "window_info",   2, window_info },
    { "window_attach", 2, window_attach },
    { "window_detach", 1, window_detach },
    { "window_set_event_mask", 2, window_set_event_mask },
    { "window_enable_events", 2, window_enable_events },
    { "window_disable_events", 2, window_disable_events },

    { "animation_info", 2, animation_info },
    { "animation_draw", 6, animation_draw },
    { "animation_copy", 6, animation_copy },
    { "animation_open", 1, animation_open },

};

epx_resource_t dict_res;
epx_resource_t pixmap_res;
epx_resource_t gc_res;
epx_resource_t font_res;
epx_resource_t backend_res;
epx_resource_t window_res;
epx_resource_t anim_res;


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
DECL_ATOM(epx_window);
DECL_ATOM(epx_backend);
DECL_ATOM(epx_dict);
DECL_ATOM(epx_gc);
DECL_ATOM(epx_font);
DECL_ATOM(epx_animation);

// simd
DECL_ATOM(emu);
DECL_ATOM(altivec);
DECL_ATOM(mmx);
DECL_ATOM(sse2);
DECL_ATOM(neon);
DECL_ATOM(auto);
DECL_ATOM(accel);
DECL_ATOM(functions);
DECL_ATOM(cpu_features);
DECL_ATOM(cpu_vendor_name);
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
// DECL_ATOM(backend);
DECL_ATOM(event_mask);

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
DECL_ATOM(pending);
DECL_ATOM(opengl);
DECL_ATOM(use_opengl);
DECL_ATOM(windows);
DECL_ATOM(pixmaps);
DECL_ATOM(epx_pixel_formats);

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
DECL_ATOM(resize);
DECL_ATOM(crossing);
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
DECL_ATOM(a8r8g8b8);
DECL_ATOM(rgba);
DECL_ATOM(r8g8b8a8);
DECL_ATOM(abgr);
DECL_ATOM(a8b8g8r8);
DECL_ATOM(bgra);
DECL_ATOM(b8g8r8a8);
DECL_ATOM(rgb);
DECL_ATOM(r8g8b8);
DECL_ATOM(bgr);
DECL_ATOM(b8g8r8);
DECL_ATOM(565);
DECL_ATOM(r5g6b5);
DECL_ATOM(565BE);
DECL_ATOM(r5g6b5BE);
DECL_ATOM(565LE);
DECL_ATOM(r5g6b5LE);
DECL_ATOM(b5g6r5);
DECL_ATOM(b5g6r5BE);
DECL_ATOM(b5g6r5LE);
DECL_ATOM(1555);
DECL_ATOM(a1r5g5b);
DECL_ATOM(gray8a8);
DECL_ATOM(gray16);
DECL_ATOM(a8);
DECL_ATOM(alpha8);
DECL_ATOM(r8);
DECL_ATOM(g8);
DECL_ATOM(b8);
DECL_ATOM(gray8);
DECL_ATOM(efnt2);
DECL_ATOM(a8l8);
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

static epx_gc_t* def_gc = 0;


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
    case EPX_MESSAGE_KILL: return "kill";
    case EPX_MESSAGE_POLL: return "poll";
    case EPX_MESSAGE_EVENT_READY: return "event_ready";
    case EPX_MESSAGE_WINDOW_ATTACH: return "window_attach";
    case EPX_MESSAGE_WINDOW_DETACH: return "window_detach";
    case EPX_MESSAGE_WINDOW_ADJUST: return "window_adjust";
    case EPX_MESSAGE_WINDOW_SWAP: return "window_swap";
    case EPX_MESSAGE_ADJUST: return "adjust";
    case EPX_MESSAGE_PIXMAP_ATTACH: return "pixmap_attach";
    case EPX_MESSAGE_PIXMAP_DETACH: return "pixmap_detach";
    case EPX_MESSAGE_PIXMAP_DRAW: return "pixmap_draw";
    default: return "unknown";
    }
}

static int epx_message_send(epx_thread_t* thr, epx_thread_t* sender,
			    epx_message_t* m)
{
    EPX_DBGFMT("epx_message_send: m=%s to=%p", format_message(m), thr);
    m->sender = sender;
    return epx_queue_put(&thr->q, m);
}

static int epx_message_recv(epx_thread_t* thr, epx_thread_t** from,
			    epx_message_t* m)
{
    int r;
    if ((r = epx_queue_get(&thr->q, m)) < 0)
	return r;
    if (from)
	*from = m->sender;
    return 0;
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

static epx_thread_t* epx_thread_start(void* (*func)(void* arg),
				      void* arg, int wake, int stack_size)
{
    ErlNifThreadOpts* opts;
    epx_thread_t* thr;

    if (!(thr = enif_alloc(sizeof(epx_thread_t))))
	return 0;
    thr->wake[0] = thr->wake[1] = -1;
    if (epx_queue_init(&thr->q) < 0)
	goto error;
    if (wake) {
	if (pipe(thr->wake) < 0)
	    goto error;
    }
    if (!(opts = enif_thread_opts_create("epx_thread_opts")))
	goto error;
    opts->suggested_stack_size = stack_size;
    thr->arg = arg;

    enif_thread_create("epx_thread", &thr->tid, func, thr, opts);
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
    if (thr->wake[1] >= 0) r=write(thr->wake[1], "W", 1);
    r=enif_thread_join(thr->tid, exit_value);
    EPX_DBGFMT("enif_thread_join: return=%d, exit_value=%p", r, *exit_value);
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

/******************************************************************************
 *
 *   Resource destructors
 *
 *****************************************************************************/

static void object_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    EPX_DBGFMT_MEM("Object: destruct obj=%p, ref=%lu",
		   obj, ((epx_object_t*)obj)->refc);
    epx_object_unref(obj);
}

static void backend_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    epx_nif_backend_t* backend = (epx_nif_backend_t*) obj;
    epx_message_t m;

    EPX_DBGFMT_MEM("BACKEND_DTOR: obj=%p, refc=%lu",
		   (backend->backend),
		   ((epx_object_t*) (backend->backend))->refc);

    if (backend->owner) {
	enif_free(backend->owner);
	backend->owner = 0;
    }
    m.type = EPX_MESSAGE_KILL;
    m.thread = backend->main;
    epx_message_send(reaper, 0, &m);
}    

static void pixmap_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    epx_pixmap_t* pixmap = (epx_pixmap_t*) obj;

    EPX_DBGFMT_MEM("PIXMAP_DTOR: obj=%p, refc=%lu",
		   obj, ((epx_object_t*)obj)->refc);
    if (pixmap->user) {
	epx_message_t m;
	epx_nif_backend_t* backend = pixmap->user;

	m.type = EPX_MESSAGE_PIXMAP_DETACH;
	m.dtor = 1;
	m.pixmap = pixmap;
	epx_message_send(backend->main, 0, &m);
    }
    else {
	epx_object_unref(pixmap);
    }

}

static void window_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    epx_window_t* window = (epx_window_t*) obj;

    EPX_DBGFMT_MEM("WINDOW_DTOR: obj=%p, refc=%lu", 
	       obj, ((epx_object_t*)obj)->refc);

    if (window->owner) {
	enif_free(window->owner);
	window->owner = 0;
    }

    if (window->user) {
	epx_message_t m;
	epx_nif_backend_t* backend = window->user;

	m.type = EPX_MESSAGE_WINDOW_DETACH;
	m.dtor = 1;
	m.window = window;
	epx_message_send(backend->main, 0, &m);
    }
    else {
	epx_object_unref(window);
    }
} 

/******************************************************************************
 *
 *   Epx resource init
 *
 *****************************************************************************/

static int epx_resource_init(ErlNifEnv* env,
			     epx_resource_t* res,
			     char* name,
			     ErlNifResourceDtor* dtor,
			     ErlNifResourceFlags flags,
			     ErlNifResourceFlags* tried)
{
    res->name = name;
    res->type = enif_make_atom(env, name);
    res->res  = enif_open_resource_type(env, 0, name, dtor, flags, tried);
    return 0;
}

void* epx_resource_alloc(epx_resource_t* rp, size_t size)
{
    return enif_alloc_resource(rp->res, size);
}


/******************************************************************************
 *
 *   make/get
 *
 *****************************************************************************/

// For now, wrap the resource object {type,pointer-val,handle}
static ERL_NIF_TERM make_object(ErlNifEnv* env,
				const ERL_NIF_TERM type,
				void* robject)
{
    return enif_make_tuple3(env,
			    type,
			    enif_make_ulong(env, (unsigned long) robject),
			    enif_make_resource(env, robject));
}

// Accept {type,pointer-val,handle}
static int get_object(ErlNifEnv* env, const ERL_NIF_TERM term,
		      epx_resource_t* rtype,
		      void** robjectp)
{
    const ERL_NIF_TERM* elem;
    int arity;
    unsigned long handle;

    if (!enif_get_tuple(env, term, &arity, &elem))
	return 0;
    if (arity != 3)
	return 0;
    if (!enif_is_atom(env, elem[0]) || (elem[0] != rtype->type))
	return 0;
    if (!enif_get_ulong(env, elem[1], &handle))
	return 0;
    if (!enif_get_resource(env, elem[2], rtype->res, robjectp))
	return 0;
    if ((unsigned long)*robjectp != handle)
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
    if (mask == 0)
	return ATOM(none);
    else if (mask == EPX_EVENT_ALL)
	return ATOM(all);
    list = enif_make_list(env, 0);

    if (mask & EPX_EVENT_KEY_PRESS)
	list = enif_make_list_cell(env, ATOM(key_press), list);
    if (mask & EPX_EVENT_KEY_RELEASE)
	list = enif_make_list_cell(env, ATOM(key_release), list);
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


static int get_bool(ErlNifEnv* env, const ERL_NIF_TERM term,
		    int* value)
{
    if (term == ATOM(true))
	*value = 1;
    else if (term == ATOM(false))
	*value = 0;
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
// Input styles:  format-name (atom)
//
static int get_pixel_format(ErlNifEnv* env, const ERL_NIF_TERM term,
			    epx_format_t* fmt)
{
    char fmtbuf[256];

    *fmt = EPX_FORMAT_INVALID;
    if (!enif_get_atom(env, term, fmtbuf, sizeof(fmtbuf), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if ((*fmt = epx_pixel_format_from_name(fmtbuf)) == EPX_FORMAT_INVALID)
	return 0;
    return 1;
}

// Return pixel format as an atom
static ERL_NIF_TERM make_pixel_format(ErlNifEnv* env, epx_format_t fmt)
{
    char* name;

    if (!(name = epx_pixel_format_to_name(fmt)))
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
    char* ptr;

    if (!(ptr = epx_pixel_format_to_name(fmt)))
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
static int get_color(ErlNifEnv* env, const ERL_NIF_TERM term,
		     epx_pixel_t* pixel)
{
    uint32_t value;
    char namebuf[256];
    const ERL_NIF_TERM* elem;
    int arity;
    unsigned int a, r, g, b;

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
	if (!enif_get_uint(env, elem[0], &r)) return 0;
	if (!enif_get_uint(env, elem[1], &g)) return 0;
	if (!enif_get_uint(env, elem[2], &b)) return 0;
    }
    else if (arity == 4) {
	if (!enif_get_uint(env, elem[0], &a)) return 0;
	if (!enif_get_uint(env, elem[1], &r)) return 0;
	if (!enif_get_uint(env, elem[2], &g)) return 0;
	if (!enif_get_uint(env, elem[3], &b)) return 0;	
    }
    else
	return 0;
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
// FIXME: make_resouce_binary - if not sub_pixmap
	return make_object(env, ATOM(epx_pixmap),  texture); 
}

static int get_texture(ErlNifEnv* env, const ERL_NIF_TERM term, 
		       epx_pixmap_t** texture)
{
    if (term == ATOM(undefined))
	*texture = 0;
    else if (!get_object(env, term, &pixmap_res, (void**) texture))
	return 0;
    return 1;
}

static int get_font(ErlNifEnv* env, const ERL_NIF_TERM term, 
		    epx_font_t** font)
{
    if (term == ATOM(undefined))
	*font = 0;
    else if (!get_object(env, term, &font_res, (void**) font))
	return 0;
    return 1;
}

static ERL_NIF_TERM make_font(ErlNifEnv* env, epx_font_t* font)
{
    if (!font)
	return ATOM(undefined);
    else
	return make_object(env, ATOM(epx_font), font);
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
//
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
    if (!enif_get_int(env, elem[0], &r.xy.x))
	return 0;
    if (!enif_get_int(env, elem[1], &r.xy.y))
	return 0;
    if (!enif_get_uint(env, elem[2], &r.wh.width))
	return 0;
    if (!enif_get_uint(env, elem[3], &r.wh.height))
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
	    if (e->key.mod & EPX_KBD_MOD_RCTRL)
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
    case EPX_EVENT_RESIZE:
	data = make_dimension_event(env, ATOM(resize), e); break;
    default:
	data = ATOM(undefined);
    }

    return enif_make_tuple3(env, ATOM(epx_event), 
			    make_object(env, ATOM(epx_window), e->window),
			    data);
}

/******************************************************************************
 *
 *****************************************************************************/

static ERL_NIF_TERM debug(ErlNifEnv* env, int argc,
			  const ERL_NIF_TERM argv[])
{
    (void) argc;
    if (argv[0] == ATOM(debug))        epx_set_debug(DLOG_DEBUG);
    else if (argv[0] == ATOM(info))    epx_set_debug(DLOG_INFO);
    else if (argv[0] == ATOM(notice))  epx_set_debug(DLOG_NOTICE);
    else if (argv[0] == ATOM(warning)) epx_set_debug(DLOG_WARNING);
    else if (argv[0] == ATOM(error))   epx_set_debug(DLOG_ERROR);
    else if (argv[0] == ATOM(critical)) epx_set_debug(DLOG_CRITICAL);
    else if (argv[0] == ATOM(alert))    epx_set_debug(DLOG_ALERT);
    else if (argv[0] == ATOM(emergency)) epx_set_debug(DLOG_EMERGENCY);
    else if (argv[0] == ATOM(none))      epx_set_debug(DLOG_NONE);
    else return enif_make_badarg(env);
    return ATOM(ok);
}



/******************************************************************************
 *
 *  simd
 *  acceltype()  :: emu|altivec|mmx|sse2|neon
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
	    else if (epx_simd->type == EPX_SIMD_NEON)    type = ATOM(neon);
	    accel -= epx_simd->type;
	}
	// list types available
	if (accel & EPX_SIMD_EMU)     avail[i++] = ATOM(emu);
	if (accel & EPX_SIMD_ALTIVEC) avail[i++] = ATOM(altivec);
	if (accel & EPX_SIMD_MMX)     avail[i++] = ATOM(mmx);
	if (accel & EPX_SIMD_SSE2)    avail[i++] = ATOM(sse2);
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
					       ATOM(bgra))),
		enif_make_tuple(env, 2, enif_make_atom(env, "blend_area"),
				enif_make_list(env, 4,
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra))),
		enif_make_tuple(env, 2, enif_make_atom(env, "alpha_area"),
				enif_make_list(env, 4,
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra))),
		enif_make_tuple(env, 2, enif_make_atom(env, "fade_area"),
				enif_make_list(env, 4,
					       ATOM(argb),
					       ATOM(abgr),
					       ATOM(rgba),
					       ATOM(bgra))),
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
    (void) argc;
    int accel;
    if (argv[0] == ATOM(none)) accel = EPX_SIMD_NONE;
    else if (argv[0] == ATOM(emu)) accel = EPX_SIMD_EMU;
    else if (argv[0] == ATOM(altivec)) accel = EPX_SIMD_ALTIVEC;
    else if (argv[0] == ATOM(mmx)) accel = EPX_SIMD_MMX;
    else if (argv[0] == ATOM(sse2)) accel = EPX_SIMD_SSE2;
    else if (argv[0] == ATOM(neon)) accel = EPX_SIMD_NEON;
    else if (argv[0] == ATOM(auto)) accel = EPX_SIMD_AUTO;
    else return enif_make_badarg(env);
    // We could check for availability here ? and return error 
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
    epx_pixmap_t* dst;
    ERL_NIF_TERM t;

    if (!enif_get_uint(env, argv[0], &width)) 
	return enif_make_badarg(env);    
    if (!enif_get_uint(env, argv[1], &height)) 
	return enif_make_badarg(env);
    if (!get_pixel_format(env, argv[2], &fmt))
	return enif_make_badarg(env);	

    dst = epx_resource_alloc(&pixmap_res, sizeof(epx_pixmap_t));
    if (!dst)
	return enif_make_badarg(env);
    if (epx_pixmap_init(dst, width, height, fmt) < 0) {
	enif_release_resource(dst);
	return enif_make_badarg(env);
    }	
    epx_object_ref(dst);
    // t = enif_make_resource_binary(env, dst, dst->data, dst->sz);
    t = make_object(env, ATOM(epx_pixmap), dst);
    enif_release_resource(dst);
    return t;
}

static ERL_NIF_TERM pixmap_info(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
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
    else if (argv[1] == ATOM(parent)) {
	if (!src->parent)
	    return ATOM(undefined);
	else
	    return make_object(env,ATOM(epx_pixmap), src->parent);
    }
    else if (argv[1] == ATOM(clip)) {
	return enif_make_tuple4(env, 
				enif_make_int(env, src->clip.xy.x),
				enif_make_int(env, src->clip.xy.y),
				enif_make_uint(env, src->clip.wh.width),
				enif_make_uint(env, src->clip.wh.height));
    }
    else if (argv[1] == ATOM(backend)) {
	epx_nif_backend_t* backend = (epx_nif_backend_t*) src->user;
	if (!backend)
	    return ATOM(undefined);
	else
	    return make_object(env,ATOM(epx_backend), backend);
    }
    else 
	return enif_make_badarg(env);
}


static ERL_NIF_TERM pixmap_sub_pixmap(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    int x;
    int y;
    unsigned int width;
    unsigned int height;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    ERL_NIF_TERM t;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &x)) 
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[2], &y))
	return enif_make_badarg(env);    
    if (!enif_get_uint(env, argv[3], &width)) 
	return enif_make_badarg(env);    
    if (!enif_get_uint(env, argv[4], &height)) 
	return enif_make_badarg(env);

    dst = epx_resource_alloc(&pixmap_res,sizeof(epx_pixmap_t));
    if (!dst)
	return enif_make_badarg(env);
    if (epx_pixmap_init_sub_pixmap(src, dst, x, y, width, height) < 0) {
	enif_release_resource(dst);
	return enif_make_badarg(env);
    }
    epx_object_ref(dst);
    t = make_object(env, ATOM(epx_pixmap), dst);
    enif_release_resource(dst);
    return t;
}

static ERL_NIF_TERM pixmap_copy(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    ERL_NIF_TERM t;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    
    dst = epx_resource_alloc(&pixmap_res, sizeof(epx_pixmap_t));
    if (!dst)
	return enif_make_badarg(env);
    if (epx_pixmap_init_copy(src, dst) < 0) {
	enif_release_resource(dst);
	return enif_make_badarg(env);
    }
    epx_object_ref(dst);
    // t = enif_make_resource_binary(env, dst, dst->data, dst->sz);
    t = make_object(env, ATOM(epx_pixmap), dst);
    enif_release_resource(dst);
    return t;
}


static ERL_NIF_TERM pixmap_get_pixel(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixel_t p;
    int x;
    int y;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[2], &y))
	return enif_make_badarg(env);
    p = epx_pixmap_get_pixel(src, x, y);
    return make_color(env, p);
}

//
// pixmap_get_pixels(Pixmap, X, Y, W, H)
//
static ERL_NIF_TERM pixmap_get_pixels(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    int x;
    int y;
    unsigned int w;
    unsigned int h;
    ErlNifBinary bin;
    ERL_NIF_TERM bt;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[2], &y))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[3], &w))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[4], &h))
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

static ERL_NIF_TERM pixmap_put_pixel(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_pixel_t pixel;
    epx_flags_t flags;
    int x;
    int y;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[2], &y))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[3], &flags))
	return enif_make_badarg(env);
    if (!get_color(env, argv[4], &pixel))
	return enif_make_badarg(env);
    epx_pixmap_put_pixel(pixmap, x, y, flags, pixel);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_put_pixels(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    int x;
    int y;
    unsigned int width;
    unsigned int height;
    epx_format_t fmt;
    epx_flags_t flags;
    ErlNifBinary bin;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[2], &y))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[3], &width))
	return enif_make_badarg(env);    
    if (!enif_get_uint(env, argv[4], &height))
	return enif_make_badarg(env);
    if (!get_pixel_format(env, argv[5], &fmt))
	return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[6], &bin))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[7], &flags))
	return enif_make_badarg(env);

    epx_pixmap_put_pixels(pixmap, x, y, width, height, fmt, flags, 
			  bin.data, bin.size);
    return ATOM(ok);
}

// epx:pixmap_set_clip(Pixmap, #epx_rect{}) -> ok | exception(badarg)
static ERL_NIF_TERM pixmap_set_clip(ErlNifEnv* env, int argc, 
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_rect_t rect;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
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

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_color(env, argv[1], &color))
	return enif_make_badarg(env);
    epx_pixmap_fill(pixmap, color);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_copy_to(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    epx_pixmap_copy_to(src, dst);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_flip(ErlNifEnv* env, int argc, 
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
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

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[2], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[3], &height))
	return enif_make_badarg(env);
    epx_pixmap_scale(src, dst, width, height);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_copy_area(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    int x_dst;
    int y_dst;
    unsigned int width;
    unsigned int height;
    epx_flags_t flags;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &x_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &y_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[6], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[7], &height))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[8], &flags))
	return enif_make_badarg(env);
    epx_pixmap_copy_area(src, dst, x_src, y_src, x_dst, y_dst,
			 width, height, flags);    
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_alpha_area(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    int x_dst;
    int y_dst;
    uint8_t alpha;
    unsigned int width;
    unsigned int height;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!get_fix_8(env, argv[2], &alpha))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &x_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[6], &y_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[7], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[8], &height))
	return enif_make_badarg(env);
    epx_pixmap_alpha_area(src, dst, alpha, x_src, y_src, x_dst, y_dst,
			  width, height);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_fade_area(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    int x_dst;
    int y_dst;
    uint8_t fade;
    unsigned int width;
    unsigned int height;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!get_fix_8(env, argv[2], &fade))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &x_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[6], &y_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[7], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[8], &height))
	return enif_make_badarg(env);
    epx_pixmap_fade_area(src, dst, fade, x_src, y_src, x_dst, y_dst,
			 width, height);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_shadow_area(ErlNifEnv* env, int argc, 
				       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    int x_dst;
    int y_dst;
    unsigned int width;
    unsigned int height;
    epx_flags_t flags;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &x_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &y_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[6], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[7], &height))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[8], &flags))
	return enif_make_badarg(env);
    epx_pixmap_shadow_area(src, dst, x_src, y_src, x_dst, y_dst,
			   width, height, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_operation_area(ErlNifEnv* env, int argc, 
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    int x_dst;
    int y_dst;
    unsigned int width;
    unsigned int height;
    epx_pixel_operation_t op;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!get_operation(env, argv[2], &op))
	return enif_make_badarg(env);	
    if (!enif_get_int(env, argv[3], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &x_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[6], &y_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[7], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[8], &height))
	return enif_make_badarg(env);
    epx_pixmap_operation_area(src, dst, op, x_src, y_src, x_dst, y_dst,
			      width, height);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_add_color_area(ErlNifEnv* env, int argc, 
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    int x_dst;
    int y_dst;
    epx_pixel_t color;
    uint8_t fade;
    unsigned int width;
    unsigned int height;
    epx_flags_t flags;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!get_fix_8(env, argv[2], &fade))
	return enif_make_badarg(env);
    if (!get_color(env, argv[3], &color))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[6], &x_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[7], &y_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[8], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[9], &height))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[10], &flags))
	return enif_make_badarg(env);
    epx_pixmap_add_color_area(src, dst, fade, color,
			      x_src, y_src, x_dst, y_dst,
			      width, height, flags);
    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_filter_area(ErlNifEnv* env, int argc, 
				       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* src;
    epx_pixmap_t* dst;
    int x_src;
    int y_src;
    int x_dst;
    int y_dst;
    epx_filter_t filter;
    ErlNifBinary factors;
    unsigned int width;
    unsigned int height;
    epx_flags_t flags;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);
    if (!get_filter(env, argv[2], &filter, &factors))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &x_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[6], &y_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[7], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[8], &height))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[9], &flags))
	return enif_make_badarg(env);
    epx_pixmap_filter_area(src, dst, &filter, x_src, y_src, x_dst, y_dst,
			   width, height, flags);
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
    unsigned int width;
    unsigned int height;
    epx_flags_t flags;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
	return enif_make_badarg(env);

    if (!enif_get_double(env, argv[2], &angle))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &x_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &y_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &xc_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[6], &yc_src))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[7], &xc_dst))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[8], &yc_dst))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[9], &width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[10], &height))
	return enif_make_badarg(env);
    if (!get_flags(env, argv[11], &flags))
	return enif_make_badarg(env);
    epx_pixmap_rotate_area(src, dst, angle,
			   x_src, y_src, 
			   xc_src, yc_src, xc_dst, yc_dst,
			   width, height, flags);
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

    if (!get_object(env, argv[0], &pixmap_res, (void**) &src))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &pixmap_res, (void**) &dst))
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
    epx_nif_backend_t* backend;
    epx_pixmap_t* pixmap;
    epx_message_t m;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &backend_res, (void**) &backend))
	return enif_make_badarg(env);
    if (pixmap->user) // already attached
	return enif_make_badarg(env);

    // make sure backend survive until EPX_MESSAGE_PIXMAP is processed
    enif_keep_resource(backend); // pixmap is using backend!!!
    pixmap->user = backend;      

    enif_keep_resource(pixmap);  // keep pixmap until attach is completed
    m.type = EPX_MESSAGE_PIXMAP_ATTACH;
    m.pixmap = pixmap;
    epx_message_send(backend->main, 0, &m);

    return ATOM(ok);
}

static ERL_NIF_TERM pixmap_detach(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_nif_backend_t* backend;
    epx_pixmap_t* pixmap;
    epx_message_t m;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!(backend = pixmap->user))
	return enif_make_badarg(env);

    enif_keep_resource(pixmap);  // keep pixmap until detach is completed
    m.type = EPX_MESSAGE_PIXMAP_DETACH;
    m.dtor = 0;
    m.pixmap = pixmap;
    epx_message_send(backend->main, 0, &m);
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
    epx_nif_backend_t* backend;
    epx_pixmap_t* pixmap;
    epx_window_t* window;
    epx_message_t m;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &window_res, (void**) &window))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &m.wdraw.src_x))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &m.wdraw.src_y))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &m.wdraw.dst_x))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[5], &m.wdraw.dst_y))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[6], &m.wdraw.width))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[7], &m.wdraw.height))
	return enif_make_badarg(env);
    if (!(backend = pixmap->user))
	return enif_make_badarg(env);
    if (backend != window->user)
	return enif_make_badarg(env);
    m.wdraw.pixmap = pixmap;
    m.wdraw.window = window;
    m.type = EPX_MESSAGE_PIXMAP_DRAW;
    epx_message_send(backend->main, 0, &m);
    return ATOM(ok);
}

// epx:pixmap_draw_point(Pixmap, Gc, X, Y)
static ERL_NIF_TERM pixmap_draw_point(ErlNifEnv* env, int argc, 
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_gc_t* gc;
    int x;
    int y;    

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[3], &y))
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

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x1))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[3], &y1))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &x2))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[5], &y2))
	return enif_make_badarg(env);
    epx_pixmap_draw_line(pixmap, gc, x1, y1, x2, y2);
    return ATOM(ok);    
}

static ERL_NIF_TERM pixmap_draw_rectangle(ErlNifEnv* env, int argc, 
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_gc_t* gc;
    int x;
    int y;    
    unsigned int width;
    unsigned int height;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[3], &y))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[4], &width))
	return enif_make_badarg(env);    
    if (!enif_get_uint(env, argv[5], &height))
	return enif_make_badarg(env);
    epx_pixmap_draw_rectangle(pixmap, gc, x, y, width, height);
    return ATOM(ok);    
}


static ERL_NIF_TERM pixmap_draw_ellipse(ErlNifEnv* env, int argc, 
					const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_gc_t* gc;
    int x;
    int y;    
    unsigned int width;
    unsigned int height;

    if (!get_object(env, argv[0], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[3], &y))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[4], &width))
	return enif_make_badarg(env);    
    if (!enif_get_uint(env, argv[5], &height))
	return enif_make_badarg(env);
    epx_pixmap_draw_ellipse(pixmap, gc, x, y, width, height);
    return ATOM(ok);    
}

static ERL_NIF_TERM animation_open(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    char path[MAX_PATH];
    int r;
    epx_animation_t* anim;
    ERL_NIF_TERM t;

    if (!(r=enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1)) 
	|| (r < 0))
	return enif_make_badarg(env);
    anim = epx_resource_alloc(&anim_res, sizeof(epx_animation_t));
    if (!anim)
	return enif_make_badarg(env); // reason?    
    if (epx_anim_open_init(anim, path) < 0) {
	enif_release_resource(anim);
	return enif_make_badarg(env); // reason?
    }
    epx_object_ref(anim);
    t = make_object(env,ATOM(epx_animation), anim);
    enif_release_resource(anim);
    return t;
}

static ERL_NIF_TERM animation_copy(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_pixmap_t* pixmap;
    epx_gc_t* gc;
    epx_animation_t* anim;
    int x;
    int y;    
    unsigned int index;
    epx_anim_pixels_t* base;
    epx_anim_pixels_t* current;

    if (!get_object(env, argv[0], &anim_res, (void**) &anim))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_object(env, argv[2], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[3], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[5], &y))
	return enif_make_badarg(env);

    if (!(base = epx_anim_get_pixels(anim, 0)))
	return enif_make_badarg(env);
    if (!(current = epx_anim_get_pixels(anim, (int)index)))
	return enif_make_badarg(env);
    epx_anim_copy_frame(pixmap, gc, x, y, 
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
    epx_pixmap_t* pixmap;
    epx_gc_t* gc;
    epx_animation_t* anim;
    int x;
    int y;    
    unsigned int index;
    epx_anim_pixels_t* base;
    epx_anim_pixels_t* current;

    if (!get_object(env, argv[0], &anim_res, (void**) &anim))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &index))
	return enif_make_badarg(env);
    if (!get_object(env, argv[2], &pixmap_res, (void**) &pixmap))
	return enif_make_badarg(env);
    if (!get_object(env, argv[3], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[4], &x))
	return enif_make_badarg(env);    
    if (!enif_get_int(env, argv[5], &y))
	return enif_make_badarg(env);

    if (!(base = epx_anim_get_pixels(anim, 0)))
	return enif_make_badarg(env);	
    if (!(current = epx_anim_get_pixels(anim, (int) index)))
	return enif_make_badarg(env);	    
    epx_anim_draw_frame(pixmap, gc, x, y, 
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
    
    if (!get_object(env, argv[0], &anim_res, (void**) &anim))
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
    epx_dict_t* dict;
    ERL_NIF_TERM t;

    dict = epx_resource_alloc(&dict_res, sizeof(epx_dict_t));
    if (!dict)
	return enif_make_badarg(env);
    epx_dict_init(dict);
    epx_object_ref(dict);
    t = make_object(env,ATOM(epx_dict), dict);
    enif_release_resource(dict);
    return t;
}

static ERL_NIF_TERM dict_copy(ErlNifEnv* env, int argc, 
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* src;
    epx_dict_t* dst;
    ERL_NIF_TERM t;

    if (!get_object(env, argv[0], &dict_res, (void**) &src))
	return enif_make_badarg(env);

    dst = epx_resource_alloc(&dict_res, sizeof(epx_dict_t));
    if (!dst)
	return enif_make_badarg(env);
    if (epx_dict_init_copy(src, dst) < 0) {
	enif_release_resource(dst);
	return enif_make_badarg(env);
    }
    epx_object_ref(dst);
    t = make_object(env, ATOM(epx_dict), dst);
    enif_release_resource(dst);
    return t;
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

    if (!get_object(env, argv[0], &dict_res, (void**) &dict))
	return enif_make_badarg(env);

    if (!get_dict_data(env, argv[1], &key, &key_data))
	return enif_make_badarg(env);	

    if (!get_dict_data(env, argv[2], &value, &value_data))
	return enif_make_badarg(env);	

    if (epx_dict_set_ent(dict, &key, &value) < 0)
	return enif_make_badarg(env);
    return argv[2];
}

static ERL_NIF_TERM dict_get(ErlNifEnv* env, int argc, 
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    epx_dict_data_t key;
    dict_data_buf_t key_data;
    epx_dict_entry_t* ent;

    if (!get_object(env, argv[0], &dict_res, (void**) &dict))
	return enif_make_badarg(env);

    if (!get_dict_data(env, argv[1], &key, &key_data))
	return enif_make_badarg(env);
    if (!(ent = epx_dict_lookup_ent(dict, &key)))
	return enif_make_badarg(env);
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

// Specialized getters where key is "string"
// return value is boolean
static ERL_NIF_TERM dict_get_boolean(ErlNifEnv* env, int argc, 
				     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_dict_t* dict;
    int value;
    char key[256];

    if (!get_object(env, argv[0], &dict_res, (void**) &dict))
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

    if (!get_object(env, argv[0], &dict_res, (void**) &dict))
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

    if (!get_object(env, argv[0], &dict_res, (void**) &dict))
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

    if (!get_object(env, argv[0], &dict_res, (void**) &dict))
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

    if (!get_object(env, argv[0], &dict_res, (void**) &dict))
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
    ERL_NIF_TERM t;

    gc = epx_resource_alloc(&gc_res, sizeof(epx_gc_t));
    if (!gc)
	return enif_make_badarg(env);
    epx_gc_init(gc);
    epx_object_ref(gc);
    t = make_object(env,ATOM(epx_gc), gc);
    enif_release_resource(gc);
    return t;
}

static ERL_NIF_TERM gc_copy(ErlNifEnv* env, int argc,
			    const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    epx_gc_t* src;
    epx_gc_t* dst;
    ERL_NIF_TERM t;

    if (!get_object(env, argv[0], &gc_res, (void**) &src))
	return enif_make_badarg(env);
    dst = epx_resource_alloc(&gc_res, sizeof(epx_gc_t));
    if (!dst)
	return enif_make_badarg(env);
    epx_gc_init_copy(src, dst);
    epx_object_ref(dst);
    t = make_object(env, ATOM(epx_gc), dst);
    enif_release_resource(dst);
    return t;    
}


static ERL_NIF_TERM gc_default(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    return make_object(env, ATOM(epx_gc), def_gc);
}

static ERL_NIF_TERM gc_set(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_gc_t* src;

    if (!get_object(env, argv[0], &gc_res, (void**) &src))
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
	if (!enif_get_uint(env, argv[2], &width))
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
	if (!get_flags(env, argv[2], &style))
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
	if (!enif_get_uint(env, argv[2], &width))
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
	if (!get_font(env, argv[2], &font))
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
	if (!enif_get_uint(env, argv[2], &width))
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

    if (!get_object(env, argv[0], &gc_res, (void**) &src))
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
    ERL_NIF_TERM t;

    if (!(r=enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1)) 
	|| (r < 0))
	return enif_make_badarg(env);
    font = epx_resource_alloc(&font_res, sizeof(epx_font_t));
    if (!font)
	return enif_make_badarg(env); // reason?
    if (epx_font_open_init(font, path) < 0) {
	enif_release_resource(font);
	return enif_make_badarg(env); // reason?
    }
    epx_object_ref(font);
    t = make_object(env,ATOM(epx_font), font);
    enif_release_resource(font);
    return t;
}


static ERL_NIF_TERM font_load(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_object(env, argv[0], &font_res, (void**) &font))
	return enif_make_badarg(env);
    if (epx_font_load(font) < 0)
	return ATOM(error); // fixme - reason
    return ATOM(ok);
}

static ERL_NIF_TERM font_unload(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_object(env, argv[0], &font_res, (void**) &font))
	return enif_make_badarg(env);
    epx_font_unload(font);
    return ATOM(ok);
}

static ERL_NIF_TERM font_map(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_object(env, argv[0], &font_res, (void**) &font))
	return enif_make_badarg(env);
    if (epx_font_map(font) < 0)
	return ATOM(error); // fixme - reason
    return ATOM(ok);
}

static ERL_NIF_TERM font_unmap(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;

    if (!get_object(env, argv[0], &font_res, (void**) &font))
	return enif_make_badarg(env);
    epx_font_unmap(font);
    return ATOM(ok);
}

static ERL_NIF_TERM font_info(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_font_t* font;
    
    if (!get_object(env, argv[0], &font_res, (void**) &font))
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

// Draw a glyph C at X,Y return {X',Y'}
static ERL_NIF_TERM font_draw_glyph(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_gc_t* gc;
    epx_pixmap_t* dst;
    int x;
    int y;
    int c;

    if (!get_texture(env, argv[0], &dst))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &y))
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
    int x;
    int y;
    int n;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;

    if (!get_texture(env, argv[0], &dst))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &y))
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
    int x;
    int y;
    ErlNifBinary bin;

    if (!get_texture(env, argv[0], &dst))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &gc_res, (void**) &gc))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &x))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &y))
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

    EPX_DBGFMT("reaper: started (%p)", self);

    while(1) {
	epx_message_t m;
	epx_thread_t* from = 0;
	void* exit_value;

	epx_message_recv(self, &from, &m); // FIXME: handle error
	
	switch(m.type) {
	case EPX_MESSAGE_KILL:
	    epx_thread_stop(m.thread, self, &exit_value);
	    break;

	case EPX_MESSAGE_STOP:
	    EPX_DBGFMT("reaper: stoppped by command");
	    epx_thread_exit(self);
	    break;
	case EPX_MESSAGE_POLL:
	case EPX_MESSAGE_EVENT_READY:
	case EPX_MESSAGE_WINDOW_ATTACH:
	case EPX_MESSAGE_WINDOW_DETACH:
	case EPX_MESSAGE_WINDOW_ADJUST:
	case EPX_MESSAGE_WINDOW_SWAP:
	case EPX_MESSAGE_ADJUST:
	case EPX_MESSAGE_PIXMAP_ATTACH:
	case EPX_MESSAGE_PIXMAP_DETACH:
	case EPX_MESSAGE_PIXMAP_DRAW:
	default:
	    EPX_DBGFMT("reaper: got unknown message %d", m.type);
	    break;
	}
    }    
    return 0;
}

/******************************************************************************
 *
 *   backend_poll
 *      thread for polling backends for events
 *   FIXME: use a pipe fd to wake up from poll
 *****************************************************************************/

static void* backend_poll(void* arg)
{
    epx_thread_t* self = arg;

    EPX_DBGFMT("backend_poll: started (%p)", self);

    while(1) {
	epx_message_t m;
	epx_thread_t* from = 0;

	epx_message_recv(self, &from, &m); // FIXME: handle error

	switch(m.type) {
	case EPX_MESSAGE_POLL: {
	    struct pollfd fds[2];
	    int do_poll = 1;
	    int nfd;
	    int timeout = 1000;

	    EPX_DBGFMT("backend_poll: poll fd=%ld", m.handle);

	    fds[0].fd = (int)((long)m.handle);
	    fds[0].events = POLLIN;
	    nfd = 1;
	    if (self->wake[0] >= 0) {
		fds[1].fd = self->wake[0];
		fds[1].events = POLLIN;
		timeout = -1;   // timeout not needed
		nfd++;
	    }

	    while(do_poll) {
		int n;
		
		if ((n = poll(fds, nfd, timeout)) > 0) {
		    if (fds[0].revents & POLLIN) {
			EPX_DBGFMT("backend_poll: read fd=%d", fds[0].fd);
			m.type = EPX_MESSAGE_EVENT_READY;
			epx_message_send(from, self, &m);
			do_poll = 0;
		    }
		    if ((nfd>1) && fds[1].revents & POLLIN) {
		        int r;
			(void) r;
			char buf[1];
			r=read(self->wake[0], &buf, 1); // consume
			do_poll = 0;
		    }
		}
		else if (n == 0) {  // timeout
		    do_poll = 0;
		}
		else if (n < 0) {
		    EPX_DBGFMT("backend_poll: error %s (%d)",
			       errno, strerror(errno));
		    // check EAGAIN or real error, loop or die
		}
	    }
	    break;
	}

	case EPX_MESSAGE_STOP:
	    EPX_DBGFMT("backend_poll: stoppped by command");
	    epx_thread_exit(self);
	    break;
	case EPX_MESSAGE_KILL:
	case EPX_MESSAGE_EVENT_READY:
	case EPX_MESSAGE_WINDOW_ATTACH:
	case EPX_MESSAGE_WINDOW_DETACH:
	case EPX_MESSAGE_WINDOW_ADJUST:
	case EPX_MESSAGE_WINDOW_SWAP:
	case EPX_MESSAGE_ADJUST:
	case EPX_MESSAGE_PIXMAP_ATTACH:
	case EPX_MESSAGE_PIXMAP_DETACH:
	case EPX_MESSAGE_PIXMAP_DRAW:
	default:
	    EPX_DBGFMT("backend_poll: got unknown message %d", m.type);
	    break;
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

static void* backend_main(void* arg)
{
    epx_thread_t* self = arg;
    epx_nif_backend_t* nbackend = self->arg;
    epx_backend_t* backend = nbackend->backend;
    epx_thread_t* poll_thr;
    EPX_HANDLE_T handle;
    epx_message_t m;
    ErlNifEnv* env;   // thread environment

    env = enif_alloc_env();

    // Maybe initialize the backend here ?

    EPX_DBGFMT("backend_main: started (%p)", self);

    // FIXME: check return value 
    handle = epx_backend_event_attach(backend);
    
    // create a poll thread stack size 4K - FIXME handle error
    poll_thr = epx_thread_start(backend_poll, 0, 1, 4);

    EPX_DBGFMT("backend_main: send EPX_MESSAGE_POLL handle=%ld",
	       handle);

    m.type = EPX_MESSAGE_POLL;  // start polling 
    m.handle = handle;
    epx_message_send(poll_thr, self, &m);

    enif_keep_resource(nbackend);

    while(1) {
	epx_thread_t* from = 0;

	enif_release_resource(nbackend);

	epx_message_recv(self, &from, &m);

	if (m.type == EPX_MESSAGE_STOP) {
	    void* poll_thr_exit = 0;
	    EPX_DBGFMT("backend_main: stopped by command");
	    enif_free_env(env);
	    epx_thread_stop(poll_thr, self, &poll_thr_exit);
	    EPX_DBGFMT("backend_main: unref backend refc=%d", backend->refc);
	    epx_object_unref(backend);
	    // release real backend
	    epx_thread_exit(self);
	}

	enif_keep_resource(nbackend);

	switch(m.type) {
	case EPX_MESSAGE_EVENT_READY: {
	    int n;
	    epx_event_t evt;

	    EPX_DBGFMT("backend_main: EPX_MESSAGE_EVENT_READ");
	    while ((n = epx_backend_event_read(backend, &evt)) > 0) {
		ERL_NIF_TERM msg;
		ErlNifPid* pid = evt.window->owner;
		// send event to window owner
		msg = make_event(env, &evt);
		enif_send(0, pid, env, msg);
		enif_clear_env(env);

		EPX_DBGFMT("backend_main: got event %08X", evt.type);
		if (n == 1)
		    break;
	    }
	    EPX_DBGFMT("backend_main: send EPX_MESSAGE_POLL");
	    m.type = EPX_MESSAGE_POLL;  // poll for more events
	    m.handle = handle;
	    epx_message_send(poll_thr, self, &m);
	    break;
	}

	case EPX_MESSAGE_WINDOW_ATTACH:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_WINDOW_ATTACH");
	    epx_backend_window_attach(backend, m.window);
	    enif_release_resource(m.window);   // thread safe?
	    break;

	case EPX_MESSAGE_WINDOW_DETACH:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_WINDOW_DETACH");
	    epx_window_detach(m.window);
	    EPX_DBGFMT("backend_main: enif_release_resource: m.window->user");
	    enif_release_resource(m.window->user);
	    m.window->user = 0;
	    EPX_DBGFMT("backend_main: enif_release_resource: m.window");
	    if (m.dtor)
		epx_object_unref(m.window);
	    else
		enif_release_resource(m.window);
	    break;

	case EPX_MESSAGE_WINDOW_ADJUST:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_WINDOW_ADJUST");
	    epx_window_adjust(m.wadjust.window, m.wadjust.param);
	    break;

	case EPX_MESSAGE_WINDOW_SWAP:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_WINDOW_SWAP");
	    epx_window_swap(m.window);
	    break;

	case EPX_MESSAGE_ADJUST:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_ADJUST");
	    epx_backend_adjust(backend, m.param);
	    break;

	case EPX_MESSAGE_PIXMAP_ATTACH:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_PIXMAP_ATTACH");
	    epx_backend_pixmap_attach(backend, m.pixmap);
	    enif_release_resource(m.pixmap);  // thread safe?
	    break;

	case EPX_MESSAGE_PIXMAP_DETACH:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_PIXMAP_DETACH");
	    epx_pixmap_detach(m.pixmap);
	    enif_release_resource(m.pixmap->user);
	    m.pixmap->user = 0;
	    if (m.dtor)
		epx_object_unref(m.pixmap);
	    else
		enif_release_resource(m.pixmap);
	    break;

	case EPX_MESSAGE_PIXMAP_DRAW:
	    EPX_DBGFMT("backend_main: EPX_MESSAGE_PIXMAP_DRAW");
	    epx_backend_draw_begin(backend, m.wdraw.window),
	    epx_backend_pixmap_draw(backend, m.wdraw.pixmap, m.wdraw.window,
				    m.wdraw.src_x, m.wdraw.src_y,
				    m.wdraw.dst_x, m.wdraw.dst_y,
				    m.wdraw.width, m.wdraw.height);
	    epx_backend_draw_end(backend, m.wdraw.window, 0);
	    epx_window_swap(m.wdraw.window);
	    break;

	case EPX_MESSAGE_POLL:
	case EPX_MESSAGE_KILL:
	case EPX_MESSAGE_STOP:
	default:
	    EPX_DBGFMT("backend_main: got unknown message %d", m.type);
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
    epx_backend_t* be;
    epx_nif_backend_t* backend;
    ERL_NIF_TERM res;
    ErlNifPid* caller;

    if (!enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &dict_res, (void**) &param))
	return enif_make_badarg(env);
    if (!(be = epx_backend_create(name, param)))
	return enif_make_badarg(env);
    backend = epx_resource_alloc(&backend_res,sizeof(epx_nif_backend_t));
    if (!backend)
	return enif_make_badarg(env);
    if (!(caller = enif_alloc(sizeof(ErlNifPid))))
	return enif_make_badarg(env);

    backend->owner = (void*) enif_self(env, caller);
    backend->backend = be;

    // start the backend thread stack size = default - FIXME handle error
    backend->main = epx_thread_start(backend_main, backend, 0, -1);

    res = make_object(env,ATOM(epx_backend), backend);
    enif_release_resource(backend);
    return res;
}

static ERL_NIF_TERM backend_info(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_backend_t* backend;
    
    if (!get_object(env, argv[0], &backend_res, (void**) &backend))
	return enif_make_badarg(env);

    if (argv[1] == ATOM(pending)) {  // number of events pending
	return enif_make_int(env, backend->pending);
    }
    else if (argv[1] == ATOM(opengl)) { // opengl support
	return make_bool(env, backend->opengl);
    }
    else if (argv[1] == ATOM(use_opengl)) { // opengl used
	return make_bool(env, backend->use_opengl);
    }
    else if (argv[1] == ATOM(windows)) { // windows attached
	epx_window_t* window = backend->window_list;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	while(window) {
	    list =
		enif_make_list_cell(env, 
				    make_object(env, ATOM(epx_window), window),
				    list);
	    window = window->next;
	}
	return list;
    }
    else if (argv[1] == ATOM(pixmaps)) { // pixmaps attached
	epx_pixmap_t* pixmap = backend->pixmap_list;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	while(pixmap) {
	    list =
		enif_make_list_cell(env, 
				    make_object(env, ATOM(epx_pixmap), pixmap),
				    list);
	    pixmap = pixmap->next;
	}
	return list;
    }
    else if (argv[1] == ATOM(epx_pixel_formats)) { // pixmap formats supported
	return enif_make_list(env, 0);
    }
    return enif_make_badarg(env);    
}

static ERL_NIF_TERM backend_adjust(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    return enif_make_badarg(env);    
}


// Windows
static ERL_NIF_TERM window_create(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    int x;
    int y;
    unsigned int width;
    unsigned int height;
    epx_window_t* window = 0;
    ERL_NIF_TERM res;
    ErlNifPid* caller;
    uint32_t events = 0;

    if (!enif_get_int(env, argv[0], &x)) 
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &y))
	return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[2], &width)) 
	return enif_make_badarg(env);    
    if (!enif_get_uint(env, argv[3], &height))
	return enif_make_badarg(env);
    if (argc == 5) {
	if (!get_event_flags(env, argv[4], &events))
	    return enif_make_badarg(env);
    }
    window = epx_resource_alloc(&window_res, sizeof(epx_window_t));
    if (!window)
	goto error;
    if (!(caller = enif_alloc(sizeof(ErlNifPid))))
	goto error;
    if (epx_window_init(window, x, y, width, height) < 0)
	goto error;
    epx_window_set_event_mask(window, events);
    window->owner = (void*) enif_self(env, caller);
    epx_object_ref(window);
    res = make_object(env,ATOM(epx_window),window);
    enif_release_resource(window);
    return res;

error:
    if (window)
	enif_release_resource(window);
    return enif_make_badarg(env);    
}

static ERL_NIF_TERM window_adjust(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;
    return enif_make_badarg(env);    
}

static ERL_NIF_TERM window_info(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_window_t* win;

    if (!get_object(env, argv[0], &window_res, (void**) &win))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(x)) {
	return enif_make_int(env, win->x);
    }
    else if (argv[1] == ATOM(y)) {
	return enif_make_uint(env, win->y);
    }
    else if (argv[1] == ATOM(width)) {
	return enif_make_uint(env, win->width);
    }
    else if (argv[1] == ATOM(height)) {
	return enif_make_uint(env, win->height);
    }
    else if (argv[1] == ATOM(backend)) {
	epx_nif_backend_t* backend = (epx_nif_backend_t*) win->user;
	if (!backend)
	    return ATOM(undefined);
	else
	    return make_object(env,ATOM(epx_backend), backend);
    }
    else if (argv[1] == ATOM(event_mask)) {
	return make_event_flags(env, win->mask);
    }
    else
	return enif_make_badarg(env);    
}

static ERL_NIF_TERM window_attach(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_nif_backend_t* backend;
    epx_window_t* window;
    epx_message_t m;

    if (!get_object(env, argv[0], &window_res, (void**) &window))
	return enif_make_badarg(env);
    if (!get_object(env, argv[1], &backend_res, (void**) &backend))
	return enif_make_badarg(env);

    if (window->user)
	return enif_make_badarg(env);

    enif_keep_resource(backend); // window reference backend
    window->user = backend;

    enif_keep_resource(window);  // keep until backend has completed
    m.type = EPX_MESSAGE_WINDOW_ATTACH;
    m.window = window;
    epx_message_send(backend->main, 0, &m);

    return ATOM(ok);
}


static ERL_NIF_TERM window_detach(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_nif_backend_t* backend;
    epx_window_t* window;
    epx_message_t m;

    if (!get_object(env, argv[0], &window_res, (void**) &window))
	return enif_make_badarg(env);
    if (!(backend = window->user))
	return enif_make_badarg(env);

    enif_keep_resource(window); // backend_main: release
    m.type = EPX_MESSAGE_WINDOW_DETACH;
    m.dtor = 0;
    m.window = window;
    epx_message_send(backend->main, 0, &m);
    return ATOM(ok);
}

static ERL_NIF_TERM window_set_event_mask(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    (void) argc;
    epx_window_t* window;
    uint32_t events;

    if (!get_object(env, argv[0], &window_res, (void**) &window))
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

    if (!get_object(env, argv[0], &window_res, (void**) &window))
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

    if (!get_object(env, argv[0], &window_res, (void**) &window))
	return enif_make_badarg(env);
    if (!get_event_flags(env, argv[1], &events))
	return enif_make_badarg(env);
    epx_window_disable_events(window, events);
    return ATOM(ok);
}


static int epx_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags tried;
    (void) env;
    (void) load_info;

    epx_set_debug(DLOG_DEFAULT);

    EPX_DBGFMT("epx_load");
    epx_init(EPX_SIMD_AUTO);

    // Create resource types
    epx_resource_init(env, &dict_res, "epx_dict", object_dtor,
		      ERL_NIF_RT_CREATE, &tried);
    epx_resource_init(env, &pixmap_res, "epx_pixmap", pixmap_dtor,
		      ERL_NIF_RT_CREATE, &tried);
    epx_resource_init(env, &gc_res, "epx_gc", object_dtor,
		      ERL_NIF_RT_CREATE, &tried);
    epx_resource_init(env, &font_res, "epx_font", object_dtor,
		      ERL_NIF_RT_CREATE, &tried);
    epx_resource_init(env, &backend_res, "epx_backend", backend_dtor,
		      ERL_NIF_RT_CREATE, &tried);
    epx_resource_init(env, &window_res, "epx_window", window_dtor,
		      ERL_NIF_RT_CREATE, &tried);
    epx_resource_init(env, &anim_res, "epx_animation",  object_dtor,
		      ERL_NIF_RT_CREATE, &tried);

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
    LOAD_ATOM(epx_window);
    LOAD_ATOM(epx_backend);
    LOAD_ATOM(epx_dict);
    LOAD_ATOM(epx_gc);
    LOAD_ATOM(epx_font);
    LOAD_ATOM(epx_animation);

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

    // simd_info
    LOAD_ATOM(emu);
    LOAD_ATOM(altivec);
    LOAD_ATOM(mmx);
    LOAD_ATOM(sse2);
    LOAD_ATOM(neon);
    LOAD_ATOM(auto);
    LOAD_ATOM(accel);
    LOAD_ATOM(functions);
    LOAD_ATOM(cpu_features);
    LOAD_ATOM(cpu_vendor_name);
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
    LOAD_ATOM(pending);
    LOAD_ATOM(opengl);
    LOAD_ATOM(use_opengl);
    LOAD_ATOM(windows);
    LOAD_ATOM(pixmaps);
    LOAD_ATOM(epx_pixel_formats);

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
    LOAD_ATOM(resize);
    LOAD_ATOM(crossing);
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
    LOAD_ATOM(a8r8g8b8);
    LOAD_ATOM(rgba);
    LOAD_ATOM(r8g8b8a8);
    LOAD_ATOM(abgr);
    LOAD_ATOM(a8b8g8r8);
    LOAD_ATOM(bgra);
    LOAD_ATOM(b8g8r8a8);
    LOAD_ATOM(rgb);
    LOAD_ATOM(r8g8b8);
    LOAD_ATOM(bgr);
    LOAD_ATOM(b8g8r8);
    LOAD_ATOM(565);
    LOAD_ATOM(r5g6b5);
    LOAD_ATOM(565BE);
    LOAD_ATOM(r5g6b5BE);
    LOAD_ATOM(565LE);
    LOAD_ATOM(r5g6b5LE);
    LOAD_ATOM(b5g6r5);
    LOAD_ATOM(b5g6r5BE);
    LOAD_ATOM(b5g6r5LE);
    LOAD_ATOM(1555);
    LOAD_ATOM(a1r5g5b);
    LOAD_ATOM(gray8a8);
    LOAD_ATOM(gray16);
    LOAD_ATOM(a8);
    LOAD_ATOM(alpha8);
    LOAD_ATOM(r8);
    LOAD_ATOM(g8);
    LOAD_ATOM(b8);
    LOAD_ATOM(gray8);
    LOAD_ATOM(efnt2);
    LOAD_ATOM(a8l8);

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

    // create the "default" gc
    def_gc = epx_resource_alloc(&gc_res, sizeof(epx_gc_t));
    epx_gc_init_copy(&epx_default_gc, def_gc);
    epx_object_ref(def_gc);

    reaper = epx_thread_start(reaper_main, 0, 0, 4);
    
    *priv_data = 0;
    return 0;
}

static int epx_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    (void) priv_data;
    EPX_DBGFMT("epx_reload");
    return 0;
}

static int epx_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
		       ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    EPX_DBGFMT("epx_upgrade");
    *priv_data = *old_priv_data;
    return 0;
}

static void epx_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    (void) priv_data;
    void* exit_value;

    // stop all backends?
    epx_thread_stop(reaper, 0, &exit_value);

    // FIXME!!!

    EPX_DBGFMT("epx_unload");
}


ERL_NIF_INIT(epx, epx_funcs,
	     epx_load, epx_reload, 
	     epx_upgrade, epx_unload)

    
