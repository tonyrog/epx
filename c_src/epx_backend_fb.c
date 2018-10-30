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
 *  Framebuffer display driver
 */

#include "../include/epx_backend.h"
#include "../include/epx_colors.h"
#include "../include/epx_debug.h"
// FIXME configure
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <linux/fb.h>
#include <linux/vt.h>
#include <linux/kd.h>

#ifdef HAVE_INPUT_EVENT
#include <sys/epoll.h>
#include <linux/input.h>
#endif 

#include <errno.h>
#include <unistd.h>
#include <string.h>
#ifdef HAVE_MTRR
#include <asm/mtrr.h>
#endif
#include <strings.h>

typedef u_int8_t  u8;
typedef u_int16_t u16;
typedef u_int32_t u32;


/* This structure is stored in epx_window_t opaque field */
typedef struct {
    int wstate;
    unsigned long dcount;
} FbWindow;

typedef struct {
    epx_backend_t b;                  /* DO NOT MOVE !!! */
    struct fb_var_screeninfo ovinfo;  /* Original screeen paramters */
    struct fb_var_screeninfo vinfo;   /* Current screeen paramters */
    struct fb_fix_screeninfo finfo;   /* Current display paramters */
    int fb_fd;                        /* Frame buffer fd */
#ifdef HAVE_MTRR
    int mtrr_fd;                      /* File descriptor for MTRR */
#endif
    int dbuf;                         /* Double buffer enabled */
    int cbuf;                         /* Current buffer displayed */
    int lcd_pi32;                     /* poll lcd_pi32 keys */
    epx_pixmap_t screen[2];           /* The screen/off-screen as a pixmap */
    char direct_pixmap_draw;   /* Shall we map fb directly into pixmap? */

    unsigned char *org_pixmap_data; /* Place holder for pixmap->data if direct draw */

#ifdef HAVE_INPUT_EVENT
#define MAX_INPUT_SOURCE 16
    int ax_valid;
    int ay_valid;
    int az_valid;
    int ap_valid;
    struct input_absinfo ax;
    struct input_absinfo ay;
    struct input_absinfo az;
    struct input_absinfo ap;
    int poll_fd;
    int input_fd[MAX_INPUT_SOURCE];
    size_t input_fd_sz;
    struct timeval mouse_ts;  // last mouse movement time
    int mouse_x;        // mouse position x
    int mouse_y;        // mouse position y
    int mouse_z;        // mouse position z
    int mouse_moved;    // any movement since last time
    u16 mouse_buttons;  // Current buttons pressed by mouse EPX_BUTTON_*
    u16 keyboard_mods;  // Current keyboard mods EPX_KBD_MOD_*
    unsigned char lcd_pi32_keys;   /* old keys */
    epx_event_t ev;                /* Current event (sync by EV_SYN) */
    epx_pixmap_t* cursor;  // cursor image
    int save_x;
    int save_y;
    epx_pixmap_t* save;    // backing store for cursor
    int hotspot_x;         // offset-x
    int hotspot_y;         // offset-y
#endif    

} FbBackend;


epx_backend_t* fb_init(epx_dict_t* param);
int fb_upgrade(epx_backend_t* be);

static int fb_finish(epx_backend_t*);
static int fb_pix_attach(epx_backend_t*, epx_pixmap_t*);
static int fb_pix_detach(epx_backend_t*, epx_pixmap_t*);
static int fb_begin(epx_window_t*);
static int fb_end(epx_window_t*,int off_screen);
static int fb_pix_draw(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
		       int src_x, int src_y, int dst_x, int dst_y,
		       unsigned int width,
		       unsigned int height);
static int fb_pix_sync(epx_backend_t*, epx_pixmap_t*, epx_window_t*);
static int fb_win_attach(epx_backend_t*, epx_window_t*);
static int fb_win_detach(epx_backend_t*, epx_window_t*);
static int fb_win_swap(epx_backend_t*, epx_window_t*);
static EPX_HANDLE_T fb_evt_attach(epx_backend_t*);
static int fb_evt_detach(epx_backend_t*);
static int fb_evt_read(epx_backend_t*, epx_event_t*);
static int fb_adjust(epx_backend_t *backend, epx_dict_t* param);
static int fb_win_adjust(epx_window_t *win, epx_dict_t* param);
static int fb_info(epx_backend_t *backend, epx_dict_t* param);


static epx_callbacks_t fb_callbacks =
{
    .finish     = fb_finish,
    .pix_attach = fb_pix_attach,
    .pix_detach = fb_pix_detach,
    .pix_draw   = fb_pix_draw,
    .pix_sync   = fb_pix_sync,
    .win_attach = fb_win_attach,
    .win_detach = fb_win_detach,
    .evt_attach = fb_evt_attach,
    .evt_detach = fb_evt_detach,
    .evt_read   = fb_evt_read,
    .adjust     = fb_adjust,
    .win_swap   = fb_win_swap,
    .begin      = fb_begin,
    .end        = fb_end,
    .win_adjust = fb_win_adjust,
    .info       = fb_info
};
#ifdef HAVE_INPUT_EVENT
/* Array indexed by struct input_event.type */
typedef 
int (*input_event_callback_t)(struct timeval ts,
			      __u16 type, 
			      __u16 code, 
			      __s32 value, 
			      FbBackend* be, 
			      epx_event_t* ev);


static input_event_callback_t ev_callbacks[EV_CNT];
static int setup_input_system(FbBackend* be, epx_dict_t *param);

static int process_input_event_syn(struct timeval ts,
				   __u16 type,
				   __u16 code, 
				   __s32 value, 
				   FbBackend* be, 
				   epx_event_t* ev);

static int process_input_event_relative(struct timeval ts,
					__u16 type,
					__u16 code, 
					__s32 value, 
					FbBackend* be, 
					epx_event_t* ev);

static int process_input_event_absolute(struct timeval ts,
					__u16 type,
					__u16 code, 
					__s32 value, 
					FbBackend* be, 
					epx_event_t* ev);

static int process_input_event_key(struct timeval ts,
				   __u16 type,
				   __u16 code, 
				   __s32 value, 
				   FbBackend* be, 
				   epx_event_t* ev);
#endif /* HAVE_INPUT_EVENT */

void fb_dump_vinfo(char *header, struct fb_var_screeninfo *vinfo)
{
    (void) vinfo; // For make relaease
    (void) header;// For make relaease
    DEBUGF("fb: %s", header);
    DEBUGF("fb: vinfo.xres            %lu", vinfo->xres);
    DEBUGF("fb: vinfo.yres            %lu", vinfo->yres);
    DEBUGF("fb: vinfo.xres_virtual    %lu", vinfo->xres_virtual);
    DEBUGF("fb: vinfo.yres_virtual    %lu", vinfo->yres_virtual);
    DEBUGF("fb: vinfo.xoffset         %lu", vinfo->xoffset);
    DEBUGF("fb: vinfo.yoffset         %lu", vinfo->yoffset);
    DEBUGF("fb: vinfo.bits_per_pixel  %lu", vinfo->bits_per_pixel);
    DEBUGF("fb: vinfo.grayscale       %lu", vinfo->grayscale);
    DEBUGF("fb: vinfo.red%s", "");
    DEBUGF("fb:          .offset      %lu", vinfo->red.offset);
    DEBUGF("fb:          .length      %lu", vinfo->red.length);
    DEBUGF("fb:          .msb_right   %lu", vinfo->red.msb_right);
    DEBUGF("fb: vinfo.green%s", "");
    DEBUGF("fb:          .offset      %lu", vinfo->green.offset);
    DEBUGF("fb:          .length      %lu", vinfo->green.length);
    DEBUGF("fb:          .msb_right   %lu", vinfo->green.msb_right);
    DEBUGF("fb: vinfo.blue%s", "");
    DEBUGF("fb:          .offset      %lu", vinfo->blue.offset);
    DEBUGF("fb:          .length      %lu", vinfo->blue.length);
    DEBUGF("fb:          .msb_right   %lu", vinfo->blue.msb_right);
    DEBUGF("fb: vinfo.transp%s", "");
    DEBUGF("fb:          .offset      %lu", vinfo->transp.offset);
    DEBUGF("fb:          .length      %lu", vinfo->transp.length);
    DEBUGF("fb:          .msb_right   %lu", vinfo->transp.msb_right);
    DEBUGF("fb: vinfo.nonstd          %lu", vinfo->nonstd);
    DEBUGF("fb: vinfo.activate        %lu", vinfo->activate);
    DEBUGF("fb: vinfo.height          %lu", vinfo->height);
    DEBUGF("fb: vinfo.width           %lu", vinfo->width);
    DEBUGF("fb: vinfo.pixclock        %lu", vinfo->pixclock);
    DEBUGF("fb: vinfo.left_margin     %lu", vinfo->left_margin);
    DEBUGF("fb: vinfo.upper_margin    %lu", vinfo->upper_margin);
    DEBUGF("fb: vinfo.lower_margin    %lu", vinfo->lower_margin);
    DEBUGF("fb: vinfo.hsync_len       %lu", vinfo->hsync_len);
    DEBUGF("fb: vinfo.vsync_len       %lu", vinfo->vsync_len);
    DEBUGF("fb: vinfo.sync            %lu", vinfo->sync);
    DEBUGF("fb: vinfo.rotate          %lu\n", vinfo->rotate);
}

void fb_dump_finfo(char *header, struct fb_fix_screeninfo *finfo)
{
    (void) finfo; // For make relaease
    (void) header;// For make relaease
    DEBUGF("fb: %s", header);
    DEBUGF("fb: finfo.id              %s", finfo->id);
    DEBUGF("fb: finfo.smem_start      0x%X", finfo->smem_start);
    DEBUGF("fb: finfo.smem_len        %lu", finfo->smem_len);
    DEBUGF("fb: finfo.type            0x%X", finfo->type);
    DEBUGF("fb: finfo.type_aux        %lu", finfo->type_aux);
    DEBUGF("fb: finfo.visual          %lu", finfo->visual);
    DEBUGF("fb: finfo.xpanstep        %hu", finfo->xpanstep);
    DEBUGF("fb: finfo.ypanstep        %hu", finfo->ypanstep);
    DEBUGF("fb: finfo.ywrapstep       %hu", finfo->ywrapstep);
    DEBUGF("fb: finfo.line_length     %lu", finfo->line_length);
    DEBUGF("fb: finfo.mmio_start      %lu", finfo->mmio_start);
    DEBUGF("fb: finfo.mmio_len        %lu", finfo->mmio_len);
    DEBUGF("fb: finf.accel            %lu", finfo->accel);
}

// load backend pixel type info rfrom vinfo
static int load_pixel_format(epx_backend_t* backend,
			     struct fb_var_screeninfo *vinfo)
{
    int r_size, g_size, b_size, a_size;
    int r_offs, g_offs, b_offs, a_offs;
    int fmt = 0;
    int bgr = 0;
    int alpha = 0;
    int alpha_first = 0;
    int little_endian = 0;
    int bits_per_pixel = 0;
    epx_format_t f;

    r_size = vinfo->red.length;
    r_offs = vinfo->red.offset;
    g_size = vinfo->green.length;
    g_offs = vinfo->green.offset;
    b_size = vinfo->blue.length;
    b_offs = vinfo->blue.offset;
    a_size = vinfo->transp.length;
    a_offs = vinfo->transp.offset;

#if BYTE_ORDER == LITTLE_ENDIAN
    little_endian = 1;  // stored in native format
#endif
    bgr = (b_offs > g_offs);
    alpha = (a_size > 0);   // alpha channel present 
    alpha_first = alpha && (a_offs > r_offs);
    bits_per_pixel = vinfo->bits_per_pixel;
    if (vinfo->grayscale) fmt = EPX_FMT_GRAY;
    else if ((r_size==g_size) && (g_size==b_size)) {
	switch(r_size) {
	case 4: fmt = EPX_FMT_RGB4; break;
	case 5: fmt = EPX_FMT_RGB5; break;
	case 8: fmt = EPX_FMT_RGB8; break;
	case 10: fmt = EPX_FMT_RGB10; break;
	case 12: fmt = EPX_FMT_RGB12; break;
	case 16: fmt = EPX_FMT_RGB16; break;
	default: break;
	}
    }
    else if ((r_size==3)&&(g_size==3)&&(b_size==2))
	fmt = EPX_FMT_RGB332;
    else if ((r_size==2)&&(g_size==3)&&(b_size==2))
	fmt = EPX_FMT_RGB232;
    else if ((r_size==5)&&(g_size==6)&&(b_size==5))
	fmt = EPX_FMT_RGB565;
    else if ((r_size>0) && (g_size==0) && (b_size==0))
	fmt = EPX_FMT_RED;
    else if ((r_size==0) && (g_size>0) && (b_size==0))
	fmt = EPX_FMT_GREEN;
    else if ((r_size==0) && (g_size==0) && (b_size>0))
	fmt = EPX_FMT_BLUE;

    DEBUGF("little_endian: %d", little_endian);
    DEBUGF("bgr: %d", bgr);
    DEBUGF("alpha: %d", alpha);
    DEBUGF("alpha_first: %d", alpha_first);
    DEBUGF("bits_per_pixel: %d", bits_per_pixel);
    DEBUGF("fmt: %d", fmt);

    f = EPX_FMT(fmt,bgr,alpha,alpha_first,little_endian,bits_per_pixel);
    backend->formats[0] = f;
    backend->nformats = 1;
    return 1;
}

static epx_pixmap_t* screen_pixels(FbBackend* be, int active)
{
  if (be->dbuf)
    return (be->cbuf == active) ? &be->screen[1] : &be->screen[0];
  else
    return &be->screen[0];
}

#ifdef HAVE_INPUT_EVENT

static uint32_t const cursorb[33][24] = {
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x01000000,0x03000000,0x03000000,0x01000000,0x01000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x02000000,0x07000000,0x10000000,0x13000000,0x0f000000,0x07000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x04000000,0x13000000,0x26000000,0xffffffff,0xf3f9f9f9,0x245c5c5c,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x07000000,0x1d000000,0x3f000000,0xe2dddddd,0xfaa4a4a4,0xdde8e8e8,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x24000000,0x50000000,0xe7d8d8d8,0xff3c3c3c,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x57000000,0xead6d6d6,0xff3c3c3c,0xff000000,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x57000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff000000,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff000000,0xff000000,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xfa959595,0xdee7e7e7,0x2a5b5b5b,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xff000000,0xfa959595,0xdde8e8e8,0x295d5d5d,0x08000000,0x01000000,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff000000,0xff000000,0xff060606,0xfc6d6d6d,0xfaa8a8a8,0xf9a9a9a9,0xf7aaaaaa,0xffffffff,0xcdeaeaea,0x0e000000,0x03000000,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x58000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff000000,0xff080808,0xff333333,0xff000000,0xfde0e0e0,0xc89f9f9f,0xad868686,0x9d949494,0x8ba7a7a7,0x4e939393,0x10000000,0x03000000,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x57000000,0xebd5d5d5,0xff3c3c3c,0xff000000,0xff212121,0xfce4e4e4,0xffffffff,0xff0d0d0d,0xff3b3b3b,0xf2eaeaea,0x58060606,0x3d000000,0x2a000000,0x18000000,0x09000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x26000000,0x57000000,0xead6d6d6,0xff3c3c3c,0xff494949,0xfbededed,0xc58d8d8d,0xe1b9b9b9,0xfd747474,0xff000000,0xf7c8c8c8,0x83adadad,0x22000000,0x11000000,0x08000000,0x03000000,0x01000000,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x08000000,0x24000000,0x52000000,0xe8d7d7d7,0xfc909090,0xf6ececec,0x8e686868,0x70000000,0x9f555555,0xfae3e3e3,0xff020202,0xff494949,0xe6e9e9e9,0x1b000000,0x06000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x07000000,0x1d000000,0x42000000,0xdddcdcdc,0xe5e3e3e3,0x5d3f3f3f,0x3d000000,0x46000000,0x69000000,0xedd8d8d8,0xff4d4d4d,0xff000000,0xf7d7d7d7,0x6ab4b4b4,0x0d000000,0x02000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x05000000,0x13000000,0x2a000000,0x54707070,0x38121212,0x25000000,0x19000000,0x26000000,0x4e000000,0xad898989,0xfabdbdbd,0xff000000,0xff565656,0xdde3e3e3,0x1a000000,0x06000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x02000000,0x08000000,0x12000000,0x17000000,0x14000000,0x0a000000,0x07000000,0x14000000,0x39000000,0x6d0e0e0e,0xf9f0f0f0,0xff262626,0xff020202,0xf8e8e8e8,0x59b5b5b5,0x09000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x01000000,0x04000000,0x05000000,0x04000000,0x01000000,0x01000000,0x0a000000,0x26000000,0x51000000,0xc2acacac,0xfca3a3a3,0xfa989898,0xffffffff,0x48919191,0x0a000000,0x01000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x04000000,0x16000000,0x37000000,0x62222222,0xfcfcfcfc,0xb9c0c0c0,0x4b444444,0x1a000000,0x07000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x01000000,0x0a000000,0x1c000000,0x30000000,0x40282828,0x31000000,0x1e000000,0x0c000000,0x03000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x02000000,0x09000000,0x11000000,0x15000000,0x11000000,0x09000000,0x03000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x02000000,0x02000000,0x01000000,0x01000000,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff},
  {0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff,0x00ffffff}
};

#define CURSOR_HOTSPOT_X 0
#define CURSOR_HOTSPOT_Y 0
#define CURSOR_HEIGHT    33
#define CURSOR_WIDTH     24
#define CURSOR_TIMEOUT   4

static int load_cursor(FbBackend* be)
{
  int x, y;
  epx_pixmap_t* def;
  struct timeval t, t1;
  DEBUGF("load_cursor %lux%lu", CURSOR_WIDTH, CURSOR_HEIGHT);

  be->hotspot_x = CURSOR_HOTSPOT_X;
  be->hotspot_y = CURSOR_HOTSPOT_Y;
  be->cursor = NULL;
  be->save   = NULL;
  be->save_x = -1;
  be->save_y = -1;
  
  gettimeofday(&t, NULL);
  t1.tv_sec = (CURSOR_TIMEOUT+1);
  t1.tv_usec = 0;
  timersub(&t, &t1, &be->mouse_ts);

  def = epx_pixmap_create(CURSOR_WIDTH, CURSOR_HEIGHT, EPX_FORMAT_ARGB);
  if (def) {
    for (y = 0; y < (int)CURSOR_HEIGHT; y++) {
      for (x = 0; x < (int)CURSOR_WIDTH; x++) {
	epx_pixel_t color;
	uint32_t px = cursorb[y][x];
	// color.px = cursorb[y][x];
	color.a = (px>>24) & 0xff;
	color.r = (px>>16) & 0xff;
	color.g = (px>>8) & 0xff;
	color.b = px & 0xff;
	epx_pixmap_put_pixel(def, x, y, 0, color);
      }
    }
  }
  be->cursor = def;
  be->save = epx_pixmap_create(CURSOR_WIDTH, CURSOR_HEIGHT, EPX_FORMAT_ARGB);
  return 0;
}


static int hide_cursor(FbBackend* be)
{
  epx_pixmap_t* scr;
  int x, y;

  if ((be->cursor == NULL) || (be->save == NULL))
    return -1;
  if (((x=be->save_x) < 0) || ((y=be->save_y) < 0))
    return 0;
  DEBUGF("hide_cursor (%d,%d)", x, y);

  scr = screen_pixels(be, 1);
  epx_pixmap_copy_area(be->save, scr,
		       0, 0, x, y,
		       be->save->width,
		       be->save->height,
		       EPX_FLAG_NONE);
  return 1;
}


static int show_cursor(FbBackend* be, int timeout)
{
  epx_pixmap_t* scr;
  int x, y;

  if ((be->cursor == NULL) || (be->save == NULL))
    return -1;
  
  if (timeout) {
    struct timeval t;
    gettimeofday(&t, NULL);
    timersub(&t, &be->mouse_ts, &t);
    if (t.tv_sec >= timeout)
      return 0;
  }
  x = be->mouse_x;
  y = be->mouse_y;
  DEBUGF("show_cursor (%d,%d)", x,  y);
  scr = screen_pixels(be, 1);
  epx_pixmap_copy_area(scr, be->save,
		       x, y, 0, 0,
		       be->save->width,
		       be->save->height,
		       EPX_FLAG_NONE);
  be->save_x = x;
  be->save_y = y;
  epx_pixmap_copy_area(be->cursor, scr,
		       0, 0, x, y,
		       be->cursor->width,
		       be->cursor->height,
		       EPX_FLAG_BLEND);
  return 1;
}

#endif

//
//  Fixme: blank can also be disabled with 
//    echo 0 > /sys/class/graphics/fb0/blank
//  
//  stop blink:
//    echo 0 > /sys/class/graphics/fbcon/cursor_blink
// http://www.armadeus.com/wiki/index.php?title=Framebuffer
//

static void send_cursor_off(int fd)
{
    const char cursoroff_str[] = "\033[?25l\033[?1c\033[25m";
    const char blankoff_str[] = "\033[9;0]";
    int r;
    r = write(fd, cursoroff_str, strlen(cursoroff_str));
    if (r < 0)
	r = write(fd, blankoff_str, strlen(blankoff_str));
    if (r < 0) {
	DEBUGF("write failed: %s", strerror(errno));
    }
}

static void cursor_off(int use_stderr)
{
    if (use_stderr) {
        send_cursor_off(2);      
    }
    else {
        int fd;
	if ((fd = open("/dev/console", O_RDWR)) < 0) {
	  DEBUGF("failed to open /dev/console: %s", strerror(errno));
	  return;
	}
	send_cursor_off(fd);
	close(fd);
    }
}


static void fb_mod_vinfo(epx_dict_t *param, struct fb_var_screeninfo *vinfo)
{
    int   int_param;
    unsigned int  uint_param;
    char* string_param;

    if (epx_dict_lookup_integer(param, "width", &int_param) != -1) {
	vinfo->xres         = int_param;
	vinfo->xres_virtual = int_param;
    }

    if (epx_dict_lookup_integer(param, "virt_width", &int_param) != -1) {
	if(int_param >= (int)vinfo->xres)
	    vinfo->xres_virtual = int_param;
    }

    if (epx_dict_lookup_integer(param, "height", &int_param) != -1) {
	vinfo->yres = int_param;
	vinfo->yres_virtual = int_param;
    }
    if (epx_dict_lookup_integer(param, "virt_height", &int_param) != -1) {
	if (int_param >= (int)vinfo->yres)
	    vinfo->yres_virtual = int_param;
    }

    // either pixel_format (string) or pixel_type (old, int)
    if (epx_dict_lookup_string(param,"pixel_format",&string_param,NULL) != -1) {
	epx_format_t fmt = epx_pixel_format_from_name(string_param);
	if (fmt != EPX_FORMAT_INVALID)
	    vinfo->bits_per_pixel = EPX_PIXEL_BIT_SIZE(fmt);
    }
    else {
	if (epx_dict_lookup_integer(param,  "pixel_type", &int_param) != -1) {
	    vinfo->bits_per_pixel = EPX_PIXEL_BIT_SIZE(int_param);
	}
    }

    if (epx_dict_lookup_integer(param, "pixclock", &int_param) != -1 &&
	(int_param != -1))
	vinfo->pixclock = int_param;

    if (epx_dict_lookup_integer(param, "xoffset", &int_param) != -1 &&
	(int_param != -1))
	vinfo->xoffset = int_param;

    if (epx_dict_lookup_integer(param, "yoffset", &int_param) != -1 &&
	(int_param != -1))
	vinfo->yoffset = int_param;

    if (epx_dict_lookup_integer(param, "left_margin", &int_param) != -1 &&
	(int_param != -1))
	vinfo->left_margin = int_param;


    if (epx_dict_lookup_integer(param, "right_margin", &int_param) != -1 &&
	(int_param != -1))
	vinfo->right_margin = int_param;

    if (epx_dict_lookup_integer(param, "upper_margin", &int_param)  != -1 &&
	(int_param != -1))
	vinfo->upper_margin = int_param;

    if (epx_dict_lookup_integer(param, "lower_margin", &int_param)  != -1 &&
	(int_param != -1))
	vinfo->lower_margin = int_param;

    if (epx_dict_lookup_integer(param, "hsync_len", &int_param)  != -1 &&
	(int_param != -1))
	vinfo->hsync_len = int_param;

    if (epx_dict_lookup_integer(param, "vsync_len", &int_param)  != -1 &&
	(int_param != -1))
	vinfo->vsync_len = int_param;

    if (epx_dict_lookup_integer(param, "sync", (int*) &uint_param) != -1  &&
	(uint_param != 0xFFFFFFFF))
	vinfo->sync = uint_param;

    if (epx_dict_lookup_integer(param, "vmode", (int*) &uint_param)  != -1 &&
	(uint_param != 0xFFFFFFFF))
	vinfo->vmode = uint_param;

}

epx_backend_t* fb_init(epx_dict_t* param)
{
    FbBackend* be;
    char* string_param;
    int   int_param;
    int   r;

    if ((be = (FbBackend*) malloc(sizeof(FbBackend))) == NULL)
	return NULL;

    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);
    be->b.name = "fb";
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.use_opengl = 0;
    be->b.width = 0;
    be->b.height = 0;
    be->b.nformats = 0;
    be->b.cb = &fb_callbacks;
    epx_object_list_init(&be->b.pixmap_list);
    epx_object_list_init(&be->b.window_list);
    be->b.event = EPX_INVALID_HANDLE;
#ifdef HAVE_MTRR
    be->mtrr_fd = -1;
#endif

#ifdef HAVE_INPUT_EVENT
    be->poll_fd = -1;
    be->input_fd_sz = 0;
    be->mouse_x = 0;
    be->mouse_y = 0;
    be->mouse_z = 0;
    be->mouse_moved = 0;
    be->mouse_buttons = 0;
    be->keyboard_mods = 0;
    memset(&be->ev, 0, sizeof(be->ev));
    load_cursor(be);
    r = sizeof(be->input_fd) / sizeof(be->input_fd[0]);
    while(r--)
	be->input_fd[r] = -1;

    // setup event type callbacks 
    memset(ev_callbacks, 0, sizeof(ev_callbacks));
    ev_callbacks[EV_KEY] = process_input_event_key;
    ev_callbacks[EV_REL] = process_input_event_relative;
    ev_callbacks[EV_ABS] = process_input_event_absolute;
    ev_callbacks[EV_SYN] = process_input_event_syn;
    
    if (!setup_input_system(be, param)) 
	DEBUGF("Failed to setup input system. Disabled");

#endif
    
    if (epx_dict_lookup_string(param, "framebuffer_device", &string_param, NULL) == -1) {
	DEBUGF("missing framebuffer_device paramter. Defaulting to /dev/fb%d",
		0);
	string_param = "/dev/fb0";
    }

    if ((be->fb_fd = open(string_param, O_RDWR)) == -1) {
	DEBUGF("Could not open frame buffer [%s]: [%s]", string_param, strerror(errno));
	goto error;
    }

    
    if (ioctl(be->fb_fd, FBIOGET_VSCREENINFO, &be->ovinfo) == -1) {
	DEBUGF("ioctl:FBIOGET_VSCREENINFO failed: [%s]", strerror(errno));
	goto error;
    }
    be->b.width = be->ovinfo.xres;
    be->b.height = be->ovinfo.yres;
    load_pixel_format(&be->b, &be->ovinfo);

    fb_dump_vinfo("Retrieved values.", &be->ovinfo);
#ifdef HAVE_INPUT_EVENT
    be->mouse_x = be->b.width / 2;
    be->mouse_y = be->b.height / 2;
#endif

    cursor_off(0);

    be->vinfo  = be->ovinfo;
    be->vinfo.bits_per_pixel = EPX_PIXEL_BIT_SIZE(EPX_FORMAT_ARGB);
    be->vinfo.xoffset = 0;
    be->vinfo.yoffset = 0;

    fb_mod_vinfo(param, &be->vinfo);


    /* Check if we should draw directly in pixmap */
    be->direct_pixmap_draw = 0;
    if (epx_dict_lookup_integer(param, "direct_pixmap_draw", &int_param) != -1)
      be->direct_pixmap_draw = int_param;

    be->cbuf = 0;
    be->dbuf = 1;
    if (epx_dict_lookup_integer(param, "double_buffer", &int_param) != -1)
	be->dbuf = int_param;

    be->lcd_pi32 = 0;
    if (epx_dict_lookup_integer(param, "lcd_pi32", &int_param) != -1)
	be->lcd_pi32 = int_param;

    return (epx_backend_t*) &(be->b);
error:
    free(be);
    return NULL;
}

int fb_upgrade(epx_backend_t* backend)
{
    backend->cb = &fb_callbacks;
    return 0;
}


static int fb_adjust(epx_backend_t *backend, epx_dict_t* param)
{
    FbBackend* be = (FbBackend *) backend;
    //
    // Retrieve info.
    //
    if (ioctl(be->fb_fd, FBIOGET_VSCREENINFO, &be->vinfo) == -1) {
	DEBUGF("ioctl:FBIOGET_VSCREENINFO failed: [%s]", strerror(errno));
	return 0;
    }

    fb_dump_vinfo("Retrieved values.", &be->vinfo);
    fb_mod_vinfo(param, &be->vinfo);

    //
    // Do some tests
    //
    be->vinfo.activate = FB_ACTIVATE_NOW;
    fb_dump_vinfo("Adjusted values to be set.", &be->vinfo);

    if (ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->vinfo) < 0) {
	DEBUGF("ioctl:FBIOPUT_VSCREENINFO/ACTIVATE) failed [%s]", strerror(errno));
	return 0;
    }

    return 1;
}


static int fb_info(epx_backend_t *backend, epx_dict_t* param)
{
    (void) backend;
    (void) param;
    return 0;
}


/* return the backend event handle */
#ifdef HAVE_INPUT_EVENT
static EPX_HANDLE_T fb_evt_attach(epx_backend_t* backend)
{
    FbBackend* be = (FbBackend*) backend;

    /* Return the epoll file descriptor that we use as
       a multiplexor */
    if (be->poll_fd != -1)
	return (EPX_HANDLE_T)((long) be->poll_fd);

    return EPX_INVALID_HANDLE;
}
#else
static EPX_HANDLE_T fb_evt_attach(epx_backend_t* backend)
{
    (void) backend;
    return EPX_INVALID_HANDLE;
}
#endif /* HAVE_INPUT_EVENT */

static int fb_evt_detach(epx_backend_t* backend)
{
    (void) backend;
    return 0;
}

static int fb_finish(epx_backend_t* backend)
{
    FbBackend* be = (FbBackend*) backend;

    munmap(be->screen[0].data, be->finfo.smem_len);
    close(be->fb_fd);

#ifdef HAVE_INPUT_EVENT
    /* Close all open input event files */
    while(be->input_fd_sz--) {
	close(be->input_fd[be->input_fd_sz]);
	be->input_fd[be->input_fd_sz] = -1;
    }
    be->input_fd_sz = 0;

    /* Close the poll descriptor */
    if (be->poll_fd != -1) {
	close(be->poll_fd);
	be->poll_fd = -1;
    }
#endif

#ifdef HAVE_MTRR
    if (be->mtrr_fd != -1)
	close(be->mtrr_fd);
#endif
    free(be);
    return 0;
}

static int fb_pix_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    FbBackend* be = (FbBackend*) backend;

/*    DBG("fb_pix_attach(%p)", pixmap); */
    if (pixmap->opaque != NULL)
	return -1;
    epx_object_link(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) 1;
    pixmap->backend = (epx_backend_t*) be;
    if (be->direct_pixmap_draw) {
	be->org_pixmap_data = pixmap->data;
	pixmap->data = be->screen[0].data;
    }
    return 0;
}

static int fb_pix_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    epx_object_unlink(&backend->pixmap_list, pixmap);
    FbBackend* be = (FbBackend*) backend;

    pixmap->opaque = NULL;
    pixmap->backend = NULL;

    // Reset the original data.
    if (be->direct_pixmap_draw)
	pixmap->data = be->org_pixmap_data;

    return 0;
}

static int fb_begin(epx_window_t* ewin)
{
  (void) ewin;
  return 0;
}

static int fb_end(epx_window_t* ewin,int off_screen)
{
  (void) ewin;
  (void) off_screen;
  return 0;
}

static int fb_pix_draw(epx_backend_t* backend, epx_pixmap_t* pixmap,
		       epx_window_t* ewin,
		       int src_x, int src_y, int dst_x, int dst_y,
		       unsigned int width,
		       unsigned int height)
{
    FbWindow*  nwin = (FbWindow*) ewin->opaque;
    FbBackend* be = (FbBackend*) backend;

    if (nwin == NULL)
	return -1;

    /* If we do not draw directly to pixmap. Copy it */
    if (!be->direct_pixmap_draw) {
	epx_pixmap_t* scr = screen_pixels(be, 0);
#ifdef HAVE_INPUT_EVENT
	hide_cursor(be);
#endif
	epx_pixmap_copy_area(pixmap, scr, src_x, src_y, dst_x, dst_y,
			     width, height, 0);
#ifdef HAVE_INPUT_EVENT
	show_cursor(be,CURSOR_TIMEOUT);
#endif
    }
    nwin->dcount++;
    return 0;
}

static int fb_pix_sync(epx_backend_t* backend, epx_pixmap_t* pixmap,
		       epx_window_t* ewin)
{
    (void) backend;
    (void) pixmap;
    (void) ewin;
    return 0;
}

static int fb_win_swap(epx_backend_t* backend, epx_window_t* ewin)
{
  FbBackend* be = (FbBackend*) backend;
  int r = 0;
  (void) ewin;

#ifdef HAVE_INPUT_EVENT
  hide_cursor(be);
#endif

  if (be->dbuf) {
    if (be->cbuf==0) {
      be->vinfo.yoffset = be->vinfo.yres;
      be->cbuf = 1;
    }
    else {
      be->vinfo.yoffset = 0;
      be->cbuf = 0;
    }
    r = ioctl(be->fb_fd, FBIOPAN_DISPLAY, &be->vinfo);
  }

#ifdef HAVE_INPUT_EVENT
  show_cursor(be,CURSOR_TIMEOUT);
#endif

  return r;
}

static int fb_win_attach(epx_backend_t* backend, epx_window_t* ewin)
{
    FbBackend* be = (FbBackend*) backend;
    FbWindow*  nwin;

/*    DBG("fb_win_attach(): winwow[%p]",  ewin); */

    if (ewin->opaque != NULL)
	return -1;
    if ((nwin = (FbWindow*) malloc(sizeof(FbWindow))) == NULL)
	return -1;

    nwin->wstate = 1;
    nwin->dcount = 0;
    epx_object_link(&backend->window_list, ewin);
    ewin->opaque  = (void*) nwin;
    ewin->backend = (epx_backend_t*) be;

    //
    // Do some tests
    //
    be->vinfo.activate = FB_ACTIVATE_NOW;
    be->vinfo.yres = be->vinfo.yres_virtual = ewin->area.wh.height;
    be->vinfo.xres = be->vinfo.xres_virtual = ewin->area.wh.width;

    fb_dump_vinfo("Modified values to be set.", &be->vinfo);
    if (ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->vinfo) < 0) {
	DEBUGF("ioctl:FBIOPUT_VSCREENINFO/ACTIVATE) failed [%s]", strerror(errno));
	return -1;
    }
    be->b.width = be->vinfo.xres;
    be->b.height = be->vinfo.yres;

#ifdef HAVE_INPUT_EVENT
    be->mouse_x = be->b.width / 2;
    be->mouse_y = be->b.height / 2;
#endif

    /* Can we double buffer? */
    if (be->dbuf) {
	be->vinfo.yres_virtual = be->vinfo.yres*2;
	if (ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->vinfo) < 0) {
	    DEBUGF("double buffer test failed [%s]", strerror(errno));
	    be->dbuf = 0;
	}
    }

    if (ioctl(be->fb_fd, FBIOGET_FSCREENINFO, &be->finfo) == -1) {
	DEBUGF("ioctl:FBIOGET_FSCREENINFO failed: [%s]", strerror(errno));
	return -1;
    }

    fb_dump_finfo("Fixed info.", &be->finfo);


    be->screen[0].data = (unsigned char *) mmap (0, be->finfo.smem_len, PROT_READ | PROT_WRITE, MAP_SHARED, be->fb_fd, 0);
    if (!be->screen[0].data) {
	DEBUGF("mmap of screen memory failed [%s].", strerror(errno));
	return -1;
    }

    /* Setup MTRR */
#ifdef HAVE_MTRR
    if ((be->mtrr_fd = open("/proc/mtrr", O_WRONLY, 0)) == -1)
    {
	if (errno == ENOENT) {
	    DEBUGF("/proc/mtrr not found: MTRR not enabled %s", "");
	}  else {
	    DEBUGF("Error opening /proc/mtrr: [%s], Disabled.", strerror(errno));
	}
    }
    else {
	struct mtrr_sentry sentry;

	sentry.base = be->finfo.smem_start & 0xFE000000;
	sentry.size = 0x2000000;
	sentry.type = MTRR_TYPE_WRCOMB;

	if ( ioctl(be->mtrr_fd, MTRRIOC_ADD_ENTRY, &sentry) == -1 ) {
	    DEBUGF("MTRRIOC_ADD_ENTRY(%p, %d): [%s] Disabled",
		   sentry.base, sentry.size, strerror(errno));
	    close(be->mtrr_fd);
	    be->mtrr_fd = -1;
	}
	else
	    DEBUGF("MTRR enabled at [%p] size[%d] type[MTRR_TYPE_WRCOMB]", sentry.base, sentry.size);
    }
#endif

    //
    // Pan to the beginning of things.
    // Also used to test out double buffering,.
    //
    if (ioctl(be->fb_fd, FBIOPAN_DISPLAY, &be->vinfo) == -1) {
	DEBUGF("Initial pan failed [%s]. Giving up on double buffering (if active)", strerror(errno));

	// Reset double buffering.
	if (be->dbuf) {
	    be->dbuf = 0;
	    be->vinfo.yres_virtual = be->vinfo.yres;
	    ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->vinfo);
	}
    }


    /*
     * Setup be->creen pixmap. clip member will be continously modified by fb_pix_draw
     */
    be->screen[0].width = be->vinfo.xres;
    be->screen[0].height = be->vinfo.yres;

    be->screen[0].bytes_per_pixel = be->vinfo.bits_per_pixel/8;

    be->screen[0].bytes_per_row = be->finfo.line_length;
    be->screen[0].sz = be->screen[0].bytes_per_row * be->screen[0].height;
    be->screen[0].clip.xy.x=0;
    be->screen[0].clip.xy.y=0;
    be->screen[0].clip.wh.width=be->screen[0].width;
    be->screen[0].clip.wh.height=be->screen[0].height;

    be->screen[1].width = be->vinfo.xres;
    be->screen[1].height = be->vinfo.yres;

    be->screen[1].bytes_per_pixel = be->vinfo.bits_per_pixel/8;

    be->screen[1].bytes_per_row = be->finfo.line_length;
    be->screen[1].sz = be->screen[1].bytes_per_row * be->screen[1].height;
    be->screen[1].clip.xy.x=0;
    be->screen[1].clip.xy.y=0;
    be->screen[1].clip.wh.width=be->screen[1].width;
    be->screen[1].clip.wh.height=be->screen[1].height;
    if (be->dbuf)
	be->screen[1].data = be->screen[0].data + be->screen[0].sz;
    else
	be->screen[1].data = NULL;

    if (load_pixel_format(&be->b, &be->vinfo) > 0) {
	be->screen[0].pixel_format = be->b.formats[0];
	be->screen[1].pixel_format = be->b.formats[0];
    }
    return 0;
}

static int fb_win_detach(epx_backend_t* backend, epx_window_t* ewin)
{
    FbBackend* be = (FbBackend*) backend;
    FbWindow*  win  = (FbWindow*)  ewin->opaque;

    if ((be != NULL) && (win->wstate != 0)) {
	free(win);
	epx_object_unlink(&backend->window_list, ewin);
	ewin->opaque  = NULL;
	ewin->backend = NULL;

	// Reset double buffering panning.
	if (be->dbuf)
	    ioctl(be->fb_fd, FBIOPAN_DISPLAY, &be->ovinfo);

	// Reset original screen stats
	be->ovinfo.activate = FB_ACTIVATE_NOW;
	ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->ovinfo);
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

#ifdef HAVE_INPUT_EVENT
static int fb_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    FbBackend* be = (FbBackend*) backend;
    struct input_event buf; /* Read all events sequentially with no buffering*/
    int nfds = 0;
    struct epoll_event ev[MAX_INPUT_SOURCE];

    /* If no mouse is open, return 0 */
    if (be->poll_fd == -1)
	return 0;

    /* Locate the descriptor in the epoll set that is ready to read */
    nfds = epoll_wait(be->poll_fd, ev, MAX_INPUT_SOURCE, -1);

    /*
       Since we can only process a single event at the time, we will
       just read the first event descriptor and return.  be->poll_fd
       will trigger immediately again, and call fb_evt_read.  Not very
       efficient, but it kind of works.
       
       What we really want is a way to return multiple events in one go.

       You can actually read process and buffer all events you want,
       but you need to maintain a queue and set the pending flag and
       return number of pending events (like x11 is processed)
       see return value in function description.
    */
    if (ev[nfds-1].data.fd == -1)
	exit(0);
    
    if (read(ev[nfds-1].data.fd, (char*) &buf, sizeof(buf))== sizeof(buf)) {
	/* If we have no windows attached, we cannot set e->window, 
	   which will trigger core.
	   return 0.
	*/
	if (be->b.window_list.first == NULL) {
	    DEBUGF("No window attached. Will return 0");
	    return 0;
	}

	if ((buf.type < EV_CNT) && (ev_callbacks[buf.type] != NULL)) {
	    int r;

	    be->ev.window = (epx_window_t*) be->b.window_list.first;
	    r = (*ev_callbacks[buf.type])(buf.time, buf.type, buf.code,
					  buf.value, be, &be->ev);
	    if (r) {  // normally only by EV_SYN
		memcpy(e, &be->ev, sizeof(be->ev));
		be->ev.type = 0;
		return 1;
	    }
	    return 0;
	}
	DEBUGF("Unknown event type[%d] code[%.4X] value[%d]. Ignored.", 
		   buf.type, buf.code, buf.value);
    }
    else
	DEBUGF("Short read");
	

    return 0;
}
#else
static int fb_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    (void) backend;
    (void) e;
    return -1;
}
#endif /* HAVE_INPUT_EVENT */

static int fb_win_adjust(epx_window_t *win, epx_dict_t* param)
{
    (void) win;
    (void) param;
    return 1;
}

#ifdef HAVE_INPUT_EVENT

static void abs_dump(char* axis, int valid, struct input_absinfo* aptr)
{
    DEBUGF("fb: abs: %s%s", axis, valid ? "" : "INVALID");
    if (valid) {
      DEBUGF("fb:   %s.value: %d", axis, aptr->value);
      DEBUGF("fb: %s.minimum: %d", axis, aptr->minimum);
      DEBUGF("fb: %s.maximum: %d", axis, aptr->maximum);
      DEBUGF("fb:    %s.fuzz: %d", axis, aptr->fuzz);
      DEBUGF("fb:    %s.flat: %d", axis, aptr->flat);
      DEBUGF("fb:     %s.res: %d", axis, aptr->resolution);
    }
}

static int setup_input_system(FbBackend* be, epx_dict_t *param)
{
    static char *input_param_keys[] = {
	"input_mouse_device",
	"input_keyboard_device",
	"input_absolute_device",
	"input_relative_device"
	/* More? */
    };
    char val[256];
    char *string_param;
    int param_ind = sizeof(input_param_keys)/sizeof(input_param_keys[0]);

    be->ax_valid = 0;
    be->ay_valid = 0;
    be->az_valid = 0;
    be->ap_valid = 0;

    /* Close off any open descriptors */
    if (be->poll_fd != -1) {
	close(be->poll_fd);
	be->poll_fd = -1;
    }

    while(be->input_fd_sz--) {
	if (be->input_fd[be->input_fd_sz] != -1) {
	    DEBUGF("Closing [%d/%d]", be->input_fd_sz, be->input_fd[be->input_fd_sz]);
	    close(be->input_fd[be->input_fd_sz]);
	    be->input_fd[be->input_fd_sz] = -1;
	}
    }
    be->input_fd_sz = 0;

    /* Go through the input_params array and try to retrieve their values */
    while(param_ind--) {
	struct epoll_event ev;
	
	/* Extract the value for the current parameter key */
	DEBUGF("Find string value for key [%s]", input_param_keys[param_ind]);
	if (epx_dict_lookup_string(param, 
				   input_param_keys[param_ind], 
				   &string_param, 
				   NULL) != -1) {
	    int fd;
	    /* Ensure that retrieved string is null terminated */
	    strncpy(val, string_param, sizeof(val) - 1);
	    val[sizeof(val)-1] = 0;

	    DEBUGF("Opening input event file[%s]", val);
	    /* Open input  */
	    if ((fd = open(val, O_RDONLY | O_NONBLOCK)) == -1) {
		WARNINGF("Error opening mouse input event file[%s]: [%s]", 
			   val, strerror(errno));
		continue;
	    }

	    if (param_ind == 0) { // input_mouse_device
	      int r;
	      // query scale info about absolute coordinates
	      r = ioctl(fd, EVIOCGABS(ABS_X), &be->ax);
	      be->ax_valid = (r >= 0);
	      abs_dump("ABS_X", be->ax_valid, &be->ax);
	      r = ioctl(fd, EVIOCGABS(ABS_Y), &be->ay);
	      be->ay_valid = (r >= 0);
	      abs_dump("ABS_Y", be->ay_valid, &be->ay);
	      r = ioctl(fd, EVIOCGABS(ABS_Z), &be->az);
	      be->az_valid = (r >= 0);
	      abs_dump("ABS_Z", be->az_valid, &be->az);
	      r = ioctl(fd, EVIOCGABS(ABS_PRESSURE), &be->ap);
	      be->ap_valid = (r >= 0);
	      abs_dump("ABS_P", be->ap_valid, &be->ap);
	    }
	    /* Setup an epoll for all input file descriptors */
	    if (be->poll_fd == -1)  {
		be->poll_fd = epoll_create(MAX_INPUT_SOURCE);
	    }
	    ev.events = EPOLLIN;
	    ev.data.fd = fd;
	    epoll_ctl(be->poll_fd, EPOLL_CTL_ADD, ev.data.fd, &ev);
	    be->input_fd[be->input_fd_sz++] = fd;
	}    
    }
    return 1;
}

static void modify(FbBackend* be, __s32 value, uint16_t mod)
{
  if (value) 
    be->keyboard_mods |= mod;
  else
    be->keyboard_mods &= ~mod;
}

static void toggle(FbBackend* be, __s32 value, uint16_t mod)
{
  if (value)
    be->keyboard_mods ^= mod;
}

static int ascii(FbBackend* be, int lower)
{
  return (be->keyboard_mods & (EPX_KBD_MOD_CAPS|EPX_KBD_MOD_SHIFT))
    ? lower - 32 : lower;
}

static int process_input_event_key(struct timeval ts, 
				   __u16 type, __u16 code, __s32 value, 
				   FbBackend* be, 
				   epx_event_t* e)
{
    DEBUGF("EV_KEY type[%d] code[%d] val[%d]", type, code, value);
    (void) ts;
    (void) be;

    if (be->lcd_pi32) {
#define SSD1289_GET_KEYS _IOR('K', 1, unsigned char *)
	unsigned char keys;
	if (ioctl(be->fb_fd, SSD1289_GET_KEYS, &keys) == -1)
	    perror("_apps ioctl get");
	else {
	    unsigned char changed = be->lcd_pi32_keys ^ keys;
	    if (changed) {
	      int i;
	      be->lcd_pi32_keys = keys;
	      for (i = 0; i < 7; i++) {
		if (changed & 0x1) {
		  e->key.sym = EPX_KBD_KEY_F1 + i;
		  value = (keys & 0x01) == 0x01;
		  goto key_value;
		}
		changed >>= 1;
		keys >>= 1;
	      }
	    }
	}
#undef SSD1289_GET_KEYS
    }
    switch(code) {
    case KEY_ESC: e->key.sym = '\e'; break;
    case KEY_1: e->key.sym = '1'; break;
    case KEY_2: e->key.sym = '2'; break;
    case KEY_3: e->key.sym = '3'; break;
    case KEY_4: e->key.sym = '4'; break;
    case KEY_5: e->key.sym = '5'; break;
    case KEY_6: e->key.sym = '6'; break;
    case KEY_7: e->key.sym = '7'; break;
    case KEY_8: e->key.sym = '8'; break;
    case KEY_9: e->key.sym = '9'; break;
    case KEY_0: e->key.sym = '0'; break;
    case KEY_MINUS: e->key.sym = '-'; break;
    case KEY_EQUAL: e->key.sym = '='; break;
    case KEY_BACKSPACE: e->key.sym = '\b'; break;
    case KEY_TAB: e->key.sym = '\t'; break;
    case KEY_Q: e->key.sym = ascii(be,'q'); break;
    case KEY_W: e->key.sym = ascii(be,'w'); break;
    case KEY_E: e->key.sym = ascii(be,'e'); break;
    case KEY_R: e->key.sym = ascii(be,'r'); break;
    case KEY_T: e->key.sym = ascii(be,'t'); break;
    case KEY_Y: e->key.sym = ascii(be,'y'); break;
    case KEY_U: e->key.sym = ascii(be,'u'); break;
    case KEY_I: e->key.sym = ascii(be,'i'); break;
    case KEY_O: e->key.sym = ascii(be,'o'); break;
    case KEY_P: e->key.sym = ascii(be,'p'); break;
    case KEY_LEFTBRACE: e->key.sym = '['; break;
    case KEY_RIGHTBRACE: e->key.sym = ']'; break;
    case KEY_ENTER: e->key.sym = '\r'; break;
    case KEY_A: e->key.sym = ascii(be,'a'); break;
    case KEY_S: e->key.sym = ascii(be,'s'); break;
    case KEY_D: e->key.sym = ascii(be,'d'); break;
    case KEY_F: e->key.sym = ascii(be,'f'); break;
    case KEY_G: e->key.sym = ascii(be,'g'); break;
    case KEY_H: e->key.sym = ascii(be,'h'); break;
    case KEY_J: e->key.sym = ascii(be,'j'); break;
    case KEY_K: e->key.sym = ascii(be,'k'); break;
    case KEY_L: e->key.sym = ascii(be,'l'); break;
    case KEY_SEMICOLON: e->key.sym = ';'; break;
    case KEY_APOSTROPHE: e->key.sym = '\''; break;
    case KEY_GRAVE: e->key.sym = '`'; break;
    case KEY_Z: e->key.sym = ascii(be,'z'); break;
    case KEY_X: e->key.sym = ascii(be,'x'); break;
    case KEY_C: e->key.sym = ascii(be,'c'); break;
    case KEY_V: e->key.sym = ascii(be,'v'); break;
    case KEY_B: e->key.sym = ascii(be,'b'); break;
    case KEY_N: e->key.sym = ascii(be,'n'); break;
    case KEY_M: e->key.sym = ascii(be,'m'); break;
    case KEY_COMMA: e->key.sym = '.'; break;
    case KEY_DOT: e->key.sym = '.'; break;
    case KEY_SLASH: e->key.sym = '/'; break;
    case KEY_KPASTERISK: e->key.sym = '*'; break;
    case KEY_SPACE: e->key.sym = ' '; break;
    case KEY_F1 : e->key.sym = EPX_KBD_KEY_F1; break;
    case KEY_F2:  e->key.sym = EPX_KBD_KEY_F2; break;
    case KEY_F3:  e->key.sym = EPX_KBD_KEY_F3; break;
    case KEY_F4:  e->key.sym = EPX_KBD_KEY_F4; break;
    case KEY_F5:  e->key.sym = EPX_KBD_KEY_F5; break;
    case KEY_F6:  e->key.sym = EPX_KBD_KEY_F6; break;
    case KEY_F7:  e->key.sym = EPX_KBD_KEY_F7; break;
    case KEY_F8:  e->key.sym = EPX_KBD_KEY_F8; break;
    case KEY_F9:  e->key.sym = EPX_KBD_KEY_F9; break;
    case KEY_F10: e->key.sym = EPX_KBD_KEY_F10; break;
    case KEY_F11: e->key.sym = EPX_KBD_KEY_F11; break;
    case KEY_F12: e->key.sym = EPX_KBD_KEY_F12; break;
    case KEY_KP7: e->key.sym = EPX_KBD_KEY_KP7; break;
    case KEY_KP8: e->key.sym = EPX_KBD_KEY_KP8; break;
    case KEY_KP9: e->key.sym = EPX_KBD_KEY_KP9; break;
    case KEY_KPMINUS: e->key.sym = EPX_KBD_KEY_KP_MINUS; break;
    case KEY_KP4: e->key.sym = EPX_KBD_KEY_KP4; break;
    case KEY_KP5: e->key.sym = EPX_KBD_KEY_KP5; break;
    case KEY_KP6: e->key.sym = EPX_KBD_KEY_KP6; break;
    case KEY_KPPLUS: e->key.sym = EPX_KBD_KEY_KP_PLUS; break;
    case KEY_KP1: e->key.sym = EPX_KBD_KEY_KP1; break;
    case KEY_KP2: e->key.sym = EPX_KBD_KEY_KP2; break;
    case KEY_KP3: e->key.sym = EPX_KBD_KEY_KP3; break;
    case KEY_KP0: e->key.sym = EPX_KBD_KEY_KP0; break;
    case KEY_KPDOT: e->key.sym = EPX_KBD_KEY_KP_PERIOD; break;
    case KEY_HOME: e->key.sym = EPX_KBD_KEY_HOME; break;
    case KEY_UP: e->key.sym = EPX_KBD_KEY_UP; break;
    case KEY_PAGEUP: e->key.sym = EPX_KBD_KEY_PAGEUP; break;
    case KEY_LEFT: e->key.sym = EPX_KBD_KEY_LEFT; break;
    case KEY_RIGHT: e->key.sym = EPX_KBD_KEY_RIGHT; break;
    case KEY_END: e->key.sym = EPX_KBD_KEY_END; break;
    case KEY_DOWN: e->key.sym = EPX_KBD_KEY_DOWN; break;
    case KEY_PAGEDOWN: e->key.sym = EPX_KBD_KEY_PAGEDOWN; break;
    case KEY_INSERT: e->key.sym = EPX_KBD_KEY_INSERT; break;
    case KEY_DELETE: e->key.sym = EPX_KBD_KEY_DELETE; break;
      // modifier keys

    case KEY_LEFTSHIFT:  modify(be,value,EPX_KBD_MOD_LSHIFT); return 0;
    case KEY_RIGHTSHIFT: modify(be,value,EPX_KBD_MOD_RSHIFT);  return 0;
    case KEY_LEFTALT:    modify(be,value,EPX_KBD_MOD_LALT);  return 0;
    case KEY_RIGHTALT:   modify(be,value,EPX_KBD_MOD_RALT);  return 0;
    case KEY_LEFTCTRL:   modify(be,value,EPX_KBD_MOD_LCTRL); return 0;
    case KEY_RIGHTCTRL:  modify(be,value,EPX_KBD_MOD_RCTRL); return 0;
    case KEY_LEFTMETA:   modify(be,value,EPX_KBD_MOD_LMETA); return 0;
    case KEY_RIGHTMETA:  modify(be,value,EPX_KBD_MOD_RMETA); return 0;
      // KEY_ALTGR: ? modify(be,value,EPX_KBD_MOD_ALTGR);  return 0;
    case KEY_CAPSLOCK:   toggle(be,value,EPX_KBD_MOD_CAPS); return 0;
    case KEY_NUMLOCK:    toggle(be,value,EPX_KBD_MOD_NUM); return 0;
    case KEY_SCREENLOCK: toggle(be,value,EPX_KBD_MOD_SCR); return 0;
    default: goto mouse_button;
    }
    e->key.mod  = be->keyboard_mods;
    e->key.code = code;

key_value:
    if (value == 0) 
	e->type = EPX_EVENT_KEY_RELEASE;
    else 
	e->type = EPX_EVENT_KEY_PRESS;
    return 0;

mouse_button:
    e->pointer.button = 0;
    switch(code) {
    case BTN_TOUCH: // simulate as left button 
    case BTN_LEFT: 
	e->pointer.button = EPX_BUTTON_LEFT;
	if (value == 0)  {
	    e->type = EPX_EVENT_BUTTON_RELEASE;
	    be->mouse_buttons &= ~EPX_BUTTON_LEFT;
	} else {
	    e->type = EPX_EVENT_BUTTON_PRESS;
	    be->mouse_buttons |= EPX_BUTTON_LEFT;
	}
	break;

    case BTN_MIDDLE: 
	e->pointer.button = EPX_BUTTON_MIDDLE;
	if (value == 0)  {
	    e->type = EPX_EVENT_BUTTON_RELEASE;
	    be->mouse_buttons &= ~EPX_BUTTON_MIDDLE;
	} else {
	    e->type = EPX_EVENT_BUTTON_PRESS;
	    be->mouse_buttons |= EPX_BUTTON_MIDDLE;
	}

	break;

    case BTN_RIGHT: 
	e->pointer.button = EPX_BUTTON_RIGHT;
	if (value == 0)  {
	    e->type = EPX_EVENT_BUTTON_RELEASE;
	    be->mouse_buttons &= ~EPX_BUTTON_RIGHT;
	} else {
	    e->type = EPX_EVENT_BUTTON_PRESS;
	    be->mouse_buttons |= EPX_BUTTON_RIGHT;
	}
	break;

    default:
	return 0;
    }
    return 0;
}


static int process_input_event_relative(struct timeval ts, 
					__u16 type, __u16 code, __s32 value, 
					FbBackend* be, 
					epx_event_t* e)
{
    (void) be;
    DEBUGF("EV_REL type[%d] code[%d] val[%d] ", type, code, value);
    (void) ts;

    switch(code) {
    case REL_X:
        be->mouse_ts = ts;
        be->mouse_x += value;
	if (value) be->mouse_moved = 1;
	if (be->mouse_x < 0) be->mouse_x = 0;
	else if (be->mouse_x >= be->b.width)
	  be->mouse_x = be->b.width-1;
	e->pointer.x = be->mouse_x + be->hotspot_x;
	break;

    case REL_Y:
        be->mouse_ts = ts;
        be->mouse_y += value;
	if (value) be->mouse_moved = 1;
	if (be->mouse_y < 0) be->mouse_y = 0;
	else if (be->mouse_y >= be->b.height)
	  be->mouse_y = be->b.height-1;
	e->pointer.y = be->mouse_y + be->hotspot_y;
	break;

    case REL_Z:
        be->mouse_ts = ts;
        be->mouse_z += value;
	if (value) be->mouse_moved = 1;
	if (be->mouse_z < 0) be->mouse_z = 0;
	e->pointer.z = be->mouse_z;
	break;

    default:
	return 0;
    }
    if (!e->type) e->type = EPX_EVENT_POINTER_MOTION;
    return 0;
}

int map_abs_value(int valid, struct input_absinfo* aptr, int vres, int value)
{
  __s32 size;
  if (valid) {
    __s32 vmin = aptr->minimum;
    __s32 vmax = aptr->maximum;
    if ((size = vmax - vmin) == 0) {
      if ((size = vres) == 0)
	size = 1;
    }
    if (value < vmin) value = vmin;
    if (value > vmax) value = vmax;
    return ((value - vmin) * vres) / size;
  }
  return value;
}

static int process_input_event_absolute(struct timeval ts, 
					__u16 type, __u16 code, __s32 value, 
					FbBackend* be, 
					epx_event_t* e)
{
    (void) ts;
    (void) be;
    DEBUGF("EV_ABS type[%d] code[%d] val[%d]", type, code, value);

    switch(code) {
    case ABS_X:
        e->pointer.x = map_abs_value(be->ax_valid,&be->ax,
				     be->vinfo.xres, value);
	break;

    case ABS_Y:
        e->pointer.y = map_abs_value(be->ay_valid,&be->ay,
				     be->vinfo.yres, value);
	break;

    case ABS_Z:
        e->pointer.z = map_abs_value(be->az_valid,&be->az,
				     256, value); // res?
	break;

    case ABS_PRESSURE:
        e->pointer.z = map_abs_value(be->ap_valid,&be->ap,
				     256, value);  // res?
	break;
    default:
	return 0;
    }
    if (!e->type) e->type = EPX_EVENT_POINTER_MOTION;
    return 0;
}

static int process_input_event_syn(struct timeval ts,
				   __u16 type,
				   __u16 code, 
				   __s32 value, 
				   FbBackend* be, 
				   epx_event_t* e)
{
    (void) ts;
    (void) be;
    DEBUGF("EV_SYN type[%d] code[%d] val[%d]", type, code, value);
    if (e->type) {
        if (be->mouse_moved) {
	  hide_cursor(be);
	  show_cursor(be,CURSOR_TIMEOUT);
	  be->mouse_moved = 1;
	}
	if ((e->type & e->window->mask) == 0) {
	    e->type = 0;  // needed ?
	    return 0;
	}
	return 1;
    }
    return 0;
}


#endif /* HAVE_INPUT_EVENT */
