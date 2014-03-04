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
#include <linux/input.h>
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
    int mtrr_fd;                      /* File descriptor for MTRR */
    int dbuf;                         /* Double buffer enabled */
    int cbuf;                         /* Current buffer displayed */
    epx_pixmap_t screen[2];                /* The screen/off-screen as a pixmap */
    char direct_pixmap_draw;   /* Shall we map fb directly into pixmap? */

    unsigned char *org_pixmap_data; /* Place holder for pixmap->data if direct draw */

    char input_mouse_dev[128];
    int mouse_fd;

    /* State for events. Will be changed as things clarify */
    struct {
	uint32_t button; /* EPX_EVENT_BUTTON_*/
	int32_t x;
	int32_t y;
	int32_t z;
    } pointer;

    
} FbBackend;


epx_backend_t* fb_init(epx_dict_t* param);

static int fb_finish(epx_backend_t*);
static int fb_pic_attach(epx_backend_t*, epx_pixmap_t*);
static int fb_pic_detach(epx_backend_t*, epx_pixmap_t*);
static int fb_begin(epx_window_t*);
static int fb_end(epx_window_t*,int off_screen);
static int fb_pic_draw(epx_backend_t*, epx_pixmap_t*, epx_window_t*,
		       int src_x, int src_y, int dst_x, int dst_y,
		       unsigned int width,
		       unsigned int height);
static int fb_win_attach(epx_backend_t*, epx_window_t*);
static int fb_win_detach(epx_backend_t*, epx_window_t*);
static int fb_win_swap(epx_backend_t*, epx_window_t*);
static EPX_HANDLE_T fb_evt_attach(epx_backend_t*);
static int fb_evt_detach(epx_backend_t*);
static int fb_evt_read(epx_backend_t*, epx_event_t*);
static int fb_adjust(epx_backend_t *backend, epx_dict_t* param);
static int fb_win_adjust(epx_window_t *win, epx_dict_t* param);
static int setup_input_system(FbBackend* be, epx_dict_t *param);

static epx_callbacks_t fb_callbacks =
{
    fb_finish,
    fb_pic_attach,
    fb_pic_detach,
    fb_pic_draw,
    fb_win_attach,
    fb_win_detach,
    fb_evt_attach,
    fb_evt_detach,
    fb_evt_read,
    fb_adjust,
    fb_win_swap,
    fb_begin,
    fb_end,
    fb_win_adjust
};

/* Array indexed by struct input_event.type */
typedef 
int (*input_event_callback_t)(struct timeval ts,
			      __u16 type, 
			      __u16 code, 
			      __s32 value, 
			      FbBackend* be, 
			      epx_event_t* ev);


#ifndef EV_MAX
#define EV_MAX 0x1f
#endif

static input_event_callback_t callbacks[EV_MAX];
static int setup_input_event_callbacks(input_event_callback_t*, int);

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

void fb_dump_vinfo(char *header, struct fb_var_screeninfo *vinfo)
{
    (void) vinfo; // For make relaease
    (void) header;// For make relaease
    EPX_DBGFMT("fb: %s", header);
    EPX_DBGFMT("fb: vinfo.xres            %lu", vinfo->xres);
    EPX_DBGFMT("fb: vinfo.yres            %lu", vinfo->yres);
    EPX_DBGFMT("fb: vinfo.xres_virtual    %lu", vinfo->xres_virtual);
    EPX_DBGFMT("fb: vinfo.yres_virtual    %lu", vinfo->yres_virtual);
    EPX_DBGFMT("fb: vinfo.xoffset         %lu", vinfo->xoffset);
    EPX_DBGFMT("fb: vinfo.yoffset         %lu", vinfo->yoffset);
    EPX_DBGFMT("fb: vinfo.bits_per_pixel  %lu", vinfo->bits_per_pixel);
    EPX_DBGFMT("fb: vinfo.grayscale       %lu", vinfo->grayscale);
    EPX_DBGFMT("fb: vinfo.red%s", "");
    EPX_DBGFMT("fb:          .offset      %lu", vinfo->red.offset);
    EPX_DBGFMT("fb:          .length      %lu", vinfo->red.length);
    EPX_DBGFMT("fb:          .msb_right   %lu", vinfo->red.msb_right);
    EPX_DBGFMT("fb: vinfo.green%s", "");
    EPX_DBGFMT("fb:          .offset      %lu", vinfo->green.offset);
    EPX_DBGFMT("fb:          .length      %lu", vinfo->green.length);
    EPX_DBGFMT("fb:          .msb_right   %lu", vinfo->green.msb_right);
    EPX_DBGFMT("fb: vinfo.blue%s", "");
    EPX_DBGFMT("fb:          .offset      %lu", vinfo->blue.offset);
    EPX_DBGFMT("fb:          .length      %lu", vinfo->blue.length);
    EPX_DBGFMT("fb:          .msb_right   %lu", vinfo->blue.msb_right);
    EPX_DBGFMT("fb: vinfo.transp%s", "");
    EPX_DBGFMT("fb:          .offset      %lu", vinfo->transp.offset);
    EPX_DBGFMT("fb:          .length      %lu", vinfo->transp.length);
    EPX_DBGFMT("fb:          .msb_right   %lu", vinfo->transp.msb_right);
    EPX_DBGFMT("fb: vinfo.nonstd          %lu", vinfo->nonstd);
    EPX_DBGFMT("fb: vinfo.activate        %lu", vinfo->activate);
    EPX_DBGFMT("fb: vinfo.height          %lu", vinfo->height);
    EPX_DBGFMT("fb: vinfo.width           %lu", vinfo->width);
    EPX_DBGFMT("fb: vinfo.pixclock        %lu", vinfo->pixclock);
    EPX_DBGFMT("fb: vinfo.left_margin     %lu", vinfo->left_margin);
    EPX_DBGFMT("fb: vinfo.upper_margin    %lu", vinfo->upper_margin);
    EPX_DBGFMT("fb: vinfo.lower_margin    %lu", vinfo->lower_margin);
    EPX_DBGFMT("fb: vinfo.hsync_len       %lu", vinfo->hsync_len);
    EPX_DBGFMT("fb: vinfo.vsync_len       %lu", vinfo->vsync_len);
    EPX_DBGFMT("fb: vinfo.sync            %lu", vinfo->sync);
    EPX_DBGFMT("fb: vinfo.rotate          %lu\n", vinfo->rotate);
}

void fb_dump_finfo(char *header, struct fb_fix_screeninfo *finfo)
{
    (void) finfo; // For make relaease
    (void) header;// For make relaease
    EPX_DBGFMT("fb: %s", header);
    EPX_DBGFMT("fb: finfo.id              %s", finfo->id);
    EPX_DBGFMT("fb: finfo.smem_start      0x%X", finfo->smem_start);
    EPX_DBGFMT("fb: finfo.smem_len        %lu", finfo->smem_len);
    EPX_DBGFMT("fb: finfo.type            0x%X", finfo->type);
    EPX_DBGFMT("fb: finfo.type_aux        %lu", finfo->type_aux);
    EPX_DBGFMT("fb: finfo.visual          %lu", finfo->visual);
    EPX_DBGFMT("fb: finfo.xpanstep        %hu", finfo->xpanstep);
    EPX_DBGFMT("fb: finfo.ypanstep        %hu", finfo->ypanstep);
    EPX_DBGFMT("fb: finfo.ywrapstep       %hu", finfo->ywrapstep);
    EPX_DBGFMT("fb: finfo.line_length     %lu", finfo->line_length);
    EPX_DBGFMT("fb: finfo.mmio_start      %lu", finfo->mmio_start);
    EPX_DBGFMT("fb: finfo.mmio_len        %lu", finfo->mmio_len);
    EPX_DBGFMT("fb: finf.accel            %lu", finfo->accel);
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
	    vinfo->bits_per_pixel = EPX_PIXEL_SIZE(fmt)*8;
    }
    else {
	if (epx_dict_lookup_integer(param,  "pixel_type", &int_param) != -1) {
	    vinfo->bits_per_pixel = EPX_PIXEL_SIZE(int_param)*8;
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
    //
    // Strings to disable blanking and cursor.
    //
    const char cursoroff_str[] = "\033[?25l\033[?1c\033[25m";
    const char blankoff_str[] = "\033[9;0]";
    FbBackend* be;
    char* string_param;
    int   int_param;
    int   r;

    if ((be = (FbBackend*) malloc(sizeof(FbBackend))) == NULL)
	return NULL;

    EPX_OBJECT_INIT((epx_backend_t*)be, EPX_BACKEND_TYPE);
    be->b.on_heap = 1;
    be->b.refc = 1;
    be->b.pending = 0;
    be->b.opengl = 0;
    be->b.cb = &fb_callbacks;
    be->b.pixmap_list = NULL;
    be->b.window_list = NULL;
    be->b.event = EPX_INVALID_HANDLE;
    be->mtrr_fd = -1;
    be->input_mouse_dev[0] = 0;
    be->mouse_fd = -1;

    be->pointer.button = 0;
    be->pointer.x = 0;
    be->pointer.y = 0;
    be->pointer.z = 0;

    setup_input_event_callbacks(callbacks, sizeof(callbacks) / sizeof(callbacks[0]));

    if (!setup_input_system(be, param)) 
	EPX_DBGFMT("Failed to setup input system. Disabled");
    
    if (epx_dict_lookup_string(param, "framebuffer_device", &string_param, NULL) == -1) {
	EPX_DBGFMT("missing framebuffer_device paramter. Defaulting to /dev/fb%d",
		0);
	string_param = "/dev/fb0";
    }

    if ((be->fb_fd = open(string_param, O_RDWR)) == -1) {
	EPX_DBGFMT("Could not open frame buffer [%s]: [%s]", string_param, strerror(errno));
	goto error;
    }

    
    if (ioctl(be->fb_fd, FBIOGET_VSCREENINFO, &be->ovinfo) == -1) {
	EPX_DBGFMT("ioctl:FBIOGET_VSCREENINFO failed: [%s]", strerror(errno));
	goto error;
    }

    fb_dump_vinfo("Retrieved values.", &be->ovinfo);

    r = write(2, cursoroff_str, strlen(cursoroff_str));
    if (r < 0)
	r = write(2, blankoff_str, strlen(blankoff_str));
    if (r < 0) {
	EPX_DBGFMT("write failed: [%s]", strerror(errno));
    }

    be->vinfo  = be->ovinfo;
    be->vinfo.bits_per_pixel = EPX_PIXEL_SIZE(EPX_FORMAT_ARGB) * 8; // Default
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

    return (epx_backend_t*) &(be->b);
error:
    free(be);
    return NULL;
}


static int fb_adjust(epx_backend_t *backend, epx_dict_t* param)
{
    FbBackend* be = (FbBackend *) backend;
    //
    // Retrieve info.
    //
    if (ioctl(be->fb_fd, FBIOGET_VSCREENINFO, &be->vinfo) == -1) {
	EPX_DBGFMT("ioctl:FBIOGET_VSCREENINFO failed: [%s]", strerror(errno));
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
	EPX_DBGFMT("ioctl:FBIOPUT_VSCREENINFO/ACTIVATE) failed [%s]", strerror(errno));
	return 0;
    }

    return 1;
}



/* return the backend event handle */
static EPX_HANDLE_T fb_evt_attach(epx_backend_t* backend)
{
    FbBackend* be = (FbBackend*) backend;
    /* FIXME:
       We must actually return two descriptors, one for kbd and one for mouse.
       This needs to be fixed in epx_nif.c:pbackend_poll() so that 
       it can handle multiple descrptors.
       We also need to add 

         int (*evt_attach_multiple)(epx_backend_t*, int*result, int *res_size)

       -and-

         int (*evt_detach_multiple)(epx_backend_t*)

       to epx_callback_t.
    
       For now we just do mouse events.
    */ 

    if (be->mouse_fd != -1)
	return (EPX_HANDLE_T)((long) be->mouse_fd);

    return EPX_INVALID_HANDLE;
}


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
    close(be->mouse_fd);
    if (be->mtrr_fd != -1)
	close(be->mtrr_fd);
    free(be);
    return 0;
}

static int fb_pic_attach(epx_backend_t* backend, epx_pixmap_t* pixmap)
{
    FbBackend* be = (FbBackend*) backend;

/*    DBG("fb_pic_attach(%p)", pixmap); */
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

static int fb_pic_detach(epx_backend_t* backend, epx_pixmap_t* pixmap)
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

static int fb_pic_draw(epx_backend_t* backend, epx_pixmap_t* pixmap,
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
      epx_pixmap_t* scr;
      if (be->dbuf)
	scr = (be->cbuf==0) ? &be->screen[1] : &be->screen[0];
      else
	scr = &be->screen[0];
      epx_pixmap_copy_area(pixmap, scr, src_x, src_y, dst_x, dst_y,
			   width, height, 0);
    }
    nwin->dcount++;
    return 0;
}

static int fb_win_swap(epx_backend_t* backend, epx_window_t* ewin)
{
  FbBackend* be = (FbBackend*) backend;
  (void) ewin;

  if (be->dbuf) {
    if (be->cbuf==0) {
      be->vinfo.yoffset = be->vinfo.yres;
      be->cbuf = 1;
    }
    else {
      be->vinfo.yoffset = 0;
      be->cbuf = 0;
    }
    return ioctl(be->fb_fd, FBIOPAN_DISPLAY, &be->vinfo);
  }
  return 0;
}

static int fb_win_attach(epx_backend_t* backend, epx_window_t* ewin)
{
    int rl, gl, bl, al;
    int ro, go, bo, ao;
    epx_format_t pt;
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
    be->vinfo.yres = be->vinfo.yres_virtual = ewin->height;
    be->vinfo.xres = be->vinfo.xres_virtual = ewin->width;

    fb_dump_vinfo("Modified values to be set.", &be->vinfo);
    if (ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->vinfo) < 0) {
	EPX_DBGFMT("ioctl:FBIOPUT_VSCREENINFO/ACTIVATE) failed [%s]", strerror(errno));
	return -1;
    }

    /* Can we double buffer? */
    if (be->dbuf) {
	be->vinfo.yres_virtual = be->vinfo.yres*2;
	if (ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->vinfo) < 0) {
	    EPX_DBGFMT("double buffer test failed [%s]", strerror(errno));
	    be->dbuf = 0;
	}
    }

    if (ioctl(be->fb_fd, FBIOGET_FSCREENINFO, &be->finfo) == -1) {
	EPX_DBGFMT("ioctl:FBIOGET_FSCREENINFO failed: [%s]", strerror(errno));
	return -1;
    }

    fb_dump_finfo("Fixed info.", &be->finfo);


    be->screen[0].data = (unsigned char *) mmap (0, be->finfo.smem_len, PROT_READ | PROT_WRITE, MAP_SHARED, be->fb_fd, 0);
    if (!be->screen[0].data) {
	EPX_DBGFMT("mmap of screen memory failed [%s].", strerror(errno));
	return -1;
    }

    /* Setup MTRR */
#ifdef HAVE_MTRR
    if ((be->mtrr_fd = open("/proc/mtrr", O_WRONLY, 0)) == -1)
    {
	if (errno == ENOENT) {
	    EPX_DBGFMT("/proc/mtrr not found: MTRR not enabled %s", "");
	}  else {
	    EPX_DBGFMT("Error opening /proc/mtrr: [%s], Disabled.", strerror(errno));
	}
    }
    else {
	struct mtrr_sentry sentry;

	sentry.base = be->finfo.smem_start & 0xFE000000;
	sentry.size = 0x2000000;
	sentry.type = MTRR_TYPE_WRCOMB;

	if ( ioctl(be->mtrr_fd, MTRRIOC_ADD_ENTRY, &sentry) == -1 ) {
	    EPX_DBGFMT("MTRRIOC_ADD_ENTRY(%p, %d): [%s] Disabled",
		   sentry.base, sentry.size, strerror(errno));
	    close(be->mtrr_fd);
	    be->mtrr_fd = -1;
	}
	else
	    EPX_DBGFMT("MTRR enabled at [%p] size[%d] type[MTRR_TYPE_WRCOMB]", sentry.base, sentry.size);
    }
#endif

    //
    // Pan to the beginning of things.
    // Also used to test out double buffering,.
    //
    if (ioctl(be->fb_fd, FBIOPAN_DISPLAY, &be->vinfo) == -1) {
	EPX_DBGFMT("Initial pan failed [%s]. Giving up on double buffering (if active)", strerror(errno));

	// Reset double buffering.
	if (be->dbuf) {
	    be->dbuf = 0;
	    be->vinfo.yres_virtual = be->vinfo.yres;
	    ioctl(be->fb_fd, FBIOPUT_VSCREENINFO, &be->vinfo);
	}
    }


    /*
     * Setup be->creen pixmap. clip member will be continously modified by fb_pic_draw
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

    rl = be->vinfo.red.length;
    ro = be->vinfo.red.offset;
    gl = be->vinfo.green.length;
    go = be->vinfo.green.offset;
    bl = be->vinfo.blue.length;
    bo = be->vinfo.blue.offset;
    al = be->vinfo.transp.length;
    ao = be->vinfo.transp.offset;

    EPX_DBGFMT("epx_fb: bpp=%d,red(%d,%d),green(%d,%d),blue(%d,%d),alpha(%d,%d)",
	   be->vinfo.bits_per_pixel,rl,ro,gl,go,bl,bo,al,ao);

    pt = 0;
    // FIXME: Buggy reversed condition ! do not know why
    if (!(bo > go)) pt |= EPX_F_Bgr;         // BGR else RGB
    if (al > 0)  pt |= EPX_F_Alpha;          // Alpha available
    if ((al>0)&&(ao<ro)) pt |= EPX_F_AFirst; // Alpha first
    pt |= ((be->vinfo.bits_per_pixel-1) & EPX_M_Size);  // pixel size
    if (be->vinfo.grayscale)
	pt |= (EPX_FMT_GRAY<<12);
    else if ((rl==gl) && (gl==bl)) {
	switch(rl) {
	case 4: pt |= (EPX_FMT_RGB4<<12); break;
	case 5: pt |= (EPX_FMT_RGB5<<12); break;
	case 8: pt |= (EPX_FMT_RGB8<<12); break;
	case 10: pt |= (EPX_FMT_RGB10<<12); break;
	case 12: pt |= (EPX_FMT_RGB12<<12); break;
	case 16: pt |= (EPX_FMT_RGB16<<12); break;
	default: break;
	}
    }
    else if ((rl==3)&&(gl==3)&&(bl==2))
	pt |= (EPX_FMT_RGB332<<12);
    else if ((rl==2)&&(gl==3)&&(bl==2))
	pt |= (EPX_FMT_RGB232<<12);
    else if ((rl==5)&&(gl==6)&&(bl==5))
	pt |= (EPX_FMT_RGB565<<12);
    else if (rl && !gl && !bl)
	pt |= (EPX_FMT_RED<<12);
    else if (!rl && gl && !bl)
	pt |= (EPX_FMT_GREEN<<12);
    else if (!rl && !gl && bl)
	pt |= (EPX_FMT_BLUE<<12);
    else
	pt = EPX_FORMAT_INVALID;

    {
	char* pt_name = epx_pixel_format_to_name(pt);
	if (pt_name != NULL)
	    EPX_DBGFMT("epx_fb: pixel format = %s", pt_name);
	else {
	    EPX_DBGFMT("epx_fb: pixel format %d = unknown", pt);
	    pt = EPX_FORMAT_RGB; // guess
	}
    }

    be->screen[0].pixel_format = pt;
    be->screen[1].pixel_format = pt;

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

static int fb_evt_read(epx_backend_t* backend, epx_event_t* e)
{
    FbBackend* be = (FbBackend*) backend;
    struct input_event buf; /* Read all events sequentially with no buffering*/



    /* If no mouse is open, return 0 */
    if (be->mouse_fd == -1)
	return 0;

    if (read(be->mouse_fd, (char*) &buf, sizeof(buf))== sizeof(buf)) {
	EPX_DBGFMT("Got[%d] code[%.4X] value[%d]", 
		   buf.type, buf.code, buf.value);
	/* If we have no windows attached, we cannot set e->window, 
	   which will trigger core.
	   return 0.
	*/
	if (!be->b.window_list) {
	    EPX_DBGFMT("No window attached. Will return 0");
	    return 0;
	}

	if (buf.type < sizeof(callbacks) / sizeof(callbacks[0]) &&
	    callbacks[buf.type] != 0) {
	    
	    e->window = be->b.window_list;  /* FIXME: Multiple windows?? */
	    return (*callbacks[buf.type])(buf.time, buf.type, buf.code, buf.value, be, e);
	}
	EPX_DBGFMT("Unknown event type[%d] code[%.4X] value[%d]", 
		   buf.type, buf.code, buf.value);
    }
    else
	EPX_DBGFMT("Short read");

	

    return 0;
}

static int fb_win_adjust(epx_window_t *win, epx_dict_t* param)
{
    (void) win;
    (void) param;
    return 1;
}

static int setup_input_system(FbBackend* be, epx_dict_t *param)
{
    char* string_param;
    if (be->mouse_fd != -1) {
	close(be->mouse_fd);
	be->mouse_fd = -1;
    }

    if (epx_dict_lookup_string(param, "input_mouse_device", &string_param, NULL) != -1) {
	strncpy(be->input_mouse_dev, string_param, sizeof(be->input_mouse_dev) - 1);
	be->input_mouse_dev[sizeof(be->input_mouse_dev)-1] = 0;
    } 	

    EPX_DBGFMT("Mouse device[%s]", be->input_mouse_dev);


    if (strlen(be->input_mouse_dev) > 0) {
	EPX_DBGFMT("Opening mouse input event device[%s]", 
		   be->input_mouse_dev);

	if (be->input_mouse_dev[0] && 
	    (be->mouse_fd = open(be->input_mouse_dev, O_RDONLY | O_NONBLOCK)) == -1) {
	    EPX_DBGFMT("Error opening mouse input event device[%s]: [%s]", 
		       be->input_mouse_dev, strerror(errno));
	    return 0;
	}

	EPX_DBGFMT("input_mouse_device[%s] is opened as descripto[%d]",  be->input_mouse_dev, be->mouse_fd);
	return 1;
    
	EPX_DBGFMT("input_mouse_device is empty, will not open");
    }
    return 1;

}

static int process_input_event_key(struct timeval ts, 
				   __u16 type, __u16 code, __s32 value, 
				   FbBackend* be, 
				   epx_event_t* e)
{
    EPX_DBGFMT("Keystroke type[%d] code[%d] val[%d]", type, code, value);
    (void) ts;
    (void) be;
    switch(code) {
    case BTN_LEFT: 
	if (value == 0) 
	    e->type = EPX_EVENT_BUTTON_RELEASE;
	else 
	    e->type = EPX_EVENT_BUTTON_PRESS;
	
	e->pointer.button = EPX_EVENT_BUTTON_LEFT;
	break;

    case BTN_MIDDLE: 
	if (value == 0) 
	    e->type = EPX_EVENT_BUTTON_RELEASE;
	else 
	    e->type = EPX_EVENT_BUTTON_PRESS;
	
	e->pointer.button = EPX_EVENT_BUTTON_MIDDLE;
	break;

    case BTN_RIGHT: 
	if (value == 0) 
	    e->type = EPX_EVENT_BUTTON_RELEASE;
	else 
	    e->type = EPX_EVENT_BUTTON_PRESS;
	
	e->pointer.button = EPX_EVENT_BUTTON_RIGHT;
	break;

    default:
	return 0;
    }

    return 1;
}


static int process_input_event_relative(struct timeval ts, 
					__u16 type, __u16 code, __s32 value, 
					FbBackend* be, 
					epx_event_t* e)
{
    (void) be;
    EPX_DBGFMT("Relative type[%d] code[%d] val[%d] ", type, code, value);
    (void) ts;

    switch(code) {
    case REL_X:
	e->type = EPX_EVENT_POINTER_MOTION;
	e->pointer.x += value;
	break;

    case REL_Y:
	e->type = EPX_EVENT_POINTER_MOTION;
	e->pointer.y += value;
	break;

    case REL_Z:
	e->type = EPX_EVENT_POINTER_MOTION;
	e->pointer.z += value;
	break;
    default:
	return 0;
    }
    return 1;
}




static int process_input_event_absolute(struct timeval ts, 
					__u16 type, __u16 code, __s32 value, 
					FbBackend* be, 
					epx_event_t* e)
{
    (void) ts;
    (void) be;
    (void) e;
    EPX_DBGFMT("Abs type[%d] code[%d] val[%d]", type, code, value);
    return 0;
}



static int setup_input_event_callbacks(input_event_callback_t* cb, int max_sz)
{
    int i = max_sz;
    /* Nil out the array */
    while(i--)
	cb[i] = 0;

    /* Make sure that we will not overflow the array */
    if (EV_SYN >= max_sz || EV_KEY >= max_sz || EV_REL >= max_sz || EV_ABS >= max_sz ||
	EV_MSC >= max_sz || EV_SW >= max_sz || EV_LED >= max_sz || EV_SND >= max_sz ||
	EV_REP >= max_sz || EV_FF >= max_sz || EV_PWR >= max_sz || EV_FF_STATUS >= max_sz)
	return 0;

    /* Setup callbacks for supported events type */
    cb[EV_KEY] = process_input_event_key;
    cb[EV_REL] = process_input_event_relative;
    cb[EV_ABS] = process_input_event_absolute;
    /* TODO: Add more handlers here */
    return 1;
}

