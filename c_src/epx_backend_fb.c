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

#include "epx_backend.h"

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
#include <errno.h>
#include <unistd.h>
#include <string.h>
#ifdef HAVE_MTTR
#include <asm/mtrr.h>
#endif
#include <strings.h>

typedef u_int8_t  u8;
typedef u_int16_t u16;
typedef u_int32_t u32;

// HACK
#undef EPX_DBGFMT
#define EPX_DBGFMT(...)							\
    do {								\
	epx_emit_error(__FILE__, __LINE__, __VA_ARGS__);		\
    } while(0)

#include "via_ioctl.h"   /* SPECIAL VIA ONLY */
#include "via_tv.h"   /* SPECIAL VIA ONLY */
#include "via_share.h"   /* SPECIAL VIA ONLY */

#define viafb_max(x,y) (((x)>(y))?(x):(y))
#define viafb_min(x,y) (((x)<(y))?(x):(y))

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
    int                   via_support;    /* this is a via fb ? */
    struct viafb_driver_version via_vsn;  /* if so this is version */
    int                   via_primary;
    int                   via_secondary;
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
static int viafb_adjust(int fd, epx_dict_t *param);

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

// General string->value map for different keywords.
static struct viafb_param_map_t {
    char *param;
    char *key; // Sadly enough, nested [] arrays are not allowed. Compiler will opt constants?
    unsigned int val;
    unsigned int val2;
} viafb_param_map[] = {
    { "active_device", "crt+dvi",  CRT_Device, DVI_Device }, // Various primay/secondary modes
    { "active_device", "dvi+crt",  DVI_Device, CRT_Device },
    { "active_device", "crt+tv",   CRT_Device, TV_Device },
    { "active_device", "tv+crt",   TV_Device, CRT_Device },
    { "active_device", "crt+lcd",  CRT_Device, LCD_Device }, 
    { "active_device", "lcd+crt",  LCD_Device, CRT_Device },
    { "active_device", "dvi+lcd",  DVI_Device, LCD_Device },
    { "active_device", "lcd+dvi",  LCD_Device, DVI_Device },
    { "active_device", "dvi+tv",   DVI_Device, TV_Device },
    { "active_device", "tv+dvi",   TV_Device,  DVI_Device },
    { "active_device", "lcd+tv",   LCD_Device, TV_Device },
    { "active_device", "tv+lcd",   TV_Device,  LCD_Device }, 
    { "active_device", "lcd+lcd2", LCD_Device, LCD2_Device },
    { "active_device", "lcd2+lcd", LCD2_Device, LCD_Device },
    { "active_device", "crt",      CRT_Device, 0 },
    { "active_device", "dvi",      DVI_Device, 0 },
    { "active_device", "lcd",      LCD_Device, 0 },
    { "active_device", "tv",       TV_Device, 0 },

    { "tv_system", "ntsc", TVTYPE_NTSC, 0 },  // TV signal type.
    { "tv_system", "pal", TVTYPE_PAL, 0 },
    { "tv_system", "480p", TVTYPE_480P, 0 },
    { "tv_system", "576p", TVTYPE_576P, 0 },
    { "tv_system", "720p", TVTYPE_720P, 0 },
    { "tv_system", "1080i", TVTYPE_1080I, 0 },

    { "tv_output_signal", "composite", TV_OUTPUT_COMPOSITE, 0 },  // Output connector and signal
    { "tv_output_signal", "svideo", TV_OUTPUT_SVIDEO, 0 },
    { "tv_output_signal", "rgb", TV_OUTPUT_RGB, 0 },
    { "tv_output_signal", "ypbpr", TV_OUTPUT_YPBPR, 0 },
    { "tv_output_signal", "composite_svideo", TV_OUTPUT_COMPOSITE_SVIDEO, 0 },
    { "tv_output_signal", "rgb_composite", TV_OUTPUT_RGB_COMPOSITE, 0 },
    { "tv_output_signal", "ypbpr_composite", TV_OUTPUT_YPBPR_COMPOSITE, 0 },
    { "tv_output_signal", "rgb_composite_svideo", TV_OUTPUT_RGB_COMPOSITE_SVIDEO, 0 },
    { "tv_output_signal", "ypbpr_composite_svideo", TV_OUTPUT_YPBPR_COMPOSITE_SVIDEO, 0 },

    { "tv_scan", "normal_scan", TV_SIZE_NORMAL_SCAN, 0 },  // Rudimentary scaling of tv image.
    { "tv_scan", "fit_scan", TV_SIZE_FIT_SCAN, 0 },
    { "tv_scan", "over_scan", TV_SIZE_OVER_SCAN, 0 },

    { "tv_dedotcrawl", "on", STATE_ON, 0 },   // Setup dot crawl fixing. http://en.wikipedia.org/wiki/Dot_crawl
    { "tv_dedotcrawl", "true", STATE_ON, 0 },
    { "tv_dedotcrawl", "1", STATE_ON, 0 },
    { "tv_dedotcrawl", "yes", STATE_ON, 0 },
    { "tv_dedotcrawl", "off", STATE_OFF, 0 },
    { "tv_dedotcrawl", "false", STATE_OFF, 0 },
    { "tv_dedotcrawl", "0", STATE_OFF, 0 },
    { "tv_dedotcrawl", "no", STATE_OFF, 0 },

    { "samm", "on", STATE_ON, 0 }, // Enable or disable Single Adapter Multiple Monitor
    { "samm", "true", STATE_ON, 0 },
    { "samm", "1", STATE_ON, 0 },
    { "samm", "yes", STATE_ON, 0 },
    { "samm", "off", STATE_OFF, 0 },
    { "samm", "false", STATE_OFF, 0 },
    { "samm", "0", STATE_OFF, 0 },
    { "samm", "no", STATE_OFF, 0 },

    { "tv_set_ffilter", "on", STATE_ON, 0 }, // Enable or disable flicker filter
    { "tv_set_ffilter", "true", STATE_ON, 0 },
    { "tv_set_ffilter", "1", STATE_ON, 0 },
    { "tv_set_ffilter", "yes", STATE_ON, 0 },
    { "tv_set_ffilter", "off", STATE_OFF, 0 },
    { "tv_set_ffilter", "false", STATE_OFF, 0 },
    { "tv_set_ffilter", "0", STATE_OFF, 0 },
    { "tv_set_ffilter", "no", STATE_OFF, 0 },

    { "tv_set_adaptive_ffilter", "on", STATE_ON, 0 }, // Enable or disable flicker filter
    { "tv_set_adaptive_ffilter", "true", STATE_ON, 0 },
    { "tv_set_adaptive_ffilter", "1", STATE_ON, 0 },
    { "tv_set_adaptive_ffilter", "yes", STATE_ON, 0 },
    { "tv_set_adaptive_ffilter", "off", STATE_OFF, 0 },
    { "tv_set_adaptive_ffilter", "false", STATE_OFF, 0 },
    { "tv_set_adaptive_ffilter", "0", STATE_OFF, 0 },
    { "tv_set_adaptive_ffilter", "no", STATE_OFF, 0 },

    { "lcd_scaling", "center", LCD_CENTERING, 0 }, // Center an image smaller than LCD screen native res.
    { "lcd_scaling", "expand", LCD_EXPANDSION, 0 }, // Expand an image smaller than LCD scren native res.

    { "lcd_mode", "spwg", LCD_SPWG, 0 },    // LCD display standard
    { "lcd_mode", "open_di", LCD_OPENLDI, 0 },// Another LCD display standard.
};

//
//
//
struct viafb_param_map_t *viafb_parse_parameter(char *param, char *key)
{
    int i = sizeof(viafb_param_map) / sizeof(viafb_param_map[0]);

    while(i--)
	if (!strcasecmp(viafb_param_map[i].param, param) &&
	    !strcasecmp(viafb_param_map[i].key, key))
	    return  &viafb_param_map[i];

    printf("parameter not found [%s:%s]\n", param, key);
    return NULL;
}



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
    be->via_support = 0;
    be->via_primary   = None_Device;
    be->via_secondary = None_Device;
    be->mtrr_fd = -1;

    if (epx_dict_lookup_string(param, "framebuffer_device", &string_param, NULL) == -1) {
	EPX_DBGFMT("mssing framebuffer_device paramter. Defaulting to /dev/fb%d", 
		0);
	string_param = "/dev/fb0";
    }
    if ((be->fb_fd = open(string_param, O_RDWR)) == -1) {
	EPX_DBGFMT("Could not open frame buffer [%s]: [%s]", string_param, strerror(errno));
	goto error;
    }

    /* probe for VIA driver */
    be->via_support = (ioctl(be->fb_fd,VIAFB_GET_DRIVER_VERSION,
			     &be->via_vsn) >= 0);

    if (ioctl(be->fb_fd, FBIOGET_VSCREENINFO, &be->ovinfo) == -1) {
	EPX_DBGFMT("ioctl:FBIOGET_VSCREENINFO failed: [%s]", strerror(errno));
	goto error;
    }

    // Unichrome bug workaround.
    if (!be->ovinfo.red.offset && 
	!be->ovinfo.green.offset && 
	!be->ovinfo.blue.offset) {
	EPX_DBGFMT("Unichrome bug workaround. Device driver reported null offsets for color channels, Will fill in BGRA");
	be->ovinfo.red.offset = 16;
	be->ovinfo.red.length = 8;
	be->ovinfo.red.msb_right = 0;

	be->ovinfo.green.offset = 8;
	be->ovinfo.green.length = 8;
	be->ovinfo.blue.msb_right = 0;

	be->ovinfo.blue.offset = 0;
	be->ovinfo.blue.length = 8;
	be->ovinfo.blue.msb_right = 0;
    }
	
    fb_dump_vinfo("Retrieved values.", &be->ovinfo);
    

    r = write(2, cursoroff_str, strlen(cursoroff_str));
    if (r < 0)
	r = write(2, blankoff_str, strlen(blankoff_str));
    if (r < 0) {
	EPX_DBGFMT("write failed: [%s]", strerror(errno));
    }

    be->vinfo  = be->ovinfo;
    be->vinfo.bits_per_pixel = EPX_PIXEL_SIZE(EPX_FORMAT_ARGB)*8; // Default
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

    /* Setup via framebuffer device if supported */
    if (be->via_support) {
	viafb_adjust(be->fb_fd, param);
    }
    return (epx_backend_t*) &(be->b);
error:
    free(be);
    return NULL;
}


static void viafb_dump(char *hdr, struct viafb_ioctl_setting *setting)
{
    puts(hdr);
    printf("viafb: device_flag:         %s\n", setting->device_flag?"true":"false");
    printf("viafb: device_status:       CRT[%c] LCD[%c] TV[%c] DVI[%c] CRT2[%c] HDTV[%c] LCD2[%c]\n",
	   (setting->device_status & CRT_Device)?'X':' ',
	   (setting->device_status & LCD_Device)?'X':' ',
	   (setting->device_status & TV_Device)?'X':' ',
	   (setting->device_status & DVI_Device)?'X':' ',
	   (setting->device_status & CRT2_Device)?'X':' ',
	   (setting->device_status & HDTV_Device)?'X':' ',
	   (setting->device_status & LCD2_Device)?'X':' ');


    printf("viafb: tv_operation_flag:   out_signal[%c] tv_system[%c] tv_level[%c] dedotcrawl[%c]\n", 
	   (setting->tv_operation_flag & OP_TV_OUT_SIGNAL)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_SYSTEM)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_LEVEL)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_DEDOTCRAWL)?'X':' ');

    printf("viafb: tv_operation_flag:   brightness[%c] contrast[%c] saturation[%c] tint[%c]\n", 
	   (setting->tv_operation_flag & OP_TV_BRIGHTNESS)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_CONTRAST)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_SATURATION)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_TINT)?'X':' ');
    
    // FFILTER == FLICKER FILTER!
    printf("viafb: tv_operation_flag:   pos[%c] setting_ffilter[%c] tuning_ffilter[%c]\n", 
	   (setting->tv_operation_flag & OP_TV_POSITION)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_SETTING_FFILTER)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_TUNING_FFILTER)?'X':' ');

    printf("viafb: tv_operation_flag:   setting_adaptive_ffilter[%c] tuning_adaptive_filter[%c]\n",
	   (setting->tv_operation_flag & OP_TV_SETTING_ADAPTIVE_FFILTER)?'X':' ',
	   (setting->tv_operation_flag & OP_TV_TUNING_ADAPTIVE_FFILTER)?'X':' ');

    printf("viafb: lcd_operation_flag:   centering[%c] panel_id[%c] mode[%c]\n",
	   (setting->lcd_operation_flag & OP_LCD_CENTERING)?'X':' ',
	   (setting->lcd_operation_flag & OP_LCD_PANEL_ID)?'X':' ',
	   (setting->lcd_operation_flag & OP_LCD_MODE)?'X':' ');

    printf("viafb: samm_status:         SAMM[%c]\n",
	   (setting->samm_status & OP_SAMM)?'X':' ');

    printf("viafb: first_dev_hor_res:   [%d]\n", setting->first_dev_hor_res);
    printf("viafb: first_dev_ver_res:   [%d]\n", setting->first_dev_ver_res);
    printf("viafb: second_dev_hor_res:  [%d]\n", setting->second_dev_hor_res);
    printf("viafb: second_dev_ver_res:  [%d]\n", setting->second_dev_ver_res);

    printf("viafb: first_dev_refresh:   [%d]\n", setting->first_dev_refresh);
    printf("viafb: first_dev_bpp:       [%d]\n", setting->first_dev_bpp);
    printf("viafb: second_dev_refresh:  [%d]\n", setting->second_dev_refresh);
    printf("viafb: second_dev_bpp:      [%d]\n", setting->second_dev_bpp);

    printf("viafb: primary_device:      CRT[%c] LCD[%c] TV[%c] DVI[%c] CRT2[%c] HDTV[%c] LCD2[%c]\n",
	   (setting->primary_device & CRT_Device)?'X':' ',
	   (setting->primary_device & LCD_Device)?'X':' ',
	   (setting->primary_device & TV_Device)?'X':' ',
	   (setting->primary_device & DVI_Device)?'X':' ',
	   (setting->primary_device & CRT2_Device)?'X':' ',
	   (setting->primary_device & HDTV_Device)?'X':' ',
	   (setting->primary_device & LCD2_Device)?'X':' ');

    printf("viafb: video_device_status: [%X]\n", setting->video_device_status);
    
    
    printf("viafb: tv_attr: system:                 NTSC[%c] PAL[%c] 480P[%c] 576P[%c] 720P[%c] 1080I[%c]\n",
	   (setting->tv_attributes.system & TVTYPE_NTSC)?'X':' ',
	   (setting->tv_attributes.system & TVTYPE_PAL)?'X':' ',
	   (setting->tv_attributes.system & TVTYPE_480P)?'X':' ',
	   (setting->tv_attributes.system & TVTYPE_576P)?'X':' ',
	   (setting->tv_attributes.system & TVTYPE_720P)?'X':' ',
	   (setting->tv_attributes.system & TVTYPE_1080I)?'X':' '
	   );

    printf("viafb: tv_attr: level:                  [%s%s%s]\n", 
	   (setting->tv_attributes.level == TV_SIZE_NORMAL_SCAN)?"normal scan":"",
	   (setting->tv_attributes.level == TV_SIZE_FIT_SCAN)?"fit scan":"",
	   (setting->tv_attributes.level == TV_SIZE_OVER_SCAN)?"over scan":""
	   );

    printf("viafb: tv_attr: out_signal:             composite[%c] svideo[%c] rgb[%c]\n", 
	   (setting->tv_attributes.out_signal & TV_OUTPUT_COMPOSITE)?'X':' ',
	   (setting->tv_attributes.out_signal & TV_OUTPUT_SVIDEO)?'X':' ',
	   (setting->tv_attributes.out_signal & TV_OUTPUT_RGB)?'X':' '
	   );
    printf("viafb: tv_attr: out_signal:             ypbpr[%c] composite_svideo[%c] rgb_composite[%c]\n", 
	   (setting->tv_attributes.out_signal & TV_OUTPUT_YPBPR)?'X':' ',
	   (setting->tv_attributes.out_signal & TV_OUTPUT_COMPOSITE_SVIDEO)?'X':' ',
	   (setting->tv_attributes.out_signal & TV_OUTPUT_RGB_COMPOSITE)?'X':' '
	   );

    printf("viafb: tv_attr: out_signal:             ypbpr_composite[%c] rgb_composite_svideo[%c] ypbpr_composite_svideo[%c]\n", 
	   (setting->tv_attributes.out_signal & TV_OUTPUT_YPBPR_COMPOSITE)?'X':' ',
	   (setting->tv_attributes.out_signal & TV_OUTPUT_RGB_COMPOSITE_SVIDEO)?'X':' ',
	   (setting->tv_attributes.out_signal & TV_OUTPUT_YPBPR_COMPOSITE_SVIDEO)?'X':' '
	   );

    printf("viafb: tv_attr: dedotcrawl:             [%s%s]\n",
	   (setting->tv_attributes.dedotcrawl == STATE_ON)?"on":"",
	   (setting->tv_attributes.dedotcrawl == STATE_OFF)?"off":"");

    
    printf("viafb: tv_attr: ffilter:                [%X]\n", setting->tv_attributes.ffilter);
    printf("viafb: tv_attr: ffilter_state:          [%X]\n", setting->tv_attributes.ffilter_state);
    printf("viafb: tv_attr: adaptive_ffilter:       [%X]\n", setting->tv_attributes.adaptive_ffilter);
    printf("viafb: tv_attr: adaptive_ffilter_state: [%X]\n", setting->tv_attributes.adaptive_ffilter_state);
    printf("viafb: tv_attr: brightness:             [%u]\n", setting->tv_attributes.brightness);
    printf("viafb: tv_attr: contrast:               [%u]\n", setting->tv_attributes.contrast);
    printf("viafb: tv_attr: saturation:             [%u]\n", setting->tv_attributes.saturation);
    printf("viafb: tv_attr: tint:                   [%u] \n", setting->tv_attributes.tint);
    printf("viafb: tv_attr: horizontal_pos:         [%u] \n", setting->tv_attributes.horizontal_pos);
    printf("viafb: tv_attr: vertical_pos:           [%u]\n", setting->tv_attributes.vertical_pos);
    printf("viafb: tv_attr: CurrentScalH:           [%u]\n", setting->tv_attributes.CurrentScalH);
    printf("viafb: tv_attr: CurrentScalV:           [%u]\n", setting->tv_attributes.CurrentScalV);
    printf("viafb: tv_attr: ScalHLevel:             [%u]\n", setting->tv_attributes.ScalHLevel);
    printf("viafb: tv_attr: ScalVLevel:             [%u]\n", setting->tv_attributes.ScalVLevel);
    printf("viafb: tv_attr: DefaultScalH:           [%u]\n", setting->tv_attributes.DefaultScalH);
    printf("viafb: tv_attr: DefaultScalV:           [%u]\n", setting->tv_attributes.DefaultScalV);
    printf("viafb: tv_attr: PositionHLevel:         [%u]\n", setting->tv_attributes.PositionHLevel);
    printf("viafb: tv_attr: PositionVLevel:         [%u]\n", setting->tv_attributes.PositionVLevel);
    printf("viafb: tv_attr: DefaultPositionH:       [%u]\n", setting->tv_attributes.DefaultPositionH);
    printf("viafb: tv_attr: DefaultPositionV:       [%u]\n", setting->tv_attributes.DefaultPositionV);

    printf("viafb: lcd_attr: panel_id:       [%X]\n", setting->lcd_attributes.panel_id);
    printf("viafb: lcd_attr: display_center: [%u]\n", setting->lcd_attributes.display_center);
    printf("viafb: lcd_attr: lcd_mode:       [%X]\n", setting->lcd_attributes.lcd_mode);
    puts("-----\n");
    
}

static int viafb_adjust(int fd, epx_dict_t *param) 
{
    struct viafb_ioctl_setting via_info;
    char* string_param;
    int int_param;
    int  int_param2;
    struct viafb_param_map_t *p_entry;


    // Get initial settings which we will modify
    if (ioctl(fd, VIAFB_GET_DEVICE_INFO, &via_info) == -1) {
	printf("ioctl:VIAFB_GET_DEVICE_INFO failed: [%s]\n", strerror(errno));
	return -1;
    }
    viafb_dump("Before", &via_info);
    
    // Active device
    if (epx_dict_lookup_string(param, "active_device", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("active_device", string_param))) {
	via_info.device_status = p_entry->val | p_entry->val2;
	via_info.primary_device = p_entry->val;
    }

    // Clear the tv and lcd flag.
    via_info.tv_operation_flag = 0x00;
    via_info.lcd_operation_flag = 0x00;

    // SAMM mode
    //  dedotcrawl (http://en.wikipedia.org/wiki/Dot_crawl)
    if (epx_dict_lookup_string(param, "samm", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("samm", string_param))) {
	via_info.samm_status = p_entry->val;
    }

    //
    // LCD  config.
    //
    if (epx_dict_lookup_string(param, "lcd_scaling", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("lcd_scaling", string_param))) {
	via_info.lcd_operation_flag |= OP_LCD_CENTERING; 
	via_info.lcd_attributes.display_center = p_entry->val;
    }

    if (epx_dict_lookup_string(param, "lcd_mode", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("lcd_mode", string_param))) {
	via_info.lcd_operation_flag |= OP_LCD_MODE; 
	via_info.lcd_attributes.display_center = p_entry->val;
    }

    if (epx_dict_lookup_integer(param, "lcd_panel_id", &int_param) != -1) {
	via_info.lcd_operation_flag |= OP_TV_TINT; 
	via_info.lcd_attributes.panel_id = viafb_max(LCD_PANEL_ID_MAXIMUM, int_param);
    }

    // 
    // TV config
    //
    if (epx_dict_lookup_string(param, "tv_system", &string_param,NULL) != -1 &&
	(p_entry = viafb_parse_parameter("tv_system", string_param))) {
	via_info.tv_operation_flag |= OP_TV_SYSTEM; 
	via_info.tv_attributes.system = p_entry->val;
    }

    if (epx_dict_lookup_string(param, "tv_output_signal", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("tv_output_signal", string_param))) {
	via_info.tv_operation_flag |= OP_TV_OUT_SIGNAL; 
	via_info.tv_attributes.out_signal = p_entry->val;
    }

    if (epx_dict_lookup_string(param, "tv_scan", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("tv_scan", string_param))) {
	via_info.tv_operation_flag |= OP_TV_LEVEL; 
	via_info.tv_attributes.level = p_entry->val;
    }

    if (epx_dict_lookup_string(param, "tv_dedotcrawl", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("tv_dedotcrawl", string_param))) {
	via_info.tv_operation_flag |= OP_TV_DEDOTCRAWL; 
	via_info.tv_attributes.dedotcrawl = p_entry->val;
    }

    if (epx_dict_lookup_integer(param, "tv_brightness", &int_param) != -1) {
	via_info.tv_operation_flag |= OP_TV_BRIGHTNESS; 
	via_info.tv_attributes.brightness = viafb_max(TV_BRIGHTNESS_MAXIMUM, int_param);
    }

    if (epx_dict_lookup_integer(param, "tv_contrast", &int_param) != -1) {
	via_info.tv_operation_flag |= OP_TV_CONTRAST; 
	via_info.tv_attributes.contrast = viafb_max(TV_CONTRAST_MAXIMUM, int_param);
    }

    if (epx_dict_lookup_integer(param, "tv_saturation", &int_param) != -1) {
	via_info.tv_operation_flag |= OP_TV_SATURATION; 
	via_info.tv_attributes.saturation = viafb_max(TV_SATURATION_MAXIMUM, int_param);
    }

    if (epx_dict_lookup_integer(param, "tv_tint", &int_param) != -1) {
	via_info.tv_operation_flag |= OP_TV_TINT; 
	via_info.tv_attributes.tint = viafb_max(TV_TINT_MAXIMUM, int_param);
    }

    if (epx_dict_lookup_integer(param, "tv_tint", &int_param) != -1) {
	via_info.tv_operation_flag |= OP_TV_TINT; 
	via_info.tv_attributes.tint = viafb_max(TV_TINT_MAXIMUM, int_param);
    }


    if (epx_dict_lookup_string(param, "tv_set_ffilter", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("tv_set_ffilter", string_param))) {
	via_info.tv_operation_flag |= OP_TV_SETTING_FFILTER; 
	via_info.tv_attributes.ffilter_state = p_entry->val;
    }


    if (epx_dict_lookup_integer(param, "tv_tune_ffilter", &int_param) != -1) {
	via_info.tv_operation_flag |= OP_TV_TUNING_FFILTER; 
	via_info.tv_attributes.ffilter = int_param;
    }

    if (epx_dict_lookup_string(param, "tv_set_adaptive_ffilter", &string_param, NULL) != -1 &&
	(p_entry = viafb_parse_parameter("tv_set_adaptive_ffilter", string_param))) {
	via_info.tv_operation_flag |= OP_TV_SETTING_ADAPTIVE_FFILTER; 
	via_info.tv_attributes.adaptive_ffilter_state = p_entry->val;
    }


    if (epx_dict_lookup_integer(param, "tv_tune_adaptive_ffilter", &int_param) != -1) {
	via_info.tv_operation_flag |= OP_TV_TUNING_ADAPTIVE_FFILTER; 
	via_info.tv_attributes.adaptive_ffilter = int_param;
    }



    // Primary and secondary height/width
    if (epx_dict_lookup_integer(param, "height", &int_param) != -1) via_info.first_dev_ver_res = int_param;
    if (epx_dict_lookup_integer(param, "width", &int_param) != -1) via_info.first_dev_hor_res = int_param;
    if (epx_dict_lookup_integer(param, "height2", &int_param) != -1) via_info.second_dev_ver_res = int_param;
    if (epx_dict_lookup_integer(param, "width2", &int_param) != -1) via_info.second_dev_hor_res = int_param;

    // Refresh rate (in hz (60, 75, 80))
    if (epx_dict_lookup_integer(param, "refresh", &int_param) != -1) via_info.first_dev_refresh = int_param;
    if (epx_dict_lookup_integer(param, "refresh2", &int_param) != -1) via_info.second_dev_refresh = int_param;

    // either pixel_format (string) or pixel_type (old, int)
    if (epx_dict_lookup_string(param,"pixel_format",&string_param,NULL) != -1) {
	epx_format_t fmt = epx_pixel_format_from_name(string_param);
	if (fmt != EPX_FORMAT_INVALID)
	    via_info.first_dev_bpp = EPX_PIXEL_SIZE(fmt)*8;
    }
    else {
	if (epx_dict_lookup_integer(param,  "pixel_type", &int_param) != -1) {
	    via_info.first_dev_bpp = EPX_PIXEL_SIZE(int_param)*8;
	}
    }

    // either pixel_format (string) or pixel_type (old, int)
    if (epx_dict_lookup_string(param,"pixel_format2",&string_param,NULL)!=-1) {
	epx_format_t fmt = epx_pixel_format_from_name(string_param);
	if (fmt != EPX_FORMAT_INVALID)
	    via_info.second_dev_bpp = EPX_PIXEL_SIZE(fmt)*8;
    }
    else {
	if (epx_dict_lookup_integer(param,  "pixel_type2", &int_param) != -1) {
	    via_info.second_dev_bpp = EPX_PIXEL_SIZE(int_param)*8;
	}
    }

    //
    // Tv position time. 
    // These parameters seem not be used by VIAFB_SET_DEVICE_INFO, so we use specific
    // ioctls for them.
    //

    //
    // Set tv size
    //
    if (epx_dict_lookup_integer(param, "tv_position_x", &int_param) != -1 &&
	epx_dict_lookup_integer(param, "tv_position_y", &int_param2) != -1) {
	POSITIONVALUE pos;
    	pos.dwX = int_param;
    	pos.dwY = int_param2;

	if (ioctl(fd, VIAFB_SET_TV_POSITION, &pos) == -1)  {
	    printf("ioctl:VIAFB_SET_TV_POSITION failed: [%s]\n", strerror(errno));
	    return -1;
	}
	puts("Done setting position\n");
    }

    //
    // Set tv size
    //
    if (epx_dict_lookup_integer(param, "tv_size_x", &int_param) != -1 &&
	epx_dict_lookup_integer(param, "tv_size_y", &int_param2) != -1) {
	POSITIONVALUE pos;
    	pos.dwX = int_param;
    	pos.dwY = int_param2;

	printf("Setting size dwX[%d] dwY[%d]\n", pos.dwX, pos.dwY);
	if (ioctl(fd, VIAFB_SET_TV_SIZE, &pos) == -1)  {
	    printf("ioctl:VIAFB_SET_TV_POSITION failed: [%s]\n", strerror(errno));
	    return -1;
	}
    }


    //
    // Set new mode
    //
    via_info.device_flag = 1;
    viafb_dump("After", &via_info);
    if (ioctl(fd, VIAFB_SET_DEVICE_INFO, &via_info) == -1) {
	printf("ioctl:VIAFB_SET_TV_POSITION failed: [%s]'n", strerror(errno));
	return -1;
    }
    return 0;

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

    // Unichrome bug workaround.
    if (!be->vinfo.red.offset && 
	!be->vinfo.green.offset && 
	!be->vinfo.blue.offset) {
	EPX_DBGFMT("Unichrome bug workaround. Device driver reported null offsets for color channels, Will fill in BGRA");
	be->vinfo.red.offset = 16;
	be->vinfo.red.length = 8;
	be->vinfo.red.msb_right = 0;

	be->vinfo.green.offset = 8;
	be->vinfo.green.length = 8;
	be->vinfo.blue.msb_right = 0;

	be->vinfo.blue.offset = 0;
	be->vinfo.blue.length = 8;
	be->vinfo.blue.msb_right = 0;

	if (be->finfo.line_length / be->vinfo.xres_virtual == 4) {
	    be->vinfo.transp.length = 8;
	    be->vinfo.transp.offset = 24;
	}
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


    // Do additioanal (and possibly overlapping) via fb adjustments.
    if (be->via_support) {
	viafb_adjust(be->fb_fd, param);
    }


    return 1;
}



/* return the backend event handle */
static EPX_HANDLE_T fb_evt_attach(epx_backend_t* backend)
{
    (void) backend;
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
    
	sentry.base = be->finfo.smem_start;
	sentry.size = 0x2000000;
	sentry.type = MTRR_TYPE_WRCOMB;
      
	if ( ioctl(be->mtrr_fd, MTRRIOC_ADD_ENTRY, &sentry) == -1 ) {
	    EPX_DBGFMT("MTRRIOC_ADD_ENTRY: [%s] Disabled", strerror(errno));
	    close(be->mtrr_fd);
	    be->mtrr_fd = -1;
	}
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

    printf("epx_fb: bpp=%d,red(%d,%d),green(%d,%d),blue(%d,%d),alpha(%d,%d)\r\n",
	   be->vinfo.bits_per_pixel,rl,ro,gl,go,bl,bo,al,ao);

    pt = 0;
    // FIXME: Buggy reversed condition ! do not know why
    if (!(bo > go)) pt |= EPX_F_Bgr;         // BGR else RGB 
    if (al > 0)  pt |= EPX_F_Alpha;          // Alpha available
    if ((al>0)&&(ao>ro)) pt |= EPX_F_AFirst; // Alpha first
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
	    printf("epx_fb: pixel format = %s\r\n", pt_name);
	else {
	    printf("epx_fb: pixel format %d = unknown\r\n", pt);
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
    (void) backend;
    (void) e;
    return -1;
}

static int fb_win_adjust(epx_window_t *win, epx_dict_t* param)
{
    (void) win;
    (void) param;
    return 1;
}
