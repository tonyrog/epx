/*
 * Copyright 1998-2007 VIA Technologies, Inc. All Rights Reserved.
 * Copyright 2001-2007 S3 Graphics, Inc. All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sub license,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) OR COPYRIGHT HOLDER(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifndef __IOCTL_H__
#define __IOCTL_H__

#ifndef __user
#define __user
#endif	

/* VIAFB IOCTL definition */
#define VIAFB_GET_INFO_SIZE     0x56494101 /* 'VIA\01' */
#define VIAFB_GET_INFO          0x56494102 /* 'VIA\02' */
#define VIAFB_HOTPLUG           0x56494103 /* 'VIA\03' */
#define VIAFB_SET_HOTPLUG_FLAG  0x56494104 /* 'VIA\04' */
#define VIAFB_GET_RESOLUTION    0x56494105 /* 'VIA\05' */
#define VIAFB_GET_TV_TYPE       0x56494106 /* 'VIA\06' */
#define VIAFB_GET_SAMM_INFO       0x56494107 /* 'VIA\07' */
#define VIAFB_TURN_ON_OUTPUT_DEVICE       0x56494108 /* 'VIA\08' */
#define VIAFB_TURN_OFF_OUTPUT_DEVICE       0x56494109 /* 'VIA\09' */
#define VIAFB_SET_DEVICE        0x5649410A /* Al Zhang Added */
#define VIAFB_GET_DEVICE        0x5649410B /* Al Zhang Added */
#define VIAFB_SET_TV_COLOR_DEFAULT  0x5649410C /* 'VIA\0C' */
#define VIAFB_SET_TV_BRIGHTNESS     0x5649410D /* 'VIA\0D' */
#define VIAFB_SET_TV_CONTRAST       0x5649410E /* 'VIA\0E' */
#define VIAFB_SET_TV_SATURATION     0x5649410F /* 'VIA\0F' */
#define VIAFB_SET_TV_TINT           0x56494110 /* 'VIA\10' */
#define VIAFB_SET_TV_FFILTER        0x56494111 /* 'VIA\11' */
#define VIAFB_GET_DRIVER_VERSION    0x56494112 /* 'VIA\12' */
#define VIAFB_GET_CHIP_INFO   0x56494113 /* 'VIA\13' */
#define VIAFB_SET_DEVICE_INFO               0x56494114 /* 'VIA\14'  S.T.Chen [2005.10.03]*/
#define VIAFB_GET_DEVICE_INFO               0x56494115 /* 'VIA\15'  S.T.Chen [2005.10.03]*/
#define VIAFB_SET_TV_POSITION               0x56494116 /* Justin Added */
#define VIAFB_SET_TV_SIZE                   0x56494117 /* Justin Added */
#define VIAFB_GET_DEVICE_SUPPORT            0x56494118 /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_DEVICE_CONNECT            0x56494119 /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_PANEL_SUPPORT_EXPAND      0x5649411A /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_TV_SUPPORT_SIGNAL         0x5649411B /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_TV_SUPPORT_STANDARD       0x5649411C /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_TV_MAX_SIZE               0x5649411D /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_TV_MAX_POSITION           0x5649411E /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_TV_SUPPORT_TUNING_ITEM    0x5649411F /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_TV_MAX_TUNING_VALUE       0x56494120 /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_TV_SUPPORT_SETTING_ITEM   0x56494121 /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_DRIVER_NAME               0x56494122 /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_DEVICE_SUPPORT_STATE      0x56494123 /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_GAMMA_LUT                 0x56494124 /* S.T.Chen [2005.12.06] */
#define VIAFB_SET_GAMMA_LUT                 0x56494125 /* S.T.Chen [2005.12.06] */
#define VIAFB_GET_GAMMA_SUPPORT_STATE       0x56494126 /* S.T.Chen [2005.12.06] */
#define VIAFB_SET_VIDEO_DEVICE              0x56494127 /* Daniel   [2006.12.11] */
#define VIAFB_GET_VIDEO_DEVICE              0x56494128 /* Daniel   [2006.12.11] */
#define VIAFB_SET_SECOND_MODE               0x56494129 /* Lobo Kong[2006.12.31] */

// S.T.Chen [2005.11.29]: Re-define value to sync with xserver driver. (Bitwise)
#define None_Device 0x00
#define CRT_Device  0x01
#define LCD_Device  0x02
#define TV_Device   0x04
#define DVI_Device  0x08
#define CRT2_Device 0x10
#define HDTV_Device 0x20
#define LCD2_Device 0x40

// S.T.Chen [2005.10.03]: Flags which are used to control operations of TV. (Bitwise)
#define OP_TV_SYSTEM                     0x0001
#define OP_TV_LEVEL                      0x0002
#define OP_TV_OUT_SIGNAL                 0x0004
#define OP_TV_DEDOTCRAWL                 0x0008
#define OP_TV_BRIGHTNESS                 0x0010   
#define OP_TV_CONTRAST                   0x0020   
#define OP_TV_SATURATION                 0x0040  
#define OP_TV_TINT                       0x0080  
#define OP_TV_POSITION                   0x0100
#define OP_TV_SETTING_FFILTER            0x0200   
#define OP_TV_TUNING_FFILTER             0x0400   
#define OP_TV_SETTING_ADAPTIVE_FFILTER   0x0800
#define OP_TV_TUNING_ADAPTIVE_FFILTER    0x1000



// S.T.Chen [2005.10.03]: Flags which are used to control operations of LCD. (Bitwise)
#define OP_LCD_CENTERING   0x01
#define OP_LCD_PANEL_ID    0x02
#define OP_LCD_MODE        0x03

//SAMM operation flag
#define OP_SAMM            0x80
 
// S.T.Chen [2005.12.14]: Define maximum value of tv/lcd items.
// The setting value of tv/lcd must be smaller than corresponding maximum value
#define TV_STANDARD_MAXIMUM           0x20    
#define TV_SIZE_LEVEL_MAXIMUM         0x02          
#define TV_OUT_SIGNAL_MAXIMUM         0x100
#define TV_FFILTER_MAXIMUM            3
#define TV_BRIGHTNESS_MAXIMUM         255
#define TV_CONTRAST_MAXIMUM           255
#define TV_ADAPTIVE_FFILTER_MAXIMUM   255
#define TV_SATURATION_MAXIMUM         65535
#define TV_TINT_MAXIMUM               2047
#define TV_HOR_POSITION_MAXIMUM       15
#define TV_VER_POSITION_MAXIMUM       15
#define LCD_PANEL_ID_MAXIMUM          19

// S.T.Chen [2005.12.05]:
#define STATE_ON            0x1
#define STATE_OFF           0x0
#define STATE_DEFAULT       0xFFFF

#define MAX_ACTIVE_DEV_NUM  2


typedef struct{
  unsigned short crt:1;
  unsigned short dvi:1;
  unsigned short tv:1;
  unsigned short lcd:1;
  unsigned short samm:1;
  unsigned short primary_dev;

  unsigned short lcd_dsp_cent:1;
  unsigned char lcd_panel_id;
  unsigned char lcd_mode:1;

  unsigned char tv_out_sig;
  unsigned short tv_level:2;
  unsigned short tv_system;
  unsigned short tv_dedotcrawl:1;
  unsigned short xres,yres;
  unsigned short xres1,yres1;
  unsigned short refresh;
  unsigned short bpp;
  unsigned short refresh1;
  unsigned short bpp1;
  unsigned short sequence;
  
  unsigned short epia_dvi:1;
  unsigned short lcd_dual_edge:1;
  unsigned short bus_width;
  unsigned short lcd2:1;
} device_t;


struct viafb_ioctl_info {
    u32     viafb_id;               /* for identifying viafb */
#define VIAID       0x56494146      /* Identify myself with 'VIAF' */
    u16     vendor_id;
    u16     device_id;
    u8      version;
    u8      revision;
    u8      reserved[246];          /* for future use */
};

struct viafb_ioctl_mode {
    u32     xres;
    u32     yres;
    u32     refresh;
    u32     bpp;
    u32     xres_sec;
    u32     yres_sec;
    u32     virtual_xres_sec;
    u32     virtual_yres_sec;
    u32     refresh_sec;
    u32     bpp_sec;
};
struct viafb_ioctl_samm {
    u32     samm_status;
    u32     size_prim; 
    u32     size_sec;
    u32     mem_base;
    u32     offset_sec; 
};

struct viafb_driver_version {
    int     iMajorNum;
    int     iKernelNum;
    int     iOSNum;
    int     iMinorNum;
};

// S.T.Chen [2005.10.03]: This structure contains variable attributes of TV.
struct viafb_ioctl_tv_attribute {
    unsigned int system;
    unsigned int level;
    unsigned int out_signal;
    unsigned int dedotcrawl;	
    unsigned int ffilter;
    unsigned int adaptive_ffilter;
    unsigned int brightness;
    unsigned int contrast;
    unsigned int saturation;
    unsigned int tint;
    unsigned int horizontal_pos;
    unsigned int vertical_pos;
    unsigned int CurrentScalH;
    unsigned int CurrentScalV;
    unsigned int ScalHLevel;
    unsigned int ScalVLevel;
    unsigned int DefaultScalH;
    unsigned int DefaultScalV;
    unsigned int PositionHLevel;
    unsigned int PositionVLevel;
    unsigned int DefaultPositionH;
    unsigned int DefaultPositionV;
    unsigned int ffilter_state;     // add by S.T.Chen [2006.2.22]
    unsigned int adaptive_ffilter_state; 
};

// S.T.Chen [2005.10.03]: This structure contains variable attributes of LCD.
struct viafb_ioctl_lcd_attribute {
    unsigned int panel_id;
    unsigned int display_center;
    unsigned int lcd_mode;
};

// S.T.Chen [2005.10.03]: This structure contains all setting information of devices.
struct viafb_ioctl_setting {
    unsigned short device_flag;           // Enable or disable active devices
    unsigned short device_status;         // Indicate which device should be turn on or turn off.
    unsigned int   tv_operation_flag;     // Indicate which TV's attribute can be changed.
    unsigned short lcd_operation_flag;    // Indicate which LCD's attribute can be changed.
    unsigned short samm_status;           // 1: SAMM ON  0: SAMM OFF

    unsigned short first_dev_hor_res;     // horizontal resolution of first device
    unsigned short first_dev_ver_res;     // vertical resolution of first device
    unsigned short second_dev_hor_res;    // horizontal resolution of second device
    unsigned short second_dev_ver_res;    // vertical resolution of second device
    unsigned short first_dev_refresh;     // refresh rate of first device
    unsigned short first_dev_bpp;         // bpp of first device
    unsigned short second_dev_refresh;    // refresh rate of second device
    unsigned short second_dev_bpp;        // bpp of second device

    unsigned int primary_device;          // Indicate which device are primary display device.
    unsigned int video_device_status;     // Indicate which device will show video. only valid in duoview mode
    struct viafb_ioctl_tv_attribute tv_attributes;
    struct viafb_ioctl_lcd_attribute lcd_attributes;
};

typedef struct _POSITIONVALUE
{
    unsigned int	dwX;
    unsigned int	dwY;
}POSITIONVALUE;
#endif /* __IOCTL_H__ */
