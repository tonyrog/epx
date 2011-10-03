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

#ifndef __TV_H__
#define __TV_H__

//#include <linux/ioport.h>
//#include <asm/io.h>

/* Definition TV System */
#define     TVTYPE_NONE     0x00
#define     TVTYPE_NTSC     0x01
#define     TVTYPE_PAL      0x02
#define     TVTYPE_480P     0x04
#define     TVTYPE_576P     0x08
#define     TVTYPE_720P     0x10
#define     TVTYPE_1080I    0x20

/* Definition TV Level */
#define     TV_SIZE_NORMAL_SCAN                    0
#define     TV_SIZE_FIT_SCAN                       1
#define     TV_SIZE_OVER_SCAN                      2

/* Definition TV Ouput Signal */
#define     TV_OUTPUT_NONE                      0x00
#define     TV_OUTPUT_COMPOSITE                 0x01
#define     TV_OUTPUT_SVIDEO                    0x02
#define     TV_OUTPUT_RGB                       0x04
#define     TV_OUTPUT_YPBPR                     0x08
#define     TV_OUTPUT_COMPOSITE_SVIDEO          0x10
#define     TV_OUTPUT_RGB_COMPOSITE             0x20
#define     TV_OUTPUT_YPBPR_COMPOSITE           0x40
#define     TV_OUTPUT_RGB_COMPOSITE_SVIDEO      0x80
#define     TV_OUTPUT_YPBPR_COMPOSITE_SVIDEO   0x100



/* Define TV Function Index */
#define TV_BASE_FUNC                0
#define TV_RGB_FUNC                 1
#define TV_YPBPR_FUNC               2
#define TV_SDTV_RGB_FUNC            3
#define TV_SDTV_YPBPR_FUNC          4
#define TV_DeDotCrawl_FUNC          5



#define NUM_CRTC_TIMING         12

/* TV Table Relative Definition */
#define     VT1622A_REG_NUM             75
#define     FS454_REG_NUM               82
#define     SAA7108A_REG_NUM            67
#define     CH7019_REG_NUM              24
#define     GFX_IGA1_CRT_NUM            14              /* number of setting CRTC's registers on IGA1 in TV table */
#define     GFX_IGA2_CRT_NUM            16              /* number of setting CRTC's registers on IGA2 in TV table */

/* define TV DeDotCrawl Register */
#define     VT1622_DEDOTCRAWL_REG       0x11
#define     VT1622A_DEDOTCRAWL_REG      0x11
#define     VT1625_DEDOTCRAWL_REG       0x11

/* define TV Version register */
#define     VT1622_VERSION_REG          0x1B
#define     VT1622_VERSION              0x03

#define     VT1622A_VERSION_REG         0x1B
#define     VT1622A_VERSION             0x10

#define     VT1625_VERSION_REG          0x1B
#define     VT1625_VERSION              0x50


/* Define TV DAC */
#define     VT1622_DAC_REG              0x0E
#define     VT1622_DAC_A                BIT3
#define     VT1622_DAC_B                BIT2
#define     VT1622_DAC_C                BIT1
#define     VT1622_DAC_D                BIT0

#define     VT1622A_DAC_REG             0x0E
#define     VT1622A_DAC_A               BIT3
#define     VT1622A_DAC_B               BIT2
#define     VT1622A_DAC_C               BIT1
#define     VT1622A_DAC_D               BIT0

#define     VT1625_DAC_REG              0x0E
#define     VT1625_DAC_A                BIT5
#define     VT1625_DAC_B                BIT4
#define     VT1625_DAC_C                BIT3
#define     VT1625_DAC_D                BIT2
#define     VT1625_DAC_E                BIT1
#define     VT1625_DAC_F                BIT0




/* TV MACRO */
#define     GET_HIGH_BYTE(x)       (unsigned char)((x) >> 8)
#define     GET_LOW_BYTE(x)        (unsigned char)((x) & 0x00FF)

/* We divide shift range into 11 partitions, so 6 is the center position. */
#define TV_POS_DEFAULT_HOR_VALUE    6
#define TV_POS_DEFAULT_VER_VALUE    6

/* Definition TV Flicker Filter Type */
#define TV_FFILTER_TYPE_NORMAL      0
#define TV_FFILTER_TYPE_ADAPTIVE    1


/* TV Common Function */
int  tv_encoder_identify(void);
void tv_disable(void);
void tv_load_timing_init(void);
void tv_load_timing(int TVModeIndex);
void tv_load_timing_modify(void);
void tv_patch(int tv_mode_index);
void tv_enable(void);
void tv_register_write(int, u8);
int tv_register_read(int);
void tv_patch_skew_K800(void);
void tv_patch_skew_cn900(int tv_mode_index);
void tv_patch_skew_cx700(int tv_mode_index);
void tv_patch_skew_p880(int tv_mode_index);
void tv_set_color(void);
void tv_set_brightness(u32 value);
void tv_set_contrast(u32 value);
void tv_set_saturation(u32 value);
void tv_set_tint(u32 value);
void tv_set_ffilter(u32 state, u32 value);
void tv_set_adaptive_ffilter(u32 status, u32 value);
int tv_get_ffilter(int ffilter_type, int* ffilter_state); 
void tv_set_position(u32 HPos, u32 VPos);
void tv_get_position(u32* HPos, u32* VPos);
void tv_set_size(u32 HPos, u32 VPos);
void set_tv_clock_source(void);
void tv_patch_skew_p4m900(int tv_mode_index);
void tv_patch_skew_k8m890(int tv_mode_index);
void tv_patch_skew_p4m890(int tv_mode_index);

#endif /* __TV_H__ */

