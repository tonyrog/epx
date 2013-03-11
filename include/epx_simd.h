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
// SIMD api
//
#ifndef __EPX_SIMD_H__
#define __EPX_SIMD_H__

#include "epx_pixel.h"

#define EPX_SIMD_AUTO     0x00
#define EPX_SIMD_MMX      0x01
#define EPX_SIMD_SSE2     0x02
#define EPX_SIMD_ALTIVEC  0x04
#define EPX_SIMD_NEON     0x08
#define EPX_SIMD_EMU      0x80
#define EPX_SIMD_NONE     0xFF


typedef void (*epx_simd_fn_t)(uint8_t* src,int src_wb,
			      uint8_t* dst,int dst_wb,
			      unsigned int width, 
			      unsigned int height);

typedef void (*epx_simd_alpha_fn_t)(uint8_t* src,int src_wb,
				    uint8_t* dst,int dst_wb,
				    uint8_t af,
				    unsigned int width, 
				    unsigned int height);

typedef void (*epx_simd_fill_fn_t)(uint8_t* dst,int dst_wb,
				   epx_pixel_t color,
				   unsigned int width, 
				   unsigned int height);

typedef void (*epx_simd_alpha_color_fn_t)(uint8_t* src,int src_wb,
					  uint8_t* dst,int dst_wb,
					  uint8_t af, epx_pixel_t color,
					  unsigned int width, 
					  unsigned int height);

typedef void (*epx_simd_copy_fn_t)(uint8_t* src, uint8_t* dst, size_t n);
typedef void (*epx_simd_fill_32_fn_t)(uint8_t* src, uint32_t v, size_t n);

typedef struct _epx_simd {
    int type; // EXP_SIMD_xxx
    epx_simd_copy_fn_t           copy;
    epx_simd_fill_32_fn_t        fill_32;
    epx_simd_alpha_color_fn_t    add_blend_area_rgba32;
    epx_simd_alpha_color_fn_t    add_blend_area_argb32;
    epx_simd_alpha_color_fn_t    add_blend_area_a8_rgba32;
    epx_simd_alpha_color_fn_t    add_blend_area_a8_argb32;
    epx_simd_alpha_fn_t          alpha_area_argb32;
    epx_simd_alpha_fn_t          alpha_area_rgba32;
    epx_simd_fn_t                blend_area_rgba32;
    epx_simd_fn_t                blend_area_argb32;
    epx_simd_alpha_fn_t          fade_area_rgba32;
    epx_simd_alpha_fn_t          fade_area_argb32;
    epx_simd_fill_fn_t           fill_area_blend_rgb24;
    epx_simd_fill_fn_t           fill_area_blend_argb32;
    epx_simd_fill_fn_t           fill_area_blend_rgba32;
} epx_simd_t;

extern epx_simd_t* epx_simd;

#define SIMD_CALL(name) (epx_simd->name)
#define SIMD_ENABLED()  (epx_simd != NULL)

extern void epx_simd_init(int accel);
extern int epx_simd_accel(void);
extern int epx_cpu_cache_line_size(void);
extern int epx_cpu_vendor_name(char* buf, size_t maxlen);
extern int epx_cpu_serial_number(unsigned char* buf, size_t maxlen);
extern int epx_cpu_features(char* buf, size_t maxlen);

#endif

