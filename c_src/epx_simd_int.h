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
// Internal SIMD api
//
#ifndef __EPX_SIMD_INT_H__
#define __EPX_SIMD_INT_H__

#include <stdint.h>
#include "epx_pixel.h"
#include "epx_pixmap.h"

#if defined(__MMX__) || defined(__SSE2__)

static inline void epx_simd_copy_x86(const uint8_t* src, uint8_t* dst, size_t n)
{
    register unsigned long int dummy;
    __asm__ __volatile__(
        "cld; rep; movsb"				
        :"=&D"(dst), "=&S"(src), "=&c"(dummy)
        :"0" (dst), "1" (src),"2" (n)
        : "memory");
}

static inline void epx_simd_fill_8_x86(uint8_t* dst, uint8_t v, size_t n)
{
    int d0, d1;
    __asm__ __volatile__(
        "cld; rep; stosb"
	: "=&c"(d0), "=&D"(d1)
	: "a" (v), "1" (dst), "0" (n)
        : "memory");
}

static inline void epx_simd_fill_32_x86(uint32_t* dst, uint32_t v, size_t n)
{
    unsigned long d0, d1;
    __asm__ __volatile__(
        "cld; rep; stosl"
	: "=&c"(d0), "=&D"(d1)
	: "a" (v), "1" (dst), "0" (n)
        : "memory");
}

#endif

/*
 * Helper functions for align operations
 */

static inline void epx_fill_row_32(uint32_t* dst, uint32_t v, size_t n)
{
    while(n--)
	*dst++ = v;
}


static inline void epx_alpha_row_argb32(uint8_t* src, uint8_t* dst, 
					uint8_t a, int width)
{
    while(width--) {
	// alpha channel = dst
	dst[1]=epx_blend(a,src[1],dst[1]);
	dst[2]=epx_blend(a,src[2],dst[2]);
	dst[3]=epx_blend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

static inline void epx_alpha_row_rgba32(uint8_t* src, uint8_t* dst, 
					uint8_t a, int width)
{
    while(width--) {
	dst[0]=epx_blend(a,src[0],dst[0]);
	dst[1]=epx_blend(a,src[1],dst[1]);
	dst[2]=epx_blend(a,src[2],dst[2]);
	// alpha channel = dst
	src += 4;
	dst += 4;
    }
}

/* Formats: R8G8B8 / B8G8R8 */
static inline void epx_alpha_row_rgb24(uint8_t* src, uint8_t* dst, 
				       uint8_t a, int width)
{
    while(width--) {
	dst[0]=epx_blend(a,src[0],dst[0]);
	dst[1]=epx_blend(a,src[1],dst[1]);
	dst[2]=epx_blend(a,src[2],dst[2]);
	src += 3;
	dst += 3;
    }
}

/* Formats: argb/abgr */
static inline void epx_fade_row_argb32(uint8_t* src,uint8_t* dst,
				       uint8_t af, int width)
{
    while(width--) {
	uint8_t a = (src[0]*af >> 8);
	dst[0]=epx_blend(a,src[0],dst[0]);
	dst[1]=epx_blend(a,src[1],dst[1]);
	dst[2]=epx_blend(a,src[2],dst[2]);
	dst[3]=epx_blend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

/* fade rgba/bgra */
static inline void epx_fade_row_rgba32(uint8_t* src,uint8_t* dst,
				       uint8_t af, int width)
{
    while(width--) {
	uint8_t a = (src[3]*af >> 8);
	dst[0]=epx_blend(a,src[0],dst[0]);
	dst[1]=epx_blend(a,src[1],dst[1]);
	dst[2]=epx_blend(a,src[2],dst[2]);
	dst[3]=epx_blend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

/* blend argb/abgr */
static inline void epx_blend_row_argb32(uint8_t* src,uint8_t* dst,int width)
{
    while(width--) {
	uint8_t a = src[0];
	dst[0]=epx_blend(a,src[0],dst[0]);
	dst[1]=epx_blend(a,src[1],dst[1]);
	dst[2]=epx_blend(a,src[2],dst[2]);
	dst[3]=epx_blend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

/* blend rgba/bgra */
static inline void epx_blend_row_rgba32(uint8_t* src,uint8_t* dst,int width)
{
    while(width--) {
	uint8_t a = src[3];
	dst[0]=epx_blend(a,src[0],dst[0]);
	dst[1]=epx_blend(a,src[1],dst[1]);
	dst[2]=epx_blend(a,src[2],dst[2]);
	dst[3]=epx_blend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

static inline void epx_sum_row_8(uint8_t* src,uint8_t* dst,int width)
{
    while(width--) {
	dst[0] = epx_add(src[0],dst[0]);
	src++;
	dst++;
    }
}


static inline void epx_fill_row_blend_argb32(uint8_t* dst, int width,
					     uint8_t a,
					     uint8_t r,uint8_t g, uint8_t b)
{
    while(width--) {
	dst[0]=epx_blend(a,a,dst[0]);
	dst[1]=epx_blend(a,r,dst[1]);
	dst[2]=epx_blend(a,g,dst[2]);
	dst[3]=epx_blend(a,b,dst[3]);
	dst += 4;
    }
}

/* blend dst with rgba/bgra color (dst alpha is untouched) 
 * caller MUST swap B/R for bgra format
 */
static inline void epx_fill_row_blend_rgba32(uint8_t* dst, int width,
					     uint8_t a,
					     uint8_t r, uint8_t g, uint8_t b)
{
    while(width--) {
	dst[0]=epx_blend(a,r,dst[0]);
	dst[1]=epx_blend(a,g,dst[1]);
	dst[2]=epx_blend(a,b,dst[2]);
	dst[3]=epx_blend(a,a,dst[3]);
	dst += 4;
    }
}

/* blend dst with RGB/BGR color (dst alpha is untouched) */
static inline void epx_fill_row_blend_rgb24(uint8_t* dst, int width,
					    uint8_t a,
					    uint8_t r, uint8_t g, uint8_t b)
{
    while(width--) {
	dst[0]=epx_blend(a,r,dst[0]);
	dst[1]=epx_blend(a,g,dst[1]);
	dst[2]=epx_blend(a,b,dst[2]);
	dst += 3;
    }
}

/* Add color & blend rgba/bgra (if color is swapped correct) */
static inline void epx_add_blend_row_rgba32(uint8_t* src, uint8_t* dst,
					    uint8_t af, epx_pixel_t color,
					    unsigned int width)
{
    while(width--) {
	uint8_t a,r,g,b;
	r = epx_add(src[0], color.r);
	g = epx_add(src[1], color.g);
	b = epx_add(src[2], color.b);
	a = epx_add(src[3], color.a);
	a = ((a * af) >> 8);
	dst[0]=epx_blend(a,r,dst[0]);
	dst[1]=epx_blend(a,g,dst[1]);
	dst[2]=epx_blend(a,b,dst[2]);
	dst[3]=epx_blend(a,a,dst[3]);
	src += 4;
	dst += 4;
    }
}

/* Add color & blend argb/ABGR (if color is swapped correct) */
static inline void epx_add_blend_row_argb32(uint8_t* src, uint8_t* dst,
					    uint8_t af, epx_pixel_t color,
					    unsigned int width)
{
    while(width--) {
	uint8_t a,r,g,b;
	a = epx_add(src[0], color.a);
	r = epx_add(src[1], color.r);
	g = epx_add(src[2], color.g);
	b = epx_add(src[3], color.b);
	a = ((a * af) >> 8);
	dst[0]=epx_blend(a,a,dst[0]);
	dst[1]=epx_blend(a,r,dst[1]);
	dst[2]=epx_blend(a,g,dst[2]);
	dst[3]=epx_blend(a,b,dst[3]);
	src += 4;
	dst += 4;
    }
}

/* Add color & blend rgba/bgra when source is a8 */
static inline void epx_add_blend_row_a8_rgba32(uint8_t* src, uint8_t* dst,
					       uint8_t af, epx_pixel_t color,
					       unsigned int width)
{
    while(width--) {
	uint8_t a;
	a = epx_add(src[0], color.a);
	a = ((a * af) >> 8);
	dst[0]=epx_blend(a,color.r,dst[0]);
	dst[1]=epx_blend(a,color.g,dst[1]);
	dst[2]=epx_blend(a,color.b,dst[2]);
	dst[3]=epx_blend(a,a,dst[3]);
	src += 1;
	dst += 4;
    }
}



/* Add color & blend argb/ABGR when source is a8 */
static inline void epx_add_blend_row_a8_argb32(uint8_t* src, uint8_t* dst,
					       uint8_t af, epx_pixel_t color,
					       unsigned int width)
{
    while(width--) {
	uint8_t a;
	a = epx_add(src[0], color.a);
	a = ((a * af) >> 8);
	dst[0]=epx_blend(a,a,dst[0]);
	dst[1]=epx_blend(a,color.r,dst[1]);
	dst[2]=epx_blend(a,color.g,dst[2]);
	dst[3]=epx_blend(a,color.b,dst[3]);
	src += 1;
	dst += 4;
    }
}


#endif
