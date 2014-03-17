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
// Common SIMD functions
// ONLY to be used by epx_simd_xxx.c as #include "epx_simd_common.c"
//
// Assume all preconditions are met in SIMD.txt
//
static inline uint32_t __attribute__((__always_inline__))
set_uint32(uint8_t x0,uint8_t x1,uint8_t x2,uint8_t x3)
{
    uint32_t r;
    uint8_t* pr = (uint8_t*) &r;
    pr[0]=x0; pr[1]=x1; pr[2]=x2; pr[3]=x3;
    return r;
}

//
//  Add source with color and blend it with dst, using af as a scaling factor
//

#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(add_blend_area_rgba32)
#define SIMD_AREA_PARAMS_DECL      uint8_t af, epx_pixel_t color,
#define SIMD_AREA_LOCAL_DECL       int iaf = af;		     \
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);	     \
    epx_vector_u8_t  c8 = epx_simd_vector_set_pixel(color.r,color.g, \
						    color.b,color.a);
#define SIMD_AREA_UNALIGNED(s,d,w) epx_add_blend_row_rgba32((s),(d),af,color,(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_fade_rgba32(fv,epx_simd_adds_u8(c8,(ts)),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION

//
// Same as add_blend_area_rgba but with alpha channel first
// 

#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(add_blend_area_argb32)
#define SIMD_AREA_PARAMS_DECL      uint8_t af, epx_pixel_t color,
#define SIMD_AREA_LOCAL_DECL       int iaf = af;		     \
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);	     \
    epx_vector_u8_t  c8 = epx_simd_vector_set_pixel(color.a,color.r, \
						    color.g,color.b);
#define SIMD_AREA_UNALIGNED(s,d,w) epx_add_blend_row_argb32((s),(d),af,color,(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_fade_argb32(fv,epx_simd_adds_u8(c8,(ts)),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION



/*****  NOT YET GENERAL ENOUGH !
#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(add_blend_area_a8_rgba32)
#define SIMD_AREA_PARAMS_DECL      uint8_t af, epx_pixel_t color,
#define SIMD_AREA_LOCAL_DECL       int iaf = af;		     \
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);	     \
    epx_vector_u8_t  c8 = epx_simd_vector_set_pixel(color.a,color.r, \
						    color.g,color.b);
#define SIMD_AREA_UNALIGNED(s,d,w) epx_add_blend_row_a8_rgba32((s),(d),af,color,(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_fade_argb32(fv,epx_simd_adds_u8(c8,(ts)),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION
*********/

void SIMD_FUNCTION(add_blend_area_a8_rgba32)
    (uint8_t* src, int src_wb, 
     uint8_t* dst, int dst_wb,
     uint8_t af, epx_pixel_t color,
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    int iaf = af;
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);
    epx_vector_u8_t  c8 = epx_simd_vector_set_pixel(color.r,color.g,
						    color.b,color.a);

    if (doffs != 0)
	walign = epx_min_int((doffs/4), width);
	
    while(height > 0) {
	unsigned int width1 = width;
	uint8_t* src1 = src;
	uint8_t* dst1 = dst;

	if (walign) {
	    epx_add_blend_row_a8_rgba32(src1,dst1,af,color,walign);
	    src1 += walign;
	    dst1 += doffs;
	    width1 -= walign;
	}
	while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
	    epx_vector_u8_t td = epx_simd_vector_load(dst1);
	    epx_vector_u8_t ts = epx_simd_vector_set_subpixel(src1,3);
	    ts = epx_simd_adds_u8(c8, ts);
	    td = epx_simd_fade_rgba32(fv,ts,td);
	    epx_simd_vector_store(dst1, td);
	    src1 += EPX_SIMD_VECTOR_PIXELS_ARGB32;
	    dst1 += EPX_SIMD_VECTOR_SIZE;
	    width1 -= EPX_SIMD_VECTOR_SIZE/4;
	}
	if (width1)
	    epx_add_blend_row_a8_rgba32(src1,dst1,af,color,width1);
	src += src_wb;
	dst += dst_wb;
	height--;
    }
    epx_simd_empty_state();
}


void SIMD_FUNCTION(add_blend_area_a8_argb32)
    (uint8_t* src, int src_wb, 
     uint8_t* dst, int dst_wb,
     uint8_t af, epx_pixel_t color,
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    int iaf = af;
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);
    epx_vector_u8_t  c8 = epx_simd_vector_set_pixel(color.a,color.r,
						    color.g,color.b);

    if (doffs != 0)
	walign = epx_min_int((doffs/4), width);
	
    while(height > 0) {
	unsigned int width1 = width;
	uint8_t* src1 = src;
	uint8_t* dst1 = dst;

	if (walign) {
	    epx_add_blend_row_a8_argb32(src1,dst1,af,color,walign);
	    src1 += walign;
	    dst1 += doffs;
	    width1 -= walign;
	}
	while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
	    epx_vector_u8_t td = epx_simd_vector_load(dst1);
	    epx_vector_u8_t ts = epx_simd_vector_set_subpixel(src1,0);
	    ts = epx_simd_adds_u8(c8, ts);
	    td = epx_simd_fade_argb32(fv,ts,td);
	    epx_simd_vector_store(dst1, td);
	    src1 += EPX_SIMD_VECTOR_PIXELS_ARGB32;
	    dst1 += EPX_SIMD_VECTOR_SIZE;
	    width1 -= EPX_SIMD_VECTOR_SIZE/4;
	}
	if (width1)
	    epx_add_blend_row_a8_argb32(src1,dst1,af,color,width1);
	src += src_wb;
	dst += dst_wb;
	height--;
    }
    epx_simd_empty_state();
}

#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(alpha_area_argb32)
#define SIMD_AREA_PARAMS_DECL      uint8_t a,
#define SIMD_AREA_LOCAL_DECL       epx_vector_i8_t a8 = epx_simd_vector_set_pixel(0,a,a,a);
#define SIMD_AREA_UNALIGNED(s,d,w) epx_alpha_row_argb32((s),(d),a,(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_alpha_32(a8,(ts),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION


#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(alpha_area_rgba32)
#define SIMD_AREA_PARAMS_DECL      uint8_t a,
#define SIMD_AREA_LOCAL_DECL       epx_vector_i8_t a8 = epx_simd_vector_set_pixel(a,a,a,0);
#define SIMD_AREA_UNALIGNED(s,d,w) epx_alpha_row_rgba32((s),(d),a,(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_alpha_32(a8,(ts),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION


#define SIMD_AREA_FUNCTION  SIMD_FUNCTION(blend_area_rgba32)
#define SIMD_AREA_PARAMS_DECL
#define SIMD_AREA_LOCAL_DECL
#define SIMD_AREA_UNALIGNED(s,d,w) epx_blend_row_rgba32((s),(d),(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_blend_rgba32((ts),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION


#define SIMD_AREA_FUNCTION  SIMD_FUNCTION(blend_area_argb32)
#define SIMD_AREA_PARAMS_DECL
#define SIMD_AREA_LOCAL_DECL
#define SIMD_AREA_UNALIGNED(s,d,w) epx_blend_row_argb32((s),(d),(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_blend_argb32((ts),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION


#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(fade_area_rgba32)
#define SIMD_AREA_PARAMS_DECL      uint8_t af,
#define SIMD_AREA_LOCAL_DECL       int iaf = af; epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);
#define SIMD_AREA_UNALIGNED(s,d,w) epx_fade_row_rgba32((s),(d),af,(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_fade_rgba32(fv,(ts),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION


#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(fade_area_argb32)
#define SIMD_AREA_PARAMS_DECL      uint8_t af,
#define SIMD_AREA_LOCAL_DECL       int iaf = af; epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);
#define SIMD_AREA_UNALIGNED(s,d,w) epx_fade_row_argb32((s),(d),af,(w))
#define SIMD_AREA_OPERATION(ts,td) epx_simd_fade_argb32(fv,(ts),(td))
#include "epx_simd_area_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION



#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(fill_area_blend_argb32)
#define SIMD_AREA_PARAMS_DECL      epx_pixel_t p,
#define SIMD_AREA_LOCAL_DECL \
    epx_vector_u8_t s8 = epx_simd_vector_set_pixel(p.a,p.r,p.g,p.b);  \
    epx_vector_u8_t a8 = epx_simd_vector_splat_u8(p.a);
#define SIMD_AREA_UNALIGNED(d,w) \
    epx_fill_row_blend_argb32((d),(w),p.a,p.r,p.g,p.b)
#define SIMD_AREA_OPERATION(td)    epx_simd_alpha_32(a8,s8,(td))
#include "epx_simd_area1_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION

#define SIMD_AREA_FUNCTION         SIMD_FUNCTION(fill_area_blend_rgba32)
#define SIMD_AREA_PARAMS_DECL      epx_pixel_t p,
#define SIMD_AREA_LOCAL_DECL \
    epx_vector_u8_t s8 = epx_simd_vector_set_pixel(p.r,p.g,p.b,p.a);	\
    epx_vector_u8_t a8 = epx_simd_vector_splat_u8(p.a);
#define SIMD_AREA_UNALIGNED(d,w) \
    epx_fill_row_blend_rgba32((d),(w),p.a,p.r,p.g,p.b)
#define SIMD_AREA_OPERATION(td)    epx_simd_alpha_32(a8,s8,(td))
#include "epx_simd_area1_body.i"
#undef SIMD_AREA_FUNCTION
#undef SIMD_AREA_PARAMS_DECL
#undef SIMD_AREA_LOCAL_DECL
#undef SIMD_AREA_UNALIGNED
#undef SIMD_AREA_OPERATION
