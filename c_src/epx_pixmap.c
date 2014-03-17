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
// EPX pixmap functions
//
//

#include <stdlib.h>
#include <memory.h>
#include <math.h>

#include "epx_simd_int.h"
#include "../include/epx_pixmap.h"
#include "../include/epx_colors.h"
#include "../include/epx_simd.h"


#define EPX_INLINE_COPY_SIZE 32
#define EPX_INLINE_FILL_SIZE 16

int epx_clip_both(epx_pixmap_t* src,epx_pixmap_t* dst,
		  int x_src, int y_src, int x_dst, int y_dst,
		  unsigned int width, unsigned int height,
		  epx_rect_t* srp, epx_rect_t* drp);

int epx_clip_dst(epx_pixmap_t* src,epx_pixmap_t* dst,
		 int x_src, int y_src, int x_dst, int y_dst,
		 unsigned int width, unsigned int height,
		 epx_rect_t* srp, epx_rect_t* drp);

// epx_copy - copy memory regions
// this code must be fixed to handle overlap and implement memmove
// semantics. This includes checking if src and dst point is to close
// memory wise to use SIMD (|src-dst| < 16)
//
static inline void epx_copy(uint8_t* src, uint8_t* dst, size_t n) 
{
    memmove(dst, src, n);
/*
    if (n < EPX_INLINE_COPY_SIZE) {
	while(n--) 
	    *dst++ = *src++;
    }
    else {
        SIMD_CALL(copy)(src,dst,n);
    }
*/
}

static inline void epx_fill_32(uint8_t* ptr,
			       uint8_t x0,uint8_t x1,uint8_t x2,uint8_t x3,
			       size_t n)
{
    uint32_t v;
    uint8_t* vp = (uint8_t*) &v;

    vp[0]=x0; vp[1]=x1; vp[2]=x2; vp[3]=x3;

    if (!SIMD_ENABLED() || (n < EPX_INLINE_FILL_SIZE)) {
	if (((((unsigned long)ptr) & 0x3) == 0) && (n > 3)) {
	    uint32_t* ptr32 = (uint32_t*) ptr;
	    while(n--)
		*ptr32++ = v;
	}
	else {
	    while(n--) {
		*ptr++ = x0;
		*ptr++ = x1;
		*ptr++ = x2;
		*ptr++ = x3;
	    }
	}
    }
    else {
	SIMD_CALL(fill_32)(ptr, v, n);
    }
}

static void epx_fill_24(uint8_t* ptr, 
			uint8_t x0,uint8_t x1,uint8_t x2,
			size_t n)
{
    if (((((unsigned long)ptr) & 0x3) == 0) && (n >= 4)) {
	uint32_t* ptr32 = (u_int32_t*) ptr;
	uint32_t v0, v1, v2;
	uint8_t* vp;

	vp = (uint8_t*) &v0;
	vp[0]=x0; vp[1]=x1; vp[2]=x2; vp[3]=x0;	
	vp = (uint8_t*) &v1;
	vp[0]=x1; vp[1]=x2; vp[2]=x0; vp[3]=x1;	
	vp = (uint8_t*) &v2;
	vp[0]=x2; vp[1]=x0; vp[2]=x1; vp[3]=x2;	

	while(n >= 4) {
	    *ptr32++ = v0;
	    *ptr32++ = v1;
	    *ptr32++ = v2;
	    n -= 4;
	}
	ptr = (uint8_t*) ptr32;
    }
    if (n) {
	while(n--) {
	    *ptr++ = x0;
	    *ptr++ = x1;
	    *ptr++ = x2;
	}
    }
}

static void epx_fill_16(uint8_t* ptr, 
			uint8_t x0, uint8_t x1,
			size_t n)
{
    if (n >= 2) {  /* FIXME better treshold value */
	size_t n32 = n / 2;
	epx_fill_32(ptr, x0, x1, x0, x1, n32);
	ptr += n32*2;
	n %= 2;
    }
    if (n) {
	if ((((unsigned long) ptr) & 0x1) == 0) {
	    uint16_t v;
	    uint8_t* vp = (uint8_t*) &v;
	    uint16_t* ptr16 = (uint16_t*) ptr;
	    vp[0]=x0; vp[1]=x1;

	    while(n--)
		*ptr16++ = v;
	}
	else {
	    while(n--) {
		*ptr++ = x0;
		*ptr++ = x1;
	    }
	}
    }
}

static void epx_fill_8(uint8_t* ptr, uint8_t x0, size_t n)
{
    if (n >= 4) { /* FIXME: better treshold value */
	size_t n32 = n/4;
	epx_fill_32(ptr, x0, x0, x0, x0, n32);
	ptr += n32*4;
	n %= 4;
    }
    while(n--)
	*ptr++ = x0;
}

// swap_8 - may be used to swap two lines of data
static void epx_swap_8(uint8_t* ptr1, uint8_t* ptr2, size_t n)
{
    while(n--) {
	uint8_t t = *ptr1;
	*ptr1++ = *ptr2;
	*ptr2++ = t;
    }
}

// not yet
#if 0 
static void epx_move_area(uint8_t* src, int src_dx, int src_dy,
			  epx_format_t src_pt,
			  uint8_t* dst, int dst_dx, int dst_dy,
			  epx_format_t dst_pt,
			  int width, int height)
{
    epx_pixel_unpack_t unpack_src = epx_pixel_unpack_func(src_pt);
    epx_pixel_pack_t   pack_dst   = epx_pixel_pack_func(dst_pt);

    while(height--) {
	uint8_t* src1   = src;
	uint8_t* dst1   = dst;
	unsigned width1 = width;
	while(width1--) {
	    epx_pixel_t p = unpack_src(src1);
	    pack_dst(p, dst1);
	    src1 += src_dx;
	    dst1 += dst_dx;
	}
	src += src_dy;
	dst += dst_dy;
    }
}
#endif


// copy pixel area 
void epx_copy_area(uint8_t* src, int src_wb, epx_format_t src_pt,
		   uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		   unsigned int width, unsigned int height)
{
    if ((dst < src) || (height <= 1)) {
	if (src_pt == dst_pt) {
	    unsigned int src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
	    unsigned int n = src_psz*width;
	    
	    while(height--) {
		epx_copy(src, dst, n);
		src += src_wb;
		dst += dst_wb;
	    }
	}
	else {
	    int src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
	    int dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
	    epx_pixel_unpack_t unpack_src = epx_pixel_unpack_func(src_pt);
	    epx_pixel_pack_t   pack_dst   = epx_pixel_pack_func(dst_pt);

	    while(height--) {
		uint8_t* src1 = src;
		uint8_t* dst1 = dst;
		unsigned width1 = width;
		while(width1--) {
		    epx_pixel_t p = unpack_src(src1);
		    pack_dst(p, dst1);
		    src1 += src_psz;
		    dst1 += dst_psz;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	}
    }
    else {
	src += (src_wb*height);
	dst += (dst_wb*height);

	if (src_pt == dst_pt) {
	    unsigned int n = EPX_PIXEL_BYTE_SIZE(src_pt)*width;
	    
	    while(height--) {
		src -= src_wb;
		dst -= dst_wb;
		epx_copy(src, dst, n);
	    }
	}
	else {
	    int src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
	    int dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
	    epx_pixel_unpack_t unpack_src = epx_pixel_unpack_func(src_pt);
	    epx_pixel_pack_t   pack_dst   = epx_pixel_pack_func(dst_pt);

	    while(height--) {
		uint8_t* src1 = src-src_wb;
		uint8_t* dst1 = dst-dst_wb;
		unsigned int width1 = width;
		src = src1;
		dst = dst1;
		while(width1--) {
		    epx_pixel_t p = unpack_src(src1);
		    pack_dst(p, dst1);
		    src1 += src_psz;
		    dst1 += dst_psz;
		}
	    }
	}
    }
}

/* fill pixel area (FIXME acceleration) */
void epx_fill_area(uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		   epx_pixel_t fill,
		   unsigned int width, unsigned int height)
{
    unsigned int psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
    uint32_t     cv = 0;
    uint8_t*     cvp = (uint8_t*) &cv;
    epx_pixel_pack_t pack = epx_pixel_pack_func(dst_pt);

    pack(fill, cvp);
    switch(psz) {
    case 1: 
	while(height--) {
	    epx_fill_8(dst, cvp[0], width);
	    dst += dst_wb;
	}
	break;
    case 2: 
	while(height--) {
	    epx_fill_16(dst, cvp[0], cvp[1], width);
	    dst += dst_wb;
	}
	break;
    case 3:
	while(height--) {
	    epx_fill_24(dst, cvp[0], cvp[1], cvp[2], width);
	    dst += dst_wb;
	}
	break;
    case 4: 
	while(height--) {
	    epx_fill_32(dst, cvp[0], cvp[1], cvp[2], cvp[3], width);
	    dst += dst_wb;
	}
	break;
    default: 
	break;
    }
}

//
// blend dst with ARGB/ABGR color (dst alpha is untouched) 
// caller MUST swap B/R for ABGR format
//
static void fill_area_blend_generic(uint8_t* dst, int dst_wb,
				    epx_format_t dst_pt, 
				    epx_pixel_t p, 
				    unsigned int width, unsigned int height)
{
    int psz;
    epx_pixel_unpack_t unpack_dst;
    epx_pixel_pack_t   pack_dst;

    psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
    unpack_dst = epx_pixel_unpack_func(dst_pt);
    pack_dst   = epx_pixel_pack_func(dst_pt);

    while(height--) {
	uint8_t* dst1 = dst;
	int width1 = width;
	while(width1--) {
	    epx_pixel_t d = unpack_dst(dst1);
	    d = epx_pixel_blend(p.a, p, d);
	    pack_dst(d, dst1);
	    dst1 += psz;
	}
	dst += dst_wb;
    }
}

static void fill_area_blend_rgb24(uint8_t* dst, int dst_wb,
				  epx_format_t dst_pt, 
				  epx_pixel_t p, 
				  unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED())
	fill_area_blend_generic(dst, dst_wb, dst_pt, p, width, height);
    else 
	SIMD_CALL(fill_area_blend_rgb24)(dst, dst_wb, p, width, height);
}

static void fill_area_blend_bgr24(uint8_t* dst, int dst_wb,
				  epx_format_t dst_pt, 
				  epx_pixel_t p, 
				  unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED())
	fill_area_blend_generic(dst, dst_wb, dst_pt, p, width, height);
    else 
	SIMD_CALL(fill_area_blend_rgb24)(dst, dst_wb, epx_pixel_swap(p),
					 width, height);
}

static void fill_area_blend_argb32(uint8_t* dst, int dst_wb,
				   epx_format_t dst_pt, 
				   epx_pixel_t p, 
				   unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED())
	fill_area_blend_generic(dst, dst_wb, dst_pt, p, width, height);
    else 
	SIMD_CALL(fill_area_blend_argb32)(dst, dst_wb, p, width, height);
}

static void fill_area_blend_abgr32(uint8_t* dst, int dst_wb,
				   epx_format_t dst_pt, 
				   epx_pixel_t p, 
				   unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED())
	fill_area_blend_generic(dst, dst_wb, dst_pt, p, width, height);
    else 
	SIMD_CALL(fill_area_blend_argb32)(dst, dst_wb,epx_pixel_swap(p),
					  width,height);
}

static void fill_area_blend_rgba32(uint8_t* dst, int dst_wb,
				   epx_format_t dst_pt, 
				   epx_pixel_t p, 
				   unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED())
	fill_area_blend_generic(dst, dst_wb, dst_pt, p, width, height);
    else 
	SIMD_CALL(fill_area_blend_rgba32)(dst, dst_wb, p, width, height);
}

static void fill_area_blend_bgra32(uint8_t* dst, int dst_wb,
				   epx_format_t dst_pt, 
				   epx_pixel_t p, 
				   unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED())
	fill_area_blend_generic(dst, dst_wb, dst_pt, p, width, height);
    else
	SIMD_CALL(fill_area_blend_rgba32)(dst, dst_wb, epx_pixel_swap(p),
					  width, height);
}

// fill pixel row with blending
void epx_fill_area_blend(uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			 epx_pixel_t p, 
			 unsigned int width, unsigned int height)
{
    switch(dst_pt) {
    case EPX_FORMAT_R8G8B8:
	fill_area_blend_rgb24(dst,dst_wb,dst_pt,p,width,height);
	break;
    case EPX_FORMAT_B8G8R8:
	fill_area_blend_bgr24(dst,dst_wb,dst_pt,p,width,height);
	break;

    case EPX_FORMAT_A8R8G8B8_BE:
    case EPX_FORMAT_X8R8G8B8_BE:
    case EPX_FORMAT_B8G8R8A8_LE:
    case EPX_FORMAT_B8G8R8X8_LE:
	fill_area_blend_argb32(dst,dst_wb,dst_pt,p,width,height);
	break;

    case EPX_FORMAT_A8B8G8R8_BE:
    case EPX_FORMAT_X8B8G8R8_BE:
    case EPX_FORMAT_R8G8B8A8_LE:
    case EPX_FORMAT_R8G8B8X8_LE:
	fill_area_blend_abgr32(dst,dst_wb,dst_pt,p,width,height);
	break;

    case EPX_FORMAT_R8G8B8A8_BE:
    case EPX_FORMAT_R8G8B8X8_BE:
    case EPX_FORMAT_A8B8G8R8_LE:
    case EPX_FORMAT_X8B8G8R8_LE:
	fill_area_blend_rgba32(dst,dst_wb,dst_pt,p,width,height);
	break;

    case EPX_FORMAT_B8G8R8A8_BE:
    case EPX_FORMAT_B8G8R8X8_BE:
    case EPX_FORMAT_A8R8G8B8_LE:
    case EPX_FORMAT_X8R8G8B8_LE:
	fill_area_blend_bgra32(dst,dst_wb,dst_pt,p,width,height);
	break;
    default:
	goto generic;
    }
    return;
generic:
    fill_area_blend_generic(dst,dst_wb,dst_pt,p,width,height);
}

/*! Experimental */
void epx_shade_area(uint8_t* dst, int dst_wb, epx_format_t dst_pt, 
		    unsigned int width, unsigned int height, 
		    epx_pixel_t Px0, epx_pixel_t Px1,
		    epx_pixel_t Py0, epx_pixel_t Py1)
{
    int psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
    int height1 = height;
    int y = 0;
    epx_pixel_unpack_t unpack_dst = epx_pixel_unpack_func(dst_pt);
    epx_pixel_pack_t   pack_dst   = epx_pixel_pack_func(dst_pt);
    
    while(height1--) {
	uint8_t* dst1 = dst;
	unsigned int width1 = width;
	int x = 0;
	while(width1--) {
	    epx_pixel_t d = unpack_dst(dst1);
	    epx_pixel_t c0;
	    epx_pixel_t c1;
	    epx_pixel_t c2;

	    c0 = epx_pixel_blend((x*255)/width, Px0, Px1);
	    c1 = epx_pixel_blend((y*255)/height, Py0, Py1);
	    c2 = epx_pixel_add(c0,c1);

	    d = epx_pixel_blend(c2.a, c2, d);
	    pack_dst(d, dst1);
	    dst1 += psz;
	    x++;
	}
	dst += dst_wb;
	y++;
    }
}

// FUNCTION: epx_blend_AREA ( ... )
#define AREA_FUNCTION         epx_blend_AREA
#define AREA_PARAMS_DECL
#define AREA_OPERATION(s,d)   epx_pixel_blend(s.a,s,d)
#include "epx_area_body.i"
#undef AREA_FUNCTION
#undef AREA_PARAMS_DECL
#undef AREA_OPERATION
// END-FUNCTION

static void blend_area_generic(uint8_t* src,int src_wb,epx_format_t src_pt,
			       uint8_t* dst,int dst_wb,epx_format_t dst_pt,
			       unsigned int width, 
			       unsigned int height)
{
    if (src_pt == dst_pt) {
	switch(src_pt) {
	case EPX_FORMAT_R8G8B8A8_BE:
	case EPX_FORMAT_B8G8R8A8_BE:
	case EPX_FORMAT_A8B8G8R8_LE:
	case EPX_FORMAT_A8R8G8B8_LE:
	    while(height--) {
		epx_blend_row_rgba32(src, dst, width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	case EPX_FORMAT_A8R8G8B8_BE:
	case EPX_FORMAT_A8B8G8R8_BE:
	case EPX_FORMAT_B8G8R8A8_LE:
	case EPX_FORMAT_R8G8B8A8_LE:
	    while(height--) {
		epx_blend_row_argb32(src, dst, width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;	
	default:
	    break;
	}
    }
    epx_blend_AREA(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
}

static void blend_area_rgba32(uint8_t* src,int src_wb,epx_format_t src_pt,
			      uint8_t* dst,int dst_wb,epx_format_t dst_pt,
			      unsigned int width, 
			      unsigned int height)
{
    if (!SIMD_ENABLED() || (src_pt != dst_pt) || (width < 8))
	blend_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
    else
	SIMD_CALL(blend_area_rgba32)(src,src_wb,dst,dst_wb,width,height);
}


static void blend_area_argb32(uint8_t* src,int src_wb,epx_format_t src_pt,
			      uint8_t* dst,int dst_wb,epx_format_t dst_pt,
			      unsigned int width, 
			      unsigned int height)
{
    if (!SIMD_ENABLED()  || (src_pt != dst_pt) || (width < 8))
	blend_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
    else
	SIMD_CALL(blend_area_argb32)(src,src_wb,dst,dst_wb,width,height);
}

void epx_blend_area(uint8_t* src, int src_wb, epx_format_t src_pt,
		    uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		    unsigned int width,
		    unsigned int height)
{
    if (!EPX_PIXEL_HAS_ALPHA(src_pt)) {
	epx_copy_area(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	return;
    }
    switch(src_pt) {
    case EPX_FORMAT_R8G8B8A8_BE:
    case EPX_FORMAT_B8G8R8A8_BE:
    case EPX_FORMAT_A8B8G8R8_LE:
    case EPX_FORMAT_A8R8G8B8_LE:
	blend_area_rgba32(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	break;
    case EPX_FORMAT_A8R8G8B8_BE:
    case EPX_FORMAT_A8B8G8R8_BE:
    case EPX_FORMAT_B8G8R8A8_LE:
    case EPX_FORMAT_R8G8B8A8_LE:
	blend_area_argb32(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	break;
    default:
	blend_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	break;
    }
}

// FUNCTION: epx_sum_area ( ... )
#define AREA_FUNCTION         epx_sum_area
#define AREA_PARAMS_DECL      
#define AREA_OPERATION(s,d)   epx_pixel_add(s, d)
#include "epx_area_body.i"
#undef AREA_FUNCTION
#undef AREA_PARAMS_DECL
#undef AREA_OPERATION
// END-FUCNTION

static inline epx_pixel_t alpha_pixel(uint8_t a, epx_pixel_t s, epx_pixel_t d)
{
    d.r = epx_blend(a,s.r,d.r);
    d.g = epx_blend(a,s.g,d.g);
    d.b = epx_blend(a,s.b,d.b);
    return d;
}

// FUNCTION: epx_alpha_AREA ( ... )
#define AREA_FUNCTION         epx_alpha_AREA
#define AREA_PARAMS_DECL      uint8_t alpha,
#define AREA_OPERATION(s,d)   alpha_pixel(alpha, s, d)
#include "epx_area_body.i"
#undef AREA_FUNCTION
#undef AREA_PARAMS_DECL
#undef AREA_OPERATION
// END-FUCNTION

static void alpha_area_generic(uint8_t* src, int src_wb, epx_format_t src_pt,
			       uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			       uint8_t alpha, 
			       unsigned int width, unsigned int height)
{
    if (src_pt == dst_pt) {
	switch(src_pt) {
	case EPX_FORMAT_B8G8R8X8_BE:
	case EPX_FORMAT_B8G8R8A8_BE:
	case EPX_FORMAT_R8G8B8X8_BE:
	case EPX_FORMAT_R8G8B8A8_BE:
	case EPX_FORMAT_X8R8G8B8_LE:
	case EPX_FORMAT_A8R8G8B8_LE:
	case EPX_FORMAT_X8B8G8R8_LE:
	case EPX_FORMAT_A8B8G8R8_LE:
	    while(height--) {
		epx_alpha_row_rgba32(src,dst,alpha,width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	case EPX_FORMAT_X8R8G8B8_BE:
	case EPX_FORMAT_A8R8G8B8_BE:
	case EPX_FORMAT_X8B8G8R8_BE:
	case EPX_FORMAT_A8B8G8R8_BE:
	case EPX_FORMAT_B8G8R8X8_LE:
	case EPX_FORMAT_B8G8R8A8_LE:
	case EPX_FORMAT_R8G8B8X8_LE:
	case EPX_FORMAT_R8G8B8A8_LE:
	    while(height--) {
		epx_alpha_row_argb32(src,dst,alpha,width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	case EPX_FORMAT_R8G8B8:
	case EPX_FORMAT_B8G8R8:
	    while(height--) {
		epx_alpha_row_rgb24(src,dst,alpha,width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	default:
	    break;
	}
    }
    epx_alpha_AREA(src,src_wb,src_pt,dst,dst_wb,dst_pt,alpha,width,height);
}

static void alpha_area_rgba32(uint8_t* src, int src_wb, epx_format_t src_pt,
			      uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			      uint8_t a,unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED() || (src_pt != dst_pt) || (width < 8))
	alpha_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,a,width,height);
    else
	SIMD_CALL(alpha_area_rgba32)(src,src_wb,dst,dst_wb,a,width,height);	
}

static void alpha_area_argb32(uint8_t* src, int src_wb, epx_format_t src_pt,
			      uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			      uint8_t a,unsigned int width, unsigned int height)
{
    if (!SIMD_ENABLED() || (src_pt != dst_pt) || (width < 8))
	alpha_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,a,width,height);
    else
	SIMD_CALL(alpha_area_argb32)(src,src_wb,dst,dst_wb,a,width,height);	
}

void epx_alpha_area(uint8_t* src, int src_wb, epx_format_t src_pt,
		    uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		    uint8_t a, unsigned int width, unsigned int height)
{
    switch(src_pt) {
    case EPX_FORMAT_B8G8R8X8_BE:
    case EPX_FORMAT_B8G8R8A8_BE:
    case EPX_FORMAT_R8G8B8X8_BE:
    case EPX_FORMAT_R8G8B8A8_BE:
    case EPX_FORMAT_X8R8G8B8_LE:
    case EPX_FORMAT_A8R8G8B8_LE:
    case EPX_FORMAT_X8B8G8R8_LE:
    case EPX_FORMAT_A8B8G8R8_LE:
	alpha_area_rgba32(src,src_wb,src_pt,dst,dst_wb,dst_pt,a,width,height);
	break;
    case EPX_FORMAT_X8R8G8B8_BE:
    case EPX_FORMAT_A8R8G8B8_BE:
    case EPX_FORMAT_X8B8G8R8_BE:
    case EPX_FORMAT_A8B8G8R8_BE:
    case EPX_FORMAT_B8G8R8X8_LE:
    case EPX_FORMAT_B8G8R8A8_LE:
    case EPX_FORMAT_R8G8B8X8_LE:
    case EPX_FORMAT_R8G8B8A8_LE:
	alpha_area_argb32(src,src_wb,src_pt,dst,dst_wb,dst_pt,a,width,height);
	break;
    default:
	alpha_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,a,width,height);
    }
}


static inline epx_pixel_t fade_pixel(uint8_t fade, epx_pixel_t s, epx_pixel_t d)
{
    uint8_t a = (s.a)*fade >> 8;
    d.r = epx_blend(a,s.r,d.r);
    d.g = epx_blend(a,s.g,d.g);
    d.b = epx_blend(a,s.b,d.b);
    d.a = epx_blend(a,s.a,d.a);
    return d;
}

// FUNCTION: epx_fade_AREA ( ... )
#define AREA_FUNCTION         epx_fade_AREA
#define AREA_PARAMS_DECL      uint8_t fade,
#define AREA_OPERATION(s,d)   fade_pixel(fade, s, d)
#include "epx_area_body.i"
#undef AREA_FUNCTION
#undef AREA_PARAMS_DECL
#undef AREA_OPERATION
// END-FUCNTION


static void fade_area_generic(uint8_t* src, int src_wb, epx_format_t src_pt,
			      uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			      uint8_t fade, 
			      unsigned int width, unsigned int height)
{
    if (fade == ALPHA_FACTOR_0)
	return;
    if (fade == ALPHA_FACTOR_1) {
	epx_blend_area(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	return;
    }
    if (src_pt == dst_pt) {
	switch(src_pt) {
	case EPX_FORMAT_R8G8B8A8_BE:
	case EPX_FORMAT_B8G8R8A8_BE:
	case EPX_FORMAT_A8B8G8R8_LE:
	case EPX_FORMAT_A8R8G8B8_LE:
	    while(height--) {
		epx_fade_row_rgba32(src, dst, fade, width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;

	case EPX_FORMAT_A8R8G8B8_BE:
	case EPX_FORMAT_A8B8G8R8_BE:
	case EPX_FORMAT_B8G8R8A8_LE:
	case EPX_FORMAT_R8G8B8A8_LE:
	    while(height--) {
		epx_fade_row_argb32(src, dst, fade, width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;

	default:
	    break;
	}
    }
    else if (src_pt == EPX_FORMAT_A8) {
	// source r=g=b=0
	switch(dst_pt) {
	case EPX_FORMAT_B8G8R8:
	case EPX_FORMAT_R8G8B8:
	    while(height--) {
		uint8_t* src1 = src;
		uint8_t* dst1 = dst;
		unsigned int width1 = width;
		while(width1--) {
		    uint8_t a = (*src1*fade >> 8);
		    dst1[0]=epx_blend(a,0,dst1[0]);
		    dst1[1]=epx_blend(a,0,dst1[1]);
		    dst1[2]=epx_blend(a,0,dst1[2]);
		    src1++;
		    dst1 += 3;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	    return;

	case EPX_FORMAT_A8R8G8B8_BE:
	case EPX_FORMAT_A8B8G8R8_BE:
	case EPX_FORMAT_B8G8R8A8_LE:
	case EPX_FORMAT_R8G8B8A8_LE:
	    while(height--) {
		uint8_t* src1 = src;
		uint8_t* dst1 = dst;
		unsigned int width1 = width;
		// FIXME: use epx_add_blend_row_a8_argb32 (with color=0!)
		while(width1--) {
		    uint8_t a = ((*src1)*fade >> 8);
		    dst1[0]=epx_blend(a,src1[0],dst1[0]); // NEW
		    dst1[1]=epx_blend(a,0,dst1[1]);
		    dst1[2]=epx_blend(a,0,dst1[2]);
		    dst1[3]=epx_blend(a,0,dst1[3]);
		    src1++;
		    dst1 += 4;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	    return;

	case EPX_FORMAT_R8G8B8A8_BE:
	case EPX_FORMAT_B8G8R8A8_BE:
	case EPX_FORMAT_A8B8G8R8_LE:
	case EPX_FORMAT_A8R8G8B8_LE:
	    while(height--) {
		uint8_t* src1 = src;
		uint8_t* dst1 = dst;
		unsigned int width1 = width;
		while(width1--) {
		    uint8_t a = ((*src1)*fade >> 8);
		    dst1[0]=epx_blend(a,0,dst1[0]);
		    dst1[1]=epx_blend(a,0,dst1[1]);
		    dst1[2]=epx_blend(a,0,dst1[2]);
		    dst1[3]=epx_blend(a,src1[0],dst1[3]);
		    src1++;
		    dst1 += 4;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	    return;

	default: {
	    unsigned int dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
	    epx_pixel_unpack_t unpack_dst = epx_pixel_unpack_func(dst_pt);
	    epx_pixel_pack_t   pack_dst = epx_pixel_pack_func(dst_pt);
	    while(height--) {
		uint8_t* src1 = src;
		uint8_t* dst1 = dst;
		unsigned int width1 = width;
		while(width1--) {
		    epx_pixel_t d = unpack_dst(dst1);
		    uint8_t a = (*src1*fade >> 8);
		    d.r = epx_blend(a,0,d.r);
		    d.g = epx_blend(a,0,d.g);
		    d.b = epx_blend(a,0,d.b);
		    d.a = epx_blend(a,src1[0],d.a);
		    pack_dst(d, dst1);
		    src1++;
		    dst1 += dst_psz;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	}

	}
    }

    epx_fade_AREA(src, src_wb, src_pt, dst, dst_wb, dst_pt,
		  fade, width, height);
}

static void fade_area_rgba32(uint8_t* src, int src_wb, epx_format_t src_pt,
			     uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			     uint8_t fade, 
			     unsigned int width, unsigned int height)
{
    if (fade == ALPHA_FACTOR_0)
	return;
    if (fade == ALPHA_FACTOR_1) {
	epx_blend_area(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	return;
    }
    if (!SIMD_ENABLED() || (src_pt != dst_pt) || (width < 8))
	fade_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,fade,
			  width,height);
    else
	SIMD_CALL(fade_area_rgba32)(src,src_wb,dst,dst_wb,fade,width,height);
}

static void fade_area_argb32(uint8_t* src, int src_wb, epx_format_t src_pt,
			     uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			     uint8_t fade, 
			     unsigned int width, unsigned int height)
{
    if (fade == ALPHA_FACTOR_0)
	return;
    if (fade == ALPHA_FACTOR_1) {
	epx_blend_area(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	return;
    }
    if (!SIMD_ENABLED() || (src_pt != dst_pt) || (width < 8))
	fade_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,fade,
			  width,height);
    else
	SIMD_CALL(fade_area_argb32)(src,src_wb,dst,dst_wb,fade,width,height);
}


void epx_fade_area(uint8_t* src, int src_wb, epx_format_t src_pt,
		   uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		   uint8_t fade, unsigned int width, unsigned int height)
{
    switch(src_pt) {
    case EPX_FORMAT_R8G8B8A8_BE:
    case EPX_FORMAT_B8G8R8A8_BE:
    case EPX_FORMAT_A8B8G8R8_LE:
    case EPX_FORMAT_A8R8G8B8_LE:
	fade_area_rgba32(src,src_wb,src_pt,dst,dst_wb,dst_pt,fade,width,height);
	break;
    case EPX_FORMAT_R8G8B8X8_BE:
    case EPX_FORMAT_B8G8R8X8_BE:
    case EPX_FORMAT_X8B8G8R8_LE:
    case EPX_FORMAT_X8R8G8B8_LE:
	alpha_area_rgba32(src,src_wb,src_pt,dst,dst_wb,dst_pt,fade,
			  width,height);
	break;

    case EPX_FORMAT_A8R8G8B8_BE:
    case EPX_FORMAT_A8B8G8R8_BE:
    case EPX_FORMAT_B8G8R8A8_LE:
    case EPX_FORMAT_R8G8B8A8_LE:
	fade_area_argb32(src,src_wb,src_pt,dst,dst_wb,dst_pt,fade,width,height);
	break;
    case EPX_FORMAT_X8R8G8B8_BE:
    case EPX_FORMAT_X8B8G8R8_BE:
    case EPX_FORMAT_B8G8R8X8_LE:
    case EPX_FORMAT_R8G8B8X8_LE:
	alpha_area_argb32(src,src_wb,src_pt,dst,dst_wb,dst_pt,fade,
			  width,height);
	break;
    default:
	fade_area_generic(src,src_wb,src_pt,dst,dst_wb,dst_pt,fade,
			  width,height);
	break;
    }
}


void epx_shadow_area(uint8_t* src, int src_wb, epx_format_t src_pt,
		     uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		     unsigned int width, unsigned int height, epx_flags_t flags)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    epx_pixel_unpack_t unpack_dst;
    epx_pixel_unpack_t unpack_src;
    epx_pixel_pack_t   pack_dst;

    src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
    dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
    unpack_src = epx_pixel_unpack_func(src_pt);
    unpack_dst = epx_pixel_unpack_func(dst_pt);
    pack_dst   = epx_pixel_pack_func(dst_pt);

    if (!(flags & EPX_FLAG_BLEND)) {
	while(height--) {
	    uint8_t* dst1 = dst;
	    uint8_t* src1 = src;
	    unsigned int width1 = width;
	    
	    while(width1--) {
		epx_pixel_t s = unpack_src(src1);
		epx_pixel_t d = unpack_dst(dst1);
		uint8_t g = epx_pixel_luminance(s);
		d = epx_pixel_shadow(g, d);
		pack_dst(d, dst1);
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	while(height--) {
	    uint8_t* dst1 = dst;
	    uint8_t* src1 = src;
	    unsigned int width1 = width;
	    
	    while(width1--) {
		epx_pixel_t s = unpack_src(src1);
		uint8_t g;
		if (s.a == EPX_ALPHA_OPAQUE) {
		    epx_pixel_t d = unpack_dst(dst1);
		    g = epx_pixel_luminance(s);
		    d = epx_pixel_shadow(g, d);
		    pack_dst(d, dst1);
		}
		else if (s.a == EPX_ALPHA_TRANSPARENT) {
		    ;
		}
		else {
		    epx_pixel_t d = unpack_dst(dst1);
		    d = epx_pixel_blend(s.a, s, d);
		    g = epx_pixel_luminance(d);
		    d = epx_pixel_shadow(g, d);
		    pack_dst(d, dst1);
		}
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	    src += src_wb;
	    dst += dst_wb;
	}
    }
}

 /*
  *  Add color (saturate) to each pixel in src and interpret
  *  the alpha channel in color as fader value (i.e multiplier)
  *
  */
void epx_add_color_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			uint8_t fader, epx_pixel_t color,
			unsigned int width, unsigned int height,
			epx_flags_t flags)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    epx_pixel_unpack_t unpack_src;
    epx_pixel_unpack_t unpack_dst;
    epx_pixel_pack_t   pack_dst;

    if ((fader == ALPHA_FACTOR_0) && (color.px == 0))
	return;

    if ((fader == ALPHA_FACTOR_1) && (color.px == 0) && 
	(src_pt != EPX_FORMAT_A8)) {  // FIXME!!!
	if (flags&EPX_FLAG_BLEND)
	    epx_blend_area(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	else
	    epx_copy_area(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	return;
    }

    if ((flags&EPX_FLAG_BLEND) == 0) // only blend sofar
	goto generic_area;

    if (!SIMD_ENABLED())
	goto generic_area;

    if ((dst_pt == src_pt) && (width>=8)) {
	switch(src_pt) {
	case EPX_FORMAT_R8G8B8A8_BE:
	case EPX_FORMAT_R8G8B8X8_BE:
	case EPX_FORMAT_A8B8G8R8_LE:
	case EPX_FORMAT_X8B8G8R8_LE:
	    SIMD_CALL(add_blend_area_rgba32)(src,src_wb,dst,dst_wb,fader,
					   color,width,height);
	    return;
	case EPX_FORMAT_B8G8R8A8_BE:
	case EPX_FORMAT_B8G8R8X8_BE:
	case EPX_FORMAT_A8R8G8B8_LE:
	case EPX_FORMAT_X8R8G8B8_LE:
	    SIMD_CALL(add_blend_area_rgba32)(src,src_wb,dst,dst_wb,fader,
					   epx_pixel_swap(color),
					   width,height);
	    return;
	case EPX_FORMAT_A8R8G8B8_BE:
	case EPX_FORMAT_X8R8G8B8_BE:
	case EPX_FORMAT_B8G8R8A8_LE:
	case EPX_FORMAT_B8G8R8X8_LE:
	    SIMD_CALL(add_blend_area_argb32)(src,src_wb,dst,dst_wb,fader,
					   color,width,height);
	    return;
	case EPX_FORMAT_A8B8G8R8_BE:
	case EPX_FORMAT_X8B8G8R8_BE:
	case EPX_FORMAT_R8G8B8A8_LE:
	case EPX_FORMAT_R8G8B8X8_LE:
	    SIMD_CALL(add_blend_area_argb32)(src,src_wb,dst,dst_wb,fader,
					   epx_pixel_swap(color),
					   width,height);
	    return;
	default:
	    goto generic_area;
	}
    }
    else if (src_pt == EPX_FORMAT_A8) {
	switch(dst_pt) {
	case EPX_FORMAT_R8G8B8A8_BE:
	case EPX_FORMAT_R8G8B8X8_BE:
	case EPX_FORMAT_A8B8G8R8_LE:
	case EPX_FORMAT_X8B8G8R8_LE:
	    SIMD_CALL(add_blend_area_a8_rgba32)(src,src_wb,dst,dst_wb,fader,
					      color,width,height);
	    return;
	case EPX_FORMAT_B8G8R8A8_BE:
	case EPX_FORMAT_B8G8R8X8_BE:
	case EPX_FORMAT_A8R8G8B8_LE:
	case EPX_FORMAT_X8R8G8B8_LE:
	    SIMD_CALL(add_blend_area_a8_rgba32)(src,src_wb,dst,dst_wb,fader,
						 epx_pixel_swap(color),
						 width,height);
	    return;
	case EPX_FORMAT_A8R8G8B8_BE:
	case EPX_FORMAT_X8R8G8B8_BE:
	case EPX_FORMAT_B8G8R8A8_LE:
	case EPX_FORMAT_B8G8R8X8_LE:
	    SIMD_CALL(add_blend_area_a8_argb32)(src,src_wb,dst,dst_wb,fader,
					      color,width,height);
	    return;
	case EPX_FORMAT_A8B8G8R8_BE:
	case EPX_FORMAT_X8B8G8R8_BE:
	case EPX_FORMAT_R8G8B8A8_LE:
	case EPX_FORMAT_R8G8B8X8_LE:
	    SIMD_CALL(add_blend_area_a8_argb32)(src,src_wb,dst,dst_wb,fader,
					      epx_pixel_swap(color),
					      width,height);
	    return;
	default:
	    goto generic_area;
	}
    }
    
generic_area:
    unpack_src = epx_pixel_unpack_func(src_pt);
    unpack_dst = epx_pixel_unpack_func(dst_pt);
    pack_dst   = epx_pixel_pack_func(dst_pt);

    src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
    dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);

    if ((flags&EPX_FLAG_BLEND) == 0) {
	while(height--) {
	    uint8_t* dst1 = dst;
	    uint8_t* src1 = src;
	    unsigned int width1 = width;
	    
	    while(width1--) {
		epx_pixel_t s = unpack_src(src1);
		s = epx_pixel_add(color,s);
		s.a = ((s.a * fader) >> 8);
		s = epx_pixel_shadow(s.a, s);
		pack_dst(s, dst1);
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	// add color with and blend
       // note! the slight difference in alpha handling
	while(height--) {
	    uint8_t* dst1 = dst;
	    uint8_t* src1 = src;
	    unsigned int width1 = width;
	    
	    while(width1--) {
		epx_pixel_t d = unpack_dst(dst1);
		epx_pixel_t s = unpack_src(src1);
		uint8_t a; 
		s = epx_pixel_add(color,s);
		a = ((s.a * fader) >> 8);
		d = epx_pixel_blend(a, s, d);
		pack_dst(d, dst1);
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	    src += src_wb;
	    dst += dst_wb;
	}
    }
}


#define EPX_AVG_MAX_N 128
/* special filter N_1 average N pixels */
static void filter_avg_N_1_area(uint8_t* src, int src_wb, epx_format_t src_pt,
				uint8_t* dst, int dst_wb, epx_format_t dst_pt,
				unsigned int width, unsigned int height,
				int n,
				epx_flags_t flags)
{
    int src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
    int dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
    epx_pixel_unpack_t unpack_src = epx_pixel_unpack_func(src_pt);
    epx_pixel_unpack_t unpack_dst = epx_pixel_unpack_func(dst_pt);
    epx_pixel_pack_t   pack_dst   = epx_pixel_pack_func(dst_pt);
    int height1 = height;


    while(height1--) {
	uint8_t* src1 = src;
	uint8_t* dst1 = dst;
	uint32_t rs = 0;
	uint32_t gs = 0;
	uint32_t bs = 0;
	epx_pixel_t  rgb[EPX_AVG_MAX_N];
	int i = 0;
	unsigned int width1 = (n-1)/2;
	
	memset(rgb, 0, sizeof(rgb));
	
	/* Do head part loading avg buffer */
	while(width1--) {
	    epx_pixel_t s = unpack_src(src1);
	    rs = (rs + s.r) - rgb[i].r;
	    gs = (gs + s.g) - rgb[i].g;
	    bs = (bs + s.b) - rgb[i].b;
	    rgb[i] = s;
	    i = (i==n-1) ? 0 : (i + 1);
	    src1 += src_psz;
	}
	
	/* Run middle part */
	width1 = width - (n-1)/2;
	while(width1--) {
	    epx_pixel_t s = unpack_src(src1);
	    
	    rs = (rs + s.r) - rgb[i].r;
	    gs = (gs + s.g) - rgb[i].g;
	    bs = (bs + s.b) - rgb[i].b;
	    rgb[i] = s;
	    i = (i==n-1) ? 0 : (i + 1);
	    
	    s.r = rs / n;
	    s.g = gs / n;
	    s.b = bs / n;

	    if (flags & EPX_FLAG_BLEND) {
		epx_pixel_t d = unpack_dst(dst1);
		d = epx_pixel_blend(s.a, s, d);
		pack_dst(d, dst1);
	    }
	    else { 
		pack_dst(s, dst1);
	    }
	    src1 += src_psz;
	    dst1 += dst_psz;
	}
	
	/* Do tail part writing rest of dst */
	width1 = n - ((n-1)/2);
	while(width1--) {
	    epx_pixel_t s;
	    
	    rs = rs - rgb[i].r;
	    gs = gs - rgb[i].g;
	    bs = bs - rgb[i].b;
	    i = (i==n-1) ? 0 : (i + 1);
	    
	    s.r = rs / n;
	    s.g = gs / n;
	    s.b = bs / n;
	    
	    if (flags & EPX_FLAG_BLEND) {
		epx_pixel_t d = unpack_dst(dst1);
		d = epx_pixel_blend(s.a, s, d);
		pack_dst(d, dst1);
	    }
	    else { 
		pack_dst(s, dst1);
	    }
	    dst1 += dst_psz;
	}
	src += src_wb;
	dst += dst_wb;
    }
}
//
// FIXME: make it possible to have src = dst ! 
// FIXME: make this work ;-)
//        the factors should be? signed (fixpoint)
//
static void filter_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			epx_filter_t* filter,
			unsigned int width, unsigned int height,
			epx_flags_t flags)
 {
     int n = filter->wh.width;
     if ((filter->wh.height == 1) && (n <= EPX_AVG_MAX_N) &&
	 (filter->fsum == (unsigned int) n)) { /* NOT Completly true ! */
	 filter_avg_N_1_area(src, src_wb, src_pt, dst, dst_wb, dst_pt,
			     width, height, n, flags);
     }
     else {
	 int src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
	 int dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
	 epx_pixel_unpack_t unpack_src = epx_pixel_unpack_func(src_pt);
	 epx_pixel_unpack_t unpack_dst = epx_pixel_unpack_func(dst_pt);
	 epx_pixel_pack_t   pack_dst   = epx_pixel_pack_func(dst_pt);
	 int height1 = height - (filter->wh.height-1);

	 /* do special treat y=0..m2-1 and  y=height-m2-1..height-1 */

	 /* do y=hh ... height-hh */
	 while(height1--) {
	     uint8_t* src1 = src;
	     uint8_t* dst1 = dst;
	     unsigned int width1 = width - (filter->wh.width-1);

	     src1 += (filter->wh.width >> 1)*src_psz;
	     dst1 += (filter->wh.width >> 1)*dst_psz;
	     /* do x=ww ... width-ww */
	     while(width1--) {
		 int fh = filter->wh.height;
		 epx_pixel_t d;
		 epx_pixel_t s;
		 uint8_t* src2  = src1;
		 uint8_t* fptr  = filter->factor;
		 // uint32_t acc_a = 0;
		 uint32_t acc_r = 0;
		 uint32_t acc_g = 0;
		 uint32_t acc_b = 0;

		 while(fh--) {
		     int fw = filter->wh.width;
		     uint8_t* sptr  = src2;

		     // Should be able to use SIMD here!
		     while(fw--) {
			 uint8_t factor = *fptr++;
			 epx_pixel_t t = unpack_src(sptr);
			 // acc_a += factor*t.a;
			 acc_r += factor*t.r;
			 acc_g += factor*t.g;
			 acc_b += factor*t.b;
			 sptr += src_psz;
		     }
		     src2 += src_wb; // next source row
		 }
		 s.r = acc_r / filter->fsum;
		 s.g = acc_g / filter->fsum;
		 s.b = acc_b / filter->fsum;
		 if (flags & EPX_FLAG_BLEND) {
		     d = unpack_dst(dst1);
		     d = epx_pixel_blend(s.a, s, d);
		     pack_dst(d, dst1);
		 }
		 else { 
		     pack_dst(s, dst1);
		 }
		 src1 += src_psz;
		 dst1 += dst_psz;
	     }
	     src += src_wb;
	     dst += dst_wb;
	 }
     }
 }


 /* interpolate a pixel (used for antialias and scaling etc) 
  *  blend the color at pixels in the surrounding of
  *  float coordinate (x,y) 
 */
 static inline epx_pixel_t epixel_interp(epx_pixmap_t* pic, float x, float y)
 {
     // To get gcc 4.1.2 to shut up about implicit declaration
     // without having to resort to -std=99, which triggers a shitload
     // of compile errors.
     extern float truncf(float);    
     extern float roundf(float);    

     int y0 = truncf(y-0.5);
     int y1 = truncf(y+0.5);
     int x0 = truncf(x-0.5);
     int x1 = truncf(x+0.5);
     float not_used;
     float fy = modff(y-0.5, &not_used);
     float fx = modff(x-0.5, &not_used);
     float f0 = (1-fx)*(1-fy);
     float f1 = fx*(1-fy);
     float f2 = (1-fx)*fy;
     float f3 = fx*fy;
     epx_pixel_t p0,p1,p2,p3;
     uint8_t* ptr;
     epx_pixel_t p;

     if (epx_point_xy_in_rect(x0,y0, &pic->clip)) {
	 ptr = EPX_PIXEL_ADDR(pic, x0, y0);
	 p0 = pic->func.unpack(ptr);
     }
     else
	 p0 = epx_pixel_transparent;

     if (epx_point_xy_in_rect(x1,y0, &pic->clip)) {
	 ptr = EPX_PIXEL_ADDR(pic, x1, y0);
	 p1 = pic->func.unpack(ptr);
     }
     else
	 p1 = epx_pixel_transparent;

     if (epx_point_xy_in_rect(x0,y1, &pic->clip)) {
	 ptr = EPX_PIXEL_ADDR(pic, x0, y1);
	 p2 = pic->func.unpack(ptr);
     }
     else
	 p2 = epx_pixel_transparent;


     if (epx_point_xy_in_rect(x1,y1, &pic->clip)) {
	 ptr = EPX_PIXEL_ADDR(pic, x1, y1);
	 p3 = pic->func.unpack(ptr);
     }
     else
	 p3 = epx_pixel_transparent;


     // This could probably be done in ALTIVEC || SSE2
     p.r = roundf(f0*p0.r + f1*p1.r + f2*p2.r + f3*p3.r);
     p.g = roundf(f0*p0.g + f1*p1.g + f2*p2.g + f3*p3.g);
     p.b = roundf(f0*p0.b + f1*p1.b + f2*p2.b + f3*p3.b);
     p.a = roundf(f0*p0.a + f1*p1.a + f2*p2.a + f3*p3.a);

     return p;
}

// Given source and destination rectangles, calculate the
// source rectangle and the destination rectangles when both
// source and destination are clipped
// both rectangles will have the same dimension on return 
// return -1 if the destination rectangle is empty!
int epx_clip_both(epx_pixmap_t* src,epx_pixmap_t* dst,
		  int x_src, int y_src, int x_dst, int y_dst,
		  unsigned int width, unsigned int height,
		  epx_rect_t* srp, epx_rect_t* drp)
{
    epx_rect_t r;

    // Set source rectangle and clip it
    epx_rect_set(srp, x_src, y_src, width, height);
    if (!epx_rect_intersect(srp, &src->clip, srp))
	return -1;  // empty

    // Set destination rectangle and clip it
    epx_rect_set(&r, x_dst, y_dst, srp->wh.width, srp->wh.height);
    if (!epx_rect_intersect(&r, &dst->clip, drp))
	return -1;  // empty

    // Adjust source rectangle according to destination
    srp->xy.x -= (r.xy.x - drp->xy.x);
    srp->xy.y -= (r.xy.y - drp->xy.y);
    srp->wh = drp->wh;
    return 0;
}


// Given source and destination rectangles, calculate the
// destination rectangles when both destination are clipped
// return -1 if the destination rectangle is empty!
int epx_clip_dst(epx_pixmap_t* src,epx_pixmap_t* dst,
		 int x_src, int y_src, int x_dst, int y_dst,
		 unsigned int width, unsigned int height,
		 epx_rect_t* srp, epx_rect_t* drp)
{
    (void) src;
    epx_rect_t r;

    epx_rect_set(srp, x_src, y_src, width, height);

    // Set destination rectangle and clip it
    epx_rect_set(&r, x_dst, y_dst, width, height);
    if (!epx_rect_intersect(&r, &dst->clip, drp))
	return -1;  // empty

    // Adjust source rectangle according to destination
    srp->xy.x -= (r.xy.x - drp->xy.x);
    srp->xy.y -= (r.xy.y - drp->xy.y);
    srp->wh = drp->wh;
    return 0;
}

static void init_pixel_area_functions(epx_pixmap_functions_t* func, 
				      epx_format_t fmt)
{
    switch(fmt) {
    case EPX_FORMAT_R8G8B8:  // RGB
	func->fill_area_blend = fill_area_blend_rgb24;
	func->blend_area      = epx_copy_area;  // no alpha for blending
	func->alpha_area      = alpha_area_generic;
	func->fade_area       = fade_area_generic;
	break;
    case EPX_FORMAT_B8G8R8:  // BGR
	func->fill_area_blend = fill_area_blend_bgr24;
	func->blend_area      = epx_copy_area;  // no src alpha
	func->alpha_area      = alpha_area_generic;
	func->fade_area       = fade_area_generic;
	break;
    case EPX_FORMAT_A8R8G8B8_BE:  // ARGB
    case EPX_FORMAT_B8G8R8A8_LE:
	func->fill_area_blend = fill_area_blend_argb32;
	func->blend_area      = blend_area_argb32;
	func->alpha_area      = alpha_area_argb32;
	func->fade_area       = fade_area_argb32;
	break;
    case EPX_FORMAT_X8R8G8B8_BE:  // XRGB
    case EPX_FORMAT_B8G8R8X8_LE:
	func->fill_area_blend = fill_area_blend_argb32;
	func->blend_area      = epx_copy_area;     // src alpha = 1
	func->alpha_area      = alpha_area_argb32;
	func->fade_area       = alpha_area_argb32; // src alpha = 1
	break;
    case EPX_FORMAT_A8B8G8R8_BE:  // ABGR
    case EPX_FORMAT_R8G8B8A8_LE:
	func->fill_area_blend = fill_area_blend_abgr32;
	func->blend_area      = blend_area_argb32;
	func->alpha_area      = alpha_area_argb32;
	func->fade_area       = fade_area_argb32;
	break;
    case EPX_FORMAT_X8B8G8R8_BE:  // XBGR
    case EPX_FORMAT_R8G8B8X8_LE:
	func->fill_area_blend = fill_area_blend_abgr32;
	func->blend_area      = epx_copy_area;  // no src alpha
	func->alpha_area      = alpha_area_argb32;  // abgr = argb for alpha
	func->fade_area       = alpha_area_argb32;  // src alpha = 1
	break;
    case EPX_FORMAT_R8G8B8A8_BE:  // RGBA
    case EPX_FORMAT_A8B8G8R8_LE:
	func->fill_area_blend = fill_area_blend_rgba32;
	func->blend_area      = blend_area_rgba32;
	func->alpha_area      = alpha_area_rgba32;
	func->fade_area       = fade_area_rgba32;
	break;
    case EPX_FORMAT_R8G8B8X8_BE:  // RGBX
    case EPX_FORMAT_X8B8G8R8_LE:
	func->fill_area_blend = fill_area_blend_rgba32;
	func->blend_area      = epx_copy_area;  // no src alpha
	func->alpha_area      = alpha_area_rgba32;
	func->fade_area       = alpha_area_rgba32;  // src alpha = 1
	break;
    case EPX_FORMAT_B8G8R8A8_BE:  // BGRA
    case EPX_FORMAT_A8R8G8B8_LE:
	func->fill_area_blend = fill_area_blend_bgra32;
	func->blend_area      = blend_area_rgba32;
	func->alpha_area      = alpha_area_rgba32;
	func->fade_area       = fade_area_rgba32;
	break;

    case EPX_FORMAT_B8G8R8X8_BE:  // BGRX
    case EPX_FORMAT_X8R8G8B8_LE:
	func->fill_area_blend = fill_area_blend_bgra32;
	func->blend_area      = epx_copy_area;  // no src alpha
	func->alpha_area      = alpha_area_rgba32;
	func->fade_area       = alpha_area_rgba32;  // src alpha = 1
	break;

    default:
	func->fill_area_blend = fill_area_blend_generic;
	func->blend_area      = blend_area_generic;
	func->alpha_area      = alpha_area_generic;
	func->fade_area       = fade_area_generic;
	break;
    }
}

int epx_pixmap_init(epx_pixmap_t* dst, unsigned int width, unsigned int height, 
		    epx_format_t fmt)
{
    uint8_t* data0;
    unsigned int bytes_per_pixel = EPX_PIXEL_BYTE_SIZE(fmt);
    unsigned int bytes_per_row   = bytes_per_pixel*width;
    epx_pixel_unpack_t unpack;
    epx_pixel_pack_t pack;

    // initialize here to make sure destructor always will work
    EPX_OBJECT_INIT(dst, EPX_PIXMAP_TYPE);
    dst->data0   = 0;
    dst->data    = 0;
    dst->backend = 0;
    dst->detach  = 0;
    dst->parent  = 0;
    dst->user    = 0;
    // Each row must by a multiple of 16!
    bytes_per_row += EPX_ALIGN_OFFS(bytes_per_row,16);

    unpack = epx_pixel_unpack_func(fmt);
    pack   = epx_pixel_pack_func(fmt);

    if ((unpack == NULL) || (pack == NULL))
	return -1;
    if (!(data0 = (uint8_t*) malloc(bytes_per_row*height+15)))
	return -1;

    epx_rect_set(&dst->clip, 0, 0, width, height);
    dst->width          = width;
    dst->bytes_per_row  = bytes_per_row;
    dst->height         = height;
    dst->bits_per_pixel = bytes_per_pixel*8;
    dst->pixel_format   = fmt;
    dst->func.unpack    = unpack;
    dst->func.pack      = pack;
    init_pixel_area_functions(&dst->func, fmt);
    dst->bytes_per_pixel = bytes_per_pixel;
    /* total number of bytes, not including padding */
    dst->sz             = bytes_per_row*height;
    dst->data0          = data0;
    dst->data           = data0 + EPX_ALIGN_OFFS(data0,16);
    return 0;
}


epx_pixmap_t* epx_pixmap_create(unsigned int width, unsigned int height, 
				epx_format_t fmt)
{
    epx_pixmap_t* px;

    if (!(px = (epx_pixmap_t*) malloc(sizeof(epx_pixmap_t))))
	return 0;
    if (epx_pixmap_init(px, width, height, fmt) < 0) {
	free(px);
	return 0;
    }
    px->on_heap = 1;
    px->refc = 1;
    return px;
}

int epx_pixmap_init_copy(epx_pixmap_t* src, epx_pixmap_t* dst)
{
    if (epx_pixmap_init(dst, src->width, src->height, src->pixel_format) < 0)
	return -1;
    if (!src->data0) // it's a sub_pixmap
	epx_copy_area(src->data, src->bytes_per_row, src->pixel_format,
		      dst->data, dst->bytes_per_row, dst->pixel_format,
		      src->width, src->height);
    else
	epx_copy(src->data, dst->data, src->sz);
    return 0;
}


// epx_pixmap_copy
//  Create an identical copy of src pixmap
//
epx_pixmap_t* epx_pixmap_copy(epx_pixmap_t* src)
{
    epx_pixmap_t* dst;

    if (!(dst = (epx_pixmap_t*) malloc(sizeof(epx_pixmap_t))))
	return 0;
    if (epx_pixmap_init_copy(src, dst) < 0) {
	free(dst);
	return 0;
    }
    dst->on_heap = 1;
    dst->refc = 1;
    return dst; 
}

// epx_pixmap_sub_pixmap
//  Create an identical copy of src pixmap
//  width and height may be updated since the copy must be
//  inclusive, it is never extended
//
int epx_pixmap_init_sub_pixmap(epx_pixmap_t* src, epx_pixmap_t* dst, 
			       int x, int y,
			       unsigned int width, unsigned int height)
{
    epx_rect_t sr;
    epx_rect_t dr;

    epx_rect_set(&sr, 0, 0, src->width, src->height);
    epx_rect_set(&dr, x, y, width, height);
    epx_rect_intersect(&dr, &sr, &dr);

    EPX_OBJECT_INIT(dst, EPX_PIXMAP_TYPE);

    dst->backend = 0;
    dst->detach  = 0;
    dst->parent  = src;
    epx_object_ref(src);  // set a reference to parent

    epx_rect_set(&dst->clip, 0, 0, dr.wh.width, dr.wh.height);
    dst->width          = dr.wh.width;
    dst->bytes_per_row  = src->bytes_per_row;
    dst->height         = dr.wh.height;
    dst->bits_per_pixel = src->bits_per_pixel;
    dst->pixel_format   = src->pixel_format;
    dst->func           = src->func;
    dst->sz             = src->bytes_per_row*dr.wh.height;

    dst->data0 = 0;  // signal sub-pixmap
    dst->data  = EPX_PIXEL_ADDR(src, dr.xy.x, dr.xy.y);
    return 0;
}


epx_pixmap_t* epx_pixmap_sub_pixmap(epx_pixmap_t* src,
				    int x, int y,
				    unsigned int width,
				    unsigned int height)
{
    epx_pixmap_t* dst;

    if (!(dst = (epx_pixmap_t*) malloc(sizeof(epx_pixmap_t))))
	return 0;
    if (epx_pixmap_init_sub_pixmap(src, dst, x, y, width, height) < 0) {
	free(dst);
	return 0;
    }
    dst->on_heap = 1;
    dst->refc    = 1;
    return dst;
}

void epx_pixmap_destroy(epx_pixmap_t* pic)
{
    epx_object_unref(pic);
}

// extern void epx_pixmap_detach(epx_pixmap_t*);

void EPX_PIXMAP_TYPE_RELEASE(void* arg)
{
    epx_pixmap_t* px = (epx_pixmap_t*) arg;

    EPX_DBGFMT_MEM("EPIXMAP_TYPE_RELEASE: %p", arg);
    if (px->detach)  // detach from backend (or something)
	px->detach(px);
    if (px->data0) {
	free(px->data0);
	px->data0 = 0;
    }
    px->data = 0;
    epx_object_unref(px->parent);
    if (px->on_heap) free(px);
}


void epx_pixmap_set_clip(epx_pixmap_t* pic, epx_rect_t* clip)
{
    epx_rect_t physRect;

    epx_rect_set(&physRect, 0, 0, pic->width, pic->height);
    epx_rect_intersect(clip, &physRect, &pic->clip);
}


/* put pixel given address */
static inline void put_apixel(uint8_t* addr, 
			      epx_pixel_unpack_t unpack,
			      epx_pixel_pack_t pack,
			      epx_flags_t flags, epx_pixel_t s)
{
    if (((flags & EPX_FLAG_BLEND)==0) || (s.a == EPX_ALPHA_OPAQUE))
	pack(s, addr);
    else if (s.a != EPX_ALPHA_TRANSPARENT) {
	epx_pixel_t d = unpack(addr);
	d = epx_pixel_blend(s.a, s, d);
	pack(d, addr);
    }
}

/* plot pixel, with fixed color */
void epx_pixmap_put_pixel(epx_pixmap_t* pic, int x, int y, epx_flags_t flags, epx_pixel_t p)
{
    uint8_t* dst;

    if (!epx_point_xy_in_rect(x, y, &pic->clip))
	return;
    dst = EPX_PIXEL_ADDR(pic,x,y);
    put_apixel(dst,pic->func.unpack,pic->func.pack,flags,p);
}


// epx_pixmap_PutPixels:
//  copy pixel data (0,0,width,height) => (x_dst,y_dst,width,height)
//  using blending function 

void epx_pixmap_put_pixels(epx_pixmap_t* dst, int x, int y,
			   unsigned int width, unsigned int height,
			   epx_format_t pixel_format, epx_flags_t flags,
			   void* data, unsigned int len)
{
    uint8_t* src_ptr;
    uint8_t* src_end;
    unsigned int src_psz = EPX_PIXEL_BYTE_SIZE(pixel_format);
    unsigned int src_wb  = width*src_psz;
    epx_rect_t sr;
    epx_rect_t dr;
    epx_rect_t dr0 = {{x,y},{width,height}};

    // Clip destination and check if we have any thing to copy
    if (!epx_rect_intersect(&dr0, &dst->clip, &dr))
	return;
    sr.wh = dr.wh;
    sr.xy.x = (epx_rect_left(&dr0) - epx_rect_left(&dr));
    sr.xy.y = (epx_rect_top(&dr0) - epx_rect_top(&dr));

    src_ptr = ((uint8_t*)data) + 
	(epx_rect_top(&sr)*src_wb) + (epx_rect_left(&sr)*src_psz);
    src_end  = ((uint8_t*)data) + 
	(epx_rect_bottom(&sr)*src_wb) + (epx_rect_right(&sr)*src_psz);
    // width,height pixelType and/or len is not matching
    if (src_end >= (((uint8_t*)data)+len))
	return;

    if ((flags & EPX_FLAG_BLEND)==0)
	epx_copy_area(src_ptr, src_wb, pixel_format,
		      EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		      dst->bytes_per_row, dst->pixel_format,
		      dr.wh.width, dr.wh.height);
    else
	dst->func.blend_area(src_ptr, src_wb, pixel_format,
			     EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			     dst->bytes_per_row, dst->pixel_format,
			     dr.wh.width, dr.wh.height);
}


// read a pixel 
epx_pixel_t epx_pixmap_get_pixel(epx_pixmap_t* pic, int x, int y)
{
    uint8_t* src;

    if (!epx_point_xy_in_rect(x,y,&pic->clip))
	return epx_pixel_black;
    src = EPX_PIXEL_ADDR(pic,x,y);
    return pic->func.unpack(src);
}

// Fill epx_pixmap_ with colors from p 
void epx_pixmap_fill(epx_pixmap_t* dst, epx_pixel_t p)
{
    if (!dst->data0 || (dst->bytes_per_pixel==3)) {  // a sub-pixmap
	epx_fill_area(dst->data, dst->bytes_per_row, dst->pixel_format, p,
		      dst->width, dst->height);
    }
    else {
	uint32_t    cv = 0;
	uint8_t*    cvp = (uint8_t*) &cv;

	dst->func.pack(p, cvp);
	switch(dst->bytes_per_pixel) {
	case 1:
	    epx_fill_8(dst->data,  cvp[0], dst->sz);
	    break;
	case 2: 
	    epx_fill_16(dst->data, cvp[0], cvp[1], dst->sz/2);	    
	    break;
	case 4: 
	    epx_fill_32(dst->data, cvp[0], cvp[1], cvp[2], cvp[3], dst->sz/4);
	    break;
	default:
	    break;
	}
    }
}

void epx_pixmap_fill_blend(epx_pixmap_t* dst, epx_pixel_t p)
{
    dst->func.fill_area_blend(dst->data, dst->bytes_per_row, 
			      dst->pixel_format, p,
			      dst->width, dst->height);
}


/* Flip the Pixmap (y direction) */
void epx_pixmap_flip(epx_pixmap_t* pic)
{
    int n = pic->height/2;
    uint8_t* ptr1 = EPX_PIXEL_ADDR(pic, 0, 0);
    uint8_t* ptr2 = EPX_PIXEL_ADDR(pic, 0, pic->height-1);

    while(n--) {
	epx_swap_8(ptr1, ptr2, pic->bytes_per_row);
	ptr1 += pic->bytes_per_row;
	ptr2 -= pic->bytes_per_row;
    }
}

// copy area and shift lines left or right 
static inline void shift_area(uint8_t* src, int src_wb, int src_pt,
			      uint8_t* dst, int dst_wb, int dst_pt,
			      unsigned int width, unsigned int height, 
			      int amount)
{
    if (amount > 0) {
	int psz = EPX_PIXEL_BYTE_SIZE(src_pt);
	src   += amount*psz;
	width -= amount;
    }
    else {
	int psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
	dst   += (-amount)*psz;
	width -= (-amount);
    }

    while(height--) {
	epx_copy_row(src, src_pt, dst, dst_pt, width);
	src += src_wb;
	dst += dst_wb;
    }
}


/* copy area and shift lines left or right */
static inline void shift_area_rotate(uint8_t* src, int src_wb, int src_pt,
				     uint8_t* dst, int dst_wb, int dst_pt,
				     unsigned int width, unsigned int height, 
				     int amount)
{
    int a = (amount < 0) ? -amount : amount;
    int src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
    int dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
    int n = width;
    int is_inline = (src == dst);
    uint8_t* src_from;
    uint8_t* dst_to;

    if (amount == 0)
	return;
    else if (amount > 0) {
	src_from = src;
	src += a*src_psz;
	n -= a;
	dst_to = dst + n*dst_psz;
    }
    else {
	n  -= a;
	src_from = src+n*src_psz;
	dst_to = dst;
	dst += a*dst_psz;
    }

    n *= src_psz;
    a *= src_psz;

    if (is_inline) {
	uint8_t* save = malloc(a);

	while(height--) {
	    memcpy(save, src_from, a);
	    memmove(dst, src, n);
	    memcpy(dst_to, save, a);
	    src      += src_wb;
	    src_from += src_wb;
	    dst      += dst_wb;
	    dst_to   += dst_wb;
	}
	free(save);
    }
    else {  /* not inline */
	while(height--) {
	    epx_copy_row(src, src_pt, dst, dst_pt, n);
	    epx_copy_row(src_from, src_pt, dst_to, dst_pt, a);
	    src      += src_wb;
	    src_from += src_wb;
	    dst      += dst_wb;
	    dst_to   += dst_wb;
	}
    }
}

/*
 * Copy pixmap data from src to dst, ignore clip region
 * if pixmap size is the same just memcpy
 * otherwise calculate the min area and copy that
 */
void epx_pixmap_copy_to(epx_pixmap_t* src, epx_pixmap_t* dst)
{
    if (SIMD_ENABLED() &&
	(src->width==dst->width) && 
	(src->height == dst->height) && 
	(src->sz == dst->sz) && 
	(src->pixel_format == dst->pixel_format)) {
	SIMD_CALL(copy)(src->data, dst->data, src->sz);
    }
    else {
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h = (src->height < dst->height) ? src->height : dst->height;
	epx_copy_area(src->data,src->bytes_per_row,src->pixel_format,
		      dst->data,dst->bytes_per_row,dst->pixel_format, w,h);
    }
}


void epx_pixmap_scroll_left(epx_pixmap_t* src, epx_pixmap_t* dst,
			    int rotate, unsigned int amount, epx_pixel_t fill)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	shift_area_rotate(src->data, src->bytes_per_row, src->pixel_format, 
			  dst->data, dst->bytes_per_row, dst->pixel_format, 
			  w, h, a);
    else {
	shift_area(src->data, src->bytes_per_row, src->pixel_format,
		   dst->data, dst->bytes_per_row, dst->pixel_format, w, h, a);
	epx_fill_area(dst->data+dst->bytes_per_pixel*(w-amount),
		      dst->bytes_per_row,dst->pixel_format, fill,
		      amount, h);
    }
}

void epx_pixmap_scroll_right(epx_pixmap_t* src, epx_pixmap_t* dst,
			     int rotate, unsigned int amount, epx_pixel_t fill)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	shift_area_rotate(src->data, src->bytes_per_row, src->pixel_format,
			  dst->data, dst->bytes_per_row, dst->pixel_format,
			  w, h, -a);
    else {
	shift_area(src->data, src->bytes_per_row, src->pixel_format,
		   dst->data, dst->bytes_per_row, dst->pixel_format, w, h, -a);
	epx_fill_area(dst->data, dst->bytes_per_row, dst->pixel_format, fill,
		      amount, h);
    }
}

void epx_pixmap_scroll_up(epx_pixmap_t* src, epx_pixmap_t* dst, 
			  int rotate, unsigned int amount, epx_pixel_t fill)
{
    if ((amount >= src->height) && !rotate)
	epx_pixmap_fill(dst, fill);
    else {
	uint8_t* dst_ptr;
	uint8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EPX_PIXEL_ADDR(src,0,amount);
	dst_ptr = EPX_PIXEL_ADDR(dst,0,0);

	if (rotate) {
	    if (src == dst) {
		uint8_t* save = malloc(amount*src->width*dst->bytes_per_pixel);
		uint8_t* dst_save = EPX_PIXEL_ADDR(dst,0,0);

		epx_copy_area(dst_save, dst->bytes_per_row, dst->pixel_format,
				save, dst->bytes_per_row, dst->pixel_format, 
				dst->width, amount);
		epx_copy_area(src_ptr, src->bytes_per_row, src->pixel_format,
				dst_ptr, dst->bytes_per_row, dst->pixel_format,
				w, h);
		dst_ptr = EPX_PIXEL_ADDR(dst,0,h);
		epx_copy_area(save, dst->bytes_per_row, dst->pixel_format,
			      dst_ptr, dst->bytes_per_row, dst->pixel_format,
			      w, amount);
		free(save);
	    }
	    else {
		epx_copy_area(src_ptr, src->bytes_per_row, src->pixel_format,
			      dst_ptr, dst->bytes_per_row, dst->pixel_format,
			      w, h);
		dst_ptr = EPX_PIXEL_ADDR(dst,0,h);
		src_ptr = EPX_PIXEL_ADDR(src,0,0);
		epx_copy_area(src_ptr, src->bytes_per_row, src->pixel_format,
			      dst_ptr, dst->bytes_per_row, dst->pixel_format,
			      w, amount);
	    }
	}
	else {
	    epx_copy_area(src_ptr, src->bytes_per_row, src->pixel_format,
			  dst_ptr, dst->bytes_per_row, dst->pixel_format, w, h);
	    dst_ptr = EPX_PIXEL_ADDR(dst,0,h);
	    epx_fill_area(dst_ptr, dst->bytes_per_row, dst->pixel_format, fill,
			  dst->width, amount);
	}
    }
}

void epx_pixmap_scroll_down(epx_pixmap_t* src, epx_pixmap_t* dst, 
			    int rotate, unsigned int amount, epx_pixel_t fill)
{
    if ((amount >= src->height) && !rotate)
	epx_pixmap_fill(dst, fill);
    else {
	uint8_t* dst_ptr;
	uint8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EPX_PIXEL_ADDR(src,0,0);
	dst_ptr = EPX_PIXEL_ADDR(dst,0,amount);

	if (rotate) {
	    if (src == dst) {
		uint8_t* save = malloc(amount*src->width*dst->bytes_per_pixel);
		uint8_t* dst_save = EPX_PIXEL_ADDR(dst,0,h);

		epx_copy_area(dst_save, dst->bytes_per_row, dst->pixel_format,
			      save, dst->bytes_per_row, dst->pixel_format, 
			      dst->width, amount);
		epx_copy_area(src_ptr, src->bytes_per_row, src->pixel_format,
			      dst_ptr, dst->bytes_per_row, dst->pixel_format,
			      w, h);
		dst_ptr = EPX_PIXEL_ADDR(dst,0,0);
		epx_copy_area(save, dst->bytes_per_row, dst->pixel_format,
			      dst_ptr, dst->bytes_per_row, dst->pixel_format,
			      w, amount);
		free(save);
	    }
	    else {
		epx_copy_area(src_ptr, src->bytes_per_row, src->pixel_format,
			      dst_ptr, dst->bytes_per_row, dst->pixel_format,
			      w, h);
		src_ptr = EPX_PIXEL_ADDR(src,0,h);
		dst_ptr = EPX_PIXEL_ADDR(dst,0,0);
		epx_copy_area(src_ptr, src->bytes_per_row, dst->pixel_format,
			      dst_ptr, dst->bytes_per_row, dst->pixel_format,
			      w, amount);
	    }
	}
	else {
	    epx_copy_area(src_ptr, src->bytes_per_row, src->pixel_format,
			    dst_ptr, dst->bytes_per_row, dst->pixel_format,
			    w, h);
	    dst_ptr = EPX_PIXEL_ADDR(dst,0,0);
	    epx_fill_area(dst_ptr, dst->bytes_per_row, dst->pixel_format, fill,
			  dst->width, amount);
	}
    }
}

/* scroll pixmap up/dow  left/right rotate/fill */
void epx_pixmap_scroll(epx_pixmap_t* src, epx_pixmap_t* dst, 
		       int horizontal, int vertical, 
		       int rotate, epx_pixel_t fill)
{
    if (vertical>0)
	epx_pixmap_scroll_up(src, dst, rotate, vertical, fill);
    else if (vertical < 0)
	epx_pixmap_scroll_down(src, dst, rotate, -vertical, fill);
    if (horizontal>0)
	epx_pixmap_scroll_right(src, dst, rotate, horizontal, fill);
    else if (horizontal < 0)
	epx_pixmap_scroll_left(src, dst, rotate, -horizontal, fill);
}



/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * blending the src with the dest 
 */
void epx_pixmap_copy_area(epx_pixmap_t* src,epx_pixmap_t* dst,
			  int x_src, int y_src, int x_dst, int y_dst,
			  unsigned int width, unsigned int height,
			  epx_flags_t flags)
{
    epx_rect_t sr, dr;

    if (epx_clip_both(src,dst,x_src,y_src,x_dst,y_dst,
			      width, height, &sr, &dr) < 0)
	return;

    if ((flags & (EPX_FLAG_BLEND|EPX_FLAG_SUM))==0)
	epx_copy_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		      src->bytes_per_row, src->pixel_format,
		      EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		      dst->bytes_per_row, dst->pixel_format,
		      dr.wh.width, dr.wh.height);
    else if  ((flags & (EPX_FLAG_BLEND|EPX_FLAG_SUM))==EPX_FLAG_SUM) {
	epx_sum_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		     src->bytes_per_row, src->pixel_format,
		     EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		     dst->bytes_per_row, dst->pixel_format,
		     dr.wh.width, dr.wh.height);
    }
    else
	src->func.blend_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			     src->bytes_per_row, src->pixel_format,
			     EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			     dst->bytes_per_row, dst->pixel_format,
			     dr.wh.width, dr.wh.height);
}


/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * blending using alpha.
 */
void epx_pixmap_alpha_area(epx_pixmap_t* src,epx_pixmap_t* dst, uint8_t alpha,
			   int x_src, int y_src, int x_dst, int y_dst,
			   unsigned int width, unsigned int height)
{
    epx_rect_t sr, dr;

    if (epx_clip_both(src,dst,x_src,y_src,x_dst,y_dst,
		      width, height, &sr, &dr) < 0)
	return;

    if (alpha == EPX_ALPHA_TRANSPARENT)
	return;
    else
	src->func.alpha_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y),
			     src->bytes_per_row, src->pixel_format,
			     EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			     dst->bytes_per_row, dst->pixel_format,
			     alpha, dr.wh.width, dr.wh.height);
}

/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * fade using fader value
 */
void epx_pixmap_fade_area(epx_pixmap_t* src,epx_pixmap_t* dst, uint8_t fade,
			  int x_src, int y_src, int x_dst, int y_dst,
			  unsigned int width, unsigned int height)
{
    epx_rect_t sr, dr;

    if (epx_clip_both(src,dst,x_src,y_src,x_dst,y_dst,
			      width, height, &sr, &dr) < 0)
	return;

    src->func.fade_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			src->bytes_per_row, src->pixel_format,
			EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			dst->bytes_per_row, dst->pixel_format,
			fade, dr.wh.width, dr.wh.height);
}


/* Shadow src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * this function will blend the pixels from source with
 * the luminance value as alpha.
 */
void epx_pixmap_shadow_area(epx_pixmap_t* src,epx_pixmap_t* dst,
			    int x_src, int y_src, int x_dst, int y_dst,
			    unsigned int width, unsigned int height,
			    epx_flags_t flags)
{
    epx_rect_t sr, dr;

    if (epx_clip_both(src,dst,x_src,y_src,x_dst,y_dst,
			      width, height, &sr, &dr) < 0)
	return;
    epx_shadow_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		    src->bytes_per_row, src->pixel_format,
		    EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		    dst->bytes_per_row, dst->pixel_format,
		    dr.wh.width, dr.wh.height, flags);
}


void epx_pixmap_add_color_area(epx_pixmap_t* src,epx_pixmap_t* dst, 
			       uint8_t fade, epx_pixel_t color,
			       int x_src, int y_src, int x_dst, int y_dst,
			       unsigned int width, unsigned int height, 
			       epx_flags_t flags)
{
    epx_rect_t sr, dr;

    if (epx_clip_both(src,dst,x_src,y_src,x_dst,y_dst,
			      width, height, &sr, &dr) < 0)
	return;

    epx_add_color_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		       src->bytes_per_row, src->pixel_format,
		       EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		       dst->bytes_per_row, dst->pixel_format,
		       fade, color, dr.wh.width, dr.wh.height, flags);
}


/* filter src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * blending using alpha.
 */
void epx_pixmap_filter_area(epx_pixmap_t* src,epx_pixmap_t* dst,
			    epx_filter_t* filter,
			    int x_src, int y_src, int x_dst, int y_dst,
			    unsigned int width, unsigned int height,
			    epx_flags_t flags)
{
    epx_rect_t sr, dr;

    if (epx_clip_both(src,dst,x_src,y_src,x_dst,y_dst,
			      width, height, &sr, &dr) < 0)
	return;
    filter_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		src->bytes_per_row, src->pixel_format,
		EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		dst->bytes_per_row, dst->pixel_format,
		filter,dr.wh.width, dr.wh.height,flags);
}

/*
 *   T = [ A  B Tx ] [x]
 *       [ C  D Ty ] [y]
 *                   [1]
 *
 *   x' = A*x + B*y + Tx
 *   y' = C*x + D*y + Ty
 */

#define TRANSFORM(xp,yp,A,B,C,D,Tx,Ty,x,y) \
    xp = (A)*(x) + (B)*(y) + (Tx); \
    yp = (C)*(x) + (D)*(y) + (Ty)

/*
 * Rotate a Pixmap x_dst and y_dst point out the rotation center
 * in the destination Pixmap
 *
 *     R: [ cos(a)   sin(a) ]
 *        [-sin(a)   cos(a) ]
 *
 *     D = R * S
 *
 *     R':[ cos(a)  -sin(a) ]
 *        [ sin(a)   cos(a) ]
 *
 *     S = R' * D
 *
 */

void epx_pixmap_rotate_area(epx_pixmap_t* src, epx_pixmap_t* dst, float angle,
			    int x_src, int y_src, int xc_src, int yc_src,
			    int xc_dst, int yc_dst,
			    unsigned int width, unsigned int height, 
			    epx_flags_t flags)
{
    epx_rect_t sr = {{x_src,y_src}, {width,height}};
    epx_rect_t dr;
    float sa = sinf(angle);
    float ca = cosf(angle);
    float min_dx, max_dx;
    float min_dy, max_dy;
    float x, y;
    float xo, yo;

    if (!epx_rect_intersect(&sr, &src->clip, &sr))
	return;
    dr = dst->clip;

    /* calculate dest
     *
     * min_dx, max_dx
     * min_dy, max_dy
     */
    xo = xc_src - x_src;
    yo = yc_src - y_src;

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, -xo, -yo);
    min_dx = max_dx = x;
    min_dy = max_dy = y;

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, (width-1)-xo, -yo);
    min_dx = epx_min_float(min_dx, x);
    max_dx = epx_max_float(max_dx, x);
    min_dy = epx_min_float(min_dy, y);
    max_dy = epx_max_float(max_dy, y);

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, (width-1)-xo, (height-1)-yo);
    min_dx = epx_min_float(min_dx, x);
    max_dx = epx_max_float(max_dx, x);
    min_dy = epx_min_float(min_dy, y);
    max_dy = epx_max_float(max_dy, y);

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, -xo, (height-1)-yo);
    min_dx = epx_min_float(min_dx, x);
    max_dx = epx_max_float(max_dx, x);
    min_dy = epx_min_float(min_dy, y);
    max_dy = epx_max_float(max_dy, y);


    for (y = min_dy; y <= max_dy; y++) {
	for (x = min_dx; x <= max_dx; x++) {
	    float xsf, ysf;
	    int xs, ys;

	    TRANSFORM(xsf,ysf, ca, -sa, sa, ca, xc_src, yc_src, x, y);
	    xs = xsf; //nearbyintf(xsf); // round(xsf);
	    ys = ysf; // nearbyintf(ysf); // round(ysf);

	    if (epx_point_xy_in_rect(xs, ys, &sr)) {
		int xd = x + xc_dst;
		int yd = y + yc_dst;

		if (epx_point_xy_in_rect(xd, yd, &dr)) {
		    uint8_t* dst_addr = EPX_PIXEL_ADDR(dst,xd,yd);
		    epx_pixel_t p;
		    if (flags & EPX_FLAG_AALIAS)
			p = epixel_interp(src, xsf, ysf);
		    else {
			uint8_t* src_addr = EPX_PIXEL_ADDR(src,xs,ys);
			p = src->func.unpack(src_addr);
		    }
		    put_apixel(dst_addr,dst->func.unpack,dst->func.pack,flags,p);
		}
	    }
	}
    }
}

//
// Take the src pixels and transform them to new width,height
// and place pixels in dst.
//
void epx_pixmap_scale(epx_pixmap_t* src, epx_pixmap_t* dst, 
		      unsigned int width, unsigned int height)
{
    epx_rect_t sr, dr;
    int y;
    unsigned h;
    float xs, ys;

    if (epx_clip_dst(src,dst,0,0,0,0, width, height, &sr, &dr) < 0)
	return;
    // The scale factor is still (width/src->width, height/src-height)
    // The inverted scale is:
    xs = src->width/(float) width;
    ys = src->height/(float) height;

    y = dr.xy.y;
    h = dr.wh.height;
    while(h--) {
	float ysf = y*ys;
	int x = dr.xy.x;
	unsigned w = dr.wh.width;

	while(w--) {
	    float xsf = x*xs;
	    epx_pixel_t p = epixel_interp(src, xsf, ysf);
	    uint8_t* addr = EPX_PIXEL_ADDR(dst,x,y);
	    dst->func.pack(p, addr);
	    x++;
	}
	y++;
    }
}

//
// Binary operation Dst = Func(Fade,Color,Src,Dst)
//
void epx_binop_area(uint8_t* src, int src_wb, epx_format_t src_pt,
		    uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		    epx_pixel_binary_op_t binop,
		    unsigned int width, 
		    unsigned int height)
{
    unsigned int       src_psz;
    unsigned int       dst_psz;
    epx_pixel_unpack_t unpack_dst;
    epx_pixel_unpack_t unpack_src;
    epx_pixel_pack_t   pack_dst;

    src_psz = EPX_PIXEL_BYTE_SIZE(src_pt);
    dst_psz = EPX_PIXEL_BYTE_SIZE(dst_pt);
    unpack_src = epx_pixel_unpack_func(src_pt);
    unpack_dst = epx_pixel_unpack_func(dst_pt);
    pack_dst   = epx_pixel_pack_func(dst_pt);

    while(height--) {
	uint8_t* dst1 = dst;
	uint8_t* src1 = src;
	unsigned int width1 = width;
	while(width1--) {
	    epx_pixel_t d = unpack_dst(dst1);
	    epx_pixel_t s = unpack_src(src1);
	    d = binop(s,d);
	    pack_dst(d, dst1);
	    src1 += src_psz;
	    dst1 += dst_psz;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

static epx_pixel_t binop_clear(epx_pixel_t a, epx_pixel_t b)
{
    (void) a;
    (void) b;
    return epx_pixel_transparent;
}

static epx_pixel_t binop_src(epx_pixel_t a, epx_pixel_t b)
{
    (void) b;
    return epx_pixel_scale(a.a, a);
}

static epx_pixel_t binop_dst(epx_pixel_t a, epx_pixel_t b)
{
    (void) a;
    return epx_pixel_scale(b.a, b);
}

static epx_pixel_t binop_src_over(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_over(a, b);
}

static epx_pixel_t binop_dst_over(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_over(b, a);
}

static epx_pixel_t binop_src_in(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_in(a, b);
}

static epx_pixel_t binop_dst_in(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_in(b, a);
}

static epx_pixel_t binop_src_out(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_out(a, b);
}

static epx_pixel_t binop_dst_out(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_out(b, a);
}


static epx_pixel_t binop_src_atop(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_atop(a, b);
}

static epx_pixel_t binop_dst_atop(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_atop(b, a);
}

static epx_pixel_t binop_xor(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_xor(a, b);
}

static epx_pixel_t binop_copy(epx_pixel_t a, epx_pixel_t b)
{
    (void) b;
    return a;
}

static epx_pixel_t binop_src_blend(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_blend(a.a, a, b);
}

static epx_pixel_t binop_dst_blend(epx_pixel_t a, epx_pixel_t b)
{
    return epx_pixel_blend(b.a, b, a);
}

// FUNCTION: epx_binop_sub_AREA ( ... )
#define AREA_FUNCTION         epx_binop_sub_AREA
#define AREA_PARAMS_DECL
#define AREA_OPERATION(s,d)   epx_pixel_sub(s,d)
#include "epx_area_body.i"
#undef AREA_FUNCTION
#undef AREA_PARAMS_DECL
#undef AREA_OPERATION
// END-FUNCTION


// FUNCTION: epx_binop_add_AREA ( ... )
#define AREA_FUNCTION         epx_binop_add_AREA
#define AREA_PARAMS_DECL
#define AREA_OPERATION(s,d)   epx_pixel_add(s,d)
#include "epx_area_body.i"
#undef AREA_FUNCTION
#undef AREA_PARAMS_DECL
#undef AREA_OPERATION
// END-FUNCTION

void epx_pixmap_operation_area(epx_pixmap_t* src,epx_pixmap_t* dst, 
			       epx_pixel_operation_t op,
			       int x_src, int y_src, int x_dst, int y_dst,
			       unsigned int width, unsigned int height)
{
    epx_rect_t sr, dr;
    epx_pixel_binary_op_t binop;

    if (epx_clip_both(src,dst,x_src,y_src,x_dst,y_dst,
			      width, height, &sr, &dr) < 0)
	return;
    switch(op) {
    case EPX_PIXEL_OP_CLEAR:    binop = binop_clear; break;
    case EPX_PIXEL_OP_SRC:      binop = binop_src; break;
    case EPX_PIXEL_OP_DST:      binop = binop_dst; break;
    case EPX_PIXEL_OP_SRC_OVER: binop = binop_src_over; break;
    case EPX_PIXEL_OP_DST_OVER: binop = binop_dst_over; break;
    case EPX_PIXEL_OP_SRC_IN:   binop = binop_src_in; break;
    case EPX_PIXEL_OP_DST_IN:   binop = binop_dst_in; break;
    case EPX_PIXEL_OP_SRC_OUT:  binop = binop_src_out; break;
    case EPX_PIXEL_OP_DST_OUT:  binop = binop_dst_out; break;
    case EPX_PIXEL_OP_SRC_ATOP: binop = binop_src_atop; break;
    case EPX_PIXEL_OP_DST_ATOP: binop = binop_dst_atop; break;
    case EPX_PIXEL_OP_XOR:      binop = binop_xor; break;
    case EPX_PIXEL_OP_COPY:     binop = binop_copy; break;
    case EPX_PIXEL_OP_ADD:
	epx_binop_add_AREA(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		   src->bytes_per_row, src->pixel_format,
			   EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			   dst->bytes_per_row, dst->pixel_format,
			   dr.wh.width, dr.wh.height);
	return;
    case EPX_PIXEL_OP_SUB:
	epx_binop_sub_AREA(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			   src->bytes_per_row, src->pixel_format,
			   EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			   dst->bytes_per_row, dst->pixel_format,
			   dr.wh.width, dr.wh.height);
	return;
    case EPX_PIXEL_OP_SRC_BLEND: binop = binop_src_blend; break;
    case EPX_PIXEL_OP_DST_BLEND: binop = binop_dst_blend; break;
    default: return;
    }
    epx_binop_area(EPX_PIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		   src->bytes_per_row, src->pixel_format,
		   EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		   dst->bytes_per_row, dst->pixel_format,
		   binop, dr.wh.width, dr.wh.height);
}


