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
 * NEON support (NOT Ready!)
 *
 */

#include "epx_simd_int.h"
#include "epx_simd_neon.h"
#include "epx_pixmap.h"

#if defined(__NEON__)

#define SIMD_FUNCTION(name) epx_simd_##name##_neon
#include "epx_simd.i"
#undef SIMD_FUNCTION

void epx_simd_copy_neon(const u_int8_t* src, u_int8_t* dst, size_t n)
{
    memcpy(dst, src, n);
}

/* This code assumes dst i at least 32 bit aligned (FIXME?) */
void epx_simd_fill_32_neon(u_int8_t* dst, u_int32_t v, size_t n)
{
    if (n < 4)
	epx_fill_row_32((u_int32_t*)dst, v, n);
    else {
	epx_vector_u8_t s8;
	unsigned int offs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
	int walign = offs ? epx_min_int((offs/4), n) : 0;

	s8 = epx_simd_vector_set_32(v,v,v,v);

	if (walign) {
	    epx_fill_row_32((u_int32_t*)dst, v, walign);
	    dst += offs;
	    n -= walign;
	}

	while(n > EPX_SIMD_VECTOR_SIZE) {
	    *((epx_vector_u8_t*)dst) = s8;
	    *((epx_vector_u8_t*)(dst+EPX_SIMD_VECTOR_SIZE)) = s8;
	    *((epx_vector_u8_t*)(dst+EPX_SIMD_VECTOR_SIZE*2)) = s8;
	    *((epx_vector_u8_t*)(dst+EPX_SIMD_VECTOR_SIZE*3)) = s8;
	    dst += EPX_SIMD_VECTOR_SIZE*4;
	    n -= EPX_SIMD_VECTOR_SIZE;
	}

	while(n >= EPX_SIMD_VECTOR_SIZE/4) {
	    *((epx_vector_u8_t*)dst) = s8;
	    dst += EPX_SIMD_VECTOR_SIZE;
	    n -= EPX_SIMD_VECTOR_SIZE/4;
	}
	if (n)
	    epx_fill_row_32((u_int32_t*)dst, v, n);
    }
}

/* DO  RGB or BGR (BGR is done by swapping r and b in p) */
void epx_simd_fill_area_blend_rgb24_neon(u_int8_t* dst,int dst_wb,
					 unsigned int width, 
					 unsigned int height, 
					 epx_pixel_t p)
{
    /* Maybe use one rotating register ? instead of 3 ... */
    epx_vector_i8_t s8_0 = epx_simd_vector_set_8(p.r,p.g,p.b,p.r,p.g,p.b,p.r,p.g,
					   p.b,p.r,p.g,p.b,p.r,p.g,p.b,p.r);
    epx_vector_i8_t s8_1 = epx_simd_vector_set_8(p.g,p.b,p.r,p.g,p.b,p.r,p.g,p.b,
					   p.r,p.g,p.b,p.r,p.g,p.b,p.r,p.g);
    epx_vector_i8_t s8_2 = epx_simd_vector_set_8(p.b,p.r,p.g,p.b,p.r,p.g,p.b,p.r,
					   p.g,p.b,p.r,p.g,p.b,p.r,p.g,p.b);
    epx_vector_i8_t a8 = epx_simd_vector_splat_8(p.a);
    unsigned int offs = EPX_ALIGN_OFFS(dst, EPX_SIMD_VECTOR_ALIGN);
    unsigned int wb = width*3; // Number of bytes

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int offs1  = offs;
	unsigned int wb1  = wb;
	
	// Align data pointer
	while((offs1>=3) && (wb1>=3)) {
	    dst1[0] = epx_blend(p.a,p.r,dst1[0]);
	    dst1[1] = epx_blend(p.a,p.g,dst1[1]);
	    dst1[2] = epx_blend(p.a,p.b,dst1[2]);
	    dst1  += 3;
	    offs1 -= 3;
	    wb1   -= 3;
	}
	offs1 = epx_min_int(offs1, wb1);
	switch(offs1) {
	case 1:
	    dst1[0] = epx_blend(p.a,p.r,dst1[0]);
	    dst1 += 1;
	    wb1  -= 1;
	    break;
	case 2:
	    dst1[0] = epx_blend(p.a,p.r,dst1[0]);
	    dst1[1] = epx_blend(p.a,p.g,dst1[1]);
	    dst1 += 2;
	    wb1  -= 2;
	    break;
	default:
	    /* either offs=0 or wb=0 */
	    break;
	}
	while(wb1 >= EPX_SIMD_VECTOR_SIZE) {
	    epx_vector_u8_t td = *((epx_vector_u8_t*)dst1);
	    switch(offs1) {
	    case 0: td = epx_simd_alpha_32_neon(a8,s8_0,td); break;
	    case 1: td = epx_simd_alpha_32_neon(a8,s8_1,td); break;
	    case 2: td = epx_simd_alpha_32_neon(a8,s8_2,td); break;
	    default: return;
	    }
	    *((epx_vector_u8_t*)dst1) = td;
	    dst1 += EPX_SIMD_VECTOR_SIZE;
	    wb1  -= EPX_SIMD_VECTOR_SIZE;
	    offs1 = (offs1+EPX_SIMD_VECTOR_SIZE) % 3;
	}
	// Do remaining RGB/BGR components
	while(wb1--) {
	    switch(offs1) {
	    case 0: *dst1 = epx_blend(p.a,p.r,dst1[0]); break;
	    case 1: *dst1 = epx_blend(p.a,p.g,dst1[0]); break;
	    case 2: *dst1 = epx_blend(p.a,p.b,dst1[0]); break;
	    default: return;
	    }
	    dst1++;
	    offs1 = (offs1+1) % 3;
	}
	dst += dst_wb;
    }
}
#endif
