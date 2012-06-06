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
// Emulator version of SIMD functions
//
#include <memory.h>
#include "epx_simd_int.h"
#include "epx_simd_emu.h"
#include "epx_pixmap.h"

#include "epx_simd_common.c"

void epx_simd_copy_emu(uint8_t* src, uint8_t* dst, size_t n)
{
    memcpy(dst, src, n);
}

void epx_simd_fill_32_emu(uint8_t* dst, uint32_t v, size_t n)
{
    switch((unsigned long)dst & 0x3) {
    case 0: {	/* 32 bit aligned */
	uint32_t* ptr32 = (uint32_t*) dst;
	while(n--) 
	    *ptr32++ = v;
	break;
    }

    case 2: { /* 16 bit aligned */
	uint16_t* ptr16 = (uint16_t*) dst;
	uint16_t v1 = v >> 16;
	uint16_t v0 = v & 0xffff;
	while(n--) {
	    *ptr16++ = v1;
	    *ptr16++ = v0;
	}
	break;
    }

    default: { /* 8 bit aligned */
	uint8_t v3 = v >> 24;
	uint8_t v2 = (v >> 16) & 0xff;
	uint8_t v1 = (v >> 8) & 0xff;
	uint8_t v0 = v & 0xff;
	while(n--) {
	    *dst++ = v3;
	    *dst++ = v2;
	    *dst++ = v1;
	    *dst++ = v0;
	}
	break;
    }

    }
}



/* DO  RGB or BGR (BGR is done by swapping r and b in p) */
void epx_simd_fill_area_blend_rgb24_emu(uint8_t* dst,int dst_wb,
					epx_pixel_t p,
					unsigned int width, 
					unsigned int height)
{
    /* Maybe use one rotating register ? instead of 3 ... */
    epx_vector_i8_t s8_0 = epx_simd_vector_set_8(p.r,p.g,p.b,p.r);
    epx_vector_i8_t s8_1 = epx_simd_vector_set_8(p.g,p.b,p.r,p.g);
    epx_vector_i8_t s8_2 = epx_simd_vector_set_8(p.b,p.r,p.g,p.b);
    epx_vector_u8_t a8 = epx_simd_vector_splat_u8(p.a);
    unsigned int offs = EPX_ALIGN_OFFS(dst, EPX_SIMD_VECTOR_ALIGN);
    unsigned int wb = width*3; // Number of bytes

    while(height--) {
	uint8_t* dst1      = dst;
	unsigned int offs1  = offs;
	unsigned int wb1    = wb;
	
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
	    case 0: td = epx_simd_alpha_32(a8,s8_0,td); break;
	    case 1: td = epx_simd_alpha_32(a8,s8_1,td); break;
	    case 2: td = epx_simd_alpha_32(a8,s8_2,td); break;
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


