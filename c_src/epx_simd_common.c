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
void SIMD_FUNCTION(add_blend_area_rgba32)
    (uint8_t* src, int src_wb,
     uint8_t* dst, int dst_wb,
     uint8_t af, epx_pixel_t color,
     unsigned int width, 
     unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    int iaf = af;
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);
    epx_vector_u8_t  c8 = epx_simd_vector_set_pixel(color.r,color.g,
						    color.b,color.a);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_add_blend_row_rgba32(src1,dst1,af,color,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		ts = epx_simd_adds_u8(c8, ts);
		td = epx_simd_fade_rgba32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_add_blend_row_rgba32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_add_blend_row_rgba32(src1,dst1,af,color,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		ts = epx_simd_adds_u8(c8, ts);
		td = epx_simd_fade_rgba32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_add_blend_row_rgba32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    epx_simd_empty_state();
}

//
// Same as add_blend_area_rgba but with alpha channel first
// 
void SIMD_FUNCTION(add_blend_area_argb32)
    (uint8_t* src, int src_wb,
     uint8_t* dst, int dst_wb,
     uint8_t af, epx_pixel_t color,
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    int iaf = af;
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);
    epx_vector_u8_t  c8 = epx_simd_vector_set_pixel(color.a,color.r,
						    color.g,color.b);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_add_blend_row_argb32(src1,dst1,af,color,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		ts = epx_simd_adds_u8(c8, ts);
		td = epx_simd_fade_argb32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_add_blend_row_argb32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_add_blend_row_argb32(src1,dst1,af,color,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		ts = epx_simd_adds_u8(c8, ts);
		td = epx_simd_fade_argb32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_add_blend_row_argb32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    epx_simd_empty_state();
}

// 
// 
//
void SIMD_FUNCTION(add_blend_area_a8_rgba32)
    (uint8_t* src, int src_wb, 
     uint8_t* dst, int dst_wb,
     uint8_t af, epx_pixel_t color,
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    int iaf = af;  // FIXME: af=255 => 0x0100 ? - check this
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
#if EPX_SIMD_VECTOR_PIXELS_ARGB32 == 4
	    uint32_t a0 = set_uint32(0,0,0,src1[0]);
	    uint32_t a1 = set_uint32(0,0,0,src1[1]);
	    uint32_t a2 = set_uint32(0,0,0,src1[2]);
	    uint32_t a3 = set_uint32(0,0,0,src1[3]);
	    epx_vector_u32_t ts = epx_simd_vector_set_32(a0,a1,a2,a3);
#elif EPX_SIMD_VECTOR_PIXELS_ARGB32 == 2
	    uint32_t a0 = set_uint32(0,0,0,src1[0]);
	    uint32_t a1 = set_uint32(0,0,0,src1[1]);
	    epx_vector_u32_t ts = epx_simd_vector_set_32(a0,a1);
#elif EPX_SIMD_VECTOR_PIXELS_ARGB32 == 1
	    uint32_t a0 = set_uint32(0,0,0,src1[0]);
	    epx_vector_u32_t ts = epx_simd_vector_set_32(a0);
#endif
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

//
// 
//
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
	    // FIXME!!!
#if EPX_SIMD_VECTOR_PIXELS_ARGB32 == 4
	    uint32_t a0 = set_uint32(src1[0],0,0,0);
	    uint32_t a1 = set_uint32(src1[1],0,0,0);
	    uint32_t a2 = set_uint32(src1[2],0,0,0);
	    uint32_t a3 = set_uint32(src1[3],0,0,0);
	    epx_vector_u32_t ts = epx_simd_vector_set_32(a0,a1,a2,a3);
#elif EPX_SIMD_VECTOR_PIXELS_ARGB32 == 2
	    uint32_t a0 = set_uint32(src1[0],0,0,0);
	    uint32_t a1 = set_uint32(src1[1],0,0,0);
	    epx_vector_u32_t ts = epx_simd_vector_set_32(a0,a1);
#elif EPX_SIMD_VECTOR_PIXELS_ARGB32 == 1
	    uint32_t a0 = set_uint32(src1[0],0,0,0);
	    epx_vector_u32_t ts = epx_simd_vector_set_32(a0);
#endif
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

//
//
//
void SIMD_FUNCTION(alpha_area_argb32)
    (uint8_t* src, int src_wb,
     uint8_t* dst, int dst_wb,
     uint8_t a,
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    epx_vector_i8_t a8 = epx_simd_vector_set_pixel(0,a,a,a);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);
	while(height--) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_alpha_row_argb32(src1,dst1,a,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_alpha_32(a8,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_alpha_row_argb32(src1, dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);
	
	while(height--) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_alpha_row_argb32(src1,dst1,a,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
		
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_alpha_32(a8,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_alpha_row_argb32(src1,dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }    
    epx_simd_empty_state();
}

void SIMD_FUNCTION(alpha_area_rgba32)
    (uint8_t* src, int src_wb,
     uint8_t* dst, int dst_wb,
     uint8_t a,
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    epx_vector_i8_t a8 = epx_simd_vector_set_pixel(a,a,a,0);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);

	while(height--) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_alpha_row_rgba32(src1,dst1,a,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
		    
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u32_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_alpha_32(a8,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_alpha_row_rgba32(src1, dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);
	
	while(height--) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;
	    if (walign) {
		epx_alpha_row_rgba32(src1,dst1,a,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
		
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_alpha_32(a8,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_alpha_row_rgba32(src1,dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }    
    epx_simd_empty_state();
}


void SIMD_FUNCTION(blend_area_rgba32)
    (uint8_t* src, int src_wb, 
     uint8_t* dst, int dst_wb,
     unsigned int width, unsigned int height)
{
    int walign = 0;
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);

    if (soffs != doffs) { // UNALIGNABLE align dst
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_blend_row_rgba32(src1, dst1, walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u32_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t  td = epx_simd_vector_load(dst1);
		td = epx_simd_blend_rgba32(ts, td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_blend_row_rgba32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_blend_row_rgba32(src1, dst1, walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_blend_rgba32(ts, td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_blend_row_rgba32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    epx_simd_empty_state();
}

void SIMD_FUNCTION(blend_area_argb32)
    (uint8_t* src, int src_wb, 
     uint8_t* dst, int dst_wb,
     unsigned int width, unsigned int height)
{
    int walign = 0;
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_blend_row_argb32(src1, dst1, walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u32_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_blend_argb32(ts, td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_blend_row_argb32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;
	    
	    if (walign) {
		epx_blend_row_argb32(src1, dst1, walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_blend_argb32(ts, td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_blend_row_argb32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    epx_simd_empty_state();
}


void SIMD_FUNCTION(fade_area_rgba32)
    (uint8_t* src, int src_wb,
     uint8_t* dst, int dst_wb,
     uint8_t af, 
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    int iaf = af;
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_fade_row_rgba32(src1,dst1,af,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u32_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_fade_rgba32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_fade_row_rgba32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_fade_row_rgba32(src1,dst1,af,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_fade_rgba32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_fade_row_rgba32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    epx_simd_empty_state();
}

void SIMD_FUNCTION(fade_area_argb32)
    (uint8_t* src, int src_wb,
     uint8_t* dst, int dst_wb,
     uint8_t af, 
     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;
    int iaf = af;
    epx_vector_u16_t fv = epx_simd_vector_splat_u16(iaf);


    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_fade_row_argb32(src1,dst1,af,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u32_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_fade_argb32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_fade_row_argb32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = epx_min_int((soffs/4), width);
	while(height > 0) {
	    unsigned int width1 = width;
	    uint8_t* src1 = src;
	    uint8_t* dst1 = dst;

	    if (walign) {
		epx_fade_row_argb32(src1,dst1,af,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_fade_argb32(fv,ts,td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_fade_row_argb32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    epx_simd_empty_state();
}


void SIMD_FUNCTION(fill_area_blend_argb32)
    (uint8_t* dst,int dst_wb,
     epx_pixel_t p,
     unsigned int width, unsigned int height)
{
    epx_vector_u8_t s8, a8;
    unsigned int offs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    int walign = offs ? epx_min_int((offs/4), width) : 0;

    s8 = epx_simd_vector_set_pixel(0,p.r,p.g,p.b);
    a8 = epx_simd_vector_splat_u8(p.a);

    while(height--) {
	uint8_t* dst1 = dst;
	unsigned int width1 = width;

	if (width1 < 4)
	    epx_fill_row_blend_argb32(dst1,width1,p.a,p.r,p.g,p.b);
	else {
	    if (walign) {
		epx_fill_row_blend_argb32(dst1,walign,p.a,p.r,p.g,p.b);
		dst1   += offs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_alpha_32(a8,s8,td);
		epx_simd_vector_store(dst1, td);
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_fill_row_blend_argb32(dst1,width1,p.a,p.r,p.g,p.b);
	}
	dst += dst_wb;
    }
    epx_simd_empty_state();
}

void SIMD_FUNCTION(fill_area_blend_rgba32)
    (uint8_t* dst,int dst_wb,
     epx_pixel_t p,
     unsigned int width, unsigned int height)
{
    epx_vector_u8_t s8, a8;
    unsigned int offs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    int walign = offs ? epx_min_int((offs/4), width) : 0;

    s8 = epx_simd_vector_set_pixel(p.r,p.g,p.b,0);
    a8 = epx_simd_vector_splat_u8(p.a);

    while(height--) {
	uint8_t* dst1 = dst;
	unsigned int width1 = width;
    
	if (width1 < 4)
	    epx_fill_row_blend_rgba32(dst1,width1,p.a,p.r,p.g,p.b);
	else {
	    if (walign) {
		epx_fill_row_blend_rgba32(dst1,walign,p.a,p.r,p.g,p.b);
		dst1   += offs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = epx_simd_alpha_32(a8,s8,td);
		epx_simd_vector_store(dst1, td);
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		epx_fill_row_blend_rgba32(dst1,width1,p.a,p.r,p.g,p.b);
	    dst += dst_wb;
	}
    }
    epx_simd_empty_state();
}
