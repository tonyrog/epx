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
// SIMD area template
//
// Idea how to make simd functions a bit more "easy"
//

// macros used:\
//   DO_UNALIGNED(src_ptr, dst_ptr, #pixels) -> void
//   DO_SIMD(src_vector, dst_vector) -> result_vector
// 

//
// Function template 
// Src Dst Width Height
//
void epx_simd_area(u_int8_t* src, int src_wb,
		   u_int8_t* dst, int dst_wb,
		   unsigned int width, unsigned int height)
{
    unsigned int doffs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    unsigned int soffs = EPX_ALIGN_OFFS(src,EPX_SIMD_VECTOR_ALIGN);
    int walign = 0;

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = epx_min_int((doffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		DO_UNALIGNED(src1,dst1,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }

	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load_ua32(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = DO_SIMD(ts, td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1) {
		DO_UNALIGNED(src1,dst1,width1);
	    }
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
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		DO_UNALIGNED(src1,dst1,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t ts = epx_simd_vector_load(src1);
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = DO_SIMD(ts, td);
		epx_simd_vector_store(dst1, td);
		src1 += EPX_SIMD_VECTOR_SIZE;
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		DO_UNALIGNED(src1,dst1,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
}

		   
