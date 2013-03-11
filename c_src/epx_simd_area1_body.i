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
// SIMD area1 template
//
// Idea how to make simd functions a bit more "easy"
//

// macros used:
//   SIMD_AREA_FUNCTION     name of the function
//   SIMD_AREA_PARAMS_DECL  extra parameters
//   SIMD_AREA_PARAMS       name of the params
//   SIMD_AREA_UNALIGNED   (src_ptr,dst_ptr,#pixels) => void
//   SIMD_AREA_OPERATION   (src_vector,dst_vector) => result_vector
// 

//
// Function template 
// Src Dst Width Height
//
void SIMD_AREA_FUNCTION(uint8_t* dst, int dst_wb,
			SIMD_AREA_PARAMS_DECL
			unsigned int width, unsigned int height)
{
    unsigned int offs = EPX_ALIGN_OFFS(dst,EPX_SIMD_VECTOR_ALIGN);
    int walign = offs ? epx_min_int((offs/4), width) : 0;
    SIMD_AREA_LOCAL_DECL

    while(height--) {
	uint8_t* dst1 = dst;
	unsigned int width1 = width;

	if (width1 < 4)
	    SIMD_AREA_UNALIGNED(dst1,width1);
	else {
	    if (walign) {
		SIMD_AREA_UNALIGNED(dst1,walign);
		dst1   += offs;
		width1 -= walign;
	    }
	    while(width1 >= EPX_SIMD_VECTOR_SIZE/4) {
		epx_vector_u8_t td = epx_simd_vector_load(dst1);
		td = SIMD_AREA_OPERATION(td);
		epx_simd_vector_store(dst1, td);
		dst1 += EPX_SIMD_VECTOR_SIZE;
		width1 -= EPX_SIMD_VECTOR_SIZE/4;
	    }
	    if (width1)
		SIMD_AREA_UNALIGNED(dst1,width1);
	}
	dst += dst_wb;
    }
    epx_simd_empty_state();
}

		   
