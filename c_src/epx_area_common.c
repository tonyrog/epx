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
// Common AREA functions
// ONLY to be used be epx_pixmap.c as
// #define AREA_PARAMS_DECL
// #define AREA_OPERATION
// #define AREA_NAME
//

void AREA_FUNCTION(uint8_t* src, int src_wb, epx_format_t src_pt,
		   uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		   AREA_PARAMS_DECL
		   unsigned int width, unsigned int height)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    epx_pixel_unpack_t unpack_dst;
    epx_pixel_unpack_t unpack_src;
    epx_pixel_pack_t   pack_dst;

    src_psz = EPX_PIXEL_SIZE(src_pt);
    dst_psz = EPX_PIXEL_SIZE(dst_pt);
    unpack_src = epx_pixel_unpack_func(src_pt);
    unpack_dst = epx_pixel_unpack_func(dst_pt);
    pack_dst   = epx_pixel_pack_func(dst_pt);

    while(height--) {
	uint8_t* src1 = src;
	uint8_t* dst1 = dst;
	unsigned int width1 = width;

	while(width1--) {
	    epx_pixel_t d = unpack_dst(dst1);
	    epx_pixel_t s = unpack_src(src1);
	    d = AREA_OPERATION(s, d);
	    pack_dst(d, dst1);
	    src1 += src_psz;
	    dst1 += dst_psz;
	}
	src += src_wb;
	dst += dst_wb;
    }
}
