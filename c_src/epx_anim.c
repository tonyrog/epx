/*
 * EPX Animation frame drawing
 *
 */

#include <stdlib.h>
#include <memory.h>
#include <math.h>

#include "epx_simd_int.h"
#include "epx_pixmap.h"
#include "epx_anim.h"
#include "epx_colors.h"
#include "epx_simd.h"


/* Copy animation frame to background */
void epx_anim_copy_frame(epx_gc_t* gc, epx_pixmap_t* pic, int x, int y,
			 int width, int height, epx_format_t src_pt,
			 epx_anim_pixels_t* base, 
			 epx_anim_pixels_t* current)
{
    (void) gc;
    u_int8_t *src = (u_int8_t*) current;
    u_int8_t* src_save = NULL;
    int n_blocks = 0;
    unsigned char *dst0 = EPX_PIXEL_ADDR(pic, x, y);
    int dst_wb = pic->bytes_per_row;
    epx_format_t src_format = src_pt;
    int srcPixelSize = EPX_PIXEL_SIZE(src_pt);
    epx_format_t dst_format = pic->pixel_format;
    int dstPixelSize = pic->bytes_per_pixel;

    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) src;
	    u_int32_t pcount;

	    if (hdr->type == EPX_ANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(epx_anim_pixels_t); // continue point
		src = isrc;
		hdr = (epx_anim_pixels_t*) src; // switch to indirect header
	    }

	    src += sizeof(epx_anim_pixels_t);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EPX_ANIM_SKIP:
		break;

	    case EPX_ANIM_SHADOW:
		epx_copy_row(src, EPX_FORMAT_A8,
			     dst, dst_format, pcount);
		src  += pcount;
		break;

	    case EPX_ANIM_RGBA:
		epx_copy_row(src,EPX_FORMAT_RGBA,
			     dst, dst_format, pcount);
		src += pcount*4;
		break;

	    case EPX_ANIM_BGRA:
		epx_copy_row(src,EPX_FORMAT_BGRA,
			       dst, dst_format, pcount);
		src += pcount*4;
		break;
		
	    case EPX_ANIM_COPY: // BGRA values!!!
		epx_copy_row(src, src_format,
			     dst, dst_format,
			     pcount);
		src += pcount*srcPixelSize;
		break;

	    case EPX_ANIM_FILL:
		epx_fill_row(dst, dst_format, 
			     epx_pixel_argb(src[0],src[1],src[2],src[3]),
			     pcount);
		src += 4;
		break;
	    default: 
		break;
	    }
	    dst += pcount*dstPixelSize;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
}

/* Draw & Blend animation frame with background */
void epx_anim_draw_frame(epx_gc_t* gc, epx_pixmap_t* pic, int x, int y,
			 int width, int height, epx_format_t src_pt,
			 epx_anim_pixels_t* base,
			 epx_anim_pixels_t* current)
{
    u_int8_t fader = gc->fader_value;
    u_int8_t *src = (u_int8_t*) current;
    u_int8_t* src_save = NULL;
    int n_blocks = 0;
    unsigned char *dst0 = EPX_PIXEL_ADDR(pic, x, y);
    int dst_wb = pic->bytes_per_row;
    epx_format_t src_format = src_pt;
    int srcPixelSize = EPX_PIXEL_SIZE(src_pt);
    epx_format_t dst_format = pic->pixel_format;
    int dstPixelSize = pic->bytes_per_pixel;
    epx_rect_t r = {{x,y}, {width,height}};
    int x0, yi;
    int clip_y0, clip_y1;
    int clip_x0, clip_x1, clip_len;
    int need_x_clip;
    
    if (epx_rect_is_subrect(&r, &pic->clip)) {
	if (src_format == dst_format) {
	    if (dst_format == EPX_FORMAT_BGRA)
		goto bgra_noclip;
	    else if (dst_format == EPX_FORMAT_RGBA)
		goto rgba_noclip;
	    goto no_clip;
	}
    }
    goto clip;

    /* if we must clip the image we do the general stuff */

clip:
    clip_y0  = epx_rect_top(&pic->clip);
    clip_y1  = epx_rect_bottom(&pic->clip);
    clip_x0  = epx_rect_left(&pic->clip);
    clip_x1  = epx_rect_right(&pic->clip);
    clip_len = epx_rect_width(&pic->clip);

    yi = y;
    x0 = x;
    need_x_clip = !( (x0 >= clip_x0) && ((x0+width-1) <= clip_x1) );

    while(height--) {
	int xi    = x0;
	int w = width;

	if (!epx_in_range(yi, clip_y0, clip_y1)) {
	    // The line is clipped but we must skip the source data
	    while(w) {
		epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) src;
		u_int32_t pcount;

		if (hdr->type == EPX_ANIM_INDIRECT) {
		    u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		    n_blocks = hdr->nblocks;   // number of blocks to run 
		    src_save = src + sizeof(epx_anim_pixels_t); // continue point
		    src = isrc;
		    hdr = (epx_anim_pixels_t*) src; // switch to indirect header
		}
		src += sizeof(epx_anim_pixels_t);
		pcount = hdr->count;
		w -= pcount;

		switch(hdr->type) {
		case EPX_ANIM_SKIP:
		    break;
		case EPX_ANIM_SHADOW:
		    src  += pcount;
		    break;
		case EPX_ANIM_BGRA:
		case EPX_ANIM_RGBA:
		    src += pcount*4;
		    break;
		case EPX_ANIM_COPY:
		    src += pcount*srcPixelSize;
		    break;
		case EPX_ANIM_FILL:
		    src += 4;
		    break;
		default:
		    break;
		}
		if (n_blocks) {  // are we running indirect blocks?
		    if (n_blocks == 1)
			src = src_save;   // restore & continue
		    n_blocks--;
		}
	    }
	}
	else if (!need_x_clip) {  // No horizontal clipping needed
	    unsigned char *dst = EPX_PIXEL_ADDR(pic, xi, yi);
	    while(w) {
		epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) src;
		u_int32_t pcount;
		u_int8_t a;

		if (hdr->type == EPX_ANIM_INDIRECT) {
		    u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		    n_blocks = hdr->nblocks;   // number of blocks to run 
		    src_save = src + sizeof(epx_anim_pixels_t); // continue point
		    src = isrc;
		    hdr = (epx_anim_pixels_t*) src; // switch to indirect header
		}

		src += sizeof(epx_anim_pixels_t);
		pcount = hdr->count;
		w -= pcount;

		switch(hdr->type) {
		case EPX_ANIM_SKIP:
		    break;

		case EPX_ANIM_SHADOW:
		    epx_add_color_row(src, EPX_FORMAT_A8,
				      dst, dst_format,
				      fader, epx_pixel_transparent,
				      pcount, EPX_FLAG_BLEND);
		    src  += pcount;
		    break;

		case EPX_ANIM_BGRA:
		    epx_fade_row(src,EPX_FORMAT_BGRA,
				   dst, dst_format, fader, pcount);
		    src += pcount*4;
		    break;

		case EPX_ANIM_RGBA:
		    epx_fade_row(src,EPX_FORMAT_RGBA,
				   dst, dst_format, fader, pcount);
		    src += pcount*4;
		    break;
		    
		case EPX_ANIM_COPY:
		    if (fader == ALPHA_FACTOR_1)
			epx_copy_row(src, src_format,
				       dst, dst_format,
				       pcount);
		    else if (fader != ALPHA_FACTOR_0)
			epx_alpha_row(src, src_format,
				      dst, dst_format, 
				      fader, pcount);
		    src += pcount*srcPixelSize;
		    break;

		case EPX_ANIM_FILL:
		    a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		    if (a == EPX_ALPHA_TRANSPARENT)
			;
		    else if (a == EPX_ALPHA_OPAQUE)
			epx_fill_row(dst, dst_format,
				     epx_pixel_argb(a,src[1],src[2],src[3]),
				     pcount);
		    else
			epx_fill_row_blend(dst, dst_format,
					   epx_pixel_argb(a,src[1],
							  src[2],src[3]),
					   pcount);
		    src += 4;
		    break;
		    
		default:
		    break;
		}

		dst += pcount*dstPixelSize;
		
		if (n_blocks) {  // are we running indirect blocks?
		    if (n_blocks == 1)
			src = src_save;   // restore & continue
		    n_blocks--;
		}
	    }
	}
	else {  // we need to clip dds
	    unsigned char *dst = EPX_PIXEL_ADDR(pic, xi, yi);

	    while(w) {
		epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) src;
		u_int32_t pcount;
		u_int8_t a;

		if (hdr->type == EPX_ANIM_INDIRECT) {
		    u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		    n_blocks = hdr->nblocks;   // number of blocks to run 
		    src_save = src + sizeof(epx_anim_pixels_t); // continue point
		    src = isrc;
		    hdr = (epx_anim_pixels_t*) src; // switch to indirect header
		}

		src += sizeof(epx_anim_pixels_t);
		pcount = hdr->count;
		w -= pcount;

		switch(hdr->type) {
		case EPX_ANIM_SKIP:
		    dst  += pcount*dstPixelSize;
		    xi   += pcount;
		    break;

		case EPX_ANIM_SHADOW: {
		    u_int8_t      *src1 = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
			
		    n = epx_intersect_segments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs;
			    dst1 += offs*dstPixelSize;
			}
			epx_add_color_row(src1, EPX_FORMAT_A8,
					  dst1, dst_format,
					  fader, epx_pixel_transparent,
					  n, EPX_FLAG_BLEND);
		    }
		    dst  += pcount*dstPixelSize;
		    src  += pcount;
		    xi   += pcount;
		    break;
		}

		case EPX_ANIM_RGBA: {
		    u_int8_t      *src1 = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = epx_intersect_segments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs*4;
			    dst1 += offs*dstPixelSize;
			}
			epx_fade_row(src1,EPX_FORMAT_RGBA,
				     dst1, dst_format, fader, n);
		    }
		    dst += pcount*dstPixelSize;
		    src += pcount*4;
		    xi  += pcount;
		    break;
		}

		case EPX_ANIM_BGRA: {
		    u_int8_t      *src1 = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = epx_intersect_segments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs*4;
			    dst1 += offs*dstPixelSize;
			}
			epx_fade_row(src1,EPX_FORMAT_BGRA,
				       dst1, dst_format, fader, n);
		    }
		    dst += pcount*dstPixelSize;
		    src += pcount*4;
		    xi  += pcount;
		    break;
		}
		    
		case EPX_ANIM_COPY: {
		    u_int8_t *src1      = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = epx_intersect_segments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs*srcPixelSize;
			    dst1 += offs*dstPixelSize;
			}
			if (fader == ALPHA_FACTOR_1)
			    epx_copy_row(src1, src_format,
					 dst1, dst_format,
					 n);
			else if (fader != ALPHA_FACTOR_0)
			    epx_alpha_row(src1, src_format,
					  dst1, dst_format, 
					  fader, n);
		    }
		    dst += pcount*dstPixelSize;
		    src += pcount*srcPixelSize;
		    xi  += pcount;
		    break;
		}

		case EPX_ANIM_FILL: {
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = epx_intersect_segments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    dst1 += offs*dstPixelSize;
			}
			a= (fader==ALPHA_FACTOR_1)?src[0]:((src[0]*fader)>>8);
			if (a == EPX_ALPHA_TRANSPARENT)
			    ;
			else if (a == EPX_ALPHA_OPAQUE)
			    epx_fill_row(dst1, dst_format,
					 epx_pixel_argb(a,src[1],src[2],src[3]), n);
			else
			    epx_fill_row_blend(dst1, dst_format,
					       epx_pixel_argb(a,src[1],src[2],src[3]),
					       n);
		    }
		    dst += pcount*dstPixelSize;
		    src += 4;
		    xi  += pcount;
		    break;
		}

		    
		default:
		    break;
		}
		
		if (n_blocks) {  // are we running indirect blocks?
		    if (n_blocks == 1)
			src = src_save;   // restore & continue
		    n_blocks--;
		}
	    }
	}
	yi++;
    }
    return;

/*
 *  General anim block draw when no clipping is needed 
 *  
 */
no_clip:
    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) src;
	    u_int32_t pcount;
	    u_int8_t a;

	    if (hdr->type == EPX_ANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(epx_anim_pixels_t); // continue point
		src = isrc;
		hdr = (epx_anim_pixels_t*) src; // switch to indirect header
	    }

	    src += sizeof(epx_anim_pixels_t);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EPX_ANIM_SKIP:
		break;

	    case EPX_ANIM_SHADOW:
		epx_add_color_row(src, EPX_FORMAT_A8,
				   dst, dst_format,
				   fader, epx_pixel_transparent,
				   pcount, EPX_FLAG_BLEND);
		src  += pcount;
		break;

	    case EPX_ANIM_RGBA:
		epx_fade_row(src,EPX_FORMAT_BGRA,
			       dst, dst_format, fader, pcount);
		src += pcount*4;
		break;

	    case EPX_ANIM_BGRA:
		epx_fade_row(src,EPX_FORMAT_BGRA,
			     dst, dst_format, fader, pcount);
		src += pcount*4;
		break;
		
	    case EPX_ANIM_COPY:
		if (fader == ALPHA_FACTOR_1)
		    epx_copy_row(src, src_format,
				   dst, dst_format, pcount);
		else if (fader != ALPHA_FACTOR_0)
		    epx_alpha_row(src, src_format,
				    dst, dst_format, fader, pcount);
		src += pcount*srcPixelSize;
		break;

	    case EPX_ANIM_FILL:
		a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		if (a == EPX_ALPHA_TRANSPARENT)
		    ;
		else if (a == EPX_ALPHA_OPAQUE)
		    epx_fill_row(dst, dst_format,
				 epx_pixel_argb(a,src[1],src[2],src[3]),
				 pcount);
		else
		    epx_fill_row_blend(dst, dst_format,
				       epx_pixel_argb(a,src[1],src[2],src[3]),
				       pcount);
		src += 4;
		break;

	    default: 
		break;
	    }
	    dst += pcount*dstPixelSize;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
    return;
/*
 * BGRA anim block draw when no clipping is needed 
 *   both src_format and dst_format is BGRA
 */
bgra_noclip:
    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) src;
	    u_int32_t pcount;
	    u_int8_t  a;

	    if (hdr->type == EPX_ANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(epx_anim_pixels_t); // continue point
		src = isrc;
		hdr = (epx_anim_pixels_t*) src; // switch to indirect header
	    }

	    src += sizeof(epx_anim_pixels_t);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EPX_ANIM_SKIP:
		break;

	    case EPX_ANIM_SHADOW:
		epx_add_color_row(src, EPX_FORMAT_A8,
				   dst, EPX_FORMAT_BGRA,
				   fader, epx_pixel_transparent,
				   pcount, EPX_FLAG_BLEND);
		src  += pcount;
		break;

	    case EPX_ANIM_BGRA:
		if (fader == ALPHA_FACTOR_0)
		    ;
		else if (fader == ALPHA_FACTOR_1)
		    SIMD_CALL(blend_area_rgba32)(src, 0, dst, 0, pcount, 1);
		else
		    SIMD_CALL(fade_area_rgba32)(src, 0, dst, 0, fader, pcount, 1);
		src += pcount*4;
		break;

	    case EPX_ANIM_RGBA:
		epx_fade_row(src, EPX_FORMAT_RGBA,
			     dst, EPX_FORMAT_BGRA, fader, pcount);
		src += pcount*4;
		break;
		
	    case EPX_ANIM_COPY: // BGRA => BGRA 
		if (fader == ALPHA_FACTOR_1)
		    SIMD_CALL(copy)(src, dst, pcount*4);
		else if (fader != ALPHA_FACTOR_0)
		    SIMD_CALL(alpha_area_rgba32)(src,0,dst,0,fader,pcount,1);
		src += pcount*4;
		break;

	    case EPX_ANIM_FILL:
		a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		if (a == EPX_ALPHA_TRANSPARENT)
		    ;
		else if (a == EPX_ALPHA_OPAQUE) {
		    // FIXME this can be done by calling wmemset directly
		    epx_fill_row(dst, EPX_FORMAT_BGRA,
				 epx_pixel_argb(a,src[1],src[2],src[3]),
				 pcount);
		}
		else /* note! swapping rgb into bgr */
		    SIMD_CALL(fill_area_blend_rgba32)(dst, 0, 
						      epx_pixel_argb(a,src[3],
								     src[2],src[1]),
						      pcount, 1);
		src += 4;
		break;

	    default: 
		break;
	    }
	    dst += pcount*4;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
    return;
/*
 * RGBA anim block draw when no clipping is needed 
 *   both src_format and dst_format is RGBA
 */
rgba_noclip:
    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    epx_anim_pixels_t* hdr = (epx_anim_pixels_t*) src;
	    u_int32_t pcount;
	    u_int8_t  a;

	    if (hdr->type == EPX_ANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(epx_anim_pixels_t); // continue point
		src = isrc;
		hdr = (epx_anim_pixels_t*) src; // switch to indirect header
	    }

	    src += sizeof(epx_anim_pixels_t);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EPX_ANIM_SKIP:
		break;

	    case EPX_ANIM_SHADOW:
		epx_add_color_row(src, EPX_FORMAT_A8,
				  dst, EPX_FORMAT_RGBA,
				  fader, epx_pixel_transparent,
				  pcount, EPX_FLAG_BLEND);
		src  += pcount;
		break;

	    case EPX_ANIM_RGBA:
		if (fader == ALPHA_FACTOR_0)
		    ;
		else if (fader == ALPHA_FACTOR_1)
		    SIMD_CALL(blend_area_rgba32)(src, 0, dst, 0, pcount, 1);
		else
		    SIMD_CALL(fade_area_rgba32)(src, 0, dst, 0, fader, pcount, 1);
		src += pcount*4;
		break;

	    case EPX_ANIM_BGRA:
		epx_fade_row(src, EPX_FORMAT_BGRA,
			       dst, EPX_FORMAT_RGBA, fader, pcount);
		src += pcount*4;
		break;
		
	    case EPX_ANIM_COPY: // RGBA => RBGA 
		if (fader == ALPHA_FACTOR_1)
		    SIMD_CALL(copy)(src, dst, pcount*4);
		else if (fader != ALPHA_FACTOR_0)
		    SIMD_CALL(alpha_area_rgba32)(src,0,dst,0,fader,pcount,1);
		src += pcount*4;
		break;

	    case EPX_ANIM_FILL:
		a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		if (a == EPX_ALPHA_TRANSPARENT)
		    ;
		else if (a == EPX_ALPHA_OPAQUE) {
		    // FIXME this can be done by calling wmemset directly
		    epx_fill_row(dst, EPX_FORMAT_RGBA,
				 epx_pixel_argb(a,src[1],src[2],src[3]),
				 pcount);
		}
		else
		    SIMD_CALL(fill_area_blend_rgba32)(dst, 0,
						      epx_pixel_argb(a,src[1],
								     src[2],src[3]),  pcount, 1);
		src += 4;
		break;

	    default: 
		break;
	    }
	    dst += pcount*4;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
}

