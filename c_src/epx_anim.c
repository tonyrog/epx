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
 * EPX Animation frame drawing
 *
 */

#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>


#include "epx_simd_int.h"
#include "../include/epx_pixmap.h"
#include "../include/epx_anim.h"
#include "../include/epx_colors.h"
#include "../include/epx_simd.h"

void epx_anim_init(epx_animation_t* anim)
{
    EPX_OBJECT_INIT(anim, EPX_ANIM_TYPE);

    memset(&anim->hdr, 0, sizeof(epx_animation_header_t));
    anim->file_name = NULL;
    anim->offset_array = NULL;
    anim->mapped_data = NULL;
    anim->mapped_size = 0;
}

void epx_anim_cleanup(epx_animation_t* anim)
{
    if (anim->file_name) {
	free(anim->file_name);
	anim->file_name = NULL;
    }
    if (anim->offset_array) {
	free(anim->offset_array);
	anim->offset_array = NULL;
    }
    if (anim->mapped_data) {
	munmap(anim->mapped_data, anim->mapped_size);
	anim->mapped_data = NULL;
	anim->mapped_size = 0;
    }
}

epx_animation_t* epx_anim_create()
{
    epx_animation_t* anim;

    if (!(anim = (epx_animation_t*) malloc(sizeof(epx_animation_t))))
	return 0;
    epx_anim_init(anim);
    anim->on_heap = 1;
    anim->refc = 1;
    return anim;
}

int epx_anim_open_init(epx_animation_t* anim, char* path)
{
    int fd;
    struct stat cache_stat;
    size_t sz;

    epx_anim_init(anim);

    if ((fd = open(path, O_RDONLY)) < 0)
	return -1;
    sz = sizeof(epx_animation_header_t);
    if (read(fd, (char *) &anim->hdr, sz) != (int) sz) {
	close(fd);
	return -EINVAL;
    }
    anim->file_name = strdup(path);
    // fixme: make header little endian ?

    if ((anim->hdr.height<1) || (anim->hdr.height>2048))
	goto error;

    if ((anim->hdr.width < 1) || (anim->hdr.width > 4096))
	goto error;

    if ((anim->hdr.image_count < 1)||(anim->hdr.image_count>10000))
	goto error;

    lseek(fd, 0, SEEK_CUR);

    // Read the offset table.
    sz = sizeof(int) * anim->hdr.image_count;
    anim->offset_array = malloc(sz);

    if (read(fd, (char *) anim->offset_array, sz) != (int)sz)
	goto error;

    lseek(fd, 0, SEEK_CUR);
    fstat(fd, &cache_stat);

    anim->mapped_data = (uint8_t*) mmap(0,
					cache_stat.st_size,
					PROT_READ, MAP_SHARED, 
					fd, 0);
    anim->mapped_size = cache_stat.st_size;
    close(fd);
    fd = -1;

    if (anim->mapped_data == (uint8_t*) 0xFFFFFFFF)
	goto error;
    return 0;

error:
    if (fd >= 0)
	close(fd);
    epx_anim_cleanup(anim);
    return -EINVAL;
}

void EPX_ANIM_TYPE_RELEASE(void* arg)
{
    epx_animation_t* anim = (epx_animation_t*) arg;
    EPX_DBGFMT_MEM("EPX_ANIM_TYPE_RELEASE: %p", arg);
    epx_anim_cleanup(anim);
    if (anim->on_heap)
	free(anim);
}

void epx_anim_destroy(epx_animation_t* anim)
{
    epx_object_unref(anim);
}


epx_anim_pixels_t* epx_anim_get_pixels(epx_animation_t* anim, int index)
{
    uint8_t* base;
    uint8_t* ptr0;
    // uint8_t* ptr1;
    int n = anim->hdr.image_count;

    if ((index < 0) || (index >= n))
	return 0;

    base = anim->mapped_data + sizeof(epx_animation_header_t) + sizeof(int)*n;
    ptr0 = base + anim->offset_array[index];
//    if (index+1 == n)    // point after end of file
//	ptr1 = anim->mapped_data + anim->mapped_size;
//    else
//	ptr1 = base + anim->offset_array[index+1];
    return (epx_anim_pixels_t*) ptr0;
}

/* Copy animation frame to background */
void epx_anim_copy_frame(epx_pixmap_t* pic,  epx_gc_t* gc, int x, int y,
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
    int srcPixelSize = EPX_PIXEL_BYTE_SIZE(src_pt);
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
void epx_anim_draw_frame(epx_pixmap_t* pic, epx_gc_t* gc, int x, int y,
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
    int srcPixelSize = EPX_PIXEL_BYTE_SIZE(src_pt);
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
		epx_fade_row(src,EPX_FORMAT_RGBA, // BGRA,
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

