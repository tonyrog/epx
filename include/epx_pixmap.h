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
// epx_pixmap.h
//
//   EPX pixmap api
//
#ifndef __EPX_PIXMAP_H__
#define __EPX_PIXMAP_H__

#include "epx_geometry.h"
#include "epx_object.h"
#include "epx_pixel.h"
#include "epx_simd.h"

typedef struct _epx_filter_ {
    epx_dimension_t wh;
    uint32_t fsum;       // sum(abs(factor[i]))
    uint8_t* factor;     // factors 
    uint8_t  data[1];    // factor if stored 
} epx_filter_t;

// #define EPX_ALIGNMENT  8   // MMX
#define EPX_ALIGNMENT  16  // SSE2
//#define EPX_ALIGNMENT  32  // AVX2

#define EPX_PIXEL_ADDR(map,x,y) \
    ((map)->data + (((y)*(int)(map)->bytes_per_row) + ((x)*(int)(map)->bytes_per_pixel)))

// align pointer (a MUST be a power of 2) (and constant ;-) 
#define EPX_ALIGN_OFFS(p,a) (((a) - (((uintptr_t)p) % (a))) % (a))
#define EPX_ALIGN(p,a) ((void*)(((uint8_t*)p)+EPX_ALIGN_OFFS(p,a)))

struct _epx_backend_t;

typedef struct _epx_pixmap_functions_t {
    /*! Pixel unpack function */
    epx_pixel_unpack_t unpack;
    /*! Pixel pack function */
    epx_pixel_pack_t pack;
    /*! Fill area with blending */
    void (*fill_area_blend)(uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			    epx_pixel_t p, 
			    unsigned int width, unsigned int height);
    /* ! Blend source and destination pixmaps */
    void (*blend_area)(uint8_t* src, int src_wb, epx_format_t src_pt,
		       uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		       unsigned int width, unsigned int height);

    /* ! Blend source and destination pixmaps using fixed alpha a */
    void (*alpha_area)(uint8_t* src, int src_wb, epx_format_t src_pt,
		       uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		       uint8_t alpha, unsigned int width, unsigned int height);
    /* ! Fade area */
    void (*fade_area)(uint8_t* src, int src_wb, epx_format_t src_pt,
		      uint8_t* dst, int dst_wb, epx_format_t dst_pt,
		      uint8_t fade, unsigned int width, unsigned int height);

} epx_pixmap_functions_t;

typedef struct _epx_pixmap_t {
    EPX_OBJECT_MEMBERS(struct _epx_pixmap_t);
    struct _epx_backend_t* backend;  /* backend pointer if attached */
    struct _epx_pixmap_t* parent;         // parent pixmap (for sub_pixmap)
    void* user;                           // extra user data
    /*! Clip rectangle restrict pixels drawn within boundary */
    epx_rect_t clip;
    /*! Width of pixmap in pixels  */
    unsigned int width;
    /*! Width of pixmap in bytes   */   
    unsigned int bytes_per_row;
    /*! Height of pixmap in pixels */
    unsigned int height;
    /*! Size of pixel in bits, including pad */
    unsigned int bits_per_pixel;
    /*! Pixel type used for pixmap */
    epx_format_t pixel_format;
    /*! Pixel functions */
    epx_pixmap_functions_t func;
    /*! Number of bytes per pixel, may also be calculated from pixel_format */
    unsigned int bytes_per_pixel;
    /*! Size of pixel area in bytes */
    size_t      sz;
    /*! Unaligned pixel data, DO NOT USED */
    uint8_t* data0;            
    /*! 16 byte aligned pixel data */
    uint8_t* data;
} epx_pixmap_t;

// General pixmap flags 
#define EPX_FLAG_NONE                 0x00000000
#define EPX_FLAG_SOLID                0x00000001
#define EPX_FLAG_BLEND                0x00000002
#define EPX_FLAG_SUM                  0x00000004
#define EPX_FLAG_AALIAS               0x00000008
#define EPX_FLAG_TEXTURED             0x00000010
#define EPX_FLAG_NE12                 0x00000020  // don't draw edge v1 - v2
#define EPX_FLAG_NE23                 0x00000040  // don't draw edge v2 - v3
#define EPX_FLAG_NE31                 0x00000080  // don't draw edge v3 - v1
#define EPX_FLAG_NE34                 0x00000100  // don't draw edge v3 - v4
#define EPX_FLAG_NE41                 0x00000200  // don't draw edge v4 - v1
#define EPX_FLAG_NV1                  0x00001000  // don't draw vertex v1
#define EPX_FLAG_NV2                  0x00002000  // don't draw vertex v2
#define EPX_FLAG_NV3                  0x00004000  // don't draw vertex v3
#define EPX_FLAG_NV4                  0x00008000  // don't draw vertex v4
#define EPX_FLAG_NFIRST               EPX_FLAG_NV1 // dont draw first pixel
#define EPX_FLAG_NLAST                EPX_FLAG_NV2 // dont draw last pixel

typedef uint32_t epx_flags_t;

extern int epx_pixmap_init(epx_pixmap_t* dst,
			   unsigned int width, unsigned int height,
			   epx_format_t fmt);
extern int epx_pixmap_init_copy(epx_pixmap_t* src, epx_pixmap_t* dst);

extern int epx_pixmap_init_sub_pixmap(epx_pixmap_t* src, epx_pixmap_t* dst, 
				      int x, int y,
				      unsigned int width, unsigned int height);


extern epx_pixmap_t* epx_pixmap_create(unsigned int width, unsigned int height,
				       epx_format_t fmt);
extern int epx_pixmap_set_format(epx_pixmap_t*, epx_format_t fmt);

extern epx_pixmap_t* epx_pixmap_copy(epx_pixmap_t* src);
extern epx_pixmap_t* epx_pixmap_sub_pixmap(epx_pixmap_t* src,
					   int x, int y,
					   unsigned int width,
					   unsigned int height);

extern void epx_pixmap_destroy(epx_pixmap_t* pixmap);

extern void epx_pixmap_copy_to(epx_pixmap_t* pixmap, epx_pixmap_t* dst);
extern void epx_pixmap_set_clip(epx_pixmap_t* pixmap, epx_rect_t* clip);
extern void epx_pixmap_fill(epx_pixmap_t* pixmap, epx_pixel_t color);
extern void epx_pixmap_fill_blend(epx_pixmap_t* pixmap, epx_pixel_t color);
extern void epx_pixmap_fill_area(epx_pixmap_t* pixmap,
				 int x, int y,
				 unsigned int width, unsigned int height,
				 epx_pixel_t color, epx_flags_t flags);
extern void epx_pixmap_scale(epx_pixmap_t* src, epx_pixmap_t* dst, 
			     unsigned int width, unsigned int height);
extern void epx_pixmap_scale_area(epx_pixmap_t* src, epx_pixmap_t* dst,
				  int x_src, int y_src,
				  int x_dst, int y_dst,
				  unsigned int w_src, unsigned int h_src,
				  unsigned int w_dst, unsigned int h_dst,
				  epx_flags_t flags);
extern void epx_pixmap_flip(epx_pixmap_t* pic);
extern void epx_pixmap_put_pixel(epx_pixmap_t* pic, int x, int y,
				 epx_flags_t flags, epx_pixel_t px);
extern void epx_pixmap_put_pixels(epx_pixmap_t* pic, int x, int y,
				  unsigned int width,  unsigned int height,
				  epx_format_t pixel_format, 
				  epx_flags_t flags,
				  void* data, unsigned int len);
extern epx_pixel_t epx_pixmap_get_pixel(epx_pixmap_t* pic, int x, int y);
extern epx_pixel_t epx_pixmap_read_pixel(int pixel_type, uint8_t* src);
extern void epx_pixmap_write_pixel(int pixel_type, epx_pixel_t p, uint8_t* dst);

// Copy, this will copy src to dest with blending
extern void epx_pixmap_copy_area(epx_pixmap_t* src, epx_pixmap_t* dst,
				 int x_src, int y_src, int x_dst, int y_dst,
				 unsigned int width, unsigned int height,
				 epx_flags_t flags);

// Like copy but will use a fixed alpha to blend pixmaps 
extern void epx_pixmap_alpha_area(epx_pixmap_t* src, epx_pixmap_t* dst,
				  uint8_t alpha,
				  int x_src, int y_src, int x_dst, int y_dst,
				  unsigned int width, unsigned int height);

// Like copy but will use a fader multiplier for alpha to blend pixmaps */
extern void epx_pixmap_fade_area(epx_pixmap_t* src,epx_pixmap_t* dst, 
				 uint8_t fade,
				 int x_src, int y_src, int x_dst, int y_dst,
				 unsigned int width, unsigned int height);

extern void epx_pixmap_shadow_area(epx_pixmap_t* src,epx_pixmap_t* dst,
				   int x_src, int y_src, int x_dst, int y_dst,
				   unsigned int width, unsigned int height,
				   epx_flags_t flags);

extern void epx_pixmap_add_color_area(epx_pixmap_t* src,epx_pixmap_t* dst,
				      uint8_t fade,
				      epx_pixel_t color,
				      int x_src,int y_src,int x_dst,int y_dst,
				      unsigned int width, unsigned int height, 
				      epx_flags_t flags);


extern void epx_pixmap_filter_area(epx_pixmap_t* src, epx_pixmap_t* dst,
				   epx_filter_t* flt,
				   int x_src, int y_src, int x_dst, int y_dst,
				   unsigned int width, unsigned int height,
				   epx_flags_t flags);

extern void epx_pixmap_rotate_area(epx_pixmap_t* src, epx_pixmap_t* dst, 
				   float angle,
				   int x_src, int y_src, int xc_src, int yc_src,
				   int xc_dst, int yc_dst,
				   unsigned int width, unsigned int height, 
				   epx_flags_t flags);

extern void epx_pixmap_scroll_left(epx_pixmap_t* src, epx_pixmap_t* dst,
				   int rotate, unsigned int amount, 
				   epx_pixel_t fill);
extern void epx_pixmap_scroll_right(epx_pixmap_t* src, epx_pixmap_t* dst, 
				    int rotate, unsigned int amount, 
				    epx_pixel_t fill);
extern void epx_pixmap_scroll_up(epx_pixmap_t* src, epx_pixmap_t* dst, 
				 int rotate, unsigned int amount, 
				 epx_pixel_t fill);
extern void epx_pixmap_scroll_down(epx_pixmap_t* src, epx_pixmap_t* dst, 
				   int rotate, unsigned int amount, 
				   epx_pixel_t fill);

extern void epx_pixmap_scroll(epx_pixmap_t* src, epx_pixmap_t* dst, 
			      int horizontal, int vertical, 
			      int rotate, epx_pixel_t fill);

extern epx_pixel_t epx_pixel_operation(epx_pixel_operation_t op,
				       epx_pixel_t a, epx_pixel_t b);

extern void epx_pixmap_operation_area(epx_pixmap_t* src,epx_pixmap_t* dst, 
				      epx_pixel_operation_t op,
				      int x_src, int y_src, int x_dst, int y_dst,
				      unsigned int width, unsigned int height);

// Low level byte interface
extern void epx_copy_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			  uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			  unsigned int width, unsigned int height);

extern void epx_fill_area(uint8_t* dst, int dst_wb, epx_format_t dst_pt, 
			  epx_pixel_t fill,
			  unsigned int width, unsigned int height);

extern void epx_fill_area_blend(uint8_t* dst, int dst_wb, epx_format_t dst_pt, 
				epx_pixel_t p,
				unsigned int width, unsigned int height);

extern void epx_shade_area(uint8_t* dst, int dst_wb, epx_format_t dst_pt, 
			   unsigned int width, unsigned int height, 
			   epx_pixel_t Px0, epx_pixel_t Px1,
			   epx_pixel_t Py0, epx_pixel_t Py1);

extern void epx_blend_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			   uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			   unsigned int width, 
			   unsigned int height);

extern void epx_sum_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			 uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			 unsigned int width, 
			 unsigned int height);

extern void epx_alpha_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			   uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			   uint8_t alpha, 
			   unsigned int width, unsigned int height);

extern void epx_fade_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			  uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			  uint8_t fade, 
			  unsigned int width, unsigned int height);

extern void epx_shadow_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			    uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			    unsigned int width, unsigned int height,
			    epx_flags_t flags);

extern void epx_add_color_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			       uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			       uint8_t fader, epx_pixel_t color,
			       unsigned int width, unsigned int height,
			       epx_flags_t flags);

extern void epx_binop_area(uint8_t* src, int src_wb, epx_format_t src_pt,
			   uint8_t* dst, int dst_wb, epx_format_t dst_pt,
			   epx_pixel_binary_op_t binop,
			   unsigned int width, 
			   unsigned int height);

extern epx_pixel_t epx_interp(epx_pixmap_t* pic, float x, float y);

// inline 'row' version of area functions 
static inline void epx_copy_row(uint8_t* src, epx_format_t src_pt,
				uint8_t* dst, epx_format_t dst_pt,
				int width)
{
    epx_copy_area(src, 0, src_pt, dst, 0, dst_pt, width, 1);
}

static inline void epx_fill_row(uint8_t* dst,int dst_pt,
				 epx_pixel_t fill, unsigned int width)
{
    epx_fill_area(dst, 0, dst_pt, fill, width, 1);
}

static inline void epx_fill_row_blend(uint8_t* dst, epx_format_t dst_pt,
				      epx_pixel_t p, unsigned int width)
{
    epx_fill_area_blend(dst, 0, dst_pt, p, width, 1);
}

static inline void epx_blend_row(uint8_t* src, epx_format_t src_pt,
				 uint8_t* dst, epx_format_t dst_pt,
				 unsigned int width)
{
    epx_blend_area(src, 0, src_pt, dst, 0, dst_pt, width, 1);
}

static inline void epx_sum_row(uint8_t* src, epx_format_t src_pt,
			       uint8_t* dst, epx_format_t dst_pt,
			       unsigned int width)
{
    epx_sum_area(src, 0, src_pt, dst, 0, dst_pt, width, 1);
}

static inline void epx_alpha_row(uint8_t* src, epx_format_t src_pt,
				 uint8_t* dst, epx_format_t dst_pt,
				 uint8_t a, unsigned int width)
{
    epx_alpha_area(src, 0, src_pt, dst, 0, dst_pt, a, width, 1);
}

static inline void epx_fade_row(uint8_t* src, epx_format_t src_pt,
				uint8_t* dst, epx_format_t dst_pt,
				uint8_t fade, unsigned int width)
{
    if (fade == ALPHA_FACTOR_0)
	return;
    else if (fade == ALPHA_FACTOR_1)
	epx_blend_area(src, 0, src_pt, dst, 0, dst_pt, width, 1);
    else
	epx_fade_area(src, 0, src_pt, dst, 0, dst_pt, fade, width, 1);
}

static inline void epx_shadow_row(uint8_t* src, epx_format_t src_pt,
		    uint8_t* dst, epx_format_t dst_pt,
		    unsigned int width, epx_flags_t flags)
{
    epx_shadow_area(src, 0, src_pt, dst, 0, dst_pt, width, 1, flags);
}


static inline void epx_add_color_row(uint8_t* src, epx_format_t src_pt,
				     uint8_t* dst, epx_format_t dst_pt,
				     uint8_t fade, epx_pixel_t color,
				     unsigned int width,
				     epx_flags_t flags)
{
    epx_add_color_area(src, 0, src_pt, dst, 0, dst_pt, 
		       fade, color, width, 1, flags);
}


#endif
