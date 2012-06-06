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
#ifndef __EPX_ANIM_H__
#define __EPX_ANIM_H__

#include "epx_pixmap.h"
#include "epx_gc.h"

#define EPX_ANIM_SKIP      0x01  // skip count DestPixelType pixels
#define EPX_ANIM_RGBA      0x02  // count * RGBA data
#define EPX_ANIM_COPY      0x03  // count * SourcePixelType pixels
#define EPX_ANIM_SHADOW    0x04  // count * ALPHA data
#define EPX_ANIM_INDIRECT  0x05  // count * indirect blocks
#define EPX_ANIM_BGRA      0x06  // count * BGRA data
#define EPX_ANIM_FILL      0x07  // fill count*color (ARGB format) pixels

typedef struct {
    u_int32_t  version;
    u_int32_t  image_count;
    u_int32_t  height;
    u_int32_t  width;
    u_int32_t  pixel_format;
} epx_animation_header_t;

typedef struct _epx_animation_t {
    EPX_OBJECT_MEMBERS(struct _epx_animation_t);
    char*         file_name;
    int*          offset_array;
    uint8_t*      mapped_data;
    off_t         mapped_size;
    epx_animation_header_t hdr;
} epx_animation_t;


typedef struct _epx_anim_pixels_t {
    uint8_t    type;       // Draw type 
    uint8_t    itype;      // Indirect Draw type
    uint16_t   nblocks;    // #linked blocks (mType==EPX_ANIM_INDIRECT) 
    uint32_t   count;      // # of pixels | offset INDIRECT
} epx_anim_pixels_t;

// Pixmap animation interface

extern void             epx_anim_init(epx_animation_t* anim);
extern void             epx_anim_cleanup(epx_animation_t* anim);
extern epx_animation_t* epx_anim_create(void);
extern void             epx_anim_destroy(epx_animation_t* anim);
extern int              epx_anim_open_init(epx_animation_t* anim, char* path);

extern epx_anim_pixels_t* epx_anim_get_pixels(epx_animation_t* anim, int index);

extern void epx_anim_copy_frame(epx_pixmap_t* pic, epx_gc_t* gc, int x, int y,
				int width, int height, epx_format_t src_pt,
				epx_anim_pixels_t* base,
				epx_anim_pixels_t* current);
extern void epx_anim_draw_frame(epx_pixmap_t* pic, epx_gc_t* gc, int x, int y,
				int width, int height, epx_format_t src_pt,
				epx_anim_pixels_t* base,
				epx_anim_pixels_t* current);


#endif
