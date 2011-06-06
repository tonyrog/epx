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

typedef struct _epx_anim_pixels_t {
    uint8_t    type;       // Draw type 
    uint8_t    itype;      // Indirect Draw type
    uint16_t   nblocks;    // #linked blocks (mType==EPX_ANIM_INDIRECT) 
    uint32_t   count;      // # of pixels | offset INDIRECT
} epx_anim_pixels_t;

// Pixmap animation interface
extern void epx_anim_copy_frame(epx_gc_t* gc, epx_pixmap_t* pic, int x, int y,
				int width, int height, epx_format_t src_pt,
				epx_anim_pixels_t* base,
				epx_anim_pixels_t* current);
extern void epx_anim_draw_frame(epx_gc_t* gc, epx_pixmap_t* pic, int x, int y,
				int width, int height, epx_format_t src_pt,
				epx_anim_pixels_t* base,
				epx_anim_pixels_t* current);

#endif
