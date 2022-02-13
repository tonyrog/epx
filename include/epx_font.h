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
// EPX Font managment
//
#ifndef __EPX_FONT_H__
#define __EPX_FONT_H__

#include "epx_object.h"
#include "epx_efnt.h"
#include "epx_pixmap.h"

/* The pixels are normally stored as a EPIXEL_FMT_ALPHA (8 bits)
 * then we will render with
 *     fully opaque a=255  as foreground_color
 *     
 * and optionally render a=0 as background
 */
typedef struct _epx_font_ {
    EPX_OBJECT_MEMBERS(struct _epx_font_t);
    /*! Font file name (strdup) */
    char* file_name;
    /*! File size - store size of font file */
    uint32_t file_size;
    /*! Font foundry name (strdup) */
    char* foundry_name;
    /*! Font family name (stdup) */
    char* family_name;
    /*! Font info - converted to host order endian */
    epx_font_info_t font_info;
    /*!  Font Raw Data (when mapped or loaded) */
    epx_font_file_t* font_file;
    /*!  Allocated font data (when using FontLoad) */
    epx_font_file_t* font_data;
    /*!  Memory mapped data (when using FontMap)  */
    epx_font_file_t* font_map;
} epx_font_t;


extern void        epx_font_init(epx_font_t* font);
extern epx_font_t* epx_font_create(void);
extern void        epx_font_destroy(epx_font_t* font);

extern int         epx_font_open_init(epx_font_t* font, char* file);
extern epx_font_t* epx_font_open(char* file);
extern int         epx_font_load(epx_font_t* font);
extern int         epx_font_unload(epx_font_t* font);
extern int         epx_font_map(epx_font_t* font);
extern int         epx_font_unmap(epx_font_t* font);
extern int         epx_font_info_init(epx_font_t* font, epx_font_file_t* ff);
extern int         epx_font_preloaded_init(epx_font_t* font, uint8_t* data);

static inline int epx_font_is_mapped(epx_font_t* font) 
{
    return (font->font_map != 0);
}

static inline int epx_font_is_loaded(epx_font_t* font)
{
    return (font->font_data != 0);
}

extern epx_glyph_t* epx_font_file_glyph(epx_font_file_t* ff, 
					uint32_t encoding);
extern const char* epx_font_file_string(epx_font_file_t* ff, 
					uint32_t offset);

#endif
