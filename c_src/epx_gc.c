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
// EPX GC functions
//

#include <stdlib.h>
#include "epx_colors.h"
#include "epx_object.h"
#include "epx_gc.h"

epx_gc_t epx_default_gc =
{
    {0, 0},
    0,            // Not on heap
    0,            // No references
    0,            // No release function
    0,            // Opaque
    EPX_GC_TYPE,  // Type
    0,            // Next

    EPX_FILL_STYLE_SOLID,  // fill_style
    EPX_PIXEL_RED,             // fill_color
    0,                      // fill_texture

    EPX_LINE_STYLE_SOLID,  // line_style
    EPX_JOIN_STYLE_MITER,  // line_join_style
    EPX_CAP_STYLE_NONE,    // line_cap_style
    1,                     // line_width
    0,                     // line_texture

    EPX_BORDER_STYLE_SOLID, // border_style
    EPX_JOIN_STYLE_MITER,   // border_join_style
    EPX_CAP_STYLE_NONE,     // border_cap_style
    EPX_PIXEL_BLACK,        // border_color
    0,                      // border_width
    0,                      // border_texture

    // Color
    EPX_PIXEL_BLUE,             // forground_color
    EPX_PIXEL_GREEN,            // background_color
    // Alpha
    255,                     // alpha factor = 1.0
    // Text
    0,                       // Font used for text drawing
    // Glyphs
    0,                       // glyph_delta_x
    0,                       // glyph_delta_y
    0,                       // glyph_fixed_width
    0                        // glyph_dot_kern
};


void epx_gc_init(epx_gc_t* gc)
{
    EPX_OBJECT_INIT(gc, EPX_GC_TYPE);

    gc->fill_style        = EPX_FILL_STYLE_SOLID;
    gc->fill_color        = epx_pixel_red;
    gc->fill_texture      = 0;

    gc->line_style        = EPX_LINE_STYLE_SOLID;
    gc->line_join_style   = EPX_JOIN_STYLE_MITER;
    gc->line_cap_style    = EPX_CAP_STYLE_NONE;
    gc->line_width        = 1;
    gc->line_texture      = 0;

    gc->border_style      = EPX_BORDER_STYLE_SOLID;
    gc->border_join_style = EPX_JOIN_STYLE_MITER;
    gc->border_cap_style  = EPX_CAP_STYLE_NONE;
    gc->border_color      = epx_pixel_black;
    gc->border_width      = 0;
    gc->border_texture    = 0;

    gc->foreground_color = epx_pixel_blue;
    gc->background_color = epx_pixel_green;

    gc->fader_value      = ALPHA_FACTOR_1;  // 1.0

    gc->font = 0;

    gc->glyph_delta_x     = 0;
    gc->glyph_delta_y     = 0;
    gc->glyph_fixed_width = 0;
    gc->glyph_dot_kern    = 0;
}

epx_gc_t* epx_gc_create()
{
    epx_gc_t* gc;

    if (!(gc = (epx_gc_t*) malloc(sizeof(epx_gc_t))))
	return 0;
    epx_gc_init(gc);
    gc->on_heap = 1;
    gc->refc = 1;
    return gc;
}

void epx_gc_destroy(epx_gc_t* gc)
{
    epx_object_unref(gc);
}

void EPX_GC_TYPE_RELEASE(void* arg)
{
    epx_gc_t* gc = (epx_gc_t*) arg;

    EPX_DBGFMT_MEM("EGC_TYPE_RELEASE: %p", arg);
    epx_object_unref(gc->fill_texture);
    epx_object_unref(gc->line_texture);
    epx_object_unref(gc->border_texture);
    epx_object_unref(gc->font);
    if (gc->on_heap) free(gc);
}

void epx_gc_init_copy(epx_gc_t* src, epx_gc_t* dst)
{
    *dst = *src;
    epx_object_ref(dst->fill_texture);
    epx_object_ref(dst->line_texture);
    epx_object_ref(dst->border_texture);
    epx_object_ref(dst->font);
    dst->next = 0;
}

epx_gc_t* epx_gc_copy(epx_gc_t* gc)
{
    epx_gc_t* copy;

    if (!(copy = (epx_gc_t*) malloc(sizeof(epx_gc_t))))
	return 0;
    // reference textures etc 
    epx_gc_init_copy(gc, copy);
    copy->refc    = 1;
    copy->on_heap = 1;
    return copy;
}
