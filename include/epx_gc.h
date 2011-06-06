//
//  Graphic context API
//
#ifndef __EPX_GC_H__
#define __EPX_GC_H__

#include "epx_pixmap.h"
#include "epx_font.h"

// FILL FLAGS
#define EPX_FILL_STYLE_NONE       EPX_FLAG_NONE
#define EPX_FILL_STYLE_SOLID      EPX_FLAG_SOLID
#define EPX_FILL_STYLE_BLEND      EPX_FLAG_BLEND
#define EPX_FILL_STYLE_SUM        EPX_FLAG_SUM
#define EPX_FILL_STYLE_AALIAS     EPX_FLAG_AALIAS
#define EPX_FILL_STYLE_TEXTURED   EPX_FLAG_TEXTURED

/* LINE FLAGS */
#define EPX_LINE_STYLE_SOLID      EPX_FLAG_SOLID
#define EPX_LINE_STYLE_BLEND      EPX_FLAG_BLEND
#define EPX_LINE_STYLE_SUM        EPX_FLAG_SUM
#define EPX_LINE_STYLE_AALIAS     EPX_FLAG_AALIAS
#define EPX_LINE_STYLE_TEXTURED   EPX_FLAG_TEXTURED
#define EPX_LINE_STYLE_DASHED     0x00000100
#define EPX_LINE_STYLE_NFIRST     EPX_FLAG_NFIRST
#define EPX_LINE_STYLE_NLAST      EPX_FLAG_NLAST

/* BORDER FLAGS */
#define EPX_BORDER_STYLE_SOLID    EPX_FLAG_SOLID
#define EPX_BORDER_STYLE_BLEND    EPX_FLAG_BLEND
#define EPX_BORDER_STYLE_SUM      EPX_FLAG_SUM
#define EPX_BORDER_STYLE_AALIAS   EPX_FLAG_AALIAS
#define EPX_BORDER_STYLE_TEXTURED EPX_FLAG_TEXTURED
#define EPX_BORDER_STYLE_DASHED   0x00010000
#define EPX_BORDER_STYLE_NTOP     0x00100000
#define EPX_BORDER_STYLE_NRIGHT   0x00200000
#define EPX_BORDER_STYLE_NBOTTOM  0x00400000
#define EPX_BORDER_STYLE_NLEFT    0x00800000
#define EPX_BORDER_STYLE_NBORDER  0x00F00000

typedef enum {
    EPX_CAP_STYLE_NONE  = 0,
    EPX_CAP_STYLE_BUTT  = 1,
    EPX_CAP_STYLE_ROUND = 2,
    EPX_CAP_STYLE_PROJECTING = 3
} epx_cap_style_t;

/* JOIN ENUMERATION */
typedef enum {
    EPX_JOIN_STYLE_MITER = 0,
    EPX_JOIN_STYLE_ROUND = 1,
    EPX_JOIN_STYLE_BEVEL = 2
} epx_join_style_t;


typedef struct _epx_gc_t {
    EPX_OBJECT_MEMBERS(struct _epx_gc_t);

    // Fill Options
    /*! EPX_FILL_STYLE_x flags */
    epx_flags_t    fill_style;
    /*! Color for fill operations. */
    epx_pixel_t    fill_color;
    /*! Texture for filling. */
    epx_pixmap_t*  fill_texture;

    /* LINE DRAWING */
    /*! EPX_FILL_STYLE_x flags */
    epx_flags_t      line_style;
    /*! EPX_JOIN_STYLE_x enum */
    epx_join_style_t line_join_style;
    /*! EPX_CAP_STYLE_x enum */
    epx_cap_style_t  line_cap_style;
    /*! Line width. */
    unsigned int     line_width;
    /*! Texture for drawing lines */
    epx_pixmap_t*    line_texture;

    /* BORDER DRAWING */
    /*! EPX_BORDER_STYLE_x flags */
    epx_flags_t      border_style;
    /*! EPX_JOIN_STYLE_x enum */
    epx_join_style_t border_join_style;
    /*! EPX_CAP_STYLE_x enum */
    epx_cap_style_t  border_cap_style;
    /*! Border base color */
    epx_pixel_t      border_color;
    /*! Border=0 means not used (when fill) */
    unsigned int     border_width;
    /*! Texture for drawing borders. */
    epx_pixmap_t*    border_texture;  

    /* Base Colors */
    /*! Line drawing/font foreground. */
    epx_pixel_t      foreground_color;
    /*! Text background and others */
    epx_pixel_t      background_color;
    /* Alpha fader (fixnum 8 bit) 255 = 1.0 */
    uint8_t          fader_value;

    /* Text Drawing */
    /*! Font used for drawing text */
    epx_font_t* font;
    
    /*! Added extra horizontal amount after glyph is draw */
    int16_t      glyph_delta_x;
    /*! Added extra vertical amount after glyph is draw */
    int16_t      glyph_delta_y;
    /*! Override glyph with for  glyph drawing only used if not zero */
    uint16_t    glyph_fixed_width;
    /*! Special dot kerning for char '.' code = 46, */
    int16_t      glyph_dot_kern;
} epx_gc_t;

extern epx_gc_t epx_default_gc;

// GC interface 
extern void      epx_gc_init(epx_gc_t* gc);
extern epx_gc_t* epx_gc_create(void);
extern epx_gc_t* epx_gc_copy(epx_gc_t* gc);
extern void      epx_gc_init_copy(epx_gc_t* src, epx_gc_t* dst);
extern void      epx_gc_destroy(epx_gc_t* gc);

// GETTERS
static inline epx_pixel_t epx_gc_get_foreground_color(epx_gc_t* gc)
{
    return gc->foreground_color;
}

static inline  epx_pixel_t epx_gc_get_background_color(epx_gc_t* gc)
{
    return gc->background_color;
}

static inline epx_flags_t epx_gc_get_fill_style(epx_gc_t* gc)
{
    return gc->fill_style;
}

static inline epx_pixel_t epx_gc_get_fill_color(epx_gc_t* gc)
{
    return gc->fill_color;
}

static inline  epx_pixmap_t* epx_gc_get_fill_texture(epx_gc_t* gc)
{
    return gc->fill_texture;
}

static inline epx_flags_t epx_gc_get_line_style(epx_gc_t* gc)
{
    return gc->line_style;
}

static inline epx_join_style_t epx_gc_get_line_join_style(epx_gc_t* gc)
{
    return gc->line_join_style;
}

static inline epx_cap_style_t epx_gc_get_line_cap_style(epx_gc_t* gc)
{
    return gc->line_cap_style;
}

static inline unsigned int epx_gc_get_line_width(epx_gc_t* gc)
{
    return gc->line_width;
}

static inline epx_pixmap_t* epx_gc_get_line_texture(epx_gc_t* gc)
{
    return gc->line_texture;
}


static inline epx_flags_t epx_gc_get_border_style(epx_gc_t* gc)
{
    return gc->border_style;
}

static inline epx_join_style_t epx_gc_get_border_join_style(epx_gc_t* gc)
{
    return gc->border_join_style;
}

static inline epx_cap_style_t epx_gc_get_border_cap_style(epx_gc_t* gc)
{
    return gc->border_cap_style;
}

static inline unsigned int epx_gc_get_border_width(epx_gc_t* gc)
{
    return gc->border_width;
}

static inline epx_pixel_t epx_gc_get_border_color(epx_gc_t* gc)
{
    return gc->border_color;
}

static inline epx_pixmap_t* epx_gc_get_border_texture(epx_gc_t* gc)
{
    return gc->border_texture;
}

static inline uint8_t epx_gc_get_fader_value(epx_gc_t* gc)
{
    return gc->fader_value;
}

static inline epx_font_t* epx_gc_get_font(epx_gc_t* gc)
{
    return gc->font;
}

static inline int16_t epx_gc_get_glyph_delta_x(epx_gc_t* gc)
{
    return gc->glyph_delta_x;
}

static inline int16_t epx_gc_get_glyph_delta_y(epx_gc_t* gc)
{
    return gc->glyph_delta_y;
}

static inline uint16_t epx_gc_get_glyph_fixed_width(epx_gc_t* gc)
{
    return gc->glyph_fixed_width;
}

static inline uint16_t epx_gc_get_glyph_dot_kern(epx_gc_t* gc)
{
    return gc->glyph_dot_kern;
}


// SETTERS
static inline void epx_gc_set_foreground_color(epx_gc_t* gc, epx_pixel_t color)
{
    gc->foreground_color = color;
}

static inline void epx_gc_set_background_color(epx_gc_t* gc, epx_pixel_t color)
{
    gc->background_color = color;
}

static inline void epx_gc_set_fill_style(epx_gc_t* gc, epx_flags_t style)
{
    gc->fill_style = style;
}

static inline void epx_gc_set_fill_color(epx_gc_t* gc, epx_pixel_t color)
{
    gc->fill_color = color;
}

static inline void epx_gc_set_fill_texture(epx_gc_t* gc, epx_pixmap_t* texture)
{
    if (texture) epx_object_ref(texture);
    epx_object_unref(gc->fill_texture);
    gc->fill_texture = texture;
}

static inline void epx_gc_set_line_style(epx_gc_t* gc, epx_flags_t style)
{
    gc->line_style = style;
}

static inline void epx_gc_set_line_join_style(epx_gc_t* gc,
					      epx_join_style_t style)
{
    gc->line_join_style = style;
}

static inline void epx_gc_set_line_cap_style(epx_gc_t* gc,
					     epx_cap_style_t style)
{
    gc->line_cap_style = style;
}


static inline void epx_gc_set_line_width(epx_gc_t* gc, unsigned int width)
{
    gc->line_width = width;
}

static inline void epx_gc_set_line_texture(epx_gc_t* gc, epx_pixmap_t* texture)
{
    if (texture)  epx_object_ref(texture);
    epx_object_unref(gc->line_texture);
    gc->line_texture = texture;
}

static inline void epx_gc_set_border_style(epx_gc_t* gc, epx_flags_t style)
{
    gc->border_style = style;
}

static inline void epx_gc_set_border_join_style(epx_gc_t* gc,
						epx_join_style_t style)
{
    gc->border_join_style = style;
}

static inline void epx_gc_set_border_cap_style(epx_gc_t* gc, 
					       epx_cap_style_t style)
{
    gc->border_cap_style = style;
}

static inline void epx_gc_set_border_width(epx_gc_t* gc, unsigned int width)
{
    gc->border_width = width;
}

static inline void epx_gc_set_border_color(epx_gc_t* gc, epx_pixel_t color)
{
    gc->border_color = color;
}

static inline void epx_gc_set_border_texture(epx_gc_t* gc, 
					     epx_pixmap_t* texture)
{
    if (texture)  epx_object_ref(texture);
    epx_object_unref(gc->border_texture);
    gc->border_texture = texture;
}

static inline void epx_gc_set_fader_value(epx_gc_t* gc, uint8_t fade)
{
    gc->fader_value = fade;
}

static inline void epx_gc_set_fader_float(epx_gc_t* gc, float fade)
{
    if (fade >= 1.0)
	gc->fader_value = 255;
    else if (fade <= 0.0)
	gc->fader_value = 0;
    else
      gc->fader_value = (uint8_t)(fade*256);
}

static inline void epx_gc_set_font(epx_gc_t* gc, epx_font_t* font)
{
    if (font) epx_object_ref(font);
    epx_object_unref(gc->font);
    gc->font = font;
}

static inline void epx_gc_set_glyph_delta_x(epx_gc_t* gc, int16_t x)
{
    gc->glyph_delta_x = x;
}

static inline void epx_gc_set_glyph_delta_y(epx_gc_t* gc, int16_t y)
{
    gc->glyph_delta_y = y;
}

static inline void epx_gc_set_glyph_delta(epx_gc_t* gc, int16_t x, int16_t y)
{
    gc->glyph_delta_x = x;
    gc->glyph_delta_y = y;
}

static inline void epx_gc_set_glyph_fixed_width(epx_gc_t* gc, uint16_t width)
{
    gc->glyph_fixed_width = width;
}

static inline void epx_gc_set_glyph_dot_kern(epx_gc_t* gc, uint16_t kern)
{
    gc->glyph_dot_kern = kern;
}

#endif
