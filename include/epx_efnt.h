//
// Font File data structures
//

#ifndef __EPX_EFNT_H__
#define __EPX_EFNT_H__

#include <stdint.h>

typedef enum {
    EPX_FONT_WEIGHT_NONE     = 0,
    EPX_FONT_WEIGHT_MEDIUM   = 1,
    EPX_FONT_WEIGHT_BOLD     = 2,
    EPX_FONT_WEIGHT_DEMIBOLD = 3
} epx_font_weight_t;

typedef enum {
    EPX_FONT_SLANT_NONE            = 0,
    EPX_FONT_SLANT_ROMAN           = 1,
    EPX_FONT_SLANT_ITALIC          = 2,
    EPX_FONT_SLANT_OBLIQUE         = 3,
    EPX_FONT_SLANT_REVERSE_ITALIC  = 4,
    EPX_FONT_SLANT_REVERSE_OBLIQUE = 5,
    EPX_FONT_SLANT_OTHER           = 6
} epx_font_slant_t;

typedef enum {
    EPX_FONT_WIDTH_NONE         = 0,
    EPX_FONT_WIDTH_NORMAL       = 1,
    EPX_FONT_WIDTH_CONDENSED    = 2,
    EPX_FONT_WIDTH_NARROW       = 3,
    EPX_FONT_WIDTH_DOUBLE_WIDE  = 4
} epx_font_width_t;

typedef enum {
    EPX_FONT_STYLE_NONE       = 0,
    EPX_FONT_STYLE_SERIF      = 1,
    EPX_FONT_STYLE_SANS_SERIF = 2,
    EPX_FONT_STYLE_INFORMAL   = 3,
    EPX_FONT_STYLE_DECORATED  = 4,
} epx_font_style_t;

typedef enum {
    EPX_FONT_SPACING_NONE         = 0,
    EPX_FONT_SPACING_PROPORTIONAL = 1,
    EPX_FONT_SPACING_MONOSPACED   = 2,
    EPX_FONT_SPACING_CHAR_CELL    = 3
} epx_font_spacing_t;

/* Note resolution and point size is given in
 * decipoints (10*) where 72.27 points equals 1 inch
 */
typedef struct _epx_font_info_t {
    /*! font weight - EFontWeight */
    uint32_t weight;
    /*! font slant - EFontSlant */
    uint32_t slant;
    /*! font width - EFontWidth */
    uint32_t width;
    /*! font style - EFontStyle */
    uint32_t style;
    /*! font spacing - EFontSpacing */
    uint32_t spacing;
    /*! font pixel type pixmap data stored with glyphs */
    uint32_t pixel_format;
    /*! font size in pixels pixel_size=points* resolution_x/dpi */
    uint32_t pixel_size;
    /*! font size in points */
    uint32_t point_size;
    /*! resolution x (Points per inch) */
    uint32_t resolution_x;
    /*! resolution y (Points per inch) */
    uint32_t resolution_y;
    /* ! font descent */
    uint32_t descent;
    /*! font ascent */
    uint32_t ascent;
    /*  NOTE! add pad to 16 byte alignment if needed */
} epx_font_info_t;

/*! Font File format */
typedef struct _epx_font_file_t {
    /*! EFNT file magic */
    char magic[4];
    /*! offset to the font foundry string (data) */
    uint32_t foundry_offset;
    /*! offset to the font family string (data) */
    uint32_t family_offset;
    /*! font info structure */
    epx_font_info_t font_info;
    /*! First glyph code */
    uint32_t encoding_start; 
    /*! Last glyph code */
    uint32_t encoding_stop;
    /*! default glyph (typically '?') */
    uint32_t encoding_default;
    /*! string table start */
    uint32_t string_table_start;
    /*! string table length */
    uint32_t string_table_length;
    /*! offset table start */
    uint32_t offset_table_start;
    /*! offset table length */
    uint32_t offset_table_length;
    /*! glyph table start */
    uint32_t glyph_table_start;
    /*! glyph table length */
    uint32_t glyph_table_length;
    /*  NOTE! add pad to 16 byte alignment if needed */
    /*! start of data table area */
    uint32_t data[0];
} epx_font_file_t;

/* 16 byte glyph overhead */
typedef struct _epx_glyph_t {
    /*! offset to glyph name in font file string table */
    uint32_t name_offset;
    /*! glyph width in pixels */
    uint16_t width;
    /*! glyph height in pixels */
    uint16_t height;
    /*! x offset from origin */
    int16_t   xoffs;
    /*! y offset from origin */
    int16_t   yoffs;
    /* ! delta x value when drawing glyph */
    int16_t   dwx;
    /* ! delta y value when drawing glyph */
    int16_t   dwy;
    /* ! glyph pixel data */
    uint32_t data[0];
} epx_glyph_t;


// EFNT2 Compression codes:
// 0 0 0 | n n n n n        n transparent pixels (0-31)
// 0 0 1 | n n n n n        n opaque pixels (0-31)
// 0 1 0 | X X X X X        5 bit bitmap
// 0 1 1 | 0 X X X X        4 bit bitmap
// 0 1 1 | 1 0 X X X        3 bit bitmap
// 0 1 1 | 1 1 0 X X        2 bit bitmap
// 0 1 1 | 1 1 1 0 X        1 bit bitmap (0=transparent, 1=opaque)
// 0 1 1 | 1 1 1 1 0        0 bit bitmap (filler byte)
// 0 1 1 | 1 1 1 1 1        esc?|ret?
// 1 a a   a a a a a        7-bit alpha value (1-7f)
//

#define EFNT2_A(a)   (0x80 | (((a)-1)>>1))
#define EFNT2_O(n)   (0x20 + ((n) & 0x1f))
#define EFNT2_T(n)   ((n) & 0x1f)

#define EFNT2_E()                (0x7F)
#define EFNT2_B0()               (0x7E)
#define EFNT2_B1(x0)             (0x7C + (x0))
#define EFNT2_B2(x1,x0)          (0x78 + (x1)*2+(x0))
#define EFNT2_B3(x2,x1,x0)       (0x70 + (x2)*4+(x1)*2+(x0))
#define EFNT2_B4(x3,x2,x1,x0)    (0x60 + (x3)*8+(x2)*4+(x1)*2+(x0))
#define EFNT2_B5(x4,x3,x2,x1,x0) (0x40 + (x4)*16+(x3)*8+(x2)*4+(x1)*2+(x0))

#define EFNT2_Rl(c)  ((c) & 0x1f)

typedef struct {
    uint8_t  n;      // run length | number of bits
    int32_t code;     // bits 0..31 bits
    uint8_t *ptr;    // src buffer
    unsigned int len; // max src len E() may terminate!
} epx_fnt2_ctx_t;

static inline void epx_fnt2_ctx_init(epx_fnt2_ctx_t* ctx, 
				     uint8_t* ptr, unsigned int len)
{
    ctx->n = 0;
    ctx->code = 0;
    ctx->ptr = ptr;
    ctx->len = len;
}

static inline uint8_t epx_fnt2_ctx_next(epx_fnt2_ctx_t* ctx)
{
    if (!ctx->n) {
	uint8_t c;
	if (!ctx->len)
	    return 0;
	c = *ctx->ptr++;
	ctx->len--;
	if (c <= 0x3F) {  // EFNT2_O(n) || EFNT2_T(n)
	    ctx->code = (c & 0x20) ? -1 : 0;
	    ctx->n = EFNT2_Rl(c);
	}
	else if (c > 0x7f)  // EFNT2_A(x)
	    return ((c << 1)+1);
	else if (c == 0x7f) { // EFNT2_E() 
	    ctx->len = 0;
	    ctx->n = 0;
	}
	else {
	    ctx->n = 5;
	    while((c & 0x20)) {
		c <<= 1;
		ctx->n--;
	    }
	    ctx->code = ((int32_t)c) << 26;
	}
    }
    if (ctx->n) {
	ctx->n--;
	ctx->code <<= 1;
	return (ctx->code < 0) ? 255 : 0;
    }
    return 0;
}



#endif
