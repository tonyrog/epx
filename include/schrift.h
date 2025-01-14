/* This file is part of libschrift.
 *
 * © 2019-2022 Thomas Oltmann and contributors
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. */

#ifndef SCHRIFT_H
#define SCHRIFT_H 1

#include <stddef.h> /* size_t */
#include <stdint.h> /* uint_fast32_t, uint_least32_t */

#ifdef __cplusplus
extern "C" {
#endif

#define USE_FLOAT_32

#ifdef USE_FLOAT_32
typedef float Float_t;
#define NextAfter(x,y) nextafterf((x),(y))
#define FloatConst(c) c##f
#define FloatAbs(x) fabsf((x))
#define FloatCeil(x) ceilf((x))
#define FloatFloor(x) floorf((x))
#else
#ifdef USE_FLOAT_64
typedef double Float_t;
#define NextAfter(x,y) nextafter((x),(y))
#define FloatConst(c) (c)
#define FloatAbs(x) fabs((x))
#define FloatCeil(x) ceil((x))
#define FloatFloor(x) floor((x))
#endif
#endif

#define SFT_DOWNWARD_Y 0x01

typedef struct SFT          SFT;
typedef struct SFT_Font     SFT_Font;
typedef uint_least32_t      SFT_UChar; /* Guaranteed to be compatible with char32_t. */
typedef uint_fast32_t       SFT_Glyph;
typedef struct SFT_LMetrics SFT_LMetrics;
typedef struct SFT_GMetrics SFT_GMetrics;
typedef struct SFT_Kerning  SFT_Kerning;
typedef struct SFT_Image    SFT_Image;
typedef Float_t SFT_Transform[6];

/* original index 
#define SFT_SX  0
#define SFT_RX  1
#define SFT_RY  2
#define SFT_SY  3
#define SFT_TX  4
#define SFT_TY  5
*/
// compatible with epx!
// SX,  RY, TX
// RX,  SY, TY
// 0    0   1
#define SFT_SX  0
#define SFT_RY  1
#define SFT_TX  2
#define SFT_RX  3
#define SFT_SY  4
#define SFT_TY  5

struct SFT
{
	SFT_Font *font;
	Float_t   xScale;
	Float_t   yScale;
	Float_t   xOffset;
	Float_t   yOffset;
	int       flags;
};

struct SFT_LMetrics
{
	Float_t ascender;
	Float_t descender;
	Float_t lineGap;
};

struct SFT_GMetrics
{
	Float_t advanceWidth;
	Float_t leftSideBearing;
	int    yOffset;
	int    minWidth;
	int    minHeight;
};

struct SFT_Kerning
{
	Float_t xShift;
	Float_t yShift;
};

// compatible with epx_pixel!
typedef union
{
    uint8_t v[4];
    uint32_t px;
    struct {
	uint8_t a;
	uint8_t r;
	uint8_t g;
	uint8_t b;
    };    
} SFT_Pixel;

struct SFT_Image
{
    void *pixels;
    int   width;
    int   height;
    // render area
    unsigned int xoffs;  // skip horizontal
    unsigned int yoffs;  // skip vertical
    unsigned int xlen;   // number of horizontal pixels
    unsigned int ylen;   // number of vertical pixels
    // fields added to support epx_pixmap
    unsigned int bytes_per_row;    // row stride
    unsigned int bytes_per_pixel;  // step size
    uint16_t pixel_format;
    void* render_arg;
    // render pixel callback or NULL
    void (*render)(unsigned char* pxaddr, int i, int j, uint8_t value,
		   uint16_t pxfmt, void* arg);
};

const char *sft_version(void);

SFT_Font *sft_loadmem (const void *mem, size_t size);
SFT_Font *sft_loadfile(const char *filename);
void      sft_freefont(SFT_Font *font);

extern int sft_lmetrics(const SFT *sft, SFT_LMetrics *metrics);
extern int sft_lookup  (const SFT *sft, SFT_UChar codepoint, SFT_Glyph *glyph);
extern int sft_gmetrics(const SFT *sft, SFT_Glyph glyph, SFT_GMetrics *metrics);
extern int sft_kerning (const SFT *sft, SFT_Glyph leftGlyph,
			SFT_Glyph rightGlyph, SFT_Kerning *kerning);
extern int sft_render  (const SFT *sft, SFT_Glyph glyph,
			SFT_Transform form, SFT_Image* imptr);

// return pointer (in mapped memory) to string with id nameID in "name"
// chunk. Also return length in len_ptr. NOT zero terminated.
// return NULL when string is not found.    
extern char* sft_name(SFT_Font* font, uint16_t nameID, uint16_t* len_ptr);
    
// debug
extern void sft_print_table_names(FILE* f, SFT_Font *font);
extern void sft_print_name_table(FILE* f, SFT_Font *font);


#ifdef __cplusplus
}
#endif

#endif

