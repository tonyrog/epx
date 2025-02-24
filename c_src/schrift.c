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

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>

#if defined(_MSC_VER)
# define restrict __restrict
#endif

#if defined(_WIN32)
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>
#else
#ifndef _POSIX_C_SOURCE
# define _POSIX_C_SOURCE 1
#endif
# include <fcntl.h>
# include <sys/mman.h>
# include <sys/stat.h>
# include <unistd.h>
#endif

#include "schrift.h"

#define SCHRIFT_VERSION "0.10.2"

#define FILE_MAGIC_ONE             0x00010000
#define FILE_MAGIC_TWO             0x74727565

#define HORIZONTAL_KERNING         0x01
#define MINIMUM_KERNING            0x02
#define CROSS_STREAM_KERNING       0x04
#define OVERRIDE_KERNING           0x08

#define POINT_IS_ON_CURVE          0x01
#define X_CHANGE_IS_SMALL          0x02
#define Y_CHANGE_IS_SMALL          0x04
#define REPEAT_FLAG                0x08
#define X_CHANGE_IS_ZERO           0x10
#define X_CHANGE_IS_POSITIVE       0x10
#define Y_CHANGE_IS_ZERO           0x20
#define Y_CHANGE_IS_POSITIVE       0x20

#define OFFSETS_ARE_LARGE          0x001
#define ACTUAL_XY_OFFSETS          0x002
#define GOT_A_SINGLE_SCALE         0x008
#define THERE_ARE_MORE_COMPONENTS  0x020
#define GOT_AN_X_AND_Y_SCALE       0x040
#define GOT_A_SCALE_MATRIX         0x080

/* macros */
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define SIGN(x)   (((x) > 0) - ((x) < 0))
/* Allocate values on the stack if they are small enough, else spill to heap. */
#define STACK_ALLOC(var, type, thresh, count) \
	type var##_stack_[thresh]; \
	var = (count) <= (thresh) ? var##_stack_ : calloc(sizeof(type), count);
#define STACK_FREE(var) \
	if (var != var##_stack_) free(var);

enum { SrcMapping, SrcUser };

/* structs */
typedef struct Point   Point;
typedef struct Line    Line;
typedef struct Curve   Curve;
typedef struct Cell    Cell;
typedef struct Outline Outline;
typedef struct Raster  Raster;

struct Point { Float_t x, y; };
struct Line  { uint_least16_t beg, end; };
struct Curve { uint_least16_t beg, end, ctrl; };
struct Cell  { Float_t area, cover; };

struct Outline
{
	Point *points;
	Curve *curves;
	Line  *lines;
	uint_least16_t numPoints;
	uint_least16_t capPoints;
	uint_least16_t numCurves;
	uint_least16_t capCurves;
	uint_least16_t numLines;
	uint_least16_t capLines;
};

struct Raster
{
	Cell *cells;
	int   width;
	int   height;
};

struct SFT_Font
{
	const uint8_t *memory;
	uint_fast32_t  size;
#if defined(_WIN32)
	HANDLE         mapping;
#endif
	int            source;
	
	uint_least16_t unitsPerEm;
	int_least16_t  locaFormat;
	uint_least16_t numLongHmtx;
};
    
/* function declarations */
/* generic utility functions */
static void *realloc_array(void *optr, size_t nmemb, size_t size);
static inline int fast_floor(Float_t x);
static inline int fast_ceil (Float_t x);
/* file loading */
static int  map_file  (SFT_Font *font, const char *filename);
static void unmap_file(SFT_Font *font);
static int  init_font (SFT_Font *font);
/* simple mathematical operations */
static inline void midpoint(Point* pts, uint_least16_t a, uint_least16_t b, uint_least16_t c);
static void transform_points(unsigned int numPts, Point *points, SFT_Transform form);
// static void clip_points(unsigned int numPts, Point *points, Float_t width, Float_t height);
/* 'outline' data structure management */
static int  init_outline(Outline *outl);
static void free_outline(Outline *outl);
static int  grow_points (Outline *outl);
static int  grow_curves (Outline *outl);
static int  grow_lines  (Outline *outl);
/* TTF parsing utilities */
static inline int is_safe_offset(SFT_Font *font, uint_fast32_t offset, uint_fast32_t margin);
static void *csearch(const void *key, const void *base,
	size_t nmemb, size_t size, int (*compar)(const void *, const void *));
static int  cmpu16(const void *a, const void *b);
static int  cmpu32(const void *a, const void *b);
static inline uint_least8_t  getu8 (SFT_Font *font, uint_fast32_t offset);
static inline int_least8_t   geti8 (SFT_Font *font, uint_fast32_t offset);
static inline uint_least16_t getu16(SFT_Font *font, uint_fast32_t offset);
static inline int_least16_t  geti16(SFT_Font *font, uint_fast32_t offset);
static inline uint_least32_t getu32(SFT_Font *font, uint_fast32_t offset);
static int gettable(SFT_Font *font, char tag[4], uint_fast32_t *offset);
/* codepoint to glyph id translation */
static int  cmap_fmt4(SFT_Font *font, uint_fast32_t table, SFT_UChar charCode, uint_fast32_t *glyph);
static int  cmap_fmt6(SFT_Font *font, uint_fast32_t table, SFT_UChar charCode, uint_fast32_t *glyph);
static int  glyph_id(SFT_Font *font, SFT_UChar charCode, uint_fast32_t *glyph);
/* glyph metrics lookup */
static int  hor_metrics(SFT_Font *font, uint_fast32_t glyph, int *advanceWidth, int *leftSideBearing);
static int  glyph_bbox(const SFT *sft, uint_fast32_t outline, int box[4]);
/* decoding outlines */
static int  outline_offset(SFT_Font *font, uint_fast32_t glyph, uint_fast32_t *offset);
static int  simple_flags(SFT_Font *font, uint_fast32_t *offset, uint_fast16_t numPts, uint8_t *flags);
static int  simple_points(SFT_Font *font, uint_fast32_t offset, uint_fast16_t numPts, uint8_t *flags, Point *points);
static int  decode_contour(uint8_t *flags, uint_fast16_t basePoint, uint_fast16_t count, Outline *outl);
static int  simple_outline(SFT_Font *font, uint_fast32_t offset, unsigned int numContours, Outline *outl);
static int  compound_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl);
static int  decode_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl);
/* tesselation */
static int  is_flat(Outline *outl, Curve curve);
static int  tesselate_curve(Curve curve, Outline *outl);
static int  tesselate_curves(Outline *outl);
/* silhouette rasterization */
static void draw_line(Raster buf, Float_t x0, Float_t y0, Float_t x1, Float_t y1);
static void draw_lines(Outline *outl, Raster buf);
/* post-processing */
static void post_process(Raster buf, SFT_Image* imptr);
/* glyph rendering */
static int  render_outline(Outline *outl, SFT_Transform t, SFT_Image* imptr);

/* function implementations */

const char *
sft_version(void)
{
	return SCHRIFT_VERSION;
}

/* Loads a font from a user-supplied memory range. */
SFT_Font *
sft_loadmem(const void *mem, size_t size)
{
	SFT_Font *font;
	if (size > UINT32_MAX) {
		return NULL;
	}
	if (!(font = calloc(1, sizeof *font))) {
		return NULL;
	}
	font->memory = mem;
	font->size   = (uint_fast32_t) size;
	font->source = SrcUser;
	if (init_font(font) < 0) {
		sft_freefont(font);
		return NULL;
	}
	return font;
}

/* Loads a font from the file system. To do so, it has to map the entire font into memory. */
SFT_Font *
sft_loadfile(char const *filename)
{
	SFT_Font *font;
	if (!(font = calloc(1, sizeof *font))) {
		return NULL;
	}
	if (map_file(font, filename) < 0) {
		free(font);
		return NULL;
	}
	if (init_font(font) < 0) {
		sft_freefont(font);
		return NULL;
	}
	return font;
}

void
sft_freefont(SFT_Font *font)
{
	if (!font) return;
	/* Only unmap if we mapped it ourselves. */
	if (font->source == SrcMapping)
		unmap_file(font);
	free(font);
}

int
sft_lmetrics(const SFT *sft, SFT_LMetrics *metrics)
{
	Float_t factor;
	uint_fast32_t hhea;
	memset(metrics, 0, sizeof *metrics);
	if (gettable(sft->font, "hhea", &hhea) < 0)
		return -1;
	if (!is_safe_offset(sft->font, hhea, 36))
		return -1;
	factor = sft->yScale / sft->font->unitsPerEm;
	metrics->ascender  = geti16(sft->font, hhea + 4) * factor;
	metrics->descender = geti16(sft->font, hhea + 6) * factor;
	metrics->lineGap   = geti16(sft->font, hhea + 8) * factor;
	return 0;
}

int
sft_lookup(const SFT *sft, SFT_UChar codepoint, SFT_Glyph *glyph)
{
	return glyph_id(sft->font, codepoint, glyph);
}

int
sft_gmetrics(const SFT *sft, SFT_Glyph glyph, SFT_GMetrics *metrics)
{
	int adv, lsb;
	Float_t xScale = sft->xScale / sft->font->unitsPerEm;
	uint_fast32_t outline;
	int bbox[4];

	memset(metrics, 0, sizeof *metrics);

	if (hor_metrics(sft->font, glyph, &adv, &lsb) < 0)
		return -1;
	metrics->advanceWidth    = (Float_t) adv * xScale;
	metrics->leftSideBearing = (Float_t) lsb * xScale + sft->xOffset;

	if (outline_offset(sft->font, glyph, &outline) < 0)
		return -1;
	if (!outline)
		return 0;
	if (glyph_bbox(sft, outline, bbox) < 0)
		return -1;
	metrics->minWidth  = bbox[2] - bbox[0] + 1;
	metrics->minHeight = bbox[3] - bbox[1] + 1;
	metrics->yOffset   = sft->flags & SFT_DOWNWARD_Y ? -bbox[3] : bbox[1];

	return 0;
}

int
sft_kerning(const SFT *sft, SFT_Glyph leftGlyph, SFT_Glyph rightGlyph,
            SFT_Kerning *kerning)
{
	void *match;
	uint_fast32_t offset;
	unsigned int numTables, numPairs, length, format, flags;
	int value;
	uint8_t key[4];

	memset(kerning, 0, sizeof *kerning);

	if (gettable(sft->font, "kern", &offset) < 0)
		return 0;

	/* Read kern table header. */
	if (!is_safe_offset(sft->font, offset, 4))
		return -1;
	if (getu16(sft->font, offset) != 0)
		return 0;
	numTables = getu16(sft->font, offset + 2);
	offset += 4;

	while (numTables > 0) {
		/* Read subtable header. */
		if (!is_safe_offset(sft->font, offset, 6))
			return -1;
		length = getu16(sft->font, offset + 2);
		format = getu8(sft->font, offset + 4);
		flags  = getu8(sft->font, offset + 5);
		offset += 6;

		if (format == 0 && (flags & HORIZONTAL_KERNING) && !(flags & MINIMUM_KERNING)) {
			/* Read format 0 header. */
			if (!is_safe_offset(sft->font, offset, 8))
				return -1;
			numPairs = getu16(sft->font, offset);
			offset += 8;
			/* Look up character code pair via binary search. */
			key[0] = (leftGlyph  >> 8) & 0xFF;
			key[1] =  leftGlyph  & 0xFF;
			key[2] = (rightGlyph >> 8) & 0xFF;
			key[3] =  rightGlyph & 0xFF;
			if ((match = bsearch(key, sft->font->memory + offset,
				numPairs, 6, cmpu32)) != NULL) {
				
				value = geti16(sft->font, (uint_fast32_t) ((uint8_t *) match - sft->font->memory + 4));
				if (flags & CROSS_STREAM_KERNING) {
				    kerning->yShift += (Float_t) value;
				} else {
					kerning->xShift += (Float_t) value;
				}
			}

		}

		offset += length;
		--numTables;
	}

	kerning->xShift = kerning->xShift / sft->font->unitsPerEm * sft->xScale;
	kerning->yShift = kerning->yShift / sft->font->unitsPerEm * sft->yScale;

	return 0;
}

static void compose(SFT_Transform T, SFT_Transform S, SFT_Transform D)
{
    Float_t Tsx = T[SFT_SX];
    Float_t Try = T[SFT_RY];
    Float_t Trx = T[SFT_RX];
    Float_t Tsy = T[SFT_SY];
    
    Float_t Ssx = S[SFT_SX];
    Float_t Sry = S[SFT_RY];
    Float_t Stx = S[SFT_TY];
    Float_t Sty = S[SFT_TY];
    D[SFT_SX] = Tsx*Ssx + Try*S[SFT_RX];
    D[SFT_RY] = Tsx*Sry + Try*S[SFT_SY];
    D[SFT_RX] = Trx*Ssx + Tsy*S[SFT_RX];
    D[SFT_SY] = Trx*Sry + Tsy*S[SFT_SY];
    D[SFT_TX] = Tsx*Stx + Try*Sty + T[SFT_TX];
    D[SFT_TY] = Trx*Stx + Tsy*Sty + T[SFT_TY];
}


int
sft_render(const SFT *sft, SFT_Glyph glyph, SFT_Transform t, SFT_Image* imptr)
{
	uint_fast32_t outline;
	SFT_Transform trf;
	int bbox[4];
	Outline outl;
	
	if (outline_offset(sft->font, glyph, &outline) < 0)
		return -1;
	if (!outline)
		return 0;
	if (glyph_bbox(sft, outline, bbox) < 0)
		return -1;
	/* Set up the transformation matrix such that
	 * the transformed bounding boxes min corner lines
	 * up with the (0, 0) point. */
	trf[SFT_SX] = sft->xScale / sft->font->unitsPerEm;
	trf[SFT_RX] = FloatConst(0.0);
	trf[SFT_RY] = FloatConst(0.0);
	trf[SFT_TX] = sft->xOffset - (Float_t)bbox[0];
	if (sft->flags & SFT_DOWNWARD_Y) {
		trf[SFT_SY] = -sft->yScale / sft->font->unitsPerEm;
		trf[SFT_TY] = (Float_t)bbox[3] - sft->yOffset;
	} else {
		trf[SFT_SY] = +sft->yScale / sft->font->unitsPerEm;
		trf[SFT_TY] = sft->yOffset - (Float_t)bbox[1];
	}
	compose(t, trf, trf);
	
	memset(&outl, 0, sizeof outl);
	if (init_outline(&outl) < 0)
		goto failure;

	if (decode_outline(sft->font, outline, 0, &outl) < 0)
		goto failure;
	if (render_outline(&outl, trf, imptr) < 0)
		goto failure;

	free_outline(&outl);
	return 0;

failure:
	free_outline(&outl);
	return -1;
}

/* This is sqrt(SIZE_MAX+1), as s1*s2 <= SIZE_MAX
 * if both s1 < MUL_NO_OVERFLOW and s2 < MUL_NO_OVERFLOW */
#define MUL_NO_OVERFLOW	((size_t)1 << (sizeof(size_t) * 4))

/* OpenBSD's realloc_array() standard libary function.
 * A wrapper for realloc() that takes two size args like calloc().
 * Useful because it eliminates common integer overflow bugs. */
static void *
realloc_array(void *optr, size_t nmemb, size_t size)
{
	if ((nmemb >= MUL_NO_OVERFLOW || size >= MUL_NO_OVERFLOW) &&
	    nmemb > 0 && SIZE_MAX / nmemb < size) {
		errno = ENOMEM;
		return NULL;
	}
	return realloc(optr, size * nmemb);
}

/* TODO maybe we should use long here instead of int. */
static inline int
fast_floor(Float_t x)
{
	int i = (int) x;
	return i - (i > x);
}

static inline int
fast_ceil(Float_t x)
{
	int i = (int) x;
	return i + (i < x);
}

#if defined(_WIN32)

static int
map_file(SFT_Font *font, const char *filename)
{
	HANDLE file;
	DWORD high, low;

	font->mapping = NULL;
	font->memory  = NULL;

	file = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if (file == INVALID_HANDLE_VALUE) {
		return -1;
	}

	low = GetFileSize(file, &high);
	if (low == INVALID_FILE_SIZE) {
		CloseHandle(file);
		return -1;
	}

	font->size = (size_t)high << (8 * sizeof(DWORD)) | low;

	font->mapping = CreateFileMapping(file, NULL, PAGE_READONLY, high, low, NULL);
	if (!font->mapping) {
		CloseHandle(file);
		return -1;
	}

	CloseHandle(file);

	font->memory = MapViewOfFile(font->mapping, FILE_MAP_READ, 0, 0, 0);
	if (!font->memory) {
		CloseHandle(font->mapping);
		font->mapping = NULL;
		return -1;
	}

	return 0;
}

static void
unmap_file(SFT_Font *font)
{
	if (font->memory) {
		UnmapViewOfFile(font->memory);
		font->memory = NULL;
	}
	if (font->mapping) {
		CloseHandle(font->mapping);
		font->mapping = NULL;
	}
}

#else

static int
map_file(SFT_Font *font, const char *filename)
{
	struct stat info;
	int fd;
	font->memory = MAP_FAILED;
	font->size   = 0;
	font->source = SrcMapping;
	if ((fd = open(filename, O_RDONLY)) < 0) {
		return -1;
	}
	if (fstat(fd, &info) < 0) {
		close(fd);
		return -1;
	}
	/* FIXME do some basic validation on info.st_size maybe - it is signed for example, so it *could* be negative .. */
	font->memory = mmap(NULL, (size_t) info.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	font->size   = (uint_fast32_t) info.st_size;
	close(fd);
	return font->memory == MAP_FAILED ? -1 : 0;
}

static void
unmap_file(SFT_Font *font)
{
	assert(font->memory != MAP_FAILED);
	munmap((void *) font->memory, font->size);
}

#endif

static int
init_font(SFT_Font *font)
{
	uint_fast32_t scalerType, head, hhea;

	if (!is_safe_offset(font, 0, 12))
		return -1;
	/* Check for a compatible scalerType (magic number). */
	scalerType = getu32(font, 0);
	if (scalerType != FILE_MAGIC_ONE && scalerType != FILE_MAGIC_TWO)
		return -1;

	if (gettable(font, "head", &head) < 0)
		return -1;
	if (!is_safe_offset(font, head, 54))
		return -1;
	font->unitsPerEm = getu16(font, head + 18);
	font->locaFormat = geti16(font, head + 50);
	
	if (gettable(font, "hhea", &hhea) < 0)
		return -1;
	if (!is_safe_offset(font, hhea, 36))
		return -1;
	font->numLongHmtx = getu16(font, hhea + 34);

	return 0;
}

static inline void midpoint(Point* pts, uint_least16_t a, uint_least16_t b, uint_least16_t c)
{
    pts[c].x = (pts[a].x + pts[b].x) / 2;
    pts[c].y = (pts[a].y + pts[b].y) / 2;
}

// inline clip point!
static inline void clip_point(Point* pt, Float_t width, Float_t height)
{
    if (pt->x < FloatConst(0.0)) {
	pt->x = FloatConst(0.0);
    }
    else if (pt->x >= width) {
	pt->x = NextAfter(width, FloatConst(0.0));
    }
    if (pt->y < FloatConst(0.0)) {
	pt->y = FloatConst(0.0);
    }
    else if (pt->y >= height) {
	pt->y = NextAfter(height, FloatConst(0.0));
    }
}

/*
static void
clip_points(unsigned int numPts, Point *pts,
	    Float_t width, Float_t height)
{
    while(numPts--) {
	clip_point(pts, width, height);
	pts++;
    }
}
*/

static inline void transform_xy(Float_t* px, Float_t* py, SFT_Transform trf)
{
    Float_t x = *px;
    Float_t y = *py;
    *px = x*trf[SFT_SX] + y*trf[SFT_RY] + trf[SFT_TX];
    *py = x*trf[SFT_RX] + y*trf[SFT_SY] + trf[SFT_TY];
}

// inline transform a point 
static inline void transform_point(Point* p, SFT_Transform trf)
{
    transform_xy(&p->x, &p->y, trf);
}

/* Applies an affine linear transformation matrix to a set of points. */

static void
transform_points(unsigned int numPts, Point *pptr, SFT_Transform trf)
{
    while(numPts--) {
	transform_point(pptr, trf);
	pptr++;
    }
}

static void
transform_and_clip_points(unsigned int numPts, Point *pts, SFT_Transform trf,
			  Float_t width, Float_t height)
{
    while(numPts--) {
	Float_t x = pts->x, y = pts->y;
	transform_xy(&x, &y, trf);
	if (x < FloatConst(0.0)) x = FloatConst(0.0);
	else if (x >= width) x = NextAfter(width, FloatConst(0.0));
	if (y < FloatConst(0.0)) y = FloatConst(0.0);
	else if (y >= height) y = NextAfter(height, FloatConst(0.0));
	pts->x = x;
	pts->y = y;
	pts++;
    }
}

static int
init_outline(Outline *outl)
{
	/* TODO Smaller initial allocations */
	outl->numPoints = 0;
	outl->capPoints = 64;
	if (!(outl->points = malloc(outl->capPoints * sizeof *outl->points)))
		return -1;
	outl->numCurves = 0;
	outl->capCurves = 64;
	if (!(outl->curves = malloc(outl->capCurves * sizeof *outl->curves)))
		return -1;
	outl->numLines = 0;
	outl->capLines = 64;
	if (!(outl->lines = malloc(outl->capLines * sizeof *outl->lines)))
		return -1;
	return 0;
}

static void
free_outline(Outline *outl)
{
	free(outl->points);
	free(outl->curves);
	free(outl->lines);
}

static int
grow_points(Outline *outl)
{
	void *mem;
	uint_fast16_t cap;
	assert(outl->capPoints);
	/* Since we use uint_fast16_t for capacities, we have to be extra careful not to trigger integer overflow. */
	if (outl->capPoints > UINT16_MAX / 2)
		return -1;
	cap = (uint_fast16_t) (2U * outl->capPoints);
	if (!(mem = realloc_array(outl->points, cap, sizeof *outl->points)))
		return -1;
	outl->capPoints = (uint_least16_t) cap;
	outl->points    = mem;
	return 0;
}

static int
grow_curves(Outline *outl)
{
	void *mem;
	uint_fast16_t cap;
	assert(outl->capCurves);
	if (outl->capCurves > UINT16_MAX / 2)
		return -1;
	cap = (uint_fast16_t) (2U * outl->capCurves);
	if (!(mem = realloc_array(outl->curves, cap, sizeof *outl->curves)))
		return -1;
	outl->capCurves = (uint_least16_t) cap;
	outl->curves    = mem;
	return 0;
}

static int
grow_lines(Outline *outl)
{
	void *mem;
	uint_fast16_t cap;
	assert(outl->capLines);
	if (outl->capLines > UINT16_MAX / 2)
		return -1;
	cap = (uint_fast16_t) (2U * outl->capLines);
	if (!(mem = realloc_array(outl->lines, cap, sizeof *outl->lines)))
		return -1;
	outl->capLines = (uint_least16_t) cap;
	outl->lines    = mem;
	return 0;
}

static inline int
is_safe_offset(SFT_Font *font, uint_fast32_t offset, uint_fast32_t margin)
{
	if (offset > font->size) return 0;
	if (font->size - offset < margin) return 0;
	return 1;
}

/* Like bsearch(), but returns the next highest element if key could not be found. */
static void *
csearch(const void *key, const void *base,
	size_t nmemb, size_t size,
	int (*compar)(const void *, const void *))
{
	const uint8_t *bytes = base, *sample;
	size_t low = 0, high = nmemb - 1, mid;
	if (!nmemb) return NULL;
	while (low != high) {
		mid = low + (high - low) / 2;
		sample = bytes + mid * size;
		if (compar(key, sample) > 0) {
			low = mid + 1;
		} else {
			high = mid;
		}
	}
	return (uint8_t *) bytes + low * size;
}

/* Used as a comparison function for [bc]search(). */
static int
cmpu16(const void *a, const void *b)
{
	return memcmp(a, b, 2);
}

/* Used as a comparison function for [bc]search(). */
static int
cmpu32(const void *a, const void *b)
{
	return memcmp(a, b, 4);
}

static inline uint_least8_t
getu8(SFT_Font *font, uint_fast32_t offset)
{
	assert(offset + 1 <= font->size);
	return *(font->memory + offset);
}

static inline int_least8_t
geti8(SFT_Font *font, uint_fast32_t offset)
{
	return (int_least8_t) getu8(font, offset);
}

static inline uint_least16_t
getu16(SFT_Font *font, uint_fast32_t offset)
{
	assert(offset + 2 <= font->size);
	const uint8_t *base = font->memory + offset;
	uint_least16_t b1 = base[0], b0 = base[1]; 
	return (uint_least16_t) (b1 << 8 | b0);
}

static inline int16_t
geti16(SFT_Font *font, uint_fast32_t offset)
{
	return (int_least16_t) getu16(font, offset);
}

static inline uint32_t
getu32(SFT_Font *font, uint_fast32_t offset)
{
	assert(offset + 4 <= font->size);
	const uint8_t *base = font->memory + offset;
	uint_least32_t b3 = base[0], b2 = base[1], b1 = base[2], b0 = base[3]; 
	return (uint_least32_t) (b3 << 24 | b2 << 16 | b1 << 8 | b0);
}

// *debug *
void sft_print_table_names(FILE* f, SFT_Font *font)
{
    unsigned int i,n;
    uint32_t offs = 12;
    const char* ptr = (char*)font->memory + offs;
    
    n = getu16(font, 4);
    for (i = 0; i < n; i++) {
	uint32_t moffs = getu32(font, offs+8);
	fprintf(f, "%c%c%c%c: %d\r\n", ptr[0],ptr[1],ptr[2],ptr[3],moffs);
	ptr += 16;
	offs += 16;
    }
}

static void print_name(const char* ptr, size_t len)
{
    while(len--) {
	int c = *ptr++;
	if (isprint(c))
	    putchar(c);
	else
	    printf("x%02x", c);
    }
}

void sft_print_name_table(FILE* f, SFT_Font *font)
{
    uint_fast32_t name;
    uint_least16_t count;
    uint_least16_t offs;
    uint_least16_t string_offs;
    int i;
    
    if (gettable(font, "name", &name) < 0)
	return;
    fprintf(f, "format: %d\r\n", geti16(font, name+0));
    count = getu16(font, name+2);
    fprintf(f, "count: %d\r\n", count);
    string_offs = getu16(font, name+4);
    fprintf(f, "soffs: %d\r\n", string_offs);

    offs = 6;
    for (i = 0; i < count; i++) {
	uint16_t platformID = getu16(font, name+offs+0);
	uint16_t platformSpecificID = getu16(font, name+offs+2);
	uint16_t languageID = getu16(font, name+offs+4);
	uint16_t nameID = getu16(font, name+offs+6);
	uint16_t length = getu16(font, name+offs+8);
	uint16_t offset = getu16(font, name+offs+10);

	fprintf(f, "nameRecord[%d] {\r\n", i);

	fprintf(f, "  platformID: %d\r\n", platformID);
	fprintf(f, "  platformSpecificID: %d\r\n", platformSpecificID);
	fprintf(f, "  languageID: %d\r\n", languageID);
	fprintf(f, "  nameID: %d\r\n", nameID);
	fprintf(f, "  length: %d\r\n", length);
	fprintf(f, "  offset: %d\r\n", offset);
	fprintf(f, "  string: ");
	print_name((char*)font->memory+name+string_offs+offset,length);
	printf("\r\n");
	fprintf(f, "};\r\n");
	offs += 6*2;
    }
}

// return the "mapped" string with nameID in name table
// return length in len_ptr
char* sft_name(SFT_Font *font, uint16_t nameID, uint16_t* len_ptr)
{
    uint_fast32_t name;
    uint_least16_t count;
    uint_least16_t offs;
    uint_least16_t string_offs;
    int i;
    
    if (gettable(font, "name", &name) < 0)
	return NULL;
    count = getu16(font, name+2);
    string_offs = getu16(font, name+4);
    // FIXME: check safty... count*12 entries + string table size

    offs = 6;
    for (i = 0; i < count; i++) {
	//uint16_t platformID = getu16(font, name+offs+0);
	//uint16_t platformSpecificID = getu16(font, name+offs+2);
	//uint16_t languageID = getu16(font, name+offs+4);
	uint16_t nameID1 = getu16(font, name+offs+6);

	if (nameID == nameID1) {
	    uint16_t offset = getu16(font, name+offs+10);
	    *len_ptr = getu16(font, name+offs+8);
	    return (char*) font->memory+name+string_offs+offset;
	}
	offs += 6*2;
    }
    return NULL;
}


static int
gettable(SFT_Font *font, char tag[4], uint_fast32_t *offset)
{
	void *match;
	unsigned int numTables;
	/* No need to bounds-check access to the first 12 bytes - this gets already checked by init_font(). */
	numTables = getu16(font, 4);
	if (!is_safe_offset(font, 12, (uint_fast32_t) numTables * 16))
		return -1;
	if (!(match = bsearch(tag, font->memory + 12, numTables, 16, cmpu32)))
		return -1;
	*offset = getu32(font, (uint_fast32_t) ((uint8_t *) match - font->memory + 8));
	return 0;
}

static int
cmap_fmt4(SFT_Font *font, uint_fast32_t table, SFT_UChar charCode, SFT_Glyph *glyph)
{
	const uint8_t *segPtr;
	uint_fast32_t segIdxX2;
	uint_fast32_t endCodes, startCodes, idDeltas, idRangeOffsets, idOffset;
	uint_fast16_t segCountX2, idRangeOffset, startCode, shortCode, idDelta, id;
	uint8_t key[2] = { (uint8_t) (charCode >> 8), (uint8_t) charCode };
	/* cmap format 4 only supports the Unicode BMP. */
	if (charCode > 0xFFFF) {
		*glyph = 0;
		return 0;
	}
	shortCode = (uint_fast16_t) charCode;
	if (!is_safe_offset(font, table, 8))
		return -1;
	segCountX2 = getu16(font, table);
	if ((segCountX2 & 1) || !segCountX2)
		return -1;
	/* Find starting positions of the relevant arrays. */
	endCodes       = table + 8;
	startCodes     = endCodes + segCountX2 + 2;
	idDeltas       = startCodes + segCountX2;
	idRangeOffsets = idDeltas + segCountX2;
	if (!is_safe_offset(font, idRangeOffsets, segCountX2))
		return -1;
	/* Find the segment that contains shortCode by binary searching over
	 * the highest codes in the segments. */
	segPtr = csearch(key, font->memory + endCodes, segCountX2 / 2, 2, cmpu16);
	segIdxX2 = (uint_fast32_t) (segPtr - (font->memory + endCodes));
	/* Look up segment info from the arrays & short circuit if the spec requires. */
	if ((startCode = getu16(font, startCodes + segIdxX2)) > shortCode)
		return 0;
	idDelta = getu16(font, idDeltas + segIdxX2);
	if (!(idRangeOffset = getu16(font, idRangeOffsets + segIdxX2))) {
		/* Intentional integer under- and overflow. */
		*glyph = (shortCode + idDelta) & 0xFFFF;
		return 0;
	}
	/* Calculate offset into glyph array and determine ultimate value. */
	idOffset = idRangeOffsets + segIdxX2 + idRangeOffset + 2U * (unsigned int) (shortCode - startCode);
	if (!is_safe_offset(font, idOffset, 2))
		return -1;
	id = getu16(font, idOffset);
	/* Intentional integer under- and overflow. */
	*glyph = id ? (id + idDelta) & 0xFFFF : 0;
	return 0;
}

static int
cmap_fmt6(SFT_Font *font, uint_fast32_t table, SFT_UChar charCode, SFT_Glyph *glyph)
{
	unsigned int firstCode, entryCount;
	/* cmap format 6 only supports the Unicode BMP. */
	if (charCode > 0xFFFF) {
		*glyph = 0;
		return 0;
	}
	if (!is_safe_offset(font, table, 4))
		return -1;
	firstCode  = getu16(font, table);
	entryCount = getu16(font, table + 2);
	if (!is_safe_offset(font, table, 4 + 2 * entryCount))
		return -1;
	if (charCode < firstCode)
		return -1;
	charCode -= firstCode;
	if (!(charCode < entryCount))
		return -1;
	*glyph = getu16(font, table + 4 + 2 * charCode);
	return 0;
}

static int
cmap_fmt12_13(SFT_Font *font, uint_fast32_t table, SFT_UChar charCode, SFT_Glyph *glyph, int which)
{
	uint32_t len, numEntries;
	uint_fast32_t i;

	*glyph = 0;

    /* check that the entire header is present */
	if (!is_safe_offset(font, table, 16))
		return -1;

	len = getu32(font, table + 4);

	/* A minimal header is 16 bytes */
	if (len < 16)
		return -1;

	if (!is_safe_offset(font, table, len))
		return -1;

	numEntries = getu32(font, table + 12);

	for (i = 0; i < numEntries; ++i) {
		uint32_t firstCode, lastCode, glyphOffset;
		firstCode = getu32(font, table + (i * 12) + 16);
		lastCode = getu32(font, table + (i * 12) + 16 + 4);
		if (charCode < firstCode || charCode > lastCode)
			continue;
		glyphOffset = getu32(font, table + (i * 12) + 16 + 8);
		if (which == 12)
			*glyph = (charCode-firstCode) + glyphOffset;
		else
			*glyph = glyphOffset;
		return 0;
	}

	return 0;
}

/* Maps Unicode code points to glyph indices. */
static int
glyph_id(SFT_Font *font, SFT_UChar charCode, SFT_Glyph *glyph)
{
	uint_fast32_t cmap, entry, table;
	unsigned int idx, numEntries;
	int type, format;
	
	*glyph = 0;

	if (gettable(font, "cmap", &cmap) < 0)
		return -1;

	if (!is_safe_offset(font, cmap, 4))
		return -1;
	numEntries = getu16(font, cmap + 2);

	if (!is_safe_offset(font, cmap, 4 + numEntries * 8))
		return -1;

	/* First look for a 'full repertoire'/non-BMP map. */
	for (idx = 0; idx < numEntries; ++idx) {
		entry = cmap + 4 + idx * 8;
		type = getu16(font, entry) * 0100 + getu16(font, entry + 2);
		/* Complete unicode map */
		if (type == 0004 || type == 0312) {
			table = cmap + getu32(font, entry + 4);
			if (!is_safe_offset(font, table, 8))
				return -1;
			/* Dispatch based on cmap format. */
			format = getu16(font, table);
			switch (format) {
			case 12:
				return cmap_fmt12_13(font, table, charCode, glyph, 12);
			default:
				return -1;
			}
		}
	}

	/* If no 'full repertoire' cmap was found, try looking for a BMP map. */
	for (idx = 0; idx < numEntries; ++idx) {
		entry = cmap + 4 + idx * 8;
		type = getu16(font, entry) * 0100 + getu16(font, entry + 2);
		/* Unicode BMP */
		if (type == 0003 || type == 0301) {
			table = cmap + getu32(font, entry + 4);
			if (!is_safe_offset(font, table, 6))
				return -1;
			/* Dispatch based on cmap format. */
			switch (getu16(font, table)) {
			case 4:
				return cmap_fmt4(font, table + 6, charCode, glyph);
			case 6:
				return cmap_fmt6(font, table + 6, charCode, glyph);
			default:
				return -1;
			}
		}
	}

	return -1;
}

static int
hor_metrics(SFT_Font *font, SFT_Glyph glyph, int *advanceWidth, int *leftSideBearing)
{
	uint_fast32_t hmtx, offset, boundary;
	if (gettable(font, "hmtx", &hmtx) < 0)
		return -1;
	if (glyph < font->numLongHmtx) {
		/* glyph is inside long metrics segment. */
		offset = hmtx + 4 * glyph;
		if (!is_safe_offset(font, offset, 4))
			return -1;
		*advanceWidth = getu16(font, offset);
		*leftSideBearing = geti16(font, offset + 2);
		return 0;
	} else {
		/* glyph is inside short metrics segment. */
		boundary = hmtx + 4U * (uint_fast32_t) font->numLongHmtx;
		if (boundary < 4)
			return -1;
		
		offset = boundary - 4;
		if (!is_safe_offset(font, offset, 4))
			return -1;
		*advanceWidth = getu16(font, offset);
		
		offset = boundary + 2 * (glyph - font->numLongHmtx);
		if (!is_safe_offset(font, offset, 2))
			return -1;
		*leftSideBearing = geti16(font, offset);
		return 0;
	}
}

static int
glyph_bbox(const SFT *sft, uint_fast32_t outline, int box[4])
{
	Float_t xScale, yScale;
	/* Read the bounding box from the font file verbatim. */
	if (!is_safe_offset(sft->font, outline, 10))
		return -1;
	box[0] = geti16(sft->font, outline + 2);
	box[1] = geti16(sft->font, outline + 4);
	box[2] = geti16(sft->font, outline + 6);
	box[3] = geti16(sft->font, outline + 8);
	if (box[2] <= box[0] || box[3] <= box[1])
		return -1;
	/* Transform the bounding box into SFT coordinate space. */
	xScale = sft->xScale / sft->font->unitsPerEm;
	yScale = sft->yScale / sft->font->unitsPerEm;
	box[0] = (int) FloatFloor(box[0] * xScale + sft->xOffset);
	box[1] = (int) FloatFloor(box[1] * yScale + sft->yOffset);
	box[2] = (int) FloatCeil (box[2] * xScale + sft->xOffset);
	box[3] = (int) FloatCeil (box[3] * yScale + sft->yOffset);
	return 0;
}

/* Returns the offset into the font that the glyph's outline is stored at. */
static int
outline_offset(SFT_Font *font, SFT_Glyph glyph, uint_fast32_t *offset)
{
	uint_fast32_t loca, glyf;
	uint_fast32_t base, this, next;

	if (gettable(font, "loca", &loca) < 0)
		return -1;
	if (gettable(font, "glyf", &glyf) < 0)
		return -1;

	if (font->locaFormat == 0) {
		base = loca + 2 * glyph;

		if (!is_safe_offset(font, base, 4))
			return -1;
		
		this = 2U * (uint_fast32_t) getu16(font, base);
		next = 2U * (uint_fast32_t) getu16(font, base + 2);
	} else {
		base = loca + 4 * glyph;

		if (!is_safe_offset(font, base, 8))
			return -1;

		this = getu32(font, base);
		next = getu32(font, base + 4);
	}

	*offset = this == next ? 0 : glyf + this;
	return 0;
}

/* For a 'simple' outline, determines each point of the outline with a set of flags. */
static int
simple_flags(SFT_Font *font, uint_fast32_t *offset, uint_fast16_t numPts, uint8_t *flags)
{
	uint_fast32_t off = *offset;
	uint_fast16_t i;
	uint8_t value = 0, repeat = 0;
	for (i = 0; i < numPts; ++i) {
		if (repeat) {
			--repeat;
		} else {
			if (!is_safe_offset(font, off, 1))
				return -1;
			value = getu8(font, off++);
			if (value & REPEAT_FLAG) {
				if (!is_safe_offset(font, off, 1))
					return -1;
				repeat = getu8(font, off++);
			}
		}
		flags[i] = value;
	}
	*offset = off;
	return 0;
}

/* For a 'simple' outline, decodes both X and Y coordinates for each point of the outline. */
static int
simple_points(SFT_Font *font, uint_fast32_t offset, uint_fast16_t numPts, uint8_t *flags, Point *points)
{
	long accum, value, bit;
	uint_fast16_t i;

	accum = 0L;
	for (i = 0; i < numPts; ++i) {
		if (flags[i] & X_CHANGE_IS_SMALL) {
			if (!is_safe_offset(font, offset, 1))
				return -1;
			value = (long) getu8(font, offset++);
			bit = !!(flags[i] & X_CHANGE_IS_POSITIVE);
			accum -= (value ^ -bit) + bit;
		} else if (!(flags[i] & X_CHANGE_IS_ZERO)) {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			accum += geti16(font, offset);
			offset += 2;
		}
		points[i].x = (Float_t) accum;
	}

	accum = 0L;
	for (i = 0; i < numPts; ++i) {
		if (flags[i] & Y_CHANGE_IS_SMALL) {
			if (!is_safe_offset(font, offset, 1))
				return -1;
			value = (long) getu8(font, offset++);
			bit = !!(flags[i] & Y_CHANGE_IS_POSITIVE);
			accum -= (value ^ -bit) + bit;
		} else if (!(flags[i] & Y_CHANGE_IS_ZERO)) {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			accum += geti16(font, offset);
			offset += 2;
		}
		points[i].y = (Float_t) accum;
	}

	return 0;
}

static int
decode_contour(uint8_t *flags, uint_fast16_t basePoint, uint_fast16_t count, Outline *outl)
{
	uint_fast16_t i;
	uint_least16_t looseEnd, beg, ctrl, center, cur;
	unsigned int gotCtrl;

	/* Skip contours with less than two points, since the following algorithm can't handle them and
	 * they should appear invisible either way (because they don't have any area). */
	if (count < 2) return 0;

	assert(basePoint <= UINT16_MAX - count);

	if (flags[0] & POINT_IS_ON_CURVE) {
		looseEnd = (uint_least16_t) basePoint++;
		++flags;
		--count;
	} else if (flags[count - 1] & POINT_IS_ON_CURVE) {
		looseEnd = (uint_least16_t) (basePoint + --count);
	} else {
		if (outl->numPoints >= outl->capPoints && grow_points(outl) < 0)
			return -1;

		looseEnd = outl->numPoints;
		midpoint(outl->points,
			 basePoint, basePoint + count - 1, outl->numPoints);
		outl->numPoints++;
	}
	beg = looseEnd;
	gotCtrl = 0;
	for (i = 0; i < count; ++i) {
		/* cur can't overflow because we ensure that basePoint + count < 0xFFFF before calling decode_contour(). */
		cur = (uint_least16_t) (basePoint + i);
		/* NOTE clang-analyzer will often flag this and another piece of code because it thinks that flags and
		 * outl->points + basePoint don't always get properly initialized -- even when you explicitly loop over both
		 * and set every element to zero (but not when you use memset). This is a known clang-analyzer bug:
		 * http://clang-developers.42468.n3.nabble.com/StaticAnalyzer-False-positive-with-loop-handling-td4053875.html */
		if (flags[i] & POINT_IS_ON_CURVE) {
			if (gotCtrl) {
				if (outl->numCurves >= outl->capCurves && grow_curves(outl) < 0)
					return -1;
				outl->curves[outl->numCurves++] = (Curve) { beg, cur, ctrl };
			} else {
				if (outl->numLines >= outl->capLines && grow_lines(outl) < 0)
					return -1;
				outl->lines[outl->numLines++] = (Line) { beg, cur };
			}
			beg = cur;
			gotCtrl = 0;
		} else {
			if (gotCtrl) {
				center = outl->numPoints;
				if (outl->numPoints >= outl->capPoints && grow_points(outl) < 0)
					return -1;
				
				midpoint(outl->points, ctrl, cur, center);
				++outl->numPoints;

				if (outl->numCurves >= outl->capCurves && grow_curves(outl) < 0)
					return -1;
				outl->curves[outl->numCurves++] = (Curve) { beg, center, ctrl };

				beg = center;
			}
			ctrl = cur;
			gotCtrl = 1;
		}
	}
	if (gotCtrl) {
		if (outl->numCurves >= outl->capCurves && grow_curves(outl) < 0)
			return -1;
		outl->curves[outl->numCurves++] = (Curve) { beg, looseEnd, ctrl };
	} else {
		if (outl->numLines >= outl->capLines && grow_lines(outl) < 0)
			return -1;
		outl->lines[outl->numLines++] = (Line) { beg, looseEnd };
	}

	return 0;
}

static int
simple_outline(SFT_Font *font, uint_fast32_t offset, unsigned int numContours, Outline *outl)
{
	uint_fast16_t *endPts = NULL;
	uint8_t *flags = NULL;
	uint_fast16_t numPts;
	unsigned int i;

	assert(numContours > 0);

	uint_fast16_t basePoint = outl->numPoints;

	if (!is_safe_offset(font, offset, numContours * 2 + 2))
		goto failure;
	numPts = getu16(font, offset + (numContours - 1) * 2);
	if (numPts >= UINT16_MAX)
		goto failure;
	numPts++;
	if (outl->numPoints > UINT16_MAX - numPts)
		goto failure;

	while (outl->capPoints < basePoint + numPts) {
		if (grow_points(outl) < 0)
			goto failure;
	}
	
	STACK_ALLOC(endPts, uint_fast16_t, 16, numContours);
	if (endPts == NULL)
		goto failure;
	STACK_ALLOC(flags, uint8_t, 128, numPts);
	if (flags == NULL)
		goto failure;

	for (i = 0; i < numContours; ++i) {
		endPts[i] = getu16(font, offset);
		offset += 2;
	}
	/* Ensure that endPts are never falling.
	 * Falling endPts have no sensible interpretation and most likely only occur in malicious input.
	 * Therefore, we bail, should we ever encounter such input. */
	for (i = 0; i < numContours - 1; ++i) {
		if (endPts[i + 1] < endPts[i] + 1)
			goto failure;
	}
	offset += 2U + getu16(font, offset);

	if (simple_flags(font, &offset, numPts, flags) < 0)
		goto failure;
	if (simple_points(font, offset, numPts, flags, outl->points + basePoint) < 0)
		goto failure;
	outl->numPoints = (uint_least16_t) (outl->numPoints + numPts);

	uint_fast16_t beg = 0;
	for (i = 0; i < numContours; ++i) {
		uint_fast16_t count = endPts[i] - beg + 1;
		if (decode_contour(flags + beg, basePoint + beg, count, outl) < 0)
			goto failure;
		beg = endPts[i] + 1;
	}

	STACK_FREE(endPts);
	STACK_FREE(flags);
	return 0;
failure:
	STACK_FREE(endPts);
	STACK_FREE(flags);
	return -1;
}

static inline Float_t field16(SFT_Font *font, uint_fast32_t offset)
{
    return (Float_t)geti16(font, offset) / FloatConst(16384.0);
}


static int
compound_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl)
{
	Float_t local[6];
	uint_fast32_t outline;
	unsigned int flags, glyph, basePoint;
	/* Guard against infinite recursion (compound glyphs that have themselves as component). */
	if (recDepth >= 4)
		return -1;
	do {
		memset(local, 0, sizeof local);
		if (!is_safe_offset(font, offset, 4))
			return -1;
		flags = getu16(font, offset);
		glyph = getu16(font, offset + 2);
		offset += 4;
		/* We don't implement point matching, and neither does stb_truetype for that matter. */
		if (!(flags & ACTUAL_XY_OFFSETS))
			return -1;
		/* Read additional X and Y offsets (in FUnits) of this component. */
		if (flags & OFFSETS_ARE_LARGE) {
			if (!is_safe_offset(font, offset, 4))
				return -1;
			local[SFT_TX] = geti16(font, offset);
			local[SFT_TY] = geti16(font, offset + 2);
			offset += 4;
		} else {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			local[SFT_TX] = geti8(font, offset);
			local[SFT_TY] = geti8(font, offset + 1);
			offset += 2;
		}
		if (flags & GOT_A_SINGLE_SCALE) {
			if (!is_safe_offset(font, offset, 2))
				return -1;
			local[SFT_SX] = field16(font, offset);
			local[SFT_SY] = local[SFT_SX];
			offset += 2;
		} else if (flags & GOT_AN_X_AND_Y_SCALE) {
			if (!is_safe_offset(font, offset, 4))
				return -1;
			local[SFT_SX] = field16(font, offset + 0);
			local[SFT_SY] = field16(font, offset + 2);
			offset += 4;
		} else if (flags & GOT_A_SCALE_MATRIX) {
			if (!is_safe_offset(font, offset, 8))
				return -1;
			local[SFT_SX] = field16(font, offset + 0);
			local[SFT_RX] = field16(font, offset + 2);
			local[SFT_RY] = field16(font, offset + 4);
			local[SFT_SY] = field16(font, offset + 6);
			offset += 8;
		} else {
			local[SFT_SX] = 1.0;
			local[SFT_SY] = 1.0;
		}
		/* At this point, Apple's spec more or less tells you to scale the matrix by its own L1 norm.
		 * But stb_truetype scales by the L2 norm. And FreeType2 doesn't scale at all.
		 * Furthermore, Microsoft's spec doesn't even mention anything like this.
		 * It's almost as if nobody ever uses this feature anyway. */
		if (outline_offset(font, glyph, &outline) < 0)
			return -1;
		if (outline) {
			basePoint = outl->numPoints;
			if (decode_outline(font, outline, recDepth + 1, outl) < 0)
				return -1;
			transform_points(outl->numPoints - basePoint, outl->points + basePoint, local);
		}
	} while (flags & THERE_ARE_MORE_COMPONENTS);

	return 0;
}

static int
decode_outline(SFT_Font *font, uint_fast32_t offset, int recDepth, Outline *outl)
{
	int numContours;
	if (!is_safe_offset(font, offset, 10))
		return -1;
	numContours = geti16(font, offset);
	if (numContours > 0) {
		/* Glyph has a 'simple' outline consisting of a number of contours. */
		return simple_outline(font, offset + 10, (unsigned int) numContours, outl);
	} else if (numContours < 0) {
		/* Glyph has a compound outline combined from mutiple other outlines. */
		return compound_outline(font, offset + 10, recDepth, outl);
	} else {
		return 0;
	}
}

/* A heuristic to tell whether a given curve can be approximated closely enough by a line. */
static int
is_flat(Outline *outl, Curve curve)
{
	const Float_t maxArea2 = 2.0;
	// Point a = outl->points[curve.beg];
	Float_t ax = outl->points[curve.beg].x;
	Float_t bx = outl->points[curve.ctrl].x;	
	Float_t gx = bx-ax;
	Float_t cx = outl->points[curve.end].x;	
	Float_t hx = cx-ax;
	
	Float_t ay = outl->points[curve.beg].y;
	Float_t by = outl->points[curve.ctrl].y;
	Float_t gy = by-ay;
	Float_t cy = outl->points[curve.end].y;	
	Float_t hy = cy-ay;

	Float_t area2 = FloatAbs(gx*hy-hx*gy);
	return area2 <= maxArea2;
}

static int
tesselate_curve(Curve curve, Outline *outl)
{
	/* From my tests I can conclude that this stack barely reaches a top height
	 * of 4 elements even for the largest font sizes I'm willing to support. And
	 * as space requirements should only grow logarithmically, I think 10 is
	 * more than enough. */
#define STACK_SIZE 10
	Curve stack[STACK_SIZE];
	unsigned int top = 0;
	for (;;) {
		if (is_flat(outl, curve) || top >= STACK_SIZE) {
			if (outl->numLines >= outl->capLines && grow_lines(outl) < 0)
				return -1;
			outl->lines[outl->numLines++] = (Line) { curve.beg, curve.end };
			if (top == 0) break;
			curve = stack[--top];
		} else {
			uint_least16_t ctrl0 = outl->numPoints;
			if (outl->numPoints >= outl->capPoints && grow_points(outl) < 0)
				return -1;
			midpoint(outl->points,curve.beg,curve.ctrl,ctrl0);
			++outl->numPoints;

			uint_least16_t ctrl1 = outl->numPoints;
			if (outl->numPoints >= outl->capPoints && grow_points(outl) < 0)
				return -1;
			midpoint(outl->points,curve.ctrl,curve.end,ctrl1);
			++outl->numPoints;

			uint_least16_t pivot = outl->numPoints;
			if (outl->numPoints >= outl->capPoints && grow_points(outl) < 0)
				return -1;
			midpoint(outl->points, ctrl0, ctrl1, pivot);
			++outl->numPoints;

			stack[top++] = (Curve) { curve.beg, pivot, ctrl0 };
			curve = (Curve) { pivot, curve.end, ctrl1 };
		}
	}
	return 0;
#undef STACK_SIZE
}

static int
tesselate_curves(Outline *outl)
{
	unsigned int i;
	for (i = 0; i < outl->numCurves; ++i) {
		if (tesselate_curve(outl->curves[i], outl) < 0)
			return -1;
	}
	return 0;
}

/* Draws a line into the buffer. Uses a custom 2D raycasting algorithm to do so. */
static void
draw_line(Raster buf, Float_t x0, Float_t y0, Float_t x1, Float_t y1)
{
    Point delta;
    Point nextCrossing;
    Point crossingIncr;
    Float_t halfDeltaX;
    Float_t prevDistance = FloatConst(0.0), nextDistance;
    Float_t xAverage, yDifference;
    struct { int x, y; } pixel;
    struct { int x, y; } dir;
    int step, numSteps = 0;
    Cell *restrict cptr, cell;

    delta.x = x1 - x0;
    delta.y = y1 - y0;
    dir.x = SIGN(delta.x);
    dir.y = SIGN(delta.y);

    if (!dir.y) {
	return;
    }

    crossingIncr.x = dir.x ? FloatAbs(FloatConst(1.0) / delta.x) : FloatConst(1.0);
    crossingIncr.y = FloatAbs(FloatConst(1.0) / delta.y);

    if (!dir.x) {
	pixel.x = fast_floor(x0);
	nextCrossing.x = FloatConst(100.0);
    } else {
	if (dir.x > 0) {
	    pixel.x = fast_floor(x0);
	    nextCrossing.x = (x0 - (Float_t)pixel.x) * crossingIncr.x;
	    nextCrossing.x = crossingIncr.x - nextCrossing.x;
	    numSteps += fast_ceil(x1) - fast_floor(x0) - 1;
	} else {
	    pixel.x = fast_ceil(x0) - 1;
	    nextCrossing.x = (x0 - (Float_t)pixel.x) * crossingIncr.x;
	    numSteps += fast_ceil(x0) - fast_floor(x1) - 1;
	}
    }

    if (dir.y > 0) {
	pixel.y = fast_floor(y0);
	nextCrossing.y = (Float_t)(y0 - (Float_t)pixel.y) * crossingIncr.y;
	nextCrossing.y = crossingIncr.y - nextCrossing.y;
	numSteps += fast_ceil(y1) - fast_floor(y0) - 1;
    } else {
	pixel.y = fast_ceil(y0) - 1;
	nextCrossing.y = (Float_t)(y0 - (Float_t) pixel.y) * crossingIncr.y;
	numSteps += fast_ceil(y0) - fast_floor(y1) - 1;
    }

    nextDistance = MIN(nextCrossing.x, nextCrossing.y);
    halfDeltaX = FloatConst(0.5) * delta.x;

    for (step = 0; step < numSteps; ++step) {
	xAverage = x0 + (prevDistance + nextDistance) * halfDeltaX;
	yDifference = (nextDistance - prevDistance) * delta.y;
	cptr = &buf.cells[pixel.y * buf.width + pixel.x];
	cell = *cptr;
	cell.cover += yDifference;
	xAverage -= (Float_t)pixel.x;
	cell.area += (FloatConst(1.0) - xAverage) * yDifference;
	*cptr = cell;
	prevDistance = nextDistance;
	int alongX = nextCrossing.x < nextCrossing.y;
	pixel.x += alongX ? dir.x : 0;
	pixel.y += alongX ? 0 : dir.y;
	nextCrossing.x += alongX ? crossingIncr.x : FloatConst(0.0);
	nextCrossing.y += alongX ? FloatConst(0.0) : crossingIncr.y;
	nextDistance = MIN(nextCrossing.x, nextCrossing.y);
    }

    xAverage = x0 + (prevDistance + FloatConst(1.0)) * halfDeltaX;
    yDifference = (FloatConst(1.0) - prevDistance) * delta.y;
    cptr = &buf.cells[pixel.y * buf.width + pixel.x];
    cell = *cptr;
    cell.cover += yDifference;
    xAverage -= (Float_t) pixel.x;
    cell.area += (FloatConst(1.0) - xAverage) * yDifference;
    *cptr = cell;
}

static void
draw_lines(Outline *outl, Raster buf)
{
    unsigned int i;
    for (i = 0; i < outl->numLines; ++i) {
	Line  line   = outl->lines[i];
	Point* p0    = &outl->points[line.beg];
	Point* p1    = &outl->points[line.end];
	draw_line(buf, p0->x, p0->y, p1->x, p1->y);
    }
}

/* Integrate the values in the buffer to arrive at the final grayscale image. */
static void
post_process(Raster buf, SFT_Image* imptr)
{
    Cell cell;
    Float_t accum = FloatConst(0.0), value;
    unsigned int i;
    unsigned int h;
    unsigned int bw = buf.width;
    unsigned char* dst0 = (unsigned char*) imptr->pixels;
    unsigned int stride = imptr->bytes_per_row;
    unsigned int pxsize = imptr->bytes_per_pixel;
    uint16_t pxfmt  = imptr->pixel_format;
	
    i = imptr->yoffs * bw;  // skip cliped rows
    h = imptr->ylen;        // number of rows rendered
    // note that imptr->pixels must point to the first pixel
    // that is to be draw, taking into account clipping etc.
    while(h--) {
	unsigned int w = imptr->xlen;
	unsigned char* dst = dst0;
	int j = i + imptr->xoffs;   // skip clipped pixels
	while(w--) {
	    cell     = buf.cells[j++];
	    value    = FloatAbs(accum + cell.area);
	    value    = MIN(value, FloatConst(1.0));
	    value    = value * FloatConst(255.0) + FloatConst(0.5);
	    if (imptr->render == NULL) // assume L8 format
		*dst = (uint8_t) value;
	    else {
		imptr->render(dst,i,j,(uint8_t)value,pxfmt,imptr->render_arg);
	    }
	    dst += pxsize;
	    accum += cell.cover;
	}
	i += bw;
	dst0 += stride;
    }
}

static int
render_outline(Outline *outl, SFT_Transform trf, SFT_Image* imptr)
{
	Cell *cells = NULL;
	Raster buf;
	unsigned int numPixels;
	
	numPixels = (unsigned int) imptr->width * (unsigned int) imptr->height;

	STACK_ALLOC(cells, Cell, 128 * 128, numPixels);
	if (!cells) {
		return -1;
	}
	memset(cells, 0, numPixels * sizeof *cells);
	buf.cells  = cells;
	buf.width  = imptr->width;
	buf.height = imptr->height;

	transform_and_clip_points(outl->numPoints, outl->points, trf,
				  (Float_t) imptr->width,
				  (Float_t) imptr->height);

//	clip_points(outl->numPoints, outl->points,
//		    (Float_t) imptr->width, (Float_t) imptr->height);

	if (tesselate_curves(outl) < 0) {
		STACK_FREE(cells);
		return -1;
	}

	draw_lines(outl, buf);

	post_process(buf, imptr);

	STACK_FREE(cells);
	return 0;
}

