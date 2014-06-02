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
 *  EPX Font functions
 */
#include <sys/mman.h>
#include <sys/types.h>
#include <fcntl.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <wchar.h>
#include <errno.h>

#include "../include/epx_font.h"
#include "../include/epx_pixmap.h"
#include "../include/epx_gc.h"

char* epx_font_FILE_string(FILE* f,  epx_font_file_t* ff, uint32_t offset);
int epx_font_info_init(epx_font_t* font, epx_font_file_t* ff);


void epx_font_init(epx_font_t* font)
{
    EPX_OBJECT_INIT(font, EPX_FONT_TYPE);

    font->file_name = 0;
    font->file_size = 0;
    font->foundry_name = 0;
    font->family_name = 0;
    memset(&font->font_info, 0, sizeof(epx_font_info_t));
    font->font_file = 0;
    font->font_data = 0;
    font->font_map = 0;
}

epx_font_t* epx_font_create()
{
    epx_font_t* font;

    if ((font = (epx_font_t*) malloc(sizeof(epx_font_t))) == 0)
	return 0;
    epx_font_init(font);
    font->on_heap = 1;
    font->refc = 1;
    return font;
}

void EPX_FONT_TYPE_RELEASE(void* arg)
{
    epx_font_t* font = (epx_font_t*) arg;
    
    EPX_DBGFMT_MEM("EPX_FONT_TYPE_RELEASE: %p", arg);

    if (font->file_name)    free(font->file_name);
    if (font->foundry_name) free(font->foundry_name);
    if (font->family_name)  free(font->family_name);
    if (font->font_map != 0)
	munmap(font->font_map, font->file_size);
    else if (font->font_data != 0)
	free(font->font_data);
    if (font->on_heap)
	free(font);
}

void epx_font_destroy(epx_font_t* font)
{
    epx_object_unref(font);
}

char* epx_font_file_string(epx_font_file_t* ff, uint32_t offset)
{
    char* table = (char*)ff->data + U32LE(ff->string_table_start);
    // get string in string table (skip length indication)
    return table + offset + 1;
}

epx_glyph_t* epx_font_file_glyph(epx_font_file_t* ff, uint32_t encoding)
{
    uint32_t start = U32LE(ff->encoding_start);
    uint32_t stop  = U32LE(ff->encoding_stop);
    uint32_t* offset_table = 
	(uint32_t*) (((char*)ff->data) + U32LE(ff->offset_table_start));
    int offset;

    if ((encoding < start) || (encoding > stop)) {
	encoding = U32LE(ff->encoding_default);
	if ((encoding < start) || (encoding > stop))
	    return 0;
    }
    offset = U32LE(offset_table[encoding - start]);
    return (epx_glyph_t*) (((char*)ff->data) + offset);
}

// Initialize the font structure from font_file structure
// Do some basic checks on data, DO not handle the string table!
int epx_font_info_init(epx_font_t* font, epx_font_file_t* ff)
{
    if (memcmp(ff->magic, "EFNT", 4) != 0)
	return -1;

    font->file_name    = 0;
    font->file_size    = 0;
    font->foundry_name = 0;
    font->family_name  = 0;
    // Copy font info, convert to host endian
    font->font_info.weight       = U32LE(ff->font_info.weight);
    font->font_info.slant        = U32LE(ff->font_info.slant);
    font->font_info.width        = U32LE(ff->font_info.width);
    font->font_info.style        = U32LE(ff->font_info.style);
    font->font_info.spacing      = U32LE(ff->font_info.spacing);
    font->font_info.pixel_format = U32LE(ff->font_info.pixel_format);
    font->font_info.pixel_size   = U32LE(ff->font_info.pixel_size);
    font->font_info.point_size   = U32LE(ff->font_info.point_size);
    font->font_info.resolution_x = U32LE(ff->font_info.resolution_x);
    font->font_info.resolution_y = U32LE(ff->font_info.resolution_y);
    font->font_info.descent      = U32LE(ff->font_info.descent);
    font->font_info.ascent       = U32LE(ff->font_info.ascent);
    font->font_file = 0;
    font->font_data = 0;
    font->font_map = 0;
    return 0;
}

// Load dynamic string from FILE, deallocate with free
char* epx_font_FILE_string(FILE* f,  epx_font_file_t* ff, uint32_t offset)
{
    int len;
    char* ptr;

    if (fseek(f, sizeof(epx_font_file_t) + U32LE(ff->string_table_start) + offset, SEEK_SET) < 0)
	return 0;
    len = fgetc(f);
    // printf("epx_font_FILE_string: len = %d\r\n", len);
    if (len < 0)
	return 0;
    if (!(ptr = malloc(len + 1)))
	return 0;
    if (fread(ptr, sizeof(char), len+1, f) < 1) {
	free(ptr);
	return 0;
    }
    // printf("String = %s\r\n", ptr);
    return ptr;
}

int epx_font_preloaded_init(epx_font_t* font, uint8_t* data)
{
    if (epx_font_info_init(font, (epx_font_file_t*) data) < 0)
	return 0;
    font->font_file = (epx_font_file_t*) data;
    return 0;
}


int epx_font_open_init(epx_font_t* font, char* file)
{
    FILE* f;
    epx_font_file_t font_file;
    size_t file_size;
    int res = -1;

    if ((f = fopen(file, "r")) == 0) {
	fprintf(stderr, "epx_font: file %s, open error: %s\n", 
		file, strerror(errno));
	return -1;
    }

    fseek(f, 0L, SEEK_END);
    file_size = ftell(f);
    fseek(f, 0L, SEEK_SET);

    if (file_size < sizeof(epx_font_file_t)) {
	fprintf(stderr, "epx_font: file %s, size error\r\n", file);
	goto error;
    }

    /* read the FontFile header structure */
    if (fread((void*)&font_file, sizeof(epx_font_file_t), 1, f) < 1) {
	fprintf(stderr, "epx_font: file %s, read error: %s\r\n", file,
		strerror(errno));
	goto error;
    }

    if (epx_font_info_init(font, &font_file) < 0) {
	fprintf(stderr, "epx_font: file %s, format error\r\n", file);
	goto error;
    }
    font->file_name = strdup(file);
    font->file_size = file_size;
    font->foundry_name = epx_font_FILE_string(f, &font_file, 
					      U32LE(font_file.foundry_offset));
    font->family_name = epx_font_FILE_string(f, &font_file,
					     U32LE(font_file.family_offset));
    res = 0;

error:
    fclose(f);
    return res;
}

/* Create the font structure and load the font info
 * but leave it unmapped and unloaded
 */
epx_font_t* epx_font_open(char* file)
{
    epx_font_t* font;

    if (!(font = epx_font_create()))
	return 0;
    if (epx_font_open_init(font, file) < 0) {
	epx_font_destroy(font);
	return 0;
    }
    return font;
}

/*
 *   Load font into memory (unmapped)
 */
int epx_font_load(epx_font_t* font)
{
    epx_font_file_t* font_data;
    FILE* f;

    if (epx_font_is_loaded(font) || epx_font_is_mapped(font)) {
	fprintf(stderr, "epx_font: file %s, already loaded or mapped\r\n",
		font->file_name);
	return -1;
    }
    
    if ((f = fopen(font->file_name, "r")) == 0) {
	fprintf(stderr, "epx_font: file %s, file error: %s\r\n",
		font->file_name, strerror(errno));
	return -1;
    }

    font_data = (epx_font_file_t*) malloc(font->file_size);
    if (fread((void*)font_data, sizeof(char), font->file_size, f) < 
	font->file_size) {
	fprintf(stderr, "epx_font: file %s size error\r\n", font->file_name);
	fclose(f);
	free(font_data);
	return -1;
    }

    font->font_data = font_data;
    font->font_file = font_data;
    return 0;
}

int epx_font_unload(epx_font_t* font)
{
    if (!epx_font_is_loaded(font))
	return 0;
    free(font->font_data);
    font->font_data = 0;
    font->font_file = 0L;
    return 0;
}

int epx_font_map(epx_font_t* font)
{
    int fd;

    if (epx_font_is_loaded(font) || epx_font_is_mapped(font)) {
	fprintf(stderr, "epx_font: file %s, already loaded or mapped\r\n",
		font->file_name);
	return -1;
    }

    if ((fd = open(font->file_name, O_RDONLY)) < 0) {
	fprintf(stderr, "epx_font: file %s, open error : %s\r\n",
		font->file_name, strerror(errno));
	return -1;
    }

    font->font_map = (epx_font_file_t*) mmap(0,
					     font->file_size,
					     PROT_READ, MAP_SHARED, 
					     fd, 0);
    close(fd);
    if ((unsigned char*) font->font_map == (unsigned char *) 0xFFFFFFFF) {
	fprintf(stderr, "epx_font: file %s, map error : %s\r\n",
		font->file_name, strerror(errno));
	font->font_map = 0;
	return -1;
    }
    font->font_file = font->font_map;
    return 0;
}

int epx_font_unmap(epx_font_t* font)
{
    if (!epx_font_is_mapped(font))
	return 0;
    if (munmap(font->font_map, font->file_size) < 0) {
	fprintf(stderr, "epx_font: file %s, munmap error : %s\r\n",
		font->file_name, strerror(errno));
	return -1;
    }
    font->font_map  = 0;
    font->font_file = 0;
    return 0;
}

void epx_fnt2_skip(epx_fnt2_ctx_t* ctx, unsigned int n)
{
    while(n--) 
	epx_fnt2_ctx_next(ctx);
}

//
// Draw a EFNT2 compressed alpha map
//

void epx_compressed_add_color_area(epx_pixmap_t* src,epx_pixmap_t* dst, 
				   uint8_t fader,
				   epx_pixel_t color,
				   int x_src, int y_src, int x_dst, int y_dst,
				   unsigned int width, unsigned int height, 
				   epx_flags_t flags)
{
    epx_rect_t sr = {{x_src,y_src}, {width,height}};
    epx_rect_t dr0, dr;
    unsigned int dst_psz;
    int src_wb;
    uint8_t* dptr;
    int dst_wb;
    epx_fnt2_ctx_t ctx;

    if (fader == ALPHA_FACTOR_0)
	return;

    // Clip source and check that there is anything to blend
    if (!epx_rect_intersect(&sr, &src->clip, &sr))
	return;
    // The destination must have the same dimension as source
    dr0.xy.x = x_dst;
    dr0.xy.y = y_dst;
    dr0.wh   = sr.wh;
    // Clip destination and check that there is anything to blend to
    if (!epx_rect_intersect(&dr0, &dst->clip, &dr))
	return;
    // Update sr with the width and relativ change dx, dy
    sr.xy.x -= (dr0.xy.x - dr.xy.x);
    sr.xy.y -= (dr0.xy.y - dr.xy.y);
    sr.wh = dr.wh;

    epx_fnt2_ctx_init(&ctx, src->data, src->width*src->height);
    
    // Skip forward to sr.xy.x & sr.xy.y!
    src_wb = src->width;  // not bytesPerRow!
    epx_fnt2_skip(&ctx, sr.xy.y*src_wb + sr.xy.x);
    
    dptr = EPX_PIXEL_ADDR(dst,dr.xy.x,dr.xy.y);
    dst_wb = dst->bytes_per_row;
    width = dr.wh.width;
    height = dr.wh.height;

    dst_psz = EPX_PIXEL_BYTE_SIZE(dst->pixel_format);
    
    if ((flags&EPX_FLAG_BLEND) == 0) {
	while(height--) {
	    uint8_t* dptr1 = dptr;
	    unsigned int width1 = width;

	    while(width1--) {
		epx_pixel_t  s;

		s.px = 0;
		s.a = epx_fnt2_ctx_next(&ctx);
		s = epx_pixel_add(color,s);
		s.a = ((s.a * fader) >> 8);
		s = epx_pixel_shadow(s.a, s);
		dst->func.pack(s, dptr1);
		dptr1 += dst_psz;
	    }
	    epx_fnt2_skip(&ctx, src_wb - width);
	    dptr += dst_wb;
	}
    }
    else {
	while(height--) {
	    uint8_t* dptr1 = dptr;
	    unsigned int width1 = width;

	    while(width1--) {
		epx_pixel_t  s;
		epx_pixel_t d;

		d = dst->func.unpack(dptr1);
		s.px = 0;
		s.a = epx_fnt2_ctx_next(&ctx);

		/* add the color to source */
		s = epx_pixel_add(color,s);
		s.a = ((s.a * fader) >> 8);
		d = epx_pixel_blend(s.a, s, d);
		dst->func.pack(d, dptr1);
		dptr1 += dst_psz;
	    }
	    epx_fnt2_skip(&ctx, src_wb - width);
	    dptr += dst_wb;
	}
    }

}


/* Draw one char and return the new x position */
void epx_font_draw_glyph(epx_gc_t* gc, epx_pixmap_t* dst, int* x, int* y, int c)
{
    epx_glyph_t* glyph;
    epx_font_t*  font = gc->font;
    int     xoffs;
    int     dwx;
    unsigned int gwidth;
    unsigned int fwidth;
    unsigned int width;

    if (!font || !font->font_file)
	return;
    if ((glyph = epx_font_file_glyph(font->font_file, c)) == 0)
	return;

    gwidth = width = U16LE(glyph->width);

    if ((fwidth = gc->glyph_fixed_width)) {
	if (fwidth > gwidth)
	    xoffs = (fwidth - gwidth) / 2;
	else {
	    xoffs = 0;
	    width = fwidth;
	}
	dwx = fwidth;
    }
    else {
	xoffs = I16LE(glyph->xoffs);
	dwx = I16LE(glyph->dwx);
    }

    if ((c == '.') && gc->glyph_dot_kern)
	dwx += gc->glyph_dot_kern;

    /* Set dst = 0 to determine change in x and y */
    if (dst != 0) {
	uint16_t pixel_format = font->font_info.pixel_format;
	int psz = EPX_PIXEL_BYTE_SIZE(pixel_format);
	unsigned int bytes_per_row;
	epx_pixmap_t gmap;

	// NOTE: each pixel data row must be 16 byte aligned!
	gmap.width     = width;
	gmap.height    = U16LE(glyph->height);
	// work around for me changing alpha first bit to better handle
	// format parsing. When only alpha then alpha first is set, before
	// it was unset. Since fonts are compiled and stored we make this
	// workaround here.
	pixel_format = ((pixel_format|EPX_F_AFirst) == EPX_FORMAT_A8) ?
	    EPX_FORMAT_A8 : pixel_format;
	gmap.pixel_format = pixel_format;
	gmap.bytes_per_pixel = psz;
	gmap.bits_per_pixel = psz*8;
	bytes_per_row = psz*gwidth;
	gmap.bytes_per_row = bytes_per_row + EPX_ALIGN_OFFS(bytes_per_row,16);
	gmap.sz = gmap.bytes_per_row*gmap.height;
	gmap.data = (void*) glyph->data;
	epx_rect_set(&gmap.clip, 0, 0, gmap.width, gmap.height);

	if (pixel_format == EPX_FORMAT_A8) {
	    epx_pixmap_add_color_area(&gmap, dst, 
				      gc->fader_value,
				      gc->foreground_color,
				      0, 0,
				      *x + xoffs,
				      *y - I16LE(glyph->yoffs) - gmap.height,
				      gmap.width,
				      gmap.height,
				      EPX_FLAG_BLEND);
	}
	else if (pixel_format == EPX_FORMAT_EFNT2) {
	    epx_compressed_add_color_area(&gmap, dst, 
					  gc->fader_value,
					  gc->foreground_color,
					  0, 0,
					  *x + xoffs,
					  *y - I16LE(glyph->yoffs)-gmap.height,
					  gmap.width,
					  gmap.height,
					  EPX_FLAG_BLEND);
	}
	else {
	    epx_pixmap_fade_area(&gmap, dst, 
				 gc->fader_value,
				 0, 0,
				 *x + xoffs,
				 *y - I16LE(glyph->yoffs) - gmap.height,
				 gmap.width,
				 gmap.height);
	}
    }
    *x += dwx + gc->glyph_delta_x;
    *y += I16LE(glyph->dwy) + gc->glyph_delta_y;
}


void epx_font_draw_string(epx_gc_t* gc, epx_pixmap_t* dst, int* x, int* y, char* string)
{
    int c;
    int n = 0;

    while((c = *string++) != 0) {
	if (n && (c == '.') && gc->glyph_dot_kern && *string)
	    *x += gc->glyph_dot_kern;
	epx_font_draw_glyph(gc, dst, x, y, c);
	n++;
    }
    if (n)
	*x -= gc->glyph_delta_x;
}

void epx_font_draw_wide_string(epx_gc_t* gc, epx_pixmap_t* dst, int* x, int* y, wchar_t* string)
{
    int c;
    int n = 0;

    while((c = *string++) != 0) {
	if (n && (c == '.') && gc->glyph_dot_kern && *string)
	    *x += gc->glyph_dot_kern;
	epx_font_draw_glyph(gc, dst, x, y, c);
	n++;
    }
    if (n)
	*x -= gc->glyph_delta_x;
}


#define UNI_REPLACEMENT_CHAR (uint32_t)0x0000FFFD
#define UNI_MAX_UTF32        (uint32_t)0x7FFFFFFF
#define UNI_MAX_LEGAL_UTF32  (uint32_t)0x0010FFFF
#define UNI_SUR_HIGH_START   (uint32_t)0xD800
#define UNI_SUR_HIGH_END     (uint32_t)0xDBFF
#define UNI_SUR_LOW_START    (uint32_t)0xDC00
#define UNI_SUR_LOW_END      (uint32_t)0xDFFF

static int is_legal_utf8(uint8_t* source, int length)
{
    uint8_t a;
    const uint8_t *srcptr = source+length;
    switch (length) {
    default: return 0;
    case 4: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return 0;
    case 3: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return 0;
    case 2: if ((a = (*--srcptr)) > 0xBF) return 0;
	switch (*source) {
	    /* no fall-through in this inner switch */
	    case 0xE0: if (a < 0xA0) return 0;
	    case 0xED: if (a > 0x9F) return 0;
	    case 0xF0: if (a < 0x90) return 0;
	    case 0xF4: if (a > 0x8F) return 0;
	    default:   if (a < 0x80) return 0;
	}
    case 1: if (*source >= 0x80 && *source < 0xC2) return 0;
    }
    if (*source > 0xF4) return 0;
    return 1;
}

void epx_font_draw_utf8(epx_gc_t* gc, epx_pixmap_t* dst, 
			int* x, int* y, char* string, size_t len)
{
    uint8_t* source = (uint8_t*) string;
    uint8_t* sourceEnd = source + len;
    int n = 0;

    while(source < sourceEnd) {
	int c;
	uint8_t bits = *source;

	if ((bits & 0x80) == 0x00)
	    c = *source++;
	else {
	    uint32_t ch = 0;
	    int n;

	    if ((bits & 0xE0) == 0xC0) n = 1;
	    else if ((bits & 0xF0) == 0xE0) n = 2;
	    else if ((bits & 0xF8) == 0xF0) n = 3;
	    else if ((bits & 0xFC) == 0xF8) n = 4;
	    else n = 5;

	    c = UNI_REPLACEMENT_CHAR;
	    if (!is_legal_utf8(source, n+1))
		break;
	    switch (n) {
	    case 5:  ch += *source++; ch <<= 6;
	    case 4:  ch += *source++; ch <<= 6;
	    case 3:  ch += *source++; ch <<= 6;
	    case 2:  ch += *source++; ch <<= 6;
	    case 1:  ch += *source++; ch <<= 6;
	    default: ch += *source++;
	    }

	    switch(n) {
	    default: break;
	    case 1: ch -= 0x00003080UL; break;
	    case 2: ch -= 0x000E2080UL; break;
	    case 3: ch -= 0x03C82080UL; break;
	    case 4: ch -= 0xFA082080UL; break;
	    case 5: ch -= 0x82082080UL; break;
	    }

	    if ((ch <= UNI_MAX_LEGAL_UTF32) &&
		((ch < UNI_SUR_HIGH_START) || (ch > UNI_SUR_LOW_END)))
		c = ch;
	}
	if (n && (c == '.') && gc->glyph_dot_kern && (source < sourceEnd))
	    *x += gc->glyph_dot_kern;
	epx_font_draw_glyph(gc, dst, x, y, c);
	n++;
    }
    if (n)
	*x -= gc->glyph_delta_x;
}
