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
 * efnttool.c
 *
 *   Convert fonts to efnt/efnt2 format
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <freetype2/ft2build.h>
#include <freetype2/freetype.h>

#include "epx.h"

int debug = 0;

int   efnt_c_fmt = 0;
int   efnt_c_line = 0;
char* efnt_name = "UNKNONW";
char* efnt_style = "NONE";
int   efnt_size = 0;


#define ERRFMT(...)      emit(stderr,1,__FILE__, __LINE__, __VA_ARGS__)

#define DBGFMT(...) do { \
	if (debug) emit(stdout,0,__FILE__, __LINE__, __VA_ARGS__); \
} while(0)

#define DEFAULT_XRES 75
#define DEFAULT_YRES 75
#define MM_INCH 0.0393700787

void usage()
{
    fprintf(stderr,
	    "usage:\n"
	    "  efnttool <options> <fontfile> <fontsize>\n"
	    " options:\n"
	    "   -efnt2\n"
	    "   -bitmap\n"
	    "   -ascii\n"
	    "   -latin1\n"
	    "   -start <char>\n"
	    "   -end <char>\n"
	    "   -o <outfile>\n"
	    "   -xres <dpi>\n"
	    "   -yres <dpi>\n"
	    "   -v\n");
    exit(1);
}

// Output helpers
void emit(FILE* f, int info, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);
    if (info)
	fprintf(f, "%s:%d: ", file, line); 
    vfprintf(f, fmt, ap);
    fputc('\n', f);
    va_end(ap);
}

#define streq(a, b) (strcasecmp((a),(b)) == 0)

inline unsigned char nibble(char x) {
    if ((x >= '0') && (x <= '9'))
	return x-'0';
    else if ((x >= 'A') && (x <= 'F'))
	return (x-'A')+10;
    else if ((x >= 'a') && (x <= 'f'))
	return (x-'a')+10;
    else
	return 0;
}

#define hexbyte(x1,x2) ((nibble(x1) << 4) | nibble(x2))

/* bits to use for default handling of values */
#define GLYPH_NAME   0x80
#define GLYPH_WIDTH  0x01
#define GLYPH_HEIGHT 0x02
#define GLYPH_XOFFS  0x04
#define GLYPH_YOFFS  0x08
#define GLYPH_DWX    0x10
#define GLYPH_DWY    0x20

#define FONT_PIXEL_SIZE   0x01
#define FONT_POINT_SIZE   0x02
#define FONT_RESOLUTION_X 0x04
#define FONT_RESOLUTION_Y 0x08
#define FONT_DESCENT      0x10
#define FONT_ASCENT       0x20


typedef struct _key_value {
    char*     key;
    u_int32_t value;
} KeyValue;

typedef struct _string_table_t {
    size_t n;       /* number of entries */
    size_t size;    /* size in bytes */
    size_t remain;  /* remain data int free area */
    char*  ptr;     /* pointer to next free data */
    char*  data;
} string_table_t;

typedef struct _glyph_t {
    size_t   size;         // size=0 means not used
    epx_glyph_t*  glyph;   // glyph=NULL also means not used
} glyph_t;

typedef struct _glyph_table_t {
    size_t     n;     /* number of glyphs in table (not counting NULL elems) */
    size_t     size;  /* size of glyph_data table */
    glyph_t* glyph_data;
} glyph_table_t;

KeyValue kv_weight[] = {
    {  "none",    EPX_FONT_WEIGHT_NONE },
    { "medium",   EPX_FONT_WEIGHT_MEDIUM},
    { "bold",     EPX_FONT_WEIGHT_BOLD },
    { "bold italic",   EPX_FONT_WEIGHT_BOLD },
    { "demibold", EPX_FONT_WEIGHT_DEMIBOLD },
    {  "",    EPX_FONT_WEIGHT_NONE },
    { NULL, 0} 
};

KeyValue kv_slant[] = {
    { "none", EPX_FONT_SLANT_NONE },
    { "r", EPX_FONT_SLANT_ROMAN },
    { "i", EPX_FONT_SLANT_ITALIC },
    { "o", EPX_FONT_SLANT_OBLIQUE},
    { "ri", EPX_FONT_SLANT_REVERSE_ITALIC},
    { "ro", EPX_FONT_SLANT_REVERSE_OBLIQUE},
    { "ot", EPX_FONT_SLANT_OTHER },
    { "roman", EPX_FONT_SLANT_ROMAN },
    { "italic", EPX_FONT_SLANT_ITALIC },
    { "bold italic", EPX_FONT_SLANT_ITALIC }, // MacOS Arial Bold italic.
    { "oblique", EPX_FONT_SLANT_OBLIQUE},
    { "reverse italic", EPX_FONT_SLANT_REVERSE_ITALIC},
    { "reverse oblique", EPX_FONT_SLANT_REVERSE_OBLIQUE},
    { "other", EPX_FONT_SLANT_OTHER },
    { "", EPX_FONT_SLANT_NONE },
    { NULL, 0} 
};
    
KeyValue kv_width[] = {
    { "none", EPX_FONT_WIDTH_NONE},
    { "normal", EPX_FONT_WIDTH_NORMAL},
    { "condensed", EPX_FONT_WIDTH_CONDENSED},
    { "narrow", EPX_FONT_WIDTH_NARROW},
    { "double wide", EPX_FONT_WIDTH_DOUBLE_WIDE},
    { "", EPX_FONT_WIDTH_NONE},
    { NULL, 0} 
};

KeyValue kv_style[] = {
    { "none",        EPX_FONT_STYLE_NONE},
    { "serif",       EPX_FONT_STYLE_SERIF},
    { "sans serfi",  EPX_FONT_STYLE_SANS_SERIF},
    { "informal",    EPX_FONT_STYLE_INFORMAL},
    { "decorated",   EPX_FONT_STYLE_DECORATED},
    { "",        EPX_FONT_STYLE_NONE},
    { NULL, 0} 
};

KeyValue kv_spacing[] = {
    { "none",       EPX_FONT_SPACING_NONE},
    { "p",   EPX_FONT_SPACING_PROPORTIONAL},
    { "m",   EPX_FONT_SPACING_MONOSPACED},
    { "c",   EPX_FONT_SPACING_CHAR_CELL},
    { "proportional", EPX_FONT_SPACING_PROPORTIONAL},
    { "monospaced",   EPX_FONT_SPACING_MONOSPACED},
    { "charcell",     EPX_FONT_SPACING_CHAR_CELL},
    { "",         EPX_FONT_SPACING_NONE},
    { NULL, 0} 
};

u_int32_t kv_lookup(KeyValue* table, char* key, int* found)
{
    int i = 0;
    *found = 0;
    while(table[i].key != NULL) {
	if (streq(table[i].key, key)) {
	    *found = 1;
	    return table[i].value;
	}
	i++;
    }
    return 0;
}

int value_u16(char* value, u_int16_t* vp, char* file, int line)
{
    unsigned long v;
    char* ep = NULL;
    v = strtoul(value, &ep, 0);
    if (*ep != '\0') goto error;
    if ((v > 0xffff)) goto out_of_range;
    *vp = v;
    return 1;
error:
    fprintf(stderr, "%s:%d: integer value expected\n", file, line);
    return 0;
out_of_range:
    fprintf(stderr, "%s:%d: value out of range\n", file, line);
    return 0;
}

int value_i16(char* value, int16_t* vp, char* file, int line)
{
    long v;
    char* ep = NULL;
    v = strtol(value, &ep, 0);
    if (*ep != '\0') goto error;
    if ((v < -0x8000) || (v > 0x7fff)) goto out_of_range;
    *vp = v;
    return 1;
error:
    fprintf(stderr, "%s:%d: integer value expected\n", file, line);
    return 0;
out_of_range:
    fprintf(stderr, "%s:%d: value out of range\n", file, line);
    return 0;
}

int value_u32(char* value, u_int32_t* vp, char* file, int line)
{
    unsigned long v;
    char* ep = NULL;
    v = strtoul(value, &ep, 0);
    if (*ep != '\0') goto error;
    *vp = v;
    return 1;
error:
    fprintf(stderr, "%s:%d: integer value expected\n", file, line);
    return 0;
}


string_table_t* new_string_table(void)
{
    return (string_table_t*) calloc(1, sizeof(string_table_t));
}

/* add a string and return "byte" offset 
 * the string is maxed to 255 characters and
 * has a leading length byte and a terminating zero char
 */
u_int32_t add_string(string_table_t* table, char* string)
{
    int sz = strlen(string);
    u_int32_t offset = table->size;

    if (table->remain < (sz+2)) {
	table->data = realloc(table->data, table->size+(sz+2)+1024);
	table->ptr  = table->data + table->size;
	table->remain = (sz+2)+1024;
    }
    table->ptr[0] = sz;
    memcpy(table->ptr+1, string, sz);
    table->ptr[sz+1] = '\0';
    table->ptr    += (sz+2);
    table->size   += (sz+2);
    table->remain -= (sz+2);
    table->n++;
    return offset;
}

glyph_table_t* new_glyph_table(void)
{
    return (glyph_table_t*) calloc(1, sizeof(glyph_table_t));
}

int add_glyph(glyph_table_t* table, int index, int vmask, int pixel_format,
	      epx_glyph_t* glyph, epx_pixmap_t* pix)
{
    size_t glyph_size;
    size_t data_size;

    if ((pix == NULL) || (glyph == NULL))
	return -1;

    if (index >= table->size) {
	int i;
	int new_size = index + 128;
	table->glyph_data = realloc(table->glyph_data,
				    sizeof(glyph_t)*new_size);
	for (i = table->size; i < new_size; i++) {
	    table->glyph_data[i].size = 0;
	    table->glyph_data[i].glyph = NULL;
	}
	table->size = new_size;
    }

    /* convert the pixmap to the destination format */
    if (pixel_format != pix->pixel_format) {
	epx_pixmap_t* npix = epx_pixmap_create(pix->width, pix->height, 
					       pixel_format);
	epx_pixmap_copy_to(pix, npix);
	epx_pixmap_destroy(pix);
	pix = npix;
    }

    /* updated default values where needed */
    if (!(vmask & GLYPH_WIDTH))    glyph->width = pix->width;
    if (!(vmask & GLYPH_HEIGHT))   glyph->height = pix->height;
    if (!(vmask & GLYPH_XOFFS))    glyph->xoffs = 0;
    if (!(vmask & GLYPH_YOFFS))    glyph->yoffs = 0;
    if (!(vmask & GLYPH_DWX))      glyph->dwx = glyph->width;
    if (!(vmask & GLYPH_DWY))      glyph->dwy = 0;

    data_size = pix->sz;

    if (pix->pixel_format == EPX_FORMAT_EFNT2) {
	u_int8_t* ptr = pix->data;
	u_int32_t n   = data_size;

	data_size = 0;
	while(n--) {
	    data_size++;
	    if (*ptr == EFNT2_E())
		break;
	    ptr++;
	}
    }
    
    glyph_size = sizeof(epx_glyph_t) + data_size;
    table->glyph_data[index].size = glyph_size;
    table->glyph_data[index].glyph = malloc(glyph_size);
    memcpy(table->glyph_data[index].glyph, glyph, sizeof(epx_glyph_t));
    memcpy(((char*)table->glyph_data[index].glyph)+sizeof(epx_glyph_t),
	   pix->data, data_size);
    epx_pixmap_destroy(pix);
    table->n++;
    return 0;
}


void display_row(unsigned char* src, int n)
{
    int i;

    printf("[");
    if (n) {
	printf(" 16#%02X", src[0]);
	for (i = 1; i < n; i++) 
	    printf(",16#%02X", src[i]);
    }
    printf("],\n");
}

void display_compressed_row(unsigned char* src, int n)
{
    int i = 0;

    while(i < n) {
	int s = src[i] >> 5;
	switch(s) {
	case 0:
	    printf("T(%d)", EFNT2_Rl(src[i]));
	    i++;
	    break;
	case 1:
	    printf("O(%d)", EFNT2_Rl(src[i]));
	    i++;
	    break;
	case 2:
	    printf("B5(%d%d%d%d%d)",
		   (src[i] & 0x10) != 0,
		   (src[i] & 0x08) != 0,
		   (src[i] & 0x04) != 0,
		   (src[i] & 0x02) != 0,
		   (src[i] & 0x01) != 0);
	    i++;
	    break;
	case 3:
	    if (!(src[i] & 0x10)) {
		printf("B4(%d%d%d%d)",
		       (src[i] & 0x08) != 0,
		       (src[i] & 0x04) != 0,
		       (src[i] & 0x02) != 0,
		       (src[i] & 0x01) != 0);

	    }
	    else if (!(src[i] & 0x08)) {
		printf("B3(%d%d%d)",
		       (src[i] & 0x04) != 0,
		       (src[i] & 0x02) != 0,
		       (src[i] & 0x01) != 0);
	    }
	    else if (!(src[i] & 0x04)) {
		printf("B2(%d%d)",
		       (src[i] & 0x02) != 0,
		       (src[i] & 0x01) != 0);
	    }
	    else if (!(src[i] & 0x02)) {
		printf("B1(%d)", 
		       (src[i] & 0x01) != 0);
	    }
	    else if (!(src[i] & 0x01)) {
		printf("B0()");
	    }
	    else {
		printf("E()");
	    }
	    i++;
	    break;
	default:
	    printf("A(%02X)", ((src[i] & 0x7f)<<1)+1);
	    i++;
	    break;
	}
    }
    putchar('\n');
}


void load_row(unsigned char* src, int n, unsigned char* dst, int bitmap)
{
    while(n) {
	unsigned char c = *src++;

	if (bitmap) {
	    int k = 8;
	    while(n && k) {
		*dst++ = (c & 0x80) ? 255 : 0;
		c <<= 1;
		k--;
		n--;
	    }
	}
	else {
	    *dst++ = c;
	    n--;
	}
    }
}
//
//  compile sequence of 0 or 1
//  T(k)             -- k (0..31) transparent pixels
//  O(k)             -- k (0..31) opaque pixels
//  B5(x4x3x2x1x0)   -- 5 bit bitmap 
//  B4(x3x2x1x0)     -- 4 bit bitmap 
//  B3(x2x1x0)       -- 3 bit bitmap 
//  B2(x1x0)         -- 2 bit bitmap 
//  B1(x0)           -- 1 bit bitmap 
//
//  00001111111111000000   => B5(00001)O(9)T(6)
//  00000  => B(00000)
//  000000 => T(6)
//
int compile_bm(unsigned char* src, int n, unsigned char* dst)
{
    unsigned char* dst0 = dst;
    unsigned char t;

    while(n >= 5) {
	int i;
	int k=1;
	// Check run length
	for (i = 1; (i < n) && (src[0]==src[i]); i++)
	    k++;
	// Need run length >= 6 to start
	if (k >= 6) {
	    while(k >= 31) {
		t = (src[0]) ? EFNT2_O(31): EFNT2_T(31);
		*dst++ = t;
		k -= 31;
		n -= 31;
		src += 31;
	    }
	    if (k >= 6) {
		t = (src[0]) ? EFNT2_O(k) : EFNT2_T(k);
		*dst++ = t;
		n -= k;
		src += k;
	    }
	}
	else {
	    t = EFNT2_B5(src[0],src[1],src[2],src[3],src[4]);
	    *dst++ = t;
	    src += 5;
	    n -= 5;
	}
    }
    switch(n) {
    case 0: return dst-dst0;
    case 1: t = EFNT2_B1(src[0]); break;
    case 2: t = EFNT2_B2(src[0],src[1]); break;
    case 3: t = EFNT2_B3(src[0],src[1],src[2]); break;
    case 4: t = EFNT2_B4(src[0],src[1],src[2],src[3]); break;
    }
    *dst++ = t;
    return dst - dst0;
}

// write level1 codes T/O/B1..B5/A
int compress_row_1(unsigned char* src, int n, unsigned char* dst)
{
    unsigned char* src_end = src + n;
    unsigned char* dst0 = dst;

    while(src < src_end) {
	unsigned char c = *src++;
	if ((c == 0) || (c == 255)) {
	    unsigned char* base = dst;
	    *dst++ = (c != 0);
	    while((src < src_end) && ((*src==0) || (*src==255))) {
		*dst++ = (*src != 0);
		src++;
	    }
	    n = compile_bm(base, dst - base, base);
	    dst = base + n;
	}
	else {
	    *dst++ = EFNT2_A(c);
	}
    }
    return dst - dst0;
}

void load_glyph(epx_pixmap_t* gpix, FT_GlyphSlot slot,int scale, int load_bitmap)
{
    unsigned char* gptr;
    unsigned char* buf = NULL;
    epx_pixel_t  p;
    int y;
    int sz = 0;
    int csz = 0;

    gptr = slot->bitmap.buffer;
    p.r = p.g = p.b = 255;

    if (load_bitmap && (slot->bitmap.num_grays > 1)) {
	fprintf(stderr, "num_grays MUST be 1 for -bitmap\n");
	exit(1);
    }
    if (!load_bitmap && (slot->bitmap.num_grays != 256)) {
	fprintf(stderr, "num_grays != 256 not supported\n");
	exit(1);
    }

    buf = (unsigned char*) malloc(sizeof(unsigned char)*slot->bitmap.width);

    for (y = 0; y < slot->bitmap.rows; y++) {
	int x;
	int n = slot->bitmap.width;
	load_row(gptr, n, buf, load_bitmap);
	sz += n;
	if (debug > 1)
	    display_row(buf, n);
	if (gpix->pixel_format == EPX_FORMAT_EFNT2) {
	    n = compress_row_1(buf, n, buf);
	    if (debug > 1)
		display_compressed_row(buf, n);
	    memcpy(gpix->data + csz, buf, n);
	    csz += n;
	}
	else {
	    for (x = 0; x < n; x++) {
		p.a = buf[x];
		if (scale == 1) 
		    epx_pixmap_put_pixel(gpix, x, y, EPX_FLAG_NONE, p);
		else {
		    int iy;
		    for (iy = 0; iy < scale; iy++) {
			int ix;
			for (ix = 0; ix < scale; ix++)
			    epx_pixmap_put_pixel(gpix, x*scale+ix,
						 y*scale+iy, EPX_FLAG_NONE, p);
		    }
		}
	    }
	}
	gptr += slot->bitmap.pitch;
    }
    if (gpix->pixel_format == EPX_FORMAT_EFNT2) {
	if (csz < gpix->sz)
	    *(gpix->data + csz) = EFNT2_E(); // terminate
    }
    if (debug > 1)
	printf("\n");
    free(buf);
    DBGFMT("size=%d bytes, compressed=%d bytes", sz, csz);
}



int load_ft(char* fontfile, int fontsize, int scale, int xres, int yres,
	    epx_font_file_t* efnt, string_table_t* string_table, 
	    glyph_table_t* glyph_table, 
	    int start_char, int end_char,
	    int load_bitmap, epx_format_t pixel_format)
{
    FT_Library  library;
    FT_Face     face;
    FT_GlyphSlot slot;
    FT_UInt      gindex;
    FT_ULong     charcode;
    int          error;
    epx_glyph_t       glyph;
    epx_pixmap_t*     gpix;
    int glyph_vmask;
    int font_vmask = 0;
    int nerror = 0;
    int found;

    error = FT_Init_FreeType(&library);
    if (error) {
	fprintf(stderr, "could not initialized free type library (0x%02X)\n",error);
	return -1;
    }

    // Vectored types are scale automatically
    if (!load_bitmap)
	scale = 1;

    error = FT_New_Face(library, fontfile, 0, &face);
    if (error) {
	if (error == FT_Err_Unknown_File_Format)
	    fprintf(stderr, "unsupported font format in file '%s'\n", fontfile);
	else 
	    fprintf(stderr, "could not open fontfile '%s'\n", fontfile);
	return -1;
    }
    slot = face->glyph;
    // Set char hight to 16pt in 100x100dpi
    // FIXME: retrieve the correct display size!!!
    FT_Set_Char_Size(face, 0, fontsize*64, xres, yres);

    DBGFMT("style_name: %s\n", face->style_name);
    efnt->font_info.weight = kv_lookup(kv_weight, face->style_name, &found);
    if (!found) {
	efnt->font_info.weight = EPX_FONT_WEIGHT_MEDIUM;
    }

    efnt->font_info.slant  = kv_lookup(kv_slant, face->style_name, &found);
    if (!found) {
	fprintf(stderr, "could not map style_name '%s'\n", 
		face->style_name);
	efnt->font_info.slant = EPX_FONT_SLANT_ROMAN;
    }
    efnt->font_info.spacing = EPX_FONT_SPACING_PROPORTIONAL;

    efnt->font_info.point_size = fontsize*scale*10;
    font_vmask |= FONT_POINT_SIZE;

    efnt->font_info.pixel_size = face->size->metrics.y_ppem*scale;
    font_vmask |= FONT_PIXEL_SIZE;

    efnt->font_info.descent = -(face->size->metrics.descender*scale) / 64;
    font_vmask |= FONT_DESCENT;
    
    efnt->font_info.ascent = (face->size->metrics.ascender*scale) / 64;
    font_vmask |= FONT_ASCENT;

    efnt->font_info.resolution_x = xres;
    font_vmask |= FONT_RESOLUTION_X;

    efnt->font_info.resolution_y = yres;
    font_vmask |= FONT_RESOLUTION_Y;

    efnt->encoding_default = 0;

    DBGFMT("Font PointSize=%.2f, PixelSize=%d at resolution=%dx%d\n",
	   efnt->font_info.point_size/10.0,
	   efnt->font_info.pixel_size,
	   efnt->font_info.resolution_x,
	   efnt->font_info.resolution_y);

    memset(&glyph, 0, sizeof(epx_glyph_t));
    gpix = NULL;
    glyph_vmask = 0;
    efnt->font_info.pixel_format = pixel_format;
    nerror = 0;

    DBGFMT("family_name: %s\n", face->family_name);
    efnt->family_offset = add_string(string_table, face->family_name);

    efnt_name = strdup(face->family_name);
    efnt_style = strdup(face->style_name);
    efnt_size  = fontsize;


    charcode = FT_Get_First_Char(face, &gindex);
    while(gindex != 0) {
	char gname[1024];

	if ((charcode < start_char) || (charcode > end_char))
	    goto next;

	if (load_bitmap)
	    error = FT_Load_Glyph(face, gindex, FT_LOAD_MONOCHROME);
	else
	    error = FT_Load_Glyph(face, gindex, FT_LOAD_NO_BITMAP);

	if (error) {
	    fprintf(stderr, "unable to load char %lu, glyph %d\n",
		    charcode,gindex);
	    goto next;
	}

	if (load_bitmap)
	    error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_MONO);
	else
	    error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
	if (error) {
	    fprintf(stderr, "unable to render char %lu, glyph %d\n",
		    charcode,gindex);
	    goto next;
	}
	error = FT_Get_Glyph_Name(face, gindex, gname, sizeof(gname));
	glyph.name_offset = 0;
	if (error) {
	    fprintf(stderr, "unabled to get glyph name for glyph %d\n",gindex);
	    gname[0]='\0';
	}
	else if (pixel_format != EPX_FORMAT_EFNT2) {
	    glyph.name_offset = add_string(string_table, gname);
	    glyph_vmask |= GLYPH_NAME;
	}
	DBGFMT("%ld: '%s' glyph_index=%d", charcode, gname, gindex);


	// Now we have the bitmap
	glyph.width  = (slot->metrics.width*scale)  / 64;
	glyph.height = (slot->metrics.height*scale) / 64;
	glyph.xoffs  = slot->bitmap_left*scale;
	glyph.yoffs  = -(slot->bitmap.rows - slot->bitmap_top)*scale;
	// glyph.yoffs  = slot->metrics.height / 64 - slot->bitmap_top;
	glyph.dwx    = (slot->advance.x*scale) / 64;
	glyph.dwy    = (slot->advance.y*scale) / 64;

	glyph_vmask |= (GLYPH_WIDTH|GLYPH_HEIGHT|
			GLYPH_XOFFS|GLYPH_YOFFS|
			GLYPH_DWX|GLYPH_DWY);

	gpix = epx_pixmap_create(slot->bitmap.width*scale,
				 slot->bitmap.rows*scale,pixel_format);
	load_glyph(gpix, slot, scale, load_bitmap);
	if (add_glyph(glyph_table, charcode, glyph_vmask,
		      efnt->font_info.pixel_format,
		      &glyph, gpix) < 0) {
	    fprintf(stderr, "could not add glyph %s[%d]\n", gname,gindex);
	    nerror++;
	}
	memset(&glyph, 0, sizeof(epx_glyph_t));
	glyph_vmask = 0;
	gpix = NULL;

    next:
	charcode = FT_Get_Next_Char(face, charcode, &gindex);
    }
    return font_vmask;
}


FILE* efnt_fopen(char* filename)
{
    FILE* f;

    if (!filename)
	f = stdout;
    else if (!(f = fopen(filename, "w"))) {
	fprintf(stderr, "efnttool: could not open %s for writing : %s\n",
		filename, strerror(errno));
	return 0;
    }
    if (f && efnt_c_fmt) {
	fprintf(f, "//\n// EFNT FILE - by efnttool\n//\n");
	fprintf(f, "#include \"efnt.h\"\n");
	fprintf(f, "u_int8_t %s_%s_%d_data[] = {\n",
		efnt_name, efnt_style, efnt_size);
	efnt_c_line = 0;
    }
    return f;
}

void efnt_fclose(FILE* f)
{
    if (f && efnt_c_fmt) {
	fprintf(f, "\n};\n");
	fprintf(f, "epx_font_file_t* %s_%s_%d = (epx_font_file_t*) %s_%s_%d_data;\n",
		efnt_name, efnt_style, efnt_size,
		efnt_name, efnt_style, efnt_size);
    }
    if (f && (f != stdout))
	fclose(f);
}

void efnt_fwrite(void* data, size_t size, size_t nitems, FILE* f)
{
    if (!efnt_c_fmt)
	fwrite(data, size, nitems, f);
    else {
	u_int8_t* ptr = (u_int8_t*) data;
	u_int32_t len = size*nitems;
	while(len--) {
	    fprintf(f, "0x%02X,", *ptr);
	    ptr++;
	    efnt_c_line++;
	    if (efnt_c_line >= 16) {
		fprintf(f, "\n");
		efnt_c_line = 0;
	    }
	}
    }
}

/* calculate padding size (e.g to reach alignment) */
#define ALIGN_SIZE(n,a) ((a - ((n) % (a))) % (a))
//
// FIXME: makesure all integers written are in little endian!!!
//
int write_efnt(char* file, epx_font_file_t* efnt, int vmask,
	       string_table_t* string_table,
	       glyph_table_t*  glyph_table)
{
    FILE* f = NULL;
    int i = 0;
    int start = -1;
    int stop  = -1;
    int max_glyph_width = 0;
    int min_glyph_width = 0x10000;
    int max_glyph_height = 0;
    int min_glyph_height = 0x10000;
    size_t glyph_table_size = 0;
    size_t table_sz;
    u_int32_t offset;
    int n;
    char pad[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

    if (!(f = efnt_fopen(file)))
	return -1;

    /* scan trough the glyph_table and calculate offset start & stop 
     * also calculate max/min width/height
     */
    i = 0;
    while(i < glyph_table->size) {
	epx_glyph_t* g = glyph_table->glyph_data[i].glyph;
	size_t sz = glyph_table->glyph_data[i].size;
	
	glyph_table_size += sz;
	if (sz) {
	    stop = i;
	    if (start < 0)
		start = i;
	    if (g->width > max_glyph_width) max_glyph_width=g->width;
	    if (g->width < min_glyph_width) min_glyph_width=g->width;
	    if (g->height > max_glyph_height) max_glyph_height=g->height;
	    if (g->height < min_glyph_height) min_glyph_height=g->height;
	}
	i++;
    }

    if (start < 0) {
	fprintf(stderr, "efnttool: could not find any glyphs\n");
	goto error;
    }

    if (!(vmask & FONT_RESOLUTION_X)) efnt->font_info.resolution_x = 75;
    if (!(vmask & FONT_RESOLUTION_Y)) efnt->font_info.resolution_y = 75;
    if (!(vmask & FONT_DESCENT)) efnt->font_info.descent = 0;
    if (!(vmask & FONT_ASCENT))  efnt->font_info.descent = max_glyph_height;
    

    efnt->encoding_start = start;
    efnt->encoding_stop  = stop;
    efnt->encoding_default = '?';

    DBGFMT("#glyphs : %lu", glyph_table->n);
    DBGFMT("glyph start=%d", start);
    DBGFMT("glyph stop=%d", stop);

    table_sz = string_table->size;
    efnt->string_table_start  = 0;
    efnt->string_table_length = table_sz + ALIGN_SIZE(table_sz, 16);

    table_sz = ((stop - start) + 1)*sizeof(u_int32_t);
    efnt->offset_table_start  = efnt->string_table_start + 
	efnt->string_table_length;
    efnt->offset_table_length = table_sz + ALIGN_SIZE(table_sz, 16);

    efnt->glyph_table_start  = 
	efnt->offset_table_start + efnt->offset_table_length;
    efnt->glyph_table_length  = glyph_table_size;

    /* Write EFNT header and FontInfo */
    efnt_fwrite(efnt, sizeof(epx_font_file_t), 1, f);

    /* Write string table */
    efnt_fwrite(string_table->data, sizeof(char), string_table->size, f);
    // Pad to 16-byte alignment */
    n = efnt->string_table_length - string_table->size;
    efnt_fwrite(pad, sizeof(char), n, f);

    /* Write offset table */
    offset = efnt->glyph_table_start;
    for (i = start; i <= stop; i++) {
	size_t sz = glyph_table->glyph_data[i].size;
	if (sz) {
	    DBGFMT("glyph: %d at offset = %u", i, offset);
	    efnt_fwrite(&offset, sizeof(u_int32_t), 1, f);
	    offset += sz;
	}
	else {
	    u_int32_t offset0 = 0;
	    DBGFMT("glyph: %d at offset = %u", i, offset0);
	    efnt_fwrite(&offset0, sizeof(u_int32_t), 1, f);
	}
    }
    table_sz = ((stop - start) + 1)*sizeof(u_int32_t);
    n = efnt->offset_table_length - table_sz;
    efnt_fwrite(pad, sizeof(char), n, f);

    /* write glyph table */
    for (i = start; i <= stop; i++) {
	size_t sz = glyph_table->glyph_data[i].size;
	if (sz)
	    efnt_fwrite(glyph_table->glyph_data[i].glyph, sizeof(char), sz, f);
    }
    efnt_fclose(f);
    return 0;
error:
    efnt_fclose(f);
    return -1;
}

/*
 *  usage: efnttool options <file> <size>
 *
 * options:
 *       -v                  verbose output
 *       -o <file>           output file name
 *       -autores            try to determine the screen resolution
 *       -xres <dpi>         target screen resolution in dpi
 *       -yres <dpi>         target screen resolution in dpi
 *       -bitmap             Load bitmap font if avail (no antialiased font)
 *       -efnt2              Used EFNT2 format (compressed alpha)
 *       -start <char>       First char code to use
 *       -end <char>         Last char code to use
 *       -latin1             Iso latin chars only
 *       -ascii              Ascii chars only
 */
int main(int argc, char** argv)
{
    int i = 1;
    char* outfile = NULL;
    char* fontfile = NULL;
    int   fontsize = 0;
    int   scale = 1;
    int   font_vmask;
    int   xres = 0;
    int   yres = 0;
    int   pixel_format = EPX_FORMAT_A8;
    int   bitmap = 0;
    epx_font_file_t efnt;
    string_table_t* string_table = new_string_table();
    glyph_table_t* glyph_table = new_glyph_table();
    int start_char = 0;
    int end_char = 65535;

    debug = 0;
    // setup efnt default values.
    memset(&efnt, 0, sizeof(epx_font_file_t));
    memcpy(efnt.magic, "EFNT", 4);
    efnt.font_info.pixel_format = EPX_FORMAT_BGRA;

    /* add a dummy string, this make sure that 
     * offsets do not start at 0
     * any zero offset will lead to the string "?"
     */
    add_string(string_table, "?");

    while(i < argc) {
	if (strcmp(argv[i], "-v") == 0) {
	    debug++;
	    i++;
	}
	else if (strcmp(argv[i], "-efnt2") == 0) {
	    pixel_format = EPX_FORMAT_EFNT2;
	    i++;
	}
	else if (strcmp(argv[i], "-latin1") == 0) {
	    end_char = 255;
	    i++;
	}
	else if (strcmp(argv[i], "-ascii") == 0) {
	    end_char = 127;
	    i++;
	}
	else if (strcmp(argv[i], "-xres") == 0) {
	    xres = atoi(argv[i+1]);
	    i += 2;
	}
	else if (strcmp(argv[i], "-yres") == 0) {
	    yres = atoi(argv[i+1]);
	    i += 2;
	}
	else if (strcmp(argv[i], "-scale") == 0) {
	    scale = atoi(argv[i+1]);
	    i += 2;
	}
	else if (strcmp(argv[i], "-start") == 0) {
	    start_char = atoi(argv[i+1]);
	    i += 2;
	}
	else if (strcmp(argv[i], "-end") == 0) {
	    end_char = atoi(argv[i+1]);
	    i += 2;
	}
	else if (strcmp(argv[i], "-bitmap") == 0) {
	    bitmap = 1;
	    i += 1;
	}
	else if (strncmp(argv[i], "-o", 2) == 0) {
	    if (argv[i][2] != '\0') {
		outfile = argv[i]+2;
		i += 1;
	    }
	    else if (argv[i+1] != 0) {
		outfile = argv[i+1];
		i += 2;
	    }
	    else
		usage();
	}
	else
	    break;
    }
    if (!argv[i] || !argv[i+1]) {
	usage();
    }
    fontfile = argv[i];
    fontsize = atoi(argv[i+1]);

    if ((xres ==  0) && (yres == 0)) {
	xres = DEFAULT_XRES;
	yres = DEFAULT_YRES;
    }
    else if (xres == 0) xres = yres;
    else if (yres == 0) yres = xres;

    if (outfile) {
	int len = strlen(outfile); 
	if ((len > 3) && (strcmp(outfile+len-2, ".c") == 0)) 
	    efnt_c_fmt = 1;
    }
    else {
	char* ptr;
	int len = strlen(fontfile);
	outfile = malloc(len + 6);
	strcpy(outfile, fontfile);
	if ((ptr = strrchr(outfile, '.')))
	    *ptr = '\0';
	if (pixel_format == EPX_FORMAT_EFNT2)
	    strcat(outfile, ".efnt2");
	else
	    strcat(outfile, "efnt");
    }

    DBGFMT("processing FT font %s:%d output to %s",fontfile,fontsize,outfile);	

    font_vmask=load_ft(fontfile, fontsize, scale, xres, yres, 
		       &efnt,string_table, glyph_table,
		       start_char, end_char, bitmap, pixel_format);
    if (font_vmask < 0)
	exit(1);
    
    if (write_efnt(outfile, &efnt, font_vmask,
		   string_table, glyph_table) < 0)
	exit(1);
    exit(0);    
}
