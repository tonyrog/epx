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
// Pixel pack/unpack functions
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../include/epx_debug.h"
#include "../include/epx_pixel.h"
#include "../include/epx_colors.h"

static struct {
    const char* name;
    const epx_format_t pixel_format;
} epx_format_table[] =
{
    // special format names
    { "[565]",         EPX_FORMAT_R5G6B5_BE },
    { "[565]/big",     EPX_FORMAT_R5G6B5_BE },
    { "[565]/little",  EPX_FORMAT_R5G6B5_LE },
    { "[1555]",        EPX_FORMAT_A1R5G5B5_BE },
    { "[1555]/big",    EPX_FORMAT_A1R5G5B5_BE },
    { "[1555]/little", EPX_FORMAT_A1R5G5B5_LE },
    { "[555]",         EPX_FORMAT_R5G5B5X1_BE },
    { "[555]/big",     EPX_FORMAT_R5G5B5X1_BE },
    { "[555]/little",  EPX_FORMAT_R5G5B5X1_LE },
    { "[efnt2]",       EPX_FORMAT_EFNT2 },
    { 0, 0 }
};

//
// "parse" a name string and return a "matching" format
// r[<n>] g[<n>] b[<n>] a[<n>] x[<n>] _[<n>] g[<n>] l[<n>]
// /little /big /native
//
#define CI_A 0
#define CI_L 1
#define CI_R 2
#define CI_G 3
#define CI_B 4
#define CI_RGB(r,g,b) \
    ((c_size[CI_R]==(r)) && (c_size[CI_G]==(g)) && (c_size[CI_B]==(b)))
#define CI_TEST(a,l,r,g,b)			\
    (((c_size[CI_A] > 0) == (a)) &&		\
     ((c_size[CI_L] > 0) == (l)) &&		\
     ((c_size[CI_R] > 0) == (r)) &&		\
     ((c_size[CI_G] > 0) == (g)) &&		\
     ((c_size[CI_B] > 0) == (b)))


epx_format_t epx_pixel_format_from_name(char* name)
{
    char* ptr = name;
    int   c_offs[5];
    int   c_size[5];
    int   ci     = -1;
    int   fmt    = -1;
    int   bgr    = 0;
    int   alpha  = 0;
    int   alpha_first = 0;
    int   little = 0;
    int   offs   = 0;   // also bits per pixel
    int   i;

    // scan "special" formats
    i = 0;
    while(epx_format_table[i].name) {
	if (strcasecmp(name, epx_format_table[i].name) == 0)
	    return epx_format_table[i].pixel_format;
	i++;
    }

    for (i = 0; i < 5; i++) {
	c_offs[i] = -1;
	c_size[i] = 0;
    }

    // handle parsable formats
    while(*ptr) {
	switch(*ptr++) {
	case 'r': ci=CI_R; break;
	case 'g': ci=CI_G; break;
	case 'b': ci=CI_B; break;
	case 'x': ci=CI_A; alpha=0; break;
	case '_': ci=CI_A; alpha=0; break;
	case 'a': ci=CI_A; alpha=1; break;
	case 'l': ci=CI_L; break;
	case '/':
	    if (strcmp(ptr, "big") == 0) {
		little = 0;
		goto done;
	    }
	    else if (strcmp(ptr, "little") == 0) {
		little = 1;
		goto done;
	    }
	    else if (strcmp(ptr, "native") == 0) {
#if BYTE_ORDER == BIG_ENDIAN
		little = 0;
#elif BYTE_ORDER == LITTLE_ENDIAN
		little = 1;
#else
		return EPX_FORMAT_INVALID;
#endif
		goto done;
	    }
	    else
		return EPX_FORMAT_INVALID;
	    break;
	default:
	    return EPX_FORMAT_INVALID;
	}
	
	c_offs[ci] = offs;
	c_size[ci] = 8;
	if ( ((*ptr >= '0') && (*ptr <= '9')) ) {
	    c_size[ci] = (*ptr++-'0');
	    if ( ((*ptr >= '0') && (*ptr <= '9')) ) {
		c_size[ci] = c_size[ci]*10 + (*ptr++-'0');
	    }
	}
	offs += c_size[ci];
    }

done:
    switch(offs) {
    case 7:  offs=8; break;
    case 8:  break;
    case 15: offs=16; break;
    case 16: break;
    case 24: break;
    case 32: break;
    default: return EPX_FORMAT_INVALID;
    }

    if (c_offs[CI_A] >= 0) {
	alpha_first = (c_offs[CI_A] == 0);
    }

    if ((c_offs[CI_B]>=0) &&
	(c_offs[CI_B] < c_offs[CI_G]) && 
	(c_offs[CI_G] < c_offs[CI_R])) {
	bgr = 1;
    }

    if (CI_RGB(2,3,2))        fmt = EPX_FMT_RGB232;
    else if (CI_RGB(3,3,2))   fmt = EPX_FMT_RGB332;
    else if (CI_RGB(5,6,5))   fmt = EPX_FMT_RGB565;
    else if (CI_TEST(1,0,0,0,0)) fmt = EPX_FMT_ALPHA;
    else if (CI_TEST(0,1,0,0,0)) fmt = EPX_FMT_GRAY;
    else if (CI_TEST(1,1,0,0,0)) fmt = EPX_FMT_GRAY;
    else if (CI_TEST(0,0,1,0,0))  fmt = EPX_FMT_RED;
    else if (CI_TEST(0,0,0,1,0))  fmt = EPX_FMT_GREEN;
    else if (CI_TEST(0,0,0,0,1))  fmt = EPX_FMT_BLUE;
    else if ((c_size[CI_R]==c_size[CI_G]) && (c_size[CI_G]==c_size[CI_B])) {
	switch(c_size[CI_R]) {
	case 4:  fmt = EPX_FMT_RGB4; break;
	case 5:  fmt = EPX_FMT_RGB5; break;
	case 8:  fmt = EPX_FMT_RGB8; break;
	case 10: fmt = EPX_FMT_RGB10; break;
	case 12: fmt = EPX_FMT_RGB12; break;
	case 16: fmt = EPX_FMT_RGB16; break;
	default: return EPX_FORMAT_INVALID;
	}
    }
    else
	return EPX_FORMAT_INVALID;
    return EPX_FMT(fmt,bgr,alpha,alpha_first,little,offs);
}

// write format characters and sizes
static inline void put_fmt_size(char** pptr, char* eptr, char chan, int size)
{
    char* ptr = *pptr;

    if (size > 0) {
	if (ptr < eptr) *ptr = chan;
	ptr++;
	if (size != 8) {
	    if ((size >= 10) && (size <= 99)) {
		if (ptr < eptr) *ptr = (size / 10) + '0';
		ptr++;
		if (ptr < eptr)	*ptr = (size % 10) + '0';
		ptr++;
	    }
	    else if ((size > 0) && (size < 10)) {
		if (ptr < eptr) *ptr = size + '0';
		ptr++;
	    }
	}
    }
    *pptr = ptr;
}

// buffer must be aleast 4*4+7+1 bytes long
char* epx_pixel_format_to_name(epx_format_t fmt,char*buf,size_t size)
{
    char* ptr = buf;
    char* eptr = buf+size-1;
    int i = 0;
    int bpp = 0;
    int a_size = 0;
    int r_size = 0;
    int g_size = 0;
    int b_size = 0;
    int l_size = 0;

    while(epx_format_table[i].name) {
	if (epx_format_table[i].pixel_format == fmt)
	    return (char*) epx_format_table[i].name;
	i++;
    }

    bpp = EPX_PIXEL_BIT_SIZE(fmt);
    switch(EPX_PIXEL_FMT(fmt)) {
    case EPX_FMT_RGB4: a_size = bpp - 12; break;
    case EPX_FMT_RGB5: a_size = bpp - 15; break;
    case EPX_FMT_RGB8: a_size = bpp - 24; break;
    case EPX_FMT_RGB10: a_size = bpp - 30; break;
    case EPX_FMT_GRAY: if (bpp>8) a_size=8; break;
    case EPX_FMT_RED:  if (bpp>8) a_size=8; break;
    case EPX_FMT_GREEN: if (bpp>8) a_size=8; break;
    case EPX_FMT_ALPHA: a_size=bpp; break;
    default: break;
    }

    switch(EPX_PIXEL_FMT(fmt)) {
    case EPX_FMT_RGB4: r_size = g_size = b_size = 4; break;
    case EPX_FMT_RGB5: r_size = g_size = b_size = 5; break;
    case EPX_FMT_RGB8: r_size = g_size = b_size = 8; break;
    case EPX_FMT_RGB10: r_size = g_size = b_size = 10; break;
    case EPX_FMT_RGB232: r_size = 2; g_size = 3; b_size = 2; break;
    case EPX_FMT_RGB332: r_size = 3; g_size = 3; b_size = 2; break;
    case EPX_FMT_RGB565: r_size = 5; g_size = 6; b_size = 5; break;
    case EPX_FMT_GRAY:  l_size = bpp - a_size; break;
    case EPX_FMT_RED:   r_size = bpp - a_size; break;
    case EPX_FMT_GREEN: g_size = bpp - a_size; break;
    case EPX_FMT_BLUE:  b_size = bpp - a_size; break;
    default: break;
    }

    if (a_size && (fmt & EPX_F_AFirst)) {
	if (fmt & EPX_F_Alpha)
	    put_fmt_size(&ptr, eptr, 'a', a_size);
	else
	    put_fmt_size(&ptr, eptr, 'x', a_size);
    }

    put_fmt_size(&ptr, eptr, 'l', l_size);
    if (fmt & EPX_F_Bgr) {
	put_fmt_size(&ptr, eptr, 'b', b_size);
	put_fmt_size(&ptr, eptr, 'g', g_size);
	put_fmt_size(&ptr, eptr, 'r', r_size);
    }
    else {
	put_fmt_size(&ptr, eptr, 'r', b_size);
	put_fmt_size(&ptr, eptr, 'g', g_size);
	put_fmt_size(&ptr, eptr, 'b', r_size);
    }

    if (a_size && !(fmt & EPX_F_AFirst)) {
	if (fmt & EPX_F_Alpha)
	    put_fmt_size(&ptr, eptr, 'a', a_size);
	else
	    put_fmt_size(&ptr, eptr, 'x', a_size);
    }

    if (fmt & EPX_F_Little) {
	if (ptr < eptr) *ptr = '/';
	ptr++;
	if ((eptr-ptr) >= 6)
	    strcpy(ptr, "little");
	ptr += 6;
    }
    if (ptr < eptr) {
	*ptr = '\0';
	return buf;
    }
    return NULL;
}


static struct {
    const char* name;
    const epx_pixel_t pixel;
} epx_color_table[] =
{
    {"aliceBlue",      EPX_PIXEL_RGB(0xF0,0xF8,0xFF)  },
    {"antiqueWhite", EPX_PIXEL_RGB(0xFA,0xEB,0xD7)  },
    {"aqua",	     EPX_PIXEL_RGB(0x00,0xFF,0xFF)  },
    {"aquamarine",   EPX_PIXEL_RGB(0x7F,0xFF,0xD4)  },
    {"azure",	     EPX_PIXEL_RGB(0xF0,0xFF,0xFF)  },
    {"beige",	     EPX_PIXEL_RGB(0xF5,0xF5,0xDC)  },
    {"bisque",	     EPX_PIXEL_RGB(0xFF,0xE4,0xC4)  },
    {"black",	     EPX_PIXEL_RGB(0x00,0x00,0x00)  },
    {"blanchedAlmond",	EPX_PIXEL_RGB(0xFF,0xEB,0xCD)	},
    {"blue",	        EPX_PIXEL_RGB(0x00,0x00,0xFF)	},
    {"blueViolet",	EPX_PIXEL_RGB(0x8A,0x2B,0xE2)	},
    {"brown",	        EPX_PIXEL_RGB(0xA5,0x2A,0x2A)	},
    {"burlyWood",	EPX_PIXEL_RGB(0xDE,0xB8,0x87)	},
    {"cadetBlue",	EPX_PIXEL_RGB(0x5F,0x9E,0xA0)	},
    {"chartreuse",	EPX_PIXEL_RGB(0x7F,0xFF,0x00)	},
    {"chocolate",	EPX_PIXEL_RGB(0xD2,0x69,0x1E)	},
    {"coral",	EPX_PIXEL_RGB(0xFF,0x7F,0x50)	},
    {"cornflowerBlue",	EPX_PIXEL_RGB(0x64,0x95,0xED)	},
    {"cornsilk",	EPX_PIXEL_RGB(0xFF,0xF8,0xDC)	},
    {"crimson",	EPX_PIXEL_RGB(0xDC,0x14,0x3C)	},
    {"cyan",	EPX_PIXEL_RGB(0x00,0xFF,0xFF)	},
    {"darkBlue",	EPX_PIXEL_RGB(0x00,0x00,0x8B)	},
    {"darkCyan",	EPX_PIXEL_RGB(0x00,0x8B,0x8B)	},
    {"darkGoldenRod",	EPX_PIXEL_RGB(0xB8,0x86,0x0B)	},
    {"darkGray",	EPX_PIXEL_RGB(0xA9,0xA9,0xA9)	},
    {"darkGrey",	EPX_PIXEL_RGB(0xA9,0xA9,0xA9)	},
    {"darkGreen",	EPX_PIXEL_RGB(0x00,0x64,0x00)	},
    {"darkKhaki",	EPX_PIXEL_RGB(0xBD,0xB7,0x6B)	},
    {"darkMagenta",	EPX_PIXEL_RGB(0x8B,0x00,0x8B)	},
    {"darkOliveGreen",	EPX_PIXEL_RGB(0x55,0x6B,0x2F)	},
    {"darkorange",	EPX_PIXEL_RGB(0xFF,0x8C,0x00)	},
    {"darkOrchid",	EPX_PIXEL_RGB(0x99,0x32,0xCC)	},
    {"darkRed",	EPX_PIXEL_RGB(0x8B,0x00,0x00)	},
    {"darkSalmon",	EPX_PIXEL_RGB(0xE9,0x96,0x7A)	},
    {"darkSeaGreen",	EPX_PIXEL_RGB(0x8F,0xBC,0x8F)	},
    {"darkSlateBlue",	EPX_PIXEL_RGB(0x48,0x3D,0x8B)	},
    {"darkSlateGray",	EPX_PIXEL_RGB(0x2F,0x4F,0x4F)	},
    {"darkSlateGrey",	EPX_PIXEL_RGB(0x2F,0x4F,0x4F)	},
    {"darkTurquoise",	EPX_PIXEL_RGB(0x00,0xCE,0xD1)	},
    {"darkViolet",	EPX_PIXEL_RGB(0x94,0x00,0xD3)	},
    {"deepPink",	EPX_PIXEL_RGB(0xFF,0x14,0x93)	},
    {"deepSkyBlue",	EPX_PIXEL_RGB(0x00,0xBF,0xFF)	},
    {"dimGray",	EPX_PIXEL_RGB(0x69,0x69,0x69)	},
    {"dimGrey",	EPX_PIXEL_RGB(0x69,0x69,0x69)	},
    {"dodgerBlue",	EPX_PIXEL_RGB(0x1E,0x90,0xFF)	},
    {"fireBrick",	EPX_PIXEL_RGB(0xB2,0x22,0x22)	},
    {"floralWhite",	EPX_PIXEL_RGB(0xFF,0xFA,0xF0)	},
    {"forestGreen",	EPX_PIXEL_RGB(0x22,0x8B,0x22)	},
    {"fuchsia",	EPX_PIXEL_RGB(0xFF,0x00,0xFF)	},
    {"gainsboro",	EPX_PIXEL_RGB(0xDC,0xDC,0xDC)	},
    {"ghostWhite",	EPX_PIXEL_RGB(0xF8,0xF8,0xFF)	},
    {"gold",	EPX_PIXEL_RGB(0xFF,0xD7,0x00)	},
    {"goldenRod",	EPX_PIXEL_RGB(0xDA,0xA5,0x20)	},
    {"gray",	EPX_PIXEL_RGB(0x80,0x80,0x80)	},
    {"grey",	EPX_PIXEL_RGB(0x80,0x80,0x80)	},
    {"green",	EPX_PIXEL_RGB(0x00,0x80,0x00)	},
    {"greenYellow",	EPX_PIXEL_RGB(0xAD,0xFF,0x2F)	},
    {"honeyDew",	EPX_PIXEL_RGB(0xF0,0xFF,0xF0)	},
    {"hotPink",	EPX_PIXEL_RGB(0xFF,0x69,0xB4)	},
    {"indianRed", 	EPX_PIXEL_RGB(0xCD,0x5C,0x5C)	},
    {"indigo", 	EPX_PIXEL_RGB(0x4B,0x00,0x82)	},
    {"ivory",	EPX_PIXEL_RGB(0xFF,0xFF,0xF0)	},
    {"khaki",	EPX_PIXEL_RGB(0xF0,0xE6,0x8C)	},
    {"lavender",	EPX_PIXEL_RGB(0xE6,0xE6,0xFA)	},
    {"lavenderBlush",	EPX_PIXEL_RGB(0xFF,0xF0,0xF5)	},
    {"lawnGreen",	EPX_PIXEL_RGB(0x7C,0xFC,0x00)	},
    {"lemonChiffon",	EPX_PIXEL_RGB(0xFF,0xFA,0xCD)	},
    {"lightBlue",	EPX_PIXEL_RGB(0xAD,0xD8,0xE6)	},
    {"lightCoral",	EPX_PIXEL_RGB(0xF0,0x80,0x80)	},
    {"lightCyan",	EPX_PIXEL_RGB(0xE0,0xFF,0xFF)	},
    {"lightGoldenRodYellow",	EPX_PIXEL_RGB(0xFA,0xFA,0xD2)	},
    {"lightGray",	EPX_PIXEL_RGB(0xD3,0xD3,0xD3)	},
    {"lightGrey",	EPX_PIXEL_RGB(0xD3,0xD3,0xD3)	},
    {"lightGreen",	EPX_PIXEL_RGB(0x90,0xEE,0x90)	},
    {"lightPink",	EPX_PIXEL_RGB(0xFF,0xB6,0xC1)	},
    {"lightSalmon",	EPX_PIXEL_RGB(0xFF,0xA0,0x7A)	},
    {"lightSeaGreen",	EPX_PIXEL_RGB(0x20,0xB2,0xAA)	},
    {"lightSkyBlue",	EPX_PIXEL_RGB(0x87,0xCE,0xFA)	},
    {"lightSlateGray",	EPX_PIXEL_RGB(0x77,0x88,0x99)	},
    {"lightSlateGrey",	EPX_PIXEL_RGB(0x77,0x88,0x99)	},
    {"lightSteelBlue",	EPX_PIXEL_RGB(0xB0,0xC4,0xDE)	},
    {"lightYellow",	EPX_PIXEL_RGB(0xFF,0xFF,0xE0)	},
    {"lime",	EPX_PIXEL_RGB(0x00,0xFF,0x00)	},
    {"limeGreen",	EPX_PIXEL_RGB(0x32,0xCD,0x32)	},
    {"linen",	EPX_PIXEL_RGB(0xFA,0xF0,0xE6)	},
    {"magenta",	EPX_PIXEL_RGB(0xFF,0x00,0xFF)	},
    {"maroon",	EPX_PIXEL_RGB(0x80,0x00,0x00)	},
    {"mediumAquaMarine",	EPX_PIXEL_RGB(0x66,0xCD,0xAA)	},
    {"mediumBlue",	EPX_PIXEL_RGB(0x00,0x00,0xCD)	},
    {"mediumOrchid",	EPX_PIXEL_RGB(0xBA,0x55,0xD3)	},
    {"mediumPurple",	EPX_PIXEL_RGB(0x93,0x70,0xD8)	},
    {"mediumSeaGreen",	EPX_PIXEL_RGB(0x3C,0xB3,0x71)	},
    {"mediumSlateBlue",	EPX_PIXEL_RGB(0x7B,0x68,0xEE)	},
    {"mediumSpringGreen",	EPX_PIXEL_RGB(0x00,0xFA,0x9A)	},
    {"mediumTurquoise",	EPX_PIXEL_RGB(0x48,0xD1,0xCC)	},
    {"mediumVioletRed",	EPX_PIXEL_RGB(0xC7,0x15,0x85)	},
    {"midnightBlue",	EPX_PIXEL_RGB(0x19,0x19,0x70)	},
    {"mintCream",	EPX_PIXEL_RGB(0xF5,0xFF,0xFA)	},
    {"mistyRose",	EPX_PIXEL_RGB(0xFF,0xE4,0xE1)	},
    {"moccasin",	EPX_PIXEL_RGB(0xFF,0xE4,0xB5)	},
    {"navajoWhite",	EPX_PIXEL_RGB(0xFF,0xDE,0xAD)	},
    {"navy",	EPX_PIXEL_RGB(0x00,0x00,0x80)	},
    {"oldLace",	EPX_PIXEL_RGB(0xFD,0xF5,0xE6)	},
    {"olive",	EPX_PIXEL_RGB(0x80,0x80,0x00)	},
    {"oliveDrab",	EPX_PIXEL_RGB(0x6B,0x8E,0x23)	},
    {"orange",	EPX_PIXEL_RGB(0xFF,0xA5,0x00)	},
    {"orangeRed",	EPX_PIXEL_RGB(0xFF,0x45,0x00)	},
    {"orchid",	EPX_PIXEL_RGB(0xDA,0x70,0xD6)	},
    {"paleGoldenRod",	EPX_PIXEL_RGB(0xEE,0xE8,0xAA)	},
    {"paleGreen",	EPX_PIXEL_RGB(0x98,0xFB,0x98)	},
    {"paleTurquoise",	EPX_PIXEL_RGB(0xAF,0xEE,0xEE)	},
    {"paleVioletRed",	EPX_PIXEL_RGB(0xD8,0x70,0x93)	},
    {"papayaWhip",  EPX_PIXEL_RGB(0xFF,0xEF,0xD5)	},
    {"peachPuff",	  EPX_PIXEL_RGB(0xFF,0xDA,0xB9)	},
    {"peru",	  EPX_PIXEL_RGB(0xCD,0x85,0x3F)	},
    {"pink",	  EPX_PIXEL_RGB(0xFF,0xC0,0xCB)	},
    {"plum",	  EPX_PIXEL_RGB(0xDD,0xA0,0xDD)	},
    {"powderBlue",  EPX_PIXEL_RGB(0xB0,0xE0,0xE6)	},
    {"purple",	  EPX_PIXEL_RGB(0x80,0x00,0x80)	},
    {"red",	  EPX_PIXEL_RGB(0xFF,0x00,0x00)	},
    {"rosyBrown",	  EPX_PIXEL_RGB(0xBC,0x8F,0x8F)	},
    {"royalBlue",	  EPX_PIXEL_RGB(0x41,0x69,0xE1)	},
    {"saddleBrown", EPX_PIXEL_RGB(0x8B,0x45,0x13)	},
    {"salmon",	  EPX_PIXEL_RGB(0xFA,0x80,0x72)	},
    {"sandyBrown",  EPX_PIXEL_RGB(0xF4,0xA4,0x60)	},
    {"seaGreen",	  EPX_PIXEL_RGB(0x2E,0x8B,0x57)	},
    {"seaShell",	  EPX_PIXEL_RGB(0xFF,0xF5,0xEE)	},
    {"sienna",	  EPX_PIXEL_RGB(0xA0,0x52,0x2D)	},
    {"silver",	  EPX_PIXEL_RGB(0xC0,0xC0,0xC0)	},
    {"skyBlue",	  EPX_PIXEL_RGB(0x87,0xCE,0xEB)	},
    {"slateBlue",	  EPX_PIXEL_RGB(0x6A,0x5A,0xCD)	},
    {"slateGray",	  EPX_PIXEL_RGB(0x70,0x80,0x90)	},
    {"slateGrey",	  EPX_PIXEL_RGB(0x70,0x80,0x90)	},
    {"snow",	          EPX_PIXEL_RGB(0xFF,0xFA,0xFA)	},
    {"springGreen",       EPX_PIXEL_RGB(0x00,0xFF,0x7F)	},
    {"steelBlue",	  EPX_PIXEL_RGB(0x46,0x82,0xB4)	},
    {"tan",	          EPX_PIXEL_RGB(0xD2,0xB4,0x8C)	},
    {"teal",	          EPX_PIXEL_RGB(0x00,0x80,0x80)	},
    {"thistle",	          EPX_PIXEL_RGB(0xD8,0xBF,0xD8)	},
    {"tomato",	          EPX_PIXEL_RGB(0xFF,0x63,0x47)	},
    {"turquoise",	  EPX_PIXEL_RGB(0x40,0xE0,0xD0)	},
    {"violet",	          EPX_PIXEL_RGB(0xEE,0x82,0xEE)	},
    {"wheat",	          EPX_PIXEL_RGB(0xF5,0xDE,0xB3)	},
    {"white",	          EPX_PIXEL_RGB(0xFF,0xFF,0xFF)	},
    {"whiteSmoke",        EPX_PIXEL_RGB(0xF5,0xF5,0xF5) },
    {"yellow",	          EPX_PIXEL_RGB(0xFF,0xFF,0x00)	},
    {"yellowGreen",       EPX_PIXEL_RGB(0x9A,0xCD,0x32) },
    { 0, EPX_PIXEL_RGB(0,0,0)}
};

// convert hex intege format 0xaarrggbb or color name to pixel
epx_pixel_t epx_pixel_from_string(char* name)
{
    char* eptr;
    unsigned long c;

    c = strtol(name,  &eptr, 16);
    if (*eptr == '\0') {
	epx_pixel_t p;
	int a = (c >> 24);
	p.a = (a==0) ? 255 : a;
	p.r = (c >> 16) & 0xff;
	p.g = (c >> 8) & 0xff;
	p.b = c & 0xff;
	return p;
    }
    else {
	int i = 0;

	while(epx_color_table[i].name) {
	    if (strcasecmp(name, epx_color_table[i].name) == 0)
		return epx_color_table[i].pixel;
	    i++;
	}
    }
    return epx_pixel_black;
}


// unpack functions
epx_pixel_t epx_unpack_a8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = src[0];
    p.r = 0;
    p.g = 0;
    p.b = 0;
    return p;
}

epx_pixel_t epx_unpack_r8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = EPX_ALPHA_OPAQUE;
    p.r = src[0];
    p.g = 0;
    p.b = 0;
    return p;
}

epx_pixel_t epx_unpack_g8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = EPX_ALPHA_OPAQUE;
    p.r = 0;
    p.g = src[0];
    p.b = 0;
    return p;
}

epx_pixel_t epx_unpack_b8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = EPX_ALPHA_OPAQUE;
    p.r = 0;
    p.g = 0;
    p.b = src[0];
    return p;
}

epx_pixel_t epx_unpack_r8g8b8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = EPX_ALPHA_OPAQUE;
    p.r = src[0];
    p.g = src[1];
    p.b = src[2];
    return p;
}

epx_pixel_t epx_unpack_r8g8b8a8(uint8_t* src)
{
    epx_pixel_t p;
    p.r=src[0];
    p.g=src[1];
    p.b=src[2];
    p.a=src[3];
    return p;
}


epx_pixel_t epx_unpack_a8r8g8b8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = src[0];
    p.r = src[1];
    p.g = src[2];
    p.b = src[3];
    return p;
}

epx_pixel_t epx_unpack_b8g8r8(uint8_t* src)
{
    epx_pixel_t p;
    p.b=src[0];
    p.g=src[1];
    p.r=src[2];
    p.a = EPX_ALPHA_OPAQUE;
    return p;
}

epx_pixel_t epx_unpack_b8g8r8a8(uint8_t* src)
{
    epx_pixel_t p;
    p.b=src[0];
    p.g=src[1];
    p.r=src[2];
    p.a=src[3];
    return p;
}

epx_pixel_t epx_unpack_a8b8g8r8(uint8_t* src)
{
    epx_pixel_t p;
    p.a=src[0];
    p.b=src[1];
    p.g=src[2];
    p.r=src[3];
    return p;
}

epx_pixel_t epx_unpack_a8y8u8v8(uint8_t* src)
{
    epx_pixel_yuv_t yuv;
    epx_pixel_t p;
    yuv.a = src[0];
    yuv.y = src[1];
    yuv.u = src[2];
    yuv.v = src[3];
    p = epx_pixel_yuv2rgb(yuv);
    return p;
}

epx_pixel_t epx_unpack_r5g5b5a1(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[0]<<8 | src[1];
    p.r = (v >> 8) & 0xf8;
    p.g = (v >> 3) & 0xf8;
    p.b = (v << 2) & 0xf8;
    p.a = (v&1) ? 255 : 0;
    return p;
}

epx_pixel_t epx_unpack_r5g5b5a1_le(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[1]<<8 | src[0];
    p.r = (v >> 8) & 0xf8;
    p.g = (v >> 3) & 0xf8;
    p.b = (v << 2) & 0xf8;
    p.a = (v&1) ? 255 : 0;
    return p;
}

epx_pixel_t epx_unpack_a1r5g5b5(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[0]<<8 | src[1];
    p.r = (v >> 7) & 0xf8;
    p.g = (v >> 2) & 0xf8;
    p.b = (v << 3) & 0xf8;
    p.a = (v >> 15) ? 255 : 0;
    return p;
}

epx_pixel_t epx_unpack_a1r5g5b5_le(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[1]<<8 | src[0];
    p.r = (v >> 7) & 0xf8;
    p.g = (v >> 2) & 0xf8;
    p.b = (v << 3) & 0xf8;
    p.a = (v >> 15) ? 255 : 0;
    return p;
}

epx_pixel_t epx_unpack_r5g6b5_be(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[0]<<8 | src[1];
    /* shift and scale to range [0-255] */
    p.r=(v >> 8) & 0xf8;
    p.g=(v >> 3) & 0xfc;
    p.b=(v & 0x1f) << 3;
    p.a=255;
    return p;
}

epx_pixel_t epx_unpack_r5g6b5_le(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[1]<<8 | src[0];
    /* shift and scale to range [0-255] */
    p.r=(v >> 8) & 0xf8;
    p.g=(v >> 3) & 0xfc;
    p.b=(v & 0x1f) << 3;
    p.a=255;
    return p;
}


epx_pixel_t epx_unpack_b5g6r5_be(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[0]<<8 | src[1];
    /* shift and scale to range [0-255] */
    p.b=(v >> 8) & 0xf8;
    p.g=(v >> 3) & 0xfc;
    p.r=(v & 0x1f) << 3;
    p.a=255;
    return p;
}

epx_pixel_t epx_unpack_b5g6r5_le(uint8_t* src)
{
    epx_pixel_t p;
    uint16_t v = src[1]<<8 | src[0];
    /* shift and scale to range [0-255] */
    p.b=(v >> 8) & 0xf8;
    p.g=(v >> 3) & 0xfc;
    p.r=(v & 0x1f) << 3;
    p.a=255;
    return p;
}

epx_pixel_t epx_unpack_l8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = EPX_ALPHA_OPAQUE;
    p.r = src[0];
    p.g = src[0];
    p.b = src[0];
    return p;
}

epx_pixel_t epx_unpack_a8l8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = src[0];
    p.r = src[1];
    p.g = src[1];
    p.b = src[1];
    return p;
}

epx_pixel_t epx_unpack_l8a8(uint8_t* src)
{
    epx_pixel_t p;
    p.a = src[1];
    p.r = src[0];
    p.g = src[0];
    p.b = src[0];
    return p;
}

epx_pixel_unpack_t epx_pixel_unpack_func(epx_format_t fmt)
{
    switch (fmt) {
    case EPX_FORMAT_EFNT2:    return epx_unpack_a8;
    case EPX_FORMAT_A8:       return epx_unpack_a8;
    case EPX_FORMAT_R8:       return epx_unpack_r8;
    case EPX_FORMAT_G8:       return epx_unpack_g8;
    case EPX_FORMAT_B8:       return epx_unpack_b8;
    case EPX_FORMAT_L8:       return epx_unpack_l8;
    case EPX_FORMAT_R8G8B8:   return epx_unpack_r8g8b8;
    case EPX_FORMAT_B8G8R8:   return epx_unpack_b8g8r8;

    case EPX_FORMAT_R8G8B8A8_BE: return epx_unpack_r8g8b8a8;
    case EPX_FORMAT_A8B8G8R8_LE: return epx_unpack_r8g8b8a8;
    case EPX_FORMAT_R8G8B8X8_BE: return epx_unpack_r8g8b8a8;
    case EPX_FORMAT_X8B8G8R8_LE: return epx_unpack_r8g8b8a8;

    case EPX_FORMAT_A8R8G8B8_BE: return epx_unpack_a8r8g8b8;
    case EPX_FORMAT_B8G8R8A8_LE: return epx_unpack_a8r8g8b8;
    case EPX_FORMAT_X8R8G8B8_BE: return epx_unpack_a8r8g8b8;
    case EPX_FORMAT_B8G8R8X8_LE: return epx_unpack_a8r8g8b8;

    case EPX_FORMAT_B8G8R8A8_BE: return epx_unpack_b8g8r8a8;
    case EPX_FORMAT_A8R8G8B8_LE: return epx_unpack_b8g8r8a8;
    case EPX_FORMAT_B8G8R8X8_BE: return epx_unpack_b8g8r8a8;
    case EPX_FORMAT_X8R8G8B8_LE: return epx_unpack_b8g8r8a8;

    case EPX_FORMAT_A8B8G8R8_BE: return epx_unpack_a8b8g8r8;
    case EPX_FORMAT_R8G8B8A8_LE: return epx_unpack_a8b8g8r8;
    case EPX_FORMAT_X8B8G8R8_BE: return epx_unpack_a8b8g8r8;
    case EPX_FORMAT_R8G8B8X8_LE: return epx_unpack_a8b8g8r8;

    case EPX_FORMAT_A8Y8U8V8: return epx_unpack_a8y8u8v8;
    case EPX_FORMAT_R5G5B5A1_BE: return epx_unpack_r5g5b5a1;
    case EPX_FORMAT_R5G5B5X1_BE: return epx_unpack_r5g5b5a1;
    case EPX_FORMAT_R5G5B5X1_LE: return epx_unpack_r5g5b5a1_le;
    case EPX_FORMAT_A1R5G5B5_BE: return epx_unpack_a1r5g5b5;
    case EPX_FORMAT_X1R5G5B5_BE: return epx_unpack_a1r5g5b5;
    case EPX_FORMAT_A1R5G5B5_LE: return epx_unpack_a1r5g5b5_le;
    case EPX_FORMAT_X1R5G5B5_LE: return epx_unpack_a1r5g5b5_le;
    case EPX_FORMAT_R5G6B5_BE: return epx_unpack_r5g6b5_be;
    case EPX_FORMAT_R5G6B5_LE: return epx_unpack_r5g6b5_le;
    case EPX_FORMAT_B5G6R5_BE: return epx_unpack_b5g6r5_be;
    case EPX_FORMAT_B5G6R5_LE: return epx_unpack_b5g6r5_le;
    case EPX_FORMAT_A8L8_BE:   return epx_unpack_a8l8;
    case EPX_FORMAT_L8A8_BE:   return epx_unpack_l8a8;
    default:
	EPX_DBGFMT("epx_pixel_unpack_func: undefined func: %x", fmt);
	return 0;
    }
}


void epx_pack_a8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.a;
}

void epx_pack_r8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.r;
}

void epx_pack_g8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.g;
}

void epx_pack_b8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.b;
}

void epx_pack_r8g8b8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.r;
    dst[1]=p.g;
    dst[2]=p.b;
}

void epx_pack_r8g8b8a8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.r;
    dst[1]=p.g;
    dst[2]=p.b;
    dst[3]=p.a;
}

void epx_pack_a8r8g8b8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.a;
    dst[1]=p.r;
    dst[2]=p.g;
    dst[3]=p.b;
}

void epx_pack_b8g8r8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.b;
    dst[1]=p.g;
    dst[2]=p.r;
}

void epx_pack_b8g8r8a8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.b;
    dst[1]=p.g;
    dst[2]=p.r;
    dst[3]=p.a;
}

void epx_pack_a8b8g8r8(epx_pixel_t p, uint8_t* dst)
{
    dst[0]=p.a;
    dst[1]=p.b;
    dst[2]=p.g;
    dst[3]=p.r;
}

void epx_pack_r5g5b5a1(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.r>>3)<<11)|((p.g>>3)<<6)|((p.b>>3) <<1)|((p.a)!=0);
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epx_pack_r5g5b5a1_le(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.r>>3)<<11)|((p.g>>3)<<6)|((p.b>>3) <<1)|((p.a)!=0);
    dst[1] = (v >> 8);
    dst[0] = v & 0xff;
}

void epx_pack_a1r5g5b5(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.r>>3)<<10)|((p.g>>3)<<5)|((p.b>>3))|(((p.a)!=0)<<15);
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epx_pack_a1r5g5b5_le(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.r>>3)<<10)|((p.g>>3)<<5)|((p.b>>3))|(((p.a)!=0)<<15);
    dst[1] = (v >> 8);
    dst[0] = v & 0xff;
}

void epx_pack_r5g6b5_be(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.r>>3) << 11) | ((p.g>>2) << 5)  | ((p.b>>3));
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epx_pack_r5g6b5_le(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.r>>3) << 11) | ((p.g>>2) << 5)  | ((p.b>>3));
    dst[0] = v & 0xff;
    dst[1] = (v >> 8);
}


void epx_pack_b5g6r5_be(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.b>>3) << 11) | ((p.g>>2) << 5)  | ((p.r>>3));
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epx_pack_b5g6r5_le(epx_pixel_t p, uint8_t* dst)
{
    uint16_t v = ((p.b>>3) << 11) | ((p.g>>2) << 5)  | ((p.r>>3));
    dst[0] = v & 0xff;
    dst[1] = (v >> 8);
}

void epx_pack_a8y8u8v8(epx_pixel_t p, uint8_t* dst)
{
    epx_pixel_yuv_t yuv = epx_pixel_rgb2yuv(p);
    dst[0] = yuv.a;
    dst[1] = yuv.y;
    dst[2] = yuv.u;
    dst[3] = yuv.v;
}

void epx_pack_l8(epx_pixel_t p, uint8_t* dst)
{
    dst[0] = epx_pixel_luminance(p);
}

void epx_pack_a8l8(epx_pixel_t p, uint8_t* dst)
{
    dst[0] = p.a;
    dst[1] = epx_pixel_luminance(p);
}

void epx_pack_l8a8(epx_pixel_t p, uint8_t* dst)
{
    dst[1] = p.a;
    dst[0] = epx_pixel_luminance(p);
}

epx_pixel_pack_t epx_pixel_pack_func(epx_format_t fmt)
{
    switch (fmt) {
    case EPX_FORMAT_EFNT2:    return epx_pack_a8;
    case EPX_FORMAT_A8:       return epx_pack_a8;
    case EPX_FORMAT_R8:       return epx_pack_r8;
    case EPX_FORMAT_G8:       return epx_pack_g8;
    case EPX_FORMAT_B8:       return epx_pack_b8;
    case EPX_FORMAT_L8:       return epx_pack_l8;
    case EPX_FORMAT_R8G8B8:   return epx_pack_r8g8b8;
    case EPX_FORMAT_B8G8R8:   return epx_pack_b8g8r8;


    case EPX_FORMAT_R8G8B8A8_BE: return epx_pack_r8g8b8a8;
    case EPX_FORMAT_A8B8G8R8_LE: return epx_pack_r8g8b8a8;
    case EPX_FORMAT_R8G8B8X8_BE: return epx_pack_r8g8b8a8;
    case EPX_FORMAT_X8B8G8R8_LE: return epx_pack_r8g8b8a8;

    case EPX_FORMAT_A8R8G8B8_BE: return epx_pack_a8r8g8b8;
    case EPX_FORMAT_B8G8R8A8_LE: return epx_pack_a8r8g8b8;
    case EPX_FORMAT_X8R8G8B8_BE: return epx_pack_a8r8g8b8;
    case EPX_FORMAT_B8G8R8X8_LE: return epx_pack_a8r8g8b8;

    case EPX_FORMAT_B8G8R8A8_BE: return epx_pack_b8g8r8a8;
    case EPX_FORMAT_A8R8G8B8_LE: return epx_pack_b8g8r8a8;
    case EPX_FORMAT_B8G8R8X8_BE: return epx_pack_b8g8r8a8;
    case EPX_FORMAT_X8R8G8B8_LE: return epx_pack_b8g8r8a8;

    case EPX_FORMAT_A8B8G8R8_BE: return epx_pack_a8b8g8r8;
    case EPX_FORMAT_R8G8B8A8_LE: return epx_pack_a8b8g8r8;
    case EPX_FORMAT_X8B8G8R8_BE: return epx_pack_a8b8g8r8;
    case EPX_FORMAT_R8G8B8X8_LE: return epx_pack_a8b8g8r8;

    case EPX_FORMAT_A8Y8U8V8: return epx_pack_a8y8u8v8;
    case EPX_FORMAT_R5G5B5A1_BE: return epx_pack_r5g5b5a1;
    case EPX_FORMAT_R5G5B5X1_BE: return epx_pack_r5g5b5a1;
    case EPX_FORMAT_R5G5B5A1_LE: return epx_pack_r5g5b5a1_le;
    case EPX_FORMAT_R5G5B5X1_LE: return epx_pack_r5g5b5a1_le;

    case EPX_FORMAT_A1R5G5B5_BE: return epx_pack_a1r5g5b5;
    case EPX_FORMAT_X1R5G5B5_BE: return epx_pack_a1r5g5b5;
    case EPX_FORMAT_R5G6B5_BE:   return epx_pack_r5g6b5_be;
    case EPX_FORMAT_R5G6B5_LE:   return epx_pack_r5g6b5_le;
    case EPX_FORMAT_B5G6R5_BE:   return epx_pack_b5g6r5_be;
    case EPX_FORMAT_B5G6R5_LE:   return epx_pack_b5g6r5_le;
    case EPX_FORMAT_A8L8_BE:     return epx_pack_a8l8;
    case EPX_FORMAT_L8A8_BE:     return epx_pack_l8a8;
    default:
	EPX_DBGFMT("epx_pixel_pack_func: undefined func: %x", fmt);
	return 0;
    }
}



