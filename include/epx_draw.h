/***************************************************************************
 *
 * Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
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
//  EPX Drawing primitives
//
#ifndef __EPX_DRAW_H__
#define __EPX_DRAW_H__

#include <wchar.h>
#include "epx_pixmap.h"
#include "epx_font.h"
#include "epx_gc.h"

extern void epx_draw_line_horizontal(epx_pixmap_t* pixmap,
				     int x1, int x2, int y,
				     int flags, epx_pixel_t fg);
extern void epx_draw_line_vertical(epx_pixmap_t* pixmap,
				   int x, int y1, int y2,
				   int flags, epx_pixel_t fg);

extern void epx_draw_line(epx_pixmap_t* pixmap,
			  int x0, int y0, int x1, int y1,
			  unsigned int line_width, int flags,
			  epx_pixel_t p);

extern void epx_draw_line_plain(epx_pixmap_t* pixmap,
				int x1, int y1,int x2,int y2,
				int flags, epx_pixel_t fg);

extern void epx_draw_line_thick(epx_pixmap_t* pixmap,
				int x0, int y0,
				int x1, int y1, int line_width,
				int flags, epx_pixel_t fg);

extern void epx_draw_ellipse(epx_pixmap_t* pic, epx_gc_t* gc,
			     int x, int y,
			     unsigned int width, unsigned int height,
			     unsigned int ww, unsigned int hh);

extern void epx_draw_ellipse_border(epx_pixmap_t* pixmap, epx_gc_t* gc,
				    int x, int y,
				    unsigned int width, unsigned int height,
				    unsigned int ww, unsigned int hh);

extern void epx_fill_triangle(epx_pixmap_t* pixmap,
			      int x0, int y0,
			      int x1, int y1,
			      int x2, int y2,
			      int flags, epx_pixel_t fg);

extern void epx_font_draw_glyph(epx_gc_t*gc, epx_pixmap_t* dst,
				int* x, int* y, int c);
extern void epx_font_draw_string(epx_gc_t* gc, epx_pixmap_t* dst,
				 int* x, int* y, char* string, size_t len);
extern void epx_font_draw_utf8(epx_gc_t* gc, epx_pixmap_t* dst,
			       int* x, int* y, char* string, size_t len);
extern void epx_font_draw_wide_string(epx_gc_t* gc, epx_pixmap_t* dst,
				      int* x, int* y, wchar_t* string,
				      size_t len);

extern void epx_draw_rectangle(epx_pixmap_t* pixmap,
			       int x, int y, 
			       unsigned int width, unsigned int height,
			       epx_flags_t ff, epx_pixel_t fc,
			       unsigned int bw, epx_flags_t bf, epx_pixel_t bc);

extern void epx_pixmap_draw_rectangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
				      int x, int y,
				      unsigned int width,
				      unsigned int height);

extern void epx_pixmap_draw_roundrect(epx_pixmap_t* pixmap, epx_gc_t* gc,
				      int x, int y,
				      unsigned int width, unsigned int height,
				      unsigned int rw, unsigned int rh);

extern void epx_pixmap_draw_point(epx_pixmap_t* pic, epx_gc_t* gc,
				  int x, int y);

extern void epx_pixmap_draw_line(epx_pixmap_t* pixmap, epx_gc_t* gc,
				 int x0, int y0,
				 int x1, int y1);

extern void epx_pixmap_draw_triangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
				     int x0, int y0,
				     int x1, int y1,
				     int x2, int y2);

extern void epx_pixmap_draw_ellipse(epx_pixmap_t* px, epx_gc_t* gc,
				    int x0, int y0,
				    unsigned int width, unsigned int height);

extern void draw_bary_triangles(epx_pixmap_t* px, epx_gc_t* gc,
				int* x, int* y, size_t n);

extern void draw_bary_poly(epx_pixmap_t* px, epx_gc_t* gc,
			   int* x, int* y, size_t n);

#endif
