//
//  EPX Drawing primitives
//
#ifndef __EPX_DRAW_H__
#define __EPX_DRAW_H__

#include <wchar.h>
#include "epx_pixmap.h"
#include "epx_font.h"
#include "epx_gc.h"

extern void epx_font_draw_glyph(epx_gc_t*gc, epx_pixmap_t* dst, 
				int* x, int* y, int c);
extern void epx_font_draw_string(epx_gc_t* gc, epx_pixmap_t* dst,
				 int* x, int* y, char* string, size_t len);
extern void epx_font_draw_utf8(epx_gc_t* gc, epx_pixmap_t* dst,
			       int* x, int* y, char* string, size_t len);
extern void epx_font_draw_wide_string(epx_gc_t* gc, epx_pixmap_t* dst,
				      int* x, int* y, wchar_t* string, 
				      size_t len);

extern void epx_pixmap_draw_rectangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
				      int x, int y,
				      unsigned int width, 
				      unsigned int height);

extern void epx_pixmap_draw_point(epx_pixmap_t* pic, epx_gc_t* gc, 
				  int x, int y);

extern void epx_pixmap_draw_line(epx_pixmap_t* pixmap, epx_gc_t* gc,
				 int x0, int y0, 
				 int x1, int y1);

extern void epx_pixmap_draw_ellipse(epx_pixmap_t* pic, epx_gc_t* gc,
				    int x0, int y0,
				    unsigned int width, 
				    unsigned int height);

// NOT YET - SOON 
#if 0
extern void epx_pixmap_draw_triangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
				     int x0, int y0,
				     int x1, int y1,
				     int x2, int y2);


extern void epx_pixmap_draw_twin_line(epx_pixmap_t* pixmap, epx_gc_t* gc,
				      int x0, int y0, 
				      int x1, int y1,
				      int x2, int y2, 
				      int x3, int y3);

extern void epx_pixmap_tex_line(epx_pixmap_t* pic, epx_gc_t* gc,
				int x0, int y0, 
				int x1, int y1,
				epx_pixmap_t* tex,
				int tx0, int tx1, float ty);


#endif

#endif
