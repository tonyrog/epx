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
// Shape drawing api
//
#include "../include/epx_pixel.h"
#include "../include/epx_pixmap.h"
#include "../include/epx_gc.h"
#include "../include/epx_draw.h"
#include "../include/epx_colors.h"


// style bits that make something to be painted, a style without any
// of these (none or []) means nothing is drawn
#define EPX_PAINT_STYLE_MASK \
    (EPX_FLAG_SOLID|EPX_FLAG_BLEND|EPX_FLAG_SUM|EPX_FLAG_AALIAS| \
     EPX_FLAG_TEXTURED|EPX_LINE_STYLE_DASHED|EPX_BORDER_STYLE_DASHED)

#define no_paint(ff) (((ff) & EPX_PAINT_STYLE_MASK) == 0)

#define no_border(ff) \
    ((((ff) & EPX_BORDER_STYLE_NBORDER) == EPX_BORDER_STYLE_NBORDER) || \
     no_paint(ff))

/* put pixel given address */
static inline void put_apixel(uint8_t* addr,
			      epx_pixel_unpack_t unpack,
			      epx_pixel_pack_t pack,
			      epx_flags_t flags, epx_pixel_t s)
{
    if (((flags & EPX_FLAG_BLEND)==0) || (s.a == EPX_ALPHA_OPAQUE))
	pack(s, addr);
    else if (s.a != EPX_ALPHA_TRANSPARENT) {
	epx_pixel_t d = unpack(addr);
	d = epx_pixel_blend(s.a, s, d);
	pack(d, addr);
    }
}

static inline void fill_rect(epx_pixmap_t* pixmap,
			     int x, int y,
			     unsigned int width, unsigned int height,
			     epx_flags_t ff, epx_pixel_t fc)
{
    epx_rect_t r, r0;
    uint8_t* ptr;
    
    epx_rect_set(&r,x,y,width,height);
    if (!epx_rect_intersect(&r, &pixmap->clip, &r0))
	return;
    ptr = EPX_PIXEL_ADDR(pixmap,r0.xy.x,r0.xy.y);
    if (((ff & EPX_FLAG_BLEND)==0) || (fc.a == EPX_ALPHA_OPAQUE))
	epx_fill_area(ptr,pixmap->bytes_per_row,pixmap->pixel_format,fc,
		      r0.wh.width,r0.wh.height);
    else if (fc.a != EPX_ALPHA_TRANSPARENT)
	epx_fill_area_blend(ptr,pixmap->bytes_per_row,
			    pixmap->pixel_format,fc,
			    r0.wh.width,r0.wh.height);
}

//
// draw a point at (x,y) in foreground color (using line style)
//
void epx_pixmap_draw_point(epx_pixmap_t* pic, epx_gc_t* gc, int x, int y)
{
    uint8_t* dst;

    /* First check clip */
    if (!epx_point_xy_in_rect(x, y, &pic->clip))
	return;
    if (!gc) gc = &epx_default_gc;
    if (no_paint(gc->line_style))
	return;
    dst = EPX_PIXEL_ADDR(pic,x,y);
    put_apixel(dst,pic->func.unpack,pic->func.pack,
	       gc->line_style,gc->foreground_color);
}

void epx_draw_rectangle(epx_pixmap_t* pixmap,
			int x, int y, unsigned int width, unsigned int height,
			epx_flags_t ff, epx_pixel_t fc,
			unsigned int bw, epx_flags_t bf, epx_pixel_t bc)
{
    epx_rect_t r;

    if (no_paint(bf))
	bw = 0;   // border style none

    epx_rect_set(&r,x,y,width,height);

    if ((ff == EPX_FILL_STYLE_NONE) && (bw <= 1))
	;
    else if (bw > 0) {
	unsigned int d;
	switch(bf & EPX_BORDER_LOCATION_MASK) {
	case EPX_BORDER_LOCATION_INSIDE:
	    break;
	case EPX_BORDER_LOCATION_CENTER:
	    d = bw/2;
	    r.xy.x -= d;
	    r.xy.y -= d;
	    r.wh.width  += 2*d;
	    r.wh.height += 2*d;
	    break;
	case EPX_BORDER_LOCATION_OUTSIDE:
	default:
	    r.xy.x -= bw;
	    r.xy.y -= bw;
	    r.wh.width += 2*bw;
	    r.wh.height += 2*bw;
	    break;
	}
    }
	
    if (!epx_rect_intersect(&r, &pixmap->clip, NULL))
	return;

    x = r.xy.x;
    y = r.xy.y;
    width = r.wh.width;
    height = r.wh.height;

    if ((width == 0) || (height == 0))
	return;

    if ((ff == EPX_FILL_STYLE_NONE) && (bw <= 1)) {
	if (no_paint(bf))
	    return;
	bf |= EPX_LINE_STYLE_NFIRST;
	if (!(bf & EPX_BORDER_STYLE_NTOP))
	    epx_draw_line_horizontal(pixmap,x,x+width-1,y,bf,bc);
	if ((height > 1) && !(bf & EPX_BORDER_STYLE_NBOTTOM))
	    epx_draw_line_horizontal(pixmap,x,x+width-1,y+height-1,bf,bc);
	if (height > 2) {
	    if (!(bf & EPX_BORDER_STYLE_NLEFT))
		epx_draw_line_vertical(pixmap,x,y+1,y+height-2,bf,bc);
	    if ((width > 1) && !(bf & EPX_BORDER_STYLE_NRIGHT))
		epx_draw_line_vertical(pixmap,x+width-1,y+1,y+height-2,bf,bc);
	}
	return;
    }
    if (bw > 0) {
	// border band widths, never wider than half the rectangle
	unsigned int bt = (2*bw > height) ? (height+1)/2 : bw; // top
	unsigned int bb = (2*bw > height) ? height/2 : bw;     // bottom
	unsigned int bl = (2*bw > width)  ? (width+1)/2 : bw;  // left
	unsigned int br = (2*bw > width)  ? width/2 : bw;      // right
	unsigned int ih = height - bt - bb;   // inner height (>= 0)
	unsigned int iw = width - bl - br;    // inner width (>= 0)
	if (!(bf & EPX_BORDER_STYLE_NTOP))
	    fill_rect(pixmap, x, y, width, bt, bf, bc);
	if (!(bf & EPX_BORDER_STYLE_NBOTTOM) && (bb > 0))
	    fill_rect(pixmap, x, y+height-bb, width, bb, bf, bc);
	if (ih > 0) {
	    if (!(bf & EPX_BORDER_STYLE_NLEFT))
		fill_rect(pixmap, x, y+bt, bl, ih, bf, bc);
	    if (!(bf & EPX_BORDER_STYLE_NRIGHT) && (br > 0))
		fill_rect(pixmap, x+width-br, y+bt, br, ih, bf, bc);
	}
	if ((ff != EPX_FILL_STYLE_NONE) && (iw > 0) && (ih > 0))
	    fill_rect(pixmap, x+bl, y+bt, iw, ih, ff, fc);
    }
    else if (ff != EPX_FILL_STYLE_NONE)
	fill_rect(pixmap, x, y, width, height, ff, fc);
}

// Draw rectangle (x,y,w,h)
void epx_pixmap_draw_rectangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
			       int x, int y,
			       unsigned int width,
			       unsigned int height)
{
    if (!gc) gc = &epx_default_gc;

    epx_draw_rectangle(pixmap, x, y, width, height,
		       gc->fill_style, gc->fill_color,
		       gc->border_width, gc->border_style, gc->border_color);
}

void epx_pixmap_draw_line(epx_pixmap_t* pixmap, epx_gc_t* gc,
			  int x0, int y0,
			  int x1, int y1)
{
    if (!gc) gc = &epx_default_gc;
    if (no_paint(gc->line_style))
	return;

    if (gc->line_width == 1) {
	if (y1 == y0)
	    epx_draw_line_horizontal(pixmap,x0,x1,y0,
				     gc->line_style,
				     gc->foreground_color);
	else if (x1 == x0) {
	    epx_draw_line_vertical(pixmap,x0,y0,y1,
				   gc->line_style,
				   gc->foreground_color);
	}
	else
	    epx_draw_line_plain(pixmap,x0,y0,x1,y1,
				gc->line_style,
				gc->foreground_color);
    }
    else if (gc->line_width > 1)
	epx_draw_line_thick(pixmap,x0,y0,x1,y1,gc->line_width,gc->line_style,
			    gc->foreground_color);
}

void epx_pixmap_draw_ellipse(epx_pixmap_t* pixmap, epx_gc_t* gc,
			     int x, int y,
			     unsigned int width, unsigned int height)
{
    if (gc == NULL) gc = &epx_default_gc;

    if (no_border(gc->border_style) || (gc->border_width == 0)) {
	epx_draw_ellipse(pixmap, gc, x, y, width, height, 0, 0);
    }
    else {
	epx_draw_ellipse_border(pixmap, gc, x, y, width, height, 0, 0);
    }
}

void epx_pixmap_draw_roundrect(epx_pixmap_t* pixmap, epx_gc_t* gc,
			       int x, int y,
			       unsigned int width, unsigned int height,
			       unsigned int rw, unsigned int rh)
{
    epx_flags_t ff, bf;
    unsigned int bw;
    epx_rect_t r;
    
    if (gc == NULL) gc = &epx_default_gc;

    epx_rect_set(&r,x,y,width,height);

    bf = gc->border_style;
    bw = no_border(bf) ? 0 : gc->border_width;
    ff = gc->fill_style;

    if ((ff == EPX_FILL_STYLE_NONE) && (bw <= 1))
	;
    else if (bw > 0) {
	unsigned int d;
	switch(bf & EPX_BORDER_LOCATION_MASK) {
	case EPX_BORDER_LOCATION_INSIDE:
	    break;
	case EPX_BORDER_LOCATION_CENTER:
	    d = bw/2;
	    r.xy.x -= d;
	    r.xy.y -= d;
	    r.wh.width  += 2*d;
	    r.wh.height += 2*d;
	    rw += d;
	    rh += d;
	    break;
	case EPX_BORDER_LOCATION_OUTSIDE:
	default:
	    r.xy.x -= bw;
	    r.xy.y -= bw;
	    r.wh.width += 2*bw;
	    r.wh.height += 2*bw;
	    rw += bw;
	    rh += bw;
	    break;
	}
    }

    if ((r.wh.width == 0) || (r.wh.height == 0))
	return;
    if (!epx_rect_intersect(&r, &pixmap->clip, NULL))
	return;

    if ((ff == EPX_FILL_STYLE_NONE) && (bw == 0)) {
	// outline only (in border style)
	if (no_paint(bf))
	    return;
	bw = 1;
    }
    // radii are clamped to half the size inside
    epx_draw_rrect_ring(pixmap, r.xy.x, r.xy.y,
			epx_rect_right(&r), epx_rect_bottom(&r),
			rw, rh, bw, bf, gc->border_color,
			ff, gc->fill_color);
}

void epx_pixmap_draw_triangle(epx_pixmap_t* pixmap, epx_gc_t* gc,
			      int x0, int y0,
			      int x1, int y1,
			      int x2, int y2)
{
    if (gc == NULL) gc = &epx_default_gc;
    
    if (gc->fill_style != EPX_FILL_STYLE_NONE)
	epx_fill_triangle(pixmap, x0,y0,x1,y1,x2,y2, gc->fill_style,
			 gc->fill_color);
    else if (!no_paint(gc->line_style)) {
	epx_draw_line(pixmap, x0,y0,x1,y1, gc->line_width,
		      gc->line_style, gc->foreground_color);
	epx_draw_line(pixmap, x1,y1,x2,y2, gc->line_width,
		      gc->line_style, gc->foreground_color);
	epx_draw_line(pixmap, x2,y2,x0,y0, gc->line_width,
		      gc->line_style, gc->foreground_color);
    }
}

static inline int min3(int a, int b, int c)
{
    int t = (a<b) ? a : b;
    return (t<c) ? t : c;
}

static inline int minv(int* vec, size_t n)
{
    if (n>0) {
	int* end = vec+n;
	int m = *vec++;
	while(vec < end) {
	    int e = *vec++;
	    m = (e < m) ? e : m;
	}
	return m;
    }
    return 0;
}

static inline int max3(int a, int b, int c)
{
    int t = (a>b) ? a : b;
    return (t>c) ? t : c;
}

static inline int maxv(int* vec, size_t n)
{
    if (n>0) {
	int* end = vec+n;
	int m = *vec++;
	while(vec < end) {
	    int e = *vec++;
	    m = (e > m) ? e : m;
	}
	return m;
    }
    return 0;    
}

static inline int cross(int x0,int y0, int x1, int y1)
{
    return x0*y1 - y0*x1;
}



static void draw_hline(epx_pixmap_t* pixmap, int x1, int x2, int y,
		       epx_flags_t flags, epx_pixel_t fg)
{
    uint8_t* ptr;

    ptr = EPX_PIXEL_ADDR(pixmap,x1,y);
    if (((flags&EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	epx_fill_row(ptr,pixmap->pixel_format,fg,(x2-x1)+1);
    else if (fg.a != EPX_ALPHA_TRANSPARENT)
	epx_fill_row_blend(ptr,pixmap->pixel_format,fg,(x2-x1)+1);
}

static void blend_pixel(uint8_t* addr,
			epx_pixel_unpack_t unpack,
			epx_pixel_pack_t pack,
			epx_pixel_t s)
{
    epx_pixel_t d = unpack(addr);
    d = epx_pixel_blend(s.a, s, d);
    pack(d, addr);
}

static void plot_pixel(uint8_t* addr,
		       epx_pixel_unpack_t unpack,
		       epx_pixel_pack_t pack,
		       epx_pixel_t s)
{
    (void) unpack;
    pack(s, addr);
}

static void plot(epx_pixmap_t* px, int x, int y,
		 epx_flags_t flags, epx_pixel_t fg)
{
    uint8_t* ptr = EPX_PIXEL_ADDR(px,x,y);
    if (((flags & EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	plot_pixel(ptr, px->func.unpack, px->func.pack, fg);
    else if (fg.a != EPX_ALPHA_TRANSPARENT)
	blend_pixel(ptr, px->func.unpack, px->func.pack, fg);
}

// k >= 0
#define PTINTRI_1(s,t,k) (((s)>=0) && ((t)>=0) && ((s)+(t)<=(k)))
// k < 0
#define PTINTRI_2(s,t,k) (((s)<0) && ((t)<0) && ((s)+(t)>(k)))

static void draw_bary_triangle_0(epx_pixmap_t* px,
				 epx_gc_t* gc,
				 int x0, int y0,
				 int x1, int y1,
				 int x2, int y2)
{
    int xl = min3(x0,x1,x2);
    int xr = max3(x0,x1,x2);
    int yu = min3(y0,y1,y2);
    int yd = max3(y0,y1,y2);
    int v1x = x1-x0;
    int v1y = y1-y0;
    int v2x = x2-x0;
    int v2y = y2-y0;    
    int qx, qy;
    int k = cross(v1x, v1y, v2x, v2y);    
    int y;
    int s, t;
    int c;
    epx_flags_t flags = gc->fill_style;
    epx_pixel_t color = gc->fill_color;

    // clip triangle
    if (xl < (c=epx_rect_left(&px->clip))) xl = c;
    if (xr > (c=epx_rect_right(&px->clip))) xr = c;
    if (yu < (c=epx_rect_top(&px->clip))) yu = c;
    if (yd > (c=epx_rect_bottom(&px->clip))) yd = c;
    
    qx = xl - x0;
    qy = yu - y0;

    s = cross(qx,qy, v2x,v2y);
    t = cross(v1x,v1y, qx,qy);    

    for (y = yu; y <= yd; y++) {
	int sx = s;
	int tx = t;
	int x = xl;
	int xa, xb;
	
	if (k >= 0) {
	    while((x <= xr) && !PTINTRI_1(sx,tx,k)) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xa = x;
	    while((x <= xr) && PTINTRI_1(sx,tx,k)) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xb = (x <= xr) ? x-1 : xr;
	    draw_hline(px, xa, xb, y, flags, color);
	}
	else {
	    while((x <= xr) && !PTINTRI_2(sx,tx,k)) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xa = x;
	    while((x <= xr) && PTINTRI_2(sx,tx,k)) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xb = (x <= xr) ? x-1 : xr;
	    draw_hline(px, xa, xb, y, flags, color);
	}
	s = s - v2x;
	t = t + v1x;
    }
}

// draw triangles P0,P1,P2  P3,P4,P5  .... n = number of points
// n=3*t (must be multiple of 3)
//
static void draw_bary_triangles_0(epx_pixmap_t* px, epx_gc_t* gc,
				  int* x, int* y, size_t n)
{
    int xl = minv(x, n);
    int xr = maxv(x, n);
    int yu = minv(y, n);
    int yd = maxv(y, n);
    size_t nt = n / 3;     // number of triangles
    int v1x[nt];
    int v1y[nt];
    int v2x[nt];
    int v2y[nt];
    int k[nt];
    int s[nt];
    int t[nt];
    int ypos;
    int c;
    int i, j;
    epx_flags_t flags = gc->fill_style;
    epx_pixel_t color = gc->fill_color;

    // clip triangle
    if (xl < (c=epx_rect_left(&px->clip))) xl = c;
    if (xr > (c=epx_rect_right(&px->clip))) xr = c;
    if (yu < (c=epx_rect_top(&px->clip))) yu = c;
    if (yd > (c=epx_rect_bottom(&px->clip))) yd = c;

    for (j=0, i=0; j<(int)nt; j++, i += 3) {
	int qx, qy;
	
	v1x[j] = x[i+1] - x[i];
	v1y[j] = y[i+1] - y[i];
	v2x[j] = x[i+2] - x[i];
	v2y[j] = y[i+2] - y[i];

	k[j] = cross(v1x[j], v1y[j], v2x[j], v2y[j]);
	
	qx = xl - x[i];
	qy = yu - y[i];

	s[j] = cross(qx,qy, v2x[j], v2y[j]);
	t[j] = cross(v1x[j], v1y[j], qx, qy);
    }
    
    for (ypos = yu; ypos <= yd; ypos++) {
	int sx[nt];
	int tx[nt];
	int xpos = xl;	

	for (j = 0; j <(int)nt; j++) {
	    sx[j] = s[j];
	    tx[j] = t[j];
	}

	while(xpos < xr) {
	    int in = 0;

	    j = 0;
	    while((j < (int)nt) && !in) {
		if (k[j] >= 0) {
		    in = PTINTRI_1(sx[j],tx[j],k[j]);
		}
		else {
		    in = PTINTRI_2(sx[j],tx[j],k[j]);
		}
		sx[j] += v2y[j];
		tx[j] -= v1y[j];		
		j++;
	    }
	    if (in)
		plot(px, xpos, ypos, flags, color);
	    while(j < (int)nt) {
		sx[j] += v2y[j];
		tx[j] -= v1y[j];
		j++;
	    }	    
	    xpos++;
	}
	for (j = 0; j < (int)nt; j++) {
	    s[j] -= v2x[j];
	    t[j] += v1x[j];
	}
    }
}

void draw_bary_triangles(epx_pixmap_t* px, epx_gc_t* gc,
			 int* x, int* y, size_t n)
{
    if (n == 3) {
	draw_bary_triangle_0(px, gc, x[0], y[0], x[1], y[1], x[2], y[2]);
    }
    else {
	draw_bary_triangles_0(px, gc, x, y, n);
    }
}

//
// fan then x[0],y[0] is center point with:
// x[1],y[1] and x[2],y[2]  is first triangle
// x[2],y[2] and x[3],y[3]  is next triangle
// ...
// x[n-2],y[n-2] and x[n-1],y[n-1] is last triangle
// closed
// x[n-1],y[n-1] and x[1],y[1] is the last triangle
//
// 
void draw_bary_fan_0(epx_pixmap_t* px, epx_gc_t* gc,
		     int* x, int* y, size_t n, int closed)
{
    int xl = minv(x, n);
    int xr = maxv(x, n);
    int yu = minv(y, n);
    int yd = maxv(y, n);
    size_t nt = closed ? n-1 : n-2; // number of triangles
    int vx[n];
    int vy[n];
    int s[nt];
    int t[nt];
    int k[nt];
    int c;
    int i;
    int qx, qy;
    int ypos;
    epx_flags_t flags = gc->fill_style;
    epx_pixel_t color = gc->fill_color;

    // clip triangle
    if (xl < (c=epx_rect_left(&px->clip)))   xl = c;
    if (xr > (c=epx_rect_right(&px->clip)))  xr = c;
    if (yu < (c=epx_rect_top(&px->clip)))    yu = c;
    if (yd > (c=epx_rect_bottom(&px->clip))) yd = c;
    
    // V(0) .. V(n-2),  V(n-1) = V(0)
    for (i = 0; i <(int)n-1; i++) {
	vx[i] = x[i+1] - x[0];
	vy[i] = y[i+1] - y[0];
    }
    if (closed) {
	vx[n-1] = vx[0];
	vy[n-1] = vy[0];
    }
    // K(0) = V(0) x V(1), K(1) = V(1) x V(2), ... K(n-3) = V(n-3) x V(n-4)
    for (i = 0; i < (int)nt;  i++) {
	k[i] = cross(vx[i], vy[i], vx[i+1], vy[i+1]);

	// i hope compile move qx and qy out of the loop
	qx = xl - x[0];
	qy = yu - y[0];

	s[i] = cross(qx, qy, vx[i+1], vy[i+1]);
	t[i] = cross(vx[i], vy[i], qx, qy);
    }

    for (ypos = yu; ypos <= yd; ypos++) {
	int sx[nt];
	int tx[nt];
	int xpos = xl;
	int j;
	
	for (j = 0; j <(int)nt; j++) {
	    sx[j] = s[j];
	    tx[j] = t[j];
	}

	while(xpos < xr) {
	    int in = 0;

	    j = 0;
	    while((j < (int)nt) && !in) {
		if (k[j] >= 0) {
		    in = PTINTRI_1(sx[j],tx[j],k[j]);
		}
		else {
		    in = PTINTRI_2(sx[j],tx[j],k[j]);
		}
		sx[j] += vy[j+1];
		tx[j] -= vy[j];
		j++;
	    }
	    if (in)
		plot(px, xpos, ypos, flags, color);
	    while(j < (int)nt) {
		sx[j] += vy[j+1];
		tx[j] -= vy[j];
		j++;
	    }
	    xpos++;
	}
	for (j = 0; j < (int)nt; j++) {
	    s[j] -= vx[j+1];
	    t[j] += vx[j];
	}
    }
}

void draw_bary_fan(epx_pixmap_t* px, epx_gc_t* gc,
		   int* x, int* y, size_t n, int closed)

{
    if (n == 3) {
	draw_bary_triangle_0(px, gc, x[0], y[0], x[1], y[1], x[2], y[2]);
    }
    else {
	draw_bary_fan_0(px, gc, x, y, n, closed);
    }
}


//
// strip
//  P0       P2      P4     ...   P(2i)
//  |     /  |     / |     /      |
//  |    /   |    /  |    /       |
//  |   /    |   /   |   /        |
//  |  /     |  /    |  /         |
//  v /      v /     v /          v
//  P1       P3       P5      ...  P(2i+1)
//
//
//  V(0)   = P(0) - P(1)  = -(P(1) - P(0))
//  V(1)   = P(2) - P(1)  =
//  ...
//  V(i)   = P(i+1) - P(i)
//
//  K(0) = -V(0) x V(1)      P0,P1,P2
//  K(1) = -V(2) x V(3)      P1,P2,P3
//  K(2) = -V(4) x V(5)      P2,P3,P4
// 
void draw_bary_strip_0(epx_pixmap_t* px, epx_gc_t* gc,
		       int* x, int* y, size_t n)
{
    int xl = minv(x, n);
    int xr = maxv(x, n);
    int yu = minv(y, n);
    int yd = maxv(y, n);
    size_t nt = n-2;
    int vx[n];
    int vy[n];
    int s[nt];
    int t[nt];
    int k[nt];
    int c;
    int i;
    int ypos;
    epx_flags_t flags = gc->fill_style;
    epx_pixel_t color = gc->fill_color;

    // clip triangle
    if (xl < (c=epx_rect_left(&px->clip)))   xl = c;
    if (xr > (c=epx_rect_right(&px->clip)))  xr = c;
    if (yu < (c=epx_rect_top(&px->clip)))    yu = c;
    if (yd > (c=epx_rect_bottom(&px->clip))) yd = c;

    for (i = 0; i < (int)n-1; i++) {
	vx[i] = x[i+1] - x[i];
	vy[i] = y[i+1] - y[i];
    }

    for (i = 0; i < (int)nt;  i++) {
	int qx, qy;
	    
	k[i] = cross(-vx[i], -vy[i], vx[i+1], vy[i+1]);

	qx = xl - x[i+1];
	qy = yu - y[i+1];
    
	s[i] = cross(qx, qy, vx[i+1], vy[i+1]);
	t[i] = cross(-vx[i], -vy[i], qx, qy);
    }

    for (ypos = yu; ypos <= yd; ypos++) {
	int sx[nt];
	int tx[nt];
	int xpos = xl;
	int j;
	
	for (j = 0; j <(int)nt; j++) {
	    sx[j] = s[j];
	    tx[j] = t[j];
	}

	while(xpos < xr) {
	    int in = 0;

	    j = 0;
	    while((j < (int)nt) && !in) {
		if (k[j] >= 0) {
		    in = PTINTRI_1(sx[j],tx[j],k[j]);
		}
		else {
		    in = PTINTRI_2(sx[j],tx[j],k[j]);
		}
		sx[j] += vy[j+1];
		tx[j] -= -vy[j];
		j++;
	    }
	    if (in)
		plot(px, xpos, ypos, flags, color);
	    while(j < (int)nt) {
		sx[j] += vy[j+1];
		tx[j] -= -vy[j];
		j++;
	    }
	    xpos++;
	}
	for (j = 0; j < (int)nt; j++) {
	    s[j] -= vx[j+1];
	    t[j] += -vx[j];
	}
    }    
    
}

void draw_bary_strip(epx_pixmap_t* px, epx_gc_t* gc,
		     int* x, int* y, size_t n)

{
    if (n == 3) {
	draw_bary_triangle_0(px, gc, x[0], y[0], x[1], y[1], x[2], y[2]);
    }
    else {
	draw_bary_strip_0(px, gc, x, y, n);
    }
}

