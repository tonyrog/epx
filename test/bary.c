//
//  Draw one or several triangles using barycentic method
//

#include <unistd.h>
#include <stdarg.h>
#include <sys/time.h>
#include <math.h>

#include "../include/epx.h"

#define WINDOW_WIDTH  640
#define WINDOW_HEIGHT 480

#define DRAW_TRIANGLE(px,x0,y0,x1,y1,x2,y2,flags,c0,c1,c2)	\
    draw_triangle_1((px),(x0),(y0),(x1),(y1),(x2),(y2),(flags),(c0))

#define DRAW_QUAD(px,x0,y0,x1,y1,x2,y2,x3,y3,flags,c0,c1,c2)		\
    draw_quad_1((px),(x0),(y0),(x1),(y1),(x2),(y2),(x3),(y3),(flags),(c0))

//#define DRAW_TRIANGLE(px,x0,y0,x1,y1,x2,y2,flags,c0,c1,c2)	\
    draw_triangle_f((px),(x0),(y0),(x1),(y1),(x2),(y2),\
		    plot_color,(flags),(c0),(c1),(c2))

typedef struct timeval timestamp_t;

void get_timestamp(timestamp_t* stamp)
{
    gettimeofday(stamp, NULL);
}

double diff_timestamp(timestamp_t* stamp1, timestamp_t* stamp0)
{
    timestamp_t stamp;
    timersub(stamp1, stamp0, &stamp);
    return stamp.tv_sec+(stamp.tv_usec/1000000.0);
}


static inline int min3(int a, int b, int c)
{
    int t = (a<b) ? a : b;
    return (t<c) ? t : c;
}

static inline int min4(int a, int b, int c, int d)
{
    int t1 = (a < b) ? a : b;
    int t2 = (c < d) ? c : d;
    return (t1 < t2) ? t1 : t2;
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

static inline int max4(int a, int b, int c, int d)
{
    int t1 = (a > b) ? a : b;
    int t2 = (c > d) ? c : d;
    return (t1 > t2) ? t1 : t2;
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

static int area(int x0, int y0,
		int x1, int y1,
		int x2, int y2)
{
    int v1x = x1-x0;
    int v1y = y1-y0;
    int v2x = x2-x0;
    int v2y = y2-y0;
    int k = cross(v1x,v1y, v2x, v2y);
    return abs(k)/2;
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

void draw_triangle_0(epx_pixmap_t* px,
		     int x0, int y0,
		     int x1, int y1,
		     int x2, int y2,
		     epx_flags_t flags, epx_pixel_t fg)
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
    int x, y;
    int s, t;
    int c;
    epx_pixel_unpack_t unpack = px->func.unpack;
    epx_pixel_pack_t   pack   = px->func.pack;
    unsigned int bytes_per_pixel  = px->bytes_per_pixel;	
    void (*plotf)(uint8_t* addr,
		  epx_pixel_unpack_t unpack,
		  epx_pixel_pack_t pack,
		  epx_pixel_t s);

    if (((flags & EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	plotf = plot_pixel;
    else if (fg.a != EPX_ALPHA_TRANSPARENT)
	plotf = blend_pixel;
    else
	return;

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
	uint8_t* ptr;

	if (k > 0) {
	    while((x <= xr) && !((sx>=0) && (tx>=0) && (sx+tx<=k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    ptr = EPX_PIXEL_ADDR(px,x,y);
	    while((x <= xr) && ((sx>=0) && (tx>=0) && (sx+tx<=k))) {
		plotf(ptr,unpack,pack,fg);
		ptr += bytes_per_pixel;
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	}
	else {
	    while((x <= xr) && !((sx<0) && (tx<0) && (sx+tx>k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    ptr = EPX_PIXEL_ADDR(px,x,y);
	    while((x <= xr) && ((sx<0) && (tx<0) && (sx+tx>k))) {
		plotf(ptr,unpack,pack,fg);
		ptr += bytes_per_pixel;	
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	}
	s = s - v2x;
	t = t + v1x;
    }
}

// draw triangles P0,P1,P2  P3,P4,P5  .... n = number of points
// n=3*t (must be multiple of 3)
//
void draw_triangles_0(epx_pixmap_t* px,
		      int* x, int* y, size_t n,
		      epx_flags_t flags, epx_pixel_t fg)
{
    int xl = minv(x, n);
    int xr = maxv(x, n);
    int yu = minv(y, n);
    int yd = maxv(y, n);
    size_t nt = n / 3;
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
    epx_pixel_unpack_t unpack = px->func.unpack;
    epx_pixel_pack_t   pack   = px->func.pack;
    unsigned int bytes_per_pixel  = px->bytes_per_pixel;	
    void (*plotf)(uint8_t* addr,
		  epx_pixel_unpack_t unpack,
		  epx_pixel_pack_t pack,
		  epx_pixel_t s);

    if (((flags & EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	plotf = plot_pixel;
    else if (fg.a != EPX_ALPHA_TRANSPARENT)
	plotf = blend_pixel;
    else
	return;
    // clip triangle
    if (xl < (c=epx_rect_left(&px->clip))) xl = c;
    if (xr > (c=epx_rect_right(&px->clip))) xr = c;
    if (yu < (c=epx_rect_top(&px->clip))) yu = c;
    if (yd > (c=epx_rect_bottom(&px->clip))) yd = c;

    for (j=0, i=0; j<nt; j++, i += 3) {
	int qx;
	int qy;
	
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
	int sx[n];
	int tx[n];
	int xpos = xl;	
	uint8_t* ptr;

	for (j = 0; j < nt; j++) {
	    sx[j] = s[j];
	    tx[j] = t[j];
	}

	while(xpos < xr) {
	    int in = 0;
	    
	    j = 0;
	    while((j < nt) && !in) {
		if (k[j] > 0) {
		    if ((sx[j]>=0) && (tx[j]>=0) && (sx[j]+tx[j]<=k[j]))
			in = 1;
		}
		else {
		    if ((sx[j]<0) && (tx[j]<0) && (sx[j]+tx[j]>k[j]))
			in = 1;
		}
		j++;
	    }
	    if (in)
		plot(px, xpos, ypos, flags, fg);
	    for (j = 0; j < nt; j++) {
		sx[j] += v2y[j];
		tx[j] -= v1y[j];
	    }
	    xpos++;
	}
	for (j = 1; j < nt; j++) {
	    s[j] -= v2x[j];
	    t[j] += v1x[j];
	}
    }
}

void plot_plain(epx_pixmap_t* px, int x, int y,
		int s, int t, int k,
		epx_flags_t flags,
		epx_pixel_t c0,
		epx_pixel_t c1,
		epx_pixel_t c2)
{
    uint8_t* ptr = EPX_PIXEL_ADDR(px,x,y);
    epx_pixel_unpack_t unpack = px->func.unpack;
    epx_pixel_pack_t   pack   = px->func.pack;
    unsigned int bytes_per_pixel  = px->bytes_per_pixel;

    if (((flags & EPX_FLAG_BLEND)==0) || (c0.a == EPX_ALPHA_OPAQUE))
	plot_pixel(ptr,px->func.unpack,px->func.pack,c0);
    else if (c0.a != EPX_ALPHA_TRANSPARENT)
	blend_pixel(ptr,px->func.unpack,px->func.pack,c0);
}

void plot_color(epx_pixmap_t* px, int x, int y,
		int s, int t, int k,
		epx_flags_t flags,
		epx_pixel_t c0,
		epx_pixel_t c1,
		epx_pixel_t c2)
{
    uint8_t* ptr = EPX_PIXEL_ADDR(px,x,y);
    epx_pixel_unpack_t unpack = px->func.unpack;
    epx_pixel_pack_t   pack   = px->func.pack;
    unsigned int bytes_per_pixel  = px->bytes_per_pixel;    
    float sf = s / (float) k;
    float tf = t / (float) k;
    float uf = 1-(sf+tf);
    epx_pixel_t p;

    p.r = uf*c0.r + sf*c1.r + tf*c2.r;
    p.g = uf*c0.g + sf*c1.g + tf*c2.g;
    p.b = uf*c0.b + sf*c1.b + tf*c2.b;
    p.a = uf*c0.a + sf*c1.a + tf*c2.a;

    if (((flags & EPX_FLAG_BLEND)==0) || (p.a == EPX_ALPHA_OPAQUE))
	plot_pixel(ptr,px->func.unpack,px->func.pack,p);
    else if (p.a != EPX_ALPHA_TRANSPARENT)
	blend_pixel(ptr,px->func.unpack,px->func.pack,p);    
}


void draw_triangle_f(epx_pixmap_t* px,
		     int x0, int y0,
		     int x1, int y1,
		     int x2, int y2,
		     void (*plotf)(epx_pixmap_t* px, int x, int y,
				   int s, int t, int k,
				   epx_flags_t flags,
				   epx_pixel_t c0,
				   epx_pixel_t c1,
				   epx_pixel_t c2),
		     epx_flags_t flags,
		     epx_pixel_t c0,
		     epx_pixel_t c1,
		     epx_pixel_t c2)     
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
    int x, y;
    int s, t;
    int c;

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

	if (k > 0) {
	    while((x <= xr) && !((sx>=0) && (tx>=0) && (sx+tx<=k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    while((x <= xr) && ((sx>=0) && (tx>=0) && (sx+tx<=k))) {
		(*plotf)(px,x,y,sx,tx,k,flags,c0,c1,c2);
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	}
	else {
	    while((x <= xr) && !((sx<0) && (tx<0) && (sx+tx>k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    while((x <= xr) && ((sx<0) && (tx<0) && (sx+tx>k))) {
		(*plotf)(px,x,y,sx,tx,k,flags,c0,c1,c2);		
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	}
	s = s - v2x;
	t = t + v1x;
    }
}



// TRIANGLE 1 - using draw_hline (already clipped !!!)
// Assume x1 < x2 and that x1,x2,y are clipped.
//
void draw_hline(epx_pixmap_t* pixmap, int x1, int x2, int y,
		epx_flags_t flags, epx_pixel_t fg)
{
    uint8_t* ptr;

    ptr = EPX_PIXEL_ADDR(pixmap,x1,y);
    if (((flags&EPX_FLAG_BLEND)==0) || (fg.a == EPX_ALPHA_OPAQUE))
	epx_fill_row(ptr,pixmap->pixel_format,fg,(x2-x1)+1);
    else if (fg.a != EPX_ALPHA_TRANSPARENT)
	epx_fill_row_blend(ptr,pixmap->pixel_format,fg,(x2-x1)+1);
}

void draw_triangle_1(epx_pixmap_t* px,
		     int x0, int y0,
		     int x1, int y1,
		     int x2, int y2,
		     epx_flags_t flags, epx_pixel_t fg)
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
    int x, y;
    int s, t;
    int c;

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
	
	if (k > 0) {
	    while((x <= xr) && !((sx>=0) && (tx>=0) && (sx+tx<=k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xa = x;
	    while((x <= xr) && ((sx>=0) && (tx>=0) && (sx+tx<=k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xb = (x <= xr) ? x-1 : xr;
	    draw_hline(px, xa, xb, y, flags, fg);
	}
	else {
	    while((x <= xr) && !((sx<0) && (tx<0) && (sx+tx>k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xa = x;
	    while((x <= xr) && ((sx<0) && (tx<0) && (sx+tx>k))) {
		sx += v2y;
		tx -= v1y;
		x++;
	    }
	    xb = (x <= xr) ? x-1 : xr;
	    draw_hline(px, xa, xb, y, flags, fg);
	}
	s = s - v2x;
	t = t + v1x;
    }
}

void draw_quad_1(epx_pixmap_t* px,
		 int x0, int y0,
		 int x1, int y1,
		 int x2, int y2,
		 int x3, int y3,
		 epx_flags_t flags, epx_pixel_t fg)
{
    int xl = min4(x0,x1,x2,x3);
    int xr = max4(x0,x1,x2,x3);
    int yu = min4(y0,y1,y2,y3);
    int yd = max4(y0,y1,y2,y3);
    int v1x = x1-x0;
    int v1y = y1-y0;
    int v2x = x2-x0;
    int v2y = y2-y0;
    int v3x = x3-x0;
    int v3y = y3-y0;
    int qx, qy;
    int k1 = cross(v1x, v1y, v2x, v2y);
    int k2 = cross(v2x, v2y, v3x, v3y);
    int x, y;
    int s1, t1;
    int s2, t2;
    int c;

    // clip triangle
    if (xl < (c=epx_rect_left(&px->clip))) xl = c;
    if (xr > (c=epx_rect_right(&px->clip))) xr = c;
    if (yu < (c=epx_rect_top(&px->clip))) yu = c;
    if (yd > (c=epx_rect_bottom(&px->clip))) yd = c;
    
    qx = xl - x0;
    qy = yu - y0;

    s1 = cross(qx,qy, v2x,v2y);
    t1 = cross(v1x,v1y, qx,qy);

    s2 = cross(qx,qy, v3x,v3y);
    t2 = cross(v2x,v2y, qx,qy);

    for (y = yu; y <= yd; y++) {
	int sx1 = s1;
	int tx1 = t1;
	int sx2 = s2;
	int tx2 = t2;
	int x = xl;
	int in1 = 0;
	int in2 = 0;
	
	while(x < xr) {
	    int in1 = 0;
	    int in2 = 0;
	    
	    if (k1 > 0) {
		if ((sx1>=0) && (tx1>=0) && (sx1+tx1<=k1))
		    in1 = 1;
	    }
	    else {
		if ((sx1<0) && (tx1<0) && (sx1+tx1>k1))
		    in1 = 1;
	    }
	    sx1 += v2y;
	    tx1 -= v1y;
		
	    if (k2 > 0) {
		if ((sx2>=0) && (tx2>=0) && (sx2+tx2<=k2))
		    in2 = 1;
	    }
	    else {
		if ((sx2<0) && (tx2<0) && (sx2+tx2>k2))
		    in2 = 1;
	    }
	    sx2 += v3y;
	    tx2 -= v2y;

	    if (in1 || in2)
		plot(px, x, y, flags, fg);
	    x++;
	}
	s1 -= v2x;
	t1 += v1x;

	s2 -= v3x;
	t2 += v2x;	
    }
}

void draw_poly_1(epx_pixmap_t* px, int* x, int* y, size_t n,
		 epx_flags_t flags, epx_pixel_t fg)
{
    int xl = minv(x, n);
    int xr = maxv(x, n);
    int yu = minv(y, n);
    int yd = maxv(y, n);
    int vx[n];
    int vy[n];
    int qx, qy;
    int xpos, ypos;
    int s[n];
    int t[n];
    int k[n];
    int c;
    int i;

    for (i = 1; i < n; i++) {
	vx[i] = x[i] - x[0];
	vy[i] = y[i] - y[0];
    }
    for (i = 1; i < n-1;  i++) {
	k[i] = cross(vx[i], vy[i], vx[i+1], vy[i+1]);
    }

    // clip triangle
    if (xl < (c=epx_rect_left(&px->clip))) xl = c;
    if (xr > (c=epx_rect_right(&px->clip))) xr = c;
    if (yu < (c=epx_rect_top(&px->clip))) yu = c;
    if (yd > (c=epx_rect_bottom(&px->clip))) yd = c;
    
    qx = xl - x[0];
    qy = yu - y[0];

    for (i = 1; i < n-1; i++) {
	s[i] = cross(qx, qy, vx[i+1], vy[i+1]);
	t[i] = cross(vx[i], vy[i], qx, qy);
    }

    for (ypos = yu; ypos <= yd; ypos++) {
	int sx[n];
	int tx[n];
	int xpos = xl;

	for (i = 0; i < n; i++) {
	    sx[i] = s[i];
	    tx[i] = t[i];
	}

	while(xpos < xr) {
	    int in = 0;
	    
	    i = 1;
	    while((i < (n-1)) && !in) {
		if (k[i] > 0) {
		    if ((sx[i]>=0) && (tx[i]>=0) && (sx[i]+tx[i]<=k[i]))
			in = 1;
		}
		else {
		    if ((sx[i]<0) && (tx[i]<0) && (sx[i]+tx[i]>k[i]))
			in = 1;
		}
		i++;
	    }

	    for (i = 1; i < n-1; i++) {
		sx[i] += vy[i+1];
		tx[i] -= vy[i];
	    }
	    if (in)
		plot(px, xpos, ypos, flags, fg);
	    xpos++;
	}
	for (i = 1; i < n-1; i++) {
	    s[i] -= vx[i+1];
	    t[i] += vx[i];
	}
    }
}


void update_window(epx_window_t* wn, epx_pixmap_t* px)
{
    int width = px->width;
    int height = px->height;
    epx_backend_t* be = px->backend;
    epx_window_draw_begin(wn);
    epx_backend_pixmap_draw(be, px, wn, 0, 0, 0, 0, width, height);
    epx_window_draw_end(wn, 0);
    epx_window_swap(wn);
}

void bounce(epx_window_t* wn, epx_pixmap_t* px, epx_gc_t* gc, size_t n)
{
    float coord[n*2];
    float delta[n*2];
    int   limit[n*2];
    int   x[n];
    int   y[n];
    int   width = px->width;
    int   height = px->height;
    int i;

    for (i = 0; i < n; i++) {
	limit[2*i]   = width;
	limit[2*i+1] = height;
    }
    for (i = 0; i < n*2; i++) {
	coord[i] = (rand() % limit[i]);
	delta[i] = ((rand() % 2001) / 1000.0) - 1.0;
    }
    while(1) {
	for (i = 0; i < n*2; i++) {
	    float ncoord = coord[i] + delta[i];
	    if ((ncoord < 0) || (ncoord >= limit[i]))
		delta[i] = -delta[i];
	    coord[i] = ncoord;
	}
	for (i = 0; i < n; i++) {
	    x[i] = roundf(coord[2*i]);
	    y[i] = roundf(coord[2*i+1]);
	}
	epx_pixmap_fill(px, epx_pixel_black);
	switch(n) {
	case 3:
	    DRAW_TRIANGLE(px, x[0],y[0], x[1],y[1], x[2],y[2],
			  EPX_FLAG_SOLID,
			  epx_pixel_cyan,
			  epx_pixel_red,
			  epx_pixel_blueViolet);
	    break;
	case 4:
	    DRAW_QUAD(px, x[0],y[0], x[1],y[1], x[2],y[2], x[3],y[3],
		      EPX_FLAG_SOLID,
		      epx_pixel_cyan,
		      epx_pixel_red,
		      epx_pixel_blueViolet);
	    break;
	default:
	    draw_triangles_0(px, x, y, n, EPX_FLAG_BLEND, epx_pixel_cyan);
	    break;
	}
	
	update_window(wn, px);
    }
}

void bench_tri(int n, float ratio, epx_pixmap_t* pixmap, epx_gc_t* gc)
{
    int i;
    int m;

    // size 1..50
    m = sqrt(pixmap->width*pixmap->height*ratio);

    for (i = 0; i < n; i++) {
	DRAW_TRIANGLE(pixmap,
		      10,  10,
		      10, m+10,
		      m+10, 10,
		      EPX_FLAG_SOLID,
		      epx_pixel_blue,
		      epx_pixel_red,
		      epx_pixel_green);
    }
}


int main(int argc, char** argv)
{
    char* accel_name = NULL;
    int do_bounce = 0;
    int accel;
    int c;
    timestamp_t t0,t1;
    epx_backend_t* be;
    epx_window_t* wn;
    epx_pixmap_t* px;    
    epx_gc_t* gc;

    while((c = getopt(argc, argv, "BA:")) != -1) {
	switch(c) {
	case 'B':
	    do_bounce = 1;
	    break;
	case 'A':
	    accel_name = optarg;
	    break;
	default:
	    break;
	}
    }

    srand(123456);

    if (accel_name == NULL)
	accel = EPX_SIMD_AUTO;  // select automatically
    else if (strcmp(accel_name, "auto")==0)
	accel = EPX_SIMD_AUTO;  // select automatically
    else if (strcmp(accel_name, "emu")==0)
	accel = EPX_SIMD_EMU;
    else if (strcmp(accel_name, "mmx")==0)
	accel = EPX_SIMD_MMX;
    else if (strcmp(accel_name, "sse2")==0)
	accel = EPX_SIMD_SSE2;
    else if (strcmp(accel_name, "avx2")==0)
	accel = EPX_SIMD_AVX2;
    else if (strcmp(accel_name, "altivec")==0)
	accel = EPX_SIMD_ALTIVEC;
    else if (strcmp(accel_name, "neon")==0)
	accel = EPX_SIMD_NEON;
    else
	accel = EPX_SIMD_AUTO;  // select automatically	

    if (do_bounce) {
	if ((be = epx_backend_create(getenv("EPX_BACKEND"), NULL)) == NULL) {
	    fprintf(stderr, "bary: no epx backend found\n");
	    exit(1);
	}
	epx_init(accel);
	
	wn = epx_window_create(50, 50, WINDOW_WIDTH, WINDOW_HEIGHT);
	epx_backend_window_attach(be, wn);
	px = epx_pixmap_create(WINDOW_WIDTH, WINDOW_HEIGHT, EPX_FORMAT_ARGB);
	epx_backend_pixmap_attach(be, px);
	epx_pixmap_fill(px, epx_pixel_black);
	gc = epx_gc_copy(&epx_default_gc);
	update_window(wn, px);

	sleep(2);

	bounce(wn, px, gc, 12);
    }
    else {
	int n = 1000;
	float ratio = 0.25;  // % of pixels in triangle
	
	epx_init(accel);
	
	epx_gc_set_fill_color(gc, epx_pixel_darkRed);
	px = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);
	gc = epx_gc_create();
    
	get_timestamp(&t0);
	bench_tri(n, ratio, px, gc);  // 50% of pixels filled
	get_timestamp(&t1);
	printf("%.2f tri/sec @ %.2f%%\n",
	       n/diff_timestamp(&t1,&t0),
	       ratio*100);
    }
    exit(0);
}
