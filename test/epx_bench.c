/*
 *
 * Bench marking/test
 *
 */

#include <unistd.h>
#include <stdarg.h>
#include <sys/time.h>
#include <math.h>

#include "../include/epx.h"

/* Time the copy area function */
void bench_copy(int n)
{
    epx_pixmap_t* a;
    epx_pixmap_t* b;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark blending function */

    a = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);
    b = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    epx_pixmap_copy_area(a, b, 0, 0, 0, 0, 640, 480, 0);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("COPY Avg: %f/s\n", tfsum/n);
}

static epx_pixel_t unpack_ARGB(u_int8_t* ptr)
{
    epx_pixel_t p;
    p.px = *((unsigned long*) ptr);
    return p;
}

static void pack_ARGB(epx_pixel_t p,unsigned char* ptr)
{
    *((unsigned long*) ptr) = p.px;
}


void bench_plot1(int n,
		 epx_pixel_t (*src_unpack)(u_int8_t*),
		 void (*dst_pack)(epx_pixel_t, u_int8_t*))
{
    epx_pixmap_t* a;
    epx_gc_t* gc;
    int j;
    double tfsum = 0.0;
    u_int8_t* src_ptr;
    unsigned int bytesPerRow;
    unsigned int bytesPerPixel;
    int x, y;

    /* Bench mark fill function */
    gc = epx_gc_create();
    a = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);
    
    epx_gc_set_fill_style(gc, EPX_FILL_STYLE_SOLID);
    epx_gc_set_border_width(gc, 0);
    epx_gc_set_fill_color(gc, epx_pixel_red);

    bytesPerRow = a->bytes_per_row;
    bytesPerPixel = a->bytes_per_pixel;

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	int i;

	gettimeofday(&t0, NULL);
	
	for (i = 0; i < 100; i++) {
	    src_ptr = EPX_PIXEL_ADDR(a, 0, 0);
	    for (y = 0; y < 480; y++) {
		u_int8_t* src1 = src_ptr;
		for (x = 0; x < 640; x++) {
		    epx_pixel_t p;
		    p = src_unpack(src1);
		    p.a = 100; p.r = 1; p.g = 1; p.b = 1;
		    dst_pack(p, src1);
		    src1 += bytesPerPixel;
		}
		src_ptr += bytesPerRow;
	    }
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("PLOT1 Avg: %f/s\n", tfsum/n);
}

/* Time the fill area function */
void bench_plot(int n, int src_pt, int dst_pt)
{
    epx_pixmap_t* a;
    epx_gc_t* gc;
    int j;
    double tfsum = 0.0;
    u_int8_t* src_ptr;
    unsigned int bytesPerRow;
    unsigned int bytesPerPixel;
    int x, y;

    /* Bench mark fill function */
    gc = epx_gc_create();
    a = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);

    epx_gc_set_fill_style(gc, EPX_FILL_STYLE_SOLID);
    epx_gc_set_border_width(gc, 0);
    epx_gc_set_fill_color(gc, epx_pixel_red);

    bytesPerRow = a->bytes_per_row;
    bytesPerPixel = a->bytes_per_pixel;
    
    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	int i;

	gettimeofday(&t0, NULL);

	for (i = 0; i < 100; i++) {
	    src_ptr = EPX_PIXEL_ADDR(a, 0, 0);
	    for (y = 0; y < 480; y++) {
		u_int8_t* src1 = src_ptr;
		for (x = 0; x < 640; x++) {
		    epx_pixel_t p = a->func.unpack(src1);
		    p.a = 100; p.r = 1; p.g = 1; p.b = 1;
		    a->func.pack(p, src1);
		    src1 += bytesPerPixel;
		}
		src_ptr += bytesPerRow;
	    }
	}

	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("PLOT Avg: %f/s\n", tfsum/n);
}

/* Time the fill area function */
void bench_fill(int n)
{
    epx_pixmap_t* a;
    epx_gc_t* gc;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark fill function */
    gc = epx_gc_create();
    a = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);

    epx_gc_set_fill_style(gc, EPX_FILL_STYLE_SOLID);
    epx_gc_set_border_width(gc, 0);
    epx_gc_set_fill_color(gc, epx_pixel_red);

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    epx_pixmap_draw_rectangle(a, gc, 0, 0, 640, 480);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("FILL Avg: %f/s\n", tfsum/n);
}

/* Time the blend area function */
void bench_blend(int n)
{
    epx_pixmap_t* a;
    epx_pixmap_t* b;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark blending function */
    a = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);
    b = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    epx_pixmap_copy_area(a, b, 0, 0, 0, 0, 640, 480, EPX_FLAG_BLEND);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	// printf("BLEND: %f/s\n", tf);
	tfsum += tf;
    }
    printf("BLEND Avg: %f/s\n", tfsum/n);
}

/* Time the blend fill area function */
void bench_blend_fill(int n)
{
    epx_pixmap_t* a;
    epx_gc_t* gc;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark blending function */
    gc = epx_gc_create();
    a = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);

    epx_gc_set_fill_style(gc, EPX_FILL_STYLE_BLEND);
    epx_gc_set_border_width(gc, 0);
    epx_gc_set_fill_color(gc, epx_pixel_argb(127,100,200,150));

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    epx_pixmap_draw_rectangle(a, gc, 0, 0, 640, 480);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("BFILL: Avg: %f/s\n", tfsum/n);
}

void bench_line(int n)
{
    epx_pixmap_t* a;
    epx_gc_t* gc;
    int x0 = 0, x1 = 639;
    int y0, y1;
    int i, j;
    double tfsum = 0.0;

    gc = epx_gc_create();
    a  = epx_pixmap_create(640, 480, EPX_FORMAT_ARGB);
    
    epx_gc_set_line_style(gc, EPX_FILL_STYLE_BLEND);
    epx_gc_set_foreground_color(gc, epx_pixel_argb(127,100,200,150));

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;

	gettimeofday(&t0, NULL);

	for (i = 0; i < 100; i++) {
	    for (y0 = 0; y0 < 480; y0++) {
		y1 = 439-y0;
		epx_pixmap_draw_line(a, gc, x0, y0, x1, y1);
	    }
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("LINE Avg: %f/s\n", tfsum/n);
}


int main(int argc, char** argv)
{
    char* accel_name = NULL;
    int accel;
    int c;

    while((c = getopt(argc, argv, "A:")) != -1) {
	switch(c) {
	case 'A':
	    accel_name = optarg;
	    break;
	default:
	    break;
	}
    }

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
    else if (strcmp(accel_name, "altivec")==0)
	accel = EPX_SIMD_ALTIVEC;
    else if (strcmp(accel_name, "neon")==0)
	accel = EPX_SIMD_NEON;
    else
	accel = EPX_SIMD_AUTO;  // select automatically	

    epx_init(accel);

    bench_copy(10);
    bench_plot(10, EPX_FORMAT_ARGB, EPX_FORMAT_BGRA);
    bench_plot1(10, unpack_ARGB, pack_ARGB);
    bench_fill(10);
    bench_blend(10);
    bench_blend_fill(10);
    bench_line(10);
    exit(0);
}
