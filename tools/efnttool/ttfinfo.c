
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <sys/time.h>

#include "../../c_src/schrift.h"
#include "../../include/epx_t2d.h"

#define BENCH_SIZE 50000
#define DEFAULT_SCALE 100
#define DEFAULT_GLYPH 'G'

// Benchruns: ttinfo times.ttf
// 2022-12-21: 61515
// 2022-12-22: 61075
//
uint64_t time_tick(void)
{
    struct timeval t;
    gettimeofday(&t, 0);
    return t.tv_sec*(uint64_t)1000000 + t.tv_usec;
}

static void print_name(const char* ptr, size_t len)
{
    if (ptr == NULL)
	printf("null");
    else {
	while(len--) {
	    int c = *ptr++;
	    if (isprint(c))
		putchar(c);
	    else
		printf("x%02x", c);
	}
    }
}

int main(int argc, char** argv)
{
    SFT_Font* font;
    SFT_LMetrics lm;
    SFT_Glyph glyph;
    char* font_family_name;
    uint16_t font_family_name_len;
    char* font_subfamily_name;
    uint16_t font_subfamily_name_len;
    char* font_full_font_name;
    uint16_t font_full_font_name_len;
    int gid = DEFAULT_GLYPH;
    SFT sft = {
	.xScale = DEFAULT_SCALE,
	.yScale = DEFAULT_SCALE,
	.flags  = SFT_DOWNWARD_Y,
    };
    uint64_t t0, t1;    
    

    if (argc < 2) {
	fprintf(stderr, "usage: ttinfo <file>.ttf [<char> | [<size>]]\n");
	exit(1);
    }

    if (argc > 2) {
	double scale;
	char* endptr = NULL;

	gid = *argv[2];	
	
	if (argc > 3) {
	    scale = strtod(argv[3], &endptr);
	    if ((endptr == NULL) || (*endptr != '\0')) {
	    	fprintf(stderr, "usage: ttinfo <file>.ttf [<char> | [<size>]]\n");
		exit(1);
	    }
	    sft.xScale = scale;
	    sft.yScale = scale;
	}
    }
    
    if ((font = sft_loadfile(argv[1])) == NULL) {
	fprintf(stderr, "unable to load ttf file %s : %s\n", argv[1],
		strerror(errno));
	exit(1);
    }

    
    // sft_print_table_names(stdout, font);

    font_family_name = sft_name(font, 1, &font_family_name_len);
    font_subfamily_name = sft_name(font, 2, &font_subfamily_name_len);
    font_full_font_name = sft_name(font, 4, &font_full_font_name_len);

    printf("Font Family: ");
    print_name(font_family_name, font_family_name_len);
    printf("\n");

    printf("Font SubFamily: ");    
    print_name(font_subfamily_name, font_subfamily_name_len);
    printf("\n");

    printf("Full Font name: ");    
    print_name(font_full_font_name, font_full_font_name_len);
    printf("\n");    
    

    // sft_print_name_table(stdout, font);
    
    sft.font = font;
    if (sft_lmetrics(&sft, &lm) < 0)
	fprintf(stderr, "no metrics\n");
    else {
	printf("ascender: %f\n", lm.ascender);
	printf("descender: %f\n", lm.descender);
	printf("lineGap: %f\n", lm.lineGap);
    }

    if (sft_lookup(&sft, gid, &glyph) < 0) {
	fprintf(stderr, "glyph '%c' not found\n", gid);
	goto error;
    }
    else {
	SFT_GMetrics gm;
	SFT_Image img;
	char* pixels;
	uint8_t* px;
	int i, j;
	SFT_Transform trf;
	epx_t2d_t mat2d;

	
	if (sft_gmetrics(&sft, glyph, &gm) < 0) {
	    fprintf(stderr, "glyph '%c' metric  not found\n", gid);
	    goto error;
	}
	printf("glyph '%c' metric\n", gid);
	printf("advanceWidth: %f\n", gm.advanceWidth);
	printf("leftSideBearing: %f\n", gm.leftSideBearing);
	printf("yOffset: %d\n", gm.yOffset);
	printf("minWidth: %d\n", gm.minWidth);
	printf("minHeight: %d\n", gm.minHeight);

	img.width = (gm.minWidth + 3) & ~3;
	img.height = gm.minHeight;
	img.pixels = malloc(img.width * img.height);

	epx_t2d_identity(&mat2d);  
	epx_t2d_rotate(&mat2d, 0.0, &mat2d);
	epx_t2d_get(&mat2d, trf);

	// render 1000 times benchmark
	t0 = time_tick();
	for (i = 0; i < BENCH_SIZE; i++) {
	    if (sft_render(&sft, glyph, trf, &img) < 0) {
		fprintf(stderr, "glyph '%c' not rendered\n", gid);
		free(img.pixels);
		goto error;
	    }
	}
	t1 = time_tick();
    
	printf("glyph '%c' pixels\n", gid);
	px = img.pixels;
	for (j = 0; j < img.height; j++) {
	    for (i = 0;  i < img.width; i++) {
		uint8_t x = *px++;
		if (x <= 10)
		    putchar(' ');
		else if (x <= 50)
		    putchar('.');
		else 
		    putchar('#');
	    }
	    putchar('\n');
	}
	printf("render/s %ld\n",
	       (long)(1000000.0*(((double)BENCH_SIZE)/(t1-t0))));
	}
done:
    sft_freefont(font);
    exit(0);
error:
    sft_freefont(font);
    exit(1);
}
