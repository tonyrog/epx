/*
 * Guillaume Cottenceau (gc at mandrakesoft.com)
 *
 * Copyright 2002 MandrakeSoft
 *
 * This software may be freely redistributed under the terms of the GNU
 * public license.
 *
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#define PNG_DEBUG 3
#include "ddspng.hh"
#include "ddsimg.hh"


static void error(const char * s, ...)
{
    va_list args;
    va_start(args, s);
    vfprintf(stderr, s, args);
    fprintf(stderr, "\n");
    va_end(args);
}

DDSPng::DDSPng(void)
{
    mPixmap = NULL;
}


DDSPng::~DDSPng(void)
{
    if (mPixmap)
	epx_pixmap_destroy(mPixmap);
}

void DDSPng::unload()
{
    if (mPixmap)
	epx_pixmap_destroy(mPixmap);
    mPixmap = NULL;
}

int DDSPng::load(char* file_name, int start, int stop)
{
    int y;
    png_byte header[8];	// 8 is the maximum size that can be checked
    png_bytep* row_pointers;
    int rowbytes;
    png_structp png_ptr;
    png_infop info_ptr;
    int useAlpha = 0;
    png_uint_32 width;
    png_uint_32 height;
    int bit_depth;
    int color_type;


    /* open file and test for it being a png */
    FILE *fp = fopen(file_name, "rb");

    if (!fp) {
	error("%s: could not be opened for reading", file_name);
	goto error;
    }


    if (fread(header, 1, 8, fp) != 8 || png_sig_cmp(header, 0, 8)) {
	error("%s: is not recognized as a PNG file", file_name);
	goto error;
    }


    /* initialize stuff */
    if ((png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,NULL,NULL,NULL)) == NULL) {
	error("%s: png_create_read_struct failed", file_name);
	goto error;
    }

    if ((info_ptr = png_create_info_struct(png_ptr)) == NULL) {
	error("%s: png_create_info_struct failed", file_name);
	goto error;
    }

    if (setjmp(png_jmpbuf(png_ptr))) {
	error("%s: Error during init_io",  file_name);
	goto error;
    }

    png_init_io(png_ptr, fp);
    png_set_sig_bytes(png_ptr, 8);

    png_read_info(png_ptr, info_ptr);
    // width       = info_ptr->width;
    // height      = info_ptr->height;
    // color_type  = info_ptr->color_type;
    png_get_IHDR(png_ptr, info_ptr, &width, &height,
		 &bit_depth, &color_type, NULL,  NULL, NULL);

    png_read_update_info(png_ptr, info_ptr);

    rowbytes = png_get_rowbytes(png_ptr, info_ptr);
    // rowbytes = info_ptr->rowbytes;

    /* read file */
    if (setjmp(png_jmpbuf(png_ptr))) {
	error("%s: Error during read_image", file_name);
	goto error;
    }

    row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
    if (row_pointers == NULL) {
	error("%s: unable to allocate %d bytes",
	      file_name, sizeof(png_bytep)*height);
	goto error;
    }

    memset(row_pointers, 0, sizeof(png_bytep)*height);
    for (y=0; y < (int)height; y++) {
	if ((row_pointers[y] = (png_byte*) malloc(rowbytes)) == NULL) {
	    error("%s: unable to allocate %d bytes",
		  file_name, rowbytes);
	    goto error;
	}
    }
    png_read_image(png_ptr, row_pointers);
    fclose(fp);

    // png_destroy_info_struct(png_ptr, info_ptr);

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    mPixmap = epx_pixmap_create(width, height, EPX_FORMAT_RGBA);

    for (y = 0; y < (int)height; y++) {
	static int c_warn = 1;
	int x;
	png_byte* ptr = row_pointers[y];

	switch(color_type) {
	case PNG_COLOR_TYPE_RGBA:
	    for (x = 0; x < (int)width; x++) {
		epx_pixel_t p;
		p.r = ptr[0];
		p.g = ptr[1];
		p.b = ptr[2];
		p.a = ptr[3];
		if (p.a != 255) useAlpha = 1;
		epx_pixmap_put_pixel(mPixmap, x, y, 0, p);
		ptr += 4;
	    }
	    break;
	case PNG_COLOR_TYPE_RGB:
	    for (x = 0; x < (int)width; x++) {
		epx_pixel_t p;
		p.r = ptr[0];
		p.g = ptr[1];
		p.b = ptr[2];
		p.a = 255;
		epx_pixmap_put_pixel(mPixmap, x, y, 0, p);
		ptr += 3;
	    }
	    break;
	default:
	    if (c_warn) {
		error("%s; unknown color type %d", file_name, color_type);
		c_warn = 0;
	    }
	    break;
	}
	free(row_pointers[y]);
    }
    mUseAlpha = useAlpha;
    return 0;

error:
    if (fp != NULL)
	fclose(fp);
    return -1;
}
