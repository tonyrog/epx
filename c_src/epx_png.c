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
// Load a PNG file as an epx_pixmap
//

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>

// we are not using tIME stuff so ignore it, just warnings anyway!
#define PNG_NO_CONVERT_tIME
#include <png.h>

#include "epx.h"


static void error(const char * s, ...)
{
    va_list args;
    va_start(args, s);
    vfprintf(stderr, s, args);
    fprintf(stderr, "\n");
    va_end(args);
}

epx_pixmap_t* epx_image_load_png(char* file_name, epx_format_t format)
{
    int y;
    png_byte header[8];	// 8 is the maximum size that can be checked
    png_bytep* row_pointers;
    int rowbytes;
    png_byte color_type;
    png_byte bit_depth;
    png_structp png_ptr = 0;
    png_infop info_ptr = 0;
    int width;
    int height;
    epx_pixmap_t* pixmap;
    png_byte interlace;

    /* open file and test for it being a png */
    FILE *fp = fopen(file_name, "rb");

    if (!fp) {
	error("%s: could not be opened for reading", file_name);
	goto error;
    }

    fread(header, 1, 8, fp);
    if (png_sig_cmp(header, 0, 8)) {
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

    width       = png_get_image_width(png_ptr, info_ptr);
    height      = png_get_image_height(png_ptr, info_ptr);
    color_type  = png_get_color_type(png_ptr, info_ptr);
    bit_depth   = png_get_bit_depth(png_ptr, info_ptr);
    interlace   = png_get_interlace_type(png_ptr, info_ptr);

    if (color_type == PNG_COLOR_TYPE_PALETTE)
	png_set_palette_to_rgb(png_ptr);

    if (color_type == PNG_COLOR_TYPE_GRAY) {
#if PNG_LIBPNG_VER >= 10209
	png_set_expand_gray_1_2_4_to_8 (png_ptr);
#else
	png_set_gray_1_2_4_to_8 (png_ptr);
#endif
    }

    if (png_get_valid (png_ptr, info_ptr, PNG_INFO_tRNS))
	png_set_tRNS_to_alpha (png_ptr);

    if (bit_depth == 16)
        png_set_strip_16 (png_ptr);

    if (bit_depth < 8)
        png_set_packing(png_ptr);

    if (color_type == PNG_COLOR_TYPE_GRAY ||
	color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
	png_set_gray_to_rgb (png_ptr);

    if (interlace != PNG_INTERLACE_NONE)
	png_set_interlace_handling (png_ptr);

    switch (color_type) {
    default:
    case PNG_COLOR_TYPE_GRAY_ALPHA:
    case PNG_COLOR_TYPE_RGB_ALPHA:
	// alpah = 1;
	// png_set_read_user_transform_fn (png, premultiply_data);
	break;
    case PNG_COLOR_TYPE_GRAY:
    case PNG_COLOR_TYPE_PALETTE:
    case PNG_COLOR_TYPE_RGB:
	// alpah = 0;
	// png_set_read_user_transform_fn (png, convert_bytes_to_data);
	png_set_filler (png_ptr, 0xff, PNG_FILLER_AFTER);
	break;
    }
    
    png_read_update_info(png_ptr, info_ptr);

    rowbytes = png_get_rowbytes(png_ptr, info_ptr);
    
    /* read file */
    if (setjmp(png_jmpbuf(png_ptr))) {
	error("%s: Error during read_image", file_name);
	goto error;
    }

    row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
    if (!row_pointers) {
	error("%s: unable to allocate %d bytes", 
	      file_name, sizeof(png_bytep)*height);
	goto error;
    }

    memset(row_pointers, 0, sizeof(png_bytep)*height);
    for (y=0; y < height; y++) {
	if ((row_pointers[y] = (png_byte*) malloc(rowbytes)) == NULL) {
	    error("%s: unable to allocate %d bytes", 
		  file_name, rowbytes);
	    goto error;
	}
    }
    png_read_image(png_ptr, row_pointers);
    fclose(fp);
    fp = 0;

    pixmap = epx_pixmap_create(width, height, format);

    for (y = 0; y < height; y++) {
	int x;
	png_byte* ptr = row_pointers[y];
	
	for (x = 0; x < width; x++) {
	    epx_pixel_t p;
	    p.r = ptr[0];
	    p.g = ptr[1];
	    p.b = ptr[2];
	    p.a = ptr[3];
	    epx_pixmap_put_pixel(pixmap, x, y, 0, p);
	    ptr += 4;
	}
	free(row_pointers[y]);
	row_pointers[y] = 0;
    }
    free(row_pointers);
    if (png_ptr)
	png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    return pixmap;

error:
    if (fp != NULL)
	fclose(fp);
    for (y = 0; y < height; y++) {
	if (row_pointers[y])
	    free(row_pointers[y]);
    }
    free(row_pointers);
    if (png_ptr)
	png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    return 0;
}
