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

#include "lodepng.h"

#include "epx.h"

epx_pixmap_t* epx_image_load_png(char* file_name, epx_format_t format)
{
    unsigned error;
    unsigned char* image;
    unsigned width, height;
    unsigned char* png = 0;
    size_t pngsize;
    LodePNGState state;
    epx_pixmap_t* pixmap;
    int y;

    lodepng_state_init(&state); // optionally customize the state

    error = lodepng_load_file(&png, &pngsize, file_name);
    if(!error) 
	error = lodepng_decode(&image, &width, &height, &state, png, pngsize);
    if (error)
	printf("error %u: %s\n", error, lodepng_error_text(error));
    free(png);

    if ((pixmap = epx_pixmap_create(width, height, format)) == NULL) {
	free(image);
	return NULL;
    }

    for (y = 0; y < height; y++) {
	int x;
	unsigned char* ptr = image + y*4*width;
	for (x = 0; x < width; x++) {
	    epx_pixel_t p;
	    p.r = ptr[0];
	    p.g = ptr[1];
	    p.b = ptr[2];
	    p.a = ptr[3];
	    epx_pixmap_put_pixel(pixmap, x, y, 0, p);
	    ptr += 4;
	}
    }


    /*use image here*/
    /*state contains extra information about the PNG such as text chunks, ...*/

    lodepng_state_cleanup(&state);
    free(image);

    return pixmap;
}
