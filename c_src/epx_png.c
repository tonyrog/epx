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
    png_structp png_ptr;
    png_infop info_ptr;
    int width;
    int height;
    epx_pixmap_t* pixmap;
    int c_warn = 1;

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

    width       = info_ptr->width;
    height      = info_ptr->height;
    color_type  = info_ptr->color_type;
    bit_depth   = info_ptr->bit_depth;

    png_read_update_info(png_ptr, info_ptr);

    rowbytes = info_ptr->rowbytes;

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

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    pixmap = epx_pixmap_create(width, height, format);

    for (y = 0; y < height; y++) {
	int x;
	epx_pixel_t p;
	png_byte* ptr = row_pointers[y];
	
	switch(color_type) {
	case PNG_COLOR_TYPE_GRAY:
	    p.a = EPX_ALPHA_OPAQUE;
	    for (x = 0; x < width; x++) {
		p.r = p.g = p.b = ptr[0];
		epx_pixmap_put_pixel(pixmap, x, y, 0, p);
		ptr += 1;
	    }
	    break;
	case PNG_COLOR_TYPE_PALETTE:
	    for (x = 0; x < width; x++) {
		png_byte index = ptr[0];
		if ((!(info_ptr->valid & PNG_INFO_PLTE)) ||
		    (index >= info_ptr->num_palette)) {
		    if (c_warn) {
			error("%s; palette or color not present", file_name);
			c_warn = 0;
		    }
		    break;
		}
		p.r = info_ptr->palette[index].red;
		p.g = info_ptr->palette[index].green;
		p.b = info_ptr->palette[index].blue;
		p.a = EPX_ALPHA_OPAQUE;
		if ((index < info_ptr->num_trans) &&
		    (info_ptr->valid & PNG_INFO_tRNS)) {
		    p.a = info_ptr->trans[index];
		}
		epx_pixmap_put_pixel(pixmap, x, y, 0, p);
		ptr += 1;
	    }
	    break;

	case PNG_COLOR_TYPE_RGB:
	    for (x = 0; x < width; x++) {
		p.r = ptr[0];
		p.g = ptr[1];
		p.b = ptr[2];
		p.a = EPX_ALPHA_OPAQUE;
		if (info_ptr->valid & PNG_INFO_tRNS) {
		    if ((p.r == info_ptr->trans_values.red) &&
			(p.g == info_ptr->trans_values.green) &&
			(p.b == info_ptr->trans_values.blue)) {
			p.a = EPX_ALPHA_TRANSPARENT;  // fully transparent
		    }
		}
		epx_pixmap_put_pixel(pixmap, x, y, 0, p);
		ptr += 3;
	    }
	    break;

	case PNG_COLOR_TYPE_RGB_ALPHA:
	    for (x = 0; x < width; x++) {
		epx_pixel_t p;
		p.r = ptr[0];
		p.g = ptr[1];
		p.b = ptr[2];
		p.a = ptr[3];
		epx_pixmap_put_pixel(pixmap, x, y, 0, p);
		ptr += 4;
	    }
	    break;

	case PNG_COLOR_TYPE_GRAY_ALPHA:
	    for (x = 0; x < width; x++) {
		p.r = p.g = p.b = ptr[0];
		p.a = ptr[1];
		epx_pixmap_put_pixel(pixmap, x, y, 0, p);
		ptr += 2;
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
	row_pointers[y] = 0;
    }
    free(row_pointers);
    return pixmap;

error:
    if (fp != NULL)
	fclose(fp);
    for (y = 0; y < height; y++) {
	if (row_pointers[y])
	    free(row_pointers[y]);
    }
    free(row_pointers);
    return 0;
}
