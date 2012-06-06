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
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>

#include <gif_lib.h>
#include "epx.h"

static void error(const char * s, ...)
{
    va_list args;
    va_start(args, s);
    vfprintf(stderr, s, args);
    fprintf(stderr, "\n");
    va_end(args);
}

epix_pixmap_t* epx_image_load_gif(char* file_name, epx_format_t format)
{
    GifFileType* ft;
    epx_pixmap_t* pixmap = NULL;
    epx_pixmap_t* pixmap0 = NULL;
    int i;

    if ((ft = DGifOpenFileName(file_name)) == NULL) {
	error("%s: could not be opened for reading", file_name);
	return 0;
    }

    if (DGifSlurp(ft) == GIF_ERROR) {
	error("%s: error reading gif file",  file_name);
	goto error;
    }
#ifdef DEBUG
    fprintf(stderr, "width: %d, height: %d\n", ft->SWidth, ft->SHeight);
    fprintf(stderr, "resolution: %d\n", ft->SColorResolution);
    fprintf(stderr, "background: %d\n", ft->SBackGroundColor);
    fprintf(stderr, "colormap: %p\n", ft->SColorMap);
    fprintf(stderr, "ImageCount: %d\n", ft->ImageCount);
#endif

    mFrames     = ft->ImageCount;
    mPixmaps   = (epx_pixmap_t**) malloc(sizeof(epx_pixmap_t*)*ft->ImageCount);
    mUseAlpha  = (int*) malloc(sizeof(int)*ft->ImageCount);

    for (i = 0; i < mFrames; i++) {
	int Transparent = 0;
	int TransparentColor = -1;
	int DelayTime = 0;
	ColorMapObject* cmap;
	int x, y;
	int j;
	uint8_t* ptr;


	SavedImage* im = &ft->SavedImages[i];
	if (pixmap0) epx_pixmap_destroy(pixmap0);
	pixmap0 = pixmap;
	pixmap  = epx_pixmap_create(ft->SWidth, ft->sHeight, format);

	cmap = (im->ImageDesc.ColorMap != NULL) ? im->ImageDesc.ColorMap :
	    ft->SColorMap;
	
	/* Copy previous image and render subimage on top */
	if (pixmap0)
	    epx_pixmap_copy_to(pixmap0, pixmap);

	for (j = 0; j < im->ExtensionBlockCount; j++) {
	    uint8_t* bp = (uint8_t*) im->ExtensionBlocks[j].Bytes;
#ifdef DEBUG
	    fprintf(stderr, "Extension: %d size=%d\n", 
		    im->ExtensionBlocks[j].Function,
		    im->ExtensionBlocks[j].ByteCount);
#endif
	    if (im->ExtensionBlocks[j].Function == GRAPHICS_EXT_FUNC_CODE) {
		Transparent = bp[0] & 1;
		DelayTime   = bp[1] + (bp[2]<<8);
		TransparentColor = bp[3];
	    }
	}
#ifdef DEBUG
	fprintf(stderr, "     left:%d,\n",  im->ImageDesc.Left);
	fprintf(stderr, "      top: %d,\n", im->ImageDesc.Top);
	fprintf(stderr, "    width: %d,\n", im->ImageDesc.Width);
	fprintf(stderr, "   height: %d,\n", im->ImageDesc.Height);
	fprintf(stderr, "interlace: %d,\n", im->ImageDesc.Interlace);
	fprintf(stderr, "colormap: %p\n",   im->ImageDesc.ColorMap);
	fprintf(stderr, "Transparent: %d\n", Transparent);
	fprintf(stderr, "TransparentColor: %d\n", TransparentColor);
	fprintf(stderr, "DelayTime: %d\n", DelayTime);
#endif
	ptr = im->RasterBits;
	for (y = 0; y < im->ImageDesc.Height; y++) {
	    for (x = 0; x < im->ImageDesc.Width; x++) {
		int xi = x + im->ImageDesc.Left;
		int yi = y + im->ImageDesc.Top;
		epx_pixel_t px;
		uint8_t p = *ptr++;
		
		if (!Transparent || (p != TransparentColor)) {
		    GifColorType* cp = &cmap->Colors[p];
		    px.a = 255;
		    px.r = cp->Red;
		    px.g = cp->Green;
		    px.b = cp->Blue;
		    epx_pixmap_put_pixel(pixmap, xi, yi, 0, px);
		}
		else if ((i == 0) && Transparent && (p==TransparentColor)) {
		    px.a = 0;
		    px.r = px.g = px.b = 0;
		    epx_pixmap_put_pixel(pixmap, xi, yi, 0, px);
		}
	    }
	}
    }

    DGifCloseFile(ft);
    if (pixmap0) epx_pixmap_destroy(pixmap0);
    return pixmap;

error:
    DGifCloseFile(ft);
    if (pixmap0) epx_pixmap_destroy(pixmap0);
    if (pixmap) epx_pixmap_destroy(pixmap);
    return 0;
}

