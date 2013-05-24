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
// Some common OpenGL utility functions
//

#include "../include/epx_pixmap.h"

#if defined(__APPLE__)
#include <machine/endian.h>
#else
#include <endian.h>
#endif

#if 0
static unsigned int nearest_pow2(unsigned int n)
{
    unsigned int i = 1;

    if (n == 0) return 0;
    if (!((n) & (n-1))) return n;
    while(i < n)
	i *= 2;
    return i;
}
#endif

#if defined(HAVE_OPENGL)
#include <GL/gl.h>
//
// useAlpha=false make alpha channel opaque
// useAlpha=true  use alpha value as is
// useClient=true make use of client stored data, assume texture data
//                survive the rendering phase.
// wrap=GL_CLAMP, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, 
//      GL_REPEAT or GL_MIRRORED_REPEAT
//
// Load textures formats:
//    1 - ARB_texture_non_power_of_two
//    2 - ARB_texture_rectangle  (used if non normalized is request)
//    3 - GL_TEXTURE_2D (possibly scaled otherwise)
//
//
int epx_gl_load_texture(epx_pixmap_t* pic, GLuint* textureName,
			int useAlpha, int useClient, GLuint wrap,
			int src_x, int src_y,
			unsigned int width,unsigned int height)
{
    GLint  tx_iformat;
    GLenum tx_format;
    GLenum tx_type;
    float  saveScale;
    float  saveBias;
    GLenum target;
    int normalized = 1;
    epx_pixmap_t* pic2 = NULL;

    switch(pic->pixel_format) {
    case EPX_FORMAT_R8G8B8:
	tx_iformat = GL_RGB8;
	tx_format  = GL_RGB;
	tx_type    = GL_UNSIGNED_BYTE;
	break;
    case EPX_FORMAT_565_BE:
	tx_iformat = GL_RGB5;
	tx_format  = GL_RGB;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_SHORT_5_6_5;
#else
	tx_type    = GL_UNSIGNED_SHORT_5_6_5_REV;
#endif
	break;
    case EPX_FORMAT_565_LE:
	tx_iformat = GL_RGB5;
	tx_format  = GL_RGB;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_SHORT_5_6_5_REV;
#else
	tx_type    = GL_UNSIGNED_SHORT_5_6_5;
#endif
	break;
    case EPX_FORMAT_B8G8R8:
	tx_iformat = GL_RGB8;
	tx_format  = GL_BGR;
	tx_type    = GL_UNSIGNED_BYTE;
	break;
    case EPX_FORMAT_A8R8G8B8:
	tx_iformat = GL_RGBA8;
	tx_format  = GL_BGRA_EXT;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_INT_8_8_8_8_REV;
#else
	tx_type    = GL_UNSIGNED_INT_8_8_8_8;
#endif
	break;
    case EPX_FORMAT_R8G8B8A8:
	tx_iformat = GL_RGBA8;
	tx_format  = GL_RGBA;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_INT_8_8_8_8;
#else
	tx_type    = GL_UNSIGNED_INT_8_8_8_8_REV;
#endif
	break;
    case EPX_FORMAT_B8G8R8A8:
	tx_iformat = GL_RGBA8;
	tx_format  = GL_BGRA;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_INT_8_8_8_8;
#else
	tx_type    = GL_UNSIGNED_INT_8_8_8_8_REV;
#endif
	break;
    default:
	return -1; // Better error code?
    }

    if (*textureName == 0)
	glGenTextures(1, textureName);

    /* FIXME check extensions better */
    if (normalized) {
#ifndef GL_ARB_texture_non_power_of_two
	unsigned int width2 = nearest_pow2(width);
	unsigned int height2 = nearest_pow2(height);

	printf("POT: width2=%d, height2=%d\n", width2, height2);
	if ((width2==0) || (height2==0))
	    return -1;

	if ((width2 != width) || (height2 != height)) {
	    epx_pixmap_t* pic2 = epx_pixmap_create(width2, height2, 
						   pic->pixel_format);
	    printf("POT: scale image\n");
	    gluScaleImage(tx_format, 
			  width, height, tx_type, EPX_PIXEL_ADDR(pic,src_x,src_y),
			  width2, height2, tx_type, EPX_PIXEL_ADDR(pic2,0,0));
	    width = width2;
	    height = height2;
	    pic = pic2;
	}
#endif
	target = GL_TEXTURE_2D;
    }
    else {
#ifdef GL_ARB_texture_rectangle
	target = GL_TEXTURE_RECTANGLE_ARB;

#endif
    }
    glBindTexture (target, *textureName);
    glTexParameteri(target,GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(target,GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(target,GL_TEXTURE_WRAP_S, wrap);
    glTexParameteri(target,GL_TEXTURE_WRAP_T, wrap);

    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    glPixelStorei (GL_UNPACK_ROW_LENGTH, 
		   pic->bytes_per_row/pic->bytes_per_pixel);
    glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
#ifdef GL_UNPACK_CLIENT_STORAGE_APPLE
    if (useClient)
	// This speeds processing up from 40-50 => 50-60  fps :-)
	// BUT WE MUST PRESERVE THE PIXELS until next reload!!!!!!
	// this is what useClient is for
	glPixelStorei (GL_UNPACK_CLIENT_STORAGE_APPLE, GL_TRUE);
#endif

    if (!useAlpha) {
	// Do not use possibly bad alpha value
	glGetFloatv(GL_ALPHA_SCALE, &saveScale);
	glGetFloatv(GL_ALPHA_BIAS, &saveBias);
	glPixelTransferf(GL_ALPHA_SCALE, 0.0);
	glPixelTransferf(GL_ALPHA_BIAS,  1.0);
    }
    // FIXME Update with:
    // glTexSubImage2D(target, 0, 0, 0, width, height, tx_format, tx_type,
    //                 EPIXEL_ADDR(pic, src_x, src_y))
    //  but only if width & height is same as original texture
    glTexImage2D (target,
		  0, 
		  tx_iformat, 
		  width, 
		  height,
		  0, 
		  tx_format, 
		  tx_type,
		  EPX_PIXEL_ADDR(pic, src_x, src_y));

    if (!useAlpha) {
	// Reset saved values 
	glPixelTransferf(GL_ALPHA_SCALE, saveScale);
	glPixelTransferf(GL_ALPHA_BIAS,  saveBias);
    }
    if (pic2)
	epx_pixmap_destroy(pic2);
    return 0;
}

#endif
