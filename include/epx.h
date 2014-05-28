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
// EPX 
//
//
#ifndef __EPX_H__
#define  __EPX_H__

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <memory.h>

#if defined(HAVE_OPENGL)
#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glext.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glext.h>
#endif
#endif

#include "epx_debug.h"
#include "epx_pixel.h"
#include "epx_colors.h"
#include "epx_simd.h"
#include "epx_dict.h"
#include "epx_pixmap.h"
#include "epx_anim.h"
#include "epx_gc.h"
#include "epx_draw.h"
#include "epx_window.h"
#include "epx_backend.h"
#include "epx_font.h"
#include "epx_hash.h"
#include "epx_lock.h"

extern void epx_init(int accel);

/* OpenGL utility functions */
#ifdef HAVE_OPENGL
extern int epx_gl_load_texture(epx_pixmap_t* pic, GLuint* textureName,
			       int useAlpha, int useClient, GLuint wrap,
			       int src_x, int src_y, 
			       unsigned int width, unsigned int height);
#endif

#endif
