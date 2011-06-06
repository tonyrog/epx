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
#include "epx_gc.h"
#include "epx_draw.h"
#include "epx_window.h"
#include "epx_backend.h"
#include "epx_font.h"
#include "epx_hash.h"

extern void epx_init(int accel);

extern int epx_cpu_serial_number(unsigned char* buf, size_t maxlen);
extern int epx_cpu_vendor_name(char* buf, size_t maxlen);

/* OpenGL utility functions */
#ifdef HAVE_OPENGL
extern int epx_gl_load_texture(epx_pixmap_t* pic, GLuint* textureName,
			       int useAlpha, int useClient, GLuint wrap,
			       int src_x, int src_y, 
			       unsigned int width, unsigned int height);
#endif

#endif
