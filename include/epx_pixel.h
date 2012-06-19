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
// Basic pixel definitions
//

#ifndef __EPX_PIXEL_H__
#define __EPX_PIXEL_H__

#include <stdio.h>
#include <stdint.h>

/* R=red, G=green, B=blue, A=alpha, X=ignore, L=luminance (gray level) */

/*
 * Layout
 *       << Format:4,          %% (RGB4/RGB5/..)
 *          _:2,               %% Reserbed
 *          BGR:1,             %% Pixels stored in BGR
 *          AlphaFirst:1,      %% Or Skip First if Alplha=0
 *          Alpha:1,           %% Use alpha
 *          LittleEndian:1     %% Pixels stored in little endian
 *          BitsPerPixel:6     %% (+1) => [1..64]
 *        >>
 */
#define EPX_F_Bgr         0x0200
#define EPX_F_Alpha       0x0100
#define EPX_F_AFirst      0x0080
#define EPX_F_Little      0x0040
#define EPX_M_Size        0x003f
#define EPX_M_Fmt         0xf000

#define EPX_FMT_RGB4   0
#define EPX_FMT_RGB5   1
#define EPX_FMT_RGB8   2
#define EPX_FMT_RGB10  3
#define EPX_FMT_RGB12  4
#define EPX_FMT_RGB16  5
#define EPX_FMT_RGB332 6
#define EPX_FMT_RGB232 7
#define EPX_FMT_RGB565 8
#define EPX_FMT_YUV8   9
#define EPX_FMT_ALPHA  10
#define EPX_FMT_GRAY   11
#define EPX_FMT_RED    12
#define EPX_FMT_GREEN  13
#define EPX_FMT_BLUE   14
#define EPX_FMT_CALPHA 15

#define EPX_FMT(Fmt,Bgr,Alpha,AlphaFirst,Little,BitsPerPixel) \
    (((Fmt)<<12)       |						\
     ((Bgr) << 9)      |						\
     ((Alpha)<<8)      |						\
     ((AlphaFirst)<<7) |						\
     ((Little)<<6)     |						\
     ((BitsPerPixel)-1))

#define EPX_BE_FMT(Fmt,Bgr,Alpha,AlphaFirst,BitsPerPixel) \
    EPX_FMT(Fmt,Bgr,Alpha,AlphaFirst,0,BitsPerPixel)

#define EPX_LE_FMT(Fmt,Bgr,Alpha,AlphaFirst,BitsPerPixel) \
    EPX_FMT(Fmt,Bgr,Alpha,AlphaFirst,1,BitsPerPixel)

// 64 bit
#define EPX_FORMAT_R16G16B16A16 EPX_BE_FMT(EPX_FMT_RGB16,0,1,0,64)
#define EPX_FORMAT_R16G16B16X16 EPX_BE_FMT(EPX_FMT_RGB16,0,0,0,64)
#define EPX_FORMAT_A16R16G16B16 EPX_BE_FMT(EPX_FMT_RGB16,0,1,1,64)
#define EPX_FORMAT_X16R16G16B16 EPX_BE_FMT(EPX_FMT_RGB16,0,0,1,64)

// 48 bit 
#define EPX_FORMAT_R16G16B16 EPX_BE_FMT(EPX_FMT_RGB16,0,0,0,48)

// 32 bit
#define EPX_FORMAT_R8G8B8A8 EPX_BE_FMT(EPX_FMT_RGB8,0,1,0,32)
#define EPX_FORMAT_R8G8B8X8 EPX_BE_FMT(EPX_FMT_RGB8,0,0,0,32)
#define EPX_FORMAT_A8R8G8B8 EPX_BE_FMT(EPX_FMT_RGB8,0,1,1,32)
#define EPX_FORMAT_X8R8G8B8 EPX_BE_FMT(EPX_FMT_RGB8,0,0,1,32)

#define EPX_FORMAT_B8G8R8A8 EPX_BE_FMT(EPX_FMT_RGB8,1,1,0,32)
#define EPX_FORMAT_B8G8R8X8 EPX_BE_FMT(EPX_FMT_RGB8,1,0,0,32)
#define EPX_FORMAT_A8B8G8R8 EPX_BE_FMT(EPX_FMT_RGB8,1,1,1,32)
#define EPX_FORMAT_X8B8G8R8 EPX_BE_FMT(EPX_FMT_RGB8,1,0,1,32)

#define EPX_FORMAT_L16A16   EPX_BE_FMT(EPX_FMT_GRAY,0,1,0,32)
#define EPX_FORMAT_A16L16   EPX_BE_FMT(EPX_FMT_GRAY,0,1,1,32)

#define EPX_FORMAT_A8Y8U8V8 EPX_BE_FMT(EPX_FMT_YUV8,0,1,1,32)
#define EPX_FORMAT_X8Y8U8V8 EPX_BE_FMT(EPX_FMT_YUV8,0,0,1,32)

#define EPX_FORMAT_A2R10G10B10 EPX_BE_FMT(EPX_FMT_RGB10,0,1,0,32)

// 24 bit
#define EPX_FORMAT_R8G8B8   EPX_BE_FMT(EPX_FMT_RGB8,0,0,0,24)
#define EPX_FORMAT_B8G8R8   EPX_BE_FMT(EPX_FMT_RGB8,1,0,0,24)
#define EPX_FORMAT_Y8U8V8   EPX_BE_FMT(EPX_FMT_YUV8,0,0,0,24)

// 16 bit
#define EPX_FORMAT_R5G5B5A1 EPX_BE_FMT(EPX_FMT_RGB5,0,1,0,16)
#define EPX_FORMAT_A1R5G5B5 EPX_BE_FMT(EPX_FMT_RGB5,0,1,1,16)
#define EPX_FORMAT_R5G5B5X1 EPX_BE_FMT(EPX_FMT_RGB5,0,0,0,16)
#define EPX_FORMAT_X1R5G5B5 EPX_BE_FMT(EPX_FMT_RGB5,0,0,1,16)
#define EPX_FORMAT_R5G6B5_BE EPX_BE_FMT(EPX_FMT_RGB565,0,0,0,16)
#define EPX_FORMAT_B5G6R5_BE EPX_BE_FMT(EPX_FMT_RGB565,1,0,0,16)
#define EPX_FORMAT_R5G6B5_LE EPX_LE_FMT(EPX_FMT_RGB565,0,0,0,16)
#define EPX_FORMAT_B5G6R5_LE EPX_LE_FMT(EPX_FMT_RGB565,1,0,0,16)
#define EPX_FORMAT_L8A8     EPX_BE_FMT(EPX_FMT_GRAY,0,1,0,16)
#define EPX_FORMAT_A8L8     EPX_BE_FMT(EPX_FMT_GRAY,0,1,1,16)
#define EPX_FORMAT_L16      EPX_BE_FMT(EPX_FMT_GRAY,0,0,0,16)

// 8 bit 
#define EPX_FORMAT_R2G3B2A1 EPX_BE_FMT(EPX_FMT_RGB232,0,1,0,8)
#define EPX_FORMAT_A1R2G3B2 EPX_BE_FMT(EPX_FMT_RGB232,0,1,1,8)
#define EPX_FORMAT_R2G3B2X1 EPX_BE_FMT(EPX_FMT_RGB232,0,0,0,8)
#define EPX_FORMAT_X1R2G3B2 EPX_BE_FMT(EPX_FMT_RGB232,0,0,1,8)
#define EPX_FORMAT_R3G3B2   EPX_BE_FMT(EPX_FMT_RGB332,0,0,0,8)
#define EPX_FORMAT_L8       EPX_BE_FMT(EPX_FMT_GRAY,0,0,0,8)
#define EPX_FORMAT_A8       EPX_BE_FMT(EPX_FMT_ALPHA,0,1,0,8)
#define EPX_FORMAT_R8       EPX_BE_FMT(EPX_FMT_RED,0,1,0,8)
#define EPX_FORMAT_G8       EPX_BE_FMT(EPX_FMT_GREEN,0,1,0,8)
#define EPX_FORMAT_B8       EPX_BE_FMT(EPX_FMT_BLUE,0,1,0,8)

#define EPX_FORMAT_L4       EPX_BE_FMT(EPX_FMT_GRAY,0,0,0,4)
#define EPX_FORMAT_A4       EPX_BE_FMT(EPX_FMT_ALPHA,0,0,0,4)
#define EPX_FORMAT_L2       EPX_BE_FMT(EPX_FMT_GRAY,0,0,0,2)
#define EPX_FORMAT_A2       EPX_BE_FMT(EPX_FMT_ALPHA,0,0,0,2)
#define EPX_FORMAT_L1       EPX_BE_FMT(EPX_FMT_GRAY,0,0,0,1)
#define EPX_FORMAT_A1       EPX_BE_FMT(EPX_FMT_ALPHA,0,0,0,1)

// Special EFNT2 format 
#define EPX_FORMAT_EFNT2    EPX_BE_FMT(EPX_FMT_CALPHA,0,1,0,8)

#define EPX_FORMAT_INVALID  0xFFFF


// ALIASES
#define EPX_FORMAT_ALPHA  EPX_FORMAT_A8
#define EPX_FORMAT_232    EPX_FORMAT_R2G3B2

#define EPX_FORMAT_565    EPX_FORMAT_R5G6B5_BE
#define EPX_FORMAT_565_LE EPX_FORMAT_R5G6B5_LE
#define EPX_FORMAT_565_BE EPX_FORMAT_R5G6B5_BE

#define EPX_FORMAT_RGB    EPX_FORMAT_R8G8B8
#define EPX_FORMAT_BGR    EPX_FORMAT_B8G8R8

#define EPX_FORMAT_RGBA   EPX_FORMAT_R8G8B8A8
#define EPX_FORMAT_ARGB   EPX_FORMAT_A8R8G8B8
#define EPX_FORMAT_BGRA   EPX_FORMAT_B8G8R8A8
#define EPX_FORMAT_ABGR   EPX_FORMAT_A8B8G8R8

#define EPX_PIXEL_BIT_SIZE(pt)   (((pt)&EPX_M_Size)+1)
#define EPX_PIXEL_SIZE(pt)       (EPX_PIXEL_BIT_SIZE(pt)>>3)
#define EPX_PIXEL_HAS_ALPHA(pt)  (((pt)&EPX_F_Alpha) != 0)

// size of pixel format
typedef uint16_t epx_format_t;

// ALPHA FACTOR (fixnum masks) 
#define ALPHA_FACTOR_0     0x00  /* 1 */
#define ALPHA_FACTOR_1     0xFF  /* 1 */
#define ALPHA_FACTOR_1_2   0x80  /* 1/2 */
#define ALPHA_FACTOR_1_4   0x40  /* 1/4 */
#define ALPHA_FACTOR_1_8   0x20  /* 1/8 */
#define ALPHA_FACTOR_1_16  0x10  /* 1/16 */
#define ALPHA_FACTOR_1_32  0x08  /* 1/32 */
#define ALPHA_FACTOR_1_64  0x04  /* 1/64 */
#define ALPHA_FACTOR_1_128 0x02  /* 1/128 */
#define ALPHA_FACTOR_1_256 0x01  /* 1/256 */

//
// This is the basic color stucture.
// It is optimised to read/write ARGB in that memory order
//

#define EPX_ALPHA_TRANSPARENT 0
#define EPX_ALPHA_OPAQUE      255

typedef struct
{
    float   a;  // 0.0-1.0
    float   r;  // 0.0-1.0
    float   g;  // 0.0-1.0
    float   b;  // 0.0-1.0
} epx_pixel_argb_01_t;

typedef struct
{
    float     a;  // 0.0 - 1.0
    float     h;  // [0.0 - 360)
    float     s;  // [0.0 - 1.0]
    float     v;  // [0.0 - 1.0]
} epx_pixel_hsv_t;

typedef struct {
    uint8_t a;
    uint8_t y;
    uint8_t u;
    uint8_t v;
} epx_pixel_yuv_t;

typedef union {
    uint8_t v[4];
    uint32_t px;
    struct {
	uint8_t a;
	uint8_t r;
	uint8_t g;
	uint8_t b;
    };
} epx_pixel_t;


#define W16(x) ((uint16_t)(x))
#define W32(x) ((uint32_t)(x))

static inline uint16_t u16REV(uint16_t x)
{
    return (x<<8) | (x>>8);
}

static inline uint32_t u32REV(uint32_t x)
{
    return (x<<24) | (x>>24) | ((x<<8) & 0x00ff0000) | ((x>>8) & 0x0000ff00);
}

#define EPX_PIXEL_ARGB(A,R,G,B) {{(A),(R),(G),(B)}}
#define EPX_PIXEL_RGB(R,G,B) EPX_PIXEL_ARGB(EPX_ALPHA_OPAQUE,(R),(G),(B))

#if BYTE_ORDER == BIG_ENDIAN

#define I16LE(x)  ((int16_t) u16REV(x))
#define I32LE(x)  ((int32_t) u32REV(x))
#define U16LE(x)  U16REV(x)
#define U32LE(x)  U32REV(x)

#define I16BE(x)  ((int16_t)(x))
#define I32BE(x)  ((int32_t)(x))
#define U16BE(x)  ((uint16_t)(x))
#define U32BE(x)  ((uint32_t)(x))

#else

#define I16LE(x)  ((int16_t)(x))
#define I32LE(x)  ((int32_t)(x))
#define U16LE(x)  ((uint16_t)(x))
#define U32LE(x)  ((uint32_t)(x))

#define I16BE(x)  ((int16_t) u16REV(x))
#define I32BE(x)  ((int32_t) u32REV(x))
#define U16BE(x)  U16REV(x)
#define U32BE(x)  U32REV(x)

#endif

#define EPX_PIXEL_WHITE       EPX_PIXEL_RGB(255,255,255)
#define EPX_PIXEL_BLACK       EPX_PIXEL_RGB(0,0,0)
#define EPX_PIXEL_RED         EPX_PIXEL_RGB(255,0,0)
#define EPX_PIXEL_GREEN       EPX_PIXEL_RGB(0,255,0)
#define EPX_PIXEL_BLUE        EPX_PIXEL_RGB(0,0,255)
#define EPX_PIXEL_TRANSPARENT EPX_PIXEL_ARGB(0,0,0,0)

typedef enum {
    EPX_PIXEL_OP_CLEAR,
    EPX_PIXEL_OP_SRC,
    EPX_PIXEL_OP_DST,
    EPX_PIXEL_OP_SRC_OVER,
    EPX_PIXEL_OP_DST_OVER,
    EPX_PIXEL_OP_SRC_IN,
    EPX_PIXEL_OP_DST_IN,
    EPX_PIXEL_OP_SRC_OUT,
    EPX_PIXEL_OP_DST_OUT,
    EPX_PIXEL_OP_SRC_ATOP,
    EPX_PIXEL_OP_DST_ATOP,
    EPX_PIXEL_OP_XOR,
    EPX_PIXEL_OP_COPY,
    EPX_PIXEL_OP_ADD,
    EPX_PIXEL_OP_SUB,
    EPX_PIXEL_OP_SRC_BLEND,
    EPX_PIXEL_OP_DST_BLEND
} epx_pixel_operation_t;


static inline epx_pixel_t epx_pixel_argb(uint8_t a, uint8_t r, 
					 uint8_t g, uint8_t b)
{
    epx_pixel_t p = EPX_PIXEL_ARGB(a,r,g,b);
    return p;
}

// Swap R and B component
static inline epx_pixel_t epx_pixel_swap(epx_pixel_t p)
{
    return epx_pixel_argb(p.a, p.b, p.g, p.r);
}

static inline epx_pixel_t epx_pixel_rgb(uint8_t r, uint8_t g, uint8_t b)
{
    epx_pixel_t p = EPX_PIXEL_RGB(r,g,b);
    return p;
}

#define epx_pixel_transparent epx_pixel_argb(EPX_ALPHA_TRANSPARENT,0,0,0)

static inline uint8_t epx_blend(uint8_t a, uint8_t s, uint8_t d)
{
    return (a*(s-d) + (d<<8))>>8;
}

static inline uint8_t epx_add(uint8_t a, uint8_t b)
{
    uint16_t s = a+b;
    if (s > 255) return 255; // saturate
    return s;
}

static inline uint8_t epx_sub(uint8_t a, uint8_t b)
{
    int16_t s = a-b;
    if (s < 0) return 0; // cut
    return s;
}

static inline uint8_t epx_multiply(uint8_t a, uint8_t b)
{
    uint16_t p = a*b;
    return p >> 8;
}

// epx_shadow(a,d) == epx_blend(a,0,d)
static inline uint8_t epx_shadow(uint8_t a, uint8_t d)
{
    return ((d<<8) - a*d)>>8;
}

// epx_scale(a,s) == epx_blend(a,s,0)
static inline uint8_t epx_scale(uint8_t a, uint8_t s)
{
    return (a*s)>>8;
}

// epx_over(a1, c1, a2, c2)
// a1*c1 + (1-a1)*a2*c2
// 
// 
static inline uint8_t epx_over(uint8_t a1, uint8_t c1, 
			       uint8_t a2, uint8_t c2)
{
    uint8_t c3 = epx_multiply(a2,c2);
    return epx_blend(a1, c1, c3);
}

static inline uint8_t epx_atop(uint8_t aa, uint8_t a, 
			       uint8_t ba, uint8_t b)
{
    a = (a*ba) >> 8;
    b = (b*ba) >> 8;
    return epx_blend(aa, a, b);
}

static inline uint8_t epx_xor(uint8_t aa, uint8_t a, 
			      uint8_t ba, uint8_t b)
{
    a = (a*(255-ba)) >> 8;
    b = (b*ba) >> 8;
    return epx_blend(aa, a, b);
}



// scale colors
static inline epx_pixel_t epx_pixel_scale(uint8_t s, epx_pixel_t a)
{
    epx_pixel_t r;
    r.a = s;
    r.r = epx_scale(s, a.r);
    r.g = epx_scale(s, a.g);
    r.b = epx_scale(s, a.b);
    return r;
}

// OVER(A,B)  R=Aa*A+(1-Aa)*B*Ba,  Ra=Aa+Ba-Aa*Ba
static inline epx_pixel_t epx_pixel_over(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t r;
    r.a = (a.a*(255-b.a) + (b.a<<8))>>8;
    r.r = epx_over(a.a, a.r, b.a, b.r);
    r.g = epx_over(a.a, a.g, b.a, b.g);
    r.b = epx_over(a.a, a.b, b.a, b.b);
    return r;
}

// IN(A,B)  R=A*Aa*Ba	Ra=Aa*Ba
static inline epx_pixel_t epx_pixel_in(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t r;
    uint8_t s = (a.a * b.a) >> 8;
    
    r.a = s;
    r.r = epx_scale(s, a.r);
    r.g = epx_scale(s, a.g);
    r.b = epx_scale(s, a.b);
    return r;
}

// OUT(A,B)  R=A*Aa*(1-Ba)	Ra=Aa*(1-Ba)
static inline epx_pixel_t epx_pixel_out(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t r;
    uint8_t s = (a.a*(255-b.a)) >> 8;

    r.a = s;
    r.r = epx_scale(s, a.r);
    r.g = epx_scale(s, a.g);
    r.b = epx_scale(s, a.b);
    return r;
}

// ATOP(A,B)  R=Aa*A*Ba+(1-Aa)*B*Ba		Ra=Ba
static inline epx_pixel_t epx_pixel_atop(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t r;
    r.a = b.a;
    r.r = epx_atop(a.a, a.r, b.a, b.r);
    r.g = epx_atop(a.a, a.g, b.a, b.g);
    r.b = epx_atop(a.a, a.b, b.a, b.b);
    return r;
}

// XOR(A,B)  R=A*Aa*(1-Ba) + B*Ba*(1-A),  R=Aa+Ba-2*Aa*Ba
static inline epx_pixel_t epx_pixel_xor(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t r;
    r.a = (((a.a + b.b) << 8) - 2*a.a*b.b) >> 8;
    r.r = epx_xor(a.a, a.r, b.a, b.r);
    r.g = epx_xor(a.a, a.g, b.a, b.g);
    r.b = epx_xor(a.a, a.b, b.a, b.b);
    return r;
}

static inline epx_pixel_t epx_pixel_fade(uint8_t fade, epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t r;
    uint8_t s = (a.a*fade >> 8);
    r.a=epx_blend(s,0,b.a); // NEW
    r.r=epx_blend(s,0,b.r);
    r.g=epx_blend(s,0,b.g);
    r.b=epx_blend(s,0,b.b);
    return r;
}

// simple blend 
static inline epx_pixel_t epx_pixel_blend(uint8_t a, epx_pixel_t s, epx_pixel_t d)
{
    epx_pixel_t p;
    p.r = epx_blend(a, s.r, d.r);
    p.g = epx_blend(a, s.g, d.g);
    p.b = epx_blend(a, s.b, d.b);
    p.a = epx_blend(a, s.a, d.a);
    return p;
}

static inline epx_pixel_t epx_pixel_add(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t s;

    s.a = epx_add(a.a, b.a);
    s.r = epx_add(a.r, b.r);
    s.g = epx_add(a.g, b.g);
    s.b = epx_add(a.b, b.b);
    return s;
}

static inline epx_pixel_t epx_pixel_sub(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t s;

    s.a = epx_sub(a.a, b.a);
    s.r = epx_sub(a.r, b.r);
    s.g = epx_sub(a.g, b.g);
    s.b = epx_sub(a.b, b.b);
    return s;
}

// multiply channel by channel
static inline epx_pixel_t epx_pixel_multiply(epx_pixel_t a, epx_pixel_t b)
{
    epx_pixel_t p;

    p.a = epx_multiply(a.a, b.a);
    p.r = epx_multiply(a.r, b.r);
    p.g = epx_multiply(a.g, b.g);
    p.b = epx_multiply(a.b, b.b);
    return p;
}


static inline epx_pixel_t epx_pixel_shadow(uint8_t g, epx_pixel_t d)
{
    epx_pixel_t p;

    p.a = epx_shadow(g, d.a);
    p.r = epx_shadow(g, d.r);
    p.g = epx_shadow(g, d.g);
    p.b = epx_shadow(g, d.b);
    return p;
}

/* luminance */
static inline uint8_t epx_pixel_luminance(epx_pixel_t p)
{
    return (p.r*299 + p.g*587 + p.b*114) / 1000;
    // return (p.r*30 + p.g*59 + p.b*11) / 100;
    // return (p.r*2989 + p.g*5870 + p.b*1140) / 10000;
}

// Y := min(abs(r * 2104 + g * 4130 + b * 802 + 4096 + 131072) >> 13, 235)
// U := min(abs(r * -1214 + g * -2384 + b * 3598 + 4096 + 1048576) >> 13, 240)
// V := min(abs(r * 3598 + g * -3013 + b * -585 + 4096 + 1048576) >> 13, 240)
static inline epx_pixel_yuv_t epx_pixel_rgb2yuv(epx_pixel_t p)
{
    epx_pixel_yuv_t q;

    q.a = p.a;
    q.y = (p.r*299 + p.g*587 + p.b*114) / 1000;
    q.u = (p.r*701 - p.g*587 - p.b*115) / 1000;
    q.v = (-p.r*299 - 587*p.g + 886+p.b) / 1000;
    return q;
}

static inline epx_pixel_t epx_pixel_yuv2rgb(epx_pixel_yuv_t q)
{
    epx_pixel_t p;
    p.a = q.a;
    p.r = q.y + q.u;
    p.g = q.y - (510*q.u - 186*q.v)/1000;
    p.b = q.y + q.v;
    return p;
}

typedef epx_pixel_t (*epx_pixel_binary_op_t)(epx_pixel_t a, epx_pixel_t b);

typedef epx_pixel_t (*epx_pixel_unpack_t)(uint8_t* src);
typedef void     (*epx_pixel_pack_t)(epx_pixel_t p, uint8_t* dst);

extern epx_pixel_unpack_t epx_pixel_unpack_func(epx_format_t fmt);
extern epx_pixel_pack_t epx_pixel_pack_func(epx_format_t fmt);

extern epx_format_t epx_pixel_format_from_name(char* name);
extern char* epx_pixel_format_to_name(epx_format_t fmt);

extern epx_pixel_t  epx_pixel_from_string(char* name);

#endif
