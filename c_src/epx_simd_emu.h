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
/*
 * SIMD/EMU functions
 */
#ifndef __EPX_SIMD_EMU__
#define __EPX_SIMD_EMU__

#include <stdint.h>

typedef uint32_t epx_vector_i8_t;
typedef uint32_t epx_vector_u8_t;
typedef uint32_t epx_vector_i16_t;
typedef uint32_t epx_vector_u16_t;
typedef uint32_t epx_vector_i32_t;
typedef uint32_t epx_vector_u32_t;
typedef uint32_t epx_vector_t;

#define EPX_SIMD_VECTOR_SIZE         4
#define EPX_SIMD_VECTOR_ALIGN        4
#define EPX_SIMD_VECTOR_PIXELS_ARGB32 1  // number of A8R8G8B8 pixels per vector
#define EPX_SIMD_VECTOR_PIXELS_ARGB16 2  // number of R5G6B5 pixels per vector
#define EPX_SIMD_VECTOR_PIXELS_ARGB15 2  // number of A1R5G5B5 pixels per vector

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_set_8(uint8_t x0,uint8_t x1,uint8_t x2,uint8_t x3)
{
    epx_vector_t r;
    uint8_t* pr = (uint8_t*) &r;
    pr[0]=x0; pr[1]=x1; pr[2]=x2; pr[3]=x3;
    return r;
}

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_set_16(uint16_t y0,uint16_t y1)
{
    epx_vector_t r = 0;  // strange, gcc handles set_8 but not this case
    uint16_t* pr = (uint16_t*) &r;
    pr[0]=y0; pr[1]=y1;
    return r;
}

#define epx_simd_vector_set_32(z0) \
    ((z0))

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load_ua32(void* ptr)
{
    return epx_simd_vector_set_32(((uint32_t*)(ptr))[0]);
}

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load(void* ptr)
{
    return *((uint32_t*)(ptr));
}

static inline void __attribute__((__always_inline__))
epx_simd_vector_store(void* ptr, epx_vector_t vec)
{
    *((uint32_t*)(ptr)) = vec;
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_splat_u8(uint8_t v)
{
    return epx_simd_vector_set_8(v,v,v,v);
}

static inline epx_vector_u16_t __attribute__((__always_inline__))
epx_simd_vector_splat_u16(uint16_t v)
{
    return epx_simd_vector_set_16(v,v);
}

static inline epx_vector_u32_t __attribute__((__always_inline__))
epx_simd_vector_splat_u32(uint32_t v)
{
    return epx_simd_vector_set_32(v);
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_set_pixel(uint8_t a,uint8_t r,uint8_t g,uint8_t b)
{
    return epx_simd_vector_set_8(a,r,g,b);
}


static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_set_subpixel (uint8_t* src,int k)
{
    switch(k) {
    case 0:
	return epx_simd_vector_set_8(src[0],0,0,0);
    case 1:
	return epx_simd_vector_set_8(0,src[0],0,0);
    case 2:
	return epx_simd_vector_set_8(0,0,src[0],0);
    case 3:
	return epx_simd_vector_set_8(0,0,0,src[0]);
    default:
	return epx_simd_vector_splat_u8(0);
    }
}


static inline void __attribute__((__always_inline__))
epx_simd_empty_state(void)
{
    // nothing here    
}

static inline void __attribute__((__always_inline__))
epx_simd_prefetch(uint8_t* ptr)
{
    (void) ptr;
    // nothing here
}


// Adjust alpha [0-255] -> [0-256] by adding the high bit
#define AADJUST(a) (a) // ((a) + ((a)>>7))

static inline epx_vector_u8_t 
epx_simd_adds_u8(epx_vector_u8_t a, 
		 epx_vector_u8_t b)
{
    uint16_t s;
    epx_vector_u8_t r;
    uint8_t* ap = (uint8_t*) &a;
    uint8_t* bp = (uint8_t*) &b;
    uint8_t* rp = (uint8_t*) &r;

    s = ap[0]+bp[0]; if (s>255) s=255;
    rp[0] = s;

    s = ap[1]+bp[1]; if (s>255) s=255;
    rp[1] = s;

    s = ap[2]+bp[2]; if (s>255) s=255;
    rp[2] = s;

    s = ap[3]+bp[3]; if (s>255) s=255;
    rp[3] = s;

    return r;
}

/* blend src and dst using a fixed alpha value 
 *  alpha may vary for the components!
 */
static inline epx_vector_u8_t 
epx_simd_alpha_32(epx_vector_u8_t alpha,
		  epx_vector_u8_t src,
		  epx_vector_u8_t dst)
{
    epx_vector_u8_t r;
    uint8_t* sp = (uint8_t*) &src;
    uint8_t* dp = (uint8_t*) &dst;
    uint8_t* ap = (uint8_t*) &alpha;
    uint8_t* rp = (uint8_t*) &r;

    rp[0] = epx_blend(AADJUST(ap[0]), sp[0], dp[0]);
    rp[1] = epx_blend(AADJUST(ap[1]), sp[1], dp[1]);
    rp[2] = epx_blend(AADJUST(ap[2]), sp[2], dp[2]);
    rp[3] = epx_blend(AADJUST(ap[3]), sp[3], dp[3]);
    return r;
}

/* blend src and dst using inline alpha value:
 * format: ARGB | ABGR
 */
static inline epx_vector_u8_t 
epx_simd_blend_argb32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    uint8_t a;
    epx_vector_u8_t r;
    uint8_t* sp = (uint8_t*) &src;
    uint8_t* dp = (uint8_t*) &dst;
    uint8_t* rp = (uint8_t*) &r;

    a = AADJUST(sp[0]);
    rp[0] = epx_blend(a, sp[0], dp[0]);
    rp[1] = epx_blend(a, sp[1], dp[1]);
    rp[2] = epx_blend(a, sp[2], dp[2]);
    rp[3] = epx_blend(a, sp[3], dp[3]);
    return r;
}

static inline epx_vector_u8_t 
epx_simd_blend_rgba32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    uint8_t a;
    epx_vector_u8_t r;
    uint8_t* sp = (uint8_t*) &src;
    uint8_t* dp = (uint8_t*) &dst;
    uint8_t* rp = (uint8_t*) &r;

    a = AADJUST(sp[3]);
    rp[0] = epx_blend(a, sp[0], dp[0]);
    rp[1] = epx_blend(a, sp[1], dp[1]);
    rp[2] = epx_blend(a, sp[2], dp[2]);
    rp[3] = epx_blend(a, sp[3], dp[3]);
    return r;
}

// fade is a vector of 8 bit fixnum fraction to be multiplied with alpha.
// fade is packed in the low part of 16-bit entities.
static inline epx_vector_u8_t 
epx_simd_fade_argb32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    uint8_t a;
    uint16_t a16;
    epx_vector_u8_t r;
    uint8_t* sp = (uint8_t*) &src;
    uint8_t* dp = (uint8_t*) &dst;
    uint8_t* rp = (uint8_t*) &r;
    uint16_t* fp = (uint16_t*) &fade;

    a16 = sp[0] * fp[0];
    a = AADJUST(a16>>8);
    rp[0] = epx_blend(a, sp[0], dp[0]);
    rp[1] = epx_blend(a, sp[1], dp[1]);
    rp[2] = epx_blend(a, sp[2], dp[2]);
    rp[3] = epx_blend(a, sp[3], dp[3]);
    return r;
}

static inline epx_vector_u8_t 
epx_simd_fade_rgba32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    uint8_t a;
    uint16_t a16;
    epx_vector_u8_t r;
    uint8_t* sp = (uint8_t*) &src;
    uint8_t* dp = (uint8_t*) &dst;
    uint8_t* rp = (uint8_t*) &r;
    uint16_t* fp = (uint16_t*) &fade;

    a16 = sp[3] * fp[0];
    a = AADJUST(a16>>8);
    rp[0] = epx_blend(a, sp[0], dp[0]);
    rp[1] = epx_blend(a, sp[1], dp[1]);
    rp[2] = epx_blend(a, sp[2], dp[2]);
    rp[3] = epx_blend(a, sp[3], dp[3]);
    return r;
}


#endif
