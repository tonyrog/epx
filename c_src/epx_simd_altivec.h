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
 * SIMD primitives using Altivec
 *
 */
#ifndef __EPX_SIMD_ALTIVEC_H__
#define __EPX_SIMD_ALTIVEC_H__

#if defined(__VEC__) && defined(__ALTIVEC__)
#include <altivec.h>

typedef vector int8_t    epx_vector_i8_t;
typedef vector uint8_t   epx_vector_u8_t;
typedef vector int16_t   epx_vector_i16_t;
typedef vector uint16_t  epx_vector_u16_t;
typedef vector int32_t   epx_vector_i32_t;
typedef vector uint32_t  epx_vector_u32_t;
typedef vector uint8_t   epx_vector_t;

#define EPX_SIMD_VECTOR_SIZE  16
#define EPX_SIMD_VECTOR_ALIGN 16
#define EPX_SIMD_VECTOR_PIXELS_ARGB32 4  // # of A8R8G8B8 pixels per vector 
#define EPX_SIMD_VECTOR_PIXELS_ARGB16 8  // # of R5G6B5 pixels per vector
#define EPX_SIMD_VECTOR_PIXELS_ARGB15 8  // # of A1R5G5B5 pixels per vector

#define SIMD_FUNCTION(name) epx_simd_##name##_altivec

#define epx_simd_vector_set_8(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) \
    ((epx_vector_i8_t)((x0),(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),	\
		       (x9),(x10),(x11),(x12),(x13),(x14),(x15)))

#define epx_simd_vector_set_16(y0,y1,y2,y3,y4,y5,y6,y7)			\
    ((epx_vector_i16_t)((y0),(y1),(y2),(y3),(y4),(y5),(y6),(y7)))

#define epx_simd_vector_set_32(z0,z1,z2,z3)	\
    ((epx_vector_i32_t)((z0),(z1),(z2),(z3)))

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load_ua32(void* ptr)
{
    return epx_vector_set_32(((uint32_t*)(ptr))[0],
			     ((uint32_t*)(ptr))[1],
			     ((uint32_t*)(ptr))[2],
			     ((uint32_t*)(ptr))[3]);
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_splat_u8(uint8_t v)
{
    return epx_simd_vector_set_8(v,v,v,v,v,v,v,v,
				 v,v,v,v,v,v,v,v);
}

static inline epx_vector_u16_t __attribute__((__always_inline__))
epx_simd_vector_splat_u16(uint16_t v)
{
    return epx_simd_vector_set_16(v,v,v,v,v,v,v,v);
}

static inline epx_vector_u32_t __attribute__((__always_inline__))
epx_simd_vector_splat_u32(uint32_t v)
{
    return epx_simd_vector_set_32(v,v,v,v);
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_set_pixel (uint8_t a,uint8_t r,uint8_t g,uint8_t b)
{
    return epx_simd_vector_set_8(a,r,g,b,
				 a,r,g,b,
				 a,r,g,b,
				 a,r,g,b);
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_set_subpixel (uint8_t* src,int k)
{
    switch(k) {
    case 0: 
	return epx_simd_vector_set_8(src[0],0,0,0,
				     src[1],0,0,0,
				     src[2],0,0,0,
				     src[3],0,0,0);
    case 1: 
	return epx_simd_vector_set_8(0,src[0],0,0,
				     0,src[1],0,0,
				     0,src[2],0,0,
				     0,src[3],0,0);
    case 2: 
	return epx_simd_vector_set_8(0,0,src[0],0,
				     0,0,src[1],0,
				     0,0,src[2],0,
				     0,0,src[3],0,);
    case 3:
	return epx_simd_vector_set_8(0,0,0,src[0],
				     0,0,0,src[1],
				     0,0,0,src[2],
				     0,0,0,src[3]);
    default:
	return epx_simd_vector_splat_u8(0);
    }
}


static inline void __attribute__((__always_inline__))
epx_simd_empty_state()
{
    // nothing here
}

static inline void __attribute__((__always_inline__))
epx_simd_prefetch(uint8_t* ptr)
{
    // nothing here
}

// add saturate
static inline epx_vector_u8_t epx_simd_adds(epx_vector_i8_t a, 
					    epx_vector_u8_t b)
{
    return vec_adds(a, b);
}

static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_alpha_32(epx_vector_u8_t alpha,
		  epx_vector_u8_t src,
		  epx_vector_u8_t dst)
{
    vector int16_t   d16, s16, a16, t16;
    vector uint16_t l16, h16;
    vector uint8_t  zero = vec_xor(src, src);

    /* LOW 64 */
    s16 = vec_mergel(zero, src);
    a16 = vec_mergel(zero, alpha);
    d16 = vec_mergel(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector uint16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    l16 = vec_sr(t16, ((vector uint16_t)(8)));

    /* HIGH 64 */
    s16 = vec_mergeh(zero, src);
    a16 = vec_mergeh(zero, alpha);
    d16 = vec_mergeh(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector uint16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    h16 = vec_sr(t16, ((vector uint16_t)(8)));
    return (vector uint8_t) vec_pack(h16, l16);
}

static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_blend_argb32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    vector int16_t   d16, s16, a16, t16;
    vector uint16_t l16, h16;
    vector uint8_t  zero = vec_xor(src, src);

    /* LOW 64 */
    /* | a1 | r1 | g1 | b1 | a2 | r2 | g2 | b2 | */
    s16 = vec_mergel(zero, src);
    a16 = vec_sr((vector uint64_t)s16, (vector uint16_t)(48));
    a16 = vec_sl((vector uint64_t)a16, (vector uint16_t)(16));
    a16 = vec_sl((vector uint64_t)a16, (vector uint16_t)(32));

    /* | A1 | R1 | G1 | B1 | A2 | R2 | G2 | B2 | */
    d16 = vec_mergel(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector uint16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    l16 = vec_sr(t16, ((vector uint16_t)(8)));

    /* HIGH 64 */
    /* | a3 | r3 | g3 | b3 | a4 | r4 | g4 | b4 | */
    s16 = vec_mergeh(zero, src);
    a16 = vec_sr((vector uint64_t)s16, (vector uint16_t)(48));
    a16 = vec_sl((vector uint64_t)a16, (vector uint16_t)(16));
    a16 = vec_sl((vector uint64_t)a16, (vector uint16_t)(32));

    /* | A1 | R1 | G1 | B1 | A2 | R2 | G2 | B2 | */
    d16 = vec_mergeh(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector uint16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    h16 = vec_sr(t16, ((vector uint16_t)(8)));

    return (vector uint8_t) vec_pack(h16, l16);
}

static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_fade_argb32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    /* FIXME */
}


static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_blend_rgba32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    vector int16_t   d16, s16, a16, t16;
    vector uint16_t l16, h16;
    vector uint8_t  zero = vec_xor(src, src);

    /* LOW 64 */
    /* | r1 | g1 | b1 | a1 | r2 | g2 | b2 | a2 | */
    s16 = vec_mergel(zero, src);
    a16 = vec_sl((vector uint64_t)s16, (vector uint16_t)(48));
    a16 = vec_sr((vector uint64_t)a16, (vector uint16_t)(16));
    a16 = vec_sr((vector uint64_t)a16, (vector uint16_t)(32));

    /* | R1 | G1 | B1 | A1 | R2 | G2 | B2 | A2 |*/
    d16 = vec_mergel(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector uint16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    l16 = vec_sr(t16, ((vector uint16_t)(8)));

    /* HIGH 64 */
    /* | r3 | g3 | b3 | a3 | r4 | g4 | b4 | a4 | */
    s16 = vec_mergeh(zero, src);
    a16 = vec_sl((vector uint64_t)s16, (vector uint16_t)(48));
    a16 = vec_sr((vector uint64_t)a16, (vector uint16_t)(16));
    a16 = vec_sr((vector uint64_t)a16, (vector uint16_t)(32));

    /* | R3 | G3 | B3 | A3 | R4 | G4 | B4 | A4 | */
    d16 = vec_mergeh(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector uint16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    h16 = vec_sr(t16, ((vector uint16_t)(8)));

    return (vector uint8_t) vec_pack(h16, l16);
}

static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_fade_rgba32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    /* FIXME */
}

#endif
#endif

