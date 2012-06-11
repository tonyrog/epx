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
 * SIMD/MMX functions
 */
#ifndef __EPX_SIMD_MMX_H__
#define __EPX_SIMD_MMX_H__

#if defined(__MMX__)
#include <mmintrin.h>

typedef __m64 epx_vector_i8_t;
typedef __m64 epx_vector_u8_t;
typedef __m64 epx_vector_i16_t;
typedef __m64 epx_vector_u16_t;
typedef __m64 epx_vector_i32_t;
typedef __m64 epx_vector_u32_t;
typedef __m64 epx_vector_t;

#define EPX_SIMD_VECTOR_SIZE  8
#define EPX_SIMD_VECTOR_ALIGN 4
#define EPX_SIMD_VECTOR_PIXELS_ARGB32 2  // # of A8R8G8B8 pixels per vector
#define EPX_SIMD_VECTOR_PIXELS_ARGB16 4  // # of R5G6B5 pixels per vector
#define EPX_SIMD_VECTOR_PIXELS_ARGB15 4  // # of A1R5G5B5 pixels per vector

#define SIMD_FUNCTION(name) epx_simd_##name##_mmx

#define epx_simd_vector_set_8(x0,x1,x2,x3,x4,x5,x6,x7)	\
    _mm_setr_pi8((x0),(x1),(x2),(x3),(x4),(x5),(x6),(x7))

#define epx_simd_vector_set_16(y0,y1,y2,y3)	\
    _mm_setr_pi16((y0),(y1),(y2),(y3))

#define epx_simd_vector_set_32(z0,z1)	\
    _mm_setr_pi32((z0),(z1))

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load_ua32(void* ptr)
{
    return epx_simd_vector_set_32(((uint32_t*)(ptr))[0],((uint32_t*)(ptr))[1]);
}

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load(void* ptr)
{
    return *((__m64 const*)(ptr));
}


static inline void __attribute__((__always_inline__))
epx_simd_vector_store(void* ptr, epx_vector_t vec)
{
    *((__m64*)(ptr)) = vec;
}


static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_splat_u8(uint8_t v)
{
    return epx_simd_vector_set_8(v,v,v,v,v,v,v,v);
}

static inline epx_vector_u16_t __attribute__((__always_inline__))
epx_simd_vector_splat_u16(uint16_t v)
{
    return epx_simd_vector_set_16(v,v,v,v);
}

static inline epx_vector_u32_t __attribute__((__always_inline__))
epx_simd_vector_splat_u32(uint32_t v)
{
    return epx_simd_vector_set_32(v,v);
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_set_pixel (uint8_t a,uint8_t r,uint8_t g,uint8_t b)
{
    return epx_simd_vector_set_8(a,r,g,b,
				 a,r,g,b);
}

static inline void __attribute__((__always_inline__))
epx_simd_empty_state()
{
    _mm_empty();
}

static inline void __attribute__((__always_inline__))
epx_simd_prefetch(uint8_t* ptr)
{
    (void) ptr;
    // nothing here
}

// add element in a b with unsigned saturation
static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_adds_u8(epx_vector_u8_t a, epx_vector_u8_t b)
{
    return _mm_adds_pu8(a, b);
}


/* blend src and dst using a fixed alpha value 
 *  alpha is may vary for the components!
 */
static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_alpha_32(epx_vector_u8_t alpha,
		  epx_vector_u8_t src,
		  epx_vector_u8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_unpacklo_pi8(alpha, zero);
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_unpackhi_pi8(alpha, zero);
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packs_pu16(l16, h16);
}

/* blend src and dst using inline alpha value:
 * format: ARGB | ABGR
 */
static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_blend_argb32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    /* src = ARGB argb */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packs_pu16(l16, h16);
}

// fade is a vector of 8 bit fixnum fraction to be multiplied with alpha.
// fade is packed in the low part of 16-bit entities.
static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_fade_argb32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packs_pu16(l16, h16);
}

// Format: R8G8B8A8 and B8G8R8A8
// src = r1,g1,b1,a1,r2,g2,b2,a2,r3,g3,b3,a3,r4,g4,b4,a4
// dst = R1,G1,B1,A1,R2,G2,B2,A2,R3,G3,B3,A3,R4,G4,B4,A4
//

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_blend_rgba32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    // LOW 64 
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);

    // HIGH 64
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    return _mm_packs_pu16(l16, h16);
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_fade_rgba32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    // LOW 64
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);

    // HIGH 64 
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    return _mm_packs_pu16(l16, h16);
}


#endif
#endif
