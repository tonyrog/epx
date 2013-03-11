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
 * SIMD privitives using SSE2
 *
 */
#ifndef __EPX_SIMD_SSE2_H__
#define __EPX_SIMD_SSE2_H__

#if defined(__SSE2__)
#include <emmintrin.h>

typedef __m128i epx_vector_i8_t;
typedef __m128i epx_vector_u8_t;
typedef __m128i epx_vector_i16_t;
typedef __m128i epx_vector_u16_t;
typedef __m128i epx_vector_i32_t;
typedef __m128i epx_vector_u32_t;
typedef __m128i epx_vector_t;

#define EPX_SIMD_VECTOR_SIZE  16
#define EPX_SIMD_VECTOR_ALIGN 16
#define EPX_SIMD_VECTOR_PIXELS_ARGB32 4  // # of A8R8G8B8 pixels per vector 
#define EPX_SIMD_VECTOR_PIXELS_ARGB16 8  // # of R5G6B5 pixels per vector
#define EPX_SIMD_VECTOR_PIXELS_ARGB15 8  // # of A1R5G5B5 pixels per vector

#define epx_simd_vector_set_8(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) \
    _mm_setr_epi8((x0),(x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),		\
		 (x9),(x10),(x11),(x12),(x13),(x14),(x15))

#define epx_simd_vector_set_16(y0,y1,y2,y3,y4,y5,y6,y7)	\
    _mm_setr_epi16((y0),(y1),(y2),(y3),(y4),(y5),(y6),(y7))

#define epx_simd_vector_set_32(z0,z1,z2,z3)	\
    _mm_setr_epi32((z0),(z1),(z2),(z3))


static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load_ua32(void* ptr)
{
    return _mm_loadu_si128((__m128i const*)(ptr));
}

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load(void* ptr)
{
    return _mm_load_si128((__m128i const*)(ptr));
}


static inline void __attribute__((__always_inline__))
epx_simd_vector_store(void* ptr, epx_vector_t vec)
{
    return _mm_store_si128((__m128i*)ptr, vec);
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
				     0,0,src[3],0);
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
    _mm_prefetch((void*)ptr,_MM_HINT_NTA);
}

// add element in a b with unsigned saturation
static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_adds_u8(epx_vector_u8_t a, epx_vector_u8_t b)
{
    return _mm_adds_epu8(a, b);
}

static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_alpha_32(epx_vector_u8_t alpha,
		  epx_vector_u8_t src,
		  epx_vector_u8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    // LOW 64 
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_unpacklo_epi8(alpha, zero);
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);
    // HIGH 64 
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_unpackhi_epi8(alpha, zero);
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packus_epi16(l16, h16);
}


static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_blend_argb32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packus_epi16(l16, h16);
}

// fade is a vector of 8 bit fixnum fraction to be multiplied with alpha.
// fade is packed in the low part of 16-bit entities.
static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_fade_argb32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    // LOW 64 
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);
    // HIGH 64 
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packus_epi16(l16, h16);
}

// Format: R8G8B8A8 and B8G8R8A8
// src = r1,g1,b1,a1,r2,g2,b2,a2,r3,g3,b3,a3,r4,g4,b4,a4
// dst = R1,G1,B1,A1,R2,G2,B2,A2,R3,G3,B3,A3,R4,G4,B4,A4
//
static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_blend_rgba32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);

    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    return _mm_packus_epi16(l16, h16);
}

static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_fade_rgba32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);

    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    return _mm_packus_epi16(l16, h16);
}

#endif
#endif
