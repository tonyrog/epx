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
 * SIMD privitives using NEON
 * -mfloat-abi=softfp -mfpu=neon -flax-vector-conversions
 */
#ifndef __EPX_SIMD_NEON_H__
#define __EPX_SIMD_NEON_H__

#if defined(__NEON__)
#include <arm_neon.h>

typedef int8x16_t   epx_vector_i8_t;
typedef uint8x16_t  epx_vector_u8_t;
typedef int16x8_t   epx_vector_i16_t;
typedef uint16x8_t  epx_vector_u16_t;
typedef int32x4_t   epx_vector_i32_t;
typedef uint32x4_t  epx_vector_u32_t;
typedef float32x4_t epx_vector_f32_t;
typedef uint8x16_t  epx_vector_t;

#define EPX_SIMD_VECTOR_SIZE  16
#define EPX_SIMD_VECTOR_ALIGN 16
#define EPX_SIMD_VECTOR_PIXELS_ARGB32 4  // # of A8R8G8B8 pixels per vector 
#define EPX_SIMD_VECTOR_PIXELS_ARGB16 8  // # of R5G6B5 pixels per vector
#define EPX_SIMD_VECTOR_PIXELS_ARGB15 8  // # of A1R5G5B5 pixels per vector

static inline epx_vector_t epx_neon_set_u8(uint8_t x1,uint8_t x2,uint8_t x3,
					   uint8_t x4,uint8_t x5,uint8_t x6,
					   uint8_t x7,uint8_t x8,uint8_t x9,
					   uint8_t x10,uint8_t x11,uint8_t x12,
					   uint8_t x13, uint8_t x14,
					   uint8_t x15, uint8_t x16)
{
    uint8_t xs[] = {x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16};
    return (epx_vector_t) vld1q_u8((const uint8_t*)xs);
}

static inline epx_vector_t epx_neon_set_u16(uint16_t y1,uint16_t y2,
					    uint16_t y3,uint16_t y4,
					    uint16_t y5,uint16_t y6,
					    uint16_t y7,uint16_t y8)
{
    uint16_t ys[] = {y1,y2,y3,y4,y5,y6,y7,y8};
    return (epx_vector_t) vld1q_u16((const uint16_t*)ys);
}

static inline epx_vector_t epx_neon_set_u32(uint32_t y1,uint32_t y2,
					    uint32_t y3,uint32_t y4)
{
    uint32_t ys[] = {y1,y2,y3,y4};
    return  (epx_vector_t) vld1q_u32((const uint32_t*)ys);
}


#define epx_simd_vector_set_8(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16) \
    epx_neon_set_u8((x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),		\
		    (x9),(x10),(x11),(x12),(x13),(x14),(x15),(x16))

#define epx_simd_vector_set_16(y1,y2,y3,y4,y5,y6,y7,y8) \
    epx_neon_set_u16((y1),(y2),(y3),(y4),(y5),(y6),(y7),(y8))

#define epx_simd_vector_set_32(y1,y2,y3,y4) \
    epx_neon_set_u32((y1),(y2),(y3),(y4))

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load_ua32(void* ptr)
{
    // unaligned load works ?
    return vld1q_u8((const uint8_t*)(ptr));
}

static inline epx_vector_t __attribute__((__always_inline__))
epx_simd_vector_load(void* ptr)
{
    return vld1q_u8((const uint8_t*)(ptr));
}

static inline void __attribute__((__always_inline__))
epx_simd_vector_store(void* ptr, epx_vector_t vec)
{
    return vst1q_u8((uint8_t*)ptr, vec);
}


static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_splat_u8(uint8_t v)
{
    return vdupq_n_u8(v);
}

static inline epx_vector_u16_t __attribute__((__always_inline__))
epx_simd_vector_splat_u16(uint16_t v)
{
    return vdupq_n_u16(v);
}

static inline epx_vector_u32_t __attribute__((__always_inline__))
epx_simd_vector_splat_u32(uint32_t v)
{
    return vdupq_n_u32(v);
}

static inline epx_vector_u8_t __attribute__((__always_inline__))
epx_simd_vector_set_pixel (uint8_t a,uint8_t r,uint8_t g,uint8_t b)
{
    uint32_t p = (b<<24)|(g<<16)|(r<<8)|a;  // reversed
    return (epx_vector_u8_t) vdupq_n_u32(p);
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
    (void) ptr;
    // nothing here
}

// add element in a b with unsigned saturation
static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_adds_u8(epx_vector_u8_t a, epx_vector_u8_t b)
{
    return vqaddq_u8(a, b);
}

// fixme:
//
//    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);    // t16 = t16*a16 + d16
// =? t16 = vmlaq_s16(d16, t16, a16)


static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_alpha_32(epx_vector_u8_t alpha,
		  epx_vector_u8_t src,
		  epx_vector_u8_t dst)
{
    int16x8_t d16, s16, a16, t16;
    uint16x8_t l16, h16;

    // LOW 64 
    s16 = vmovl_u8(vget_low_u8(src));
    a16 = vmovl_u8(vget_low_u8(alpha));
    d16 = vmovl_u8(vget_low_u8(dst));
    
    t16 = vsubq_s16(s16, d16);                   // t16 = s16-d16
    d16 = vshlq_n_s16(d16, 8);                   // d16 <<= 8
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);    // t16 = t16*a16 + d16
    l16 = vshrq_n_s16(t16, 8);                   // l16 = t16>>8

    // HIGH 64 
    s16 = vmovl_u8(vget_high_u8(src));
    a16 = vmovl_u8(vget_high_u8(alpha));
    d16 = vmovl_u8(vget_high_u8(dst));

    t16 = vsubq_s16(s16, d16);                   // t16 = s16-d16
    d16 = vshlq_n_s16(d16, 8);                   // d16 <<= 8
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);    // t16 = t16*a16 + d16
    h16 = vshrq_n_s16(t16, 8);                   // h16 = t16>>8

    return vcombine_u8(vmovn_u16(l16), vmovn_u16(h16));
}


static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_blend_argb32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    int16x8_t d16, s16, a16, t16;
    int16x8_t l16, h16;

    // LOW 64
    s16 = vmovl_u8(vget_low_u8(src));
    a16 = vshlq_n_u64(s16, 48);
    a16 = veorq_s16(a16, vshrq_n_u64(a16, 16));
    a16 = veorq_s16(a16, vshrq_n_u64(a16, 32));

    d16 = vmovl_u8(vget_low_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    l16 = vshrq_n_s16(t16, 8);

    // HIGH 64
    s16 = vmovl_u8(vget_high_u8(src));
    a16 = vshlq_n_u64(s16, 48);
    a16 = veorq_u8(a16, vshrq_n_u64(a16, 16));
    a16 = veorq_u8(a16, vshrq_n_u64(a16, 32));

    d16 = vmovl_u8(vget_high_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    h16 = vshrq_n_s16(t16, 8);

    return vcombine_u8(vmovn_u16(l16), vmovn_u16(h16));
}

// fade is a vector of 8 bit fixnum fraction to be multiplied with alpha.
// fade is packed in the low part of 16-bit entities.
static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_fade_argb32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    int16x8_t d16, s16, a16, t16;
    int16x8_t l16, h16;

    // LOW 64 
    s16 = vmovl_u8(vget_low_u8(src));
    a16 = vshlq_n_u64(s16, 48);
    a16 = veorq_u8(a16, vshrq_n_u64(a16, 16));
    a16 = veorq_u8(a16, vshrq_n_u64(a16, 32));
    a16 = vmulq_u16(fade,a16);
    a16 = vshrq_n_u16(a16, 8);

    d16 = vmovl_u8(vget_low_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    l16 = vshrq_n_s16(t16, 8);

    // HIGH 64 
    s16 = vmovl_u8(vget_high_u8(src));
    a16 = vshlq_n_u64(s16, 48);
    a16 = veorq_u8(a16, vshrq_n_u64(a16, 16));
    a16 = veorq_u8(a16, vshrq_n_u64(a16, 32));
    a16 = vmulq_u16(fade,a16);
    a16 = vshrq_n_u16(a16, 8);

    d16 = vmovl_u8(vget_high_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    h16 = vshrq_n_s16(t16, 8);
    
    return vcombine_u8(vmovn_u16(l16), vmovn_u16(h16));
}

// Format: R8G8B8A8 and B8G8R8A8
// src = r1,g1,b1,a1,r2,g2,b2,a2,r3,g3,b3,a3,r4,g4,b4,a4
// dst = R1,G1,B1,A1,R2,G2,B2,A2,R3,G3,B3,A3,R4,G4,B4,A4
//
static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_blend_rgba32(epx_vector_u8_t src,
		      epx_vector_u8_t dst)
{
    int16x8_t d16, s16, a16, t16;
    int16x8_t l16, h16;

    // LOW 64
    s16 = vmovl_u8(vget_low_u8(src));
    a16 = vshrq_n_u64(s16, 48);
    a16 = veorq_s16(a16, vshlq_n_u64(a16, 16));
    a16 = veorq_s16(a16, vshlq_n_u64(a16, 32));

    d16 = vmovl_u8(vget_low_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    l16 = vshrq_n_s16(t16, 8);

    // HIGH 64
    s16 = vmovl_u8(vget_high_u8(src));
    a16 = vshrq_n_u64(s16, 48);
    a16 = veorq_u8(a16, vshlq_n_u64(a16, 16));
    a16 = veorq_u8(a16, vshlq_n_u64(a16, 32));

    d16 = vmovl_u8(vget_high_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    h16 = vshrq_n_s16(t16, 8);

    return vcombine_u8(vmovn_u16(l16), vmovn_u16(h16));
}

static inline epx_vector_u8_t __attribute__((__always_inline__)) 
epx_simd_fade_rgba32(epx_vector_u16_t fade,
		     epx_vector_u8_t src,
		     epx_vector_u8_t dst)
{
    int16x8_t d16, s16, a16, t16;
    int16x8_t l16, h16;

    // LOW 64 
    s16 = vmovl_u8(vget_low_u8(src));
    a16 = vshrq_n_u64(s16, 48);
    a16 = veorq_u8(a16, vshlq_n_u64(a16, 16));
    a16 = veorq_u8(a16, vshlq_n_u64(a16, 32));
    a16 = vmulq_u16(fade,a16);
    a16 = vshrq_n_u16(a16, 8);

    d16 = vmovl_u8(vget_low_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    l16 = vshrq_n_s16(t16, 8);

    // HIGH 64 
    s16 = vmovl_u8(vget_high_u8(src));
    a16 = vshrq_n_u64(s16, 48);
    a16 = veorq_u8(a16, vshlq_n_u64(a16, 16));
    a16 = veorq_u8(a16, vshlq_n_u64(a16, 32));
    a16 = vmulq_u16(fade,a16);
    a16 = vshrq_n_u16(a16, 8);

    d16 = vmovl_u8(vget_high_u8(dst));
    t16 = vsubq_s16(s16, d16);
    d16 = vshlq_n_s16(d16, 8);
    t16 = vaddq_s16(vmulq_s16(t16,a16), d16);
    h16 = vshrq_n_s16(t16, 8);
    
    return vcombine_u8(vmovn_u16(l16), vmovn_u16(h16));
}

#endif
#endif
