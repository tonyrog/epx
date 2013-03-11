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
 * Simd setup fucntions
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <memory.h>

#include "epx_debug.h"
#include "epx_cpuid.h"
#include "epx_simd.h"

#define EXTERN_SIMD_API(type) \
    extern void epx_simd_copy##type(uint8_t* src, uint8_t* dst, size_t n); \
    extern void epx_simd_fill_32##type(uint8_t* src, uint32_t v, size_t n); \
    extern void epx_simd_add_blend_area_rgba32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af, epx_pixel_t color,unsigned int width,unsigned int height); \
    extern void epx_simd_add_blend_area_argb32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af, epx_pixel_t color,unsigned int width,unsigned int height); \
    extern void epx_simd_add_blend_area_a8_rgba32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af, epx_pixel_t color,unsigned int width,unsigned int height); \
    extern void epx_simd_add_blend_area_a8_argb32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af, epx_pixel_t color,unsigned int width,unsigned int height); \
    extern void epx_simd_alpha_area_argb32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af,unsigned int width,unsigned int height); \
    extern void epx_simd_alpha_area_rgba32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af,unsigned int width,unsigned int height); \
    extern void epx_simd_blend_area_rgba32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,unsigned int width,unsigned int height); \
    extern void epx_simd_blend_area_argb32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,unsigned int width,unsigned int height); \
    extern void epx_simd_fade_area_rgba32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af,unsigned int width,unsigned int height); \
    extern void epx_simd_fade_area_argb32##type(uint8_t* src,int src_wb,uint8_t* dst,int dst_wb,uint8_t af,unsigned int width,unsigned int height); \
    extern void epx_simd_fill_area_blend_rgb24##type(uint8_t* dst,int dst_wb,epx_pixel_t color,unsigned int width,unsigned int height); \
    extern void epx_simd_fill_area_blend_argb32##type(uint8_t* dst,int dst_wb,epx_pixel_t color,unsigned int width,unsigned int height); \
    extern void epx_simd_fill_area_blend_rgba32##type(uint8_t* dst,int dst_wb,epx_pixel_t color,unsigned int width,unsigned int height)

#define INIT_SIMD_API(t,n) \
    { .type = (t),					\
      .copy = epx_simd_copy##n,		\
      .fill_32 = epx_simd_fill_32##n,				\
      .add_blend_area_rgba32 = epx_simd_add_blend_area_rgba32##n, \
      .add_blend_area_argb32 = epx_simd_add_blend_area_argb32##n, \
      .add_blend_area_a8_rgba32 = epx_simd_add_blend_area_a8_rgba32##n, \
      .add_blend_area_a8_argb32 = epx_simd_add_blend_area_a8_argb32##n, \
      .alpha_area_argb32 = epx_simd_alpha_area_argb32##n,		\
      .alpha_area_rgba32 = epx_simd_alpha_area_rgba32##n,		\
      .blend_area_rgba32 = epx_simd_blend_area_rgba32##n,		\
      .blend_area_argb32 = epx_simd_blend_area_argb32##n,		\
      .fade_area_rgba32 = epx_simd_fade_area_rgba32##n,		\
      .fade_area_argb32 = epx_simd_fade_area_argb32##n,		\
      .fill_area_blend_rgb24 = epx_simd_fill_area_blend_rgb24##n, \
      .fill_area_blend_argb32 = epx_simd_fill_area_blend_argb32##n, \
      .fill_area_blend_rgba32 = epx_simd_fill_area_blend_rgba32##n, \
    }

EXTERN_SIMD_API(_emu);
static epx_simd_t simd_emu = INIT_SIMD_API(EPX_SIMD_EMU,_emu);

#if defined(__VEC__) && defined(__ALTIVEC__)
EXTERN_SIMD_API(_altivec);
static epx_simd_t simd_altivec = INIT_SIMD_API(EPX_SIMD_ALTIVEC,_altivec);
#endif

#if defined(__MMX__) && defined(USE_MMX)
EXTERN_SIMD_API(_mmx);
static epx_simd_t simd_mmx = INIT_SIMD_API(EPX_SIMD_MMX,_mmx);
#endif

#if defined(__SSE2__) && defined(USE_SSE2)
EXTERN_SIMD_API(_sse2);
static epx_simd_t simd_sse2 = INIT_SIMD_API(EPX_SIMD_SSE2,_sse2);
#endif

// SIMD function block pointer
epx_simd_t* epx_simd = NULL;

static unsigned char   cpu_serial_number[64];
static size_t          cpu_serial_number_len = 0;
static char            cpu_vendor_name[64];
static size_t          cpu_vendor_name_len = 0;
static char            cpu_features[256];
static size_t          cpu_features_len = 0;
static int             cpu_cache_line_size = 0;
//
// Return cpu serial number if available
// return 0 if not available otherwise the 
// length of the full serial number in bytes
//
int epx_cpu_serial_number(unsigned char* buf, size_t maxlen)
{
    int n = (cpu_serial_number_len > maxlen) ? maxlen : cpu_serial_number_len;
    memcpy(buf, cpu_serial_number, n);
    return cpu_serial_number_len;
}

//
// Return cpu vendor name if available
// retur 0 if not avaiable otherwise the 
// lenfth of the full cpu vendor name in bytes
//
int epx_cpu_vendor_name(char* buf, size_t maxlen)
{
    int n = (cpu_vendor_name_len > maxlen) ? maxlen : cpu_vendor_name_len;
    memcpy(buf, cpu_vendor_name, n);
    return n;
}

int epx_cpu_features(char* buf, size_t maxlen)
{
    int n = (cpu_features_len > maxlen) ? maxlen : cpu_features_len;
    memcpy(buf, cpu_features, n);
    return n;
}

int epx_cpu_cache_line_size()
{
    return cpu_cache_line_size;
}

int epx_simd_accel()
{
    int accel = 0;

    accel |= EPX_SIMD_EMU;
#if defined(__ppc__) || defined(__ppc64__)
#if defined(__VEC__) && defined(__ALTIVEC__)
    accel |= EPX_SIMD_ALTIVEC;
#endif
#endif
#if defined(__MMX__) && defined(USE_MMX)
    accel |= EPX_SIMD_MMX;
#endif
#if defined(__SSE2__) && defined(USE_SSE2)
    accel |= EPX_SIMD_SSE2;
#endif
#if defined(__NEON__) && defined(USE_NEON)
    accel |= EPX_SIMD_NEON;
#endif
    return accel;
}

#if defined(__i386__) || defined(__x86_64__)

static char* cpuid_feature_name[32] =
{
    "fpu",  "vme",   "de",    "pse",
    "tsc",  "msr",   "pae",   "mce",
    "cx8",  "apic",  "b10",   "sep",
    "mtrr", "pge",   "mca",   "cmov",
    "pat",  "pse36", "psn",   "clfsh",
    "b20",  "ds",    "acpi",  "mmx",
    "fxsr", "sse",   "sse2",  "ss", 
    "htt",  "tm",    "ia64",  "pbe" 
};

static void cpuid(int f, int *eax, int *ebx, int* ecx, int* edx)
{
    // FIXME add check if cpuid instruction is available,
    //  modern cpu's should have it so it's not very important right now.
    asm volatile ("mov %%ebx, %%esi\n\t" /* Save %ebx.  */
		  "cpuid\n\t"
		  "xchgl %%ebx, %%esi" /* Restore %ebx.  */
		  : "=a" (*eax), "=S" (*ebx), "=c" (*ecx), "=d" (*edx)
		  : "0" (f)
		  : "memory");
}

/* static void cpuid2(int f1, int f2, int *eax, int *ebx, int* ecx, int* edx) */
/* { */
/*     asm volatile ("mov %%ebx, %%esi\n\t" /\* Save %ebx.  *\/ */
/* 		  "cpuid\n\t" */
/* 		  "xchgl %%ebx, %%esi" /\* Restore %ebx.  *\/ */
/* 		  : "=a" (*eax), "=S" (*ebx), "=c" (*ecx), "=d" (*edx) */
/* 		  : "0" (f1), "c" (f2) */
/* 		  : "memory"); */
/* } */

/* static int cpuidMaxInputValue() */
/* { */
/*     int a,b,c,d; */
/*     cpuid(0,&a,&b,&c,&d); */
/*     return a; */
/* } */


// name must be at least 13 chars long 
static char* cpuidVendorName(char* name)
{
    int a,b,c,d;

    cpuid(0,&a,&b,&c,&d);

    *((int*)&name[0]) = b;
    *((int*)&name[4]) = d;
    *((int*)&name[8]) = c;
    name[12] = '\0';
    return name;
}

static int cpuidFeature()
{
    int a, b, c, d;

    cpuid(1, &a, &b, &c, &d);
    return d;
}

// Serial number is 12 bytes 
static int cpuidSerial(unsigned char* serial)
{
    int a, b, c, d;
    int i;
    cpuid(1, &a, &b, &c, &d);
    for (i = 0; i < 3; i++) {
	*serial++ = (a >> 24);
	a <<= 8;
    }
    cpuid(3, &a, &b, &c, &d);
    for (i = 0; i < 3; i++) {
	*serial++ = (d >> 24);
	d <<= 8;
    }
    for (i = 0; i < 3; i++) {
	*serial++ = (c >> 24);
	c <<= 8;
    }
    return 12;
} 

/* static int multiCoresPerProcPak() */
/* { */
/*     int a, b, c, d; */
/*     cpuid2(4,0, &a,&b,&c,&d); */
/*     return ((a & CPUID_CORES_PER_PROCPAK) >> 26) + 1; */
/* } */

/* static int getApicID() */
/* { */
/*     int a, b, c, d; */

/*     cpuid(1, &a, &b, &c, &d); */
/*     return (b & CPUID_LOCAL_APIC_ID) >> 24; */
/* } */

static int cpuidCacheLineSize()
{
    int a, b, c, d;

    cpuid(1, &a, &b, &c, &d);
    return ((b & CPUID_CLFUSH_SIZE) >> 8) << 3;
}
#endif


void epx_simd_init(int accel)
{
    int feature = 0;


#if defined(__i386__) || defined(__x86_64__)
    char* name;
    char* hex = "0123456789ABCDEF";
    int i;
    int j;

    name = cpuidVendorName(cpu_vendor_name);
    cpu_vendor_name_len = strlen(name);
    EPX_DBGFMT("cpu: %s\r\n", name);

    EPX_DBGFMT("Features:");
    feature = cpuidFeature();
    j = 0;
    for (i = 0; i < 32; i++) {
	if ((1 << i) & feature) {
	    size_t len = strlen(cpuid_feature_name[i]);
	    // EPX_DBGFMT("  %s", cpuid_feature_name[i]);
	    if (j+len+1 < (int)sizeof(cpu_features)) {
		memcpy(&cpu_features[j], cpuid_feature_name[i], len);
		cpu_features[j+len] = ',';
		j++;
		j += len;
	    }
	}
    }
    if (j > 0) {
	j--;
	cpu_features[j] = '\0';
    }
    cpu_features_len = j;

    cpu_cache_line_size = cpuidCacheLineSize();
    EPX_DBGFMT("cache_line_size: %d", cpu_cache_line_size);

    if (feature & CPUID_PSN) {
	char xserial[25];
	cpuidSerial(cpu_serial_number);
	cpu_serial_number_len = 12;

	for (i = 0; i < 12; i++) {
	    xserial[2*i] = hex[(cpu_serial_number[i] >> 4)&0xf];
	    xserial[2*i+1] = hex[cpu_serial_number[i] & 0xf];
	}
	xserial[24] = 0;
	EPX_DBGFMT("Serial: %s", xserial);
    }
    else {
	memset(cpu_serial_number, 0, sizeof(cpu_serial_number));
	cpu_serial_number_len = 0;
	EPX_DBGFMT("Serial: Not available");
    }
#endif

    epx_simd = NULL;

    if (accel == EPX_SIMD_NONE) {
	EPX_DBGFMT("SIMD: disabled");
	return;
    }

    if ((accel & EPX_SIMD_EMU) || (accel==EPX_SIMD_AUTO)) {
	EPX_DBGFMT("SIMD: Enable emu");
	epx_simd = &simd_emu;
    }

#if defined(__ppc__) || defined(__ppc64__)
#if defined(__VEC__) && defined(__ALTIVEC__)
    if ((accel & EPX_SIMD_ALTIVEC)) {
	EPX_DBGFMT("SIMD: Enable altivec");
	epx_simd = &simd_altivec;
    }
#endif
#endif

#if defined(__MMX__) && defined(USE_MMX)
    if ((feature & CPUID_MMX) &&
	((accel==EPX_SIMD_AUTO) || (accel & EPX_SIMD_MMX))) {
	EPX_DBGFMT("SIMD: Enable mmx");
	epx_simd = &simd_mmx;
    }
#endif


#if defined(__SSE2__) && defined(USE_SSE2)
    if ((feature & CPUID_SSE2) &&
	((accel==EPX_SIMD_AUTO) || (accel & EPX_SIMD_SSE2))) {
	EPX_DBGFMT("SIMD: Enable sse2");
	epx_simd = &simd_sse2;
    }
#endif

}
