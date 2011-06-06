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

#if 0
#define EXTERN_SIMD_API(type) \
    extern epx_simd_copy_fn_t        epx_simd_copy##type;	       \
    extern epx_simd_fill_32_fn_t     epx_simd_fill_32##type;		\
    extern epx_simd_alpha_color_fn_t epx_simd_add_blend_area_rgba32##type; \
    extern epx_simd_alpha_color_fn_t epx_simd_add_blend_area_argb32##type; \
    extern epx_simd_alpha_color_fn_t epx_simd_add_blend_area_a8_rgba32##type; \
    extern epx_simd_alpha_color_fn_t epx_simd_add_blend_area_a8_argb32##type; \
    extern epx_simd_alpha_fn_t       epx_simd_alpha_area_argb32##type;	\
    extern epx_simd_alpha_fn_t       epx_simd_alpha_area_rgba32##type;	\
    extern epx_simd_fn_t             epx_simd_blend_area_rgba32##type;	\
    extern epx_simd_fn_t             epx_simd_blend_area_argb32##type;	\
    extern epx_simd_alpha_fn_t       epx_simd_fade_area_rgba32##type;	\
    extern epx_simd_alpha_fn_t       epx_simd_fade_area_argb32##type;	\
    extern epx_simd_fill_fn_t        epx_simd_fill_area_blend_rgb24##type; \
    extern epx_simd_fill_fn_t        epx_simd_fill_area_blend_argb32##type; \
    extern epx_simd_fill_fn_t        epx_simd_fill_area_blend_rgba32##type
#endif

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

#define INIT_SIMD_API(type) { \
    epx_simd_copy##type,			\
    epx_simd_fill_32##type,			\
    epx_simd_add_blend_area_rgba32##type,	\
    epx_simd_add_blend_area_argb32##type,	 \
    epx_simd_add_blend_area_a8_rgba32##type,	 \
    epx_simd_add_blend_area_a8_argb32##type,	 \
    epx_simd_alpha_area_argb32##type,		 \
    epx_simd_alpha_area_rgba32##type,		 \
    epx_simd_blend_area_rgba32##type,		 \
    epx_simd_blend_area_argb32##type,		 \
    epx_simd_fade_area_rgba32##type,		 \
    epx_simd_fade_area_argb32##type,		 \
    epx_simd_fill_area_blend_rgb24##type,	 \
    epx_simd_fill_area_blend_argb32##type,	 \
    epx_simd_fill_area_blend_rgba32##type }

EXTERN_SIMD_API(_emu);
static epx_simd_t simd_emu = INIT_SIMD_API(_emu);

#if defined(__VEC__) && defined(__ALTIVEC__)
EXTERN_SIMD_API(_altivec);
static epx_simd_t simd_altivec = INIT_SIMD_API(_altivec);
#endif

#if defined(__MMX__) && defined(USE_MMX)
EXTERN_SIMD_API(_mmx);
static epx_simd_t simd_mmx = INIT_SIMD_API(_mmx);
#endif

#if defined(__SSE2__) && defined(USE_SSE2)
EXTERN_SIMD_API(_sse2);
static epx_simd_t simd_sse2 = INIT_SIMD_API(_sse2);
#endif

// SIMD function block pointer
epx_simd_t* epx_simd;

static unsigned char   cpu_serial_number[64];
static size_t          cpu_serial_number_len = 0;
static char            cpu_vendor_name[64];
static size_t          cpu_vendor_name_len = 0;

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
    return cpu_vendor_name_len;
}

#if defined(__i386__) || defined(__x86_64__)

static char* cpuid_feature_name[32] =
{
    "FPU", "VME",   "DE",      "PSE",
    "TSC", "MSR",   "PAE",     "MCE",
    "CX8", "APIC",  "B10",     "SEP",
    "MTRR",  "PGE", "MCA",     "CMOV",
    "PAT",   "PSE36", "PSN",   "CLFSH",
    "B20",   "DS",    "ACPI",  "MMX",
    "FXSR",  "SSE",   "SSE2",  "SS", 
    "HTT",   "TM",    "IA64",  "PBE" 
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
    int cacheline = 64;
#if defined(__i386__) || defined(__x86_64__)
    char* hex = "0123456789ABCDEF";
    int i;

    feature = cpuidFeature();
    cacheline = cpuidCacheLineSize();

    EPX_DBGFMT("cpu: %s\r\n", cpuidVendorName(cpu_vendor_name));
    cpu_vendor_name_len = strlen(cpu_vendor_name);

    EPX_DBGFMT("Features:");
    for (i = 0; i < 32; i++) {
	if ((1 << i) & feature) {
	    // EPX_DBGFMT("  %s", cpuid_feature_name[i]);
	}
    }
    EPX_DBGFMT("cache_line_size: %d", cacheline);

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
    EPX_DBGFMT("SIMD: Enable emu");
    epx_simd = &simd_emu;
#if defined(__ppc__) || defined(__ppc64__)
#if defined(__VEC__) && defined(__ALTIVEC__)
    if (accel & EPX_SIMD_ALTIVEC) {
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
