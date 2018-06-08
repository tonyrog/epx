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

#include "epx_cpuid.h"
#include "../include/epx_debug.h"
#include "../include/epx_simd.h"

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

#if defined(__NEON__)
EXTERN_SIMD_API(_neon);
static epx_simd_t simd_neon = INIT_SIMD_API(EPX_SIMD_NEON,_neon);
#endif

#if defined(__MMX__)
EXTERN_SIMD_API(_mmx);
static epx_simd_t simd_mmx = INIT_SIMD_API(EPX_SIMD_MMX,_mmx);
#endif

#if defined(__SSE2__)
EXTERN_SIMD_API(_sse2);
static epx_simd_t simd_sse2 = INIT_SIMD_API(EPX_SIMD_SSE2,_sse2);
#endif

#if defined(__AVX2__)
EXTERN_SIMD_API(_avx2);
static epx_simd_t simd_avx2 = INIT_SIMD_API(EPX_SIMD_AVX2,_avx2);
#endif

// SIMD function block pointer
epx_simd_t* epx_simd = NULL;

static unsigned char   cpu_serial_number[64];
static size_t          cpu_serial_number_len = 0;
static char            cpu_vendor_name[64];
static size_t          cpu_vendor_name_len = 0;
static char            cpu_brand_string[64];
static size_t          cpu_brand_string_len = 0;
static char            cpu_features[1024];
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

int epx_cpu_brand_string(char* buf, size_t maxlen)
{
    int n = (cpu_brand_string_len > maxlen) ? maxlen : cpu_brand_string_len;
    memcpy(buf, cpu_brand_string, n);
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


#if defined(__i386__) || defined(__x86_64__)
//
// http://en.wikipedia.org/wiki/CPUID
//
static char* cpuid_feature_name_dx[32] =
{
    "fpu",    // 0  - Floating-point Unit On-Chip
    "vme",    // 1  - Virtual Mode Extension
    "de",     // 2  - Debugging Extension
    "pse",    // 3  - Page Size Extension
    "tsc",    // 4  - Time Stamp Counter
    "msr",    // 5  - Model Specific Registers
    "pae",    // 6  - Physical Address Extension
    "mce",    // 7  - Machine-Check Exception
    "cx8",    // 8  - CMPXCHG8 Instruction
    "apic",   // 9  - On-chip APIC Hardware
    "_",      // 10 - Reserved
    "sep",    // 11 - Fast System Call
    "mtrr",   // 12 - Memory Type Range Registers
    "pge",    // 13 - Page Global Enable
    "mca",    // 14 - Machine-Check Architecture
    "cmov",   // 15 - Conditional Move Instruction
    "pat",    // 16 - Page Attribute Table
    "pse36",  // 17 - 36-bit Page Size Extension
    "psn",    // 18 - Processor serial number is present and enabled
    "clfsh",  // 19 - CLFLUSH Instruction
    "_",      // 20 - Reserved
    "ds",     // 21 - Debug Store
    "acpi",   // 22 - Thermal Monitor and Software Controlled Clock Facilities
    "mmx",    // 23 - MMX technology
    "fxsr",   // 24 - FXSAVE and FXSTOR Instructions
    "sse",    // 25 - Streaming SIMD Extensions
    "sse2",   // 26 - Streaming SIMD Extensions 2
    "ss",     // 27 - Self-Snoop
    "ht",     // 28 - Multi-Threading
    "tm",     // 29 - Thermal Monitor
    "ia64",   // 30 - Reserved ? 64-bit
    "pbe"     // 31 - Pending Break Enable
};

// uses short name sse3 instead of pni !
static char* cpuid_feature_name_cx[32] =
{
    "sse3",         // 0  - Streaming SIMD Extensions 3
    "pclmulqdq",    // 1  - PCLMULDQ Instruction
    "dtes64",       // 2  - 64-Bit Debug Store
    "monitor",      // 3  - MONITOR/MWAIT
    "ds_cpl",       // 4  - CPL Qualified Debug Store
    "vmx",          // 5  - Virtual Machine Extensions
    "smx",          // 6  - Safer Mode Extensions
    "est",          // 7  - Enhanced Intel SpeedStep® Technology
    "tm2",          // 8  - Thermal Monitor 2
    "ssse3",        // 9  - Supplemental Streaming SIMD Extensions 3
    "cid",          // 10 - L1 Context ID
    "_",            // 11 - Reserved
    "fma",          // 12 - Fused Multiply Add
    "cx16",         // 13 - CMPXCHG16B
    "xtpr",         // 14 - xTPR Update Control
    "pdcm",         // 15 - Perfmon and Debug Capability
    "_",            // 16 - Reserved
    "pcid",         // 17 - Process Context Identifiers
    "dca",          // 18 - Direct Cache Access
    "sse41",        // 19 - Streaming SIMD Extensions 4.1
    "sse42",        // 20 - Streaming SIMD Extensions 4.2
    "x2apic",       // 21 - Extended xAPIC Support
    "movbe",        // 22 - MOVBE Instruction
    "popcnt",       // 23 - POPCNT Instruction
    "tscdeadline",  // 24 - Time Stamp Counter Deadline
    "aes",          // 25 - AES Instruction Extensions
    "xsave",        // 26 - XSAVE/XSTOR States
    "osxsave",      // 27 - OS-Enabled Extended State Management
    "avx",          // 28 - Advanced Vector Extensions
    "f16c",         // 29 - 16-bit floating-point conversion instructions
    "rdrnd",        // 30 - RDRAND instruction supported
    "hypervisor"    // 31 - (Not used in Intel doc...)
};

// EAX=0x80000001, EDX
static char* cpuid_amd_feature_name_dx[32] =
{
    "fpu",    // 0  - Floating-point Unit On-Chip
    "vme",    // 1  - Virtual Mode Extension
    "de",     // 2  - Debugging Extension
    "pse",    // 3  - Page Size Extension
    "tsc",    // 4  - Time Stamp Counter
    "msr",    // 5  - Model Specific Registers
    "pae",    // 6  - Physical Address Extension
    "mce",    // 7  - Machine-Check Exception
    "cx8",    // 8  - CMPXCHG8 Instruction
    "apic",   // 9  - On-chip APIC Hardware
    "_",      // 10 - Reserved
    "sep",    // 11 - Fast System Call (SYSCALL/SYSRET)
    "mtrr",   // 12 - Memory Type Range Registers
    "pge",    // 13 - Page Global Enable
    "mca",    // 14 - Machine-Check Architecture
    "cmov",   // 15 - Conditional Move Instruction
    "pat",    // 16 - Page Attribute Table
    "pse36",  // 17 - 36-bit Page Size Extension
    "_",      // 18 -
    "mp",     // 19 - Multiprocessor Capable
    "nx",     // 20 - NX bit
    "_",      // 21 - reserved
    "mmxext", // 22 - Extended MMX
    "mmx",    // 23 - MMX instructions
    "fxsr",   // 24 - FXSAVE and FXSTOR Instructions
    "fxsr_opt",// 25 - FXSAVE/FXSTOR optimizations
    "pdpelgb",  // 26 - Gibibyte pages
    "rdtscp",   // 27 - RDTSCP instruction
    "_",        // 28 -
    "lm",       // 29 - Long mode
    "3dnowext", // 30 - Extended 3DNow!
    "3dnow"     // 31 - 3DNow!
};

// EAX=0x80000001, ECX
static char* cpuid_amd_feature_name_cx[32] =
{
    "lahf_lm",      // 0  -
    "cmp_legacy",   // 1  -
    "svm",          // 2  -
    "extapic",      // 3  -
    "cr8_legacy",   // 4  -
    "abm",          // 5  -
    "sse4a",        // 6  -
    "misalignsse",  // 7  -
    "3dnowprefetch",// 8  -
    "osvw",         // 9  -
    "ibs",          // 10 -
    "xop",          // 11 -
    "skinit",       // 12 -
    "wdt",          // 13 -
    "_",            // 14 -
    "lwp",          // 15 -
    "fma4",         // 16 -
    "tce",          // 17 -
    "_",            // 18 -
    "nodeid_msr",   // 19 -
    "_",            // 20 -
    "tbm",          // 21 -
    "topoext",      // 22 -
    "perfctr_core", // 23 -
    "perfctr_nb",   // 24 -
    "_",            // 25 -
    "dbx",          // 26 -
    "perftsc",      // 27 -
    "pcx_l2i",      // 28 -
    "_",            // 29 -
    "_",            // 30 -
    "_"             // 31 -
};


// EAX=7, EXC=0 Extended Features
static char* cpuid_ext_feature_name_bx[32] =
{
    "fsgsbase",      // 0 - Access to base of %fs and %gs
    "_",             // 1 - IA32_TSC_ADJUST
    "sgx",           // 2 - Software Guard Extensions
    "bmi1",          // 3 - Bit Manipulation Instruction Set 1
    "hle",           // 4 - Transactional Synchronization Extensions
    "avx2",          // 5 - Advanced Vector Extensions 2
    "_",             // 6 - 
    "smep",          // 7 - Supervisor-Mode Execution Prevention
    "bmi2",          // 8 - Bit Manipulation Instruction Set 2
    "erms",          // 9 - Enhanced REP MOVSB/STOSB
    "invpcid",       // 10 - INVPCID instruction
    "rtm",           // 11 - Transactional Synchronization Extensions
    "pqm",           // 12 - Platform Quality of Service Monitoring
    "_",             // 13 - 
    "mpx",           // 14 - Intel MPX (Memory Protection Extensions)
    "pqe",           // 15 - Platform Quality of Service Enforcement
    "avx512f",       // 16 - AVX-512 Foundation
    "avx512dq",      // 17 - AVX-512 Doubleword and Quadword Instructions
    "rdseed",        // 18 - RDSEED instruction
    "adx",           // 19 - Multi-Precision Add-Carry Instruction Extensions
    "smap",          // 20 - Supervisor Mode Access Prevention
    "avx512ifma",    // 21 - AVX-512 Integer Fused Multiply-Add Instructions
    "pcommit",       // 22  -PCOMMIT instruction
    "clflushopt",    // 23 - CLFLUSHOPT instruction
    "clwb",          // 24 - CLWB instruction
    "intel_pt",      // 25 - Intel Processor Trace
    "avx512pf",      // 26 - AVX-512 Prefetch Instructions
    "avx512er",      // 27 - AVX-512 Exponential and Reciprocal Instructions
    "avx512cd",      // 28 - AVX-512 Conflict Detection Instructions
    "sha",           // 29 - Intel SHA extensions
    "avx512bw",      // 30 - AVX-512 Byte and Word Instructions
    "avx512vl"       // 31 - AVX-512 Vector Length Extensions
};

static char* cpuid_ext_feature_name_cx[32] =
{
    "prefetchwt1",      // 0 - PREFETCHWT1 instruction
    "avx512vbmi",       // 1 - AVX-512 Vector Bit Manipulation Instructions
    "umip",             // 2 - User-mode Instruction Prevention
    "pku",              // 3 - Memory Protection Keys for User-mode pages
    "ospke",            // 4 - PKU enabled by OS
    "_",                // 5 - 
    "avx512vbmi2",      // 6 - AVX-512 Vector Bit Manipulation Instructions 2
    "_",                // 7 - 
    "gfni",             // 8 - Galois Field instructions
    "vaes",             // 9 - AES instruction set (VEX-256/EVEX)
    "vpclmulqdq",       // 10 - CLMUL instruction set (VEX-256/EVEX)
    "avx512vnni",       // 11 - AVX-512 Vector Neural Network Instructions
    "avx512bitalg",     // 12 - AVX-512 BITALG instructions
    "_",                // 13 - 
    "avx512vpopcntdq",  // 14 - AVX-512 Vector Population Count D/Q
    "_",                // 15 - 
    "_",                // 16 - 
    "mawau0",           // 17 - 
    "mawau1",           // 18 - 
    "mawau2",           // 19 - 
    "mawau3",           // 20 - 
    "mawau4",           // 21 - 
    "rdpid",            // 22 - Read Processor ID 
    "_",                // 23 - 
    "_",                // 24 - 
    "_",      // 25 - 
    "_",      // 26 - 
    "_",      // 27 - 
    "_",      // 28 - 
    "_",       // 29 - 
    "sgx_lc",      // 30 - SGX Launch Configuration
    "_"       // 31 - 
};

static char* cpuid_ext_feature_name_dx[32] =
{
    "_",              // 0 -
    "_",              // 1 -
    "avx512_4vnniw",  // 2 - AVX-512 4-register Neural Network Instructions
    "avx512_4fmaps",  // 3 - AVX-512 4-register Multiply Accumulation Single
    "",               // 4 -
    "_",              // 5 - 
    "_",              // 6 - 
    "_",              // 7 - 
    "",               // 8 - 
    "",               // 9 - 
    "",       // 10 - 
    "",       // 11 - 
    "",       // 12 - 
    "",       // 13 - 
    "",       // 14 - 
    "_",      // 15 - 
    "_",      // 16 - 
    "",       // 17 - 
    "",       // 18 - 
    "",       // 19 - 
    "",       // 20 - 
    "",       // 21 - 
    "",       // 22 -
    "_",      // 23 - 
    "_",      // 24 - 
    "_",      // 25 - 
    "_",      // 26 - 
    "_",      // 27 - 
    "_",      // 28 - 
    "_",       // 29 - 
    "_",      // 30 - 
    "_"       // 31 - 
};

static void cpuid(uint32_t f,
		  uint32_t *eax, uint32_t *ebx,
		  uint32_t* ecx, uint32_t* edx)
{
  __asm__ __volatile__(
#if defined(__x86_64__) || defined(_M_AMD64) || defined (_M_X64)
    "pushq %%rbx     \n\t"    /* save %rbx */
    "xorq  %%rcx,%%rcx \n\t"  /* clear %rcx */
#else
    "pushl %%ebx       \n\t" /* save %ebx */
    "xorl  %%ecx,%%ecx \n\t" /* clear %ecx */
#endif
    "cpuid            \n\t"
    "movl %%ebx ,%[ebx]  \n\t" /* write the result into output var */
#if defined(__x86_64__) || defined(_M_AMD64) || defined (_M_X64)
    "popq %%rbx \n\t"
#else
    "popl %%ebx \n\t"
#endif
    : "=a"(*eax), [ebx] "=r"(*ebx), "=c"(*ecx), "=d"(*edx)
    : "a"(f));
}

// name must be at least 13 chars long
static char* cpuid_vendor_name(char* name)
{
    uint32_t a,b,c,d;

    cpuid(0,&a,&b,&c,&d);

    *((uint32_t*)&name[0]) = b;
    *((uint32_t*)&name[4]) = d;
    *((uint32_t*)&name[8]) = c;
    name[12] = '\0';
    return name;
}

static int cpuid_feature(uint32_t* ecx, uint32_t* edx)
{
    uint32_t a, b, c, d;

    cpuid(1, &a, &b, &c, &d);
    *ecx = c;
    *edx = d;
    return 0;
}

static int cpuid_ext_feature(uint32_t* ebx, uint32_t* ecx, uint32_t* edx)
{
    uint32_t a, b, c=0, d;

    cpuid(7, &a, &b, &c, &d);
    *ebx = b;
    *ecx = c;
    *edx = d;
    return 0;
}

static int cpuid_get_highfunsup(uint32_t* eax)
{
    uint32_t a, b, c, d;

    cpuid(0x80000000, &a, &b, &c, &d);
    *eax = a;
    return 0;
}

static int cpuid_amd_feature(uint32_t* ecx, uint32_t* edx)
{
    uint32_t a, b, c, d;

    cpuid(0x80000001, &a, &b, &c, &d);
    *ecx = c;
    *edx = d;
    return 0;
}

// name must be at least 13 chars long 
static char* cpuid_amd_brand_string(char* name)
{
    uint32_t a,b,c,d;

    cpuid(0x80000002,&a,&b,&c,&d);
    *((uint32_t*)&name[0])  = a;
    *((uint32_t*)&name[4])  = b;
    *((uint32_t*)&name[8])  = c;
    *((uint32_t*)&name[12]) = d;

    cpuid(0x80000003,&a,&b,&c,&d);
    *((uint32_t*)&name[16]) = a;
    *((uint32_t*)&name[20]) = b;
    *((uint32_t*)&name[24]) = c;
    *((uint32_t*)&name[28]) = d;

    cpuid(0x80000004,&a,&b,&c,&d);
    *((uint32_t*)&name[32]) = a;
    *((uint32_t*)&name[36]) = b;
    *((uint32_t*)&name[40]) = c;
    *((uint32_t*)&name[44]) = d;
    name[48] = '\0';
    return name;
}

// name must be at least 17 characters long
#if 0
static char* cpuid_amd_easteregg(char* name)
{
    uint32_t a,b,c,d;

    cpuid(0x8fffffff,&a,&b,&c,&d);
    *((uint32_t*)&name[0])  = a;
    *((uint32_t*)&name[4])  = b;
    *((uint32_t*)&name[8])  = c;
    *((uint32_t*)&name[12]) = d;
    name[16] = '\0';
    return name;
}
#endif

// Serial number is 12 bytes 
static int cpuid_serial(unsigned char* serial)
{
    uint32_t a, b, c, d;
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

static int cpuid_cache_line_size()
{
    uint32_t a, b, c, d;

    cpuid(1, &a, &b, &c, &d);
    return ((b & CPUID_CLFUSH_SIZE) >> 8) << 3;
}

#endif

static int copy_features(int pos, uint32_t feature, char** feature_name)
{
    int i;
    
    for (i = 0; i < 32; i++) {
	if ((1 << i) & feature) {
	    size_t len = strlen(feature_name[i]);
	    if ((len>1) && (pos+len+1 < (int)sizeof(cpu_features))) {
		memcpy(&cpu_features[pos], feature_name[i], len);
		cpu_features[pos+len] = ',';
		pos++;
		pos += len;
	    }
	}
    }
    return pos;
}

int epx_simd_accel()
{
    int accel = 0;
    uint32_t feature_cx = 0;
    uint32_t feature_dx = 0;
    uint32_t ext_feature_bx = 0;
    uint32_t ext_feature_cx = 0;
    uint32_t ext_feature_dx = 0;

    accel |= EPX_SIMD_EMU;

#if defined(__i386__) || defined(__x86_64__)
    cpuid_feature(&feature_cx, &feature_dx);
    cpuid_ext_feature(&ext_feature_bx, &ext_feature_cx, &ext_feature_dx);
#endif
    
#if defined(__ppc__) || defined(__ppc64__)
#if defined(__VEC__) && defined(__ALTIVEC__)
    accel |= EPX_SIMD_ALTIVEC;
#endif
#endif
#if defined(__MMX__)
    if (feature_dx & CPUID_MMX)
	accel |= EPX_SIMD_MMX;
#endif
#if defined(__SSE2__)
    if (feature_dx & CPUID_SSE2)
	accel |= EPX_SIMD_SSE2;
#endif
#if defined(__AVX2__)
    if (ext_feature_bx & CPUID_AVX2)
	accel |= EPX_SIMD_AVX2;
#endif
#if defined(__NEON__)
    accel |= EPX_SIMD_NEON;
#endif
    return accel;
}

void epx_simd_init(int accel)
{
    uint32_t feature_cx = 0;
    uint32_t feature_dx = 0;
    uint32_t amd_feature_cx = 0;
    uint32_t amd_feature_dx = 0;
    uint32_t ext_feature_bx = 0;
    uint32_t ext_feature_cx = 0;
    uint32_t ext_feature_dx = 0;

#if defined(__i386__) || defined(__x86_64__)
    char* ptr;
    int j;
    uint32_t highfun;

    ptr = cpuid_vendor_name(cpu_vendor_name);
    cpu_vendor_name_len = strlen(ptr);
    DEBUGF("cpu: %s\r\n", ptr);

    cpuid_get_highfunsup(&highfun);
    if (highfun >= 0x80000004) {
	ptr = cpuid_amd_brand_string(cpu_brand_string);
	cpu_brand_string_len = strlen(ptr);
    }
    else {
	ptr = "N/A";
	cpu_brand_string[0] = '\0';
	cpu_brand_string_len = 0;
    }
    DEBUGF("brand: %s\r\n", ptr);

    // cpuid_amd_easteregg(easter);

    DEBUGF("Features:");
    cpuid_feature(&feature_cx, &feature_dx); 
    cpuid_ext_feature(&ext_feature_bx, &ext_feature_cx, &ext_feature_dx);
    
    j = 0;
    // first the old dx flags
    j = copy_features(j, feature_dx, cpuid_feature_name_dx);
    j = copy_features(j, feature_cx, cpuid_feature_name_cx);
    j = copy_features(j, ext_feature_bx, cpuid_ext_feature_name_bx);
    j = copy_features(j, ext_feature_cx, cpuid_ext_feature_name_cx);
    j = copy_features(j, ext_feature_dx, cpuid_ext_feature_name_dx);
    if (highfun >= 0x80000001) {
	cpuid_amd_feature(&amd_feature_cx, &amd_feature_dx);
	j = copy_features(j, amd_feature_dx, cpuid_amd_feature_name_dx);
	j = copy_features(j, amd_feature_cx, cpuid_amd_feature_name_cx);
    }
    if (j > 0) {
	j--;
	cpu_features[j] = '\0';
    }
    cpu_features_len = j;

    cpu_cache_line_size = cpuid_cache_line_size();
    DEBUGF("cache_line_size: %d", cpu_cache_line_size);

    if (feature_dx & CPUID_PSN) {
	cpuid_serial(cpu_serial_number);
	cpu_serial_number_len = 12;

	DEBUGF("Serial: %02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x",
	       cpu_serial_number[0],cpu_serial_number[1],
	       cpu_serial_number[2],cpu_serial_number[3],
	       cpu_serial_number[4],cpu_serial_number[5],
	       cpu_serial_number[6],cpu_serial_number[7],
	       cpu_serial_number[8],cpu_serial_number[9],
	       cpu_serial_number[10],cpu_serial_number[11]);
    }
    else {
	memset(cpu_serial_number, 0, sizeof(cpu_serial_number));
	cpu_serial_number_len = 0;
	DEBUGF("Serial: Not available");
    }
#endif

    epx_simd = NULL;

    if (accel == EPX_SIMD_NONE) {
	DEBUGF("SIMD: disabled");
	return;
    }

    if ((accel & EPX_SIMD_EMU) || (accel==EPX_SIMD_AUTO)) {
	DEBUGF("SIMD: Enable emu");
	epx_simd = &simd_emu;
    }

#if defined(__ppc__) || defined(__ppc64__)
#if defined(__VEC__) && defined(__ALTIVEC__)
    if ((accel & EPX_SIMD_ALTIVEC) || (accel==EPX_SIMD_AUTO)) {
	DEBUGF("SIMD: Enable altivec");
	epx_simd = &simd_altivec;
    }
#endif
#endif

#if defined(__arm__)
#if defined(__NEON__)
    if ((accel & EPX_SIMD_NEON) || (accel==EPX_SIMD_AUTO)) {
	DEBUGF("SIMD: Enable neon");
	epx_simd = &simd_neon;
    }
#endif
#endif

#if defined(__MMX__)
    if ((feature_dx & CPUID_MMX) &&
	((accel==EPX_SIMD_AUTO) || (accel & EPX_SIMD_MMX))) {
	DEBUGF("SIMD: Enable mmx");
	epx_simd = &simd_mmx;
    }
#endif

#if defined(__SSE2__)
    if ((feature_dx & CPUID_SSE2) &&
	((accel==EPX_SIMD_AUTO) || (accel & EPX_SIMD_SSE2))) {
	DEBUGF("SIMD: Enable sse2");
	epx_simd = &simd_sse2;
    }
#endif

#if defined(__AVX2__)
    if ((ext_feature_bx & CPUID_AVX2) &&
	((accel==EPX_SIMD_AUTO) || (accel & EPX_SIMD_AVX2))) {
	DEBUGF("SIMD: Enable avx2");
	epx_simd = &simd_avx2;
    }
#endif    

}
