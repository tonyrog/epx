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
// CPUID x86 
//
#ifndef __EPX_CPUID_H__
#define __EPX_CPUID_H__

/* CPUID FUNCTION 1, edx info */
#define	CPUID_FPU	0x00000001
#define	CPUID_VME	0x00000002
#define	CPUID_DE	0x00000004
#define	CPUID_PSE	0x00000008
#define	CPUID_TSC	0x00000010
#define	CPUID_MSR	0x00000020
#define	CPUID_PAE	0x00000040
#define	CPUID_MCE	0x00000080
#define	CPUID_CX8	0x00000100
#define	CPUID_APIC	0x00000200
#define	CPUID_B10	0x00000400
#define	CPUID_SEP	0x00000800
#define	CPUID_MTRR	0x00001000
#define	CPUID_PGE	0x00002000
#define	CPUID_MCA	0x00004000
#define	CPUID_CMOV	0x00008000
#define	CPUID_PAT	0x00010000
#define	CPUID_PSE36	0x00020000
#define	CPUID_PSN	0x00040000
#define	CPUID_CLFSH	0x00080000
#define	CPUID_B20	0x00100000
#define	CPUID_DS	0x00200000
#define	CPUID_ACPI	0x00400000
#define	CPUID_MMX	0x00800000
#define	CPUID_FXSR	0x01000000
#define	CPUID_SSE	0x02000000
#define	CPUID_XMM	0x02000000
#define	CPUID_SSE2	0x04000000
#define	CPUID_SS	0x08000000
#define	CPUID_HTT	0x10000000  // Hyperhreading support
#define	CPUID_TM	0x20000000
#define	CPUID_IA64	0x40000000
#define	CPUID_PBE	0x80000000

/* CPUID FUNCTION 1, ecx info */
#define	CPUID_SSE3	  0x00000001
#define CPUID_PCLMULQDQ   0x00000002 // PCLMULQDQ support
#define CPUID_DTES64 	  0x00000004 // 64-bit debug store (edx bit 21)
#define	CPUID_SSSE3	  0x00000200
#define CPUID_SSE4_1      0x00080000
#define CPUID_SSE4_2      0x00100000
#define CPUID_POPCNT      0x00800000 // POPCNT instruction
#define CPUID_TSCDEADLINE 0x01000000
#define CPUID_AES         0x02000000 // AES instruction set
#define CPUID_XSAVE       0x04000000
#define CPUID_OSXSAVE     0x08000000
#define CPUID_AVX         0x10000000 // Advanced Vector Extensions

/* CPUID FUNCTION 1 ebx info */
#define	CPUID_BRAND_INDEX	0x000000ff
#define	CPUID_CLFUSH_SIZE	0x0000ff00
#define	CPUID_HTT_CORES		0x00ff0000
#define	CPUID_LOCAL_APIC_ID	0xff000000

/* CPUID FUNCTION 4 eax info */
#define CPUID_CORES_PER_PROCPAK 0xFC000000

/* FUNCTION 4,0 edx? */
#define	CPUID2_SSE3	0x00000001
#define	CPUID2_MON	0x00000008
#define	CPUID2_DS_CPL	0x00000010
#define	CPUID2_VMX	0x00000020
#define	CPUID2_EST	0x00000080
#define	CPUID2_TM2	0x00000100
#define	CPUID2_SSSE3	0x00000200
#define	CPUID2_CNXTID	0x00000400
#define	CPUID2_CX16	0x00002000
#define	CPUID2_XTPR	0x00004000

/* FUNCTION 7, ecx=0 */
/* EBX */
#define CPUID_FSGSBASE   0x00000001
#define CPUID_SGX        0x00000004
#define CPUID_BMI1       0x00000008
#define CPUID_HLE        0x00000010
#define CPUID_AVX2       0x00000020 // Advanced Vector Extensions 2
#define CPUID_SMEP       0x00000080
#define CPUID_BMI2       0x00000100
#define CPUID_ERMS       0x00000200
#define CPUID_INVPCID    0x00000400
#define CPUID_RTM        0x00000800
#define CPUID_PQM        0x00001000
#define CPUID_MPX        0x00004000
#define CPUID_PQE        0x00008000
#define CPUID_AVX512F    0x00010000 // AVX-512 Foundation
#define CPUID_AVX512DQ   0x00020000 // AVX-512 Doubleword and Quadword
#define CPUID_RDSEED     0x00040000
#define CPUID_ADX        0x00080000
#define CPUID_SMAP       0x00100000
#define CPUID_AVX512IFMA 0x00200000 // AVX-512 Integer Fused Multiply-Add
#define CPUID_PCOMMIT    0x00400000
#define CPUID_CLFLUSHOPT 0x00800000
#define CPUID_CLWB       0x01000000
#define CPUID_INTEL_PT   0x02000000
#define CPUID_AVX512PF   0x04000000 // AVX-512 Prefetch
#define CPUID_AVX512ER   0x08000000 // AVX-512 Exponential and Reciprocal 
#define CPUID_AVX512CD   0x10000000 // AVX-512 Conflict Detection
#define CPUID_SHA        0x20000000 // Intel SHA extensions
#define CPUID_AVX512BW   0x40000000 // AVX-512 Byte and Word
#define CPUID_AVX512VL   0x80000000 // AVX-512 Vector Length Extensions

/* FUNCTION 7, ecx=0 */
/* ECX */
#define CPUID_PREFETCHWT1     0x00000001 // PREFETCHWT1 instruction
#define CPUID_AVX512VBMI      0x00000002 // AVX-512 Vector Bit Manipulation
#define CPUID_UMIP            0x00000004 // User-mode Instruction Prevention
#define CPUID_PKU             0x00000008 // Memory Protection Keys for User-mode pages
#define CPUID_OSPKE           0x00000010 // PKU enabled by OS

#define CPUID_AVX512VBMI2     0x00000040 // AVX-512 Vector Bit Manipulation 2
#define CPUID_GFNI 	      0x00000100 // Galois Field instructions
#define CPUID_VAES 	      0x00000200 // AES instruction set (VEX-256/EVEX)
#define CPUID_VPCLMULQDQ      0x00000400 // CLMUL instruction set (VEX-256/EVEX)
#define CPUID_AVX512VNNI      0x00000800 // AVX-512 Vector Neural Network
#define CPUID_AVX512BITALG    0x00001000 // AVX-512 BITALG instructions
#define CPUID_AVX512VPOPCNTDQ 0x00004000 // AVX-512 Vector Population Count D/Q

#define CPUID_RDPID 	      0x00400000 // Read Processor ID
#define CPUID_SGX_LC 	      0x40000000 // SGX Launch Configuration

/* FUNCTION 7, ecx=0 */
/* EDX */
#define CPUID_AVX512_4VNNIW   0x00000004 // AVX-512 4-register Neural Network Instructions
#define CPUID_AVX512_4FMAPS   0x00000008 // AVX-512 4-register Multiply Accumulation Single precision


#define	AMDID_SYSCALL	0x00000800
#define	AMDID_MP	0x00080000
#define	AMDID_NX	0x00100000
#define	AMDID_EXT_MMX	0x00400000
#define	AMDID_FFXSR	0x01000000
#define	AMDID_RDTSCP	0x08000000
#define	AMDID_LM	0x20000000
#define	AMDID_EXT_3DNOW	0x40000000
#define	AMDID_3DNOW	0x80000000

#define	AMDID2_LAHF	0x00000001
#define	AMDID2_CMP	0x00000002
#define	AMDID2_SVM	0x00000004
#define	AMDID2_EXT_APIC	0x00000008
#define	AMDID2_CR8	0x00000010
#define	AMDID2_PREFETCH	0x00000100

/*
 * AMD extended function 8000_0008h ecx info
 */
#define	AMDID_CMP_CORES		0x000000ff


#define EXT_CPUID_3DNOW         0x80000000
#define EXT_CPUID_AMD_3DNOWEXT  0x40000000
#define EXT_CPUID_AMD_MMXEXT    0x00400000

#define FF_CPU_MMX            0x00000001
#define FF_CPU_MMXEXT         0x00000002
#define FF_CPU_SSE            0x00000004
#define FF_CPU_SSE2           0x00000008
#define FF_CPU_3DNOW          0x00000010
#define FF_CPU_3DNOWEXT       0x00000020
#define FF_CPU_TSC            0x00000040

#endif
