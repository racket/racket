/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler for the arm
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 2011 Free Software Foundation
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 ***********************************************************************/

#ifndef __lightning_asm_h
#define __lightning_asm_h

typedef enum {
    _R0,	/* argument / result */
    _R1,	/* argument */
    _R2,	/* argument */
    _R3,	/* argument */
    _R4,	/* variable */
    _R5,	/* variable */
    _R6,	/* variable */
    _R7,	/* variable */
    _R8,	/* variable */
    _R9,	/* variable */
    _R10,	/* sl - stack limit */
    _R11,	/* fp - frame pointer */
    _R12,	/* ip - temporary */
    _R13,	/* sp - stack pointer */
    _R14,	/* lr - link register */
    _R15,	/* pc - program counter */
} jit_gpr_t;

#define JIT_RET		_R0
#define JIT_PC		_R15
#define JIT_LR		_R14
#define JIT_SP		_R13
#define JIT_FP		_R11
#define JIT_TMP		_R3
#define JIT_TMP2	_R7
#define JIT_FTMP        JIT_TMP2

/* Software FP without thumb needs 2 consecutive registers,
   so JIT_DTMP must be an even-numbered register. It conceptually
   overlaps with JIT_TMP and JIT_FTMP, but shuffle registers
   locally to make those two consecutive. */
#define JIT_DTMP      _R2
#define push_DTMP()   jit_movr_p(JIT_FTMP, JIT_DTMP)
#define pop_DTMP()    jit_movr_p(JIT_DTMP, JIT_FTMP)
#define alt_DTMP(r)   ((r == JIT_DTMP) ? JIT_FTMP : r)

/* must use blx to call functions or jit instruction set matches runtime? */
#define jit_exchange_p()		1

/* _VxxxQ macros mean _Q0=_D0, _Q1=_D2, ... */
typedef enum {
    _S0, _D0 = _S0, _Q0 = _D0,
    _S1,
    _S2, _D1 = _S2,
    _S3,
    _S4, _D2 = _S4, _Q1 = _D2,
    _S5,
    _S6, _D3 = _S6,
    _S7,
    _S8, _D4 = _S8, _Q2 = _D4,
    _S9,
    _S10, _D5 = _S10,
    _S11,
    _S12, _D6 = _S12, _Q3 = _D6,
    _S13,
    _S14, _D7 = _S14,
    _S15,
    _S16, _D8 = _S16, _Q4 = _D8,
    _S17,
    _S18, _D9 = _S18,
    _S19,
    _S20, _D10 = _S20, _Q5 = _D10,
    _S21,
    _S22, _D11 = _S22,
    _S23,
    _S24, _D12 = _S24, _Q6 = _D12,
    _S25,
    _S26, _D13 = _S26,
    _S27,
    _S28, _D14 = _S28, _Q7 = _D14,
    _S29,
    _S30, _D15 = _S30,
    _S31,
    JIT_FPRET,	/* for abstraction of returning a float/double result */
} jit_fpr_t;

/* match vfpv3 result */
#define NAN_TO_INT_IS_ZERO		1

#if defined(__ARM_ARCH_4__)
# define JIT_ARM_VERSION 4
#elif defined(__ARM_ARCH_4T__)
# define JIT_ARM_THUMB 1
# define JIT_ARM_VERSION 4
#elif (defined(__ARM_ARCH_5T__) || defined(__ARM_ARCH_5__))
# define JIT_ARM_THUMB 1
# define JIT_ARM_VERSION 5
#elif (defined(__ARM_ARCH_5TE__) || defined(__ARM_ARCH_5TEJ__))
# define JIT_ARM_THUMB 1
# define JIT_ARM_VERSION 5
# define JIT_ARM_EXTENDED 1
#elif defined(__ARM_ARCH_6__) || defined(__ARM_ARCH_6K__) || defined(__ARM_ARCH_6ZK__)
# define JIT_ARM_VERSION 6
#elif defined(__ARM_ARCH_6T__) || defined(__ARM_ARCH_6T2__)
# define JIT_ARM_THUMB 2
# define JIT_ARM_VERSION 6
#elif (defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7R__) || defined(__ARM_ARCH_7M__) || defined(__ARM_ARCH_7__))
# define JIT_ARM_THUMB 2
# define JIT_ARM_VERSION 7
# if defined(__ARM_ARCH_7R__)
#  define JIT_ARM_REALTIME 1
# endif
#else
# define JIT_ARM_VERSION 4
#endif

#if defined(__ARM_PCS_VFP) || (defined(__VFP_FP__) && !defined(__SOFTFP__))
# define JIT_ARM_VFP 1
#endif

#if defined(__ARM_NEON__)
# define JIT_ARM_NEON 1
#endif

#ifndef JIT_ARM_THUMB
# define JIT_ARM_THUMB 0
#endif

#ifndef JIT_ARM_NEON
# define JIT_ARM_NEON 0
#endif

#ifndef JIT_ARM_EXTENDED
# define JIT_ARM_EXTENDED 0
#endif

#ifndef JIT_ARM_REALTIME 
# define JIT_ARM_REALTIME 0
#endif

#ifndef JIT_ARM_VFP
# define JIT_ARM_VFP 0
#endif

/* For now, thumb doesn't work with software float: */
#if JIT_ARM_THUMB && !JIT_ARM_VFP
# undef JIT_ARM_THUMB
# define JIT_ARM_THUMB 0
#endif

# define jit_thumb_p() JIT_ARM_THUMB
# define jit_neon_p()  JIT_ARM_NEON
# define jit_hardfp_p() JIT_ARM_VFP
# define jit_swf_p()    !JIT_ARM_VFP

#define jit_armv5_p()   (JIT_ARM_VERSION >= 5)
#define jit_armv5e_p()  ((JIT_ARM_VERSION > 5) || JIT_ARM_EXTENDED)
#define jit_armv6_p()   (JIT_ARM_VERSION >= 6)
#define jit_armv6t_p()  ((JIT_ARM_VERSION >= 6) && jit_thumb_p())
#define jit_armv7r_p()	((JIT_ARM_VERSION >= 7) && JIT_ARM_REALTIME)


typedef union _jit_thumb_t {
    int		i;
    short	s[2];
} jit_thumb_t;

#if __BYTE_ORDER == __LITTLE_ENDIAN
#  define _jit_WW(i, j)			do { _jit_W(j); _jit_W(i); } while (0)
#  define code2thumb(t0, t1, c0, c1)	do { t1 = c0; t0 = c1; } while (0)
#  define thumb2code(t0, t1, c0, c1)	do { c0 = t1; c1 = t0; } while (0)
#else
#  define _jit_WW(i, j)			do { _jit_W(i); _jit_W(j); } while (0)
#  define code2thumb(t0, t1, c0, c1)	do { t0 = c0; t1 = c1; } while (0)
#  define thumb2code(t0, t1, c0, c1)	do { c0 = t0; c1 = t1; } while (0)
#endif

#define ARM_CC_EQ	0x00000000	/* Z=1 */
#define ARM_CC_NE	0x10000000	/* Z=0 */
#define ARM_CC_HS	0x20000000	/* C=1 */
#  define ARM_CC_CS	ARM_CC_HS
#define ARM_CC_LO	0x30000000	/* C=0 */
#  define ARM_CC_CC	ARM_CC_LO
#define ARM_CC_MI	0x40000000	/* N=1 */
#define ARM_CC_PL	0x50000000	/* N=0 */
#define ARM_CC_VS	0x60000000	/* V=1 */
#define ARM_CC_VC	0x70000000	/* V=0 */
#define ARM_CC_HI	0x80000000	/* C=1 && Z=0 */
#define ARM_CC_LS	0x90000000	/* C=0 || Z=1 */
#define ARM_CC_GE	0xa0000000	/* N=V */
#define ARM_CC_LT	0xb0000000	/* N!=V */
#define ARM_CC_GT	0xc0000000	/* Z=0 && N=V */
#define ARM_CC_LE	0xd0000000	/* Z=1 || N!=V */
#define ARM_CC_AL	0xe0000000	/* always */
#define ARM_CC_NV	0xf0000000	/* reserved */

#define ARM_MOV		0x01a00000
#define THUMB_MOV	    0x4600
#define ARM_MOVWI	0x03000000
#define THUMB_MOVI	    0x2000
#define THUMB2_MOVI	0xf0400000
#define THUMB2_MOVWI	0xf2400000
#define ARM_MOVTI	0x03400000
#define THUMB2_MOVTI	0xf2c00000
#define ARM_MVN		0x01e00000
#define THUMB_MVN	    0x43c0
#define THUMB2_MVN	0xea600000
#define THUMB2_MVNI	0xf0600000
#define ARM_MRC		0x0e100010
#define ARM_MRRC	0x0c500000
#define ARM_MRS		0x010f0000
#define ARM_MSRI	0x03200000
#define ARM_MSR		0x0120f000

#define ARM_I		0x02000000	/* immediate */
#define ARM_S		0x00100000	/* set flags */
#define ARM_ADD		0x00800000
#define THUMB_ADD	    0x1800
#define THUMB_ADDX	    0x4400
#define THUMB2_ADD	0xeb000000
#define THUMB_ADDI3	    0x1c00
#define THUMB_ADDI8	    0x3000
#define THUMB2_ADDI	0xf1000000
#define THUMB2_ADDWI	0xf2000000
#define ARM_ADC		0x00a00000
#define THUMB_ADC	    0x4140
#define THUMB2_ADC	0xeb400000
#define THUMB2_ADCI	0xf1400000
#define ARM_SUB		0x00400000
#define THUMB_SUB	    0x1a00
#define THUMB2_SUB	0xeba00000
#define THUMB_SUBI3	    0x1e00
#define THUMB_SUBI8	    0x3800
#define THUMB2_SUBI	0xf1a00000
#define THUMB2_SUBWI	0xf2a00000
#define ARM_SBC		0x00c00000
#define THUMB_SBC	    0x4180
#define THUMB2_SBC	0xeb600000
#define THUMB2_SBCI	0xf1600000
#define ARM_RSB		0x00600000
#define THUMB2_RSB	0xebc00000
#define THUMB_RSBI	    0x4240
#define THUMB2_RSBI	0xf1c00000
#define ARM_MUL		0x00000090
#define THUMB_MUL	    0x4340
#define THUMB2_MUL	0xfb00f000
#define ARM_SMULL	0x00c00090
#define THUMB2_SMULL	0xfb800000
#define ARM_UMULL	0x00800090
#define THUMB2_UMULL	0xfba00000
#define THUMB2_SDIV	0xfb90f0f0
#define THUMB2_UDIV	0xfbb0f0f0

#define ARM_AND		0x00000000
#define THUMB_AND	    0x4000
#define THUMB2_AND	0xea000000
#define THUMB2_ANDI	0xf0000000
#define ARM_BIC		0x01c00000
#define THUMB2_BIC	0xea200000
#define THUMB2_BICI	0xf0200000
#define ARM_ORR		0x01800000
#define THUMB_ORR	    0x4300
#define THUMB2_ORR	0xea400000
#define THUMB2_ORRI	0xf0400000
#define ARM_EOR		0x00200000
#define THUMB_EOR	    0x4040
#define THUMB2_EOR	0xea800000
#define THUMB2_EORI	0xf0800000
#define ARM_REV		0x06bf0f30
#define THUMB_REV	    0xba00
#define THUMB2_REV	0xfa90f080
#define ARM_REV16	0x06bf0fb0
#define THUMB_REV16	    0xba40
#define THUMB2_REV16	0xfa90f090
#define ARM_SXTB	0x06af0070
#define THUMB_SXTB	    0xb240
#define THUMB2_SXTB	0xfa4f0080
#define ARM_UXTB	0x06ef0070
#define THUMB_UXTB	    0xb2c0
#define THUMB2_UXTB	0xfa5f0080
#define ARM_SXTH	0x06bf0070
#define THUMB_SXTH	    0xb200
#define THUMB2_SXTH	0xfa0f0080
#define ARM_UXTH	0x06ff0070
#define THUMB_UXTH	    0xb280
#define THUMB2_UXTH	0xfa1f0080
#define ARM_XTR8	0x00000400	/* ?xt? rotate 8 bits */
#define ARM_XTR16	0x00000800	/* ?xt? rotate 16 bits */
#define ARM_XTR24	0x00000c00	/* ?xt? rotate 24 bits */

#define ARM_SHIFT	0x01a00000
#define ARM_R		0x00000010	/* register shift */
#define ARM_LSL		0x00000000
#define THUMB_LSL	    0x4080
#define THUMB2_LSL	0xfa00f000
#define THUMB_LSLI	    0x0000
#define THUMB2_LSLI	0xea4f0000
#define ARM_LSR		0x00000020
#define THUMB_LSR	    0x40c0
#define THUMB2_LSR	0xfa20f000
#define THUMB_LSRI	    0x0800
#define THUMB2_LSRI	0xea4f0010
#define ARM_ASR		0x00000040
#define THUMB_ASR	    0x4100
#define THUMB2_ASR	0xfa40f000
#define THUMB_ASRI	    0x1000
#define THUMB2_ASRI	0xea4f0020
#define ARM_ROR		0x00000060
#define ARM_PKH		0x06800010	/* v6T2,v7 */

#define ARM_CMP		0x01500000
#define THUMB_CMP	    0x4280
#define THUMB_CMPX	    0x4500
#define THUMB2_CMP	0xebb00000
#define THUMB_CMPI	    0x2800
#define THUMB2_CMPI	0xf1b00000
#define ARM_CMN		0x01700000
#define THUMB_CMN	    0x42c0
#define THUMB2_CMN	0xeb100000
#define THUMB2_CMNI	0xf1100000
#define ARM_TST		0x01100000
#define THUMB_TST	    0x4200
#define THUMB2_TST	0xea100000
#define THUMB2_TSTI	0xf0100000
#define ARM_TEQ		0x01300000

/* branch */
#define ARM_BX		0x012fff10
#define ARM_BLX		0x012fff30
#define THUMB_BLX	    0x4780
#define ARM_BLXI	0xfa000000
#define THUMB2_BLXI	0xf000c000
#define ARM_B		0x0a000000
#define THUMB_CC_B	    0xd000
#define THUMB_B		    0xe000
#define THUMB2_CC_B	0xf0008000
#define THUMB2_B	0xf0009000
#define ARM_BLI		0x0b000000
#define THUMB2_BLI	0xf000d000

/* ldr/str */
#define ARM_P		0x00800000	/* positive offset
					 * actually, the errata manual
					 * calls this to U field and
					 * 1<<24 the P field */
#define THUMB2_P	0x00000400
#define THUMB2_U	0x00000200
#define THUMB2_W	0x00000100
#define ARM_LDRSB	0x011000d0
#define THUMB_LDRSB	    0x5600
#define THUMB2_LDRSB	0xf9100000
#define ARM_LDRSBI	0x015000d0
#define THUMB2_LDRSBI	0xf9100c00
#define THUMB2_LDRSBWI	0xf9900000
#define ARM_LDRB	0x07500000
#define THUMB_LDRB	    0x5c00
#define THUMB2_LDRB	0xf8100000
#define ARM_LDRBI	0x05500000
#define THUMB_LDRBI	    0x7800
#define THUMB2_LDRBI	0xf8100c00
#define THUMB2_LDRBWI	0xf8900000
#define ARM_LDRSH	0x011000f0
#define THUMB_LDRSH	    0x5e00
#define THUMB2_LDRSH	0xf9300000
#define ARM_LDRSHI	0x015000f0
#define THUMB2_LDRSHI	0xf9300c00
#define THUMB2_LDRSHWI	0xf9b00000
#define ARM_LDRH	0x011000b0
#define THUMB_LDRH	    0x5a00
#define THUMB2_LDRH	0xf8300000
#define ARM_LDRHI	0x015000b0
#define THUMB_LDRHI	    0x8800
#define THUMB2_LDRHI	0xf8300c00
#define THUMB2_LDRHWI	0xf8b00000
#define ARM_LDR		0x07100000
#define THUMB_LDR	    0x5800
#define THUMB2_LDR	0xf8500000
#define ARM_LDRI	0x05100000
#define THUMB_LDRI	    0x6800
#define THUMB_LDRISP	    0x9800
#define THUMB2_LDRI	0xf8500c00	/* manual says v6t2/v7; does not work */
#define THUMB2_LDRWI	0xf8d00000
#define ARM_LDRD	0x010000d0
#define ARM_LDRDI	0x014000d0
#define THUMB2_LDRDI	0xe8500000
#define ARM_STRB	0x07400000
#define THUMB_STRB	    0x5400
#define THUMB2_STRB	0xf8000000
#define ARM_STRBI	0x05400000
#define THUMB_STRBI	    0x7000
#define THUMB2_STRBI	0xf8000c00
#define THUMB2_STRBWI	0xf8800000
#define ARM_STRH	0x010000b0
#define THUMB_STRH	    0x5200
#define THUMB2_STRH	0xf8200000
#define ARM_STRHI	0x014000b0
#define THUMB_STRHI	    0x8000
#define THUMB2_STRHI	0xf8200c00
#define THUMB2_STRHWI	0xf8a00000
#define ARM_STR		0x07000000
#define THUMB_STR	    0x5000
#define THUMB2_STR	0xf8400000
#define ARM_STRI	0x05000000
#define THUMB_STRI	    0x6000
#define THUMB_STRISP	    0x9000
#define THUMB2_STRI	0xf8400c00
#define THUMB2_STRWI	0xf8c00000
#define ARM_STRD	0x010000f0
#define ARM_STRDI	0x014000f0
#define THUMB2_STRDI	0xe8400000	/* manual says v6t2/v7; does not work */

/* ldm/stm */
#define ARM_M		0x08000000
#define ARM_M_L		0x00100000	/* load; store if not set */
#define ARM_M_I		0x00800000	/* inc; dec if not set */
#define ARM_M_B		0x01000000	/* before; after if not set */
#define ARM_M_U		0x00200000	/* update Rn */
#define THUMB2_LDM_W	0x00200000
#define THUMB2_LDM_P	0x00008000
#define THUMB2_LDM_M	0x00004000
#define THUMB_LDMIA	    0xc800
#define THUMB2_LDMIA	0xe8900000
#define THUMB2_LDMDB	0xe9100000
#define THUMB_PUSH	    0xb400
#define THUMB2_PUSH	0xe92d0000
#define THUMB_POP	    0xbc00
#define THUMB2_POP	0xe8bd0000

/* misc */
#define ARM_CLZ		0x016f0f10
#define ARM_PLD		0xf750f000
#define ARM_PLDW	0xf710f000
#define ARM_PLDI	0xf510f000
#define ARM_PLDWI	0xf510f000
#define ARM_PLDLI	0xf55ff000
#define ARM_PLD_U	0x00800000
#define ARM_PLII	0xf4d0f000
#define ARM_PLINI	0xf450f000
#define ARM_PLI		0xf6d0f000
#define ARM_PLIN	0xf650f000

#define FPSCR_N		0x80000000/* Negative condition code flag */
#define FPSCR_Z		0x40000000/* Zero condition code flag */
#define FPSCR_C		0x20000000/* Carry condition code flag */
#define FPSCR_V		0x10000000/* Overflow condition code flag */
#define FPSCR_QC	0x08000000/* Cumulative saturation flag */
#define FPSCR_AHP	0x04000000/* Alternative half-precision (unset is IEEE format) */
#define FPSCR_DN	0x02000000/* Default NaN mode */
#define FPSCR_FZ	0x01000000/* Flush to zero (unset is fully IEEE-754 compliant) */
#define FPSCR_RMASK	0x00c00000
#  define FPSCR_RN	0x00000000	/* Round to Nearest */
#  define FPSCR_RP	0x00400000	/* Round towards Plus Infinity */
#  define FPSCR_RM	0x00800000	/* Round towards Minus Infinity */
#  define FPSCR_RZ	0x00c00000	/* Round towards Zero */
#define FPSCR_STRIDE	0x00300000
#define FPSCR_RES1	0x00080000/* Reserved, UNK/SBZP */
#define FPSCR_LEN	0x00070000
#define FPSCR_IDE	0x00008000/* Input Denormal exception trap enable */
#define FPSCR_IXE	0x00001000/* Inexact exception trap enable */
#define FPSCR_UFE	0x00000800/* Underflow exception trap enable */
#define FPSCR_OFE	0x00000400/* Overflow exception trap enable */
#define FPSCR_DZE	0x00000200/* Division by zero exception trap enable */
#define FPSCR_IOE	0x00000100/* Invalid Operation exception trap enable */
#define FPSCR_IDC	0x00000080/* Input Denormal cumulative exception flag */
#define FPSCR_RES0	0x00000060/* Reserved, UNK/SBZP */
#define FPSCR_IXC	0x00000010/* Inexact cumulative exception flag */
#define FPSCR_UFC	0x00000008/* Underflow cumulative exception flag */
#define FPSCR_OFC	0x00000004/* Overflow cumulative exception flag */
#define FPSCR_DZC	0x00000002/* Division by zero cumulative exception flag */
#define FPSCR_IOC	0x00000001/* Invalid Operation cumulative exception flag */

/***********************************************************************
 * VFPv2 and VFPv3 (encoding T2/A2) instructions
 ***********************************************************************/
#define ARM_V_E		0x00000080	/* ARM_VCMP exception if NaN arg(s) */
#define ARM_V_Z		0x00010000	/* ARM_VCMP with zero */
#define ARM_V_F64	0x00000100	/* Undefined in single precision only variant */
#define ARM_VADD_F	0x0e300a00
#define ARM_VSUB_F	0x0e300a40
#define ARM_VMUL_F	0x0e200a00
#define ARM_VDIV_F	0x0e800a00
#define ARM_VABS_F	0x0eb00ac0
#define ARM_VNEG_F	0x0eb10a40
#define ARM_VSQRT_F	0x0eb10ac0
#define ARM_VMOV_F	0x0eb00a40
#define ARM_VMOV_A_S	0x0e100a10	/* vmov rn, sn */
#define ARM_VMOV_S_A	0x0e000a10	/* vmov sn, rn */
#define ARM_VMOV_AA_D	0x0c500b10	/* vmov rn,rn, dn */
#define ARM_VMOV_D_AA	0x0c400b10	/* vmov dn, rn,rn */
#define ARM_VCMP	0x0eb40a40
#define ARM_VMRS	0x0ef10a10
#define ARM_VMSR	0x0ee10a10
#define ARM_VCVT_2I		0x00040000	/* to integer */
#define ARM_VCVT_2S		0x00010000	/* to signed */
#define ARM_VCVT_RS		0x00000080	/* round to zero or signed */
#define ARM_VCVT		0x0eb80a40
#define ARM_VCVT_S32_F32	ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_2S|ARM_VCVT_RS
#define ARM_VCVT_U32_F32	ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_RS
#define ARM_VCVT_S32_F64	ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_2S|ARM_VCVT_RS|ARM_V_F64
#define ARM_VCVT_U32_F64	ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_RS|ARM_V_F64
#define ARM_VCVT_F32_S32	ARM_VCVT|ARM_VCVT_RS
#define ARM_VCVT_F32_U32	ARM_VCVT
#define ARM_VCVT_F64_S32	ARM_VCVT|ARM_VCVT_RS|ARM_V_F64
#define ARM_VCVT_F64_U32	ARM_VCVT|ARM_V_F64
#define ARM_VCVT_F		0x0eb70ac0
#define ARM_VCVT_F32_F64	ARM_VCVT_F
#define ARM_VCVT_F64_F32	ARM_VCVT_F|ARM_V_F64

/* does not set bit 7, meaning to use rounding mode of FPSCR */
#define ARM_VCVTR_S32_F32	ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_2S
#define ARM_VCVTR_U32_F32	ARM_VCVT|ARM_VCVT_2I
#define ARM_VCVTR_S32_F64	ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_2S|ARM_V_F64
#define ARM_VCVTR_U32_F64	ARM_VCVT|ARM_VCVT_2I|ARM_V_F64

/***********************************************************************
 * NEON instructions (encoding T1/A1) (condition must always be ARM_CC_NV)
 ***********************************************************************/
#define ARM_V_D		0x00400000
#define ARM_V_N		0x00000080
#define ARM_V_Q		0x00000040
#define ARM_V_M		0x00000020
#define ARM_V_U		0x01000000
#define ARM_V_I16	0x00100000
#define ARM_V_I32	0x00200000
#define ARM_V_I64	0x00300000
#define ARM_V_S16	0x00040000
#define ARM_V_S32	0x00080000

#define ARM_VADD_I	0x02000800
#define ARM_VQADD_I	0x02000010	/* sets flag on overflow/carry */
#define ARM_VADDL_I	0x02800000	/* q=d+d */
#define ARM_VADDW_I	0x02800100	/* q=q+d */
#define ARM_VSUB_I	0x03000800
#define ARM_VQSUB_I	0x02000210	/* sets flag on overflow/carry */
#define ARM_VSUBL_I	0x02800200
#define ARM_VSUBW_I	0x02800300
#define ARM_VMUL_I	0x02000910
#define ARM_VMULL_I	0x02800c00

#define ARM_VABS_I	0x03b10300
#define ARM_VQABS_I	0x03b00700	/* sets flag on overflow */
#define ARM_VNEG_I	0x03b10380
#define ARM_VQNEG_I	0x03b00780	/* sets flag on overflow */

#define ARM_VAND	0x02000110
#define ARM_VBIC	0x02100110
#define ARM_VORR	0x02200110
#define ARM_VORN	0x02300110
#define ARM_VEOR	0x03000110
#define ARM_VMOVL_S8	0x00080000
#define ARM_VMOVL_S16	0x00100000
#define ARM_VMOVL_S32	0x00200000
#define ARM_VMOVL_I	0x02800a10

#define ARM_VMVSI	0x0eb00a00
#define ARM_VMOVI	0x02800010
#define ARM_VMVNI	0x02800030

#define ARM_VLDR	0x0d100a00
#define ARM_VSTR	0x0d000a00
#define ARM_VM		0x0c000a00

/***********************************************************************
 * Advanced SIMD (encoding T2/A2) instructions
 ***********************************************************************/
#define ARM_VMOV_ADV_U	0x00800000 	/* zero extend, sign extend if unset */
#define ARM_VMOV_ADV_8	0x00400000
#define ARM_VMOV_ADV_16	0x00000020
#define ARM_VMOV_A_D	0x0e100b10
#define ARM_VMOV_D_A	0x0e000b10

static int
encode_vfp_double(int mov, int inv, unsigned lo, unsigned hi)
{
    int		code, mode, imm, mask;

    if (!jit_neon_p()) return -1;

    if (hi != lo) {
	if (mov && !inv) {
	    /* (I64)
	     *	aaaaaaaabbbbbbbbccccccccddddddddeeeeeeeeffffffffgggggggghhhhhhhh
	     */
	    for (mode = 0, mask = 0xff; mode < 4; mask <<= 8, mode++) {
		imm = lo & mask;
		if (imm != mask && imm != 0)
		    goto fail;
		imm = hi & mask;
		if (imm != mask && imm != 0)
		    goto fail;
	    }
	    mode = 0xe20;
	    imm = (((hi & 0x80000000) >> 24) | ((hi & 0x00800000) >> 17) |
		   ((hi & 0x00008000) >> 10) | ((hi & 0x00000080) >>  3) |
		   ((lo & 0x80000000) >> 28) | ((lo & 0x00800000) >> 21) |
		   ((lo & 0x00008000) >> 14) | ((lo & 0x00000080) >>  7));
	    goto success;
	}
	goto fail;
    }
    /*  (I32)
     *  00000000 00000000 00000000 abcdefgh
     *  00000000 00000000 abcdefgh 00000000
     *  00000000 abcdefgh 00000000 00000000
     *  abcdefgh 00000000 00000000 00000000 */
    for (mode = 0, mask = 0xff; mode < 4; mask <<= 8, mode++) {
	if ((lo & mask) == lo) {
	    imm = lo >> (mode << 3);
	    mode <<= 9;
	    goto success;
	}
    }
    /*  (I16)
     *  00000000 abcdefgh 00000000 abcdefgh
     *  abcdefgh 00000000 abcdefgh 00000000 */
    for (mode = 0, mask = 0xff; mode < 2; mask <<= 8, mode++) {
	if ((lo & mask) && ((lo & (mask << 16)) >> 16) == (lo & mask)) {
	    imm = lo >> (mode << 3);
	    mode = 0x800 | (mode << 9);
	    goto success;
	}
    }
    if (mov) {
	/*  (I32)
	 *  00000000 00000000 abcdefgh 11111111
	 *  00000000 abcdefgh 11111111 11111111 */
	for (mode = 0, mask = 0xff; mode < 2;
	     mask = (mask << 8) | 0xff, mode++) {
	    if ((lo & mask) == mask &&
		!((lo & ~mask) >> 8) &&
		(imm = lo >> (8 + (mode << 8)))) {
		mode = 0xc00 | (mode << 8);
		goto success;
	    }
	}
	if (!inv) {
	    /* (F32)
	     *  aBbbbbbc defgh000 00000000 00000000
	     *  from the ARM Architecture Reference Manual:
	     *  In this entry, B = NOT(b). The bit pattern represents the
	     *  floating-point number (-1)^s* 2^exp * mantissa, where
	     *  S = UInt(a),
	     *  exp = UInt(NOT(b):c:d)-3 and
	     *  mantissa = (16+UInt(e:f:g:h))/16. */
	    if ((lo & 0x7ffff) == 0 &&
		(((lo & 0x7e000000) == 0x3e000000) ||
		 ((lo & 0x7e000000) == 0x40000000))) {
		mode = 0xf00;
		imm = ((lo >> 24) & 0x80) | ((lo >> 19) & 0x7f);
		goto success;
	    }
	}
    }

fail:
    /* need another approach (load from memory, move from arm register, etc) */
    return (-1);

success:
    code = inv ? ARM_VMVNI : ARM_VMOVI;
    switch ((mode & 0xf00) >> 8) {
	case 0x0:	case 0x2:	case 0x4:	case 0x6:
	case 0x8:	case 0xa:
	    if (inv)	mode |= 0x20;
	    if (!mov)	mode |= 0x100;
	    break;
	case 0x1:	case 0x3:	case 0x5:	case 0x7:
	    /* should actually not reach here */
	    jit_assert(!inv);
	case 0x9:	case 0xb:
	    jit_assert(!mov);
	    break;
	case 0xc:	case 0xd:
	    /* should actually not reach here */
	    jit_assert(inv);
	case 0xe:
	    jit_assert(mode & 0x20);
	    jit_assert(mov && !inv);
	    break;
	default:
	    jit_assert(!(mode & 0x20));
	    break;
    }
    imm = ((imm & 0x80) << 17) | ((imm & 0x70) << 12) | (imm & 0x0f);
    code |= mode | imm;
    if (jit_thumb_p()) {
	if (code & 0x1000000)
	    code |= 0xff000000;
	else
	    code |= 0xef000000;
    }
    else
	code |= ARM_CC_NV;
    return (code);
}

#define arm_vodi(oi,r0)		_arm_vodi(_jitp,oi,r0)
__jit_inline void
_arm_vodi(jit_state_t _jitp, int oi, int r0)
{
    jit_thumb_t	thumb;
    jit_assert(!(oi  & 0x0000f000));
    jit_assert(!(r0 & 1));	r0 >>= 1;
    thumb.i = oi|(_u4(r0)<<12);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_voqi(oi,r0)		_arm_voqi(_jitp,oi,r0)
__jit_inline void
_arm_voqi(jit_state_t _jitp, int oi, int r0)
{
    jit_thumb_t	thumb;
    jit_assert(!(oi  & 0x0000f000));
    jit_assert(!(r0 & 3));	r0 >>= 1;
    thumb.i = oi|(_u4(r0)<<12);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vo_ss(o,r0,r1)	 _arm_cc_vo_ss(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vo_ss(cc,o,r0,r1) _arm_cc_vo_ss(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vo_ss(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    if (r0 & 1)	o |= ARM_V_D;	r0 >>= 1;
    if (r1 & 1)	o |= ARM_V_M;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r0)<<12)|_u4(r1);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vo_dd(o,r0,r1)	 _arm_cc_vo_dd(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vo_dd(cc,o,r0,r1) _arm_cc_vo_dd(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vo_dd(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    jit_assert(!(r0 & 1) && !(r1 & 1));
    r0 >>= 1;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r0)<<12)|_u4(r1);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vo_qd(o,r0,r1)	 _arm_cc_vo_qd(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vo_qd(cc,o,r0,r1) _arm_cc_vo_qd(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vo_qd(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    jit_assert(!(r0 & 3) && !(r1 & 1));
    r0 >>= 1;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r0)<<12)|_u4(r1);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vo_qq(o,r0,r1)	 _arm_cc_vo_qq(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vo_qq(cc,o,r0,r1) _arm_cc_vo_qq(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vo_qq(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    jit_assert(!(r0 & 3) && !(r1 & 3));
    r0 >>= 1;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r0)<<12)|_u4(r1);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vorr_(o,r0,r1)	 _arm_cc_vorr_(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vorr_(cc,o,r0,r1) _arm_cc_vorr_(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vorr_(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vors_(o,r0,r1)	 _arm_cc_vors_(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vors_(cc,o,r0,r1) _arm_cc_vors_(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vors_(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    if (r1 & 1)	o |= ARM_V_N;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vorv_(o,r0,r1)	 _arm_cc_vorv_(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vorv_(cc,o,r0,r1) _arm_cc_vorv_(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vorv_(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    if (r1 & 1)	cc |= ARM_V_M;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vori_(o,r0,r1)	 _arm_cc_vori_(_jitp,ARM_CC_NV,o,r0,r1)
#define arm_cc_vori_(cc,o,r0,r1) _arm_cc_vori_(_jitp,cc,o,r0,r1)
__jit_inline void
_arm_cc_vori_(jit_state_t _jitp, int cc, int o, int r0, int r1)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    /* use same bit pattern, to set opc1... */
    if (r1 & 1)	o |= ARM_V_I32;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vorrd(o,r0,r1,r2)	    _arm_cc_vorrd(_jitp,ARM_CC_NV,o,r0,r1,r2)
#define arm_cc_vorrd(cc,o,r0,r1,r2) _arm_cc_vorrd(_jitp,cc,o,r0,r1,r2)
__jit_inline void
_arm_cc_vorrd(jit_state_t _jitp, int cc, int o, int r0, int r1, int r2)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff00f));
    jit_assert(!(r2 & 1));
    r2 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_vosss(o,r0,r1,r2)	    _arm_cc_vosss(_jitp,ARM_CC_NV,o,r0,r1,r2)
#define arm_cc_vosss(cc,o,r0,r1,r2) _arm_cc_vosss(_jitp,cc,o,r0,r1,r2)
__jit_inline void
_arm_cc_vosss(jit_state_t _jitp, int cc, int o, int r0, int r1, int r2)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff00f));
    if (r0 & 1)	o |= ARM_V_D;	r0 >>= 1;
    if (r1 & 1)	o |= ARM_V_N;	r1 >>= 1;
    if (r2 & 1)	o |= ARM_V_M;	r2 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_voddd(o,r0,r1,r2)	    _arm_cc_voddd(_jitp,ARM_CC_NV,o,r0,r1,r2)
#define arm_cc_voddd(cc,o,r0,r1,r2) _arm_cc_voddd(_jitp,cc,o,r0,r1,r2)
__jit_inline void
_arm_cc_voddd(jit_state_t _jitp, int cc, int o, int r0, int r1, int r2)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff00f));
    jit_assert(!(r0 & 1) && !(r1 & 1) && !(r2 & 1));
    r0 >>= 1;	r1 >>= 1;	r2 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_voqdd(o,r0,r1,r2)	    _arm_cc_voqdd(_jitp,ARM_CC_NV,o,r0,r1,r2)
#define arm_cc_voqdd(cc,o,r0,r1,r2) _arm_cc_voqdd(_jitp,cc,o,r0,r1,r2)
__jit_inline void
_arm_cc_voqdd(jit_state_t _jitp, int cc, int o, int r0, int r1, int r2)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff00f));
    jit_assert(!(r0 & 3) && !(r1 & 1) && !(r2 & 1));
    r0 >>= 1;	r1 >>= 1;	r2 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_voqqd(o,r0,r1,r2)	    _arm_cc_voqqd(_jitp,ARM_CC_NV,o,r0,r1,r2)
#define arm_cc_voqqd(cc,o,r0,r1,r2) _arm_cc_voqqd(_jitp,cc,o,r0,r1,r2)
__jit_inline void
_arm_cc_voqqd(jit_state_t _jitp, int cc, int o, int r0, int r1, int r2)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff00f));
    jit_assert(!(r0 & 3) && !(r1 & 3) && !(r2 & 1));
    r0 >>= 1;	r1 >>= 1;	r2 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_voqqq(o,r0,r1,r2)	    _arm_cc_voqqq(_jitp,ARM_CC_NV,o,r0,r1,r2)
#define arm_cc_voqqq(cc,o,r0,r1,r2) _arm_cc_voqqq(_jitp,cc,o,r0,r1,r2)
__jit_inline void
_arm_cc_voqqq(jit_state_t _jitp, int cc, int o, int r0, int r1, int r2)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff00f));
    jit_assert(!(r0 & 3) && !(r1 & 3) && !(r2 & 3));
    r0 >>= 1;	r1 >>= 1;	r2 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_cc_vldst(cc,o,r0,r1,i0) _arm_cc_vldst(_jitp,cc,o,r0,r1,i0)
__jit_inline void
_arm_cc_vldst(jit_state_t _jitp, int cc, int o, int r0, int r1, int i0)
{
    jit_thumb_t	thumb;
    /* i0 << 2 is byte offset */
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff0ff));
    if (r0 & 1) {
	jit_assert(!(o & ARM_V_F64));
	o |= ARM_V_D;
    }
    r0 >>= 1;
    thumb.i = cc|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u8(i0);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

#define arm_cc_vorsl(cc,o,r0,r1,i0) _arm_cc_vorsl(_jitp,cc,o,r0,r1,i0)
__jit_inline void
_arm_cc_vorsl(jit_state_t _jitp, int cc, int o, int r0, int r1, int i0)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff0ff));
    /* save i0 double precision registers */
    if (o & ARM_V_F64)		i0 <<= 1;
    jit_assert(i0 && !(i0 & 1) && r1 + i0 <= 32);
    /* if (r1 & 1) cc & ARM_V_F64 must be false */
    if (r1 & 1)	o |= ARM_V_D;	r1 >>= 1;
    thumb.i = cc|o|(_u4(r0)<<16)|(_u4(r1)<<12)|_u8(i0);
    if (jit_thumb_p())
	_jit_WW(thumb.s[0], thumb.s[1]);
    else
	_jit_I(thumb.i);
}

/***********************************************************************
 * VFPv2 and VFPv3 (encoding T2/A2) instructions
 ***********************************************************************/
#define _CC_VADD_F32(cc,r0,r1,r2)	arm_cc_vosss(cc,ARM_VADD_F,r0,r1,r2)
#define _VADD_F32(r0,r1,r2)		_CC_VADD_F32(ARM_CC_AL,r0,r1,r2)
#define _CC_VADD_F64(cc,r0,r1,r2)	arm_cc_voddd(cc,ARM_VADD_F|ARM_V_F64,r0,r1,r2)
#define _VADD_F64(r0,r1,r2)		_CC_VADD_F64(ARM_CC_AL,r0,r1,r2)
#define _CC_VSUB_F32(cc,r0,r1,r2)	arm_cc_vosss(cc,ARM_VSUB_F,r0,r1,r2)
#define _VSUB_F32(r0,r1,r2)		_CC_VSUB_F32(ARM_CC_AL,r0,r1,r2)
#define _CC_VSUB_F64(cc,r0,r1,r2)	arm_cc_voddd(cc,ARM_VSUB_F|ARM_V_F64,r0,r1,r2)
#define _VSUB_F64(r0,r1,r2)		_CC_VSUB_F64(ARM_CC_AL,r0,r1,r2)
#define _CC_VMUL_F32(cc,r0,r1,r2)	arm_cc_vosss(cc,ARM_VMUL_F,r0,r1,r2)
#define _VMUL_F32(r0,r1,r2)		_CC_VMUL_F32(ARM_CC_AL,r0,r1,r2)
#define _CC_VMUL_F64(cc,r0,r1,r2)	arm_cc_voddd(cc,ARM_VMUL_F|ARM_V_F64,r0,r1,r2)
#define _VMUL_F64(r0,r1,r2)		_CC_VMUL_F64(ARM_CC_AL,r0,r1,r2)
#define _CC_VDIV_F32(cc,r0,r1,r2)	arm_cc_vosss(cc,ARM_VDIV_F,r0,r1,r2)
#define _VDIV_F32(r0,r1,r2)		_CC_VDIV_F32(ARM_CC_AL,r0,r1,r2)
#define _CC_VDIV_F64(cc,r0,r1,r2)	arm_cc_voddd(cc,ARM_VDIV_F|ARM_V_F64,r0,r1,r2)
#define _VDIV_F64(r0,r1,r2)		_CC_VDIV_F64(ARM_CC_AL,r0,r1,r2)
#define _CC_VABS_F32(cc,r0,r1)		arm_cc_vo_ss(cc,ARM_VABS_F,r0,r1)
#define _VABS_F32(r0,r1)		_CC_VABS_F32(ARM_CC_AL,r0,r1)
#define _CC_VABS_F64(cc,r0,r1)		arm_cc_vo_dd(cc,ARM_VABS_F|ARM_V_F64,r0,r1)
#define _VABS_F64(r0,r1)		_CC_VABS_F64(ARM_CC_AL,r0,r1)
#define _CC_VNEG_F32(cc,r0,r1)		arm_cc_vo_ss(cc,ARM_VNEG_F,r0,r1)
#define _VNEG_F32(r0,r1)		_CC_VNEG_F32(ARM_CC_AL,r0,r1)
#define _CC_VNEG_F64(cc,r0,r1)		arm_cc_vo_dd(cc,ARM_VNEG_F|ARM_V_F64,r0,r1)
#define _VNEG_F64(r0,r1)		_CC_VNEG_F64(ARM_CC_AL,r0,r1)
#define _CC_VSQRT_F32(cc,r0,r1)		arm_cc_vo_ss(cc,ARM_VSQRT_F,r0,r1)
#define _VSQRT_F32(r0,r1)		_CC_VSQRT_F32(ARM_CC_AL,r0,r1)
#define _CC_VSQRT_F64(cc,r0,r1)		arm_cc_vo_dd(cc,ARM_VSQRT_F|ARM_V_F64,r0,r1)
#define _VSQRT_F64(r0,r1)		_CC_VSQRT_F64(ARM_CC_AL,r0,r1)
#define _CC_VMOV_F32(cc,r0,r1)		arm_cc_vo_ss(cc,ARM_VMOV_F,r0,r1)
#define _VMOV_F32(r0,r1)		_CC_VMOV_F32(ARM_CC_AL,r0,r1)
#define _CC_VMOV_F64(cc,r0,r1)		arm_cc_vo_dd(cc,ARM_VMOV_F|ARM_V_F64,r0,r1)
#define _VMOV_F64(r0,r1)		_CC_VMOV_F64(ARM_CC_AL,r0,r1)
#define _CC_VMOV_AA_D(cc,r0,r1,r2)	arm_cc_vorrd(cc,ARM_VMOV_AA_D,r0,r1,r2)
#define _VMOV_AA_D(r0,r1,r2)		_CC_VMOV_AA_D(ARM_CC_AL,r0,r1,r2)
#define _CC_VMOV_D_AA(cc,r0,r1,r2)	arm_cc_vorrd(cc,ARM_VMOV_D_AA,r1,r2,r0)
#define _VMOV_D_AA(r0,r1,r2)		_CC_VMOV_D_AA(ARM_CC_AL,r0,r1,r2)
#define _CC_VMOV_A_S(cc,r0,r1)		arm_cc_vors_(cc,ARM_VMOV_A_S,r0,r1)
#define _VMOV_A_S(r0,r1)		_CC_VMOV_A_S(ARM_CC_AL,r0,r1)
#define _CC_VMOV_S_A(cc,r0,r1)		arm_cc_vors_(cc,ARM_VMOV_S_A,r1,r0)
#define _VMOV_S_A(r0,r1)		_CC_VMOV_S_A(ARM_CC_AL,r0,r1)
#define _CC_VCMP_F32(cc,r0,r1)		arm_cc_vo_ss(cc,ARM_VCMP,r0,r1)
#define _VCMP_F32(r0,r1)		_CC_VCMP_F32(ARM_CC_AL,r0,r1)
#define _CC_VCMP_F64(cc,r0,r1)		arm_cc_vo_dd(cc,ARM_VCMP|ARM_V_F64,r0,r1)
#define _VCMP_F64(r0,r1)		_CC_VCMP_F64(ARM_CC_AL,r0,r1)
#define _CC_VCMPE_F32(cc,r0,r1)		arm_cc_vo_ss(cc,ARM_VCMP|ARM_V_E,r0,r1)
#define _VCMPE_F32(r0,r1)		_CC_VCMPE_F32(ARM_CC_AL,r0,r1)
#define _CC_VCMPE_F64(cc,r0,r1)		arm_cc_vo_dd(cc,ARM_VCMP|ARM_V_E|ARM_V_F64,r0,r1)
#define _VCMPE_F64(r0,r1)		_CC_VCMPE_F64(ARM_CC_AL,r0,r1)
#define _CC_VCMPZ_F32(cc,r0)		arm_cc_vo_ss(cc,ARM_VCMP|ARM_V_Z,r0,0)
#define _VCMPZ_F32(r0)			_CC_VCMPZ_F32(ARM_CC_AL,r0)
#define _CC_VCMPZ_F64(cc,r0)		arm_cc_vo_dd(cc,ARM_VCMP|ARM_V_Z|ARM_V_F64,r0,0)
#define _VCMPZ_F64(r0)			_CC_VCMPZ_F64(ARM_CC_AL,r0)
#define _CC_VCMPEZ_F32(cc,r0)		arm_cc_vo_ss(cc,ARM_VCMP|ARM_V_Z|ARM_V_E,r0,0)
#define _VCMPEZ_F32(r0)			_CC_VCMPEZ_F32(ARM_CC_AL,r0)
#define _CC_VCMPEZ_F64(cc,r0)		arm_cc_vo_dd(cc,ARM_VCMP|ARM_V_Z|ARM_V_E|ARM_V_F64,r0,0)
#define _VCMPEZ_F64(r0)			_CC_VCMPEZ_F64(ARM_CC_AL,r0)
#define _CC_VMRS(cc,r0)			arm_cc_vorr_(cc,ARM_VMRS,r0,0)
#define _VMRS(r0)			_CC_VMRS(ARM_CC_AL,r0)
#define _CC_VMSR(cc,r0)			arm_cc_vorr_(cc,ARM_VMSR,r0,0)
#define _VMSR(r0)			_CC_VMSR(ARM_CC_AL,r0)
#define _CC_VCVT_S32_F32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_S32_F32,r0,r1)
#define _VCVT_S32_F32(r0,r1)		_CC_VCVT_S32_F32(ARM_CC_AL,r0,r1)
#define _CC_VCVT_U32_F32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_U32_F32,r0,r1)
#define _VCVT_U32_F32(r0,r1)		_CC_VCVT_U32_F32(ARM_CC_AL,r0,r1)
#define _CC_VCVT_S32_F64(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_S32_F64,r0,r1)
#define _VCVT_S32_F64(r0,r1)		_CC_VCVT_S32_F64(ARM_CC_AL,r0,r1)
#define _CC_VCVT_U32_F64(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_U32_F64,r0,r1)
#define _VCVT_U32_F64(r0,r1)		_CC_VCVT_U32_F64(ARM_CC_AL,r0,r1)
#define _CC_VCVT_F32_S32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_F32_S32,r0,r1)
#define _VCVT_F32_S32(r0,r1)		_CC_VCVT_F32_S32(ARM_CC_AL,r0,r1)
#define _CC_VCVT_F32_U32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_F32_U32,r0,r1)
#define _VCVT_F32_U32(r0,r1)		_CC_VCVT_F32_U32(ARM_CC_AL,r0,r1)
#define _CC_VCVT_F64_S32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_F64_S32,r0,r1)
#define _VCVT_F64_S32(r0,r1)		_CC_VCVT_F64_S32(ARM_CC_AL,r0,r1)
#define _CC_VCVT_F64_U32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_F64_U32,r0,r1)
#define _VCVT_F64_U32(r0,r1)		_CC_VCVT_F64_U32(ARM_CC_AL,r0,r1)
#define _CC_VCVT_F32_F64(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_F32_F64,r0,r1)
#define _VCVT_F32_F64(r0,r1)		_CC_VCVT_F32_F64(ARM_CC_AL,r0,r1)
#define _CC_VCVT_F64_F32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVT_F64_F32,r0,r1)
#define _VCVT_F64_F32(r0,r1)		_CC_VCVT_F64_F32(ARM_CC_AL,r0,r1)
/* use rounding mode in fpscr (intended for floor, ceil, etc) */
#define _CC_VCVTR_S32_F32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVTR_S32_F32,r0,r1)
#define _VCVTR_S32_F32(r0,r1)		_CC_VCVTR_S32_F32(ARM_CC_AL,r0,r1)
#define _CC_VCVTR_U32_F32(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVTR_U32_F32,r0,r1)
#define _VCVTR_U32_F32(r0,r1)		_CC_VCVTR_U32_F32(ARM_CC_AL,r0,r1)
#define _CC_VCVTR_S32_F64(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVTR_S32_F64,r0,r1)
#define _VCVTR_S32_F64(r0,r1)		_CC_VCVTR_S32_F64(ARM_CC_AL,r0,r1)
#define _CC_VCVTR_U32_F64(cc,r0,r1)	arm_cc_vo_ss(cc,ARM_VCVTR_U32_F64,r0,r1)
#define _VCVTR_U32_F64(r0,r1)		_CC_VCVTR_U32_F64(ARM_CC_AL,r0,r1)
/***********************************************************************
 * NEON instructions (encoding T1/A1) (condition must always be ARM_CC_NV)
 ***********************************************************************/
#define _CC_VLDMIA_F32(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_L|ARM_M_I,r0,r1,i0)
#define _VLDMIA_F32(r0,r1,i0)		_CC_VLDMIA_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDMIA_F64(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_L|ARM_M_I|ARM_V_F64,r0,r1,i0)
#define _VLDMIA_F64(r0,r1,i0)		_CC_VLDMIA_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTMIA_F32(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_I,r0,r1,i0)
#define _VSTMIA_F32(r0,r1,i0)		_CC_VSTMIA_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTMIA_F64(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_I|ARM_V_F64,r0,r1,i0)
#define _VSTMIA_F64(r0,r1,i0)		_CC_VSTMIA_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDMIA_U_F32(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_L|ARM_M_I|ARM_M_U,r0,r1,i0)
#define _VLDMIA_U_F32(r0,r1,i0)		_CC_VLDMIA_U_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDMIA_U_F64(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_L|ARM_M_I|ARM_M_U|ARM_V_F64,r0,r1,i0)
#define _VLDMIA_U_F64(r0,r1,i0)		_CC_VLDMIA_U_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTMIA_U_F32(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_I|ARM_M_U,r0,r1,i0)
#define _VSTMIA_U_F32(r0,r1,i0)		_CC_VSTMIA_U_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTMIA_U_F64(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_I|ARM_M_U|ARM_V_F64,r0,r1,i0)
#define _VSTMIA_U_F64(r0,r1,i0)		_CC_VSTMIA_U_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDMDB_U_F32(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_L|ARM_M_B|ARM_M_U,r0,r1,i0)
#define _VLDMDB_U_F32(r0,r1,i0)		_CC_VLDMDB_U_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDMDB_U_F64(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_L|ARM_M_B|ARM_M_U|ARM_V_F64,r0,r1,i0)
#define _VLDMDB_U_F64(r0,r1,i0)		_CC_VLDMDB_U_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTMDB_U_F32(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_B|ARM_M_U,r0,r1,i0)
#define _VSTMDB_U_F32(r0,r1,i0)		_CC_VSTMDB_U_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTMDB_U_F64(cc,r0,r1,i0)	arm_cc_vorsl(cc,ARM_VM|ARM_M_B|ARM_M_U|ARM_V_F64,r0,r1,i0)
#define _VSTMDB_U_F64(r0,r1,i0)		_CC_VSTMDB_U_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VPUSH_F32(cc,r0,i0)		_CC_VSTMDB_U_F32(cc,JIT_SP,r0,i0)
#define _VPUSH_F32(r0,i0)		_CC_VPUSH_F32(ARM_CC_AL,r0,i0)
#define _CC_VPUSH_F64(cc,r0,i0)		_CC_VSTMDB_U_F64(cc,JIT_SP,r0,i0)
#define _VPUSH_F64(r0,i0)		_CC_VPUSH_F64(ARM_CC_AL,r0,i0)
#define _CC_VPOP_F32(cc,r0,i0)		_CC_VLDMIA_U_F32(cc,JIT_SP,r0,i0)
#define _VPOP_F32(r0,i0)		_CC_VPOP_F32(ARM_CC_AL,r0,i0)
#define _CC_VPOP_F64(cc,r0,i0)		_CC_VLDMIA_U_F64(cc,JIT_SP,r0,i0)
#define _VPOP_F64(r0,i0)		_CC_VPOP_F64(ARM_CC_AL,r0,i0)
/***********************************************************************
 * Advanced SIMD (encoding T2/A2) instructions
 ***********************************************************************/
#define _CC_VMOV_A_S8(cc,r0,r1)		arm_cc_vorv_(cc,ARM_VMOV_A_D|ARM_VMOV_ADV_8,r0,r1)
#define _VMOV_A_S8(r0,r1)		_CC_VMOV_A_S8(ARM_CC_AL,r0,r1)
#define _CC_VMOV_A_U8(cc,r0,r1)		arm_cc_vorv_(cc,ARM_VMOV_A_D|ARM_VMOV_ADV_8|ARM_VMOV_ADV_U,r0,r1)
#define _VMOV_A_U8(r0,r1)		_CC_VMOV_A_U8(ARM_CC_AL,r0,r1)
#define _CC_VMOV_A_S16(cc,r0,r1)	arm_cc_vorv_(cc,ARM_VMOV_A_D|ARM_VMOV_ADV_16,r0,r1)
#define _VMOV_A_S16(r0,r1)		_CC_VMOV_A_S16(ARM_CC_AL,r0,r1)
#define _CC_VMOV_A_U16(cc,r0,r1)	arm_cc_vorv_(cc,ARM_VMOV_A_D|ARM_VMOV_ADV_16|ARM_VMOV_ADV_U,r0,r1)
#define _VMOV_A_U16(r0,r1)		_CC_VMOV_A_U16(ARM_CC_AL,r0,r1)
#define _CC_VMOV_A_S32(cc,r0,r1)	arm_cc_vori_(cc,ARM_VMOV_A_D,r0,r1)
#define _VMOV_A_S32(r0,r1)		_CC_VMOV_A_S32(ARM_CC_AL,r0,r1)
#define _CC_VMOV_A_U32(cc,r0,r1)	arm_cc_vori_(cc,ARM_VMOV_A_D|ARM_VMOV_ADV_U,r0,r1)
#define _VMOV_A_U32(r0,r1)		_CC_VMOV_A_U32(ARM_CC_AL,r0,r1)
#define _CC_VMOV_V_I8(cc,r0,r1)		arm_cc_vorv_(cc,ARM_VMOV_D_A|ARM_VMOV_ADV_8,r1,r0)
#define _VMOV_V_I8(r0,r1)		_CC_VMOV_V_I8(ARM_CC_AL,r0,r1)
#define _CC_VMOV_V_I16(cc,r0,r1)	arm_cc_vorv_(cc,ARM_VMOV_D_A|ARM_VMOV_ADV_16,r1,r0)
#define _VMOV_V_I16(r0,r1)		_CC_VMOV_V_I16(ARM_CC_AL,r0,r1)
#define _CC_VMOV_V_I32(cc,r0,r1)	arm_cc_vori_(cc,ARM_VMOV_D_A,r1,r0)
#define _VMOV_V_I32(r0,r1)		_CC_VMOV_V_I32(ARM_CC_AL,r0,r1)
#define _VADD_I8(r0,r1,r2)		arm_voddd(ARM_VADD_I,r0,r1,r2)
#define _VADDQ_I8(r0,r1,r2)		arm_voqqq(ARM_VADD_I|ARM_V_Q,r0,r1,r2)
#define _VADD_I16(r0,r1,r2)		arm_voddd(ARM_VADD_I|ARM_V_I16,r0,r1,r2)
#define _VADDQ_I16(r0,r1,r2)		arm_voqqq(ARM_VADD_I|ARM_V_I16|ARM_V_Q,r0,r1,r2)
#define _VADD_I32(r0,r1,r2)		arm_voddd(ARM_VADD_I|ARM_V_I32,r0,r1,r2)
#define _VADDQ_I32(r0,r1,r2)		arm_voqqq(ARM_VADD_I|ARM_V_I32|ARM_V_Q,r0,r1,r2)
#define _VADD_I64(r0,r1,r2)		arm_voddd(ARM_VADD_I|ARM_V_I64,r0,r1,r2)
#define _VADDQ_I64(r0,r1,r2)		arm_voqqq(ARM_VADD_I|ARM_V_I64|ARM_V_Q,r0,r1,r2)
#define _VQADD_S8(r0,r1,r2)		arm_voddd(ARM_VQADD_I,r0,r1,r2)
#define _VQADDQ_S8(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_Q,r0,r1,r2)
#define _VQADD_U8(r0,r1,r2)		arm_voddd(ARM_VQADD_I|ARM_V_U,r0,r1,r2)
#define _VQADDQ_U8(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VQADD_S16(r0,r1,r2)		arm_voddd(ARM_VQADD_I|ARM_V_I16,r0,r1,r2)
#define _VQADDQ_S16(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_I16|ARM_V_Q,r0,r1,r2)
#define _VQADD_U16(r0,r1,r2)		arm_voddd(ARM_VQADD_I|ARM_V_I16|ARM_V_U,r0,r1,r2)
#define _VQADDQ_U16(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_I16|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VQADD_S32(r0,r1,r2)		arm_voddd(ARM_VQADD_I|ARM_V_I32,r0,r1,r2)
#define _VQADDQ_S32(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_I32|ARM_V_Q,r0,r1,r2)
#define _VQADD_U32(r0,r1,r2)		arm_voddd(ARM_VQADD_I|ARM_V_I32|ARM_V_U,r0,r1,r2)
#define _VQADDQ_U32(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_I32|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VQADD_S64(r0,r1,r2)		arm_voddd(ARM_VQADD_I|ARM_V_I64,r0,r1,r2)
#define _VQADDQ_S64(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_I64|ARM_V_Q,r0,r1,r2)
#define _VQADD_U64(r0,r1,r2)		arm_voddd(ARM_VQADD_I|ARM_V_I64|ARM_V_U,r0,r1,r2)
#define _VQADDQ_U64(r0,r1,r2)		arm_voqqq(ARM_VQADD_I|ARM_V_I64|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VADDL_S8(r0,r1,r2)		arm_voqdd(ARM_VADDL_I,r0,r1,r2)
#define _VADDL_U8(r0,r1,r2)		arm_voqdd(ARM_VADDL_I|ARM_V_U,r0,r1,r2)
#define _VADDL_S16(r0,r1,r2)		arm_voqdd(ARM_VADDL_I|ARM_V_I16,r0,r1,r2)
#define _VADDL_U16(r0,r1,r2)		arm_voqdd(ARM_VADDL_I|ARM_V_I16|ARM_V_U,r0,r1,r2)
#define _VADDL_S32(r0,r1,r2)		arm_voqdd(ARM_VADDL_I|ARM_V_I32,r0,r1,r2)
#define _VADDL_U32(r0,r1,r2)		arm_voqdd(ARM_VADDL_I|ARM_V_I32|ARM_V_U,r0,r1,r2)
#define _VADDW_S8(r0,r1,r2)		arm_voqqd(ARM_VADDW_I,r0,r1,r2)
#define _VADDW_U8(r0,r1,r2)		arm_voqqd(ARM_VADDW_I|ARM_V_U,r0,r1,r2)
#define _VADDW_S16(r0,r1,r2)		arm_voqqd(ARM_VADDW_I|ARM_V_I16,r0,r1,r2)
#define _VADDW_U16(r0,r1,r2)		arm_voqqd(ARM_VADDW_I|ARM_V_I16|ARM_V_U,r0,r1,r2)
#define _VADDW_S32(r0,r1,r2)		arm_voqqd(ARM_VADDW_I|ARM_V_I32,r0,r1,r2)
#define _VADDW_U32(r0,r1,r2)		arm_voqqd(ARM_VADDW_I|ARM_V_I32|ARM_V_U,r0,r1,r2)
#define _VSUB_I8(r0,r1,r2)		arm_voddd(ARM_VSUB_I,r0,r1,r2)
#define _VSUBQ_I8(r0,r1,r2)		arm_voqqq(ARM_VSUB_I|ARM_V_Q,r0,r1,r2)
#define _VSUB_I16(r0,r1,r2)		arm_voddd(ARM_VSUB_I|ARM_V_I16,r0,r1,r2)
#define _VSUBQ_I16(r0,r1,r2)		arm_voqqq(ARM_VSUB_I|ARM_V_I16|ARM_V_Q,r0,r1,r2)
#define _VSUB_I32(r0,r1,r2)		arm_voddd(ARM_VSUB_I|ARM_V_I32,r0,r1,r2)
#define _VSUBQ_I32(r0,r1,r2)		arm_voqqq(ARM_VSUB_I|ARM_V_I32|ARM_V_Q,r0,r1,r2)
#define _VSUB_I64(r0,r1,r2)		arm_voddd(ARM_VSUB_I|ARM_V_I64,r0,r1,r2)
#define _VSUBQ_I64(r0,r1,r2)		arm_voqqq(ARM_VSUB_I|ARM_V_I64|ARM_V_Q,r0,r1,r2)
#define _VQSUB_S8(r0,r1,r2)		arm_voddd(ARM_VQSUB_I,r0,r1,r2)
#define _VQSUBQ_S8(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_Q,r0,r1,r2)
#define _VQSUB_U8(r0,r1,r2)		arm_voddd(ARM_VQSUB_I|ARM_V_U,r0,r1,r2)
#define _VQSUBQ_U8(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VQSUB_S16(r0,r1,r2)		arm_voddd(ARM_VQSUB_I|ARM_V_I16,r0,r1,r2)
#define _VQSUBQ_S16(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_I16|ARM_V_Q,r0,r1,r2)
#define _VQSUB_U16(r0,r1,r2)		arm_voddd(ARM_VQSUB_I|ARM_V_I16|ARM_V_U,r0,r1,r2)
#define _VQSUBQ_U16(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_I16|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VQSUB_S32(r0,r1,r2)		arm_voddd(ARM_VQSUB_I|ARM_V_I32,r0,r1,r2)
#define _VQSUBQ_S32(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_I32|ARM_V_Q,r0,r1,r2)
#define _VQSUB_U32(r0,r1,r2)		arm_voddd(ARM_VQSUB_I|ARM_V_I32|ARM_V_U,r0,r1,r2)
#define _VQSUBQ_U32(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_I32|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VQSUB_S64(r0,r1,r2)		arm_voddd(ARM_VQSUB_I|ARM_V_I64,r0,r1,r2)
#define _VQSUBQ_S64(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_I64|ARM_V_Q,r0,r1,r2)
#define _VQSUB_U64(r0,r1,r2)		arm_voddd(ARM_VQSUB_I|ARM_V_I64|ARM_V_U,r0,r1,r2)
#define _VQSUBQ_U64(r0,r1,r2)		arm_voqqq(ARM_VQSUB_I|ARM_V_I64|ARM_V_U|ARM_V_Q,r0,r1,r2)
#define _VSUBL_S8(r0,r1,r2)		arm_voqdd(ARM_VSUBL_I,r0,r1,r2)
#define _VSUBL_U8(r0,r1,r2)		arm_voqdd(ARM_VSUBL_I|ARM_V_U,r0,r1,r2)
#define _VSUBL_S16(r0,r1,r2)		arm_voqdd(ARM_VSUBL_I|ARM_V_I16,r0,r1,r2)
#define _VSUBL_U16(r0,r1,r2)		arm_voqdd(ARM_VSUBL_I|ARM_V_I16|ARM_V_U,r0,r1,r2)
#define _VSUBL_S32(r0,r1,r2)		arm_voqdd(ARM_VSUBL_I|ARM_V_I32,r0,r1,r2)
#define _VSUBL_U32(r0,r1,r2)		arm_voqdd(ARM_VSUBL_I|ARM_V_I32|ARM_V_U,r0,r1,r2)
#define _VSUBW_S8(r0,r1,r2)		arm_voqqd(ARM_VSUBW_I,r0,r1,r2)
#define _VSUBW_U8(r0,r1,r2)		arm_voqqd(ARM_VSUBW_I|ARM_V_U,r0,r1,r2)
#define _VSUBW_S16(r0,r1,r2)		arm_voqqd(ARM_VSUBW_I|ARM_V_I16,r0,r1,r2)
#define _VSUBW_U16(r0,r1,r2)		arm_voqqd(ARM_VSUBW_I|ARM_V_I16|ARM_V_U,r0,r1,r2)
#define _VSUBW_S32(r0,r1,r2)		arm_voqqd(ARM_VSUBW_I|ARM_V_I32,r0,r1,r2)
#define _VSUBW_U32(r0,r1,r2)		arm_voqqd(ARM_VSUBW_I|ARM_V_I32|ARM_V_U,r0,r1,r2)
#define _VMUL_I8(r0,r1,r2)		arm_voddd(ARM_VMUL_I,r0,r1,r2)
#define _VMULQ_I8(r0,r1,r2)		arm_voqqq(ARM_VMUL_I|ARM_V_Q,r0,r1,r2)
#define _VMUL_I16(r0,r1,r2)		arm_voddd(ARM_VMUL_I|ARM_V_I16,r0,r1,r2)
#define _VMULQ_I16(r0,r1,r2)		arm_voqqq(ARM_VMUL_I|ARM_V_Q|ARM_V_I16,r0,r1,r2)
#define _VMUL_I32(r0,r1,r2)		arm_voddd(ARM_VMUL_I|ARM_V_I32,r0,r1,r2)
#define _VMULQ_I32(r0,r1,r2)		arm_voqqq(ARM_VMUL_I|ARM_V_Q|ARM_V_I32,r0,r1,r2)
#define _VMULL_S8(r0,r1,r2)		arm_voqdd(ARM_VMULL_I,r0,r1,r2)
#define _VMULL_U8(r0,r1,r2)		arm_voqdd(ARM_VMULL_I|ARM_V_U,r0,r1,r2)
#define _VMULL_S16(r0,r1,r2)		arm_voqdd(ARM_VMULL_I|ARM_V_I16,r0,r1,r2)
#define _VMULL_U16(r0,r1,r2)		arm_voqdd(ARM_VMULL_I|ARM_V_U|ARM_V_I16,r0,r1,r2)
#define _VMULL_S32(r0,r1,r2)		arm_voqdd(ARM_VMULL_I|ARM_V_I32,r0,r1,r2)
#define _VMULL_U32(r0,r1,r2)		arm_voqdd(ARM_VMULL_I|ARM_V_U|ARM_V_I32,r0,r1,r2)
#define _VABS_S8(r0,r1)			arm_vo_dd(ARM_VABS_I,r0,r1)
#define _VABSQ_S8(r0,r1)		arm_vo_qq(ARM_VABS_I|ARM_V_Q,r0,r1)
#define _VABS_S16(r0,r1)		arm_vo_dd(ARM_VABS_I|ARM_V_S16,r0,r1)
#define _VABSQ_S16(r0,r1)		arm_vo_qq(ARM_VABS_I|ARM_V_S16|ARM_V_Q,r0,r1)
#define _VABS_S32(r0,r1)		arm_vo_dd(ARM_VABS_I|ARM_V_S32,r0,r1)
#define _VABSQ_S32(r0,r1)		arm_vo_qq(ARM_VABS_I|ARM_V_S32|ARM_V_Q,r0,r1)
#define _VQABS_S8(r0,r1)		arm_vo_dd(ARM_VQABS_I,r0,r1)
#define _VQABSQ_S8(r0,r1)		arm_vo_qq(ARM_VQABS_I|ARM_V_Q,r0,r1)
#define _VQABS_S16(r0,r1)		arm_vo_dd(ARM_VQABS_I|ARM_V_S16,r0,r1)
#define _VQABSQ_S16(r0,r1)		arm_vo_qq(ARM_VQABS_I|ARM_V_S16|ARM_V_Q,r0,r1)
#define _VQABS_S32(r0,r1)		arm_vo_dd(ARM_VQABS_I|ARM_V_S32,r0,r1)
#define _VQABSQ_S32(r0,r1)		arm_vo_qq(ARM_VQABS_I|ARM_V_S32|ARM_V_Q,r0,r1)
#define _VNEG_S8(r0,r1)			arm_vo_dd(ARM_VNEG_I,r0,r1)
#define _VNEGQ_S8(r0,r1)		arm_vo_qq(ARM_VNEG_I|ARM_V_Q,r0,r1)
#define _VNEG_S16(r0,r1)		arm_vo_dd(ARM_VNEG_I|ARM_V_S16,r0,r1)
#define _VNEGQ_S16(r0,r1)		arm_vo_qq(ARM_VNEG_I|ARM_V_S16|ARM_V_Q,r0,r1)
#define _VNEG_S32(r0,r1)		arm_vo_dd(ARM_VNEG_I|ARM_V_S32,r0,r1)
#define _VNEGQ_S32(r0,r1)		arm_vo_qq(ARM_VNEG_I|ARM_V_S32|ARM_V_Q,r0,r1)
#define _VQNEG_S8(r0,r1)		arm_vo_dd(ARM_VQNEG_I,r0,r1)
#define _VQNEGQ_S8(r0,r1)		arm_vo_qq(ARM_VQNEG_I|ARM_V_Q,r0,r1)
#define _VQNEG_S16(r0,r1)		arm_vo_dd(ARM_VQNEG_I|ARM_V_S16,r0,r1)
#define _VQNEGQ_S16(r0,r1)		arm_vo_qq(ARM_VQNEG_I|ARM_V_S16|ARM_V_Q,r0,r1)
#define _VQNEG_S32(r0,r1)		arm_vo_dd(ARM_VQNEG_I|ARM_V_S32,r0,r1)
#define _VQNEGQ_S32(r0,r1)		arm_vo_qq(ARM_VQNEG_I|ARM_V_S32|ARM_V_Q,r0,r1)
#define _VAND(r0,r1,r2)			arm_voddd(ARM_VAND,r0,r1,r2)
#define _VANDQ(r0,r1,r2)		arm_voqqq(ARM_VAND|ARM_V_Q,r0,r1,r2)
#define _VBIC(r0,r1,r2)			arm_voddd(ARM_VBIC,r0,r1,r2)
#define _VBICQ(r0,r1,r2)		arm_voqqq(ARM_VBIC|ARM_V_Q,r0,r1,r2)
#define _VORR(r0,r1,r2)			arm_voddd(ARM_VORR,r0,r1,r2)
#define _VORRQ(r0,r1,r2)		arm_voqqq(ARM_VORR|ARM_V_Q,r0,r1,r2)
#define _VORN(r0,r1,r2)			arm_voddd(ARM_VORN,r0,r1,r2)
#define _VORNQ(r0,r1,r2)		arm_voqqq(ARM_VORN|ARM_V_Q,r0,r1,r2)
#define _VEOR(r0,r1,r2)			arm_voddd(ARM_VEOR,r0,r1,r2)
#define _VEORQ(r0,r1,r2)		arm_voqqq(ARM_VEOR|ARM_V_Q,r0,r1,r2)
#define _VMOV(r0,r1)			_VORR(r0,r1,r1)
#define _VMOVQ(r0,r1)			_VORRQ(r0,r1,r1)
#define _VMOVL_S8(r0,r1)		arm_vo_qd(ARM_VMOVL_I|ARM_VMOVL_S8,r0,r1)
#define _VMOVL_U8(r0,r1)		arm_vo_qd(ARM_VMOVL_I|ARM_V_U|ARM_VMOVL_S8,r0,r1)
#define _VMOVL_S16(r0,r1)		arm_vo_qd(ARM_VMOVL_I|ARM_VMOVL_S16,r0,r1)
#define _VMOVL_U16(r0,r1)		arm_vo_qd(ARM_VMOVL_I|ARM_V_U|ARM_VMOVL_S16,r0,r1)
#define _VMOVL_S32(r0,r1)		arm_vo_qd(ARM_VMOVL_I|ARM_VMOVL_S32,r0,r1)
#define _VMOVL_U32(r0,r1)		arm_vo_qd(ARM_VMOVL_I|ARM_V_U|ARM_VMOVL_S32,r0,r1)
/* "oi" should be the result of encode_vfp_double */
#define _VIMM(oi,r0)			arm_vodi(oi,r0)
#define _VIMMQ(oi,r0)			arm_voqi(oi|ARM_V_Q,r0)
/* index is multipled by four */
#define _CC_VLDRN_F32(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VLDR,r0,r1,i0)
#define _VLDRN_F32(r0,r1,i0)		_CC_VLDRN_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDR_F32(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VLDR|ARM_P,r0,r1,i0)
#define _VLDR_F32(r0,r1,i0)		_CC_VLDR_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDRN_F64(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VLDR|ARM_V_F64,r0,r1,i0)
#define _VLDRN_F64(r0,r1,i0)		_CC_VLDRN_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VLDR_F64(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VLDR|ARM_V_F64|ARM_P,r0,r1,i0)
#define _VLDR_F64(r0,r1,i0)		_CC_VLDR_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTRN_F32(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VSTR,r0,r1,i0)
#define _VSTRN_F32(r0,r1,i0)		_CC_VSTRN_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTR_F32(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VSTR|ARM_P,r0,r1,i0)
#define _VSTR_F32(r0,r1,i0)		_CC_VSTR_F32(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTRN_F64(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VSTR|ARM_V_F64,r0,r1,i0)
#define _VSTRN_F64(r0,r1,i0)		_CC_VSTRN_F64(ARM_CC_AL,r0,r1,i0)
#define _CC_VSTR_F64(cc,r0,r1,i0)	arm_cc_vldst(cc,ARM_VSTR|ARM_V_F64|ARM_P,r0,r1,i0)
#define _VSTR_F64(r0,r1,i0)		_CC_VSTR_F64(ARM_CC_AL,r0,r1,i0)

/* from binutils */
#define rotate_left(v, n)	(v << n | v >> (32 - n))
static int
encode_arm_immediate(unsigned int v)
{
    unsigned int	a, i;

    for (i = 0; i < 32; i += 2)
	if ((a = rotate_left(v, i)) <= 0xff)
	    return (a | (i << 7));

    return (-1);
}

#define corrr(cc,o,rn,rd,rm)		_corrr(_jitp,cc,o,rn,rd,rm)
__jit_inline void
_corrr(jit_state_t _jitp, int cc, int o, int rn, int rd, int rm)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o & 0xf00fff0f));
    _jit_I(cc|o|(_u4(rn)<<16)|(_u4(rd)<<12)|_u4(rm));
}

#define corri(cc,o,rn,rd,im)		_corri(_jitp,cc,o,rn,rd,im)
__jit_inline void
_corri(jit_state_t _jitp, int cc, int o, int rn, int rd, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00fffff));
    jit_assert(!(im & 0xfffff000));
    _jit_I(cc|o|(_u4(rn)<<16)|(_u4(rd)<<12)|_u12(im));
}

#define coriw(cc,o,rd,im)			_coriw(_jitp,cc,o,rd,im)
__jit_inline void
_coriw(jit_state_t _jitp, int cc, int o, int rd, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00fffff));
    jit_assert(!(im & 0xffff0000));
    _jit_I(cc|o|((im&0xf000)<<4)|(_u4(rd)<<12)|(im&0xfff));
}

#define corri8(cc,o,rn,rt,im)	_corri8(_jitp,cc,o,rn,rt,im)
__jit_inline void
_corri8(jit_state_t _jitp, int cc, int o, int rn, int rt, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00fff0f));
    jit_assert(!(im & 0xffffff00));
    _jit_I(cc|o|(_u4(rn)<<16)|(_u4(rt)<<12)|((im&0xf0)<<4)|(im&0x0f));
}

#define corrrr(cc,o,rh,rl,rm,rn) _corrrr(_jitp,cc,o,rh,rl,rm,rn)
__jit_inline void
_corrrr(jit_state_t _jitp, int cc, int o, int rh, int rl, int rm, int rn)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00fff0f));
    _jit_I(cc|o|(_u4(rh)<<16)|(_u4(rl)<<12)|(_u4(rm)<<8)|_u4(rn));
}

#define corrrs(cc,o,rn,rd,rm,im)	_corrrs(_jitp,cc,o,rn,rd,rm,im)
__jit_inline void
_corrrs(jit_state_t _jitp, int cc, int o, int rn, int rd, int rm, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000ff8f));
    _jit_I(cc|o|(_u4(rd)<<12)|(_u4(rn)<<16)|(im<<7)|_u4(rm));
}

#define cshift(cc,o,rd,rm,rn,im)	_cshift(_jitp,cc,o,rd,rm,rn,im)
__jit_inline void
_cshift(jit_state_t _jitp, int cc, int o, int rd, int rm, int rn, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xffe0ff8f));
    jit_assert(((_u4(rm)<<8)&(im<<7)) == 0);
    _jit_I(cc|ARM_SHIFT|o|(_u4(rd)<<12)|(_u4(rm)<<8)|(im<<7)|_u4(rn));
}

#define cb(cc,o,im)			_cb(_jitp,cc,o,im)
__jit_inline void
_cb(jit_state_t _jitp, int cc, int o, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf0ffffff));
    _jit_I(cc|o|_u24(im));
}

#define cbx(cc,o,rm)			_cbx(_jitp,cc,o,rm)
__jit_inline void
_cbx(jit_state_t _jitp, int cc, int o, int rm)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000000f));
    _jit_I(cc|o|_u4(rm));
}

#define corl(cc,o,r0,i0)		_corl(_jitp,cc,o,r0,i0)
__jit_inline void
_corl(jit_state_t _jitp, int cc, int o, int r0, int i0)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00fffff));
    _jit_I(cc|o|(_u4(r0)<<16)|_u16(i0));
}

#define c6orr(cc,o,rd,rm)		_c6orr(_jitp,cc,o,rd,rm)
__jit_inline void
_c6orr(jit_state_t _jitp, int cc, int o, int rd, int rm)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf000f00f));
    _jit_I(cc|o|(_u4(rd)<<12)|_u4(rm));
}

#define arm_cc_pkh(cc,o,rn,rd,rm,im)	_arm_cc_pkh(_jitp,cc,o,rn,rd,rm,im)
__jit_inline void
_arm_cc_pkh(jit_state_t _jitp, int cc, int o, int rn, int rd, int rm, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(!(o  & 0xf00ff00f));
    _jit_I(cc|o|(_u4(rn)<<16)|(_u4(rd)<<12)|(_u5(im)<<7)|_u4(rm));
}


#define _CC_MOV(cc,rd,rm)	corrr(cc,ARM_MOV,0,rd,rm)
#define _MOV(rd,rm)		_CC_MOV(ARM_CC_AL,rd,rm)
#define T1_MOV(rd,rm)		_jit_W(THUMB_MOV|((_u4(rd)&8)<<4)|(_u4(rm)<<3)|(rd&7))
#define T2_MOV(rd,rm)		T2_ORR(rd,_R15,rm)
#define _CC_MOVI(cc,rd,im)	corri(cc,ARM_MOV|ARM_I,0,rd,im)
#define _MOVI(rd,im)		_CC_MOVI(ARM_CC_AL,rd,im)
#define _CC_MOVWI(cc,rd,im)	coriw(cc,ARM_MOVWI,rd,im)
#define _MOVWI(rd,im)		_CC_MOVWI(ARM_CC_AL,rd,im)
#define T1_MOVI(rd,im)		_jit_W(THUMB_MOVI|(_u3(rd)<<8)|_u8(im))
#define T2_MOVI(rd,im)		torri(THUMB2_MOVI,_R15,rd,im)
#define T2_MOVWI(rd,im)		toriw(THUMB2_MOVWI,rd,im)
#define _CC_MOVTI(cc,rd,im)	coriw(cc,ARM_MOVTI,rd,im)
#define _MOVTI(rd,im)		_CC_MOVTI(ARM_CC_AL,rd,im)
#define T2_MOVTI(rd,im)		toriw(THUMB2_MOVTI,rd,im)
#define _CC_MVN(cc,rd,rm)	corrr(cc,ARM_MVN,0,rd,rm)
#define _MVN(rd,rm)		_CC_MVN(ARM_CC_AL,rd,rm)
#define T1_MVN(rd,rm)		_jit_W(THUMB_MVN|(_u3(rm)<<3)|_u3(rd))
#define T2_MVN(rd,rm)		torrr(THUMB2_MVN,rd,_R15,rm)
#define _CC_MVNI(cc,rd,im)	corri(cc,ARM_MVN|ARM_I,0,rd,im)
#define _MVNI(rd,im)		_CC_MVNI(ARM_CC_AL,rd,im)
#define T2_MVNI(rd,im)		torri(THUMB2_MVNI,_R15,rd,im)
#define _CC_NOT(cc,rd,rm)	_CC_MVN(cc,rd,rm)
#define _NOT(rd,rm)		_CC_NOT(ARM_CC_AL,rd,rm)
#define T1_NOT(rd,rm)		T1_MVN(rd,rm)
#define T2_NOT(rd,rm)		T2_MVN(rd,rm)
#define _NOP()			_MOV(_R0, _R0)
#define T1_NOP()		_jit_W(0xbf00)

#define _CC_ADD(cc,rd,rn,rm)	corrr(cc,ARM_ADD,rn,rd,rm)
#define _ADD(rd,rn,rm)		_CC_ADD(ARM_CC_AL,rd,rn,rm)
#define T1_ADD(rd,rn,rm)	_jit_W(THUMB_ADD|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rd))
#define T1_ADDX(rdn,rm)		_jit_W(THUMB_ADDX|((_u4(rdn)&8)<<4)|(_u4(rm)<<3)|(rdn&7))
#define T2_ADD(rd,rn,rm)	torrr(THUMB2_ADD,rn,rd,rm)
#define _CC_ADDI(cc,rd,rn,im)	corri(cc,ARM_ADD|ARM_I,rn,rd,im)
#define _ADDI(rd,rn,im)		_CC_ADDI(ARM_CC_AL,rd,rn,im)
#define T1_ADDI3(rd,rn,im)	_jit_W(THUMB_ADDI3|(_u3(im)<<6)|(_u3(rn)<<3)|_u3(rd))
#define T1_ADDI8(rdn,im)	_jit_W(THUMB_ADDI8|(_u3(rdn)<<8)|_u8(im))
#define T2_ADDI(rd,rn,im)	torri(THUMB2_ADDI,rn,rd,im)
#define T2_ADDWI(rd,rn,im)	torri(THUMB2_ADDWI,rn,rd,im)
#define _CC_ADDS(cc,rd,rn,rm)	corrr(cc,ARM_ADD|ARM_S,rn,rd,rm)
#define _ADDS(rd,rn,rm)		_CC_ADDS(ARM_CC_AL,rd,rn,rm)
#define T2_ADDS(rd,rn,rm)	torrr(THUMB2_ADD|ARM_S,rn,rd,rm)
#define _ADDSI(rd,rn,im)	corri(ARM_CC_AL,ARM_ADD|ARM_S|ARM_I,rn,rd,im)
#define T2_ADDSI(rd,rn,im)	torri(THUMB2_ADDI|ARM_S,rn,rd,im)
#define _CC_ADC(cc,rd,rn,rm)	corrr(cc,ARM_ADC,rn,rd,rm)
#define _ADC(rd,rn,rm)		_CC_ADC(ARM_CC_AL,rd,rn,rm)
#define T1_ADC(rdn,rm)		_jit_W(THUMB_ADC|(_u3(rm)<<3)|_u3(rdn))
#define T2_ADC(rd,rn,rm)	torrr(THUMB2_ADC,rn,rd,rm)
#define _CC_ADCI(cc,rd,rn,im)	corri(cc,ARM_ADC|ARM_I,rn,rd,im)
#define _ADCI(rd,rn,im)		_CC_ADCI(ARM_CC_AL,rd,rn,im)
#define T2_ADCI(rd,rn,im)	torri(THUMB2_ADCI,rn,rd,im)
#define _CC_ADCS(cc,rd,rn,rm)	corrr(cc,ARM_ADC|ARM_S,rn,rd,rm)
#define _ADCS(rd,rn,rm)		_CC_ADCS(ARM_CC_AL,rd,rn,rm)
#define T2_ADCS(rd,rn,rm)	torrr(THUMB2_ADC|ARM_S,rn,rd,rm)
#define _CC_ADCSI(cc,rd,rn,im)	corri(cc,ARM_ADC|ARM_S|ARM_I,rn,rd,im)
#define _ADCSI(rd,rn,im)	_CC_ADCSI(ARM_CC_AL,rd,rn,im)
#define T2_ADCSI(rd,rn,im)	torri(THUMB2_ADCI|ARM_S,rn,rd,im)
#define _CC_SUB(cc,rd,rn,rm)	corrr(cc,ARM_SUB,rn,rd,rm)
#define _SUB(rd,rn,rm)		_CC_SUB(ARM_CC_AL,rd,rn,rm)
#define T1_SUB(rd,rn,rm)	_jit_W(THUMB_SUB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rd))
#define T2_SUB(rd,rn,rm)	torrr(THUMB2_SUB,rn,rd,rm)
#define _CC_SUBI(cc,rd,rn,im)	corri(cc,ARM_SUB|ARM_I,rn,rd,im)
#define _SUBI(rd,rn,im)		_CC_SUBI(ARM_CC_AL,rd,rn,im)
#define T1_SUBI3(rd,rn,im)	_jit_W(THUMB_SUBI3|(_u3(im)<<6)|(_u3(rn)<<3)|_u3(rd))
#define T1_SUBI8(rdn,im)	_jit_W(THUMB_SUBI8|(_u3(rdn)<<8)|_u8(im))
#define T2_SUBI(rd,rn,im)	torri(THUMB2_SUBI,rn,rd,im)
#define T2_SUBWI(rd,rn,im)	torri(THUMB2_SUBWI,rn,rd,im)
#define _CC_SUBS(cc,rd,rn,rm)	corrr(cc,ARM_SUB|ARM_S,rn,rd,rm)
#define _SUBS(rd,rn,rm)		_CC_SUBS(ARM_CC_AL,rd,rn,rm)
#define T2_SUBS(rd,rn,rm)	torrr(THUMB2_SUB|ARM_S,rn,rd,rm)
#define _CC_SUBSI(cc,rd,rn,im)	corri(cc,ARM_SUB|ARM_S|ARM_I,rn,rd,im)
#define _SUBSI(rd,rn,im)	_CC_SUBSI(ARM_CC_AL,rd,rn,im)
#define T2_SUBSI(rd,rn,im)	torri(THUMB2_SUBI|ARM_S,rn,rd,im)
#define _CC_SBC(cc,rd,rn,rm)	corrr(cc,ARM_SBC,rn,rd,rm)
#define _SBC(rd,rn,rm)		_CC_SBC(ARM_CC_AL,rd,rn,rm)
#define T1_SBC(rdn,rm)		_jit_W(THUMB_SBC|(_u3(rm)<<3)|_u3(rdn))
#define T2_SBC(rd,rn,rm)	torrr(THUMB2_SBC,rn,rd,rm)
#define _CC_SBCI(cc,rd,rn,im)	corri(cc,ARM_SBC|ARM_I,rn,rd,im)
#define _SBCI(rd,rn,im)		_CC_SBCI(ARM_CC_AL,rd,rn,im)
#define T2_SBCI(rd,rn,im)	torri(THUMB2_SBCI,rn,rd,im)
#define _CC_SBCS(cc,rd,rn,rm)	corrr(cc,ARM_SBC|ARM_S,rn,rd,rm)
#define _SBCS(rd,rn,rm)		_CC_SBCS(ARM_CC_AL,rd,rn,rm)
#define T2_SBCS(rd,rn,rm)	torrr(THUMB2_SBC|ARM_S,rn,rd,rm)
#define _CC_SBCSI(cc,rd,rn,im)	corri(cc,ARM_SBC|ARM_S|ARM_I,rn,rd,im)
#define _SBCSI(rd,rn,im)	_CC_SBCSI(ARM_CC_AL,rd,rn,im)
#define T2_SBCSI(rd,rn,im)	torri(THUMB2_SBCI|ARM_S,rn,rd,im)
#define _CC_RSB(cc,rd,rn,rm)	corrr(cc,ARM_RSB,rn,rd,rm)
#define _RSB(rd,rn,rm)		_CC_RSB(ARM_CC_AL,rd,rn,rm)
#define T2_RSB(rd,rn,rm)	torrr(THUMB2_RSB,rn,rd,rm)
#define _CC_RSBI(cc,rd,rn,im)	corri(cc,ARM_RSB|ARM_I,rn,rd,im)
#define _RSBI(rd,rn,im)		_CC_RSBI(ARM_CC_AL,rd,rn,im)
#define T1_RSBI(rd,rn)		_jit_W(THUMB_RSBI|(_u3(rn)<<3)|_u3(rd))
#define T2_RSBI(rd,rn,im)	torri(THUMB2_RSBI,rn,rd,im)

#define _CC_MUL(cc,rl,rn,rm)	corrrr(cc,ARM_MUL,rl,0,rm,rn)
#define _MUL(rl,rn,rm)		_CC_MUL(ARM_CC_AL,rl,rn,rm)
#define T1_MUL(rdm,rn)		_jit_W(THUMB_MUL|(_u3(rn)<<3)|_u3(rdm))
#define T2_MUL(rd,rn,rm)	torrr(THUMB2_MUL,rn,rd,rm)
#define _CC_SMULL(cc,rl,rh,rn,rm)	corrrr(cc,ARM_SMULL,rh,rl,rm,rn)
#define _SMULL(rl,rh,rn,rm)	_CC_SMULL(ARM_CC_AL,rl,rh,rn,rm)
#define T2_SMULL(rl,rh,rn,rm)	torrrr(THUMB2_SMULL,rn,rl,rh,rm)
#define _CC_UMULL(cc,rl,rh,rn,rm)	corrrr(cc,ARM_UMULL,rh,rl,rm,rn)
#define _UMULL(rl,rh,rn,rm)	_CC_UMULL(ARM_CC_AL,rl,rh,rn,rm)
#define T2_UMULL(rl,rh,rn,rm)	torrrr(THUMB2_UMULL,rn,rl,rh,rm)
#define T2_SDIV(rd,rn,rm)	torrr(THUMB2_SDIV,rn,rd,rm)
#define T2_UDIV(rd,rn,rm)	torrr(THUMB2_UDIV,rn,rd,rm)

#define _CC_MULS(cc,rd,rn,rm)   corrrr(cc,ARM_MUL|ARM_S,rd,0,rm,rn)
#define _MULS(rd,rn,rm)         _CC_MULS(ARM_CC_AL,rd,rn,rm)
#define T2_MULS(rd,rn,rm)       torrr(THUMB2_ADD|ARM_S,rn,rd,rm)

#define _CC_AND(cc,rd,rn,rm)	corrr(cc,ARM_AND,rn,rd,rm)
#define _AND(rd,rn,rm)		_CC_AND(ARM_CC_AL,rd,rn,rm)
#define T1_AND(rdn,rm)		_jit_W(THUMB_AND|(_u3(rm)<<3)|_u3(rdn))
#define T2_AND(rd,rn,rm)	torrr(THUMB2_AND,rn,rd,rm)
#define _CC_ANDI(cc,rd,rn,im)	corri(cc,ARM_AND|ARM_I,rn,rd,im)
#define _ANDI(rd,rn,im)		_CC_ANDI(ARM_CC_AL,rd,rn,im)
#define T2_ANDI(rd,rn,im)	torri(THUMB2_ANDI,rn,rd,im)
#define _CC_ANDS(cc,rd,rn,rm)	corrr(cc,ARM_AND|ARM_S,rn,rd,rm)
#define _ANDS(rd,rn,rm)		_CC_ANDS(ARM_CC_AL,rd,rn,rm)
#define T2_ANDS(rd,rn,rm)	torrr(THUMB2_AND|ARM_S,rn,rd,rm)
#define _CC_ANDSI(cc,rd,rn,im)	corri(cc,ARM_AND|ARM_S|ARM_I,rn,rd,im)
#define _ANDSI(rd,rn,im)	_CC_ANDSI(ARM_CC_AL,rd,rn,im)
#define T2_ANDSI(rd,rn,im)	torri(ARM_CC_AL,THUMB2_ANDI|ARM_S,rn,rd,im)
#define _CC_BIC(cc,rd,rn,rm)	corrr(cc,ARM_BIC,rn,rd,rm)
#define _BIC(rd,rn,rm)		_CC_BIC(ARM_CC_AL,rd,rn,rm)
#define T2_BIC(rd,rn,rm)	torrr(THUMB2_BIC,rn,rd,rm)
#define _CC_BICI(cc,rd,rn,im)	corri(cc,ARM_BIC|ARM_I,rn,rd,im)
#define _BICI(rd,rn,im)		_CC_BICI(ARM_CC_AL,rd,rn,im)
#define T2_BICI(rd,rn,im)	torri(THUMB2_BICI,rn,rd,im)
#define _CC_BICS(cc,rd,rn,rm)	corrr(cc,ARM_BIC|ARM_S,rn,rd,rm)
#define _BICS(rd,rn,rm)		_CC_BICS(ARM_CC_AL,rd,rn,rm)
#define T2_BICS(rd,rn,rm)	torrr(THUMB2_BIC|ARM_S,rn,rd,rm)
#define _CC_BICSI(cc,rd,rn,im)	corri(cc,ARM_BIC|ARM_S|ARM_I,rn,rd,im)
#define _BICSI(rd,rn,im)	_CC_BICSI(ARM_CC_AL,rd,rn,im)
#define T2_BICSI(rd,rn,im)	torri(ARM_CC_AL,THUMB2_BICI|ARM_S,rn,rd,im)
#define _CC_ORR(cc,rd,rn,rm)	corrr(cc,ARM_ORR,rn,rd,rm)
#define _ORR(rd,rn,rm)		_CC_ORR(ARM_CC_AL,rd,rn,rm)
#define T1_ORR(rdn,rm)		_jit_W(THUMB_ORR|(_u3(rm)<<3)|_u3(rdn))
#define T2_ORR(rd,rn,rm)	torrr(THUMB2_ORR,rn,rd,rm)
#define _CC_ORR_SI(cc,rd,rn,rm,sh,im)					\
    corrrs(cc,ARM_ORR|sh,rn,rd,rm,im)
#define _ORR_SI(rd,rn,rm,sh,im)	_CC_ORR_SI(ARM_CC_AL,rd,rn,rm,sh,im)
#define _CC_ORRI(cc,rd,rn,im)	corri(cc,ARM_ORR|ARM_I,rn,rd,im)
#define _ORRI(rd,rn,im)		_CC_ORRI(ARM_CC_AL,rd,rn,im)
#define T2_ORRI(rd,rn,im)	torri(THUMB2_ORRI,rn,rd,im)
#define _CC_EOR(cc,rd,rn,rm)	corrr(cc,ARM_EOR,rn,rd,rm)
#define _EOR(rd,rn,rm)		_CC_EOR(ARM_CC_AL,rd,rn,rm)
#define T1_EOR(rdn,rm)		_jit_W(THUMB_EOR|(_u3(rm)<<3)|_u3(rdn))
#define T2_EOR(rd,rn,rm)	torrr(THUMB2_EOR,rn,rd,rm)
#define _CC_EOR_SI(cc,rd,rn,rm,sh,im)					\
    corrrs(cc,ARM_EOR|sh,rn,rd,rm,im)
#define _EOR_SI(rd,rn,rm,sh,im)	_CC_EOR_SI(ARM_CC_AL,rd,rn,rm,sh,im)
#define _CC_EORI(cc,rd,rn,im)	corri(cc,ARM_EOR|ARM_I,rn,rd,im)
#define _EORI(rd,rn,im)		_CC_EORI(ARM_CC_AL,rd,rn,im)
#define T2_EORI(rd,rn,im)	torri(THUMB2_EORI,rn,rd,im)

#define _CC_REV(cc,rd,rm)	c6orr(cc,ARM_REV,rd,rm)
#define _REV(rd,rm)		_CC_REV(ARM_CC_AL,rd,rm)
#define T1_REV(rd,rm)		_jit_W(THUMB_REV|(_u3(rm)<<3)|_u3(rd))
#define T2_REV(rd,rm)		torrr(THUMB2_REV,rm,rd,rm)
#define _CC_REV16(cc,rd,rm)	c6orr(cc,ARM_REV16,rd,rm)
#define _REV16(rd,rm)		_CC_REV16(ARM_CC_AL,rd,rm)
#define T1_REV16(rd,rm)		_jit_W(THUMB_REV16|(_u3(rm)<<3)|_u3(rd))
#define T2_REV16(rd,rm)		torrr(THUMB2_REV16,rm,rd,rm)
#define _CC_SXTB(cc,rd,rm)	c6orr(cc,ARM_SXTB,rd,rm)
#define _SXTB(rd,rm)		_CC_SXTB(ARM_CC_AL,rd,rm)
#define T1_SXTB(rd,rm)		_jit_W(THUMB_SXTB|(_u3(rm)<<3)|_u3(rd))
#define T2_SXTB(rd,rm)		torrr(THUMB2_SXTB,_R15,rd,rm)
#define _CC_UXTB(cc,rd,rm)	c6orr(cc,ARM_UXTB,rd,rm)
#define _UXTB(rd,rm)		_CC_UXTB(ARM_CC_AL,rd,rm)
#define T1_UXTB(rd,rm)		_jit_W(THUMB_UXTB|(_u3(rm)<<3)|_u3(rd))
#define T2_UXTB(rd,rm)		torrr(THUMB2_UXTB,_R15,rd,rm)
#define _CC_SXTH(cc,rd,rm)	c6orr(cc,ARM_SXTH,rd,rm)
#define _SXTH(rd,rm)		_CC_SXTH(ARM_CC_AL,rd,rm)
#define T1_SXTH(rd,rm)		_jit_W(THUMB_SXTH|(_u3(rm)<<3)|_u3(rd))
#define T2_SXTH(rd,rm)		torrr(THUMB2_SXTH,_R15,rd,rm)
#define _CC_UXTH(cc,rd,rm)	c6orr(cc,ARM_UXTH,rd,rm)
#define _UXTH(rd,rm)		_CC_UXTH(ARM_CC_AL,rd,rm)
#define T1_UXTH(rd,rm)		_jit_W(THUMB_UXTH|(_u3(rm)<<3)|_u3(rd))
#define T2_UXTH(rd,rm)		torrr(THUMB2_UXTH,_R15,rd,rm)

#define _CC_SHIFT(cc,o,rd,rm,rn,im) cshift(cc,o,rd,rm,rn,im)
#define _CC_LSL(cc,rd,rn,rm)	_CC_SHIFT(cc,ARM_LSL|ARM_R,rd,rm,rn,0)
#define _LSL(rd,rn,rm)		_CC_LSL(ARM_CC_AL,rd,rn,rm)
#define T1_LSL(rdn,rm)		_jit_W(THUMB_LSL|(_u3(rm)<<3)|_u3(rdn))
#define T2_LSL(rd,rn,rm)	torrr(THUMB2_LSL,rn,rd,rm)
#define _CC_LSLI(cc,rd,rn,im)	_CC_SHIFT(cc,ARM_LSL,rd,0,rn,im)
#define _LSLI(rd,rn,im)		_CC_LSLI(ARM_CC_AL,rd,rn,im)
#define T1_LSLI(rd,rm,im)	_jit_W(THUMB_LSLI|(_u5(im)<<6)|(_u3(rm)<<3)|_u3(rd))
#define T2_LSLI(rd,rm,im)	tshift(THUMB2_LSLI,rd,rm,im)
#define _CC_LSR(cc,rd,rn,rm)	_CC_SHIFT(cc,ARM_LSR|ARM_R,rd,rm,rn,0)
#define _LSR(rd,rn,rm)		_CC_LSR(ARM_CC_AL,rd,rn,rm)
#define T1_LSR(rdn,rm)		_jit_W(THUMB_LSR|(_u3(rm)<<3)|_u3(rdn))
#define T2_LSR(rd,rn,rm)	torrr(THUMB2_LSR,rn,rd,rm)
#define _CC_LSRI(cc,rd,rn,im)	_CC_SHIFT(cc,ARM_LSR,rd,0,rn,im)
#define _LSRI(rd,rn,im)		_CC_LSRI(ARM_CC_AL,rd,rn,im)
#define T1_LSRI(rd,rm,im)	_jit_W(THUMB_LSRI|(_u5(im)<<6)|(_u3(rm)<<3)|_u3(rd))
#define T2_LSRI(rd,rm,im)	tshift(THUMB2_LSRI,rd,rm,im)
#define _CC_ASR(cc,rd,rn,rm)	_CC_SHIFT(cc,ARM_ASR|ARM_R,rd,rm,rn,0)
#define _ASR(rd,rn,rm)		_CC_ASR(ARM_CC_AL,rd,rn,rm)
#define T1_ASR(rdn,rm)		_jit_W(THUMB_ASR|(_u3(rm)<<3)|_u3(rdn))
#define T2_ASR(rd,rn,rm)	torrr(THUMB2_ASR,rn,rd,rm)
#define _CC_ASRI(cc,rd,rn,im)	_CC_SHIFT(cc,ARM_ASR,rd,0,rn,im)
#define _ASRI(rd,rn,im)		_CC_ASRI(ARM_CC_AL,rd,rn,im)
#define T1_ASRI(rd,rm,im)	_jit_W(THUMB_ASRI|(_u5(im)<<6)|(_u3(rm)<<3)|_u3(rd))
#define T2_ASRI(rd,rm,im)	tshift(THUMB2_ASRI,rd,rm,im)

#define _CC_CMP(cc,rn,rm)	corrr(cc,ARM_CMP,rn,0,rm)
#define _CC_CMPSH(cc,rn,rm,t,s) corrrs(cc,ARM_CMP|t,rn,0,rm,s)
#define _CMP(rn,rm)		_CC_CMP(ARM_CC_AL,rn,rm)
#define T1_CMP(rn,rm)		_jit_W(THUMB_CMP|(_u3(rm)<<3)|_u3(rn))
#define T1_CMPX(rn,rm)		_jit_W(THUMB_CMPX|((_u4(rn)&8)<<4)|(_u4(rm)<<3)|(rn&7))
#define T2_CMP(rn,rm)		torrr(THUMB2_CMP,rn,_R15,rm)
#define _CC_CMPI(cc,rn,im)	corri(cc,ARM_CMP|ARM_I,rn,0,im)
#define _CMPI(rn,im)		_CC_CMPI(ARM_CC_AL,rn,im)
#define T1_CMPI(rn,im)		_jit_W(THUMB_CMPI|(_u3(rn)<<8)|_u8(im))
#define T2_CMPI(rn,im)		torri(THUMB2_CMPI,rn,_R15,im)
#define _CC_CMN(cc,rn,rm)	corrr(cc,ARM_CMN,rn,0,rm)
#define _CMN(rn,rm)		_CC_CMN(ARM_CC_AL,rn,rm)
#define T1_CMN(rn,rm)		_jit_W(THUMB_CMN|(_u3(rm)<<3)|_u3(rm))
#define T2_CMN(rn,rm)		torrr(THUMB2_CMN,rn,_R15,rm)
#define _CC_CMNI(cc,rn,im)	corri(cc,ARM_CMN|ARM_I,rn,0,im)
#define _CMNI(rn,im)		_CC_CMNI(ARM_CC_AL,rn,im)
#define T2_CMNI(rn,im)		torri(THUMB2_CMNI,rn,_R15,im)
#define _CC_TST(cc,rn,rm)	corrr(cc,ARM_TST,rn,r0,rm)
#define _TST(rn,rm)		_CC_TST(ARM_CC_AL,rn,rm)
#define T1_TST(rn,rm)		_jit_W(THUMB_TST|(_u3(rm)<<3)|_u3(rn))
#define T2_TST(rn,rm)		torrr(THUMB2_TST,rn,_R15,rm)
#define _CC_TSTI(cc,rn,im)	corri(cc,ARM_TST|ARM_I,rn,0,im)
#define _TSTI(rn,im)		_CC_TSTI(ARM_CC_AL,rn,im)
#define T2_TSTI(rn,im)		torri(THUMB2_TSTI,rn,_R15,im)
#define _CC_TEQ(cc,rn,rm)	corrr(cc,ARM_TEQ,rn,0,rm)
#define _TEQ(rn,rm)		_CC_TEQ(ARM_CC_AL,rn,rm)
#define _CC_TEQI(cc,rm,im)	corri(cc,ARM_TEQ|ARM_I,rn,0,im)
#define _TEQI(rn,im)		_CC_TEQI(ARM_CC_AL,rn,im)

#define _CC_BX(cc,rm)		cbx(cc,ARM_BX,rm)
#define _BX(rm)			_CC_BX(ARM_CC_AL,rm)
#define T1_BX(rm)		_jit_W(0x4700|(_u4(rm)<<3))
#define _CC_BLX(cc,rm)		cbx(cc,ARM_BLX,rm)
#define _BLX(rm)		_CC_BLX(ARM_CC_AL,rm)
#define T1_BLX(rm)		_jit_W(THUMB_BLX|(_u4(rm)<<3))
#define _BLXI(im)		blxi(im)
#define T2_BLXI(im)		tb(THUMB2_BLXI,im)
#define _CC_B(cc,im)		cb(cc,ARM_B,im)
#define _B(im)			_CC_B(ARM_CC_AL,im)
#define T1_CC_B(cc,im)		tc8(cc,im)
#define T1_B(im)		t11(im)
#define T2_CC_B(cc,im)		tcb(cc,im)
#define T2_B(im)		tb(THUMB2_B,im)
#define _CC_BLI(cc,im)		cb(cc,ARM_BLI,im)
#define _BLI(im)		_CC_BLI(ARM_CC_AL,im)
#define T2_BLI(im)		tb(THUMB2_BLI,im)

#define _CC_LDRSB(cc,rt,rn,rm)	corrr(cc,ARM_LDRSB|ARM_P,rn,rt,rm)
#define _LDRSB(rt,rn,rm)	_CC_LDRSB(ARM_CC_AL,rt,rn,rm)
#define T1_LDRSB(rt,rn,rm)	_jit_W(THUMB_LDRSB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_LDRSB(rt,rn,rm)	torxr(THUMB2_LDRSB,rn,rt,rm)
#define _CC_LDRSBN(cc,rt,rn,rm)	corrr(cc,ARM_LDRSB,rn,rt,rm)
#define _LDRSBN(rt,rn,rm)	_CC_LDRSBN(ARM_CC_AL,rt,rn,rm)
#define _CC_LDRSBI(cc,rt,rn,im)	corri8(cc,ARM_LDRSBI|ARM_P,rn,rt,im)
#define _LDRSBI(rt,rn,im)	_CC_LDRSBI(ARM_CC_AL,rt,rn,im)
#define T2_LDRSBI(rt,rn,im)	torri8(THUMB2_LDRSBI|THUMB2_U,rn,rt,im)
#define T2_LDRSBWI(rt,rn,im)	torri12(THUMB2_LDRSBWI,rn,rt,im)
#define _CC_LDRSBIN(cc,rt,rn,im)	corri8(cc,ARM_LDRSBI,rn,rt,im)
#define _LDRSBIN(rt,rn,im)	_CC_LDRSBIN(ARM_CC_AL,rt,rn,im)
#define T2_LDRSBIN(rt,rn,im)	torri8(THUMB2_LDRSBI,rn,rt,im)
#define _CC_LDRB(cc,rt,rn,rm)	corrr(cc,ARM_LDRB|ARM_P,rn,rt,rm)
#define _LDRB(rt,rn,rm)		_CC_LDRB(ARM_CC_AL,rt,rn,rm)
#define T1_LDRB(rt,rn,rm)	_jit_W(THUMB_LDRB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_LDRB(rt,rn,rm)	torxr(THUMB2_LDRB,rn,rt,rm)
#define _CC_LDRBN(cc,rt,rn,rm)	corrr(cc,ARM_LDRB,rn,rt,rm)
#define _LDRBN(rt,rn,rm)	_CC_LDRBN(ARM_CC_AL,rt,rn,rm)
#define _CC_LDRBI(cc,rt,rn,im)	corri(cc,ARM_LDRBI|ARM_P,rn,rt,im)
#define _LDRBI(rt,rn,im)	_CC_LDRBI(ARM_CC_AL,rt,rn,im)
#define T1_LDRBI(rt,rn,im)	_jit_W(THUMB_LDRBI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_LDRBI(rt,rn,im)	torri8(THUMB2_LDRBI|THUMB2_U,rn,rt,im)
#define T2_LDRBWI(rt,rn,im)	torri12(THUMB2_LDRBWI,rn,rt,im)
#define _CC_LDRBIN(cc,rt,rn,im)	corri(cc,ARM_LDRBI,rn,rt,im)
#define _LDRBIN(rt,rn,im)	_CC_LDRBIN(ARM_CC_AL,rt,rn,im)
#define T2_LDRBIN(rt,rn,im)	torri8(THUMB2_LDRBI,rn,rt,im)
#define _CC_LDRSH(cc,rt,rn,rm)	corrr(cc,ARM_LDRSH|ARM_P,rn,rt,rm)
#define _LDRSH(rt,rn,rm)	_CC_LDRSH(ARM_CC_AL,rt,rn,rm)
#define T1_LDRSH(rt,rn,rm)	_jit_W(THUMB_LDRSH|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_LDRSH(rt,rn,rm)	torxr(THUMB2_LDRSH,rn,rt,rm)
#define _CC_LDRSHN(cc,rt,rn,rm)	corrr(cc,ARM_LDRSH,rn,rt,rm)
#define _LDRSHN(rt,rn,rm)	_CC_LDRSHN(ARM_CC_AL,rt,rn,rm)
#define _CC_LDRSHI(cc,rt,rn,im)	corri8(cc,ARM_LDRSHI|ARM_P,rn,rt,im)
#define _LDRSHI(rt,rn,im)	_CC_LDRSHI(ARM_CC_AL,rt,rn,im)
#define T2_LDRSHI(rt,rn,im)	torri8(THUMB2_LDRSHI|THUMB2_U,rn,rt,im)
#define T2_LDRSHWI(rt,rn,im)	torri12(THUMB2_LDRSHWI,rn,rt,im)
#define _CC_LDRSHIN(cc,rt,rn,im)	corri8(cc,ARM_LDRSHI,rn,rt,im)
#define _LDRSHIN(rt,rn,im)	_CC_LDRSHIN(ARM_CC_AL,rt,rn,im)
#define T2_LDRSHIN(rt,rn,im)	torri8(THUMB2_LDRSHI,rn,rt,im)
#define _CC_LDRH(cc,rt,rn,rm)	corrr(cc,ARM_LDRH|ARM_P,rn,rt,rm)
#define _LDRH(rt,rn,rm)		_CC_LDRH(ARM_CC_AL,rt,rn,rm)
#define T1_LDRH(rt,rn,rm)	_jit_W(THUMB_LDRH|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_LDRH(rt,rn,rm)	torxr(THUMB2_LDRH,rn,rt,rm)
#define _CC_LDRHN(cc,rt,rn,rm)	corrr(cc,ARM_LDRH,rn,rt,rm)
#define _LDRHN(rt,rn,rm)	CC_LDRHN(ARM_CC_AL,rt,rn,rm)
#define _CC_LDRHI(cc,rt,rn,im)	corri8(cc,ARM_LDRHI|ARM_P,rn,rt,im)
#define _LDRHI(rt,rn,im)	_CC_LDRHI(ARM_CC_AL,rt,rn,im)
#define T1_LDRHI(rt,rn,im)	_jit_W(THUMB_LDRHI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_LDRHI(rt,rn,im)	torri8(THUMB2_LDRHI|THUMB2_U,rn,rt,im)
#define T2_LDRHWI(rt,rn,im)	torri12(THUMB2_LDRHWI,rn,rt,im)
#define _CC_LDRHIN(cc,rt,rn,im)	corri8(cc,ARM_LDRHI,rn,rt,im)
#define _LDRHIN(rt,rn,im)	_CC_LDRHIN(ARM_CC_AL,rt,rn,im)
#define T2_LDRHIN(rt,rn,im)	torri8(THUMB2_LDRHI,rn,rt,im)
#define _CC_LDR(cc,rt,rn,rm)	corrr(cc,ARM_LDR|ARM_P,rn,rt,rm)
#define _LDR(rt,rn,rm)		_CC_LDR(ARM_CC_AL,rt,rn,rm)
#define T1_LDR(rt,rn,rm)	_jit_W(THUMB_LDR|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_LDR(rt,rn,rm)	torxr(THUMB2_LDR,rn,rt,rm)
#define _CC_LDRN(cc,rt,rn,rm)	corrr(cc,ARM_LDR,rn,rt,rm)
#define _LDRN(rt,rn,rm)		_CC_LDRN(ARM_CC_AL,rt,rn,rm)
#define _CC_LDRI(cc,rt,rn,im)	corri(cc,ARM_LDRI|ARM_P,rn,rt,im)
#define _LDRI(rt,rn,im)		_CC_LDRI(ARM_CC_AL,rt,rn,im)
#define T1_LDRI(rt,rn,im)	_jit_W(THUMB_LDRI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T1_LDRISP(rt,im)	_jit_W(THUMB_LDRISP|(_u3(rt)<<8)|_u8(im))
#define T2_LDRI(rt,rn,im)	torri8(THUMB2_LDRI|THUMB2_U,rn,rt,im)
#define T2_LDRWI(rt,rn,im)	torri12(THUMB2_LDRWI,rn,rt,im)
#define _CC_LDRIN(cc,rt,rn,im)	corri(cc,ARM_LDRI,rn,rt,im)
#define _LDRIN(rt,rn,im)	_CC_LDRIN(ARM_CC_AL,rt,rn,im)
#define T2_LDRIN(rt,rn,im)	torri8(THUMB2_LDRI,rn,rt,im)
#define _CC_LDRD(cc,rt,rn,rm)	corrr(cc,ARM_LDRD|ARM_P,rn,rt,rm)
#define _LDRD(rt,rn,rm)		_CC_LDRD(ARM_CC_AL,rt,rn,rm)
#define _CC_LDRDN(cc,rt,rn,rm)	corrr(cc,ARM_LDRD,rn,rt,rm)
#define _LDRDN(rd,rn,rm)	_CC_LDRDN(ARM_CC_AL,rn,rt,rm)
#define _CC_LDRDI(cc,rt,rn,im)	corri8(cc,ARM_LDRDI|ARM_P,rn,rt,im)
#define _LDRDI(rt,rn,im)	_CC_LDRDI(ARM_CC_AL,rt,rn,im)
#define T2_LDRDI(rt,rt2,rn,im)	torrri8(THUMB2_LDRDI|ARM_P,rn,rt,rt2,im)
#define _CC_LDRDIN(cc,rt,rn,im)	corri8(cc,ARM_LDRDI,rn,rt,im)
#define _LDRDIN(rt,rn,im)	_CC_LDRDIN(ARM_CC_AL,rt,rn,im)
#define T2_LDRDIN(rt,rt2,rn,im)	torrri8(THUMB2_LDRDI,rn,rt,rt2,im)
#define _CC_STRB(cc,rt,rn,rm)	corrr(cc,ARM_STRB|ARM_P,rn,rt,rm)
#define _STRB(rt,rn,rm)		_CC_STRB(ARM_CC_AL,rt,rn,rm)
#define T1_STRB(rt,rn,rm)	_jit_W(THUMB_STRB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_STRB(rt,rn,rm)	torxr(THUMB2_STRB,rn,rt,rm)
#define _CC_STRBN(cc,rt,rn,rm)	corrr(cc,ARM_STRB,rn,rt,rm)
#define _STRBN(rt,rn,rm)	_CC_STRBN(ARM_CC_AL,rt,rn,rm)
#define _CC_STRBI(cc,rt,rn,im)	corri(cc,ARM_STRBI|ARM_P,rn,rt,im)
#define _STRBI(rt,rn,im)	_CC_STRBI(ARM_CC_AL,rt,rn,im)
#define T1_STRBI(rt,rn,im)	_jit_W(THUMB_STRBI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_STRBI(rt,rn,im)	torri8(THUMB2_STRBI|THUMB2_U,rn,rt,im)
#define T2_STRBWI(rt,rn,im)	torri12(THUMB2_STRBWI,rn,rt,im)
#define _CC_STRBIN(cc,rt,rn,im)	corri(cc,ARM_STRBI,rn,rt,im)
#define _STRBIN(rt,rn,im)	_CC_STRBIN(ARM_CC_AL,rt,rn,im)
#define T2_STRBIN(rt,rn,im)	torri8(THUMB2_STRBI,rn,rt,im)
#define _CC_STRH(cc,rt,rn,rm)	corrr(cc,ARM_STRH|ARM_P,rn,rt,rm)
#define _STRH(rt,rn,rm)		_CC_STRH(ARM_CC_AL,rt,rn,rm)
#define T1_STRH(rt,rn,rm)	_jit_W(THUMB_STRH|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_STRH(rt,rn,rm)	torxr(THUMB2_STRH,rn,rt,rm)
#define _CC_STRHN(cc,rt,rn,rm)	corrr(cc,ARM_STRH,rn,rt,rm)
#define _STRHN(rt,rn,rm)	_CC_STRHN(ARM_CC_AL,rt,rn,rm)
#define _CC_STRHI(cc,rt,rn,im)	corri8(cc,ARM_STRHI|ARM_P,rn,rt,im)
#define _STRHI(rt,rn,im)	_CC_STRHI(ARM_CC_AL,rt,rn,im)
#define T1_STRHI(rt,rn,im)	_jit_W(THUMB_STRHI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_STRHI(rt,rn,im)	torri8(THUMB2_STRHI|THUMB2_U,rn,rt,im)
#define T2_STRHWI(rt,rn,im)	torri12(THUMB2_STRHWI,rn,rt,im)
#define _CC_STRHIN(cc,rt,rn,im)	corri8(cc,ARM_STRHI,rn,rt,im)
#define _STRHIN(rt,rn,im)	_CC_STRHIN(ARM_CC_AL,rt,rn,im)
#define T2_STRHIN(rt,rn,im)	torri8(THUMB2_STRHI,rn,rt,im)
#define _CC_STR(cc,rt,rn,rm)	corrr(cc,ARM_STR|ARM_P,rn,rt,rm)
#define _STR(rt,rn,rm)		_CC_STR(ARM_CC_AL,rt,rn,rm)
#define T1_STR(rt,rn,rm)	_jit_W(THUMB_STR|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T2_STR(rt,rn,rm)	torxr(THUMB2_STR,rn,rt,rm)
#define _CC_STRN(cc,rt,rn,rm)	corrr(cc,ARM_STR,rn,rt,rm)
#define _STRN(rt,rn,rm)		_CC_STRN(ARM_CC_AL,rt,rn,rm)
#define _CC_STRI(cc,rt,rn,im)	corri(cc,ARM_STRI|ARM_P,rn,rt,im)
#define _STRI(rt,rn,im)		_CC_STRI(ARM_CC_AL,rt,rn,im)
#define T1_STRI(rt,rn,im)	_jit_W(THUMB_STRI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt))
#define T1_STRISP(rt,im)	_jit_W(THUMB_STRISP|(_u3(rt)<<8)|(_u8(im)))
#define T2_STRI(rt,rn,im)	torri8(THUMB2_STRI|THUMB2_U,rn,rt,im)
#define T2_STRWI(rt,rn,im)	torri12(THUMB2_STRWI,rn,rt,im)
#define _CC_STRIN(cc,rt,rn,im)	corri(cc,ARM_STRI,rn,rt,im)
#define _STRIN(rt,rn,im)	_CC_STRIN(ARM_CC_AL,rt,rn,im)
#define T2_STRIN(rt,rn,im)	torri8(THUMB2_STRI,rn,rt,im)
#define _CC_STRD(cc,rt,rn,rm)	corrr(cc,ARM_STRD|ARM_P,rn,rt,rm)
#define _STRD(rt,rn,rm)		_CC_STRD(ARM_CC_AL,rt,rn,rm)
#define _CC_STRDN(cc,rt,rn,rm)	corrr(cc,ARM_STRD,rn,rt,rm)
#define _STRDN(rt,rn,rm)	_CC_STRDN(ARM_CC_AL,rt,rn,rm)
#define _CC_STRDI(cc,rt,rn,im)	corri8(cc,ARM_STRDI|ARM_P,rn,rt,im)
#define _STRDI(rt,rn,im)	_CC_STRDI(ARM_CC_AL,rt,rn,im)
#define T2_STRDI(rt,rt2,rn,im)	torrri8(THUMB2_STRDI|ARM_P,rn,rt,rt2,im)
#define _CC_STRDIN(cc,rt,rn,im)	corri8(cc,ARM_STRDI,rn,rt,im)
#define _STRDIN(rt,rn,im)	_CC_STRDIN(ARM_CC_AL,rt,rn,im)
#define T2_STRDIN(rt,rt2,rn,im)	torrri8(THUMB2_STRDI,rn,rt,rt2,im)

#define _CC_LDMIA(cc,rn,im)	corl(cc,ARM_M|ARM_M_L|ARM_M_I,rn,im)
#define _LDMIA(rn,im)		_CC_LDMIA(ARM_CC_AL,rn,im)
#define _CC_LDM(cc,rn,im)	_CC_LDMIA(cc,rn,im)
#define _LDM(rn,im)		_LDMIA(rn,im)
#define T1_LDMIA(rn,im)		_jit_W(THUMB_LDMIA|(_u3(rn)<<8)|im)
#define T2_LDMIA(rn,im)		torl(THUMB2_LDMIA,rn,im)
#define _CC_LDMIA_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_L|ARM_M_I|ARM_M_U,rn,im)
#define _LDMIA_U(rn,im)		_CC_LDMIA_U(ARM_CC_AL,rn,im)
#define _LDM_U(r0,i0)		_LDMIA_U(r0,i0)
#define _CC_LDMIB(cc,rn,im)	corl(cc,ARM_M|ARM_M_L|ARM_M_I|ARM_M_B,rn,im)
#define _LDMIB(rn,im)		_CC_LDMIB(ARM_CC_AL,rn,im)
#define _CC_LDMIB_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_L|ARM_M_I|ARM_M_B|ARM_M_U,rn,im)
#define _LDMIB_U(rn,im)		_CC_LDMIB_U(ARM_CC_AL,rn,im)
#define _CC_LDMDA(cc,rn,im)	corl(cc,ARM_M|ARM_M_L,rn,im)
#define _LDMDA(rn,im)		_CC_LDMDA(ARM_CC_AL,rn,im)
#define _CC_LDMDA_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_L|ARM_M_U,rn,im)
#define _LDMDA_U(rn,im)		_CC_LDMDA_U(ARM_CC_AL,rn,im)
#define _CC_LDMDB(cc,rn,im)	corl(cc,ARM_M|ARM_M_L|ARM_M_B,rn,im)
#define _LDMDB(rn,im)		_CC_LDMDB(ARM_CC_AL,rn,im)
#  define T2_LDMDB(rn,im)		torl(THUMB2_LDMDB,rn,im)

#define _CC_LDMDB_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_L|ARM_M_B|ARM_M_U,rn,im)
#define _LDMDB_U(rn,im)		_CC_LDMDB_U(ARM_CC_AL,rn,im)
#define _CC_STMIA(cc,rn,im)	corl(cc,ARM_M|ARM_M_I,rn,im)
#define _STMIA(rn,im)		_CC_STMIA(ARM_CC_AL,rn,im)
#define _CC_STM(cc,rn,im)	_CC_STMIA(cc,rn,im)
#define _STM(rn,im)		_STMIA(rn,im)
#define _CC_STMIA_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_I|ARM_M_U,rn,im)
#define _STMIA_U(rn,im)		_CC_STMIA_U(ARM_CC_AL,rn,im)
#define _CC_STM_U(cc,rn,im)	_CC_STMIA_U(cc,rn,im)
#define _STM_U(rn,im)		_STMIA_U(rn,im)
#define _CC_STMIB(cc,rn,im)	corl(cc,ARM_M|ARM_M_I|ARM_M_B,rn,im)
#define _STMIB(rn,im)		_CC_STMIB(ARM_CC_AL,rn,im)
#define _CC_STMIB_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_I|ARM_M_B|ARM_M_U,rn,im)
#define _STMIB_U(rn,im)		_CC_STMIB_U(ARM_CC_AL,rn,im)
#define _CC_STMDA(cc,rn,im)	corl(cc,ARM_M,rn,im)
#define _STMDA(rn,im)		_CC_STMDA(ARM_CC_AL,rn,im)
#define _CC_STMDA_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_U,rn,im)
#define _STMDA_U(rn,im)		_CC_STMDA_U(ARM_CC_AL,rn,im)
#define _CC_STMDB(cc,rn,im)	corl(cc,ARM_M|ARM_M_B,rn,im)
#define _STMDB(rn,im)		_CC_STMDB(ARM_CC_AL,rn,im)
#define _CC_STMDB_U(cc,rn,im)	corl(cc,ARM_M|ARM_M_B|ARM_M_U,rn,im)
#define _STMDB_U(rn,im)		_CC_STMDB_U(ARM_CC_AL,rn,im)
#define _CC_PUSH(cc,im)		_CC_STMDB_U(cc,JIT_SP,im)
#define _PUSH(im)		_STMDB_U(JIT_SP,im)
#define T1_PUSH(im)		_jit_W(THUMB_PUSH|((im&0x4000)>>6)|(im&0xff))
#define T2_PUSH(im)		tpp(THUMB2_PUSH,im)
#define _CC_POP(cc,im)		_LDMIA_U(cc,JIT_SP,im)
#define _POP(im)		_LDMIA_U(JIT_SP,im)
#define T1_POP(im)		_jit_W(THUMB_POP|((im&0x8000)>>7)|(im&0xff))
#define T2_POP(im)		tpp(THUMB2_POP,im)

#define _CC_PKHBTI(cc,rd,rn,rm,im) arm_cc_pkh(cc,ARM_PKH,rn,rd,rm,im)
#define _CC_PKHTBI(cc,rd,rn,rm,im) arm_cc_pkh(cc,ARM_PKH|ARM_ASR,rn,rd,rm,im)
#define _PKHBTI(rd,rn,rm,im)	_CC_PKHBTI(ARM_CC_AL,rd,rn,rm,im)
#define _PKHTBI(rd,rn,rm,im)	_CC_PKHTBI(ARM_CC_AL,rd,rn,rm,im)
#define _PKHBT(rd,rn,rm)	_CC_PKHBTI(ARM_CC_AL,rd,rn,rm,0)
#define _PKHTB(rd,rn,rm)	_CC_PKHTBI(ARM_CC_AL,rd,rn,rm,0)

static int
encode_thumb_immediate(unsigned int v)
{
    int			i;
    unsigned int	m;
    unsigned int	n;
    /* 00000000 00000000 00000000 abcdefgh */
    if ((v & 0xff) == v)
	return (v);
    /* 00000000 abcdefgh 00000000 abcdefgh */
    if ((v & 0xff00ff) == v && ((v & 0xff0000) >> 16) == (v & 0xff))
	return ((v & 0xff) | (1 << 12));
    /* abcdefgh 00000000 abcdefgh 00000000 */
    if (((v & 0xffff0000) >> 16) == (v & 0xffff) && (v & 0xff) == 0)
	return ((v & 0x000000ff) | (2 << 12));
    /* abcdefgh abcdefgh abcdefgh abcdefgh */
    if ( (v &    0xff)        == ((v &     0xff00) >>  8) &&
	((v &   0xff00) >> 8) == ((v &   0xff0000) >> 16) &&
	((v & 0xff0000) << 8) ==  (v & 0xff000000))
	return ((v & 0xff) | (3 << 12));
    /* 1bcdefgh << 24 ... 1bcdefgh << 1 */
    for (i = 8, m = 0xff000000, n = 0x80000000;
	 i < 23; i++, m >>= 1,  n >>= 1) {
	if ((v & m) == v && (v & n)) {
	    v >>= 32 - i;
	    if (!(i & 1))
		v &= 0x7f;
	    i >>= 1;
	    return (((i & 7) << 12) | ((i & 8) << 23) | v);
	}
    }
    return (-1);
}

static int
encode_thumb_word_immediate(unsigned int v)
{
    if ((v & 0xfffff000) == 0)
	return (((v & 0x800) << 15) | ((v & 0x700) << 4) | (v & 0xff));
    return (-1);
}

static int
encode_thumb_jump(int v)
{
    int		s, i1, i2, j1, j2;
    if (v >= (int)-0x800000 && v <= 0x7fffff) {
	s  = !!(v & 0x800000);
	i1 = !!(v & 0x400000);
	i2 = !!(v & 0x200000);
	j1 = s ? i1 : !i1;
	j2 = s ? i2 : !i2;
	return ((s<<26)|((v&0x1ff800)<<5)|(j1<<13)|(j2<<11)|(v&0x7ff));
    }
    return (-1);
}

static int
encode_thumb_cc_jump(int v)
{
    int		s, j1, j2;
    if (v >= (int)-0x80000 && v <= 0x7ffff) {
	s  = !!(v & 0x80000);
	j1 = !!(v & 0x20000);
	j2 = !!(v & 0x40000);
	return ((s<<26)|((v&0x1f800)<<5)|(j1<<13)|(j2<<11)|(v&0x7ff));
    }
    return (-1);
}

#if !NAN_TO_INT_IS_ZERO
static int
encode_thumb_shift(int v, int type)
{
    switch (type) {
	case ARM_ASR:
	case ARM_LSL:
	case ARM_LSR:		type >>= 1;	break;
	default:		jit_assert(!"handled shift");
    }
    jit_assert(v >= 0 && v <= 31);
    return (((v & 0x1c) << 10) | ((v & 3) << 6) | type);
}
#endif

#define thumb2_orri(o,rn,rd,im)		_torri(_jitp,o,rn,rd,im)
#define torri(o,rn,rt,im)		_torri(_jitp,o,rn,rt,im)
__jit_inline void
_torri(jit_state_t _jitp, int o, int rn, int rd, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o  & 0x0c0f7fff));
    jit_assert(!(im & 0xfbff8f00));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rd)<<8)|im;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define torri8(o,rn,rt,im)		_torri8(_jitp,o,rn,rt,im)
__jit_inline void
_torri8(jit_state_t _jitp, int o, int rn, int rt, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o  & 0x000ff0ff));
    jit_assert(!(im & 0xffffff00));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rt)<<12)|im;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define torri12(o,rn,rt,im)		_torri12(_jitp,o,rn,rt,im)
__jit_inline void
_torri12(jit_state_t _jitp, int o, int rn, int rt, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o  & 0x000fffff));
    jit_assert(!(im & 0xfffff000));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rt)<<12)|im;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define toriw(o,rd,im)			_toriw(_jitp,o,rd,im)
__jit_inline void
_toriw(jit_state_t _jitp, int o, int rd, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o  & 0x40f7fff));
    jit_assert(!(im & 0xffff0000));
    thumb.i = o|((im&0xf000)<<4)|((im&0x800)<<15)|((im&0x700)<<4)|(_u4(rd)<<8)|(im&0xff);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define torrr(o,rn,rd,rm)		_torrr(_jitp,o,rn,rd,rm)
__jit_inline void
_torrr(jit_state_t _jitp, int o, int rn, int rd, int rm)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0xf0f0f));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rd)<<8)|_u4(rm);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define torrrs(o,rn,rd,rm,im)		_torrrs(_jitp,o,rn,rd,rm,im)
__jit_inline void
_torrrs(jit_state_t _jitp, int o, int rn, int rd, int rm, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o  & 0x000f0f0f));
    jit_assert(!(im & 0xffff8f0f));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rd)<<8)|im|_u4(rm);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define torxr(o,rn,rt,rm)		_torxr(_jitp,o,rn,rt,rm)
__jit_inline void
_torxr(jit_state_t _jitp, int o, int rn, int rt, int rm)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0xf0f0f));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rt)<<12)|_u4(rm);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define torrrr(o,rn,rl,rh,rm)		_torrrr(_jitp,o,rn,rl,rh,rm)
__jit_inline void
_torrrr(jit_state_t _jitp, int o, int rn, int rl, int rh, int rm)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0x000fff0f));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rl)<<12)|(_u4(rh)<<8)|_u4(rm);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define torrri8(o,rn,rt,rt2,im)		_torrri8(_jitp,o,rn,rt,rt2,im)
__jit_inline void
_torrri8(jit_state_t _jitp, int o, int rn, int rt, int rt2, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o  & 0x000fffff));
    jit_assert(!(im & 0xffffff00));
    thumb.i = o|(_u4(rn)<<16)|(_u4(rt)<<12)|(_u4(rt2)<<8)|im;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define tc8(cc,im)			_tc8(_jitp,cc,im)
__jit_inline void
_tc8(jit_state_t _jitp, int cc, int im)
{
    jit_assert(!(cc & 0x0fffffff));
    jit_assert(cc != ARM_CC_AL && cc != ARM_CC_NV);
    jit_assert(_s8P(im));
    _jit_W(THUMB_CC_B|(cc>>20)|(im&0xff));
}

#define t11(im)				_t11(_jitp,im)
__jit_inline void
_t11(jit_state_t _jitp, int im)
{
    jit_assert(!(im & 0xfffff800));
    _jit_W(THUMB_B|im);
}

#define tcb(cc,im)			_tcb(_jitp,cc,im)
__jit_inline void
_tcb(jit_state_t _jitp, int cc, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(cc & 0xfffffff));
    jit_assert(cc != ARM_CC_AL && cc != ARM_CC_NV);
    cc = ((unsigned)cc) >> 6;
    jit_assert(!(im & (THUMB2_CC_B|cc)));
    thumb.i = THUMB2_CC_B|cc|im;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define blxi(im)				_blxi(_jitp,im)
__jit_inline void
_blxi(jit_state_t _jitp, int im)
{
    jit_assert(!(im & 0xfe000000));
    _jit_I(ARM_BLXI|im);
}

#define tb(o,im)				_tb(_jitp,o,im)
__jit_inline void
_tb(jit_state_t _jitp, int o, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0x07ff2fff));
    jit_assert(!(o & im));
    thumb.i = o|im;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define tshift(o,rd,rm,im)		_tshift(_jitp,o,rd,rm,im)
__jit_inline void
_tshift(jit_state_t _jitp, int o, int rd, int rm, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0x7fcf));
    jit_assert(im >= 0 && im < 32);
    thumb.i = o|((im&0x1c)<<10)|(_u4(rd)<<8)|((im&3)<<6)|_u4(rm);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define thumb2_orrr(o,rn,rd,rm)		_thumb2_orrr(_jitp,o,rn,rd,rm)
__jit_inline void
_thumb2_orrr(jit_state_t _jitp, int o, int rn, int rd, int rm)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0x000f0f0f));
    thumb.i = o | (_u4(rn)<<16) | (_u4(rd)<<8) | _u4(rm);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define thumb2_bfx(o,rn,rd,lsb,width)	_thumb2_bfx(_jitp,o,rn,rd,lsb,width)
__jit_inline void
_thumb2_bfx(jit_state_t _jitp, int o, int rn, int rd, int lsb, int width)
{
    int		msb;
    jit_thumb_t	thumb;
    jit_assert(!(o & 0x7fdf));
    jit_assert(lsb >= 0 && lsb < 32 && width >= 0 && width <= 32);
    msb = lsb + width;
    if (msb > 31)
	msb = 31;
    thumb.i = o | (_u4(rn) << 16) | (_u4(rd) << 8) |
	      ((msb & 7) << 10) | ((msb & 3) << 5) | msb;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define thumb2_cbxz(o,rn,im)		_thumb2_cbxz(_jitp,o,rn,im)
__jit_inline void
_thumb2_cbxz(jit_state_t _jitp, int o, int rn, int im)
{
    jit_assert(!(o & 0x2ff));
    /* branch forward only */
    jit_assert(im >= 0 && im < 128 && !(im & 1));
    im >>= 1;
    _jit_W(o|((im&0x80)<<5)|((im&0x1f)<<3)|_u3(rn));
}

#define thumb2_dbg(h,l,im)		_thumb2_dbg(_jitp,h,l,im)
__jit_inline void
_thumb2_dbg(jit_state_t _jitp, int h, int l, int im)
{
    jit_assert(!(h & ~0xffff));
    jit_assert(!(l & 0xffff000f));
    _jit_WW(h, l | _u4(im));
}

#define tpp(o,im)			_tpp(_jitp,o,im)
__jit_inline void
_tpp(jit_state_t _jitp, int o, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o  & 0x0000ffff));
    jit_assert(!(im & 0xffff2000));
    if (o == THUMB2_PUSH)
	jit_assert(!(im & 0x8000));
    jit_assert(__builtin_popcount(im & 0x1fff) > 1);
    thumb.i = o|im;
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define THUMB2_IT			0
#define THUMB2_ITT			1
#define THUMB2_ITE			2
#define THUMB2_ITTT			3
#define THUMB2_ITET			4
#define THUMB2_ITTE			5
#define THUMB2_ITEE			6
#define THUMB2_ITTTT			7
#define THUMB2_ITETT			8
#define THUMB2_ITTET			9
#define THUMB2_ITEET			10
#define THUMB2_ITTTE			11
#define THUMB2_ITETE			12
#define THUMB2_ITTEE			13
#define THUMB2_ITEEE			14
#define tcit(tc,it)			_tcit(_jitp,tc,it)
__jit_inline void
_tcit(jit_state_t _jitp, unsigned int tc, int it)
{
    int		c;
    int		m;
    c = (tc >> 28) & 1;
    jit_assert(!(tc & 0xfffffff) && tc != ARM_CC_NV);
    switch (it) {
	case THUMB2_IT:		m =   1<<3; 			break;
	case THUMB2_ITT:	m =  (c<<3)| (1<<2);		break;
	case THUMB2_ITE:	m = (!c<<3)| (1<<2);		break;
	case THUMB2_ITTT:	m =  (c<<3)| (c<<2)| (1<<1);	break;
	case THUMB2_ITET:	m = (!c<<3)| (c<<2)| (1<<1);	break;
	case THUMB2_ITTE:	m =  (c<<3)|(!c<<2)| (1<<1);	break;
	case THUMB2_ITEE:	m = (!c<<3)|(!c<<2)| (1<<1);	break;
	case THUMB2_ITTTT:	m =  (c<<3)| (c<<2)| (c<<1)|1;	break;
	case THUMB2_ITETT:	m = (!c<<3)| (c<<2)| (c<<1)|1;	break;
	case THUMB2_ITTET:	m =  (c<<3)|(!c<<2)| (c<<1)|1;	break;
	case THUMB2_ITEET:	m = (!c<<3)|(!c<<2)| (c<<1)|1;	break;
	case THUMB2_ITTTE:	m =  (c<<3)| (c<<2)|(!c<<1)|1;	break;
	case THUMB2_ITETE:	m = (!c<<3)| (c<<2)|(!c<<1)|1;	break;
	case THUMB2_ITTEE:	m =  (c<<3)|(!c<<2)|(!c<<1)|1;	break;
	case THUMB2_ITEEE:	m = (!c<<3)|(!c<<2)|(!c<<1)|1;	break;
        default:		jit_assert(!"valid it code!"); m = 0;
    }
    jit_assert(m && (tc != ARM_CC_AL || !(m & (m - 1))));
    _jit_W(0xbf00 | (tc >> 24) | m);
}
#define _IT(cc)				tcit(cc,THUMB2_IT)
#define _ITT(cc)			tcit(cc,THUMB2_ITT)
#define _ITE(cc)			tcit(cc,THUMB2_ITE)
#define _ITTT(cc)			tcit(cc,THUMB2_ITTT)
#define _ITTE(cc)			tcit(cc,THUMB2_ITTE)
#define _ITET(cc)			tcit(cc,THUMB2_ITET)
#define _ITEE(cc)			tcit(cc,THUMB2_ITEE)
#define _ITTTT(cc)			tcit(cc,THUMB2_ITTTT)
#define _ITETT(cc)			tcit(cc,THUMB2_ITETT)
#define _ITTET(cc)			tcit(cc,THUMB2_ITTET)
#define _ITEET(cc)			tcit(cc,THUMB2_ITEET)
#define _ITTTE(cc)			tcit(cc,THUMB2_ITTTE)
#define _ITETE(cc)			tcit(cc,THUMB2_ITETE)
#define _ITTEE(cc)			tcit(cc,THUMB2_ITTEE)
#define _ITEEE(cc)			tcit(cc,THUMB2_ITEEE)

#define torl(o,rn,im)			_torl(_jitp,o,rn,im)
__jit_inline void
_torl(jit_state_t _jitp, int o, int rn, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0xf1fff));
    jit_assert(rn != _R15 || !im || ((o & 0xc000) == 0xc000));
    jit_assert(!(o & THUMB2_LDM_W) || !(im & (1 << rn)));
    thumb.i = o | (_u4(rn)<<16) | _u13(im);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define thumb2_mrrc(o,t2,t,cc,o1,m)	_thumb2_mrrc(_jitp,o,t2,t,cc,o1,m)
__jit_inline void
_thumb2_mrrc(jit_state_t _jitp, int o, int t2, int t, int cc, int o1, int m)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0x03afffff));
    thumb.i = o|(_u4(t2)<<16)|(_u4(t)<<12)|(_u4(cc)<<8)|(_u4(o1)<<4)|_u4(m);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

#define thumb2_pkh(o,rn,rd,rm,im)	_thumb2_pkh(_jitp,o,rn,rd,rm,im)
__jit_inline void
_thumb2_pkh(jit_state_t _jitp, int o, int rn, int rd, int rm, int im)
{
    jit_thumb_t	thumb;
    jit_assert(!(o & 0x7ffcf));
    thumb.i = o|(_u4(rn)<<16)|((_u5(im)&0x1c)<<10)|
	      (_u4(rd)<<12)|((im&3)<<6)|_u4(rm);
    _jit_WW(thumb.s[0], thumb.s[1]);
}

/* v6T2, v7 */
#define THUMB2_BFX			0xf3600000
#define T2_BFC(rd,lsb,width)		thumb2_bfx(THUMB2_BFX,_R15,rd,lsb,width)
#define T2_BFI(rd,rn,lsb,width)		thumb2_bfx(THUMB2_BFX,rn,rd,lsb,width)
/* not permitted in IT block */
#define THUMB2_CBZ			0xb100
#define T2_CBZ(rn,im)			thumb2_cbxz(THUMB2_CBZ,rn,im)
#define THUMB2_CBNZ			0xb900
#define T2_CBNZ(rn,im)			thumb2_cbxz(THUMB2_CBNZ,rn,im)
/* CDP/CDP2 here - same encoding as arm, but can start at "&3 == 2 offset" */
#define THUMB2_CLZ			0xfab0f080
#define T2_CLZ(rd,rm)			thumb2_orrr(THUMB2_CLZ,rd,rm,rm)

#endif /* __lightning_asm_h */
