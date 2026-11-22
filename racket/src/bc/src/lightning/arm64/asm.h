/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler for the AArch64
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2011, 2013-2023, 2026 Free Software Foundation, Inc.
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
 *
 * The opcode constants below are adapted from GNU lightning 2.x
 * ("lib/jit_aarch64-cpu.c" and "lib/jit_aarch64-fpu.c", which in turn
 * draw on binutils' "aarch64-tbl.h"), repackaged as lightning
 * 1.x-style emitters for Racket BC.
 *
 * The logical-immediate encoder `a64_encode_logical_immediate` is
 * adapted from "aarch64-logical-immediates.c" as distributed with GNU
 * lightning 2.x, which its author placed in the public domain (CC0).
 ***********************************************************************/

#ifndef __lightning_asm_h
#define __lightning_asm_h

/* General-purpose registers are numbered 0-30 as in the hardware.
   Encoding 31 means either the zero register or the stack pointer,
   depending on the instruction and operand. To avoid confusion,
   lightning-level code refers to the stack pointer as `_SP` (which is
   not 31), and the encoders below convert `_SP` to 31 only in operand
   positions that accept the stack pointer. `_ZR` is the zero
   register, valid only in operand positions that treat 31 as zero. */
typedef int jit_gpr_t;

#define _X0	0	/* argument / result */
#define _X1	1	/* argument */
#define _X2	2	/* argument */
#define _X3	3	/* argument */
#define _X4	4
#define _X5	5
#define _X6	6
#define _X7	7
#define _X8	8
#define _X9	9	/* outgoing-argument staging */
#define _X10	10	/* outgoing-argument staging */
#define _X11	11	/* outgoing-argument staging */
#define _X12	12	/* outgoing-argument staging */
#define _X13	13
#define _X14	14
#define _X15	15
#define _X16	16	/* IP0: JIT_TMP */
#define _X17	17	/* IP1: JIT_TMP2 */
#define _X18	18	/* platform register; never used */
#define _X19	19	/* callee-saved: JIT_V0 */
#define _X20	20	/* callee-saved: JIT_V1 */
#define _X21	21	/* callee-saved: JIT_V2 */
#define _X22	22	/* callee-saved: JIT_V3 */
#define _X29	29	/* frame pointer */
#define _X30	30	/* link register */
#define _ZR	31
#define _SP	32

#define JIT_RET		_X0
#define JIT_LR		_X30
#define JIT_SP		_SP
#define JIT_FP		_X29
/* JIT_TMP is used for addresses (long jumps, calls, and memory
   offsets that do not fit in an instruction), and JIT_TMP2 is used for
   immediate operands that do not fit in an instruction. */
#define JIT_TMP		_X16
#define JIT_TMP2	_X17
/* First register used to stage outgoing arguments: */
#define JIT_ARG_STAGE	_X9
#define JIT_MAX_ARGS	4

typedef int jit_fpr_t;
#define JIT_FPRET	0
#define JIT_FPTMP	31

/* Condition codes */
#define A64_CC_EQ	0x0
#define A64_CC_NE	0x1
#define A64_CC_HS	0x2
#define A64_CC_LO	0x3
#define A64_CC_MI	0x4
#define A64_CC_PL	0x5
#define A64_CC_VS	0x6
#define A64_CC_VC	0x7
#define A64_CC_HI	0x8
#define A64_CC_LS	0x9
#define A64_CC_GE	0xa
#define A64_CC_LT	0xb
#define A64_CC_GT	0xc
#define A64_CC_LE	0xd
#define A64_CC_AL	0xe

#define a64_invert_cc(cc) ((cc) ^ 1)

/* Operand-field helpers. (These avoid function calls such as
   assertions, since they're used in expression positions that
   Racket's "xform" pass does not allow to contain calls.) */
#define _A64_R(r)	((_ui)(r) & 0x1f)			/* 31 = ZR */
#define _A64_RSP(r)	((_ui)((r) == _SP ? 31 : (r)) & 0x1f)	/* 31 = SP */
#define _A64_F(r)	((_ui)(r) & 0x1f)
#define _A64_SF(sf)	((_ui)(sf) << 31)

/* Opcodes, adapted from GNU lightning 2.x: */
#define A64_ADDI	0x11000000
#define A64_ADDSI	0x31000000
#define A64_SUBI	0x51000000
#define A64_SUBSI	0x71000000
#define A64_ADD		0x0b000000
#define A64_ADDS	0x2b000000
#define A64_SUB		0x4b000000
#define A64_SUBS	0x6b000000
#define A64_ADDX	0x0b206000	/* extended register, UXTX */
#define A64_ADDSX	0x2b206000
#define A64_SUBX	0x4b206000
#define A64_SUBSX	0x6b206000
#define A64_SBFM	0x13000000
#define A64_UBFM	0x53000000
#define A64_B		0x14000000
#define A64_BL		0x94000000
#define A64_BR		0xd61f0000
#define A64_BLR		0xd63f0000
#define A64_RET		0xd65f0000
#define A64_B_C		0x54000000
#define A64_CSINC	0x1a800400
#define A64_UDIV	0x1ac00800
#define A64_SDIV	0x1ac00c00
#define A64_LSLV	0x1ac02000
#define A64_LSRV	0x1ac02400
#define A64_ASRV	0x1ac02800
#define A64_MADD	0x1b000000
#define A64_MSUB	0x1b008000
#define A64_SMULH	0x9b407c00
#define A64_UMULH	0x9bc07c00
#define A64_ANDI	0x12000000
#define A64_ORRI	0x32000000
#define A64_EORI	0x52000000
#define A64_ANDSI	0x72000000
#define A64_AND		0x0a000000
#define A64_ORR		0x2a000000
#define A64_ORN		0x2a200000
#define A64_EOR		0x4a000000
#define A64_ANDS	0x6a000000
#define A64_MOVN	0x12800000
#define A64_MOVZ	0x52800000
#define A64_MOVK	0x72800000
#define A64_NOP		0xd503201f

#define A64_SHIFT_LSL	0
#define A64_SHIFT_LSR	1
#define A64_SHIFT_ASR	2

/* Loads and stores, in "unsigned scaled immediate" form; the
   unscaled-immediate and register-offset forms are derived by
   `a64_ldst()`. */
#define A64_STRB	0x39000000
#define A64_LDRB	0x39400000
#define A64_LDRSBX	0x39800000
#define A64_LDRSBW	0x39c00000
#define A64_STRH	0x79000000
#define A64_LDRH	0x79400000
#define A64_LDRSHX	0x79800000
#define A64_LDRSHW	0x79c00000
#define A64_STRW	0xb9000000
#define A64_LDRW	0xb9400000
#define A64_LDRSW	0xb9800000
#define A64_STRX	0xf9000000
#define A64_LDRX	0xf9400000
#define A64_STRS	0xbd000000
#define A64_LDRS	0xbd400000
#define A64_STRD	0xfd000000
#define A64_LDRD	0xfd400000

#define A64_STP_PRE	0xa9800000
#define A64_STP		0xa9000000
#define A64_LDP		0xa9400000
#define A64_LDP_POST	0xa8c00000

/* Floating point (double precision unless noted) */
#define A64_FMOV	0x1e604000
#define A64_FADD	0x1e602800
#define A64_FSUB	0x1e603800
#define A64_FMUL	0x1e600800
#define A64_FDIV	0x1e601800
#define A64_FABS	0x1e60c000
#define A64_FNEG	0x1e614000
#define A64_FSQRT	0x1e61c000
#define A64_FCMP	0x1e602000
#define A64_FCVT_SD	0x1e624000	/* single <- double */
#define A64_FCVT_DS	0x1e22c000	/* double <- single */
#define A64_SCVTF_DW	0x1e620000
#define A64_SCVTF_DX	0x9e620000
#define A64_FCVTZS_WD	0x1e780000
#define A64_FCVTZS_XD	0x9e780000
#define A64_FMOV_DX	0x9e670000	/* double <- 64-bit integer bits */
#define A64_FMOV_XD	0x9e660000	/* 64-bit integer bits <- double */

/* Encoders. Each `sf` argument is 1 for a 64-bit operation, 0 for a
   32-bit operation. */

#define A64_OP_RRR(op, sf, d, n, m) \
  _jit_I((op) | _A64_SF(sf) | (_A64_R(m) << 16) | (_A64_R(n) << 5) | _A64_R(d))
#define A64_OP_SHIFTED(op, sf, d, n, m, sh, amt) \
  _jit_I((op) | _A64_SF(sf) | ((_ui)(sh) << 22) | (_A64_R(m) << 16) \
         | ((_ui)(amt) << 10) | (_A64_R(n) << 5) | _A64_R(d))

/* Add/subtract, immediate; `d` is SP-capable only for non-flag-setting
   variants. */
#define A64_ADDSUB_IMM(op, sf, d, n, imm12, lsl12) \
  _jit_I((op) | _A64_SF(sf) | ((_ui)(lsl12) << 22) | (((_ui)(imm12) & 0xfff) << 10) \
         | (_A64_RSP(n) << 5) \
         | ((((op) == A64_ADDSI) || ((op) == A64_SUBSI)) ? _A64_R(d) : _A64_RSP(d)))
/* Add/subtract, extended register with UXTX (i.e., no extension or
   shift), which allows SP as `n` and (for non-flag-setting) `d` */
#define A64_ADDSUB_EXT(op, d, n, m) \
  _jit_I((op) | _A64_SF(1) | (_A64_R(m) << 16) | (_A64_RSP(n) << 5) \
         | ((((op) == A64_ADDSX) || ((op) == A64_SUBSX)) ? _A64_R(d) : _A64_RSP(d)))

/* Logical, immediate: `enc` is the 13-bit N:immr:imms encoding */
#define A64_LOGIC_IMM(op, sf, d, n, enc) \
  _jit_I((op) | _A64_SF(sf) | (((_ui)(enc) & 0x1fff) << 10) | (_A64_R(n) << 5) \
         | (((op) == A64_ANDSI) ? _A64_R(d) : _A64_RSP(d)))

#define A64_MOVWIDE(op, sf, d, imm16, hw) \
  _jit_I((op) | _A64_SF(sf) | ((_ui)(hw) << 21) | (((_ui)(imm16) & 0xffff) << 5) | _A64_R(d))

#define A64_BITFIELD(op, sf, d, n, immr, imms) \
  _jit_I((op) | _A64_SF(sf) | ((_ui)(sf) << 22) | (((_ui)(immr) & 0x3f) << 16) \
         | (((_ui)(imms) & 0x3f) << 10) | (_A64_R(n) << 5) | _A64_R(d))

#define A64_MADDSUB(op, sf, d, n, m, a) \
  _jit_I((op) | _A64_SF(sf) | (_A64_R(m) << 16) | (_A64_R(a) << 10) | (_A64_R(n) << 5) | _A64_R(d))
#define A64_MUL(sf, d, n, m)	A64_MADDSUB(A64_MADD, sf, d, n, m, _ZR)
#define A64_MSUBR(sf, d, n, m, a) A64_MADDSUB(A64_MSUB, sf, d, n, m, a)

#define A64_SMULHR(d, n, m) \
  _jit_I(A64_SMULH | (_A64_R(m) << 16) | (_A64_R(n) << 5) | _A64_R(d))

#define A64_CSINCR(sf, d, n, m, cc) \
  _jit_I(A64_CSINC | _A64_SF(sf) | (_A64_R(m) << 16) | ((_ui)(cc) << 12) | (_A64_R(n) << 5) | _A64_R(d))
#define A64_CSET(sf, d, cc)	A64_CSINCR(sf, d, _ZR, _ZR, a64_invert_cc(cc))

#define A64_BI(d26)		_jit_I(A64_B | ((_ui)(d26) & 0x3ffffff))
#define A64_BLI(d26)		_jit_I(A64_BL | ((_ui)(d26) & 0x3ffffff))
#define A64_BCC(cc, d19)	_jit_I(A64_B_C | (((_ui)(d19) & 0x7ffff) << 5) | (_ui)(cc))
#define A64_BRR(n)		_jit_I(A64_BR | (_A64_R(n) << 5))
#define A64_BLRR(n)		_jit_I(A64_BLR | (_A64_R(n) << 5))
#define A64_RETR(n)		_jit_I(A64_RET | (_A64_R(n) << 5))
#define A64_NOPI()		_jit_I(A64_NOP)

/* Load/store pair of X registers; `ofs` is in bytes */
#define A64_LDSTP(op, t1, t2, n, ofs) \
  _jit_I((op) | ((((_ui)((ofs) >> 3)) & 0x7f) << 15) | (_A64_R(t2) << 10) \
         | (_A64_RSP(n) << 5) | _A64_R(t1))

/* Floating-point encoders */
#define A64_FOP_RR(op, d, n) \
  _jit_I((op) | (_A64_F(n) << 5) | _A64_F(d))
#define A64_FOP_RRR(op, d, n, m) \
  _jit_I((op) | (_A64_F(m) << 16) | (_A64_F(n) << 5) | _A64_F(d))
#define A64_FCMPR(n, m) \
  _jit_I(A64_FCMP | (_A64_F(m) << 16) | (_A64_F(n) << 5))
/* Conversions between general and FP registers */
#define A64_FOP_FG(op, d, n)	_jit_I((op) | (_A64_R(n) << 5) | _A64_F(d))  /* FP <- general */
#define A64_FOP_GF(op, d, n)	_jit_I((op) | (_A64_F(n) << 5) | _A64_R(d))  /* general <- FP */

/* Logical-immediate encoding: returns the 13-bit N:immr:imms field,
   or -1 if `val` cannot be represented. For a 32-bit operation, `val`
   must be replicated into both halves, which `a64_encode_logical_immediate32`
   does. */
#ifdef _MSC_VER
# include <intrin.h>
__jit_inline int a64_ctz64(uint64_t v)
{
  unsigned long i;
  _BitScanForward64(&i, v);
  return (int)i;
}
__jit_inline int a64_clz64(uint64_t v)
{
  unsigned long i;
  _BitScanReverse64(&i, v);
  return 63 - (int)i;
}
#else
# define a64_ctz64(v) __builtin_ctzll(v)
# define a64_clz64(v) __builtin_clzll(v)
#endif

__jit_inline int a64_encode_logical_immediate(uint64_t val)
{
  int rotation, zeroes, ones, size, immr, imms, N;
  uint64_t normalized, v;

  if (val == 0 || ~val == 0)
    return -1;

  /* rotate so that the least significant bit is 1 and the most is 0 */
  v = val & (val + 1); /* clear trailing ones */
  rotation = v ? a64_ctz64(v) : 64;
  rotation &= 63;
  normalized = rotation ? ((val >> rotation) | (val << (64 - rotation))) : val;

  zeroes = a64_clz64(normalized);
  ones = a64_ctz64(~normalized);
  size = zeroes + ones;

  /* check that the pattern repeats */
  if ((((val >> (size & 63)) | (val << ((64 - size) & 63))) != val))
    return -1;

  immr = -rotation & (size - 1);
  imms = -(size << 1) | (ones - 1);
  N = (size >> 6);

  return (N << 12) | (immr << 6) | (imms & 0x3f);
}

__jit_inline int a64_encode_logical_immediate32(uint32_t val)
{
  return a64_encode_logical_immediate(((uint64_t)val << 32) | val);
}

#endif /* __lightning_asm_h */
