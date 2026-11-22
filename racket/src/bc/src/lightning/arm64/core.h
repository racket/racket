/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer (AArch64 version)
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
 * This AArch64 layer for Racket BC is modeled on the ARM layer in
 * "../arm/core.h" (backported for Racket from Paulo Cesar Pereira de
 * Andrade's original ARM port) and uses instruction selection derived
 * from the AArch64 port in GNU lightning 2.x.
 ***********************************************************************/

#ifndef __lightning_core_arm64_h
#define __lightning_core_arm64_h

/*
  Conventions for Racket BC on AArch64:

  * JIT_R0-R2 are x0-x2, which are also the first argument registers.
    JIT_V0-V3 are the callee-saved x19-x22. JIT_TMP and JIT_TMP2 are
    x16 and x17, which are reserved for this layer's internal use.
    Outgoing C arguments are staged in x9-x12 by jit_pusharg_*() and
    moved into place by jit_finish().

  * The `_l`, `_ul`, and `_p` operations work on 64-bit values. Like
    the x86_64 port, the `_i` operations work on 32-bit values, and a
    result is zero-extended into its 64-bit register; `_i` comparisons
    use only the low 32 bits.

  * A frame created by jit_prolog() is (with offsets relative to FP,
    which is x29 and also SP after the prolog):

        112: [caller's frame]
        104: JIT_LOCAL4
         96: JIT_LOCAL3
         88: JIT_LOCAL2
         80: JIT_LOCAL1
         48: incoming arguments x0-x3, saved for jit_getarg_*()
         16: saved x19-x22
          8: saved x30 (return address)
          0: saved x29 (caller's frame pointer)

    This is a standard AArch64 frame record, so the frame chain can be
    followed through C and JIT-generated frames. Space for unboxed
    floating-point values is allocated below the frame by decrementing
    SP in units of 16 bytes, which keeps SP 16-byte aligned.

  * In long-jump mode (the default), each branch and call is a fixed
    sequence that loads the target address into JIT_TMP, so it can
    reach any address and can be patched. In short-jump mode, a branch
    is a single PC-relative instruction.
*/

#define _jitl		_jitp->jitl

#define JIT_R_NUM	3
#define JIT_R(i)	(i)
#define JIT_V_NUM	4
#define JIT_V(i)	(_X19 + (i))

#define JIT_R0		_X0
#define JIT_R1		_X1
#define JIT_R2		_X2
#define JIT_V0		_X19
#define JIT_V1		_X20
#define JIT_V2		_X21

#define JIT_FRAME_SIZE		112
#define JIT_FRAME_SAVED_OFFSET	16
#define JIT_FRAME_ARGS_OFFSET	48
#define JIT_FRAME_LOCALS_OFFSET	80

/*========================================================================*/
/* Moves                                                                  */
/*========================================================================*/

#define jit_nop()			A64_NOPI()

__jit_inline void
a64_movr(jit_state_t _jitp, int sf, jit_gpr_t r0, jit_gpr_t r1)
{
  if (r0 != r1) {
    if ((r0 == JIT_SP) || (r1 == JIT_SP))
      A64_ADDSUB_IMM(A64_ADDI, 1, r0, r1, 0, 0);
    else
      A64_OP_SHIFTED(A64_ORR, sf, r0, _ZR, r1, 0, 0);
  }
}

/* Variable-length immediate load */
__jit_inline void
a64_movi(jit_state_t _jitp, int sf, jit_gpr_t r0, uint64_t v)
{
  int i, n = (sf ? 4 : 2), nz = 0, nf = 0, first = 1, enc;
  _ui chunk;

  if (!sf)
    v &= 0xFFFFFFFF;

  for (i = 0; i < n; i++) {
    chunk = (_ui)((v >> (i * 16)) & 0xFFFF);
    if (chunk == 0) nz++;
    if (chunk == 0xFFFF) nf++;
  }

  if ((nz < n - 1) && (nf < n - 1)) {
    /* more than one instruction via MOVZ/MOVN; try a logical immediate */
    if (sf)
      enc = a64_encode_logical_immediate(v);
    else
      enc = a64_encode_logical_immediate32((uint32_t)v);
    if (enc != -1) {
      A64_LOGIC_IMM(A64_ORRI, sf, r0, _ZR, enc);
      return;
    }
  }

  if (nf > nz) {
    /* MOVN, then MOVK for chunks that are not 0xFFFF */
    for (i = 0; i < n; i++) {
      chunk = (_ui)((v >> (i * 16)) & 0xFFFF);
      if (chunk != 0xFFFF) {
        if (first) {
          A64_MOVWIDE(A64_MOVN, sf, r0, (~chunk) & 0xFFFF, i);
          first = 0;
        } else
          A64_MOVWIDE(A64_MOVK, sf, r0, chunk, i);
      }
    }
    if (first) /* all 0xFFFF */
      A64_MOVWIDE(A64_MOVN, sf, r0, 0, 0);
  } else {
    for (i = 0; i < n; i++) {
      chunk = (_ui)((v >> (i * 16)) & 0xFFFF);
      if (chunk != 0) {
        if (first) {
          A64_MOVWIDE(A64_MOVZ, sf, r0, chunk, i);
          first = 0;
        } else
          A64_MOVWIDE(A64_MOVK, sf, r0, chunk, i);
      }
    }
    if (first) /* zero */
      A64_MOVWIDE(A64_MOVZ, sf, r0, 0, 0);
  }
}

/* Fixed-length, patchable immediate load: MOVZ + 3 MOVKs */
__jit_inline void
a64_movi_fixed(jit_state_t _jitp, jit_gpr_t r0, uint64_t v)
{
  A64_MOVWIDE(A64_MOVZ, 1, r0, v & 0xFFFF, 0);
  A64_MOVWIDE(A64_MOVK, 1, r0, (v >> 16) & 0xFFFF, 1);
  A64_MOVWIDE(A64_MOVK, 1, r0, (v >> 32) & 0xFFFF, 2);
  A64_MOVWIDE(A64_MOVK, 1, r0, (v >> 48) & 0xFFFF, 3);
}

/* A pointer-sized immediate whose length depends only on whether the
   value is small, so that code size is stable even if a pointer value
   changes between JIT passes. */
__jit_inline jit_insn *
a64_movi_p(jit_state_t _jitp, jit_gpr_t r0, void *p)
{
  jit_insn *l = _jitp->x.pc;
  uint64_t v = (uint64_t)(uintptr_t)p;

  if (!(v >> 16) || !((~v) >> 16)) {
    a64_movi(_jitp, 1, r0, v);
  } else if (!(v >> 48)) {
    A64_MOVWIDE(A64_MOVZ, 1, r0, v & 0xFFFF, 0);
    A64_MOVWIDE(A64_MOVK, 1, r0, (v >> 16) & 0xFFFF, 1);
    A64_MOVWIDE(A64_MOVK, 1, r0, (v >> 32) & 0xFFFF, 2);
  } else
    a64_movi_fixed(_jitp, r0, v);

  return l;
}

/* Like the x86_64 port, which encodes them as 32-bit values that are
   sign-extended, immediate operands to arithmetic and logical
   operations and memory offsets are truncated to 32 bits. Racket's
   JIT relies on that, since it computes some negative offsets using
   unsigned 32-bit arithmetic. Comparisons and moves use the full
   64-bit value. */
/* Avoid INT64_MIN, which MSVC defines using an `i64` suffix that
   xform does not recognize: */
#define A64_INT64_MIN			((int64_t)((uint64_t)1 << 63))
#define _A64_IMM32(i)			((int64_t)(int)(intptr_t)(i))
#define _A64_DISP(i)			((intptr_t)(int)(intptr_t)(i))

#define jit_movr_l(r0, r1)		a64_movr(_jitp, 1, r0, r1)
#define jit_movr_i(r0, r1)		a64_movr(_jitp, 0, r0, r1)
#define jit_movr_p(r0, r1)		jit_movr_l(r0, r1)
#define jit_movr_ul(r0, r1)		jit_movr_l(r0, r1)
#define jit_movi_l(r0, i0)		a64_movi(_jitp, 1, r0, (uint64_t)(intptr_t)(i0))
#define jit_movi_ul(r0, i0)		jit_movi_l(r0, i0)
#define jit_movi_i(r0, i0)		a64_movi(_jitp, 0, r0, (uint64_t)(uint32_t)(int)(i0))
#define jit_movi_ui(r0, i0)		jit_movi_i(r0, i0)
#define jit_movi_p(r0, i0)		a64_movi_p(_jitp, r0, (void *)(i0))

#define jit_patchable_movi_p(r0, i0)	a64_patchable_movi_p(_jitp, r0, (void *)(i0))
__jit_inline jit_insn *
a64_patchable_movi_p(jit_state_t _jitp, jit_gpr_t r0, void *p)
{
  jit_insn *l = _jitp->x.pc;
  a64_movi_fixed(_jitp, r0, (uint64_t)(uintptr_t)p);
  return l;
}

#define jit_patch_movi(pa, pv)		a64_patch_movi((jit_insn *)(pa), (void *)(pv))
__jit_inline void
a64_patch_movi(jit_insn *pa, void *pv)
{
  _ui *u = (_ui *)pa;
  uint64_t v = (uint64_t)(uintptr_t)pv;
  int i;

  for (i = 0; i < 4; i++) {
    jit_assert((u[i] & 0x7f800000) == (i ? A64_MOVK : A64_MOVZ));
    u[i] = (u[i] & ~(0xFFFF << 5)) | ((_ui)((v >> (i * 16)) & 0xFFFF) << 5);
  }
}

/*========================================================================*/
/* Arithmetic and logic                                                   */
/*========================================================================*/

/* Returns a register that holds the value of `r`, which is `r` itself
   unless `r` is the stack pointer. */
__jit_inline jit_gpr_t
a64_src(jit_state_t _jitp, jit_gpr_t r, jit_gpr_t tmp)
{
  if (r == JIT_SP) {
    A64_ADDSUB_IMM(A64_ADDI, 1, tmp, JIT_SP, 0, 0);
    return tmp;
  }
  return r;
}

/* d = s1 +/- s2 */
__jit_inline void
a64_addsubr(jit_state_t _jitp, int sf, int sub, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if ((r0 == JIT_SP) || (r1 == JIT_SP) || (r2 == JIT_SP)) {
    jit_assert(sf);
    if (r2 == JIT_SP) {
      if (!sub && (r1 != JIT_SP)) {
        jit_gpr_t t = r1; r1 = r2; r2 = t;
      } else
        r2 = a64_src(_jitp, r2, JIT_TMP);
    }
    A64_ADDSUB_EXT(sub ? A64_SUBX : A64_ADDX, r0, r1, r2);
  } else
    A64_OP_SHIFTED(sub ? A64_SUB : A64_ADD, sf, r0, r1, r2, 0, 0);
}

__jit_inline void
a64_addi(jit_state_t _jitp, int sf, jit_gpr_t r0, jit_gpr_t r1, int64_t i0)
{
  int sub = 0;
  uint64_t u;

  if (!sf)
    i0 = (int)i0;

  if (i0 < 0 && i0 != A64_INT64_MIN) {
    sub = 1;
    u = (uint64_t)(-i0);
  } else
    u = (uint64_t)i0;

  if (!u) {
    a64_movr(_jitp, sf, r0, r1);
  } else if (_u12P(u))
    A64_ADDSUB_IMM(sub ? A64_SUBI : A64_ADDI, sf, r0, r1, u, 0);
  else if (!(u & 0xFFF) && _u12P(u >> 12))
    A64_ADDSUB_IMM(sub ? A64_SUBI : A64_ADDI, sf, r0, r1, u >> 12, 1);
  else if (_u12P(u & 0xFFF) && _u12P(u >> 12) && (r0 != JIT_SP)) {
    A64_ADDSUB_IMM(sub ? A64_SUBI : A64_ADDI, sf, r0, r1, u >> 12, 1);
    A64_ADDSUB_IMM(sub ? A64_SUBI : A64_ADDI, sf, r0, r0, u & 0xFFF, 0);
  } else {
    a64_movi(_jitp, sf, JIT_TMP2, (uint64_t)i0);
    a64_addsubr(_jitp, sf, 0, r0, r1, JIT_TMP2);
  }
}

#define jit_addr_l(r0, r1, r2)		a64_addsubr(_jitp, 1, 0, r0, r1, r2)
#define jit_addr_i(r0, r1, r2)		a64_addsubr(_jitp, 0, 0, r0, r1, r2)
#define jit_subr_l(r0, r1, r2)		a64_addsubr(_jitp, 1, 1, r0, r1, r2)
#define jit_subr_i(r0, r1, r2)		a64_addsubr(_jitp, 0, 1, r0, r1, r2)
#define jit_addi_l(r0, r1, i0)		a64_addi(_jitp, 1, r0, r1, _A64_IMM32(i0))
#define jit_addi_i(r0, r1, i0)		a64_addi(_jitp, 0, r0, r1, (int64_t)(int)(i0))
#define jit_subi_l(r0, r1, i0)		a64_addi(_jitp, 1, r0, r1, -_A64_IMM32(i0))
#define jit_subi_i(r0, r1, i0)		a64_addi(_jitp, 0, r0, r1, -(int64_t)(int)(i0))
#define jit_rsbr_l(r0, r1, r2)		jit_subr_l(r0, r2, r1)
#define jit_rsbr_i(r0, r1, r2)		jit_subr_i(r0, r2, r1)
#define jit_rsbi_l(r0, r1, i0)		(jit_movi_l(JIT_TMP2, i0), jit_subr_l(r0, JIT_TMP2, r1))
#define jit_rsbi_i(r0, r1, i0)		(jit_movi_i(JIT_TMP2, i0), jit_subr_i(r0, JIT_TMP2, r1))

#define jit_negr_l(r0, r1)		A64_OP_SHIFTED(A64_SUB, 1, r0, _ZR, r1, 0, 0)
#define jit_negr_i(r0, r1)		A64_OP_SHIFTED(A64_SUB, 0, r0, _ZR, r1, 0, 0)
#define jit_notr_l(r0, r1)		A64_OP_SHIFTED(A64_ORN, 1, r0, _ZR, r1, 0, 0)
#define jit_notr_i(r0, r1)		A64_OP_SHIFTED(A64_ORN, 0, r0, _ZR, r1, 0, 0)

#define jit_mulr_l(r0, r1, r2)		A64_MUL(1, r0, r1, r2)
#define jit_mulr_i(r0, r1, r2)		A64_MUL(0, r0, r1, r2)
#define jit_mulr_ui(r0, r1, r2)		A64_MUL(0, r0, r1, r2)
#define jit_muli_l(r0, r1, i0)		(jit_movi_l(JIT_TMP2, _A64_IMM32(i0)), A64_MUL(1, r0, r1, JIT_TMP2))
#define jit_muli_i(r0, r1, i0)		(jit_movi_i(JIT_TMP2, i0), A64_MUL(0, r0, r1, JIT_TMP2))
#define jit_muli_ui(r0, r1, i0)		jit_muli_i(r0, r1, i0)
#define jit_hmulr_l(r0, r1, r2)		A64_SMULHR(r0, r1, r2)

#define jit_divr_l(r0, r1, r2)		A64_OP_RRR(A64_SDIV, 1, r0, r1, r2)
#define jit_divr_i(r0, r1, r2)		A64_OP_RRR(A64_SDIV, 0, r0, r1, r2)
#define jit_divr_ul(r0, r1, r2)		A64_OP_RRR(A64_UDIV, 1, r0, r1, r2)
#define jit_divr_ui(r0, r1, r2)		A64_OP_RRR(A64_UDIV, 0, r0, r1, r2)
#define jit_divi_l(r0, r1, i0)		(jit_movi_l(JIT_TMP2, i0), jit_divr_l(r0, r1, JIT_TMP2))
#define jit_divi_i(r0, r1, i0)		(jit_movi_i(JIT_TMP2, i0), jit_divr_i(r0, r1, JIT_TMP2))
#define jit_modr_l(r0, r1, r2)		a64_modr(_jitp, 1, 1, r0, r1, r2)
#define jit_modr_i(r0, r1, r2)		a64_modr(_jitp, 0, 1, r0, r1, r2)
#define jit_modr_ul(r0, r1, r2)		a64_modr(_jitp, 1, 0, r0, r1, r2)
#define jit_modr_ui(r0, r1, r2)		a64_modr(_jitp, 0, 0, r0, r1, r2)
#define jit_modi_l(r0, r1, i0)		(jit_movi_l(JIT_TMP2, i0), jit_modr_l(r0, r1, JIT_TMP2))
#define jit_modi_i(r0, r1, i0)		(jit_movi_i(JIT_TMP2, i0), jit_modr_i(r0, r1, JIT_TMP2))
__jit_inline void
a64_modr(jit_state_t _jitp, int sf, int sgn, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  jit_assert(r1 != JIT_TMP && r2 != JIT_TMP);
  A64_OP_RRR(sgn ? A64_SDIV : A64_UDIV, sf, JIT_TMP, r1, r2);
  A64_MSUBR(sf, r0, JIT_TMP, r2, r1); /* r0 = r1 - (r1/r2)*r2 */
}

/* Logical operations with an immediate */
__jit_inline void
a64_logici(jit_state_t _jitp, _ui op_i, _ui op_r, int sf, jit_gpr_t r0, jit_gpr_t r1, int64_t i0)
{
  int enc;
  uint64_t v = (uint64_t)i0;

  if (!sf)
    v &= 0xFFFFFFFF;

  if (sf)
    enc = a64_encode_logical_immediate(v);
  else
    enc = a64_encode_logical_immediate32((uint32_t)v);

  if (enc != -1)
    A64_LOGIC_IMM(op_i, sf, r0, r1, enc);
  else {
    a64_movi(_jitp, sf, JIT_TMP2, v);
    A64_OP_SHIFTED(op_r, sf, r0, r1, JIT_TMP2, 0, 0);
  }
}

#define jit_andr_l(r0, r1, r2)		A64_OP_SHIFTED(A64_AND, 1, r0, r1, r2, 0, 0)
#define jit_andr_i(r0, r1, r2)		A64_OP_SHIFTED(A64_AND, 0, r0, r1, r2, 0, 0)
#define jit_orr_l(r0, r1, r2)		A64_OP_SHIFTED(A64_ORR, 1, r0, r1, r2, 0, 0)
#define jit_orr_i(r0, r1, r2)		A64_OP_SHIFTED(A64_ORR, 0, r0, r1, r2, 0, 0)
#define jit_xorr_l(r0, r1, r2)		A64_OP_SHIFTED(A64_EOR, 1, r0, r1, r2, 0, 0)
#define jit_xorr_i(r0, r1, r2)		A64_OP_SHIFTED(A64_EOR, 0, r0, r1, r2, 0, 0)
#define jit_andi_l(r0, r1, i0)		a64_logici(_jitp, A64_ANDI, A64_AND, 1, r0, r1, _A64_IMM32(i0))
#define jit_andi_i(r0, r1, i0)		a64_logici(_jitp, A64_ANDI, A64_AND, 0, r0, r1, (int64_t)(int)(i0))
#define jit_ori_l(r0, r1, i0)		a64_logici(_jitp, A64_ORRI, A64_ORR, 1, r0, r1, _A64_IMM32(i0))
#define jit_ori_i(r0, r1, i0)		a64_logici(_jitp, A64_ORRI, A64_ORR, 0, r0, r1, (int64_t)(int)(i0))
#define jit_xori_l(r0, r1, i0)		a64_logici(_jitp, A64_EORI, A64_EOR, 1, r0, r1, _A64_IMM32(i0))
#define jit_xori_i(r0, r1, i0)		a64_logici(_jitp, A64_EORI, A64_EOR, 0, r0, r1, (int64_t)(int)(i0))

/* Shifts */
__jit_inline void
a64_shifti(jit_state_t _jitp, int sf, int kind, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int bits = (sf ? 64 : 32);
  i0 &= (bits - 1);
  if (!i0)
    a64_movr(_jitp, sf, r0, r1);
  else if (kind == A64_SHIFT_LSL)
    A64_BITFIELD(A64_UBFM, sf, r0, r1, bits - i0, bits - 1 - i0);
  else if (kind == A64_SHIFT_LSR)
    A64_BITFIELD(A64_UBFM, sf, r0, r1, i0, bits - 1);
  else
    A64_BITFIELD(A64_SBFM, sf, r0, r1, i0, bits - 1);
}

#define jit_lshr_l(r0, r1, r2)		A64_OP_RRR(A64_LSLV, 1, r0, r1, r2)
#define jit_lshr_i(r0, r1, r2)		A64_OP_RRR(A64_LSLV, 0, r0, r1, r2)
#define jit_rshr_l(r0, r1, r2)		A64_OP_RRR(A64_ASRV, 1, r0, r1, r2)
#define jit_rshr_i(r0, r1, r2)		A64_OP_RRR(A64_ASRV, 0, r0, r1, r2)
#define jit_rshr_ul(r0, r1, r2)		A64_OP_RRR(A64_LSRV, 1, r0, r1, r2)
#define jit_rshr_ui(r0, r1, r2)		A64_OP_RRR(A64_LSRV, 0, r0, r1, r2)
#define jit_lshi_l(r0, r1, i0)		a64_shifti(_jitp, 1, A64_SHIFT_LSL, r0, r1, i0)
#define jit_lshi_i(r0, r1, i0)		a64_shifti(_jitp, 0, A64_SHIFT_LSL, r0, r1, i0)
#define jit_rshi_l(r0, r1, i0)		a64_shifti(_jitp, 1, A64_SHIFT_ASR, r0, r1, i0)
#define jit_rshi_i(r0, r1, i0)		a64_shifti(_jitp, 0, A64_SHIFT_ASR, r0, r1, i0)
#define jit_rshi_ul(r0, r1, i0)		a64_shifti(_jitp, 1, A64_SHIFT_LSR, r0, r1, i0)
#define jit_rshi_ui(r0, r1, i0)		a64_shifti(_jitp, 0, A64_SHIFT_LSR, r0, r1, i0)

/* Sign and zero extension */
#define jit_extr_c_l(r0, r1)		A64_BITFIELD(A64_SBFM, 1, r0, r1, 0, 7)
#define jit_extr_s_l(r0, r1)		A64_BITFIELD(A64_SBFM, 1, r0, r1, 0, 15)
#define jit_extr_i_l(r0, r1)		A64_BITFIELD(A64_SBFM, 1, r0, r1, 0, 31)
#define jit_extr_c_i(r0, r1)		A64_BITFIELD(A64_SBFM, 0, r0, r1, 0, 7)
#define jit_extr_s_i(r0, r1)		A64_BITFIELD(A64_SBFM, 0, r0, r1, 0, 15)
#define jit_extr_c_ul(r0, r1)		A64_BITFIELD(A64_UBFM, 0, r0, r1, 0, 7)
#define jit_extr_s_ul(r0, r1)		A64_BITFIELD(A64_UBFM, 0, r0, r1, 0, 15)
#define jit_extr_i_ul(r0, r1)		A64_OP_SHIFTED(A64_ORR, 0, r0, _ZR, r1, 0, 0)
#define jit_extr_c_ui(r0, r1)		jit_extr_c_ul(r0, r1)
#define jit_extr_s_ui(r0, r1)		jit_extr_s_ul(r0, r1)

/*========================================================================*/
/* Branches                                                               */
/*========================================================================*/

/* Emits a branch to `i0` that is taken when `cc` holds. Returns the
   address to use with jit_patch_at().

   In short-jump mode, a target is expected to be within the code
   block being generated: either a backward target or a forward one
   that is patched later (in which case `i0` is the current address).
   Racket's JIT sometimes branches to separately allocated code while
   in short-jump mode, which is fine on x86_64 where "short" means
   +/- 2^31, but such a target is not necessarily in range of an
   AArch64 branch; recognize that case and use the long form, which
   keeps the code size the same no matter where the block resides. */
__jit_inline jit_insn *
a64_branch(jit_state_t _jitp, int cc, jit_insn *i0)
{
  jit_insn *l;

  if (!_jitl.long_jumps
      && ((uintptr_t)i0 >= (uintptr_t)_jitl.block_start)
      && ((uintptr_t)i0 <= (uintptr_t)_jitp->x.pc)) {
    intptr_t d;
    l = _jitp->x.pc;
    d = ((intptr_t)i0 - (intptr_t)l) >> 2;
    if (cc == A64_CC_AL) {
      jit_assert(_s26P(d));
      A64_BI(d);
    } else {
      jit_assert(_s19P(d));
      A64_BCC(cc, d);
    }
  } else {
    if (cc != A64_CC_AL)
      A64_BCC(a64_invert_cc(cc), 6); /* skip 4 MOVs and BR */
    l = _jitp->x.pc;
    a64_movi_fixed(_jitp, JIT_TMP, (uint64_t)(uintptr_t)i0);
    A64_BRR(JIT_TMP);
  }

  return l;
}

#define jit_patch_at(jump, label)	a64_patch_at((jit_insn *)(jump), (jit_insn *)(label))
#define jit_patch_calli(call, label)	a64_patch_at((jit_insn *)(call), (jit_insn *)(label))
__jit_inline void
a64_patch_at(jit_insn *jump, jit_insn *label)
{
  _ui *u = (_ui *)jump;
  _ui i = u[0];
  intptr_t d = ((intptr_t)label - (intptr_t)jump) >> 2;

  if ((i & 0x7c000000) == A64_B) {
    /* B or BL */
    jit_assert(_s26P(d));
    u[0] = (i & 0xfc000000) | ((_ui)d & 0x3ffffff);
  } else if ((i & 0xff000010) == A64_B_C) {
    jit_assert(_s19P(d));
    u[0] = (i & 0xff00001f) | (((_ui)d & 0x7ffff) << 5);
  } else if ((i & 0xff800000) == (A64_MOVZ | 0x80000000)) {
    a64_patch_movi(jump, label);
  } else
    jit_assert(!"unhandled branch opcode");
}

__jit_inline void
a64_cmpi(jit_state_t _jitp, int sf, jit_gpr_t r0, int64_t i0)
{
  uint64_t u;
  int neg = 0;

  if (!sf)
    i0 = (int)i0;

  if (i0 < 0 && i0 != A64_INT64_MIN) {
    neg = 1;
    u = (uint64_t)(-i0);
  } else
    u = (uint64_t)i0;

  if (_u12P(u))
    A64_ADDSUB_IMM(neg ? A64_ADDSI : A64_SUBSI, sf, _ZR, r0, u, 0);
  else if (!(u & 0xFFF) && _u12P(u >> 12))
    A64_ADDSUB_IMM(neg ? A64_ADDSI : A64_SUBSI, sf, _ZR, r0, u >> 12, 1);
  else {
    a64_movi(_jitp, sf, JIT_TMP2, (uint64_t)i0);
    if (r0 == JIT_SP)
      A64_ADDSUB_EXT(A64_SUBSX, _ZR, r0, JIT_TMP2);
    else
      A64_OP_SHIFTED(A64_SUBS, sf, _ZR, r0, JIT_TMP2, 0, 0);
  }
}

__jit_inline void
a64_cmpr(jit_state_t _jitp, int sf, jit_gpr_t r0, jit_gpr_t r1)
{
  if ((r0 == JIT_SP) || (r1 == JIT_SP)) {
    jit_assert(sf);
    r1 = a64_src(_jitp, r1, JIT_TMP2);
    A64_ADDSUB_EXT(A64_SUBSX, _ZR, r0, r1);
  } else
    A64_OP_SHIFTED(A64_SUBS, sf, _ZR, r0, r1, 0, 0);
}

__jit_inline jit_insn *
a64_bccr(jit_state_t _jitp, int sf, int cc, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  a64_cmpr(_jitp, sf, r0, r1);
  return a64_branch(_jitp, cc, i0);
}

__jit_inline jit_insn *
a64_bcci(jit_state_t _jitp, int sf, int cc, jit_insn *i0, jit_gpr_t r0, int64_t i1)
{
  a64_cmpi(_jitp, sf, r0, i1);
  return a64_branch(_jitp, cc, i0);
}

#define jit_bltr_l(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_LT, i0, r0, r1)
#define jit_bler_l(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_LE, i0, r0, r1)
#define jit_bgtr_l(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_GT, i0, r0, r1)
#define jit_bger_l(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_GE, i0, r0, r1)
#define jit_beqr_l(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_EQ, i0, r0, r1)
#define jit_bner_l(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_NE, i0, r0, r1)
#define jit_bltr_ul(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_LO, i0, r0, r1)
#define jit_bler_ul(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_LS, i0, r0, r1)
#define jit_bgtr_ul(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_HI, i0, r0, r1)
#define jit_bger_ul(i0, r0, r1)		a64_bccr(_jitp, 1, A64_CC_HS, i0, r0, r1)
#define jit_beqr_ul(i0, r0, r1)		jit_beqr_l(i0, r0, r1)
#define jit_bner_ul(i0, r0, r1)		jit_bner_l(i0, r0, r1)

#define jit_blti_l(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_LT, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_blei_l(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_LE, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_bgti_l(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_GT, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_bgei_l(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_GE, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_beqi_l(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_EQ, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_bnei_l(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_NE, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_blti_ul(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_LO, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_blei_ul(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_LS, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_bgti_ul(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_HI, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_bgei_ul(i0, r0, i1)		a64_bcci(_jitp, 1, A64_CC_HS, i0, r0, (int64_t)(intptr_t)(i1))
#define jit_beqi_ul(i0, r0, i1)		jit_beqi_l(i0, r0, i1)
#define jit_bnei_ul(i0, r0, i1)		jit_bnei_l(i0, r0, i1)

#define jit_bltr_i(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_LT, i0, r0, r1)
#define jit_bler_i(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_LE, i0, r0, r1)
#define jit_bgtr_i(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_GT, i0, r0, r1)
#define jit_bger_i(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_GE, i0, r0, r1)
#define jit_beqr_i(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_EQ, i0, r0, r1)
#define jit_bner_i(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_NE, i0, r0, r1)
#define jit_bltr_ui(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_LO, i0, r0, r1)
#define jit_bler_ui(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_LS, i0, r0, r1)
#define jit_bgtr_ui(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_HI, i0, r0, r1)
#define jit_bger_ui(i0, r0, r1)		a64_bccr(_jitp, 0, A64_CC_HS, i0, r0, r1)

#define jit_blti_i(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_LT, i0, r0, (int64_t)(int)(i1))
#define jit_blei_i(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_LE, i0, r0, (int64_t)(int)(i1))
#define jit_bgti_i(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_GT, i0, r0, (int64_t)(int)(i1))
#define jit_bgei_i(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_GE, i0, r0, (int64_t)(int)(i1))
#define jit_beqi_i(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_EQ, i0, r0, (int64_t)(int)(i1))
#define jit_bnei_i(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_NE, i0, r0, (int64_t)(int)(i1))
#define jit_blti_ui(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_LO, i0, r0, (int64_t)(int)(i1))
#define jit_blei_ui(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_LS, i0, r0, (int64_t)(int)(i1))
#define jit_bgti_ui(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_HI, i0, r0, (int64_t)(int)(i1))
#define jit_bgei_ui(i0, r0, i1)		a64_bcci(_jitp, 0, A64_CC_HS, i0, r0, (int64_t)(int)(i1))

/* Mask tests */
__jit_inline jit_insn *
a64_bmxr(jit_state_t _jitp, int sf, int cc, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  A64_OP_SHIFTED(A64_ANDS, sf, _ZR, r0, r1, 0, 0);
  return a64_branch(_jitp, cc, i0);
}

__jit_inline jit_insn *
a64_bmxi(jit_state_t _jitp, int sf, int cc, jit_insn *i0, jit_gpr_t r0, int64_t i1)
{
  int enc;
  uint64_t v = (uint64_t)i1;

  if (!sf)
    v &= 0xFFFFFFFF;

  if (sf)
    enc = a64_encode_logical_immediate(v);
  else
    enc = a64_encode_logical_immediate32((uint32_t)v);

  if (enc != -1)
    A64_LOGIC_IMM(A64_ANDSI, sf, _ZR, r0, enc);
  else {
    a64_movi(_jitp, sf, JIT_TMP2, v);
    A64_OP_SHIFTED(A64_ANDS, sf, _ZR, r0, JIT_TMP2, 0, 0);
  }
  return a64_branch(_jitp, cc, i0);
}

/* Like x86_64, `_i` register-register mask tests use all 64 bits */
#define jit_bmsr_l(i0, r0, r1)		a64_bmxr(_jitp, 1, A64_CC_NE, i0, r0, r1)
#define jit_bmcr_l(i0, r0, r1)		a64_bmxr(_jitp, 1, A64_CC_EQ, i0, r0, r1)
#define jit_bmsr_i(i0, r0, r1)		a64_bmxr(_jitp, 1, A64_CC_NE, i0, r0, r1)
#define jit_bmcr_i(i0, r0, r1)		a64_bmxr(_jitp, 1, A64_CC_EQ, i0, r0, r1)
#define jit_bmsi_l(i0, r0, i1)		a64_bmxi(_jitp, 1, A64_CC_NE, i0, r0, _A64_IMM32(i1))
#define jit_bmci_l(i0, r0, i1)		a64_bmxi(_jitp, 1, A64_CC_EQ, i0, r0, _A64_IMM32(i1))
#define jit_bmsi_i(i0, r0, i1)		a64_bmxi(_jitp, 0, A64_CC_NE, i0, r0, (int64_t)(int)(i1))
#define jit_bmci_i(i0, r0, i1)		a64_bmxi(_jitp, 0, A64_CC_EQ, i0, r0, (int64_t)(int)(i1))
#define jit_bmsr_ul(i0, r0, r1)		jit_bmsr_l(i0, r0, r1)
#define jit_bmcr_ul(i0, r0, r1)		jit_bmcr_l(i0, r0, r1)
#define jit_bmsi_ul(i0, r0, i1)		jit_bmsi_l(i0, r0, i1)
#define jit_bmci_ul(i0, r0, i1)		jit_bmci_l(i0, r0, i1)

/* Overflow (signed) and carry/borrow (unsigned) branches; the first
   register operand is updated with the result */
__jit_inline jit_insn *
a64_baddsubr(jit_state_t _jitp, int sf, int sub, int cc, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  A64_OP_SHIFTED(sub ? A64_SUBS : A64_ADDS, sf, r0, r0, r1, 0, 0);
  return a64_branch(_jitp, cc, i0);
}

__jit_inline jit_insn *
a64_baddsubi(jit_state_t _jitp, int sf, int sub, int cc, jit_insn *i0, jit_gpr_t r0, int64_t i1)
{
  if (!sf)
    i1 = (int)i1;

  if (_u12P(i1))
    A64_ADDSUB_IMM(sub ? A64_SUBSI : A64_ADDSI, sf, r0, r0, i1, 0);
  else if ((cc == A64_CC_VS) && (i1 < 0) && _u12P(-i1))
    A64_ADDSUB_IMM(sub ? A64_ADDSI : A64_SUBSI, sf, r0, r0, -i1, 0);
  else {
    a64_movi(_jitp, sf, JIT_TMP2, (uint64_t)i1);
    A64_OP_SHIFTED(sub ? A64_SUBS : A64_ADDS, sf, r0, r0, JIT_TMP2, 0, 0);
  }
  return a64_branch(_jitp, cc, i0);
}

#define jit_boaddr_l(i0, r0, r1)	a64_baddsubr(_jitp, 1, 0, A64_CC_VS, i0, r0, r1)
#define jit_bosubr_l(i0, r0, r1)	a64_baddsubr(_jitp, 1, 1, A64_CC_VS, i0, r0, r1)
#define jit_boaddi_l(i0, r0, i1)	a64_baddsubi(_jitp, 1, 0, A64_CC_VS, i0, r0, _A64_IMM32(i1))
#define jit_bosubi_l(i0, r0, i1)	a64_baddsubi(_jitp, 1, 1, A64_CC_VS, i0, r0, _A64_IMM32(i1))
#define jit_boaddr_ul(i0, r0, r1)	a64_baddsubr(_jitp, 1, 0, A64_CC_HS, i0, r0, r1)
#define jit_bosubr_ul(i0, r0, r1)	a64_baddsubr(_jitp, 1, 1, A64_CC_LO, i0, r0, r1)
#define jit_boaddi_ul(i0, r0, i1)	a64_baddsubi(_jitp, 1, 0, A64_CC_HS, i0, r0, _A64_IMM32(i1))
#define jit_bosubi_ul(i0, r0, i1)	a64_baddsubi(_jitp, 1, 1, A64_CC_LO, i0, r0, _A64_IMM32(i1))
#define jit_boaddr_i(i0, r0, r1)	a64_baddsubr(_jitp, 0, 0, A64_CC_VS, i0, r0, r1)
#define jit_bosubr_i(i0, r0, r1)	a64_baddsubr(_jitp, 0, 1, A64_CC_VS, i0, r0, r1)
#define jit_boaddi_i(i0, r0, i1)	a64_baddsubi(_jitp, 0, 0, A64_CC_VS, i0, r0, (int64_t)(int)(i1))
#define jit_bosubi_i(i0, r0, i1)	a64_baddsubi(_jitp, 0, 1, A64_CC_VS, i0, r0, (int64_t)(int)(i1))
#define jit_boaddr_ui(i0, r0, r1)	a64_baddsubr(_jitp, 0, 0, A64_CC_HS, i0, r0, r1)
#define jit_bosubr_ui(i0, r0, r1)	a64_baddsubr(_jitp, 0, 1, A64_CC_LO, i0, r0, r1)

/* r0 = r0 * r1, branching on signed overflow */
#define jit_bomulr_l(i0, r0, r1)	a64_bomulr_l(_jitp, i0, r0, r1)
__jit_inline jit_insn *
a64_bomulr_l(jit_state_t _jitp, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  A64_SMULHR(JIT_TMP2, r0, r1);
  A64_MUL(1, r0, r0, r1);
  /* overflow if high part is not the sign extension of the low part */
  A64_OP_SHIFTED(A64_SUBS, 1, _ZR, JIT_TMP2, r0, A64_SHIFT_ASR, 63);
  return a64_branch(_jitp, A64_CC_NE, i0);
}

#define jit_jmpi(i0)			a64_branch(_jitp, A64_CC_AL, (jit_insn *)(i0))
/* Use RET for the link register so that the return predictor stays in
   sync, which is consistent with how mz_epilog() uses jit_jmpr() */
#define jit_jmpr(r0)			((r0) == JIT_LR ? A64_RETR(r0) : A64_BRR(r0))

/*========================================================================*/
/* Loads and stores                                                       */
/*========================================================================*/

/* `op` is an unsigned-scaled-immediate opcode, and `sz` is log2 of the
   access size. `rt` is a general or FP register, depending on `op`. */
__jit_inline void
a64_ldst(jit_state_t _jitp, _ui op, int sz, int rt, jit_gpr_t rn, intptr_t off)
{
  if ((off >= 0) && !(off & ((1 << sz) - 1)) && _u12P(off >> sz)) {
    _jit_I(op | (((_ui)(off >> sz)) << 10) | (_A64_RSP(rn) << 5) | (_ui)rt);
  } else if (_s9P(off)) {
    _jit_I((op & ~0x01000000) | ((((_ui)off) & 0x1ff) << 12) | (_A64_RSP(rn) << 5) | (_ui)rt);
  } else {
    jit_assert(rn != JIT_TMP);
    a64_movi(_jitp, 1, JIT_TMP, (uint64_t)off);
    _jit_I((op & ~0x01000000) | 0x00206800 | (_A64_R(JIT_TMP) << 16) | (_A64_RSP(rn) << 5) | (_ui)rt);
  }
}

__jit_inline void
a64_ldstr(jit_state_t _jitp, _ui op, int rt, jit_gpr_t rn, jit_gpr_t rm)
{
  if (rm == JIT_SP) {
    jit_gpr_t t = rn; rn = rm; rm = t;
  }
  jit_assert(rm != JIT_SP);
  _jit_I((op & ~0x01000000) | 0x00206800 | (_A64_R(rm) << 16) | (_A64_RSP(rn) << 5) | (_ui)rt);
}

__jit_inline void
a64_ldsti(jit_state_t _jitp, _ui op, int sz, int rt, void *addr)
{
  a64_movi_p(_jitp, JIT_TMP, addr);
  a64_ldst(_jitp, op, sz, rt, JIT_TMP, 0);
}

/* Stores; the stack pointer cannot be stored directly, so it's
   first copied to JIT_TMP2 */
__jit_inline void
a64_st(jit_state_t _jitp, _ui op, int sz, jit_gpr_t rt, jit_gpr_t rn, intptr_t off)
{
  rt = a64_src(_jitp, rt, JIT_TMP2);
  a64_ldst(_jitp, op, sz, rt, rn, off);
}

__jit_inline void
a64_str(jit_state_t _jitp, _ui op, jit_gpr_t rt, jit_gpr_t rn, jit_gpr_t rm)
{
  rt = a64_src(_jitp, rt, JIT_TMP2);
  a64_ldstr(_jitp, op, rt, rn, rm);
}

__jit_inline void
a64_sti(jit_state_t _jitp, _ui op, int sz, jit_gpr_t rt, void *addr)
{
  rt = a64_src(_jitp, rt, JIT_TMP2);
  a64_ldsti(_jitp, op, sz, rt, addr);
}

#define jit_ldxi_c(r0, r1, i0)		a64_ldst(_jitp, A64_LDRSBW, 0, _A64_R(r0), r1, _A64_DISP(i0))
#define jit_ldxi_uc(r0, r1, i0)		a64_ldst(_jitp, A64_LDRB, 0, _A64_R(r0), r1, _A64_DISP(i0))
#define jit_ldxi_s(r0, r1, i0)		a64_ldst(_jitp, A64_LDRSHW, 1, _A64_R(r0), r1, _A64_DISP(i0))
#define jit_ldxi_us(r0, r1, i0)		a64_ldst(_jitp, A64_LDRH, 1, _A64_R(r0), r1, _A64_DISP(i0))
#define jit_ldxi_i(r0, r1, i0)		a64_ldst(_jitp, A64_LDRW, 2, _A64_R(r0), r1, _A64_DISP(i0))
#define jit_ldxi_ui(r0, r1, i0)		jit_ldxi_i(r0, r1, i0)
#define jit_ldxi_l(r0, r1, i0)		a64_ldst(_jitp, A64_LDRX, 3, _A64_R(r0), r1, _A64_DISP(i0))

#define jit_ldxr_c(r0, r1, r2)		a64_ldstr(_jitp, A64_LDRSBW, _A64_R(r0), r1, r2)
#define jit_ldxr_uc(r0, r1, r2)		a64_ldstr(_jitp, A64_LDRB, _A64_R(r0), r1, r2)
/* Like x86_64, a 16-bit indexed load sign-extends to 64 bits: */
#define jit_ldxr_s(r0, r1, r2)		a64_ldstr(_jitp, A64_LDRSHX, _A64_R(r0), r1, r2)
#define jit_ldxr_us(r0, r1, r2)		a64_ldstr(_jitp, A64_LDRH, _A64_R(r0), r1, r2)
#define jit_ldxr_i(r0, r1, r2)		a64_ldstr(_jitp, A64_LDRW, _A64_R(r0), r1, r2)
#define jit_ldxr_ui(r0, r1, r2)		jit_ldxr_i(r0, r1, r2)
#define jit_ldxr_l(r0, r1, r2)		a64_ldstr(_jitp, A64_LDRX, _A64_R(r0), r1, r2)

#define jit_ldr_c(r0, r1)		jit_ldxi_c(r0, r1, 0)
#define jit_ldr_uc(r0, r1)		jit_ldxi_uc(r0, r1, 0)
#define jit_ldr_s(r0, r1)		jit_ldxi_s(r0, r1, 0)
#define jit_ldr_us(r0, r1)		jit_ldxi_us(r0, r1, 0)
#define jit_ldr_i(r0, r1)		jit_ldxi_i(r0, r1, 0)
#define jit_ldr_ui(r0, r1)		jit_ldxi_ui(r0, r1, 0)
#define jit_ldr_l(r0, r1)		jit_ldxi_l(r0, r1, 0)

#define jit_ldi_c(r0, i0)		a64_ldsti(_jitp, A64_LDRSBW, 0, _A64_R(r0), (void *)(i0))
#define jit_ldi_uc(r0, i0)		a64_ldsti(_jitp, A64_LDRB, 0, _A64_R(r0), (void *)(i0))
#define jit_ldi_s(r0, i0)		a64_ldsti(_jitp, A64_LDRSHW, 1, _A64_R(r0), (void *)(i0))
#define jit_ldi_us(r0, i0)		a64_ldsti(_jitp, A64_LDRH, 1, _A64_R(r0), (void *)(i0))
#define jit_ldi_i(r0, i0)		a64_ldsti(_jitp, A64_LDRW, 2, _A64_R(r0), (void *)(i0))
#define jit_ldi_ui(r0, i0)		jit_ldi_i(r0, i0)
#define jit_ldi_l(r0, i0)		a64_ldsti(_jitp, A64_LDRX, 3, _A64_R(r0), (void *)(i0))

#define jit_stxi_c(i0, r0, r1)		a64_st(_jitp, A64_STRB, 0, r1, r0, _A64_DISP(i0))
#define jit_stxi_s(i0, r0, r1)		a64_st(_jitp, A64_STRH, 1, r1, r0, _A64_DISP(i0))
#define jit_stxi_i(i0, r0, r1)		a64_st(_jitp, A64_STRW, 2, r1, r0, _A64_DISP(i0))
#define jit_stxi_l(i0, r0, r1)		a64_st(_jitp, A64_STRX, 3, r1, r0, _A64_DISP(i0))

#define jit_stxr_c(r0, r1, r2)		a64_str(_jitp, A64_STRB, r2, r0, r1)
#define jit_stxr_s(r0, r1, r2)		a64_str(_jitp, A64_STRH, r2, r0, r1)
#define jit_stxr_i(r0, r1, r2)		a64_str(_jitp, A64_STRW, r2, r0, r1)
#define jit_stxr_l(r0, r1, r2)		a64_str(_jitp, A64_STRX, r2, r0, r1)

#define jit_str_c(r0, r1)		jit_stxi_c(0, r0, r1)
#define jit_str_s(r0, r1)		jit_stxi_s(0, r0, r1)
#define jit_str_i(r0, r1)		jit_stxi_i(0, r0, r1)
#define jit_str_l(r0, r1)		jit_stxi_l(0, r0, r1)

#define jit_sti_c(i0, r0)		a64_sti(_jitp, A64_STRB, 0, r0, (void *)(i0))
#define jit_sti_s(i0, r0)		a64_sti(_jitp, A64_STRH, 1, r0, (void *)(i0))
#define jit_sti_i(i0, r0)		a64_sti(_jitp, A64_STRW, 2, r0, (void *)(i0))
#define jit_sti_l(i0, r0)		a64_sti(_jitp, A64_STRX, 3, r0, (void *)(i0))

/* Store an immediate; JIT_TMP2 holds the value, while JIT_TMP may be
   needed for the address */
#define jit_stixi_l(id, rd, is)		(jit_movi_l(JIT_TMP2, is), jit_stxi_l(id, rd, JIT_TMP2))
#define jit_stixi_p(id, rd, is)		jit_stixi_l(id, rd, (intptr_t)(is))
#define jit_stir_l(rd, is)		(jit_movi_l(JIT_TMP2, (intptr_t)(is)), jit_str_l(rd, JIT_TMP2))
#define jit_stir_p(rd, is)		jit_stir_l(rd, is)

/* Push and pop use a full 16 bytes each to keep SP aligned */
#define jit_pushr_l(r0)			_jit_I(0xf8000c00 | ((0x1f0 & 0x1ff) << 12) | (31 << 5) | _A64_R(r0))
#define jit_popr_l(r0)			_jit_I(0xf8400400 | ((16 & 0x1ff) << 12) | (31 << 5) | _A64_R(r0))
#define jit_pushr_i(r0)			jit_pushr_l(r0)
#define jit_popr_i(r0)			jit_popr_l(r0)

/*========================================================================*/
/* Functions                                                              */
/*========================================================================*/

#define jit_prolog(n)			a64_prolog(_jitp, n)
__jit_inline void
a64_prolog(jit_state_t _jitp, int n)
{
  jit_assert(n >= 0 && n <= JIT_MAX_ARGS);

  /* stp x29, x30, [sp, #-JIT_FRAME_SIZE]! */
  A64_LDSTP(A64_STP_PRE, JIT_FP, JIT_LR, JIT_SP, -JIT_FRAME_SIZE);
  /* mov x29, sp */
  A64_ADDSUB_IMM(A64_ADDI, 1, JIT_FP, JIT_SP, 0, 0);
  /* save callee-saved registers that are used as V registers */
  A64_LDSTP(A64_STP, _X19, _X20, JIT_SP, JIT_FRAME_SAVED_OFFSET);
  A64_LDSTP(A64_STP, _X21, _X22, JIT_SP, JIT_FRAME_SAVED_OFFSET + 16);
  /* save arguments for jit_getarg_*() */
  if (n == 1)
    a64_ldst(_jitp, A64_STRX, 3, _X0, JIT_SP, JIT_FRAME_ARGS_OFFSET);
  else if (n >= 2)
    A64_LDSTP(A64_STP, _X0, _X1, JIT_SP, JIT_FRAME_ARGS_OFFSET);
  if (n == 3)
    a64_ldst(_jitp, A64_STRX, 3, _X2, JIT_SP, JIT_FRAME_ARGS_OFFSET + 16);
  else if (n == 4)
    A64_LDSTP(A64_STP, _X2, _X3, JIT_SP, JIT_FRAME_ARGS_OFFSET + 16);

  _jitl.nextarg_get = 0;
}

#define jit_ret()			a64_ret(_jitp)
__jit_inline void
a64_ret(jit_state_t _jitp)
{
  A64_LDSTP(A64_LDP, _X19, _X20, JIT_FP, JIT_FRAME_SAVED_OFFSET);
  A64_LDSTP(A64_LDP, _X21, _X22, JIT_FP, JIT_FRAME_SAVED_OFFSET + 16);
  /* mov sp, x29 */
  A64_ADDSUB_IMM(A64_ADDI, 1, JIT_SP, JIT_FP, 0, 0);
  /* ldp x29, x30, [sp], #JIT_FRAME_SIZE */
  A64_LDSTP(A64_LDP_POST, JIT_FP, JIT_LR, JIT_SP, JIT_FRAME_SIZE);
  A64_RETR(JIT_LR);
}

#define jit_callr(r0)			A64_BLRR(r0)

#define jit_calli(i0)			a64_calli(_jitp, (void *)(i0))
__jit_inline jit_insn *
a64_calli(jit_state_t _jitp, void *i0)
{
  jit_insn *l = _jitp->x.pc;
  a64_movi_fixed(_jitp, JIT_TMP, (uint64_t)(uintptr_t)i0);
  A64_BLRR(JIT_TMP);
  return l;
}

/* A call within the same code block */
#define jit_short_calli(i0)		a64_short_calli(_jitp, (jit_insn *)(i0))
__jit_inline jit_insn *
a64_short_calli(jit_state_t _jitp, jit_insn *i0)
{
  jit_insn *l = _jitp->x.pc;
  intptr_t d = ((intptr_t)i0 - (intptr_t)l) >> 2;
  jit_assert(_s26P(d));
  A64_BLI(d);
  return l;
}

#define jit_prepare_i(i0)		a64_prepare_i(_jitp, i0)
__jit_inline void
a64_prepare_i(jit_state_t _jitp, int i0)
{
  jit_assert(i0 >= 0 && i0 <= JIT_MAX_ARGS && !_jitl.nextarg_put);
}

#define jit_arg_c()			a64_arg(_jitp)
#define jit_arg_uc()			a64_arg(_jitp)
#define jit_arg_s()			a64_arg(_jitp)
#define jit_arg_us()			a64_arg(_jitp)
#define jit_arg_i()			a64_arg(_jitp)
#define jit_arg_ui()			a64_arg(_jitp)
#define jit_arg_l()			a64_arg(_jitp)
#define jit_arg_ul()			a64_arg(_jitp)
#define jit_arg_p()			a64_arg(_jitp)
__jit_inline int
a64_arg(jit_state_t _jitp)
{
  int ofs = _jitl.nextarg_get++;
  jit_assert(ofs < JIT_MAX_ARGS);
  return JIT_FRAME_ARGS_OFFSET + (ofs << 3);
}

/* Arguments are saved in the frame by jit_prolog() */
#define jit_getarg_c(r0, ofs)		jit_ldxi_c(r0, JIT_FP, ofs)
#define jit_getarg_uc(r0, ofs)		jit_ldxi_uc(r0, JIT_FP, ofs)
#define jit_getarg_s(r0, ofs)		jit_ldxi_s(r0, JIT_FP, ofs)
#define jit_getarg_us(r0, ofs)		jit_ldxi_us(r0, JIT_FP, ofs)
#define jit_getarg_i(r0, ofs)		jit_ldxi_i(r0, JIT_FP, ofs)
#define jit_getarg_ui(r0, ofs)		jit_ldxi_ui(r0, JIT_FP, ofs)
#define jit_getarg_l(r0, ofs)		jit_ldxi_l(r0, JIT_FP, ofs)
#define jit_getarg_ul(r0, ofs)		jit_ldxi_l(r0, JIT_FP, ofs)
#define jit_getarg_p(r0, ofs)		jit_ldxi_l(r0, JIT_FP, ofs)

/* Arguments are pushed last-to-first. They are staged in registers
   starting with JIT_ARG_STAGE, so that pushing an argument does not
   clobber an argument register that is the source of another
   argument. */
#define jit_pusharg_i(r0)		a64_pusharg(_jitp, r0)
#define jit_pusharg_l(r0)		a64_pusharg(_jitp, r0)
__jit_inline void
a64_pusharg(jit_state_t _jitp, jit_gpr_t r0)
{
  int ofs = _jitl.nextarg_put++;
  jit_assert(ofs < JIT_MAX_ARGS);
  a64_movr(_jitp, 1, JIT_ARG_STAGE + ofs, r0);
}

__jit_inline void
a64_patch_arguments(jit_state_t _jitp)
{
  int n = _jitl.nextarg_put, i;
  _jitl.nextarg_put = 0;
  for (i = 0; i < n; i++)
    a64_movr(_jitp, 1, JIT_R(i), JIT_ARG_STAGE + (n - 1 - i));
}

#define jit_normal_pushonlyarg_i(r0)	jit_movr_l(JIT_R0, r0)

#define jit_finishr(r0)			a64_finishr(_jitp, r0)
__jit_inline void
a64_finishr(jit_state_t _jitp, jit_gpr_t r0)
{
  jit_movr_l(JIT_TMP2, r0);
  a64_patch_arguments(_jitp);
  A64_BLRR(JIT_TMP2);
}

#define jit_finish(i0)			a64_finishi(_jitp, (void *)(i0))
__jit_inline jit_insn *
a64_finishi(jit_state_t _jitp, void *i0)
{
  a64_patch_arguments(_jitp);
  return a64_calli(_jitp, i0);
}

#define jit_normal_finish(i0)		a64_calli(_jitp, (void *)(i0))

#define jit_retval_i(r0)		jit_movr_l(r0, JIT_RET)
#define jit_retval_l(r0)		jit_movr_l(r0, JIT_RET)

#endif /* __lightning_core_arm64_h */
