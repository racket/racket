/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler & support macros for the AArch64 math unit
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
 * Instruction selection is derived from GNU lightning 2.x
 * ("lib/jit_aarch64-fpu.c"); the comparison and branch conventions
 * follow the lightning 1.x-style VFP layer in "../arm/fp.h".
 ***********************************************************************/

#ifndef __lightning_fp_arm64_h
#define __lightning_fp_arm64_h

/* v0-v7 are not preserved across calls, but Racket's JIT does not keep
   floating-point values in registers across calls. */
#define JIT_FPR_NUM			6
#define JIT_FPR(n)			(n)

#define jit_movr_d(f0, f1)		((f0) != (f1) ? (void)A64_FOP_RR(A64_FMOV, f0, f1) : (void)0)
#define jit_movr_f(f0, f1)		jit_movr_d(f0, f1)

#define jit_movi_d(f0, d0)		a64_movi_d(_jitp, f0, d0)
__jit_inline void
a64_movi_d(jit_state_t _jitp, jit_fpr_t f0, double d0)
{
  union { double d; uint64_t l; } u;
  u.d = d0;
  if (!u.l)
    A64_FOP_FG(A64_FMOV_DX, f0, _ZR);
  else {
    a64_movi(_jitp, 1, JIT_TMP2, u.l);
    A64_FOP_FG(A64_FMOV_DX, f0, JIT_TMP2);
  }
}

#define jit_addr_d(f0, f1, f2)		A64_FOP_RRR(A64_FADD, f0, f1, f2)
#define jit_subr_d(f0, f1, f2)		A64_FOP_RRR(A64_FSUB, f0, f1, f2)
#define jit_subrr_d(f0, f1, f2)		A64_FOP_RRR(A64_FSUB, f0, f2, f1)
#define jit_mulr_d(f0, f1, f2)		A64_FOP_RRR(A64_FMUL, f0, f1, f2)
#define jit_divr_d(f0, f1, f2)		A64_FOP_RRR(A64_FDIV, f0, f1, f2)
#define jit_divrr_d(f0, f1, f2)		A64_FOP_RRR(A64_FDIV, f0, f2, f1)
#define jit_negr_d(f0, f1)		A64_FOP_RR(A64_FNEG, f0, f1)
#define jit_absr_d(f0, f1)		A64_FOP_RR(A64_FABS, f0, f1)
#define jit_abs_d(f0, f1)		jit_absr_d(f0, f1)
#define jit_sqrtr_d(f0, f1)		A64_FOP_RR(A64_FSQRT, f0, f1)
#define jit_sqrt_d(f0, f1)		jit_sqrtr_d(f0, f1)

#define jit_extr_i_d(f0, r0)		A64_FOP_FG(A64_SCVTF_DW, f0, r0)
#define jit_extr_l_d(f0, r0)		A64_FOP_FG(A64_SCVTF_DX, f0, r0)
#define jit_extr_d_f(f0, f1)		A64_FOP_RR(A64_FCVT_SD, f0, f1)
#define jit_extr_f_d(f0, f1)		A64_FOP_RR(A64_FCVT_DS, f0, f1)

#define jit_truncr_d_i(r0, f0)		A64_FOP_GF(A64_FCVTZS_WD, r0, f0)
#define jit_truncr_d_l(r0, f0)		A64_FOP_GF(A64_FCVTZS_XD, r0, f0)
/* As in the x86 SSE port: Racket uses rounding only for
   inexact->exact of fixnum-sized values, where truncation suffices. */
#define jit_roundr_d_i(r0, f0)		jit_truncr_d_i(r0, f0)
#define jit_roundr_d_l(r0, f0)		jit_truncr_d_l(r0, f0)

/* Loads and stores */
#define jit_ldxi_d(f0, r0, i0)		a64_ldst(_jitp, A64_LDRD, 3, _A64_F(f0), r0, _A64_DISP(i0))
#define jit_ldxi_f(f0, r0, i0)		a64_ldst(_jitp, A64_LDRS, 2, _A64_F(f0), r0, _A64_DISP(i0))
#define jit_ldxr_d(f0, r0, r1)		a64_ldstr(_jitp, A64_LDRD, _A64_F(f0), r0, r1)
#define jit_ldxr_f(f0, r0, r1)		a64_ldstr(_jitp, A64_LDRS, _A64_F(f0), r0, r1)
#define jit_ldr_d(f0, r0)		jit_ldxi_d(f0, r0, 0)
#define jit_ldr_f(f0, r0)		jit_ldxi_f(f0, r0, 0)
#define jit_ldi_d(f0, i0)		a64_ldsti(_jitp, A64_LDRD, 3, _A64_F(f0), (void *)(i0))
#define jit_ldi_f(f0, i0)		a64_ldsti(_jitp, A64_LDRS, 2, _A64_F(f0), (void *)(i0))

#define jit_stxi_d(i0, r0, f0)		a64_ldst(_jitp, A64_STRD, 3, _A64_F(f0), r0, _A64_DISP(i0))
#define jit_stxi_f(i0, r0, f0)		a64_ldst(_jitp, A64_STRS, 2, _A64_F(f0), r0, _A64_DISP(i0))
#define jit_stxr_d(r0, r1, f0)		a64_ldstr(_jitp, A64_STRD, _A64_F(f0), r0, r1)
#define jit_stxr_f(r0, r1, f0)		a64_ldstr(_jitp, A64_STRS, _A64_F(f0), r0, r1)
#define jit_str_d(r0, f0)		jit_stxi_d(0, r0, f0)
#define jit_str_f(r0, f0)		jit_stxi_f(0, r0, f0)
#define jit_sti_d(i0, f0)		a64_ldsti(_jitp, A64_STRD, 3, _A64_F(f0), (void *)(i0))
#define jit_sti_f(i0, f0)		a64_ldsti(_jitp, A64_STRS, 2, _A64_F(f0), (void *)(i0))

/* Branches. After FCMP, an unordered result sets NZCV to 0011, so the
   condition codes below are chosen so that the ordered comparisons
   are false for NaN, while their negations ("un" and "anti" variants)
   are true for NaN. */
__jit_inline jit_insn *
a64_bcmp_d(jit_state_t _jitp, int cc, jit_insn *i0, jit_fpr_t f0, jit_fpr_t f1)
{
  A64_FCMPR(f0, f1);
  return a64_branch(_jitp, cc, i0);
}

#define jit_bltr_d(i0, f0, f1)		a64_bcmp_d(_jitp, A64_CC_MI, i0, f0, f1)
#define jit_bler_d(i0, f0, f1)		a64_bcmp_d(_jitp, A64_CC_LS, i0, f0, f1)
#define jit_beqr_d(i0, f0, f1)		a64_bcmp_d(_jitp, A64_CC_EQ, i0, f0, f1)
#define jit_bger_d(i0, f0, f1)		a64_bcmp_d(_jitp, A64_CC_GE, i0, f0, f1)
#define jit_bgtr_d(i0, f0, f1)		a64_bcmp_d(_jitp, A64_CC_GT, i0, f0, f1)
#define jit_bner_d(i0, f0, f1)		a64_bcmp_d(_jitp, A64_CC_NE, i0, f0, f1)
#define jit_bunltr_d(i0, f0, f1)	a64_bcmp_d(_jitp, A64_CC_LT, i0, f0, f1)
#define jit_bunler_d(i0, f0, f1)	a64_bcmp_d(_jitp, A64_CC_LE, i0, f0, f1)
#define jit_bungtr_d(i0, f0, f1)	a64_bcmp_d(_jitp, A64_CC_HI, i0, f0, f1)
#define jit_bunger_d(i0, f0, f1)	a64_bcmp_d(_jitp, A64_CC_PL, i0, f0, f1)
#define jit_bordr_d(i0, f0, f1)		a64_bcmp_d(_jitp, A64_CC_VC, i0, f0, f1)
#define jit_bunordr_d(i0, f0, f1)	a64_bcmp_d(_jitp, A64_CC_VS, i0, f0, f1)

/* The "anti" variants branch when the comparison does not hold,
   including when an argument is NaN */
#define jit_bantigtr_d(i0, f0, f1)	jit_bunler_d(i0, f0, f1)
#define jit_bantiger_d(i0, f0, f1)	jit_bunltr_d(i0, f0, f1)
#define jit_bantiltr_d(i0, f0, f1)	jit_bunger_d(i0, f0, f1)
#define jit_bantiler_d(i0, f0, f1)	jit_bungtr_d(i0, f0, f1)
#define jit_bantieqr_d(i0, f0, f1)	jit_bner_d(i0, f0, f1)

#define jit_retval_d(f0)		jit_movr_d(f0, JIT_FPRET)

#endif /* __lightning_fp_arm64_h */
