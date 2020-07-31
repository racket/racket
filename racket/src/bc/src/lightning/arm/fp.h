/******************************** -*- C -*- ****************************
 *
 *	Floating-point support (arm)
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 2011 Free Software Foundation, Inc.
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

#ifndef __lightning_fp_arm_h
#define __lightning_fp_arm_h

#define JIT_FPR_NUM			6

#define JIT_FPR(n) (_D0+((n)<<1))

#include "fp-swf.h"
#include "fp-vfp.h"

#define jit_movr_f(r0, r1)		arm_movr_f(_jitp, r0, r1)
__jit_inline void
arm_movr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    if (jit_swf_p())	swf_movr_f(_jitp, r0, r1);
    else		vfp_movr_f(_jitp, r0, r1);
}

#define jit_movr_d(r0, r1)		arm_movr_d(_jitp, r0, r1)
__jit_inline void
arm_movr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    if (jit_swf_p())	swf_movr_d(_jitp, r0, r1);
    else		vfp_movr_d(_jitp, r0, r1);
}

#define jit_movi_f(r0, i0)		arm_movi_f(_jitp, r0, i0)
__jit_inline void
arm_movi_f(jit_state_t _jitp, jit_fpr_t r0, float i0)
{
    if (jit_swf_p())	swf_movi_f(_jitp, r0, i0);
    else		vfp_movi_f(_jitp, r0, i0);
}

#define jit_movi_d(r0, i0)		arm_movi_d(_jitp, r0, i0)
__jit_inline void
arm_movi_d(jit_state_t _jitp, jit_fpr_t r0, double i0)
{
    if (jit_swf_p())	swf_movi_d(_jitp, r0, i0);
    else		vfp_movi_d(_jitp, r0, i0);
}

#define jit_extr_i_f(r0, r1)		arm_extr_i_f(_jitp, r0, r1)
__jit_inline void
arm_extr_i_f(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_extr_i_f(_jitp, r0, r1);
    else		vfp_extr_i_f(_jitp, r0, r1);
}

#define jit_extr_i_d(r0, r1)		arm_extr_i_d(_jitp, r0, r1)
__jit_inline void
arm_extr_i_d(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_extr_i_d(_jitp, r0, r1);
    else		vfp_extr_i_d(_jitp, r0, r1);
}

#define jit_extr_d_f(r0, r1)		arm_extr_d_f(_jitp, r0, r1)
__jit_inline void
arm_extr_d_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	swf_extr_d_f(_jitp, r0, r1);
    else		vfp_extr_d_f(_jitp, r0, r1);
}

#define jit_extr_f_d(r0, r1)		arm_extr_f_d(_jitp, r0, r1)
__jit_inline void
arm_extr_f_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    if (jit_swf_p())	swf_extr_f_d(_jitp, r0, r1);
    else		vfp_extr_f_d(_jitp, r0, r1);
}

#define jit_rintr_f_i(r0, r1)		arm_rintr_f_i(_jitp, r0, r1)
__jit_inline void
arm_rintr_f_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_rintr_f_i(_jitp, r0, r1);
    else		vfp_rintr_f_i(_jitp, r0, r1);
}

#define jit_rintr_d_i(r0, r1)		arm_rintr_d_i(_jitp, r0, r1)
__jit_inline void
arm_rintr_d_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_rintr_d_i(_jitp, r0, r1);
    else		vfp_rintr_d_i(_jitp, r0, r1);
}

#define jit_roundr_f_i(r0, r1)		arm_roundr_f_i(_jitp, r0, r1)
__jit_inline void
arm_roundr_f_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_roundr_f_i(_jitp, r0, r1);
    else		vfp_roundr_f_i(_jitp, r0, r1);
}

#define jit_roundr_d_i(r0, r1)		arm_roundr_d_i(_jitp, r0, r1)
__jit_inline void
arm_roundr_d_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_roundr_d_i(_jitp, r0, r1);
    else		vfp_roundr_d_i(_jitp, r0, r1);
}

#define jit_truncr_f_i(r0, r1)		arm_truncr_f_i(_jitp, r0, r1)
__jit_inline void
arm_truncr_f_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_truncr_f_i(_jitp, r0, r1);
    else		vfp_truncr_f_i(_jitp, r0, r1);
}

#define jit_truncr_d_i(r0, r1)		arm_truncr_d_i(_jitp, r0, r1)
__jit_inline void
arm_truncr_d_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_truncr_d_i(_jitp, r0, r1);
    else		vfp_truncr_d_i(_jitp, r0, r1);
}

#define jit_ceilr_f_i(r0, r1)		arm_ceilr_f_i(_jitp, r0, r1)
__jit_inline void
arm_ceilr_f_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_ceilr_f_i(_jitp, r0, r1);
    else		vfp_ceilr_f_i(_jitp, r0, r1);
}

#define jit_ceilr_d_i(r0, r1)		arm_ceilr_d_i(_jitp, r0, r1)
__jit_inline void
arm_ceilr_d_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_ceilr_d_i(_jitp, r0, r1);
    else		vfp_ceilr_d_i(_jitp, r0, r1);
}

#define jit_floorr_f_i(r0, r1)		arm_floorr_f_i(_jitp, r0, r1)
__jit_inline void
arm_floorr_f_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_floorr_f_i(_jitp, r0, r1);
    else		vfp_floorr_f_i(_jitp, r0, r1);
}

#define jit_floorr_d_i(r0, r1)		arm_floorr_d_i(_jitp, r0, r1)
__jit_inline void
arm_floorr_d_i(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_floorr_d_i(_jitp, r0, r1);
    else		vfp_floorr_d_i(_jitp, r0, r1);
}

#define jit_absr_f(r0, r1)		arm_absr_f(_jitp, r0, r1)
__jit_inline void
arm_absr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	swf_absr_f(_jitp, r0, r1);
    else		vfp_absr_f(_jitp, r0, r1);
}

#define jit_absr_d(r0, r1)		arm_absr_d(_jitp, r0, r1)
__jit_inline void
arm_absr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	swf_absr_d(_jitp, r0, r1);
    else		vfp_absr_d(_jitp, r0, r1);
}
#define jit_abs_d(r0, r1) jit_absr_d(r0, r1)

#define jit_negr_f(r0, r1)		arm_negr_f(_jitp, r0, r1)
__jit_inline void
arm_negr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	swf_negr_f(_jitp, r0, r1);
    else		vfp_negr_f(_jitp, r0, r1);
}

#define jit_negr_d(r0, r1)		arm_negr_d(_jitp, r0, r1)
__jit_inline void
arm_negr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	swf_negr_d(_jitp, r0, r1);
    else		vfp_negr_d(_jitp, r0, r1);
}

#define jit_sqrtr_f(r0, r1)		arm_sqrtr_f(_jitp, r0, r1)
__jit_inline void
arm_sqrtr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	swf_sqrtr_f(_jitp, r0, r1);
    else		vfp_sqrtr_f(_jitp, r0, r1);
}

#define jit_sqrtr_d(r0, r1)		arm_sqrtr_d(_jitp, r0, r1)
__jit_inline void
arm_sqrtr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	swf_sqrtr_d(_jitp, r0, r1);
    else		vfp_sqrtr_d(_jitp, r0, r1);
}
#define jit_sqrt_d(r0, r1) jit_sqrtr_d(r0, r1)

#define jit_addr_f(r0, r1, r2)		arm_addr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_addr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_addr_f(_jitp, r0, r1, r2);
    else		vfp_addr_f(_jitp, r0, r1, r2);
}

#define jit_addr_d(r0, r1, r2)		arm_addr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_addr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_addr_d(_jitp, r0, r1, r2);
    else		vfp_addr_d(_jitp, r0, r1, r2);
}

#define jit_subr_f(r0, r1, r2)		arm_subr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_subr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_subr_f(_jitp, r0, r1, r2);
    else		vfp_subr_f(_jitp, r0, r1, r2);
}

#define jit_subr_d(r0, r1, r2)		arm_subr_d(_jitp, r0, r1, r2)
#define jit_subrr_d(r0, r1, r2)		arm_subr_d(_jitp, r0, r2, r1)
__jit_inline void
arm_subr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_subr_d(_jitp, r0, r1, r2);
    else		vfp_subr_d(_jitp, r0, r1, r2);
}

#define jit_mulr_f(r0, r1, r2)		arm_mulr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_mulr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_mulr_f(_jitp, r0, r1, r2);
    else		vfp_mulr_f(_jitp, r0, r1, r2);
}

#define jit_mulr_d(r0, r1, r2)		arm_mulr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_mulr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_mulr_d(_jitp, r0, r1, r2);
    else		vfp_mulr_d(_jitp, r0, r1, r2);
}

#define jit_divr_f(r0, r1, r2)		arm_divr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_divr_f(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_divr_f(_jitp, r0, r1, r2);
    else		vfp_divr_f(_jitp, r0, r1, r2);
}

#define jit_divr_d(r0, r1, r2)		arm_divr_d(_jitp, r0, r1, r2)
#define jit_divrr_d(r0, r1, r2)		arm_divr_d(_jitp, r0, r2, r1)
__jit_inline void
arm_divr_d(jit_state_t _jitp, jit_fpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_divr_d(_jitp, r0, r1, r2);
    else		vfp_divr_d(_jitp, r0, r1, r2);
}

#define jit_ltr_f(r0, r1, r2)		arm_ltr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ltr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ltr_f(_jitp, r0, r1, r2);
    else		vfp_ltr_f(_jitp, r0, r1, r2);
}

#define jit_ltr_d(r0, r1, r2)		arm_ltr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ltr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ltr_d(_jitp, r0, r1, r2);
    else		vfp_ltr_d(_jitp, r0, r1, r2);
}

#define jit_ler_f(r0, r1, r2)		arm_ler_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ler_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ler_f(_jitp, r0, r1, r2);
    else		vfp_ler_f(_jitp, r0, r1, r2);
}

#define jit_ler_d(r0, r1, r2)		arm_ler_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ler_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ler_d(_jitp, r0, r1, r2);
    else		vfp_ler_d(_jitp, r0, r1, r2);
}

#define jit_eqr_f(r0, r1, r2)		arm_eqr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_eqr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_eqr_f(_jitp, r0, r1, r2);
    else		vfp_eqr_f(_jitp, r0, r1, r2);
}

#define jit_eqr_d(r0, r1, r2)		arm_eqr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_eqr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_eqr_d(_jitp, r0, r1, r2);
    else		vfp_eqr_d(_jitp, r0, r1, r2);
}

#define jit_ger_f(r0, r1, r2)		arm_ger_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ger_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ger_f(_jitp, r0, r1, r2);
    else		vfp_ger_f(_jitp, r0, r1, r2);
}

#define jit_ger_d(r0, r1, r2)		arm_ger_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ger_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ger_d(_jitp, r0, r1, r2);
    else		vfp_ger_d(_jitp, r0, r1, r2);
}

#define jit_gtr_f(r0, r1, r2)		arm_gtr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_gtr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_gtr_f(_jitp, r0, r1, r2);
    else		vfp_gtr_f(_jitp, r0, r1, r2);
}

#define jit_gtr_d(r0, r1, r2)		arm_gtr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_gtr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_gtr_d(_jitp, r0, r1, r2);
    else		vfp_gtr_d(_jitp, r0, r1, r2);
}

#define jit_ner_f(r0, r1, r2)		arm_ner_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ner_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ner_f(_jitp, r0, r1, r2);
    else		vfp_ner_f(_jitp, r0, r1, r2);
}

#define jit_ner_d(r0, r1, r2)		arm_ner_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ner_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ner_d(_jitp, r0, r1, r2);
    else		vfp_ner_d(_jitp, r0, r1, r2);
}

#define jit_unltr_f(r0, r1, r2)		arm_unltr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_unltr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unltr_f(_jitp, r0, r1, r2);
    else		vfp_unltr_f(_jitp, r0, r1, r2);
}

#define jit_unltr_d(r0, r1, r2)		arm_unltr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_unltr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unltr_d(_jitp, r0, r1, r2);
    else		vfp_unltr_d(_jitp, r0, r1, r2);
}

#define jit_unler_f(r0, r1, r2)		arm_unler_f(_jitp, r0, r1, r2)
__jit_inline void
arm_unler_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unler_f(_jitp, r0, r1, r2);
    else		vfp_unler_f(_jitp, r0, r1, r2);
}

#define jit_unler_d(r0, r1, r2)		arm_unler_d(_jitp, r0, r1, r2)
__jit_inline void
arm_unler_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unler_d(_jitp, r0, r1, r2);
    else		vfp_unler_d(_jitp, r0, r1, r2);
}

#define jit_uneqr_f(r0, r1, r2)		arm_uneqr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_uneqr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_uneqr_f(_jitp, r0, r1, r2);
    else		vfp_uneqr_f(_jitp, r0, r1, r2);
}

#define jit_uneqr_d(r0, r1, r2)		arm_uneqr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_uneqr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_uneqr_d(_jitp, r0, r1, r2);
    else		vfp_uneqr_d(_jitp, r0, r1, r2);
}

#define jit_unger_f(r0, r1, r2)		arm_unger_f(_jitp, r0, r1, r2)
__jit_inline void
arm_unger_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unger_f(_jitp, r0, r1, r2);
    else		vfp_unger_f(_jitp, r0, r1, r2);
}

#define jit_unger_d(r0, r1, r2)		arm_unger_d(_jitp, r0, r1, r2)
__jit_inline void
arm_unger_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unger_d(_jitp, r0, r1, r2);
    else		vfp_unger_d(_jitp, r0, r1, r2);
}

#define jit_ungtr_f(r0, r1, r2)		arm_ungtr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ungtr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ungtr_f(_jitp, r0, r1, r2);
    else		vfp_ungtr_f(_jitp, r0, r1, r2);
}

#define jit_ungtr_d(r0, r1, r2)		arm_ungtr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ungtr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ungtr_d(_jitp, r0, r1, r2);
    else		vfp_ungtr_d(_jitp, r0, r1, r2);
}

#define jit_ltgtr_f(r0, r1, r2)		arm_ltgtr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ltgtr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ltgtr_f(_jitp, r0, r1, r2);
    else		vfp_ltgtr_f(_jitp, r0, r1, r2);
}

#define jit_ltgtr_d(r0, r1, r2)		arm_ltgtr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ltgtr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ltgtr_d(_jitp, r0, r1, r2);
    else		vfp_ltgtr_d(_jitp, r0, r1, r2);
}

#define jit_ordr_f(r0, r1, r2)		arm_ordr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ordr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ordr_f(_jitp, r0, r1, r2);
    else		vfp_ordr_f(_jitp, r0, r1, r2);
}

#define jit_ordr_d(r0, r1, r2)		arm_ordr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ordr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_ordr_d(_jitp, r0, r1, r2);
    else		vfp_ordr_d(_jitp, r0, r1, r2);
}

#define jit_unordr_f(r0, r1, r2)	arm_unordr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_unordr_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unordr_f(_jitp, r0, r1, r2);
    else		vfp_unordr_f(_jitp, r0, r1, r2);
}

#define jit_unordr_d(r0, r1, r2)	arm_unordr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_unordr_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1, jit_fpr_t r2)
{
    assert(r1 != JIT_FPRET && r2 != JIT_FPRET);
    if (jit_swf_p())	swf_unordr_d(_jitp, r0, r1, r2);
    else		vfp_unordr_d(_jitp, r0, r1, r2);
}

#define jit_bltr_f(i0, r0, r1)		arm_bltr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bltr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bltr_f(_jitp, i0, r0, r1));
    return (vfp_bltr_f(_jitp, i0, r0, r1));
}

#define jit_bltr_d(i0, r0, r1)		arm_bltr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bltr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bltr_d(_jitp, i0, r0, r1));
    return (vfp_bltr_d(_jitp, i0, r0, r1));
}

#define jit_bler_f(i0, r0, r1)		arm_bler_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bler_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bler_f(_jitp, i0, r0, r1));
    return (vfp_bler_f(_jitp, i0, r0, r1));
}

#define jit_bler_d(i0, r0, r1)		arm_bler_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bler_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bler_d(_jitp, i0, r0, r1));
    return (vfp_bler_d(_jitp, i0, r0, r1));
}

#define jit_beqr_f(i0, r0, r1)		arm_beqr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_beqr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_beqr_f(_jitp, i0, r0, r1));
    return (vfp_beqr_f(_jitp, i0, r0, r1));
}

#define jit_beqr_d(i0, r0, r1)		arm_beqr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_beqr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_beqr_d(_jitp, i0, r0, r1));
    return (vfp_beqr_d(_jitp, i0, r0, r1));
}

#define jit_bger_f(i0, r0, r1)		arm_bger_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bger_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bger_f(_jitp, i0, r0, r1));
    return (vfp_bger_f(_jitp, i0, r0, r1));
}

#define jit_bger_d(i0, r0, r1)		arm_bger_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bger_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bger_d(_jitp, i0, r0, r1));
    return (vfp_bger_d(_jitp, i0, r0, r1));
}

#define jit_bgtr_f(i0, r0, r1)		arm_bgtr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bgtr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bgtr_f(_jitp, i0, r0, r1));
    return (vfp_bgtr_f(_jitp, i0, r0, r1));
}

#define jit_bgtr_d(i0, r0, r1)		arm_bgtr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bgtr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bgtr_d(_jitp, i0, r0, r1));
    return (vfp_bgtr_d(_jitp, i0, r0, r1));
}

#define jit_bner_f(i0, r0, r1)		arm_bner_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bner_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bner_f(_jitp, i0, r0, r1));
    return (vfp_bner_f(_jitp, i0, r0, r1));
}

#define jit_bner_d(i0, r0, r1)		arm_bner_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bner_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bner_d(_jitp, i0, r0, r1));
    return (vfp_bner_d(_jitp, i0, r0, r1));
}

#define jit_bunltr_f(i0, r0, r1)	arm_bunltr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunltr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunltr_f(_jitp, i0, r0, r1));
    return (vfp_bunltr_f(_jitp, i0, r0, r1));
}

#define jit_bunltr_d(i0, r0, r1)	arm_bunltr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunltr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunltr_d(_jitp, i0, r0, r1));
    return (vfp_bunltr_d(_jitp, i0, r0, r1));
}

#define jit_bunler_f(i0, r0, r1)	arm_bunler_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunler_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunler_f(_jitp, i0, r0, r1));
    return (vfp_bunler_f(_jitp, i0, r0, r1));
}

#define jit_bunler_d(i0, r0, r1)	arm_bunler_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunler_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunler_d(_jitp, i0, r0, r1));
    return (vfp_bunler_d(_jitp, i0, r0, r1));
}

#define jit_buneqr_f(i0, r0, r1)	arm_buneqr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_buneqr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_buneqr_f(_jitp, i0, r0, r1));
    return (vfp_buneqr_f(_jitp, i0, r0, r1));
}

#define jit_buneqr_d(i0, r0, r1)	arm_buneqr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_buneqr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_buneqr_d(_jitp, i0, r0, r1));
    return (vfp_buneqr_d(_jitp, i0, r0, r1));
}

#define jit_bunger_f(i0, r0, r1)	arm_bunger_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunger_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunger_f(_jitp, i0, r0, r1));
    return (vfp_bunger_f(_jitp, i0, r0, r1));
}

#define jit_bunger_d(i0, r0, r1)	arm_bunger_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunger_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunger_d(_jitp, i0, r0, r1));
    return (vfp_bunger_d(_jitp, i0, r0, r1));
}

#define jit_bungtr_f(i0, r0, r1)	arm_bungtr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bungtr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bungtr_f(_jitp, i0, r0, r1));
    return (vfp_bungtr_f(_jitp, i0, r0, r1));
}

#define jit_bungtr_d(i0, r0, r1)	arm_bungtr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bungtr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bungtr_d(_jitp, i0, r0, r1));
    return (vfp_bungtr_d(_jitp, i0, r0, r1));
}

#define jit_bltgtr_f(i0, r0, r1)	arm_bltgtr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bltgtr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bltgtr_f(_jitp, i0, r0, r1));
    return (vfp_bltgtr_f(_jitp, i0, r0, r1));
}

#define jit_bltgtr_d(i0, r0, r1)	arm_bltgtr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bltgtr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bltgtr_d(_jitp, i0, r0, r1));
    return (vfp_bltgtr_d(_jitp, i0, r0, r1));
}

#define jit_bordr_f(i0, r0, r1)		arm_bordr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bordr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bordr_f(_jitp, i0, r0, r1));
    return (vfp_bordr_f(_jitp, i0, r0, r1));
}

#define jit_bordr_d(i0, r0, r1)		arm_bordr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bordr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bordr_d(_jitp, i0, r0, r1));
    return (vfp_bordr_d(_jitp, i0, r0, r1));
}

#define jit_bunordr_f(i0, r0, r1)	arm_bunordr_f(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunordr_f(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunordr_f(_jitp, i0, r0, r1));
    return (vfp_bunordr_f(_jitp, i0, r0, r1));
}

#define jit_bunordr_d(i0, r0, r1)	arm_bunordr_d(_jitp, i0, r0, r1)
__jit_inline jit_insn *
arm_bunordr_d(jit_state_t _jitp, jit_insn *i0, jit_fpr_t r0, jit_fpr_t r1)
{
    assert(r0 != JIT_FPRET && r1 != JIT_FPRET);
    if (jit_swf_p())	return (swf_bunordr_d(_jitp, i0, r0, r1));
    return (vfp_bunordr_d(_jitp, i0, r0, r1));
}

#define jit_ldr_f(r0, r1)		arm_ldr_f(_jitp, r0, r1)
__jit_inline void
arm_ldr_f(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldr_f(_jitp, r0, r1);
    else		vfp_ldr_f(_jitp, r0, r1);
}

#define jit_ldr_d(r0, r1)		arm_ldr_d(_jitp, r0, r1)
__jit_inline void
arm_ldr_d(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldr_d(_jitp, r0, r1);
    else		vfp_ldr_d(_jitp, r0, r1);
}

#define jit_ldi_f(r0, i0)		arm_ldi_f(_jitp, r0, i0)
__jit_inline void
arm_ldi_f(jit_state_t _jitp, jit_fpr_t r0, void *i0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldi_f(_jitp, r0, i0);
    else		vfp_ldi_f(_jitp, r0, i0);
}

#define jit_ldi_d(r0, i0)		arm_ldi_d(_jitp, r0, i0)
__jit_inline void
arm_ldi_d(jit_state_t _jitp, jit_fpr_t r0, void *i0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldi_d(_jitp, r0, i0);
    else		vfp_ldi_d(_jitp, r0, i0);
}

#define jit_ldxr_f(r0, r1, r2)		arm_ldxr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_ldxr_f(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldxr_f(_jitp, r0, r1, r2);
    else		vfp_ldxr_f(_jitp, r0, r1, r2);
}

#define jit_ldxr_d(r0, r1, r2)		arm_ldxr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_ldxr_d(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldxr_d(_jitp, r0, r1, r2);
    else		vfp_ldxr_d(_jitp, r0, r1, r2);
}

#define jit_ldxi_f(r0, r1, i0)		arm_ldxi_f(_jitp, r0, r1, i0)
__jit_inline void
arm_ldxi_f(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1, int i0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldxi_f(_jitp, r0, r1, i0);
    else		vfp_ldxi_f(_jitp, r0, r1, i0);
}

#define jit_ldxi_d(r0, r1, i0)		arm_ldxi_d(_jitp, r0, r1, (int)i0)
__jit_inline void
arm_ldxi_d(jit_state_t _jitp, jit_fpr_t r0, jit_gpr_t r1, int i0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_ldxi_d(_jitp, r0, r1, i0);
    else		vfp_ldxi_d(_jitp, r0, r1, i0);
}

#define jit_str_f(r0, r1)		arm_str_f(_jitp, r0, r1)
__jit_inline void
arm_str_f(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_str_f(_jitp, r0, r1);
    else		vfp_str_f(_jitp, r0, r1);
}

#define jit_str_d(r0, r1)		arm_str_d(_jitp, r0, r1)
__jit_inline void
arm_str_d(jit_state_t _jitp, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_str_d(_jitp, r0, r1);
    else		vfp_str_d(_jitp, r0, r1);
}

#define jit_sti_f(i0, r0)		arm_sti_f(_jitp, i0, r0)
__jit_inline void
arm_sti_f(jit_state_t _jitp, void *i0, jit_fpr_t r0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_sti_f(_jitp, i0, r0);
    else		vfp_sti_f(_jitp, i0, r0);
}

#define jit_sti_d(i0, r0)		arm_sti_d(_jitp, i0, r0)
__jit_inline void
arm_sti_d(jit_state_t _jitp, void *i0, jit_fpr_t r0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_sti_d(_jitp, i0, r0);
    else		vfp_sti_d(_jitp, i0, r0);
}

#define jit_stxr_f(r0, r1, r2)		arm_stxr_f(_jitp, r0, r1, r2)
__jit_inline void
arm_stxr_f(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_fpr_t r2)
{
    assert(r2 != JIT_FPRET);
    if (jit_swf_p())	swf_stxr_f(_jitp, r0, r1, r2);
    else		vfp_stxr_f(_jitp, r0, r1, r2);
}

#define jit_stxr_d(r0, r1, r2)		arm_stxr_d(_jitp, r0, r1, r2)
__jit_inline void
arm_stxr_d(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_fpr_t r2)
{
    assert(r2 != JIT_FPRET);
    if (jit_swf_p())	swf_stxr_d(_jitp, r0, r1, r2);
    else		vfp_stxr_d(_jitp, r0, r1, r2);
}

#define jit_stxi_f(i0, r0, r1)		arm_stxi_f(_jitp, i0, r0, r1)
__jit_inline void
arm_stxi_f(jit_state_t _jitp, int i0, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_stxi_f(_jitp, i0, r0, r1);
    else		vfp_stxi_f(_jitp, i0, r0, r1);
}

#define jit_stxi_d(i0, r0, r1)		arm_stxi_d(_jitp, (int)i0, r0, r1)
__jit_inline void
arm_stxi_d(jit_state_t _jitp, int i0, jit_gpr_t r0, jit_fpr_t r1)
{
    assert(r1 != JIT_FPRET);
    if (jit_swf_p())	swf_stxi_d(_jitp, i0, r0, r1);
    else		vfp_stxi_d(_jitp, i0, r0, r1);
}

#define jit_prolog_f(i0)		do {} while (0)
#define jit_prolog_d(i0)		do {} while (0)

#define jit_prepare_f(i0)		arm_prepare_f(_jitp, i0)
__jit_inline void
arm_prepare_f(jit_state_t _jitp, int i0)
{
  assert(0);
}

#define jit_prepare_d(i0)		arm_prepare_d(_jitp, i0)
__jit_inline int
arm_prepare_d(jit_state_t _jitp, int i0)
{
  assert(0);
  return 0;
}

#define jit_arg_f()			arm_arg_f(_jitp)
__jit_inline void
arm_arg_f(jit_state_t _jitp)
{
  assert(0);
}

#define jit_arg_d()			arm_arg_d(_jitp)
__jit_inline int
arm_arg_d(jit_state_t _jitp)
{
  assert(0);
  return 0;
}

#define jit_getarg_f(r0, i0)		arm_getarg_f(_jitp, r0, i0)
__jit_inline void
arm_getarg_f(jit_state_t _jitp, jit_fpr_t r0, int i0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_getarg_f(_jitp, r0, i0);
    else		vfp_getarg_f(_jitp, r0, i0);
}

#define jit_getarg_d(r0, i0)		arm_getarg_d(_jitp, r0, i0)
__jit_inline void
arm_getarg_d(jit_state_t _jitp, jit_fpr_t r0, int i0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_getarg_d(_jitp, r0, i0);
    else		vfp_getarg_d(_jitp, r0, i0);
}

#define jit_pusharg_f(r0)		arm_pusharg_f(_jitp, r0)
__jit_inline void
arm_pusharg_f(jit_state_t _jitp, jit_fpr_t r0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_pusharg_f(_jitp, r0);
    else		vfp_pusharg_f(_jitp, r0);
}

#define jit_pusharg_d(r0)		arm_pusharg_d(_jitp, r0)
__jit_inline void
arm_pusharg_d(jit_state_t _jitp, jit_fpr_t r0)
{
    assert(r0 != JIT_FPRET);
    if (jit_swf_p())	swf_pusharg_d(_jitp, r0);
    else		vfp_pusharg_d(_jitp, r0);
}

#define jit_retval_f(r0)		arm_retval_f(_jitp, r0)
__jit_inline void
arm_retval_f(jit_state_t _jitp, jit_fpr_t r0)
{
    if (r0 != JIT_FPRET) {
	if (jit_swf_p())	swf_retval_f(_jitp, r0);
	else			vfp_retval_f(_jitp, r0);
    }
    /* else assume chaining call to jit_retval_f as done in tests/funcfp.c */
}

#define jit_retval_d(r0)		arm_retval_d(_jitp, r0)
__jit_inline void
arm_retval_d(jit_state_t _jitp, jit_fpr_t r0)
{
    if (r0 != JIT_FPRET) {
	if (jit_swf_p())	swf_retval_d(_jitp, r0);
	else			vfp_retval_d(_jitp, r0);
    }
    /* else assume chaining call to jit_retval_d as done in tests/funcfp.c */
}

#define jit_bantigtr_d(dest, r0, r1) jit_bunler_d(dest, r0, r1)
#define jit_bantiger_d(dest, r0, r1) jit_bunltr_d(dest, r0, r1)
#define jit_bantiltr_d(dest, r0, r1) jit_bunger_d(dest, r0, r1)
#define jit_bantiler_d(dest, r0, r1) jit_bungtr_d(dest, r0, r1)
#define jit_bantieqr_d(dest, r0, r1) jit_bner_d(dest, r0, r1)

#endif /* __lightning_fp_arm_h */
