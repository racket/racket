/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler & support macros for the i386 math coprocessor
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002, 2004 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 ***********************************************************************/


#ifndef __lightning_asm_fp_h
#define __lightning_asm_fp_h

#ifdef JIT_X86_SSE

# include "fp-sse.h"

# ifdef MZ_LONG_DOUBLE
#  include "fp-extfpu.h"
# endif

#else

# include "fp-extfpu.h"

# define JIT_FPR_NUM JIT_FPU_FPR_NUM
# define JIT_FPR(i)  JIT_FPU_FPR(i)

# define jit_fxch(rs, op) jit_fpu_fxch(rs, op)
# define jit_addr_d(rd,s1,s2) jit_fpu_addr_d(rd,s1,s2)
# define jit_subr_d(rd,s1,s2) jit_fpu_subr_d(rd,s1,s2)
# define jit_subrr_d(rd,s1,s2) jit_fpu_subrr_d(rd,s1,s2)
# define jit_mulr_d(rd,s1,s2) jit_fpu_mulr_d(rd,s1,s2)
# define jit_divr_d(rd,s1,s2) jit_fpu_divr_d(rd,s1,s2)
# define jit_divrr_d(rd,s1,s2) jit_fpu_divrr_d(rd,s1,s2)
# define jit_abs_d(rd,rs) jit_fpu_abs_d(rd,rs)
# define jit_negr_d(rd,rs) jit_fpu_negr_d(rd,rs)
# define jit_sqrt_d(rd,rs) jit_fpu_sqrt_d(rd,rs)
# define jit_addr_d_fppop(rd,s1,s2) jit_fpu_addr_d_fppop(rd,s1,s2)
# define jit_subr_d_fppop(rd,s1,s2) jit_fpu_subr_d_fppop(rd,s1,s2)
# define jit_subrr_d_fppop(rd,s1,s2) jit_fpu_subrr_d_fppop(rd,s1,s2)
# define jit_mulr_d_fppop(rd,s1,s2) jit_fpu_mulr_d_fppop(rd,s1,s2)
# define jit_divr_d_fppop(rd,s1,s2) jit_fpu_divr_d_fppop(rd,s1,s2)
# define jit_divrr_d_fppop(rd,s1,s2) jit_fpu_divrr_d_fppop(rd,s1,s2)
# define jit_negr_d_fppop(rd,rs) jit_fpu_negr_d_fppop(rd,rs)
# define jit_abs_d_fppop(rd,rs) jit_fpu_abs_d_fppop(rd,rs)
# define jit_sqrt_d_fppop(rd,rs) jit_fpu_sqrt_d_fppop(rd,rs)
# define jit_addr_ld(rd,s1,s2) jit_fpu_addr_ld(rd,s1,s2)
# define jit_subr_ld(rd,s1,s2) jit_fpu_subr_ld(rd,s1,s2)
# define jit_subrr_ld(rd,s1,s2) jit_fpu_subrr_ld(rd,s1,s2)
# define jit_mulr_ld(rd,s1,s2) jit_fpu_mulr_ld(rd,s1,s2)
# define jit_ldivr_ld(rd,s1,s2) jit_fpu_ldivr_ld(rd,s1,s2)
# define jit_ldivrr_ld(rd,s1,s2) jit_fpu_ldivrr_ld(rd,s1,s2)
# define jit_abs_ld(rd,rs) jit_fpu_abs_ld(rd,rs)
# define jit_negr_ld(rd,rs) jit_fpu_negr_ld(rd,rs)
# define jit_sqrt_ld(rd,rs) jit_fpu_sqrt_ld(rd,rs)
# define jit_addr_ld_fppop(rd,s1,s2) jit_fpu_addr_ld_fppop(rd,s1,s2)
# define jit_subr_ld_fppop(rd,s1,s2) jit_fpu_subr_ld_fppop(rd,s1,s2)
# define jit_subrr_ld_fppop(rd,s1,s2) jit_fpu_subrr_ld_fppop(rd,s1,s2)
# define jit_mulr_ld_fppop(rd,s1,s2) jit_fpu_mulr_ld_fppop(rd,s1,s2)
# define jit_divr_ld_fppop(rd,s1,s2) jit_fpu_divr_ld_fppop(rd,s1,s2)
# define jit_divrr_ld_fppop(rd,s1,s2) jit_fpu_divrr_ld_fppop(rd,s1,s2)
# define jit_negr_ld_fppop(rd,rs) jit_fpu_negr_ld_fppop(rd,rs)
# define jit_abs_ld_fppop(rd,rs) jit_fpu_abs_ld_fppop(rd,rs)
# define jit_sqrt_ld_fppop(rd,rs) jit_fpu_sqrt_ld_fppop(rd,rs)
# define jit_movr_d(rd,s1) jit_fpu_movr_d(rd,s1)
# define jit_movr_d_rel(rd,s1) jit_fpu_movr_d_rel(rd,s1)
# define jit_movr_d_fppush(rd,s1) jit_fpu_movr_d_fppush(rd,s1)
# define jit_movr_ld(rd,s1) jit_fpu_movr_ld(rd,s1)
# define jit_movr_ld_rel(rd,s1) jit_fpu_movr_ld_rel(rd,s1)
# define jit_movr_ld_fppush(rd,s1) jit_fpu_movr_ld_fppush(rd,s1)
# define jit_movi_d(rd,immd) jit_fpu_movi_d(rd,immd)
# define jit_ldi_d(rd, is) jit_fpu_ldi_d(rd, is)
# define jit_ldi_d_fppush(rd, is) jit_fpu_ldi_d_fppush(rd, is)
# define jit_ldi_ld(rd, is) jit_fpu_ldi_ld(rd, is)
# define jit_ldi_ld_fppush(rd, is) jit_fpu_ldi_ld_fppush(rd, is)
# define jit_ldr_d(rd, rs) jit_fpu_ldr_d(rd, rs)
# define jit_ldr_d_fppush(rd, rs) jit_fpu_ldr_d_fppush(rd, rs)
# define jit_ldr_f_fppush(rd, rs) jit_fpu_ldr_f_fppush(rd, rs)
# define jit_ldr_ld(rd, rs) jit_fpu_ldr_ld(rd, rs)
# define jit_ldr_ld_fppush(rd, rs) jit_fpu_ldr_ld_fppush(rd, rs)
# define jit_ldxi_d(rd, rs, is) jit_fpu_ldxi_d(rd, rs, is)
# define jit_ldxi_d_fppush(rd, rs, is) jit_fpu_ldxi_d_fppush(rd, rs, is)
# define jit_ldxi_ld(rd, rs, is) jit_fpu_ldxi_ld(rd, rs, is)
# define jit_ldxi_ld_fppush(rd, rs, is) jit_fpu_ldxi_ld_fppush(rd, rs, is)
# define jit_ldxr_d(rd, s1, s2) jit_fpu_ldxr_d(rd, s1, s2)
# define jit_ldxr_d_fppush(rd, s1, s2) jit_fpu_ldxr_d_fppush(rd, s1, s2)
# define jit_ldxr_ld(rd, s1, s2) jit_fpu_ldxr_ld(rd, s1, s2)
# define jit_ldxr_ld_fppush(rd, s1, s2) jit_fpu_ldxr_ld_fppush(rd, s1, s2)
# define jit_extr_i_d(rd, rs) jit_fpu_extr_i_d(rd, rs)
# define jit_extr_i_d_fppush(rd, rs) jit_fpu_extr_i_d_fppush(rd, rs)
# define jit_extr_i_ld_fppush(rd, rs) jit_fpu_extr_i_ld_fppush(rd, rs)
# define jit_extr_l_d_fppush(rd, rs) jit_fpu_extr_l_d_fppush(rd, rs)
# define jit_extr_l_ld_fppush(rd, rs) jit_fpu_extr_l_ld_fppush(rd, rs)
# define jit_extr_d_f(rd, rs) jit_fpu_extr_d_f(rd, rs)
# define jit_extr_f_d(rd, rs) jit_fpu_extr_f_d(rd, rs)
# define jit_stxi_f(id, rd, rs) jit_fpu_stxi_f(id, rd, rs)
# define jit_stxr_f(d1, d2, rs) jit_fpu_stxr_f(d1, d2, rs)
# define jit_stxi_d(id, rd, rs) jit_fpu_stxi_d(id, rd, rs)
# define jit_stxr_d(d1, d2, rs) jit_fpu_stxr_d(d1, d2, rs)
# define jit_sti_d(id, rs) jit_fpu_sti_d(id, rs)
# define jit_str_d(rd, rs) jit_fpu_str_d(rd, rs)
# define jit_str_f(rd, rs) jit_fpu_str_f(rd, rs)
# define jit_sti_d_fppop(id, rs) jit_fpu_sti_d_fppop(id, rs)
# define jit_sti_ld_fppop(id, rs) jit_fpu_sti_ld_fppop(id, rs)
# define jit_stxi_d_fppop(id, rd, rs) jit_fpu_stxi_d_fppop(id, rd, rs)
# define jit_str_d_fppop(rd, rs) jit_fpu_str_d_fppop(rd, rs)
# define jit_str_f_fppop(rd, rs) jit_fpu_str_f_fppop(rd, rs)
# define jit_stxr_d_fppop(d1, d2, rs) jit_fpu_stxr_d_fppop(d1, d2, rs)
# define jit_stxi_ld_fppop(id, rd, rs) jit_fpu_stxi_ld_fppop(id, rd, rs)
# define jit_str_ld_fppop(rd, rs) jit_fpu_str_ld_fppop(rd, rs)
# define jit_stxr_ld_fppop(d1, d2, rs) jit_fpu_stxr_ld_fppop(d1, d2, rs)
# define jit_floorr_d_i(rd, rs) jit_fpu_floorr_d_i(rd, rs)
# define jit_ceilr_d_i(rd, rs) jit_fpu_ceilr_d_i(rd, rs)
# define jit_truncr_d_i(rd, rs) jit_fpu_truncr_d_i(rd, rs)
# define jit_roundr_d_i(rd, rs) jit_fpu_roundr_d_i(rd, rs)
# define jit_roundr_ld_i(rd, rs) jit_fpu_roundr_ld_i(rd, rs)
# define jit_roundr_d_l(rd, rs) jit_fpu_roundr_d_l(rd, rs)
# define jit_roundr_ld_l(rd, rs) jit_fpu_roundr_ld_l(rd, rs)
# define jit_roundr_d_l_fppop(rd, rs) jit_fpu_roundr_d_l_fppop(rd, rs)
# define jit_roundr_ld_l_fppop(rd, rs) jit_fpu_roundr_ld_l_fppop(rd, rs)
# define jit_gtr_d(d, s1, s2) jit_fpu_gtr_d(d, s1, s2)
# define jit_ger_d(d, s1, s2) jit_fpu_ger_d(d, s1, s2)
# define jit_unler_d(d, s1, s2) jit_fpu_unler_d(d, s1, s2)
# define jit_unltr_d(d, s1, s2) jit_fpu_unltr_d(d, s1, s2)
# define jit_ltr_d(d, s1, s2) jit_fpu_ltr_d(d, s1, s2)
# define jit_ler_d(d, s1, s2) jit_fpu_ler_d(d, s1, s2)
# define jit_unger_d(d, s1, s2) jit_fpu_unger_d(d, s1, s2)
# define jit_ungtr_d(d, s1, s2) jit_fpu_ungtr_d(d, s1, s2)
# define jit_eqr_d(d, s1, s2) jit_fpu_eqr_d(d, s1, s2)
# define jit_ner_d(d, s1, s2) jit_fpu_ner_d(d, s1, s2)
# define jit_ltgtr_d(d, s1, s2) jit_fpu_ltgtr_d(d, s1, s2)
# define jit_uneqr_d(d, s1, s2) jit_fpu_uneqr_d(d, s1, s2)
# define jit_ordr_d(d, s1, s2) jit_fpu_ordr_d(d, s1, s2)
# define jit_unordr_d(d, s1, s2) jit_fpu_unordr_d(d, s1, s2)
# define jit_bgtr_d(d, s1, s2) jit_fpu_bgtr_d(d, s1, s2)
# define jit_bger_d(d, s1, s2) jit_fpu_bger_d(d, s1, s2)
# define jit_bger_ld(d, s1, s2) jit_fpu_bger_ld(d, s1, s2)
# define jit_bantigtr_d(d, s1, s2) jit_fpu_bantigtr_d(d, s1, s2)
# define jit_bantiger_d(d, s1, s2) jit_fpu_bantiger_d(d, s1, s2)
# define jit_bunler_d(d, s1, s2) jit_fpu_bunler_d(d, s1, s2)
# define jit_bunltr_d(d, s1, s2) jit_fpu_bunltr_d(d, s1, s2)
# define jit_bltr_d(d, s1, s2) jit_fpu_bltr_d(d, s1, s2)
# define jit_bltr_ld(d, s1, s2) jit_fpu_bltr_ld(d, s1, s2)
# define jit_bler_d(d, s1, s2) jit_fpu_bler_d(d, s1, s2)
# define jit_bantiltr_d(d, s1, s2) jit_fpu_bantiltr_d(d, s1, s2)
# define jit_bantiler_d(d, s1, s2) jit_fpu_bantiler_d(d, s1, s2)
# define jit_bunger_d(d, s1, s2) jit_fpu_bunger_d(d, s1, s2)
# define jit_bungtr_d(d, s1, s2) jit_fpu_bungtr_d(d, s1, s2)
# define jit_beqr_d(d, s1, s2) jit_fpu_beqr_d(d, s1, s2)
# define jit_beqr_ld(d, s1, s2) jit_fpu_beqr_ld(d, s1, s2)
# define jit_bantieqr_d(d, s1, s2) jit_fpu_bantieqr_d(d, s1, s2)
# define jit_bantieqr_ld(d, s1, s2) jit_fpu_bantieqr_ld(d, s1, s2)
# define jit_bner_d(d, s1, s2) jit_fpu_bner_d(d, s1, s2)
# define jit_bltgtr_d(d, s1, s2) jit_fpu_bltgtr_d(d, s1, s2)
# define jit_buneqr_d(d, s1, s2) jit_fpu_buneqr_d(d, s1, s2)
# define jit_bordr_d(d, s1, s2) jit_fpu_bordr_d(d, s1, s2)
# define jit_bunordr_d(d, s1, s2) jit_fpu_bunordr_d(d, s1, s2)
# define jit_bger_d_fppop(d, s1, s2) jit_fpu_bger_d_fppop(d, s1, s2)
# define jit_bger_ld_fppop(d, s1, s2) jit_fpu_bger_ld_fppop(d, s1, s2)
# define jit_bantiger_d_fppop(d, s1, s2) jit_fpu_bantiger_d_fppop(d, s1, s2)
# define jit_bantiger_ld_fppop(d, s1, s2) jit_fpu_bantiger_ld_fppop(d, s1, s2)
# define jit_bler_d_fppop(d, s1, s2) jit_fpu_bler_d_fppop(d, s1, s2)
# define jit_bler_ld_fppop(d, s1, s2) jit_fpu_bler_ld_fppop(d, s1, s2)
# define jit_bantiler_d_fppop(d, s1, s2) jit_fpu_bantiler_d_fppop(d, s1, s2)
# define jit_bantiler_ld_fppop(d, s1, s2) jit_fpu_bantiler_ld_fppop(d, s1, s2)
# define jit_bgtr_d_fppop(d, s1, s2) jit_fpu_bgtr_d_fppop(d, s1, s2)
# define jit_bgtr_ld_fppop(d, s1, s2) jit_fpu_bgtr_ld_fppop(d, s1, s2)
# define jit_bantigtr_d_fppop(d, s1, s2) jit_fpu_bantigtr_d_fppop(d, s1, s2)
# define jit_bantigtr_ld_fppop(d, s1, s2) jit_fpu_bantigtr_ld_fppop(d, s1, s2)
# define jit_bltr_d_fppop(d, s1, s2) jit_fpu_bltr_d_fppop(d, s1, s2)
# define jit_bltr_ld_fppop(d, s1, s2) jit_fpu_bltr_ld_fppop(d, s1, s2)
# define jit_bantiltr_d_fppop(d, s1, s2) jit_fpu_bantiltr_d_fppop(d, s1, s2)
# define jit_bantiltr_ld_fppop(d, s1, s2) jit_fpu_bantiltr_ld_fppop(d, s1, s2)
# define jit_beqr_d_fppop(d, s1, s2) jit_fpu_beqr_d_fppop(d, s1, s2)
# define jit_beqr_ld_fppop(d, s1, s2) jit_fpu_beqr_ld_fppop(d, s1, s2)
# define jit_bantieqr_d_fppop(d, s1, s2) jit_fpu_bantieqr_d_fppop(d, s1, s2)
# define jit_bantieqr_ld_fppop(d, s1, s2) jit_fpu_bantieqr_ld_fppop(d, s1, s2)

#endif

#endif /* __lightning_asm_h */
