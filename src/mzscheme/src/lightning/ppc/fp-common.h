/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer floating-point interface
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
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

#define JIT_FPR0			JIT_FPR(0)
#define JIT_FPR1			JIT_FPR(1)
#define JIT_FPR2			JIT_FPR(2)
#define JIT_FPR3			JIT_FPR(3)
#define JIT_FPR4			JIT_FPR(4)
#define JIT_FPR5			JIT_FPR(5)

#ifdef JIT_RZERO
#ifndef jit_ldi_f
#define jit_ldi_f(rd, is)               jit_ldxi_f((rd), JIT_RZERO, (is))
#define jit_sti_f(id, rs)               jit_stxi_f((id), JIT_RZERO, (rs))
#define jit_ldi_d(rd, is)               jit_ldxi_d((rd), JIT_RZERO, (is))
#define jit_sti_d(id, rs)               jit_stxi_d((id), JIT_RZERO, (rs))
#endif

#ifndef jit_ldr_f
#define jit_ldr_f(rd, rs)               jit_ldxr_f((rd), JIT_RZERO, (rs))
#define jit_str_f(rd, rs)               jit_stxr_f((rd), JIT_RZERO, (rs))
#define jit_ldr_d(rd, rs)               jit_ldxr_d((rd), JIT_RZERO, (rs))
#define jit_str_d(rd, rs)               jit_stxr_d((rd), JIT_RZERO, (rs))
#endif
#endif

#define jit_extr_l_d(rd, rs)		jit_extr_i_d(rd, rs)

#ifndef jit_addr_f
#define jit_addr_f(rd,s1,s2)		jit_addr_d(rd,s1,s2)
#define jit_subr_f(rd,s1,s2)		jit_subr_d(rd,s1,s2)
#define jit_mulr_f(rd,s1,s2)		jit_mulr_d(rd,s1,s2)
#define jit_divr_f(rd,s1,s2)		jit_divr_d(rd,s1,s2)
#define jit_movr_f(rd,rs)		jit_movr_d(rd,rs)
#define jit_abs_f(rd,rs)		jit_abs_d(rd,rs)
#define jit_negr_f(rd,rs)		jit_negr_d(rd,rs)
#define jit_sqrt_f(rd,rs)		jit_sqrt_d(rd,rs)
#define jit_extr_f_d(rs, rd)
#define jit_extr_d_f(rs, rd)
#define jit_extr_i_f(rd, rs)		jit_extr_i_d(rd, rs)
#define jit_roundr_f_i(rd, rs)		jit_roundr_d_i(rd, rs)
#define jit_floorr_f_i(rd, rs)		jit_floorr_d_i(rd, rs)
#define jit_ceilr_f_i(rd, rs)		jit_ceilr_d_i(rd, rs)
#define jit_truncr_f_i(rd, rs)		jit_truncr_d_i(rd, rs)
#define jit_ltr_f(d, s1, s2)		jit_ltr_d(d, s1, s2)
#define jit_ler_f(d, s1, s2)		jit_ler_d(d, s1, s2)
#define jit_eqr_f(d, s1, s2)		jit_eqr_d(d, s1, s2)
#define jit_ner_f(d, s1, s2)		jit_ner_d(d, s1, s2)
#define jit_ger_f(d, s1, s2)		jit_ger_d(d, s1, s2)
#define jit_gtr_f(d, s1, s2)		jit_gtr_d(d, s1, s2)
#define jit_unltr_f(d, s1, s2)		jit_unltr_d(d, s1, s2)
#define jit_unler_f(d, s1, s2)		jit_unler_d(d, s1, s2)
#define jit_uneqr_f(d, s1, s2)		jit_uneqr_d(d, s1, s2)
#define jit_ltgtr_f(d, s1, s2)		jit_ltgtr_d(d, s1, s2)
#define jit_unger_f(d, s1, s2)		jit_unger_d(d, s1, s2)
#define jit_ungtr_f(d, s1, s2)		jit_ungtr_d(d, s1, s2)
#define jit_ordr_f(d, s1, s2)		jit_ordr_d(d, s1, s2)
#define jit_unordr_f(d, s1, s2)		jit_unordr_d(d, s1, s2)
#define jit_retval_f(rs)		jit_retval_d(rs)
#endif
