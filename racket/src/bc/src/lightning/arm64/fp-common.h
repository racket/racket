/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer floating-point interface
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002, 2026 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
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
 ***********************************************************************/

#define JIT_FPR0			JIT_FPR(0)
#define JIT_FPR1			JIT_FPR(1)
#define JIT_FPR2			JIT_FPR(2)
#define JIT_FPR3			JIT_FPR(3)
#define JIT_FPR4			JIT_FPR(4)
#define JIT_FPR5			JIT_FPR(5)

#define jit_addr_f(rd,s1,s2)		jit_addr_d(rd,s1,s2)
#define jit_subr_f(rd,s1,s2)		jit_subr_d(rd,s1,s2)
#define jit_mulr_f(rd,s1,s2)		jit_mulr_d(rd,s1,s2)
#define jit_divr_f(rd,s1,s2)		jit_divr_d(rd,s1,s2)
#define jit_absr_f(rd,rs)		jit_absr_d(rd,rs)
#define jit_negr_f(rd,rs)		jit_negr_d(rd,rs)
#define jit_sqrtr_f(rd,rs)		jit_sqrtr_d(rd,rs)

#define jit_beqr_f(lab, a, b)		jit_beqr_d(lab, a, b)
#define jit_bner_f(lab, a, b)		jit_bner_d(lab, a, b)
#define jit_bgtr_f(lab, a, b)		jit_bgtr_d(lab, a, b)
#define jit_bger_f(lab, a, b)		jit_bger_d(lab, a, b)
#define jit_bltr_f(lab, a, b)		jit_bltr_d(lab, a, b)
#define jit_bler_f(lab, a, b)		jit_bler_d(lab, a, b)
#define jit_bungtr_f(lab, a, b)		jit_bungtr_d(lab, a, b)
#define jit_bunger_f(lab, a, b)		jit_bunger_d(lab, a, b)
#define jit_bunltr_f(lab, a, b)		jit_bunltr_d(lab, a, b)
#define jit_bunler_f(lab, a, b)		jit_bunler_d(lab, a, b)
#define jit_bordr_f(lab, a, b)		jit_bordr_d(lab, a, b)
#define jit_bunordr_f(lab, a, b)	jit_bunordr_d(lab, a, b)

#define jit_retval_f(op1)		jit_movr_f((op1), JIT_FPRET)

#define jit_prolog_f(numargs)
#define jit_prolog_d(numargs)
#define jit_leaf_f(numargs)		jit_prolog_f(numargs)
#define jit_leaf_d(numargs)		jit_prolog_d(numargs)
