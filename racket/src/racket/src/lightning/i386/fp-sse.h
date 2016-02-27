/******************************** -*- C -*- ****************************
 *
 *	Support macros for SSE floating-point math
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2006,2010 Free Software Foundation, Inc.
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
 *	Paolo Bonzini
 *	Paulo Cesar Pereira de Andrade
 ***********************************************************************/


#ifndef __lightning_fp_sse_h
#define __lightning_fp_sse_h

#ifdef _WIN64
/* Win64 ABI has only 6 volatile XMM registers, and we need to
   use one register as scratch: */
# define JIT_FPR_NUM 5
#else
# define JIT_FPR_NUM 6
#endif

#define _XMM0 0x60
/* It night be better to avoid the first 8 registers for
   non-Win64 x86_64 mode, since those registers can be
   used for arguments in C function calls. Racket doesn't
   use FP arguments for C calls, though. */
#define JIT_FPR(i) (_XMM0 + (i))
#define JIT_FPTMP0 JIT_FPR(JIT_FPR_NUM)

#define jit_addr_d(f0, f1, f2)                   \
  ((f0 == f1)  \
   ? ADDSDrr(f2, f0) \
   : ((f0 == f2) \
      ? ADDSDrr(f1, f0) \
      : (MOVSDrr(f1, f0), ADDSDrr(f2, f0))))

#define jit_subr_d(f0, f1, f2)                   \
  ((f0 == f1)  \
   ? SUBSDrr(f2, f0) \
   : ((f0 == f2) \
      ? (MOVSDrr(f0, JIT_FPTMP0), MOVSDrr(f1, f0), SUBSDrr(JIT_FPTMP0, f0)) \
      : (MOVSDrr(f1, f0), SUBSDrr(f2, f0))))

#define jit_subrr_d(f0, f1, f2) jit_subr_d(f0, f2, f1)

#define jit_mulr_d(f0, f1, f2)                   \
  ((f0 == f1)  \
   ? MULSDrr(f2, f0) \
   : ((f0 == f2) \
      ? MULSDrr(f1, f0) \
      : (MOVSDrr(f1, f0), MULSDrr(f2, f0))))

#define jit_divr_d(f0, f1, f2)                   \
  ((f0 == f1)  \
   ? DIVSDrr(f2, f0) \
   : ((f0 == f2) \
      ? (MOVSDrr(f0, JIT_FPTMP0), MOVSDrr(f1, f0), DIVSDrr(JIT_FPTMP0, f0)) \
      : (MOVSDrr(f1, f0), DIVSDrr(f2, f0))))

#define jit_divrr_d(f0, f1, f2) jit_divr_d(f0, f2, f1)

#define jit_ldr_f(f0, r0) MOVSSmr(0, r0, _NOREG, _SCL1, f0)
#define jit_ldr_d(f0, r0) MOVSDmr(0, r0, _NOREG, _SCL1, f0)

#define _jit_ldi_d(f0, i0) MOVSDmr((intptr_t)(i0), _NOREG, _NOREG, _SCL1, f0)
#ifdef JIT_X86_64
# define jit_ldi_d(f0, i0)			\
  (_u32P((intptr_t)(i0))			\
   ? _jit_ldi_d(f0, i0)				\
   : (jit_movi_l(JIT_REXTMP, (intptr_t)(i0)), jit_ldr_d(f0, JIT_REXTMP)))
#else
# define jit_ldi_d(f0, i0) _jit_ldi_d(f0, i0)
#endif

#define jit_ldxr_d(f0, r0, r1) MOVSDmr(0, r0, r1, _SCL1, f0)

#define jit_ldxi_d(f0, r0, i0) MOVSDmr(i0, r0, _NOREG, _SCL1, f0)

#define jit_str_f(r0, f0) MOVSSrm(f0, 0, r0, _NOREG, _SCL1)
#define jit_str_d(r0, f0) MOVSDrm(f0, 0, r0, _NOREG, _SCL1)

#define _jit_sti_d(i0, f0) MOVSDrm(f0, (long)i0, _NOREG, _NOREG, _SCL1)
#ifdef JIT_X86_64
# define jit_sti_d(i0, f0) \
  (_u32P((intptr_t)(i0)) \
   ? _jit_sti_d((intptr_t)(i0), f0)					\
   : (jit_movi_l(JIT_REXTMP, (intptr_t)(i0)), jit_str_d(JIT_REXTMP, f0)))
#else
# define jit_sti_d(i0, f0) _jit_sti_d(i0, f0) 
#endif

#define jit_stxr_d(r0, r1, f0) MOVSDrm(f0, 0, r0, r1, _SCL1)

#define jit_stxi_d(i0, r1, f0) MOVSDrm(f0, i0, r1, _NOREG, _SCL1)

#define jit_movi_d(f0, i0) \
  (_jitl.d_data.d = i0, \
   ((_jitl.d_data.d == 0.0 && !(_jitl.d_data.i[1] & 0x80000000)) \
    ? XORPDrr(f0, f0) \
    : finish_movi_d(f0, i0)))
#ifdef JIT_X86_64
# define finish_movi_d(f0, i0) (jit_movi_l(JIT_REXTMP, _jitl.d_data.l), MOVDQXrr(JIT_REXTMP, f0))
#else
# define finish_movi_d(f0, i0) \
  (jit_pushi_i(_jitl.d_data.i[1]), jit_pushi_i(_jitl.d_data.i[0]), \
   jit_ldr_d(f0, JIT_SP),                    \
   jit_addi_l(JIT_SP, JIT_SP, sizeof(double)))
#endif

# define jit_movr_d(f0, f1) ((f0 != f1) ? MOVSDrr(f1, f0) : (void)0)
# define jit_extr_i_d(f0, r0) CVTSI2SDLrr(r0, f0)

#ifdef JIT_X86_64
# define jit_extr_l_d(f0, r0) CVTSI2SDQrr(r0, f0)
#else
# define jit_extr_l_d(f0, r0) jit_extr_i_d(f0, r0)
#endif

# define jit_extr_d_f(f0, f1) CVTSD2SSrr(f1, f0)
# define jit_extr_f_d(f0, f1) CVTSS2SDrr(f1, f0)

#define jit_abs_d(f0, f1) \
  ((f0 == f1) \
   ? (PCMPEQLrr(JIT_FPTMP0, JIT_FPTMP0), PSRLQir(1, JIT_FPTMP0), ANDPDrr(JIT_FPTMP0, f0)) \
   : (PCMPEQLrr(f0, f0), PSRLQir(1, f0), ANDPDrr(f1, f0)))

#define jit_sqrt_d(f0, f1) SQRTSDrr(f1, f0)

#ifdef JIT_X86_64
# define jit_negr_d(f0, f1)  \
  (jit_movi_l(JIT_REXTMP, 0x8000000000000000), \
   ((f0 == f1) \
    ? (MOVDQXrr(JIT_REXTMP, JIT_FPTMP0), \
       XORPDrr(JIT_FPTMP0, f0)) \
    : (MOVDQXrr(JIT_REXTMP, f0), \
       XORPDrr(f1, f0))))
#else
# define jit_negr_d(f0, f1) \
  (jit_pushi_i(0x80000000), \
   jit_pushi_i(0), \
   ((f0 == f1) \
    ? (jit_ldr_d(JIT_FPTMP0, JIT_SP), \
       XORPDrr(JIT_FPTMP0, f0)) \
    : (jit_ldr_d(f0, JIT_SP), \
       XORPDrr(f1, f0))), \
   jit_addi_l(JIT_SP, JIT_SP, sizeof(int) << 1))
#endif

/* Racket uses jit_roundr_l only for inexact->exact of fixnums,
   so a truncate is good enough. */
#define jit_roundr_d_i(r0, f0) jit_truncr_d_i(r0, f0)
#define jit_roundr_d_l(r0, f0) jit_truncr_d_l(r0, f0)

#define jit_truncr_d_i(r0, f0) CVTTSD2SILrr(f0, r0)
#ifdef JIT_X86_64
# define jit_truncr_d_l(r0, f0) CVTTSD2SIQrr(f0, r0)
#else
# define jit_truncr_d_l(r0, f0) jit_truncr_d_i(r0, f0) 
#endif

#define jit_bltr_d(label, f0, f1) (UCOMISDrr(f0, f1), JAEm(label,0,0,0), (_jit.x.pc))
#define jit_bler_d(label, f0, f1) (UCOMISDrr(f0, f1), JBEm(label,0,0,0), (_jit.x.pc))
#define jit_bgtr_d(label, f0, f1) (UCOMISDrr(f1, f0), JAm(label,0,0,0), (_jit.x.pc))
#define jit_bger_d(label, f0, f1) (UCOMISDrr(f1, f0), JAEm(label,0,0,0), (_jit.x.pc))
#define jit_beqr_d(label, f0, f1) \
   (UCOMISDrr(f0, f1),  \
    _O_D8(0x70|(0xa), 0), /*JP */ \
    _jitl.tmp_label = _jit.x.pc, \
    JEm(label,0,0,0), \
    jit_patch_tiny_at(_jitl.tmp_label, _jit.x.pc), \
    _jit.x.pc)

#define jit_bantiltr_d(label, f0, f1) (UCOMISDrr(f0, f1), JBEm(label,0,0,0), (_jit.x.pc))
#define jit_bantiler_d(label, f0, f1) (UCOMISDrr(f0, f1), JBm(label,0,0,0), (_jit.x.pc))
#define jit_bantigtr_d(label, f0, f1) (UCOMISDrr(f1, f0), JBEm(label,0,0,0), (_jit.x.pc))
#define jit_bantiger_d(label, f0, f1) (UCOMISDrr(f1, f0), JBm(label,0,0,0), (_jit.x.pc))
#define jit_bantieqr_d(label, f0, f1) \
   (UCOMISDrr(f0, f1),  \
    _O_D8(0x70|(0xb), 0), /*JNP */ \
    _jitl.tmp_label = _jit.x.pc, \
    CMPLir(0, JIT_SP), \
    jit_patch_tiny_at(_jitl.tmp_label, _jit.x.pc), \
    JNEm(label,0,0,0), \
    _jit.x.pc)

#endif /* __lightning_fp_sse_h */
