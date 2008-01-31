/******************************** -*- C -*- ****************************
 *
 *	Support macros for SSE floating-point math
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2006 Free Software Foundation, Inc.
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


#ifndef __lightning_fp_h
#define __lightning_fp_h

#include <float.h>

#define JIT_FPR_NUM	9
#define JIT_FPRET	_XMM0
#define JIT_FPR(i)	(_XMM7 + (i))
#define JIT_FPTMP	_XMM6

/* Either use a temporary register that is finally AND/OR/XORed with RS = RD,
   or use RD as the temporary register and to the AND/OR/XOR with RS.  */
#define jit_unop_tmp(rd, rs, op)		\
	( (rs) == (rd)				\
	 ? op((rd), JIT_FPTMP, JIT_FPTMP))	\
	 : op((rd), (rd), (rs)))

#define jit_unop_f(rd, rs, op)						\
	((rs) == (rd) ? op((rd)) : (MOVSSrr ((rs), (rd)), op((rd))))

#define jit_unop_d(rd, rs, op)					\
	((rs) == (rd) ? op((rd)) : (MOVSDrr ((rs), (rd)), op((rd))))

#define jit_3opc_f(rd, s1, s2, op)				\
	( (s1) == (rd) ? op((s2), (rd))				\
	  : ((s2) == (rd) ? op((s1), (rd))			\
	     : (MOVSSrr ((s1), (rd)), op((s2), (rd)))))

#define jit_3opc_d(rd, s1, s2, op)				\
	( (s1) == (rd) ? op((s2), (rd))				\
	  : ((s2) == (rd) ? op((s1), (rd))			\
	     : (MOVSDrr ((s1), (rd)), op((s2), (rd)))))

#define jit_3op_f(rd, s1, s2, op)				\
	( (s1) == (rd) ? op((s2), (rd))				\
	  : ((s2) == (rd)					\
	     ? (MOVSSrr ((rd), JIT_FPTMP), MOVSSrr ((s1), (rd)), op(JIT_FPTMP, (rd)))	\
	     : (MOVSSrr ((s1), (rd)), op((s2), (rd)))))

#define jit_3op_d(rd, s1, s2, op)				\
	( (s1) == (rd) ? op((s2), (rd))				\
	  : ((s2) == (rd) 					\
	     ? (MOVSDrr ((rd), JIT_FPTMP), MOVSDrr ((s1), (rd)), op(JIT_FPTMP, (rd)))	\
	     : (MOVSDrr ((s1), (rd)), op((s2), (rd)))))

#define jit_addr_f(rd,s1,s2)	jit_3opc_f((rd), (s1), (s2), ADDSSrr)
#define jit_subr_f(rd,s1,s2)	jit_3op_f((rd), (s1), (s2), SUBSSrr)
#define jit_mulr_f(rd,s1,s2)	jit_3opc_f((rd), (s1), (s2), MULSSrr)
#define jit_divr_f(rd,s1,s2)	jit_3op_f((rd), (s1), (s2), DIVSSrr)

#define jit_addr_d(rd,s1,s2)	jit_3opc_d((rd), (s1), (s2), ADDSDrr)
#define jit_subr_d(rd,s1,s2)	jit_3op_d((rd), (s1), (s2), SUBSDrr)
#define jit_mulr_d(rd,s1,s2)	jit_3opc_d((rd), (s1), (s2), MULSDrr)
#define jit_divr_d(rd,s1,s2)	jit_3op_d((rd), (s1), (s2), DIVSDrr)

#define jit_movr_f(rd,rs)	MOVSSrr((rs), (rd))
#define jit_movr_d(rd,rs)	MOVSDrr((rs), (rd))

/* either pcmpeqd %xmm7, %xmm7 / psrld $1, %xmm7 / andps %xmm7, %RD (if RS = RD)
       or pcmpeqd %RD, %RD / psrld $1, %RD / andps %RS, %RD (if RS != RD) */
#define _jit_abs_f(rd,cnst,rs)						\
	(PCMPEQDrr((cnst), (cnst)), PSRLDir (1, (cnst)), ANDPSrr ((rs), (rd)))
#define _jit_neg_f(rd,cnst,rs)						\
	(PCMPEQDrr((cnst), (cnst)), PSLLDir (31, (cnst)), XORPSrr ((rs), (rd)))
#define jit_abs_f(rd,rs)	jit_unop_tmp ((rd), (rs), _jit_abs_f)
#define jit_neg_f(rd,rs)	jit_unop_tmp ((rd), (rs), _jit_neg_f)

#define _jit_abs_d(rd,cnst,rs)						\
	(PCMPEQDrr((cnst), (cnst)), PSRLQir (1, (cnst)), ANDPDrr ((rs), (rd)))
#define _jit_neg_d(rd,cnst,rs)						\
	(PCMPEQDrr((cnst), (cnst)), PSLLQir (63, (cnst)), XORPDrr ((rs), (rd)))
#define jit_abs_d(rd,rs)	jit_unop_tmp ((rd), (rs), _jit_abs_d)
#define jit_neg_d(rd,rs)	jit_unop_tmp ((rd), (rs), _jit_neg_d)

#define jit_sqrt_d(rd,rs)	SQRTSSrr((rs), (rd))
#define jit_sqrt_f(rd,rs)	SQRTSDrr((rs), (rd))

#define _jit_ldi_f(d, is)               MOVSSmr((is), 0,    0,    0,  (d))
#define _jit_ldxi_f(d, rs, is)          MOVSSmr((is), (rs), 0,    0,  (d))
#define jit_ldr_f(d, rs)                MOVSSmr(0,    (rs), 0,    0,  (d))
#define jit_ldxr_f(d, s1, s2)           MOVSSmr(0,    (s1), (s2), 1,  (d))

#define _jit_sti_f(id, rs)              MOVSSrm((rs), (id), 0,    0,    0)
#define _jit_stxi_f(id, rd, rs)         MOVSSrm((rs), (id), (rd), 0,    0)
#define jit_str_f(rd, rs)               MOVSSrm((rs), 0,    (rd), 0,    0)
#define jit_stxr_f(d1, d2, rs)          MOVSSrm((rs), 0,    (d1), (d2), 1)

#define jit_ldi_f(d, is)                (_u32P((long)(is)) ? _jit_ldi_f((d), (is)) : (jit_movi_l(JIT_REXTMP, (is)), jit_ldr_f((d), JIT_REXTMP)))
#define jit_sti_f(id, rs)               (_u32P((long)(id)) ? _jit_sti_f((id), (rs)) : (jit_movi_l(JIT_REXTMP, (id)), jit_str_f (JIT_REXTMP, (rs))))
#define jit_ldxi_f(d, rs, is)           (_u32P((long)(is)) ? _jit_ldxi_f((d), (rs), (is)) : (jit_movi_l(JIT_REXTMP, (is)), jit_ldxr_f((d), (rs), JIT_REXTMP)))
#define jit_stxi_f(id, rd, rs)          (_u32P((long)(id)) ? _jit_stxi_f((id), (rd), (rs)) : (jit_movi_l(JIT_REXTMP, (id)), jit_stxr_f (JIT_REXTMP, (rd), (rs))))

#define _jit_ldi_d(d, is)               MOVSDmr((is), 0,    0,    0,  (d))
#define _jit_ldxi_d(d, rs, is)          MOVSDmr((is), (rs), 0,    0,  (d))
#define jit_ldr_d(d, rs)                MOVSDmr(0,    (rs), 0,    0,  (d))
#define jit_ldxr_d(d, s1, s2)           MOVSDmr(0,    (s1), (s2), 1,  (d))

#define _jit_sti_d(id, rs)              MOVSDrm((rs), (id), 0,    0,    0)
#define _jit_stxi_d(id, rd, rs)         MOVSDrm((rs), (id), (rd), 0,    0)
#define jit_str_d(rd, rs)               MOVSDrm((rs), 0,    (rd), 0,    0)
#define jit_stxr_d(d1, d2, rs)          MOVSDrm((rs), 0,    (d1), (d2), 1)

#define jit_ldi_d(d, is)                (_u32P((long)(is)) ? _jit_ldi_d((d), (is)) : (jit_movi_l(JIT_REXTMP, (is)), jit_ldr_d((d), JIT_REXTMP)))
#define jit_sti_d(id, rs)               (_u32P((long)(id)) ? _jit_sti_d((id), (rs)) : (jit_movi_l(JIT_REXTMP, (id)), jit_str_d (JIT_REXTMP, (rs))))
#define jit_ldxi_d(d, rs, is)           (_u32P((long)(is)) ? _jit_ldxi_d((d), (rs), (is)) : (jit_movi_l(JIT_REXTMP, (is)), jit_ldxr_d((d), (rs), JIT_REXTMP)))
#define jit_stxi_d(id, rd, rs)          (_u32P((long)(id)) ? _jit_stxi_d((id), (rd), (rs)) : (jit_movi_l(JIT_REXTMP, (id)), jit_stxr_d (JIT_REXTMP, (rd), (rs))))


#define jit_movi_f(rd,immf)                     \
 ((immf) == 0.0 ? XORSSrr ((rd), (rd)) :					      \
        (_O (0x50),                            \
         MOVLim (0x12345678L, 0, _ESP, 0, 0),				       \
         *((float *) (_jit.x.uc_pc - 4)) = (float) immf, \
        jit_ldr_f((rd), _ESP),                 \
        ADDLir(4, _ESP)))

union jit_double_imm {
  double d;
  long l;
};

#define jit_movi_d(rd,immd)                                                           \
 ((immd) == 0.0 ? XORSDrr ((rd), (rd)) :					      \
        (_O (0x50),                                                                   \
         MOVQir (0x123456789abcdef0L, _EAX),				              \
         ((union jit_double_imm *) (_jit.x.uc_pc - 8))->d = (double) immd,            \
         _O (0x50), jit_ldr_d((rd), _ESP),                                            \
         _O (0x58), _O (0x58)))

#define jit_extr_i_d(rd, rs)	CVTSI2SDLrr((rs), (rd))
#define jit_extr_i_f(rd, rs)	CVTSI2SSLrr((rs), (rd))
#define jit_extr_l_d(rd, rs)	CVTSI2SDQrr((rs), (rd))
#define jit_extr_l_f(rd, rs)	CVTSI2SSQrr((rs), (rd))
#define jit_roundr_d_i(rd, rs)	CVTSD2SILrr((rs), (rd))
#define jit_roundr_f_i(rd, rs)	CVTSS2SILrr((rs), (rd))
#define jit_roundr_d_l(rd, rs)	CVTSD2SIQrr((rs), (rd))
#define jit_roundr_f_l(rd, rs)	CVTSS2SIQrr((rs), (rd))
#define jit_truncr_d_i(rd, rs)	CVTTSD2SILrr((rs), (rd))
#define jit_truncr_f_i(rd, rs)	CVTTSS2SILrr((rs), (rd))
#define jit_truncr_d_l(rd, rs)	CVTTSD2SIQrr((rs), (rd))
#define jit_truncr_f_l(rd, rs)	CVTTSS2SIQrr((rs), (rd))


#define jit_ceilr_f_i(rd, rs) do {			\
	jit_roundr_f_i ((rd), (rs));		\
	jit_extr_i_f (JIT_FPTMP, (rd));			\
	UCOMISSrr ((rs), JIT_FPTMP);			\
	ADCLir (0, (rd));				\
  } while (0)

#define jit_ceilr_d_i(rd, rs) do {			\
	jit_roundr_d_i ((rd), (rs));		\
	jit_extr_i_d (JIT_FPTMP, (rd));			\
	UCOMISDrr ((rs), JIT_FPTMP);			\
	ADCLir (0, (rd));				\
  } while (0)

#define jit_ceilr_f_l(rd, rs) do {			\
	jit_roundr_f_l ((rd), (rs));		\
	jit_extr_l_f (JIT_FPTMP, (rd));			\
	UCOMISSrr ((rs), JIT_FPTMP);			\
	ADCLir (0, (rd));				\
  } while (0)

#define jit_ceilr_d_l(rd, rs) do {			\
	jit_roundr_d_l ((rd), (rs));		\
	jit_extr_l_d (JIT_FPTMP, (rd));			\
	UCOMISDrr ((rs), JIT_FPTMP);			\
	ADCLir (0, (rd));				\
  } while (0)

#define jit_floorr_f_i(rd, rs) do {			\
	jit_roundr_f_i ((rd), (rs));		\
	jit_extr_i_f (JIT_FPTMP, (rd));			\
	UCOMISSrr (JIT_FPTMP, (rs));			\
	SBBLir (0, (rd));				\
  } while (0)

#define jit_floorr_d_i(rd, rs) do {			\
	jit_roundr_d_i ((rd), (rs));		\
	jit_extr_i_d (JIT_FPTMP, (rd));			\
	UCOMISDrr (JIT_FPTMP, (rs));			\
	SBBLir (0, (rd));				\
  } while (0)

#define jit_floorr_f_l(rd, rs) do {			\
	jit_roundr_f_l ((rd), (rs));		\
	jit_extr_l_f (JIT_FPTMP, (rd));			\
	UCOMISSrr (JIT_FPTMP, (rs));			\
	SBBLir (0, (rd));				\
  } while (0)

#define jit_floorr_d_l(rd, rs) do {			\
	jit_roundr_d_l ((rd), (rs));		\
	jit_extr_l_d (JIT_FPTMP, (rd));			\
	UCOMISDrr (JIT_FPTMP, (rs));			\
	SBBLir (0, (rd));				\
  } while (0)

#define jit_bltr_f(d, s1, s2)            (UCOMISSrr ((s1), (s2)), JAm ((d)))
#define jit_bler_f(d, s1, s2)            (UCOMISSrr ((s1), (s2)), JAEm ((d)))
#define jit_beqr_f(d, s1, s2)            (UCOMISSrr ((s1), (s2)), _OO (0x7a06), JEm ((d)))
#define jit_bner_f(d, s1, s2)            (UCOMISSrr ((s1), (s2)), _OO (0x7a03), _OO (0x7405), JMPm (((d)))) /* JP to JMP, JZ past JMP */
#define jit_bger_f(d, s1, s2)            (UCOMISSrr ((s2), (s1)), JAEm ((d)))
#define jit_bgtr_f(d, s1, s2)            (UCOMISSrr ((s2), (s1)), JAm ((d)))
#define jit_bunltr_f(d, s1, s2)          (UCOMISSrr ((s2), (s1)), JNAEm ((d)))
#define jit_bunler_f(d, s1, s2)          (UCOMISSrr ((s2), (s1)), JNAm ((d)))
#define jit_buneqr_f(d, s1, s2)          (UCOMISSrr ((s1), (s2)), JEm ((d)))
#define jit_bltgtr_f(d, s1, s2)          (UCOMISSrr ((s1), (s2)), JNEm ((d)))
#define jit_bunger_f(d, s1, s2)          (UCOMISSrr ((s1), (s2)), JNAm ((d)))
#define jit_bungtr_f(d, s1, s2)          (UCOMISSrr ((s1), (s2)), JNAEm ((d)))
#define jit_bordr_f(d, s1, s2)           (UCOMISSrr ((s1), (s2)), JNPm ((d)))
#define jit_bunordr_f(d, s1, s2)         (UCOMISSrr ((s1), (s2)), JPm ((d)))

#define jit_bltr_d(d, s1, s2)            (UCOMISDrr ((s1), (s2)), JAm ((d)))
#define jit_bler_d(d, s1, s2)            (UCOMISDrr ((s1), (s2)), JAEm ((d)))
#define jit_beqr_d(d, s1, s2)            (UCOMISDrr ((s1), (s2)), _OO (0x7a06), JEm ((d)))
#define jit_bner_d(d, s1, s2)            (UCOMISDrr ((s1), (s2)), _OO (0x7a03), _OO (0x7405), JMPm (((d)))) /* JP to JMP, JZ past JMP */ 
#define jit_bger_d(d, s1, s2)            (UCOMISDrr ((s2), (s1)), JAEm ((d)))
#define jit_bgtr_d(d, s1, s2)            (UCOMISDrr ((s2), (s1)), JAm ((d)))
#define jit_bunltr_d(d, s1, s2)          (UCOMISDrr ((s2), (s1)), JNAEm ((d)))
#define jit_bunler_d(d, s1, s2)          (UCOMISDrr ((s2), (s1)), JNAm ((d)))
#define jit_buneqr_d(d, s1, s2)          (UCOMISDrr ((s1), (s2)), JEm ((d)))
#define jit_bltgtr_d(d, s1, s2)          (UCOMISDrr ((s1), (s2)), JNEm ((d)))
#define jit_bunger_d(d, s1, s2)          (UCOMISDrr ((s1), (s2)), JNAm ((d)))
#define jit_bungtr_d(d, s1, s2)          (UCOMISDrr ((s1), (s2)), JNAEm ((d)))
#define jit_bordr_d(d, s1, s2)           (UCOMISDrr ((s1), (s2)), JNPm ((d)))
#define jit_bunordr_d(d, s1, s2)         (UCOMISDrr ((s1), (s2)), JPm ((d)))

#define jit_ltr_f(d, s1, s2)            (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETAr (jit_reg8((d))))
#define jit_ler_f(d, s1, s2)            (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETAEr (jit_reg8((d))))
#define jit_eqr_f(d, s1, s2)            (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), _OO(0x7a03), SETEr (jit_reg8((d))))
#define jit_ner_f(d, s1, s2)            (UCOMISSrr ((s1), (s2)), MOVLir (1, (d)), _OO(0x7a03), SETNEr (jit_reg8((d))))
#define jit_ger_f(d, s1, s2)            (XORLrr ((d), (d)), UCOMISSrr ((s2), (s1)), SETAEr (jit_reg8((d))))
#define jit_gtr_f(d, s1, s2)            (XORLrr ((d), (d)), UCOMISSrr ((s2), (s1)), SETAr (jit_reg8((d))))
#define jit_unltr_f(d, s1, s2)          (XORLrr ((d), (d)), UCOMISSrr ((s2), (s1)), SETNAEr (jit_reg8((d))))
#define jit_unler_f(d, s1, s2)          (XORLrr ((d), (d)), UCOMISSrr ((s2), (s1)), SETNAr (jit_reg8((d))))
#define jit_uneqr_f(d, s1, s2)          (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETEr (jit_reg8((d))))
#define jit_ltgtr_f(d, s1, s2)          (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETNEr (jit_reg8((d))))
#define jit_unger_f(d, s1, s2)          (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETNAr (jit_reg8((d))))
#define jit_ungtr_f(d, s1, s2)          (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETNAEr (jit_reg8((d))))
#define jit_ordr_f(d, s1, s2)           (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETNPr (jit_reg8((d))))
#define jit_unordr_f(d, s1, s2)         (XORLrr ((d), (d)), UCOMISSrr ((s1), (s2)), SETPr (jit_reg8((d))))

#define jit_ltr_d(d, s1, s2)            (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETAr (jit_reg8((d))))
#define jit_ler_d(d, s1, s2)            (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETAEr (jit_reg8((d))))
#define jit_eqr_d(d, s1, s2)            (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), _OO(0x7a03), SETEr (jit_reg8((d))))
#define jit_ner_d(d, s1, s2)            (UCOMISDrr ((s1), (s2)), MOVLir (1, (d)), _OO(0x7a03), SETNEr (jit_reg8((d))))
#define jit_ger_d(d, s1, s2)            (XORLrr ((d), (d)), UCOMISDrr ((s2), (s1)), SETAEr (jit_reg8((d))))
#define jit_gtr_d(d, s1, s2)            (XORLrr ((d), (d)), UCOMISDrr ((s2), (s1)), SETAr (jit_reg8((d))))
#define jit_unltr_d(d, s1, s2)          (XORLrr ((d), (d)), UCOMISDrr ((s2), (s1)), SETNAEr (jit_reg8((d))))
#define jit_unler_d(d, s1, s2)          (XORLrr ((d), (d)), UCOMISDrr ((s2), (s1)), SETNAr (jit_reg8((d))))
#define jit_uneqr_d(d, s1, s2)          (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETEr (jit_reg8((d))))
#define jit_ltgtr_d(d, s1, s2)          (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETNEr (jit_reg8((d))))
#define jit_unger_d(d, s1, s2)          (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETNAr (jit_reg8((d))))
#define jit_ungtr_d(d, s1, s2)          (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETNAEr (jit_reg8((d))))
#define jit_ordr_d(d, s1, s2)           (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETNPr (jit_reg8((d))))
#define jit_unordr_d(d, s1, s2)         (XORLrr ((d), (d)), UCOMISDrr ((s1), (s2)), SETPr (jit_reg8((d))))

#define jit_prepare_f(num)              (_jitl.nextarg_putfp = _XMM0 + (num))
#define jit_prepare_d(num)              (_jitl.nextarg_putfp = _XMM0 + (num))

#define jit_arg_f()                     (_XMM0 + _jitl.nextarg_getfp++)
#define jit_arg_d()                     (_XMM0 + _jitl.nextarg_getfp++)

#define jit_getarg_f(rd, ofs)           (jit_movr_f ((rd), (ofs)))
#define jit_getarg_d(rd, ofs)           (jit_movr_d ((rd), (ofs)))

#define jit_pusharg_f(rs)               (--_jitl.nextarg_putfp, jit_movr_f (_jitl.nextarg_putfp, (rs)))
#define jit_pusharg_d(rs)               (--_jitl.nextarg_putfp, jit_movr_d (_jitl.nextarg_putfp, (rs)))

#endif /* __lightning_fp_h */
