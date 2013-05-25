/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer (PowerPC version)
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




#ifndef __lightning_core_h
#define __lightning_core_h

struct jit_local_state {
   int	nextarg_puti;  /* number of integer args */
   int	nextarg_putf;  /* number of float args   */
   int	nextarg_putd;  /* number of double args  */
   int	nextarg_geti;  /* Next r20-r25 reg. to be read */
   int	nextarg_getd;  /* The FP args are picked up from FPR1 -> FPR10 */
   int  nbArgs;        /* Number of arguments for the prolog */
   int long_jumps;    /* 1 => patch or leave room for long jumps */
#  define LONG_JUMPS_DEFAULT(jitl) 1
};

#define JIT_SP			1
#define JIT_FP			1
#define JIT_RET			3
#define JIT_R_NUM		3
#define JIT_V_NUM		7
#define JIT_R(i)		(9+(i))
#define JIT_V(i)		(31-(i))
#define JIT_AUX			JIT_V(JIT_V_NUM)  /* for 32-bit operands & shift counts */

#define jit_pfx_start()   (_jit.jitl.trampolines)
#define jit_pfx_end()     (_jit.jitl.free)

/* If possible, use the `small' instruction (rd, rs, imm)
 * else load imm into r26 and use the `big' instruction (rd, rs, r26)
 */
#define jit_chk_ims(imm, small, big)		(_siP(16,(imm)) ? (small) : (MOVEIri(JIT_AUX, imm),  (big)) )
#define jit_chk_imu(imm, small, big)		(_uiP(16,(imm)) ? (small) : (MOVEIri(JIT_AUX, imm),  (big)) )
#define jit_chk_imu15(imm, small, big)		(_uiP(15,(imm)) ? (small) : (MOVEIri(JIT_AUX, imm),  (big)) )

#define jit_big_ims(imm, big)	               (MOVEIri(JIT_AUX, imm),  (big))
#define jit_big_imu(imm, big)	               (MOVEIri(JIT_AUX, imm),  (big))

/* Helper macros for branches */
#define jit_s_brai(rs, is, jmp)			(jit_chk_ims (is, CMPWIri(rs, is), CMPWrr(rs, JIT_AUX)),   jmp, _jit.x.pc)
#define jit_s_brar(s1, s2, jmp)			(		  CMPWrr(s1, s2), 		           jmp, _jit.x.pc)
#define jit_u_brai(rs, is, jmp)			(jit_chk_imu (is, CMPLWIri(rs, is), CMPLWrr(rs, JIT_AUX)), jmp, _jit.x.pc)
#define jit_u_brar(s1, s2, jmp)			(		  CMPLWrr(s1, s2), 		           jmp, _jit.x.pc)

/* Helper macros for boolean tests. */
#define jit_sbooli(d, rs, is, jmp)		(jit_chk_ims (is, CMPWIri (rs, is), CMPWrr(rs, JIT_AUX)),  MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)))
#define jit_sboolr(d, s1, s2, jmp)		(		  CMPWrr  (s1, s2), 		           MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)))
#define jit_sbooli2(d, rs, is, jmp)		(jit_chk_ims (is, CMPWIri (rs, is), CMPWrr(rs, JIT_AUX)),  MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)), XORIrri((d), (d), 1))
#define jit_sboolr2(d, s1, s2, jmp)		(		  CMPWrr  (s1, s2), 		           MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)), XORIrri((d), (d), 1))
#define jit_ubooli(d, rs, is, jmp)		(jit_chk_imu (is, CMPLWIri(rs, is), CMPLWrr(rs, JIT_AUX)), MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)))
#define jit_uboolr(d, s1, s2, jmp)		(		  CMPLWrr (s1, s2), 		           MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)))
#define jit_ubooli2(d, rs, is, jmp)		(jit_chk_imu (is, CMPLWIri(rs, is), CMPLWrr(rs, JIT_AUX)), MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)), XORIrri((d), (d), 1))
#define jit_uboolr2(d, s1, s2, jmp)		(		  CMPLWrr (s1, s2), 		           MFCRr((d)), EXTRWIrrii((d), (d), 1, (jmp)), XORIrri((d), (d), 1))

/* modulus with immediate
 * movei r26, imm
 * mtlr  r31
 * divw  r31, rs, r26		(or divwu)
 * mullw r31, r31, r26
 * sub   rs, rs, r26
 * mflr  r31
 */

#define _jit_mod(div, rs, imm)			(MOVEIri(JIT_AUX, (imm)), MTLRr(31), (div), \
						MULLWrrr(31, 31, JIT_AUX), SUBrrr((rs), (rs), JIT_AUX), \
						MFLRr(31))

/* Patch a movei instruction made of a LIS at lis_pc and an ORI at ori_pc. */
#define jit_patch_movei(lis_pc, ori_pc, dest)			\
	(*(lis_pc) &= ~_MASK(16), *(lis_pc) |= _HI(dest),		\
	 *(ori_pc) &= ~_MASK(16), *(ori_pc) |= _LO(dest))		\

/* Patch a branch instruction */
#define jit_patch_branch(jump_pc,pv)				\
	(*(jump_pc) &= ~_MASK(16) | 3,				\
	 *(jump_pc) |= (_jit_UL(pv) - _jit_UL(jump_pc)) & _MASK(16))

#define jit_patch_ucbranch(jump_pc,pv)                          \
         (*(jump_pc) &= ~_MASK(26) | 3,                         \
         (*(jump_pc) |= (_jit_UL((pv)) - _jit_UL(jump_pc)) & _MASK(26)))

#define _jit_b_encoding		(18 << 26)
#define _jit_blr_encoding	((19 << 26) | (20 << 21) | (00 << 16) | (00 << 11) | (16 << 1))
#define _jit_is_ucbranch(a)     (((*(a) & (63<<26)) == _jit_b_encoding))

#define jit_patch_at(jump_pc, value) (				\
	((*(jump_pc - 1) & ~1) == _jit_blr_encoding)		\
	  ? jit_patch_movei(((jump_pc) - 4), ((jump_pc) - 3), (value))	\
	  : ( _jit_is_ucbranch((jump_pc) - 1)                   \
             ? jit_patch_ucbranch((jump_pc) - 1, (value))       \
             : jit_patch_branch((jump_pc) - 1, (value))))

#define jit_patch_movi(movi_pc, val)					\
	jit_patch_movei((movi_pc) - 2, (movi_pc) - 1, (val))

#define	jit_arg_c()			(_jitl.nextarg_geti++)
#define	jit_arg_i()			(_jitl.nextarg_geti++)
#define	jit_arg_l()			(_jitl.nextarg_geti++)
#define	jit_arg_p()			(_jitl.nextarg_geti++)
#define	jit_arg_s()			(_jitl.nextarg_geti++)
#define	jit_arg_uc()			(_jitl.nextarg_geti++)
#define	jit_arg_ui()			(_jitl.nextarg_geti++)
#define	jit_arg_ul()			(_jitl.nextarg_geti++)
#define	jit_arg_us()			(_jitl.nextarg_geti++)

/* Check Mach-O-Runtime documentation: Must skip GPR(s) whenever "corresponding" FPR is used */
#define jit_arg_f()                    (_jitl.nextarg_geti-- ,_jitl.nextarg_getd++)
#define jit_arg_d()                    (_jitl.nextarg_geti-=2,_jitl.nextarg_getd++)

#define jit_addi_i(d, rs, is)		jit_chk_ims((is), ADDICrri((d), (rs), (is)), ADDrrr((d), (rs), JIT_AUX))
#define jit_addr_i(d, s1, s2)				  ADDrrr((d), (s1), (s2))
#define jit_addci_i(d, rs, is)		jit_chk_ims((is), ADDICrri((d), (rs), (is)), ADDCrrr((d), (rs), JIT_AUX))
#define jit_addcr_i(d, s1, s2)				  ADDCrrr((d), (s1), (s2))
#define jit_addxi_i(d, rs, is)		(MOVEIri(JIT_AUX, (is)), ADDErrr((d), (rs), JIT_AUX))
#define jit_addxr_i(d, s1, s2)				         ADDErrr((d), (s1), (s2))
#define jit_andi_i(d, rs, is)		jit_chk_imu((is), ANDI_rri((d), (rs), (is)), ANDrrr((d), (rs), JIT_AUX))
#define jit_andr_i(d, s1, s2)				  ANDrrr((d), (s1), (s2))
#define jit_bmsi_i(label, rs, is)	(jit_chk_imu((is), ANDI_rri(JIT_AUX, (rs), (is)), AND_rrr(JIT_AUX, (rs), JIT_AUX)), BGTi((label)), _jit.x.pc)
#define jit_bmci_i(label, rs, is)	(jit_chk_imu((is), ANDI_rri(JIT_AUX, (rs), (is)), AND_rrr(JIT_AUX, (rs), JIT_AUX)), BEQi((label)), _jit.x.pc)
#define jit_bmsr_i(label, s1, s2)	(		   AND_rrr(JIT_AUX, (s1), (s2)),				    BGTi((label)), _jit.x.pc)
#define jit_bmcr_i(label, s1, s2)	(		   AND_rrr(JIT_AUX, (s1), (s2)),				    BEQi((label)), _jit.x.pc)
#define jit_beqi_i(label, rs, is)	jit_s_brai((rs), (is), BEQi((label)) )
#define jit_beqr_i(label, s1, s2)	jit_s_brar((s1), (s2), BEQi((label)) )
#define jit_bgei_i(label, rs, is)	jit_s_brai((rs), (is), BGEi((label)) )
#define jit_bgei_ui(label, rs, is)	jit_u_brai((rs), (is), BGEi((label)) )
#define jit_bger_i(label, s1, s2)	jit_s_brar((s1), (s2), BGEi((label)) )
#define jit_bger_ui(label, s1, s2)	jit_u_brar((s1), (s2), BGEi((label)) )
#define jit_bgti_i(label, rs, is)	jit_s_brai((rs), (is), BGTi((label)) )
#define jit_bgti_ui(label, rs, is)	jit_u_brai((rs), (is), BGTi((label)) )
#define jit_bgtr_i(label, s1, s2)	jit_s_brar((s1), (s2), BGTi((label)) )
#define jit_bgtr_ui(label, s1, s2)	jit_u_brar((s1), (s2), BGTi((label)) )
#define jit_blei_i(label, rs, is)	jit_s_brai((rs), (is), BLEi((label)) )
#define jit_blei_ui(label, rs, is)	jit_u_brai((rs), (is), BLEi((label)) )
#define jit_bler_i(label, s1, s2)	jit_s_brar((s1), (s2), BLEi((label)) )
#define jit_bler_ui(label, s1, s2)	jit_u_brar((s1), (s2), BLEi((label)) )
#define jit_blti_i(label, rs, is)	jit_s_brai((rs), (is), BLTi((label)) )
#define jit_blti_ui(label, rs, is)	jit_u_brai((rs), (is), BLTi((label)) )
#define jit_bltr_i(label, s1, s2)	jit_s_brar((s1), (s2), BLTi((label)) )
#define jit_bltr_ui(label, s1, s2)	jit_u_brar((s1), (s2), BLTi((label)) )
#define jit_bnei_i(label, rs, is)	jit_s_brai((rs), (is), BNEi((label)) )
#define jit_bner_i(label, s1, s2)	jit_s_brar((s1), (s2), BNEi((label)) )
#define jit_boaddi_i(label, rs, is)	(MOVEIri(JIT_AUX, (is)), ADDOrrr((rs), (rs), JIT_AUX), MCRXRi(0), BGTi((label)), _jit.x.pc) /* GT = bit 1 of XER = OV */
#define jit_bosubi_i(label, rs, is)	(MOVEIri(JIT_AUX, (is)), SUBCOrrr((rs), (rs), JIT_AUX), MCRXRi(0), BGTi((label)), _jit.x.pc)
#define jit_boaddr_i(label, s1, s2)	(		         ADDOrrr((s1), (s1), (s2)), 	   MCRXRi(0), BGTi((label)), _jit.x.pc)
#define jit_bosubr_i(label, s1, s2)	(		  	 SUBCOrrr((s1), (s1), (s2)), 	   MCRXRi(0), BGTi((label)), _jit.x.pc)
#define jit_boaddi_ui(label, rs, is)	(jit_chk_ims ((is), ADDICri((rs), (rs), is), ADDCrr((rs), JIT_AUX)),       MCRXRi(0), BEQi((label)), _jit.x.pc) /* EQ = bit 2 of XER = CA */
#define jit_bosubi_ui(label, rs, is)	(jit_chk_ims ((is), SUBICri((rs), (rs), is), SUBCrr((rs), JIT_AUX)),       MCRXRi(0), BEQi((label)), _jit.x.pc)
#define jit_boaddr_ui(label, s1, s2)	(		  			     ADDCrr((s1), (s1), (s2)), 	   MCRXRi(0), BEQi((label)), _jit.x.pc)
#define jit_bosubr_ui(label, s1, s2)	(		  			     SUBCrr((s1), (s1), (s2)), 	   MCRXRi(0), BEQi((label)), _jit.x.pc)
#define jit_bomulr_i(label, s1, s2)	(		         MULLWOrrr((s1), (s1), (s2)), 	   MCRXRi(0), BGTi((label)), _jit.x.pc)
#define jit_calli(label)	        ((void)jit_movi_p(JIT_AUX, (label)), MTCTRr(JIT_AUX), BCTRL(), _jitl.nextarg_puti = _jitl.nextarg_putf = _jitl.nextarg_putd = 0, _jit.x.pc)
#define jit_callr(reg)			(MTCTRr(reg), BCTRL())
#define jit_divi_i(d, rs, is)		jit_big_ims((is), DIVWrrr ((d), (rs), JIT_AUX))
#define jit_divi_ui(d, rs, is)	jit_big_imu((is), DIVWUrrr((d), (rs), JIT_AUX))
#define jit_divr_i(d, s1, s2)		DIVWrrr ((d), (s1), (s2))
#define jit_divr_ui(d, s1, s2)	DIVWUrrr((d), (s1), (s2))
#define jit_eqi_i(d, rs, is)		(jit_chk_ims((is), SUBIrri(JIT_AUX, (rs), (is)), SUBrrr(JIT_AUX, (rs), JIT_AUX)), SUBFICrri((d), JIT_AUX, 0), ADDErrr((d), (d), JIT_AUX))
#define jit_eqr_i(d, s1, s2)		(SUBrrr(JIT_AUX, (s1), (s2)), SUBFICrri((d), JIT_AUX, 0), ADDErrr((d), (d), JIT_AUX))
#define jit_extr_c_i(d, rs)		EXTSBrr((d), (rs))
#define jit_extr_s_i(d, rs)		EXTSHrr((d), (rs))
#define jit_gei_i(d, rs, is)		jit_sbooli2((d), (rs), (is), _lt)
#define jit_gei_ui(d, rs, is)		jit_ubooli2((d), (rs), (is), _lt)
#define jit_ger_i(d, s1, s2)		jit_sboolr2((d), (s1), (s2), _lt)
#define jit_ger_ui(d, s1, s2)		jit_uboolr2((d), (s1), (s2), _lt)
#define jit_gti_i(d, rs, is)		jit_sbooli ((d), (rs), (is), _gt)
#define jit_gti_ui(d, rs, is)		jit_ubooli ((d), (rs), (is), _gt)
#define jit_gtr_i(d, s1, s2)		jit_sboolr ((d), (s1), (s2), _gt)
#define jit_gtr_ui(d, s1, s2)		jit_uboolr ((d), (s1), (s2), _gt)
#define jit_hmuli_i(d, rs, is)		jit_big_ims((is), MULHWrrr ((d), (rs), JIT_AUX))
#define jit_hmuli_ui(d, rs, is)		jit_big_imu((is), MULHWUrrr((d), (rs), JIT_AUX))
#define jit_hmulr_i(d, s1, s2)				        MULHWrrr ((d), (s1), (s2))
#define jit_hmulr_ui(d, s1, s2)				        MULHWUrrr((d), (s1), (s2))
#define jit_jmpi(label)			(B_EXT((label)), _jit.x.pc)
#define jit_jmpr(reg)			(MTLRr(reg), BLR())
#define jit_ldxi_c(d, rs, is)		(jit_ldxi_uc((d), (rs), (is)), jit_extr_c_i((d), (d)))
#define jit_ldxr_c(d, s1, s2)		(jit_ldxr_uc((d), (s1), (s2)), jit_extr_c_i((d), (d)))
#define jit_ldxi_i(d, rs, is)		jit_chk_ims((is), LWZrm((d), (is), (rs)), LWZrx((d), JIT_AUX, (rs)))
#define jit_ldxi_s(d, rs, is)		jit_chk_ims((is), LHArm((d), (is), (rs)), LHArx((d), JIT_AUX, (rs)))
#define jit_ldxi_uc(d, rs, is)		jit_chk_ims((is), LBZrm((d), (is), (rs)), LBZrx((d), JIT_AUX, (rs)))
#define jit_ldxi_us(d, rs, is)		jit_chk_ims((is), LHZrm((d), (is), (rs)), LHZrx((d), JIT_AUX, (rs)))
#define jit_ldxr_i(d, s1, s2)				 LWZrx((d), (s1), (s2))
#define jit_ldxr_s(d, s1, s2)				 LHArx((d), (s1), (s2))
#define jit_ldxr_uc(d, s1, s2)				 LBZrx((d), (s1), (s2))
#define jit_ldxr_us(d, s1, s2)				 LHZrx((d), (s1), (s2))
#define jit_lei_i(d, rs, is)		jit_sbooli2((d), (rs), (is), _gt )
#define jit_lei_ui(d, rs, is)		jit_ubooli2((d), (rs), (is), _gt )
#define jit_ler_i(d, s1, s2)		jit_sboolr2((d), (s1), (s2), _gt )
#define jit_ler_ui(d, s1, s2)		jit_uboolr2((d), (s1), (s2), _gt )
#define jit_lshi_i(d, rs, is)					     SLWIrri((d), (rs), (is))
#define jit_lshr_i(d, s1, s2)		(ANDI_rri(JIT_AUX, (s2), 31), SLWrrr ((d), (s1), JIT_AUX))
#define jit_lti_i(d, rs, is)		jit_sbooli ((d), (rs), (is), _lt )
#define jit_lti_ui(d, rs, is)		jit_ubooli ((d), (rs), (is), _lt )
#define jit_ltr_i(d, s1, s2)		jit_sboolr ((d), (s1), (s2), _lt )
#define jit_ltr_ui(d, s1, s2)		jit_uboolr ((d), (s1), (s2), _lt )
#define jit_modi_i(d, rs, is)		_jit_mod(jit_divi_i (31, (rs), JIT_AUX), (rs), (is))
#define jit_modi_ui(d, rs, is)		_jit_mod(jit_divi_ui(31, (rs), JIT_AUX), (rs), (is))
#define jit_modr_i(d, s1, s2)		(DIVWrrr(JIT_AUX, (s1), (s2)), MULLWrrr(JIT_AUX, JIT_AUX, (s2)), SUBrrr((d), (s1), JIT_AUX))
#define jit_modr_ui(d, s1, s2)		(DIVWUrrr(JIT_AUX, (s1), (s2)), MULLWrrr(JIT_AUX, JIT_AUX, (s2)), SUBrrr((d), (s1), JIT_AUX))
#define jit_movi_i(d, is)		MOVEIri((d), (is))
#define jit_movi_p(d, is)		(LISri((d), _HI((is))),ORIrri((d),(d),_LO((is))),_jit.x.pc)

#define jit_movr_i(d, rs)		(((d) == (rs)) ? 0 : MRrr((d), (rs)))
#define jit_muli_i(d, rs, is)		jit_chk_ims  ((is), MULLIrri((d), (rs), (is)), MULLWrrr((d), (rs), JIT_AUX))
#define jit_muli_ui(d, rs, is)		jit_chk_imu15((is), MULLIrri((d), (rs), (is)), MULLWrrr((d), (rs), JIT_AUX))
#define jit_mulr_i(d, s1, s2)				    MULLWrrr((d), (s1), (s2))
#define jit_mulr_ui(d, s1, s2)				    MULLWrrr((d), (s1), (s2))
#define jit_nei_i(d, rs, is)		(jit_chk_ims((is), SUBIrri(JIT_AUX, (rs), (is)), SUBrrr(JIT_AUX, (rs), JIT_AUX)), ADDICrri((d), JIT_AUX, -1), SUBFErrr((d), (d), JIT_AUX))
#define jit_ner_i(d, s1, s2)		(SUBrrr(JIT_AUX, (s1), (s2)), ADDICrri((d), JIT_AUX, -1), SUBFErrr((d), (d), JIT_AUX))
#define jit_nop()			NOP()
#define jit_ori_i(d, rs, is)		jit_chk_imu((is), ORIrri((d), (rs), (is)), ORrrr((d), (rs), JIT_AUX))
#define jit_orr_i(d, s1, s2)				  ORrrr((d), (s1), (s2))
#define jit_popr_i(rs)			(LWZrm((rs), 0, 1), ADDIrri(1, 1, 4))
#define jit_prepare_i(numi)		(_jitl.nextarg_puti = numi)
#define jit_prepare_f(numf)		(_jitl.nextarg_putf = numf)
#define jit_prepare_d(numd)		(_jitl.nextarg_putd = numd)
#define jit_prolog(n)			_jit_prolog(&_jit, (n))
#define jit_pushr_i(rs)			STWUrm((rs), -4, 1)
#define jit_pusharg_i(rs)		(--_jitl.nextarg_puti, MRrr((3 + _jitl.nextarg_putd * 2 + _jitl.nextarg_putf + _jitl.nextarg_puti), (rs)))
#define jit_save_argstate(curstate)		(curstate = _jitl.nextarg_puti)
#define jit_restore_argstate(curstate)		(_jitl.nextarg_puti = curstate)
#define jit_ret()			_jit_epilog(&_jit)
#define jit_retval_i(rd)		MRrr((rd), 3)
#define jit_rsbi_i(d, rs, is)		jit_chk_ims((is), SUBFICrri((d), (rs), (is)), SUBFCrrr((d), (rs), JIT_AUX))
#define jit_rshi_i(d, rs, is)					     SRAWIrri((d), (rs), (is))
#define jit_rshi_ui(d, rs, is)					     SRWIrri ((d), (rs), (is))
#define jit_rshr_i(d, s1, s2)		(ANDI_rri(JIT_AUX, (s2), 31), SRAWrrr ((d), (s1), JIT_AUX))
#define jit_rshr_ui(d, s1, s2)		(ANDI_rri(JIT_AUX, (s2), 31), SRWrrr  ((d), (s1), JIT_AUX))
#define jit_stxi_c(id, rd, rs)		jit_chk_ims((id), STBrm((rs), (id), (rd)), STBrx((rs), (rd), JIT_AUX))
#define jit_stxi_i(id, rd, rs)		jit_chk_ims((id), STWrm((rs), (id), (rd)), STWrx((rs), (rd), JIT_AUX))
#define jit_stxi_s(id, rd, rs)		jit_chk_ims((id), STHrm((rs), (id), (rd)), STHrx((rs), (rd), JIT_AUX))
#define jit_stxr_c(d1, d2, rs)				  STBrx((rs), (d1), (d2))
#define jit_stxr_i(d1, d2, rs)				  STWrx((rs), (d1), (d2))
#define jit_stxr_s(d1, d2, rs)				  STHrx((rs), (d1), (d2))
#define jit_subr_i(d, s1, s2)				  SUBrrr((d), (s1), (s2))
#define jit_subcr_i(d, s1, s2)				  SUBCrrr((d), (s1), (s2))
#define jit_subxi_i(d, rs, is)		jit_big_ims((is), SUBErrr((d), (rs), JIT_AUX))
#define jit_subxr_i(d, s1, s2)				  SUBErrr((d), (s1), (s2))
#define jit_xori_i(d, rs, is)		jit_chk_imu((is), XORIrri((d), (rs), (is)), XORrrr((d), (rs), JIT_AUX))
#define jit_xorr_i(d, s1, s2)				  XORrrr((d), (s1), (s2))

/* Cannot use JIT_RZERO because having 0 in a register field on the PowerPC
 * does not mean `a register whose value is 0', but rather `no register at
 * all' */

#define jit_negr_i(d, rs)		jit_rsbi_i((d), (rs), 0)
#define jit_negr_l(d, rs)		jit_rsbi_l((d), (rs), 0)
#define jit_ldr_c(rd, rs)		jit_ldxr_c((rd), 0, (rs))	      
#define jit_str_c(rd, rs)		jit_stxr_c(0, (rd), (rs))	      
#define jit_ldr_s(rd, rs)		jit_ldxr_s((rd), 0, (rs))	      
#define jit_str_s(rd, rs)		jit_stxr_s(0, (rd), (rs))	      
#define jit_ldr_i(rd, rs)		jit_ldxr_i((rd), 0, (rs))	      
#define jit_str_i(rd, rs)		jit_stxr_i(0, (rd), (rs))	      
#define jit_ldr_uc(rd, rs)		jit_ldxr_uc((rd), 0, (rs))	      
#define jit_ldr_us(rd, rs)		jit_ldxr_us((rd), 0, (rs))	      

#define jit_stir_l(rd, is) (jit_movi_l(JIT_AUX, (intptr_t)(is)), jit_str_l(rd, JIT_AUX))
#define jit_stixi_l(id, rd, is) (jit_movi_l(JIT_AUX, is), jit_stxi_l(id, rd, JIT_AUX))
#define jit_stir_p(rd, is) jit_stir_l(rd, is)
#define jit_stixi_p(id, rd, is) jit_stixi_l(id, rd, is) 

/* e.g.
 *	0x01234567	_HA << 16 = 0x01230000	_LA = 0x00004567 _HA << 16 + LA = 0x01234567
 * 	0x89abcdef	_HA << 16 = 0x89ac0000  _LA = 0xffffcdef _HA << 16 + LA = 0x89abcdef
 */
#define _HA(addr)			((_jit_UL(addr) >> 16) + (_jit_US(_jit_UL(addr)) >> 15))
#define _LA(addr)			(_jit_UL(addr) - (_HA(addr) << 16))

#define jit_ldi_c(rd, is)		(LISri(JIT_AUX, _HA(is)), jit_ldxi_c((rd), JIT_AUX, _LA(is)))
#define jit_sti_c(id, rs)		(LISri(JIT_AUX, _HA(id)), jit_stxi_c(_LA(id), JIT_AUX, (rs)))
#define jit_ldi_s(rd, is)		(LISri(JIT_AUX, _HA(is)), jit_ldxi_s((rd), JIT_AUX, _LA(is)))
#define jit_sti_s(id, rs)		(LISri(JIT_AUX, _HA(id)), jit_stxi_s(_LA(id), JIT_AUX, (rs)))
#define jit_ldi_i(rd, is)		(LISri(JIT_AUX, _HA(is)), jit_ldxi_i((rd), JIT_AUX, _LA(is)))
#define jit_sti_i(id, rs)		(LISri(JIT_AUX, _HA(id)), jit_stxi_i(_LA(id), JIT_AUX, (rs)))
#define jit_ldi_uc(rd, is)		(LISri(JIT_AUX, _HA(is)), jit_ldxi_uc((rd), JIT_AUX, _LA(is)))
#define jit_ldi_us(rd, is)		(LISri(JIT_AUX, _HA(is)), jit_ldxi_us((rd), JIT_AUX, _LA(is)))

#endif /* __lightning_core_h */
