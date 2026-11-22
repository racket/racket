/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer support (AArch64 version)
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002, 2003, 2026 Free Software Foundation, Inc.
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
 * This is a 64-bit-only reduction of "../arm/core-common.h".
 ***********************************************************************/

#ifndef __lightning_core_common_h_
#define __lightning_core_common_h_

#define JIT_NOREG			(-1)

#define	jit_get_raw_ip()		_jitp->x.pc
#define	jit_get_ip()			(_jitp->x.pc)
/* Also records the start of a code block, so that short-mode branches
   can recognize targets outside the block: */
#define	jit_set_ip(ptr)			(_jitp->x.pc = (ptr), _jitl.block_start = _jitp->x.pc, jit_get_ip ())
#define	jit_get_label()			jit_get_ip()
#define	jit_forward()			jit_get_ip()

#define jit_adjust_ip(x)		(x)
#define jit_unadjust_ip(x)		(x)

#define	jit_field(struc, f)		( ((intptr_t) (&((struc *) 8)->f) ) - 8)
#define	jit_ptr_field(struc_p, f)	( ((intptr_t) (&((struc_p) 8)->f) ) - 8)

#ifndef jit_align
#define jit_align(n)
#endif

/* jit_code: union of many possible function pointer types.  Returned
 * by jit_get_ip().
 */
typedef union jit_code {
  char		 *ptr;
  void		 (*vptr)(void);
  char		 (*cptr)(void);
  unsigned char	 (*ucptr)(void);
  short		 (*sptr)(void);
  unsigned short (*usptr)(void);
  int		 (*iptr)(void);
  unsigned int	 (*uiptr)(void);
  long		 (*lptr)(void);
  unsigned long	 (*ulptr)(void);
  void *	 (*pptr)(void);
  float		 (*fptr)(void);
  double	 (*dptr)(void);
} jit_code;

/* ALU synonyms */
#define jit_addi_ui(d, rs, is)		jit_addi_i((d), (rs), (is))
#define jit_addr_ui(d, s1, s2)		jit_addr_i((d), (s1), (s2))
#define jit_andi_ui(d, rs, is)		jit_andi_i((d), (rs), (is))
#define jit_andr_ui(d, s1, s2)		jit_andr_i((d), (s1), (s2))
#define jit_lshi_ui(d, rs, is)		jit_lshi_i((d), (rs), (is))
#define jit_lshr_ui(d, s1, s2)		jit_lshr_i((d), (s1), (s2))
#define jit_movr_ui(d, rs)		jit_movr_i((d), (rs))
#define jit_ori_ui(d, rs, is)		jit_ori_i((d), (rs), (is))
#define jit_orr_ui(d, s1, s2)		jit_orr_i((d), (s1), (s2))
#define jit_rsbi_ui(d, rs, is)		jit_rsbi_i((d), (rs), (is))
#define jit_rsbr_ui(d, s1, s2)		jit_rsbr_i((d), (s1), (s2))
#define jit_subi_ui(d, rs, is)		jit_subi_i((d), (rs), (is))
#define jit_subr_ui(d, s1, s2)		jit_subr_i((d), (s1), (s2))
#define jit_xori_ui(d, rs, is)		jit_xori_i((d), (rs), (is))
#define jit_xorr_ui(d, s1, s2)		jit_xorr_i((d), (s1), (s2))
#define jit_notr_ui(d, rs)		jit_notr_i((d), (rs))

#define jit_addi_ul(d, rs, is)		jit_addi_l((d), (rs), (is))
#define jit_addr_ul(d, s1, s2)		jit_addr_l((d), (s1), (s2))
#define jit_andi_ul(d, rs, is)		jit_andi_l((d), (rs), (is))
#define jit_andr_ul(d, s1, s2)		jit_andr_l((d), (s1), (s2))
#define jit_lshi_ul(d, rs, is)		jit_lshi_l((d), (rs), (is))
#define jit_lshr_ul(d, s1, s2)		jit_lshr_l((d), (s1), (s2))
#define jit_ori_ul(d, rs, is)		jit_ori_l((d), (rs), (is))
#define jit_orr_ul(d, s1, s2)		jit_orr_l((d), (s1), (s2))
#define jit_rsbi_ul(d, rs, is)		jit_rsbi_l((d), (rs), (is))
#define jit_rsbr_ul(d, s1, s2)		jit_rsbr_l((d), (s1), (s2))
#define jit_subi_ul(d, rs, is)		jit_subi_l((d), (rs), (is))
#define jit_subr_ul(d, s1, s2)		jit_subr_l((d), (s1), (s2))
#define jit_xori_ul(d, rs, is)		jit_xori_l((d), (rs), (is))
#define jit_xorr_ul(d, s1, s2)		jit_xorr_l((d), (s1), (s2))
#define jit_mulr_ul(d, s1, s2)		jit_mulr_l((d), (s1), (s2))
#define jit_muli_ul(d, rs, is)		jit_muli_l((d), (rs), (is))
#define jit_notr_ul(d, rs)		jit_notr_l((d), (rs))
#define jit_negr_ul(d, rs)		jit_negr_l((d), (rs))

#define jit_addr_p(d, s1, s2)		jit_addr_ul((d), (s1),        (s2))
#define jit_addi_p(d, rs, is)		jit_addi_ul((d), (rs), (intptr_t) (is))
#define jit_subr_p(d, s1, s2)		jit_subr_ul((d), (s1),        (s2))
#define jit_subi_p(d, rs, is)		jit_subi_ul((d), (rs), (intptr_t) (is))
#define jit_rsbi_p(d, rs, is)		jit_rsbi_ul((d), (rs), (intptr_t) (is))
#define jit_rsbr_p(d, s1, s2)		jit_subr_p((d), (s2), (s1))

#define jit_patch(pv)			jit_patch_at ((pv), jit_get_ip())

/* Extensions */
#define jit_extr_c_s(d, rs)		jit_extr_c_i((d), (rs))
#define jit_extr_c_us(d, rs)		jit_extr_c_ui((d), (rs))
#define jit_extr_uc_s(d, rs)		jit_extr_uc_i((d), (rs))
#define jit_extr_uc_us(d, rs)		jit_extr_uc_ui((d), (rs))
#define jit_extr_uc_i(d, rs)		jit_extr_c_ui((d), (rs))
#define jit_extr_uc_ui(d, rs)		jit_extr_c_ui((d), (rs))
#define jit_extr_us_i(d, rs)		jit_extr_s_ui((d), (rs))
#define jit_extr_us_ui(d, rs)		jit_extr_s_ui((d), (rs))
#define jit_extr_uc_l(d, rs)		jit_extr_c_ul((d), (rs))
#define jit_extr_uc_ul(d, rs)		jit_extr_c_ul((d), (rs))
#define jit_extr_us_l(d, rs)		jit_extr_s_ul((d), (rs))
#define jit_extr_us_ul(d, rs)		jit_extr_s_ul((d), (rs))
#define jit_extr_ui_l(d, rs)		jit_extr_i_ul((d), (rs))
#define jit_extr_ui_ul(d, rs)		jit_extr_i_ul((d), (rs))

/* Stack synonyms */
#define jit_pushr_ui(rs)		jit_pushr_i(rs)
#define jit_popr_ui(rs)			jit_popr_i(rs)
#define jit_pushr_ul(rs)		jit_pushr_l(rs)
#define jit_popr_ul(rs)			jit_popr_l(rs)
#define jit_pushr_p(rs)			jit_pushr_ul(rs)
#define jit_popr_p(rs)			jit_popr_ul(rs)

#define jit_prepare(nint)		jit_prepare_i((nint))
#define jit_pusharg_c(rs)		jit_pusharg_i(rs)
#define jit_pusharg_s(rs)		jit_pusharg_i(rs)
#define jit_pusharg_uc(rs)		jit_pusharg_i(rs)
#define jit_pusharg_us(rs)		jit_pusharg_i(rs)
#define jit_pusharg_ui(rs)		jit_pusharg_i(rs)
#define jit_pusharg_ul(rs)		jit_pusharg_l(rs)
#define jit_pusharg_p(rs)		jit_pusharg_ul(rs)

#define jit_normal_pushonlyarg_p(r0)	jit_normal_pushonlyarg_i(r0)

/* Memory synonyms */
#define jit_str_uc(rd, rs)		jit_str_c((rd), (rs))
#define jit_sti_uc(id, rs)		jit_sti_c((id), (rs))
#define jit_stxr_uc(d1, d2, rs)		jit_stxr_c((d1), (d2), (rs))
#define jit_stxi_uc(id, rd, is)		jit_stxi_c((id), (rd), (is))

#define jit_str_us(rd, rs)		jit_str_s((rd), (rs))
#define jit_sti_us(id, rs)		jit_sti_s((id), (rs))
#define jit_stxr_us(d1, d2, rs)		jit_stxr_s((d1), (d2), (rs))
#define jit_stxi_us(id, rd, is)		jit_stxi_s((id), (rd), (is))

#define jit_str_ui(rd, rs)		jit_str_i((rd), (rs))
#define jit_sti_ui(id, rs)		jit_sti_i((id), (rs))
#define jit_stxr_ui(d1, d2, rs)		jit_stxr_i((d1), (d2), (rs))
#define jit_stxi_ui(id, rd, is)		jit_stxi_i((id), (rd), (is))

#define jit_str_ul(rd, rs)		jit_str_l((rd), (rs))
#define jit_sti_ul(id, rs)		jit_sti_l((id), (rs))
#define jit_stxr_ul(d1, d2, rs)		jit_stxr_l((d1), (d2), (rs))
#define jit_stxi_ul(id, rd, is)		jit_stxi_l((id), (rd), (is))

#define jit_str_p(rd, rs)		jit_str_l((rd), (rs))
#define jit_sti_p(id, rs)		jit_sti_l((id), (rs))
#define jit_stxr_p(d1, d2, rs)		jit_stxr_l((d1), (d2), (rs))
#define jit_stxi_p(id, rd, is)		jit_stxi_l((id), (rd), (is))

#define jit_ldr_p(rd, rs)		jit_ldr_l((rd), (rs))
#define jit_ldi_p(rd, is)		jit_ldi_l((rd), (is))
#define jit_ldxr_p(rd, s1, s2)		jit_ldxr_l((rd), (s1), (s2))
#define jit_ldxi_p(rd, rs, is)		jit_ldxi_l((rd), (rs), (is))

#define jit_ldr_ul(d, rs)		jit_ldr_l((d), (rs))
#define jit_ldi_ul(d, is)		jit_ldi_l((d), (is))
#define jit_ldxr_ul(d, s1, s2)		jit_ldxr_l((d), (s1), (s2))
#define jit_ldxi_ul(d, rs, is)		jit_ldxi_l((d), (rs), (is))

/* Branch synonyms */
#define jit_beqr_ui(label, s1, s2)	jit_beqr_i((label), (s1), (s2))
#define jit_beqi_ui(label, rs, is)	jit_beqi_i((label), (rs), (is))
#define jit_bner_ui(label, s1, s2)	jit_bner_i((label), (s1), (s2))
#define jit_bnei_ui(label, rs, is)	jit_bnei_i((label), (rs), (is))
#define jit_bmcr_ui(label, s1, s2)	jit_bmcr_i((label), (s1), (s2))
#define jit_bmci_ui(label, rs, is)	jit_bmci_i((label), (rs), (is))
#define jit_bmsr_ui(label, s1, s2)	jit_bmsr_i((label), (s1), (s2))
#define jit_bmsi_ui(label, rs, is)	jit_bmsi_i((label), (rs), (is))

#define jit_bltr_p(label, s1, s2)	jit_bltr_ul((label), (s1), (s2))
#define jit_blti_p(label, rs, is)	jit_blti_ul((label), (rs), (intptr_t)(is))
#define jit_bler_p(label, s1, s2)	jit_bler_ul((label), (s1), (s2))
#define jit_blei_p(label, rs, is)	jit_blei_ul((label), (rs), (intptr_t)(is))
#define jit_bgtr_p(label, s1, s2)	jit_bgtr_ul((label), (s1), (s2))
#define jit_bgti_p(label, rs, is)	jit_bgti_ul((label), (rs), (intptr_t)(is))
#define jit_bger_p(label, s1, s2)	jit_bger_ul((label), (s1), (s2))
#define jit_bgei_p(label, rs, is)	jit_bgei_ul((label), (rs), (intptr_t)(is))
#define jit_beqr_p(label, s1, s2)	jit_beqr_ul((label), (s1), (s2))
#define jit_beqi_p(label, rs, is)	jit_beqi_ul((label), (rs), (intptr_t)(is))
#define jit_bner_p(label, s1, s2)	jit_bner_ul((label), (s1), (s2))
#define jit_bnei_p(label, rs, is)	jit_bnei_ul((label), (rs), (intptr_t)(is))

#define jit_retval_ui(rd)		jit_retval_i((rd))
#define jit_retval_uc(rd)		jit_retval_i((rd))
#define jit_retval_us(rd)		jit_retval_i((rd))
#define jit_retval_ul(rd)		jit_retval_l((rd))
#define jit_retval_p(rd)		jit_retval_ul((rd))
#define jit_retval_c(rd)		jit_retval_i((rd))
#define jit_retval_s(rd)		jit_retval_i((rd))

/* This was a bug, but we keep it; on AArch64, jit_retval_i() moves all
   64 bits, so jit_retval() works for pointers. */
#define jit_retval(rd)			jit_retval_i ((rd))

#ifndef jit_leaf
#define jit_leaf(numargs)		jit_prolog(numargs)
#endif

#endif /* __lightning_core_common_h_ */
