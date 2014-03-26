/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer support
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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


#ifndef __lightning_core_common_h_
#define __lightning_core_common_h_

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

typedef struct {
  union {
    jit_insn             *pc;
    _uc                  *uc_pc;
    _us                  *us_pc;
    _ui                  *ui_pc;
    _ul                  *ul_pc;
    jit_code             code;
  }                       x;
  struct jit_fp		 *fp;
  struct jit_local_state jitl;
} jit_state;

#if 0
# ifdef jit_init
static jit_state 			_jit = jit_init ();
# else
static jit_state 			_jit;
# endif
#endif

#define JIT_NOREG			(-1)
#define JIT_R0				JIT_R(0)
#define JIT_R1				JIT_R(1)
#define JIT_R2				JIT_R(2)
#define JIT_V0				JIT_V(0)
#define JIT_V1				JIT_V(1)
#define JIT_V2				JIT_V(2)

#define _jitl				_jit.jitl

#define	jit_get_ip()			(_jit.x.pc)
#define	jit_get_raw_ip()                jit_get_ip()
#define	jit_set_ip(ptr)			(_jit.x.pc = (ptr), jit_get_ip ())
#define	jit_get_label()			(_jit.x.pc)
#define	jit_forward()			(_jit.x.pc)

#define jit_adjust_ip(x) x
#define jit_unadjust_ip(x) x

#define	jit_field(struc, f)		( ((long) (&((struc *) 8)->f) ) - 8)
#define	jit_ptr_field(struc_p, f)	( ((long) (&((struc_p) 8)->f) ) - 8)

/* realignment via N-byte no-ops */

#ifndef jit_align
#define jit_align(n)
#endif


#ifndef jit_fill_delay_after
#define jit_fill_delay_after(branch)	(branch)
#endif

#define jit_delay(insn, branch)		((insn), jit_fill_delay_after(branch))


/* ALU synonyms */
#define jit_addi_ui(d, rs, is)		jit_addi_i((d), (rs), (is))	
#define jit_addr_ui(d, s1, s2)		jit_addr_i((d), (s1), (s2))
#define jit_addci_ui(d, rs, is)		jit_addci_i((d), (rs), (is))	
#define jit_addcr_ui(d, s1, s2)		jit_addcr_i((d), (s1), (s2))
#define jit_addxi_ui(d, rs, is)		jit_addxi_i((d), (rs), (is))	
#define jit_addxr_ui(d, s1, s2)		jit_addxr_i((d), (s1), (s2))
#define jit_andi_ui(d, rs, is)		jit_andi_i((d), (rs), (is))	
#define jit_andr_ui(d, s1, s2)		jit_andr_i((d), (s1), (s2))
#define jit_lshi_ui(d, rs, is)		jit_lshi_i((d), (rs), (is))	
#define jit_lshr_ui(d, s1, s2)		jit_lshr_i((d), (s1), (s2))
#define jit_movi_ui(d, rs)		jit_movi_i((d), (rs))
#define jit_movr_ui(d, rs)		jit_movr_i((d), (rs))
#define jit_ori_ui(d, rs, is)		jit_ori_i((d), (rs), (is))	
#define jit_orr_ui(d, s1, s2)		jit_orr_i((d), (s1), (s2))
#define jit_rsbi_ui(d, rs, is)		jit_rsbi_i((d), (rs), (is))	
#define jit_rsbr_ui(d, s1, s2)		jit_rsbr_i((d), (s1), (s2))
#define jit_subi_ui(d, rs, is)		jit_subi_i((d), (rs), (is))	
#define jit_subr_ui(d, s1, s2)		jit_subr_i((d), (s1), (s2))
#define jit_subci_ui(d, rs, is)		jit_subci_i((d), (rs), (is))	
#define jit_subcr_ui(d, s1, s2)		jit_subcr_i((d), (s1), (s2))
#define jit_subxi_ui(d, rs, is)		jit_subxi_i((d), (rs), (is))	
#define jit_subxr_ui(d, s1, s2)		jit_subxr_i((d), (s1), (s2))
#define jit_xori_ui(d, rs, is)		jit_xori_i((d), (rs), (is))	
#define jit_xorr_ui(d, s1, s2)		jit_xorr_i((d), (s1), (s2))

#define jit_addi_ul(d, rs, is)		jit_addi_l((d), (rs), (is))	
#define jit_addr_ul(d, s1, s2)		jit_addr_l((d), (s1), (s2))
#define jit_addci_ul(d, rs, is)		jit_addci_l((d), (rs), (is))	
#define jit_addcr_ul(d, s1, s2)		jit_addcr_l((d), (s1), (s2))
#define jit_addxi_ul(d, rs, is)		jit_addxi_l((d), (rs), (is))	
#define jit_addxr_ul(d, s1, s2)		jit_addxr_l((d), (s1), (s2))
#define jit_andi_ul(d, rs, is)		jit_andi_l((d), (rs), (is))	
#define jit_andr_ul(d, s1, s2)		jit_andr_l((d), (s1), (s2))
#define jit_lshi_ul(d, rs, is)		jit_lshi_l((d), (rs), (is))	
#define jit_lshr_ul(d, s1, s2)		jit_lshr_l((d), (s1), (s2))
#define jit_movi_ul(d, rs)		jit_movi_l((d), (rs))
#define jit_movr_ul(d, rs)		jit_movr_l((d), (rs))
#define jit_ori_ul(d, rs, is)		jit_ori_l((d), (rs), (is))	
#define jit_orr_ul(d, s1, s2)		jit_orr_l((d), (s1), (s2))
#define jit_rsbi_ul(d, rs, is)		jit_rsbi_l((d), (rs), (is))	
#define jit_rsbr_ul(d, s1, s2)		jit_rsbr_l((d), (s1), (s2))
#define jit_subi_ul(d, rs, is)		jit_subi_l((d), (rs), (is))	
#define jit_subr_ul(d, s1, s2)		jit_subr_l((d), (s1), (s2))
#define jit_subci_ul(d, rs, is)		jit_subci_l((d), (rs), (is))	
#define jit_subcr_ul(d, s1, s2)		jit_subcr_l((d), (s1), (s2))
#define jit_subxi_ui(d, rs, is)		jit_subxi_i((d), (rs), (is))	
#define jit_subxi_ul(d, rs, is)		jit_subxi_l((d), (rs), (is))	
#define jit_subxr_ui(d, s1, s2)		jit_subxr_i((d), (s1), (s2))
#define jit_subxr_ul(d, s1, s2)		jit_subxr_i((d), (s1), (s2))
#define jit_xori_ul(d, rs, is)		jit_xori_l((d), (rs), (is))	
#define jit_xorr_ul(d, s1, s2)		jit_xorr_l((d), (s1), (s2))

#define jit_addr_p(d, s1, s2)		jit_addr_ul((d), (s1), 	      (s2))
#define jit_addi_p(d, rs, is)		jit_addi_ul((d), (rs), (long) (is))
#define jit_movr_p(d, rs)		jit_movr_ul((d),              (rs))
#define jit_subr_p(d, s1, s2)		jit_subr_ul((d), (s1),        (s2))
#define jit_subi_p(d, rs, is)		jit_subi_ul((d), (rs), (long) (is))
#define jit_rsbi_p(d, rs, is)		jit_rsbi_ul((d), (rs), (long) (is))

#ifndef jit_movi_p
#define jit_movi_p(d, is)		(jit_movi_ul((d),       (long) (is)), _jit.x.pc)
#endif

#define jit_patchable_movi_p(r, i)      jit_movi_p(r, i)
#define jit_patch(pv)        		jit_patch_at ((pv), (_jit.x.pc))

#ifndef jit_addci_i
#define jit_addci_i(d, rs, is)		jit_addi_i((d), (rs), (is))	
#define jit_addcr_i(d, s1, s2)		jit_addr_i((d), (s1), (s2))
#define jit_addci_l(d, rs, is)		jit_addi_l((d), (rs), (is))	
#define jit_addcr_l(d, s1, s2)		jit_addr_l((d), (s1), (s2))
#endif

#ifndef jit_subcr_i
#define jit_subcr_i(d, s1, s2)		jit_subr_i((d), (s1), (s2))
#endif

/* NEG is not mandatory -- pick an appropriate implementation */
#ifndef jit_negr_i
# ifdef JIT_RZERO
#  define jit_negr_i(d, rs)		jit_subr_i((d), JIT_RZERO, (rs))
#  define jit_negr_l(d, rs)		jit_subr_l((d), JIT_RZERO, (rs))
# else /* !JIT_RZERO */
#  ifndef jit_rsbi_i
#   define jit_negr_i(d, rs)		(jit_xori_i((d), (rs), -1), jit_addi_l((d), (d), 1))
#   define jit_negr_l(d, rs)		(jit_xori_l((d), (rs), -1), jit_addi_l((d), (d), 1))
#  else /* jit_rsbi_i */
#   define jit_negr_i(d, rs)		jit_rsbi_i((d), (rs), 0)
#   define jit_negr_l(d, rs)		jit_rsbi_l((d), (rs), 0)
#  endif /* jit_rsbi_i */
# endif /* !JIT_RZERO */
#endif /* !jit_negr_i */

/* RSB is not mandatory */
#ifndef jit_rsbi_i
# define jit_rsbi_i(d, rs, is)		(jit_subi_i((d), (rs), (is)), jit_negr_i((d), (d)))

# ifndef jit_rsbi_l
#  define jit_rsbi_l(d, rs, is)		(jit_subi_l((d), (rs), (is)), jit_negr_l((d), (d)))
# endif
#endif

/* Common 'shortcut' implementations */
#define jit_subi_i(d, rs, is)		jit_addi_i((d), (rs), -(is))
#define jit_subi_l(d, rs, is)		jit_addi_l((d), (rs), -(is))
#define jit_subci_i(d, rs, is)		jit_addci_i((d), (rs), -(is))
#define jit_subci_l(d, rs, is)		jit_addci_l((d), (rs), -(is))
#define jit_rsbr_f(d, s1, s2)		jit_subr_f((d), (s2), (s1))
#define jit_rsbr_d(d, s1, s2)		jit_subr_d((d), (s2), (s1))
#define jit_rsbr_i(d, s1, s2)		jit_subr_i((d), (s2), (s1))
#define jit_rsbr_l(d, s1, s2)		jit_subr_l((d), (s2), (s1))
#define jit_rsbr_p(d, s1, s2)		jit_subr_p((d), (s2), (s1))

/* Unary */
#define jit_notr_c(d, rs)		jit_xori_c((d), (rs), 255)
#define jit_notr_uc(d, rs)		jit_xori_c((d), (rs), 255)
#define jit_notr_s(d, rs)		jit_xori_s((d), (rs), 65535)
#define jit_notr_us(d, rs)		jit_xori_s((d), (rs), 65535)
#define jit_notr_i(d, rs)		jit_xori_i((d), (rs), ~0)
#define jit_notr_ui(d, rs)		jit_xori_i((d), (rs), ~0)
#define jit_notr_l(d, rs)		jit_xori_l((d), (rs), ~0L)
#define jit_notr_ul(d, rs)		jit_xori_l((d), (rs), ~0L)

#ifndef jit_extr_c_ui
#define jit_extr_c_ui(d, rs)		jit_andi_ui((d), (rs), 0xFF)
#endif
#ifndef jit_extr_s_ui
#define jit_extr_s_ui(d, rs)		jit_andi_ui((d), (rs), 0xFFFF)
#endif
#ifndef jit_extr_c_i
#define jit_extr_c_i(d, rs)		(jit_lshi_i((d), (rs), 24), jit_rshi_i((d), (d), 24))
#endif
#ifndef jit_extr_s_i
#define jit_extr_s_i(d, rs)		(jit_lshi_i((d), (rs), 16), jit_rshi_i((d), (d), 16))
#endif

#ifdef jit_addi_l /* sizeof(long) != sizeof(int) */
#ifndef jit_extr_c_l
#define jit_extr_c_l(d, rs)		(jit_lshi_l((d), (rs), 56), jit_rshi_l((d), (d), 56))
#endif
#ifndef jit_extr_s_l
#define jit_extr_s_l(d, rs)		(jit_lshi_l((d), (rs), 48), jit_rshi_l((d), (d), 48))
#endif
#ifndef jit_extr_i_l
#define jit_extr_i_l(d, rs)		(jit_lshi_l((d), (rs), 32), jit_rshi_l((d), (d), 32))
#endif
#ifndef jit_extr_c_ul
#define jit_extr_c_ul(d, rs)		jit_andi_l((d), (rs), 0xFF)
#endif
#ifndef jit_extr_s_ul
#define jit_extr_s_ul(d, rs)		jit_andi_l((d), (rs), 0xFFFF)
#endif
#ifndef jit_extr_i_ul
#define jit_extr_i_ul(d, rs)		jit_andi_l((d), (rs), 0xFFFFFFFFUL)
#endif
#endif

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


/* NTOH/HTON is not mandatory for big endian architectures */
#ifndef jit_ntoh_ui /* big endian */
#define jit_ntoh_ui(d, rs)		((d) == (rs) ? (void)0 : jit_movr_i((d), (rs)))
#define jit_ntoh_us(d, rs)		((d) == (rs) ? (void)0 : jit_movr_i((d), (rs)))
#endif /* big endian */

/* hton is a synonym for ntoh */
#define jit_hton_ui(d, rs)		jit_ntoh_ui((d), (rs))
#define jit_hton_us(d, rs)		jit_ntoh_us((d), (rs))

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
#define jit_normal_pushonlyarg_p(rs)    jit_pusharg_ul(rs)

/* Memory synonyms */

#ifdef JIT_RZERO
#ifndef jit_ldi_c
#define jit_ldi_c(rd, is)		jit_ldxi_c((rd), JIT_RZERO, (is))		
#define jit_sti_c(id, rs)		jit_stxi_c((id), JIT_RZERO, (rs))		
#define jit_ldi_s(rd, is)		jit_ldxi_s((rd), JIT_RZERO, (is))		
#define jit_sti_s(id, rs)		jit_stxi_s((id), JIT_RZERO, (rs))		
#define jit_ldi_i(rd, is)		jit_ldxi_i((rd), JIT_RZERO, (is))		
#define jit_sti_i(id, rs)		jit_stxi_i((id), JIT_RZERO, (rs))		
#define jit_ldi_l(rd, is)		jit_ldxi_l((rd), JIT_RZERO, (is))		
#define jit_sti_l(id, rs)		jit_stxi_l((id), JIT_RZERO, (rs))		
#define jit_ldi_uc(rd, is)		jit_ldxi_uc((rd), JIT_RZERO, (is))		
#define jit_ldi_us(rd, is)		jit_ldxi_us((rd), JIT_RZERO, (is))		
#define jit_ldi_ui(rd, is)		jit_ldxi_ui((rd), JIT_RZERO, (is))		
#define jit_ldi_ul(rd, is)		jit_ldxi_ul((rd), JIT_RZERO, (is))		
#endif

#ifndef jit_ldr_c
#define jit_ldr_c(rd, rs)		jit_ldxr_c((rd), JIT_RZERO, (rs))		
#define jit_str_c(rd, rs)		jit_stxr_c(JIT_RZERO, (rd), (rs))		
#define jit_ldr_s(rd, rs)		jit_ldxr_s((rd), JIT_RZERO, (rs))		
#define jit_str_s(rd, rs)		jit_stxr_s(JIT_RZERO, (rd), (rs))		
#define jit_ldr_i(rd, rs)		jit_ldxr_i((rd), JIT_RZERO, (rs))		
#define jit_str_i(rd, rs)		jit_stxr_i(JIT_RZERO, (rd), (rs))		
#define jit_ldr_l(rd, rs)		jit_ldxr_l((rd), JIT_RZERO, (rs))		
#define jit_str_l(rd, rs)		jit_stxr_l(JIT_RZERO, (rd), (rs))		
#define jit_ldr_uc(rd, rs)		jit_ldxr_uc((rd), JIT_RZERO, (rs))		
#define jit_ldr_us(rd, rs)		jit_ldxr_us((rd), JIT_RZERO, (rs))		
#define jit_ldr_ui(rd, rs)		jit_ldxr_ui((rd), JIT_RZERO, (rs))		
#define jit_ldr_ul(rd, rs)		jit_ldxr_ul((rd), JIT_RZERO, (rs))		
#endif
#endif

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


/* Boolean & branch synonyms */
#define jit_eqr_ui(d, s1, s2)		jit_eqr_i((d), (s1), (s2))
#define jit_eqi_ui(d, rs, is)		jit_eqi_i((d), (rs), (is))
#define jit_ner_ui(d, s1, s2)		jit_ner_i((d), (s1), (s2))
#define jit_nei_ui(d, rs, is)		jit_nei_i((d), (rs), (is))

#define jit_eqr_ul(d, s1, s2)		jit_eqr_l((d), (s1), (s2))
#define jit_eqi_ul(d, rs, is)		jit_eqi_l((d), (rs), (is))
#define jit_ner_ul(d, s1, s2)		jit_ner_l((d), (s1), (s2))
#define jit_nei_ul(d, rs, is)		jit_nei_l((d), (rs), (is))

#define jit_beqr_ui(label, s1, s2)	jit_beqr_i((label), (s1), (s2))
#define jit_beqi_ui(label, rs, is)	jit_beqi_i((label), (rs), (is))
#define jit_bner_ui(label, s1, s2)	jit_bner_i((label), (s1), (s2))
#define jit_bnei_ui(label, rs, is)	jit_bnei_i((label), (rs), (is))
#define jit_bmcr_ui(label, s1, s2)	jit_bmcr_i((label), (s1), (s2))
#define jit_bmci_ui(label, rs, is)	jit_bmci_i((label), (rs), (is))
#define jit_bmsr_ui(label, s1, s2)	jit_bmsr_i((label), (s1), (s2))
#define jit_bmsi_ui(label, rs, is)	jit_bmsi_i((label), (rs), (is))

#define jit_beqr_ul(label, s1, s2)	jit_beqr_l((label), (s1), (s2))
#define jit_beqi_ul(label, rs, is)	jit_beqi_l((label), (rs), (is))
#define jit_bner_ul(label, s1, s2)	jit_bner_l((label), (s1), (s2))
#define jit_bnei_ul(label, rs, is)	jit_bnei_l((label), (rs), (is))
#define jit_bmcr_ul(label, s1, s2)	jit_bmcr_l((label), (s1), (s2))
#define jit_bmci_ul(label, rs, is)	jit_bmci_l((label), (rs), (is))
#define jit_bmsr_ul(label, s1, s2)	jit_bmsr_l((label), (s1), (s2))
#define jit_bmsi_ul(label, rs, is)	jit_bmsi_l((label), (rs), (is))

#define jit_ltr_p(d, s1, s2)		jit_ltr_ul((d), (s1), (s2))
#define jit_lti_p(d, rs, is)		jit_lti_ul((d), (rs), (is))
#define jit_ler_p(d, s1, s2)		jit_ler_ul((d), (s1), (s2))
#define jit_lei_p(d, rs, is)		jit_lei_ul((d), (rs), (is))
#define jit_gtr_p(d, s1, s2)		jit_gtr_ul((d), (s1), (s2))
#define jit_gti_p(d, rs, is)		jit_gti_ul((d), (rs), (is))
#define jit_ger_p(d, s1, s2)		jit_ger_ul((d), (s1), (s2))
#define jit_gei_p(d, rs, is)		jit_gei_ul((d), (rs), (is))
#define jit_eqr_p(d, s1, s2)		jit_eqr_ul((d), (s1), (s2))
#define jit_eqi_p(d, rs, is)		jit_eqi_ul((d), (rs), (is))
#define jit_ner_p(d, s1, s2)		jit_ner_ul((d), (s1), (s2))
#define jit_nei_p(d, rs, is)		jit_nei_ul((d), (rs), (is))

#define jit_bltr_p(label, s1, s2)	jit_bltr_ul((label), (s1), (s2))
#define jit_blti_p(label, rs, is)	jit_blti_ul((label), (rs), (is))
#define jit_bler_p(label, s1, s2)	jit_bler_ul((label), (s1), (s2))
#define jit_blei_p(label, rs, is)	jit_blei_ul((label), (rs), (is))
#define jit_bgtr_p(label, s1, s2)	jit_bgtr_ul((label), (s1), (s2))
#define jit_bgti_p(label, rs, is)	jit_bgti_ul((label), (rs), (is))
#define jit_bger_p(label, s1, s2)	jit_bger_ul((label), (s1), (s2))
#define jit_bgei_p(label, rs, is)	jit_bgei_ul((label), (rs), (is))
#define jit_beqr_p(label, s1, s2)	jit_beqr_ul((label), (s1), (s2))
#define jit_beqi_p(label, rs, is)	jit_beqi_ul((label), (rs), (is))
#define jit_bner_p(label, s1, s2)	jit_bner_ul((label), (s1), (s2))
#define jit_bnei_p(label, rs, is)	jit_bnei_ul((label), (rs), (is))

#define jit_retval_ui(rd)		jit_retval_i((rd))
#define jit_retval_uc(rd)		jit_retval_i((rd))
#define jit_retval_us(rd)		jit_retval_i((rd))
#define jit_retval_ul(rd)		jit_retval_l((rd))
#define jit_retval_p(rd)		jit_retval_ul((rd))
#define jit_retval_c(rd)		jit_retval_i((rd))
#define jit_retval_s(rd)		jit_retval_i((rd))

/* This was a bug, but we keep it.  */
#define jit_retval(rd)			jit_retval_i ((rd))

#ifndef jit_finish
#define jit_finish(sub)			jit_calli(sub)
#endif

#define jit_normal_finish(sub)          jit_finish(sub)

#ifndef jit_finishr
#define jit_finishr(reg)		jit_callr(reg)
#endif

#ifndef jit_prolog
#define jit_prolog(numargs)
#endif

#ifndef jit_leaf
#define jit_leaf(numargs)		jit_prolog(numargs)
#endif

#ifndef jit_getarg_c
#define jit_getarg_c(reg, ofs)		jit_extr_c_i  ((reg), (ofs))
#define jit_getarg_i(reg, ofs)		jit_movr_i    ((reg), (ofs))
#define jit_getarg_l(reg, ofs)		jit_movr_l    ((reg), (ofs))
#define jit_getarg_p(reg, ofs)		jit_movr_p    ((reg), (ofs))
#define jit_getarg_s(reg, ofs)		jit_extr_s_i  ((reg), (ofs))
#define jit_getarg_uc(reg, ofs)		jit_extr_uc_ui((reg), (ofs))
#define jit_getarg_ui(reg, ofs)		jit_movr_ui   ((reg), (ofs))
#define jit_getarg_ul(reg, ofs)		jit_extr_uc_ul((reg), (ofs))
#define jit_getarg_us(reg, ofs)		jit_extr_us_ul((reg), (ofs))
#endif


/* Common definitions when sizeof(long) = sizeof(int) */
#ifndef jit_addi_l
#define JIT_LONG_IS_INT

/* ALU */
#define jit_addi_l(d, rs, is)		jit_addi_i((d), (rs), (is))	
#define jit_addr_l(d, s1, s2)		jit_addr_i((d), (s1), (s2))
#define jit_addci_l(d, rs, is)		jit_addci_i((d), (rs), (is))	
#define jit_addcr_l(d, s1, s2)		jit_addcr_i((d), (s1), (s2))
#define jit_addxi_l(d, rs, is)		jit_addxi_i((d), (rs), (is))	
#define jit_addxr_l(d, s1, s2)		jit_addxr_i((d), (s1), (s2))
#define jit_andi_l(d, rs, is)		jit_andi_i((d), (rs), (is))	
#define jit_andr_l(d, s1, s2)		jit_andr_i((d), (s1), (s2))
#define jit_divi_l(d, rs, is)		jit_divi_i((d), (rs), (is))	
#define jit_divr_l(d, s1, s2)		jit_divr_i((d), (s1), (s2))
#define jit_hmuli_l(d, rs, is)		jit_hmuli_i((d), (rs), (is))	
#define jit_hmulr_l(d, s1, s2)		jit_hmulr_i((d), (s1), (s2))
#define jit_lshi_l(d, rs, is)		jit_lshi_i((d), (rs), (is))	
#define jit_lshr_l(d, s1, s2)		jit_lshr_i((d), (s1), (s2))
#define jit_modi_l(d, rs, is)		jit_modi_i((d), (rs), (is))	
#define jit_modr_l(d, s1, s2)		jit_modr_i((d), (s1), (s2))
#define jit_muli_l(d, rs, is)		jit_muli_i((d), (rs), (is))	
#define jit_mulr_l(d, s1, s2)		jit_mulr_i((d), (s1), (s2))
#define jit_ori_l(d, rs, is)		jit_ori_i((d), (rs), (is))	
#define jit_orr_l(d, s1, s2)		jit_orr_i((d), (s1), (s2))
#define jit_rshi_l(d, rs, is)		jit_rshi_i((d), (rs), (is))	
#define jit_rshr_l(d, s1, s2)		jit_rshr_i((d), (s1), (s2))
#define jit_subr_l(d, s1, s2)		jit_subr_i((d), (s1), (s2))
#define jit_subcr_l(d, s1, s2)		jit_subcr_i((d), (s1), (s2))
#define jit_subxi_l(d, rs, is)		jit_subxi_i((d), (rs), (is))	
#define jit_subxr_l(d, s1, s2)		jit_subxr_i((d), (s1), (s2))
#define jit_xori_l(d, rs, is)		jit_xori_i((d), (rs), (is))	
#define jit_xorr_l(d, s1, s2)		jit_xorr_i((d), (s1), (s2))

#ifndef jit_rsbi_l
#define jit_rsbi_l(d, rs, is)		jit_rsbi_i((d), (rs), (is))	
#endif

#define jit_divi_ul(d, rs, is)		jit_divi_ui((d), (rs), (is))	
#define jit_divr_ul(d, s1, s2)		jit_divr_ui((d), (s1), (s2))
#define jit_hmuli_ul(d, rs, is)		jit_hmuli_ui((d), (rs), (is))	
#define jit_hmulr_ul(d, s1, s2)		jit_hmulr_ui((d), (s1), (s2))
#define jit_modi_ul(d, rs, is)		jit_modi_ui((d), (rs), (is))	
#define jit_modr_ul(d, s1, s2)		jit_modr_ui((d), (s1), (s2))
#define jit_muli_ul(d, rs, is)		jit_muli_ui((d), (rs), (is))	
#define jit_mulr_ul(d, s1, s2)		jit_mulr_ui((d), (s1), (s2))
#define jit_rshi_ul(d, rs, is)		jit_rshi_ui((d), (rs), (is))	
#define jit_rshr_ul(d, s1, s2)		jit_rshr_ui((d), (s1), (s2))

/* Sign/Zero extension */
#define jit_extr_c_l(d, rs)		jit_extr_c_i(d, rs)
#define jit_extr_c_ul(d, rs)		jit_extr_c_ui(d, rs)
#define jit_extr_s_l(d, rs)		jit_extr_s_i(d, rs)
#define jit_extr_s_ul(d, rs)		jit_extr_s_ui(d, rs)
#define jit_extr_i_l(d, rs)		jit_movr_i(d, rs)
#define jit_extr_i_ul(d, rs)		jit_movr_i(d, rs)

/* Unary */
#define jit_movi_l(d, rs)		jit_movi_i((d), (rs))
#define jit_movr_l(d, rs)		jit_movr_i((d), (rs))

/* Stack */
#define jit_pushr_l(rs)			jit_pushr_i(rs)
#define jit_popr_l(rs)			jit_popr_i(rs)		
#define jit_pusharg_l(rs)		jit_pusharg_i(rs)

/* Memory */
#ifndef JIT_RZERO
#define jit_ldr_l(d, rs)		jit_ldr_i((d), (rs))
#define jit_ldi_l(d, is)		jit_ldi_i((d), (is))
#define jit_str_l(d, rs)		jit_str_i((d), (rs))
#define jit_sti_l(d, is)		jit_sti_i((d), (is))
#define jit_ldr_ui(d, rs)		jit_ldr_i((d), (rs))
#define jit_ldi_ui(d, is)		jit_ldi_i((d), (is))
#define jit_ldr_ul(d, rs)		jit_ldr_ui((d), (rs))
#define jit_ldi_ul(d, is)		jit_ldi_ui((d), (is))
#endif

#define jit_ldxr_l(d, s1, s2)		jit_ldxr_i((d), (s1), (s2))
#define jit_ldxi_l(d, rs, is)		jit_ldxi_i((d), (rs), (is))
#define jit_stxr_l(d, s1, s2)		jit_stxr_i((d), (s1), (s2))
#define jit_stxi_l(d, rs, is)		jit_stxi_i((d), (rs), (is))
#define jit_ldxr_ui(d, s1, s2)		jit_ldxr_i((d), (s1), (s2))
#define jit_ldxi_ui(d, rs, is)		jit_ldxi_i((d), (rs), (is))
#define jit_ldxr_ul(d, s1, s2)		jit_ldxr_ui((d), (s1), (s2))
#define jit_ldxi_ul(d, rs, is)		jit_ldxi_ui((d), (rs), (is))


/* Boolean */
#define jit_ltr_l(d, s1, s2)		jit_ltr_i((d), (s1), (s2))
#define jit_lti_l(d, rs, is)		jit_lti_i((d), (rs), (is))
#define jit_ler_l(d, s1, s2)		jit_ler_i((d), (s1), (s2))
#define jit_lei_l(d, rs, is)		jit_lei_i((d), (rs), (is))
#define jit_gtr_l(d, s1, s2)		jit_gtr_i((d), (s1), (s2))
#define jit_gti_l(d, rs, is)		jit_gti_i((d), (rs), (is))
#define jit_ger_l(d, s1, s2)		jit_ger_i((d), (s1), (s2))
#define jit_gei_l(d, rs, is)		jit_gei_i((d), (rs), (is))
#define jit_eqr_l(d, s1, s2)		jit_eqr_i((d), (s1), (s2))
#define jit_eqi_l(d, rs, is)		jit_eqi_i((d), (rs), (is))
#define jit_ner_l(d, s1, s2)		jit_ner_i((d), (s1), (s2))
#define jit_nei_l(d, rs, is)		jit_nei_i((d), (rs), (is))
#define jit_ltr_ul(d, s1, s2)		jit_ltr_ui((d), (s1), (s2))
#define jit_lti_ul(d, rs, is)		jit_lti_ui((d), (rs), (is))
#define jit_ler_ul(d, s1, s2)		jit_ler_ui((d), (s1), (s2))
#define jit_lei_ul(d, rs, is)		jit_lei_ui((d), (rs), (is))
#define jit_gtr_ul(d, s1, s2)		jit_gtr_ui((d), (s1), (s2))
#define jit_gti_ul(d, rs, is)		jit_gti_ui((d), (rs), (is))
#define jit_ger_ul(d, s1, s2)		jit_ger_ui((d), (s1), (s2))
#define jit_gei_ul(d, rs, is)		jit_gei_ui((d), (rs), (is))

/* Branches */
#define jit_bltr_l(label, s1, s2)	jit_bltr_i((label), (s1), (s2))
#define jit_blti_l(label, rs, is)	jit_blti_i((label), (rs), (is))
#define jit_bler_l(label, s1, s2)	jit_bler_i((label), (s1), (s2))
#define jit_blei_l(label, rs, is)	jit_blei_i((label), (rs), (is))
#define jit_bgtr_l(label, s1, s2)	jit_bgtr_i((label), (s1), (s2))
#define jit_bgti_l(label, rs, is)	jit_bgti_i((label), (rs), (is))
#define jit_bger_l(label, s1, s2)	jit_bger_i((label), (s1), (s2))
#define jit_bgei_l(label, rs, is)	jit_bgei_i((label), (rs), (is))
#define jit_beqr_l(label, s1, s2)	jit_beqr_i((label), (s1), (s2))
#define jit_beqi_l(label, rs, is)	jit_beqi_i((label), (rs), (is))
#define jit_bner_l(label, s1, s2)	jit_bner_i((label), (s1), (s2))
#define jit_bnei_l(label, rs, is)	jit_bnei_i((label), (rs), (is))
#define jit_bmcr_l(label, s1, s2)	jit_bmcr_i((label), (s1), (s2))
#define jit_bmci_l(label, rs, is)	jit_bmci_i((label), (rs), (is))
#define jit_bmsr_l(label, s1, s2)	jit_bmsr_i((label), (s1), (s2))
#define jit_bmsi_l(label, rs, is)	jit_bmsi_i((label), (rs), (is))
#define jit_boaddr_l(label, s1, s2)	jit_boaddr_i((label), (s1), (s2))
#define jit_boaddi_l(label, rs, is)	jit_boaddi_i((label), (rs), (is))
#define jit_bosubr_l(label, s1, s2)	jit_bosubr_i((label), (s1), (s2))
#define jit_bosubi_l(label, rs, is)	jit_bosubi_i((label), (rs), (is))
#define jit_bomulr_l(label, s1, s2)	jit_bomulr_i((label), (s1), (s2))
#define jit_bltr_ul(label, s1, s2)	jit_bltr_ui((label), (s1), (s2))
#define jit_blti_ul(label, rs, is)	jit_blti_ui((label), (rs), (is))
#define jit_bler_ul(label, s1, s2)	jit_bler_ui((label), (s1), (s2))
#define jit_blei_ul(label, rs, is)	jit_blei_ui((label), (rs), (is))
#define jit_bgtr_ul(label, s1, s2)	jit_bgtr_ui((label), (s1), (s2))
#define jit_bgti_ul(label, rs, is)	jit_bgti_ui((label), (rs), (is))
#define jit_bger_ul(label, s1, s2)	jit_bger_ui((label), (s1), (s2))
#define jit_bgei_ul(label, rs, is)	jit_bgei_ui((label), (rs), (is))
#define jit_boaddr_ul(label, s1, s2)	jit_boaddr_ui((label), (s1), (s2))
#define jit_boaddi_ul(label, rs, is)	jit_boaddi_ui((label), (rs), (is))
#define jit_bosubr_ul(label, s1, s2)	jit_bosubr_ui((label), (s1), (s2))
#define jit_bosubi_ul(label, rs, is)	jit_bosubi_ui((label), (rs), (is))

#define jit_retval_l(rd)		jit_retval_i((rd))

#endif

#endif /* __lightning_core_common_h_ */
