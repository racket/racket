/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer (i386 version)
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



#ifndef __lightning_core_h
#define __lightning_core_h

#define JIT_FP			_EBP
#define JIT_SP			_ESP
#define JIT_RET			_EAX

#define JIT_R_NUM		3
#define JIT_V_NUM		3
#define JIT_R(i)		(_EAX + (i))
#define JIT_V(i)		((i) == 0 ? _EBX : _ESI + (i) - 1)

struct jit_local_state {
#ifdef JIT_X86_64
  int   long_jumps;
  int   nextarg_geti;
#else
  int	framesize;
#endif
  int	argssize;
};

/* 3-parameter operation */
#define jit_opr_(d, s1, s2, op1d, op2d)					\
	( (s2 == d) ? op1d :						\
	  (  ((s1 == d) ? (void)0 : (void)MOVLrr(s1, d)), op2d )	\
	)
#define jit_qopr_(d, s1, s2, op1d, op2d)				\
	( (s2 == d) ? op1d :						\
	  (  ((s1 == d) ? (void)0 : (void)MOVQrr(s1, d)), op2d )	\
	)

/* 3-parameter operation, with immediate */
#define jit_op_(d, s1, op2d)				\
	((s1 == d) ? op2d : (MOVLrr(s1, d), op2d))
#define jit_qop_(d, s1, op2d)				\
	((s1 == d) ? op2d : (MOVQrr(s1, d), op2d))

/* 3-parameter operation, optimizable */
#define jit_opo_(d, s1, s2, op1d, op2d, op12d)		\
	((s2 == d) ? op2d : 				\
	((s1 == d) ? op1d : op12d))

/* 3-parameter operation, optimizable, with immediate */
#define jit_opi_(d, rs, opdi, opdri)			\
	((rs == d) ? opdi : opdri)

/* An operand is forced into a register */
#define jit_replace(rd, rs, forced, op)					\
	((rd == forced) ? JITSORRY("Register conflict for " # op) :	\
	 (rs == forced) ? op : (PUSHLr(forced), MOVLrr(rs, forced), op, POPLr(forced)))

/* For LT, LE, ... */
#define jit_replace8(d, op)						\
	(jit_check8(d)							\
	  ? (MOVLir(0, d), op(d))					\
	  : (PUSHLr(_EAX), MOVLir(0, _EAX), op(_EAX), MOVLrr(_EAX, (d)), POPLr(_EAX)))

#define jit_bool_r(d, s1, s2, op)					\
	(CMPLrr(s2, s1), jit_replace8(d, op))

#define jit_bool_i(d, rs, is, op)					\
	(CMPLir(is, rs), jit_replace8(d, op))

/* When CMP with 0 can be replaced with TEST */
#define jit_bool_i0(d, rs, is, op, op0)					\
	((is) != 0							\
	  ? (CMPLir(is, rs), jit_replace8(d, op)) 			\
	  : (TESTLrr(rs, rs), jit_replace8(d, op0)))

/* For BLT, BLE, ... */
#define jit_bra_r(s1, s2, op)		(CMPLrr(s2, s1), op, _jit.x.pc)
#define jit_bra_qr(s1, s2, op)		(CMPQrr(s2, s1), op, _jit.x.pc)
#define jit_bra_i(rs, is, op)		(CMPLir(is, rs), op, _jit.x.pc)
#define _jit_bra_l(rs, is, op)		(CMPQir(is, rs), op, _jit.x.pc)

#ifdef JIT_X86_64
# define jit_bra_l(rs, is, op) (_s32P((long)(is)) \
                                ? _jit_bra_l(rs, is, op) \
                                : (MOVQir(is, JIT_REXTMP), jit_bra_qr(JIT_REXTMP, rs, op)))
#else
# define jit_bra_l(rs, is, op) _jit_bra_l(rs, is, op)
#endif

/* When CMP with 0 can be replaced with TEST */
#define jit_bra_i0(rs, is, op, op0)					\
	( (is) == 0 ? (TESTLrr(rs, rs), op0, _jit.x.pc) : (CMPLir(is, rs), op, _jit.x.pc))
#define jit_bra_l0(rs, is, op, op0)					\
	( (is) == 0 ? (TESTQrr(rs, rs), op0, _jit.x.pc) : jit_bra_l(rs, is, op))

/* Used to implement ldc, stc, ... */
#define jit_check8(rs)		( (rs) <= _EBX )
#define jit_reg8(rs)		( ((rs) == _SI || (rs) == _DI) ? _AL : ((rs) & _BH) | _AL )
#define jit_reg16(rs)		( ((rs) & _BH) | _AX )

/* In jit_replace below, _EBX is dummy */
#define jit_movbrm(rs, dd, db, di, ds)                                                \
	(jit_check8(rs)                                                         \
	        ? MOVBrm(jit_reg8(rs), dd, db, di, ds)                          \
	        : jit_replace(_EBX, rs, _EAX, MOVBrm(_AL, dd, db, di, ds)))

/* Reduce arguments of XOR/OR/TEST */
#ifdef JIT_X86_64
# define JIT_CAN_16 0
#else
# define JIT_CAN_16 1
#endif
#define jit_reduce_(op)	op
#define jit_reduce(op, is, rs)							\
	(_u8P(is) && jit_check8(rs) ? jit_reduce_(op##Bir(is, jit_reg8(rs))) :	\
	(_u16P(is) && JIT_CAN_16 ? jit_reduce_(op##Wir(is, jit_reg16(rs))) :			\
	jit_reduce_(op##Lir(is, rs)) ))
#define jit_reduceQ(op, is, rs)							\
	(_u8P(is) && jit_check8(rs) ? jit_reduce_(op##Bir(is, jit_reg8(rs))) :	\
	jit_reduce_(op##Qir(is, rs)) )

/* Helper macros for MUL/DIV/IDIV */
#define jit_might(d, s1, op)					\
	((s1 == d) ? 0 : op)

#define jit_mulr_ui_(s1, s2)	jit_opr_(_EAX, s1, s2, MULLr(s1),  MULLr(s2))
#define jit_mulr_i_(s1, s2)	jit_opr_(_EAX, s1, s2, IMULLr(s1), IMULLr(s2))


#define jit_muli_i_(is, rs)				\
	(MOVLir(is, rs == _EAX ? _EDX : _EAX),		\
	 IMULLr(rs == _EAX ? _EDX : rs))

#define jit_muli_ui_(is, rs)				\
	(MOVLir(is, rs == _EAX ? _EDX : _EAX),		\
	 IMULLr(rs == _EAX ? _EDX : rs))

#define jit_divi_i_(result, d, rs, is)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	jit_might (rs,   _EAX, MOVLrr(rs, _EAX)),	\
	jit_might (rs,   _EDX, MOVLrr(rs, _EDX)),	\
	MOVLir(is, _ECX),				\
	SARLir(31, _EDX),				\
	IDIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))

#define jit_divr_i_(result, d, s1, s2)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	((s1 == _ECX) ? PUSHLr(_ECX) : 0),		\
	jit_might (s2,   _ECX, MOVLrr(s2, _ECX)),	\
	((s1 == _ECX) ? POPLr(_EDX) :			\
	jit_might (s1,   _EDX, MOVLrr(s1, _EDX))),	\
	MOVLrr(_EDX, _EAX),				\
	SARLir(31, _EDX),				\
	IDIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))

#define jit_divi_ui_(result, d, rs, is)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	jit_might (rs,   _EAX, MOVLrr(rs, _EAX)),	\
	MOVLir(is, _ECX),				\
	XORLrr(_EDX, _EDX),				\
	DIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))

#define jit_divr_ui_(result, d, s1, s2)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	((s1 == _ECX) ? PUSHLr(_ECX) : 0),		\
	jit_might (s2,   _ECX, MOVLrr(s2, _ECX)),	\
	((s1 == _ECX) ? POPLr(_EAX) :			\
	jit_might (s1,   _EAX, MOVLrr(s1, _EAX))),	\
	XORLrr(_EDX, _EDX),				\
	DIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))


/* ALU */
#define jit_addi_i(d, rs, is)	jit_opi_((d), (rs),       ADDLir((is), (d)), 			LEALmr((is), (rs), 0, 0, (d))  )
#define jit_addr_i(d, s1, s2)	jit_opo_((d), (s1), (s2), ADDLrr((s2), (d)), ADDLrr((s1), (d)), LEALmr(0, (s1), (s2), 1, (d))  )
#define jit_addci_i(d, rs, is)	jit_op_ ((d), (rs),       ADDLir((is), (d)) 		       )
#define jit_addcr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), ADDLrr((s1), (d)), ADDLrr((s2), (d)) )
#define jit_addxi_i(d, rs, is)	jit_op_ ((d), (rs),       ADCLir((is), (d)) 		       )
#define jit_addxr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), ADCLrr((s1), (d)), ADCLrr((s2), (d)) )
#define jit_andi_i(d, rs, is)	jit_op_ ((d), (rs),       ANDLir((is), (d)) 		       )
#define jit_andr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), ANDLrr((s1), (d)), ANDLrr((s2), (d)) )
#define jit_orr_i(d, s1, s2)	jit_opr_((d), (s1), (s2),  ORLrr((s1), (d)),  ORLrr((s2), (d)) )
#define jit_subr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), (SUBLrr((s1), (d)), NEGLr(d)),	SUBLrr((s2), (d))	       )
#define jit_subcr_i(d, s1, s2)	jit_subr_i((d), (s1), (s2))
#define jit_subxr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), SBBLrr((s1), (d)), SBBLrr((s2), (d)) )
#define jit_subxi_i(d, rs, is)	jit_op_ ((d), (rs),       SBBLir((is), (d)) 		       )
#define jit_xorr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), XORLrr((s1), (d)), XORLrr((s2), (d)) )

#define jit_addi_l(d, rs, is)	jit_opi_((d), (rs),       ADDQir((is), (d)), 			LEAQmr((is), (rs), 0, 0, (d))  )
#define jit_addr_l(d, s1, s2)	jit_opo_((d), (s1), (s2), ADDQrr((s2), (d)), ADDQrr((s1), (d)), LEAQmr(0, (s1), (s2), 1, (d))  )
#define jit_andi_l(d, rs, is)	jit_qop_ ((d), (rs),       ANDQir((is), (d)) 		       )
#define jit_andr_l(d, s1, s2)	jit_qopr_((d), (s1), (s2), ANDQrr((s1), (d)), ANDQrr((s2), (d)) )
#define jit_orr_l(d, s1, s2)	jit_qopr_((d), (s1), (s2),  ORQrr((s1), (d)),  ORQrr((s2), (d)) )
#define jit_subr_l(d, s1, s2)	jit_qopr_((d), (s1), (s2), (SUBQrr((s1), (d)), NEGQr(d)),	SUBQrr((s2), (d))	       )
#define jit_xorr_l(d, s1, s2)	jit_qopr_((d), (s1), (s2), XORQrr((s1), (d)), XORQrr((s2), (d)) )

/* These can sometimes use byte or word versions! */
#define jit_ori_i(d, rs, is)	jit_op_ ((d), (rs),        jit_reduce(OR, (is), (d))	       )
#define jit_xori_i(d, rs, is)	jit_op_ ((d), (rs),        jit_reduce(XOR, (is), (d))	       )
#define jit_ori_l(d, rs, is)	jit_qop_ ((d), (rs),        jit_reduceQ(OR, (is), (d))	       )
#define jit_xori_l(d, rs, is)	jit_qop_ ((d), (rs),        jit_reduceQ(XOR, (is), (d))	       )

#define jit_muli_i(d, rs, is)	jit_op_ ((d), (rs),       IMULLir((is), (d)) 		       )
#define jit_mulr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), IMULLrr((s1), (d)), IMULLrr((s2), (d)) )

/* As far as low bits are concerned, signed and unsigned multiplies are
   exactly the same. */
#define jit_muli_ui(d, rs, is)	jit_op_ ((d), (rs),       IMULLir((is), (d)) 		       )
#define jit_mulr_ui(d, s1, s2)	jit_opr_((d), (s1), (s2), IMULLrr((s1), (d)), IMULLrr((s2), (d)) )

#define jit_hmuli_i(d, rs, is)														\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_muli_i_((is), (rs)), 				     POPLr(_EAX)		) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_muli_i_((is), (rs)), MOVLrr(_EDX, _EAX),	     POPLr(_EDX) ) :	\
	               (PUSHLr(_EDX), PUSHLr(_EAX), jit_muli_i_((is), (rs)), MOVLrr(_EDX, (d)), POPLr(_EAX), POPLr(_EDX) )))

#define jit_hmulr_i(d, s1, s2)													\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_mulr_i_((s1), (s2)), 			  POPLr(_EAX)		    ) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_mulr_i_((s1), (s2)), MOVLrr(_EDX, _EAX), 	       POPLr(_EDX)  ) :	\
	 	       (PUSHLr(_EDX), PUSHLr(_EAX), jit_mulr_i_((s1), (s2)), MOVLrr(_EDX, (d)),   POPLr(_EAX), POPLr(_EDX)  )))

#define jit_hmuli_ui(d, rs, is)														\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_muli_ui_((is), (rs)), 				      POPLr(_EAX)		) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_muli_ui_((is), (rs)), MOVLrr(_EDX, _EAX),	      POPLr(_EDX) ) :	\
	               (PUSHLr(_EDX), PUSHLr(_EAX), jit_muli_ui_((is), (rs)), MOVLrr(_EDX, (d)), POPLr(_EAX), POPLr(_EDX) )))

#define jit_hmulr_ui(d, s1, s2)													\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_mulr_ui_((s1), (s2)), 			  POPLr(_EAX)		    ) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_mulr_ui_((s1), (s2)), MOVLrr(_EDX, _EAX), 	       POPLr(_EDX)  ) :	\
	 	       (PUSHLr(_EDX), PUSHLr(_EAX), jit_mulr_ui_((s1), (s2)), MOVLrr(_EDX, (d)),  POPLr(_EAX), POPLr(_EDX)  )))

#define jit_divi_i(d, rs, is)	jit_divi_i_(_EAX, (d), (rs), (is))
#define jit_divi_ui(d, rs, is)	jit_divi_ui_(_EAX, (d), (rs), (is))
#define jit_modi_i(d, rs, is)	jit_divi_i_(_EDX, (d), (rs), (is))
#define jit_modi_ui(d, rs, is)	jit_divi_ui_(_EDX, (d), (rs), (is))
#define jit_divr_i(d, s1, s2)	jit_divr_i_(_EAX, (d), (s1), (s2))
#define jit_divr_ui(d, s1, s2)	jit_divr_ui_(_EAX, (d), (s1), (s2))
#define jit_modr_i(d, s1, s2)	jit_divr_i_(_EDX, (d), (s1), (s2))
#define jit_modr_ui(d, s1, s2)	jit_divr_ui_(_EDX, (d), (s1), (s2))


/* Shifts */
#define jit_lshi_i(d, rs, is)	((is) <= 3 ?   LEALmr(0, 0, (rs), 1 << (is), (d))   :   jit_op_ ((d), (rs), SHLLir((is), (d)) ))
#define jit_rshi_i(d, rs, is)								jit_op_ ((d), (rs), SARLir((is), (d))  )
#define jit_rshi_ui(d, rs, is)								jit_op_ ((d), (rs), SHRLir((is), (d))  )
#define jit_lshr_i(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_op_ ((d), (r1), SHLLrr(_CL,  (d)) ))
#define jit_rshr_i(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_op_ ((d), (r1), SARLrr(_CL,  (d)) ))
#define jit_rshr_ui(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_op_ ((d), (r1), SHRLrr(_CL,  (d)) ))

#define jit_lshi_l(d, rs, is)	((is) <= 3 ?   LEAQmr(0, 0, (rs), 1 << (is), (d))   :   jit_qop_ ((d), (rs), SHLQir((is), (d)) ))
#define jit_rshi_l(d, rs, is)								jit_qop_ ((d), (rs), SARQir((is), (d))  )
#define jit_rshi_ul(d, rs, is)								jit_qop_ ((d), (rs), SHRQir((is), (d))  )
#define jit_lshr_l(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_qop_ ((d), (r1), SHLQrr(_CL,  (d)) ))
#define jit_rshr_l(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_qop_ ((d), (r1), SARQrr(_CL,  (d)) ))
#define jit_rshr_ul(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_qop_ ((d), (r1), SHRQrr(_CL,  (d)) ))

/* Stack */
#define jit_pushr_i(rs)		PUSHLr(rs)
#define jit_popr_i(rs)		POPLr(rs)
#define jit_pushr_l(rs) jit_pushr_i(rs)
#define jit_popr_l(rs)  jit_popr_i(rs)

#ifdef JIT_X86_64
# define jit_base_prolog() (PUSHQr(_EBP), MOVQrr(_ESP, _EBP), PUSHQr(_EBX), PUSHQr(_R12), PUSHQr(_R13))
# define jit_prolog(n) (_jitl.nextarg_geti = 0, jit_base_prolog())
#else
# define jit_base_prolog() (PUSHLr(_EBP), MOVLrr(_ESP, _EBP), PUSHLr(_EBX), PUSHLr(_ESI), PUSHLr(_EDI))
# define jit_prolog(n) (_jitl.framesize = 8, jit_base_prolog())
#endif

/* The += allows for stack pollution */

#ifdef JIT_X86_64
/* Stack isn't used for arguments: */
# define jit_prepare_i(ni)	(_jitl.argssize = 0)
#else
# ifdef _CALL_DARWIN
  /* Stack must stay 16-byte aligned: */
#  define jit_prepare_i(ni)	(((ni & 0x3) \
                                  ? SUBLir(4 * ((((ni) + 3) & ~(0x3)) - (ni)), JIT_SP) \
                                  : (void)0), \
                                 _jitl.argssize += (((ni) + 3) & ~(0x3)))
# else
#  define jit_prepare_i(ni)	(_jitl.argssize += (ni))
# endif
#endif

#define jit_prepare_f(nf)	(_jitl.argssize += (nf))
#define jit_prepare_d(nd)	(_jitl.argssize += 2 * (nd))
#ifdef JIT_X86_64
# define jit_pusharg_i(rs)	(_jitl.argssize++, MOVQrr(rs, JIT_CALLTMPSTART + _jitl.argssize - 1))
# define jit_finish(sub)        (jit_shift_args(), (void)jit_calli((sub)), jit_restore_locals())
# define jit_reg_is_arg(reg) ((reg == _EDI) || (reg ==_ESI) || (reg == _EDX))
# define jit_finishr(reg)	((jit_reg_is_arg((reg)) ? MOVQrr(reg, JIT_REXTMP) : (void)0), \
                                 jit_shift_args(), \
                                 jit_reg_is_arg((reg)) ? CALQsr((JIT_REXTMP)) : jit_callr((reg)), \
                                 jit_restore_locals())
/* R12 and R13 are callee-save, instead of EDI and ESI */
# define jit_shift_args() \
   (MOVQrr(_ESI, _R12), MOVQrr(_EDI, _R13), \
   (_jitl.argssize--  \
    ? (MOVQrr(JIT_CALLTMPSTART + _jitl.argssize, jit_arg_reg_order[0]),  \
       (_jitl.argssize--  \
        ? (MOVQrr(JIT_CALLTMPSTART + _jitl.argssize, jit_arg_reg_order[1]),  \
           (_jitl.argssize--  \
            ? MOVQrr(JIT_CALLTMPSTART, jit_arg_reg_order[2])  \
            : (void)0)) \
        : (void)0)) \
    : (void)0))
# define jit_restore_locals() \
    (MOVQrr(_R12, _ESI), MOVQrr(_R13, _EDI))
#else
# define jit_pusharg_i(rs)	PUSHLr(rs)
# define jit_finish(sub)        ((void)jit_calli((sub)), ADDLir(sizeof(long) * _jitl.argssize, JIT_SP), _jitl.argssize = 0)
# define jit_finishr(reg)	(jit_callr((reg)), ADDLir(sizeof(long) * _jitl.argssize, JIT_SP), _jitl.argssize = 0)
#endif
#define jit_pusharg_l(rs) jit_pusharg_i(rs)
#define jit_retval_i(rd)	((void)jit_movr_i ((rd), _EAX))
#define jit_retval_l(rd)	((void)jit_movr_l ((rd), _EAX))

#ifdef JIT_X86_64
#define	jit_arg_i()	        (_jitl.nextarg_geti++)
#define	jit_arg_l()	        (_jitl.nextarg_geti++)
#define	jit_arg_p()	        (_jitl.nextarg_geti++)
#define jit_arg_reg(p)          (jit_arg_reg_order[p])
static int jit_arg_reg_order[] = { _EDI, _ESI, _EDX, _ECX };
#else
#define	jit_arg_c()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_uc()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_s()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_us()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_i()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_ui()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_l()		((_jitl.framesize += sizeof(long)) - sizeof(long))
#define	jit_arg_ul()		((_jitl.framesize += sizeof(long)) - sizeof(long))
#define	jit_arg_p()		((_jitl.framesize += sizeof(long)) - sizeof(long))
#endif

#define	jit_arg_f()		((_jitl.framesize += sizeof(float)) - sizeof(float))
#define	jit_arg_d()		((_jitl.framesize += sizeof(double)) - sizeof(double))

/* Unary */
#define jit_negr_i(d, rs)	jit_opi_((d), (rs), NEGLr(d), (XORLrr((d), (d)), SUBLrr((rs), (d))) )
#define jit_negr_l(d, rs)	jit_opi_((d), (rs), NEGQr(d), (XORQrr((d), (d)), SUBQrr((rs), (d))) )

#define jit_movr_i(d, rs)	((void)((rs) == (d) ? 0 : MOVLrr((rs), (d))))
#define jit_movi_i(d, is)	((is) ? MOVLir((is), (d)) : XORLrr ((d), (d)) )
#define jit_movr_l(d, rs)	((void)((rs) == (d) ? 0 : MOVQrr((rs), (d))))
#define jit_movi_l(d, is)	((is) \
                                 ? (_u32P((long)(is)) \
                                    ? MOVLir((is), (d)) \
                                    : MOVQir((is), (d))) \
                                 : XORLrr ((d), (d)) )
#define jit_movi_p(d, is)       jit_movi_l(d, ((long)(is)))
#define jit_patchable_movi_p(d, is) (MOVQir((is), (d)), _jit.x.pc)
#define jit_patch_movi(pa,pv)   (*_PSL((pa) - sizeof(long)) = _jit_SL((pv)))

#define jit_ntoh_ui(d, rs)	jit_op_((d), (rs), BSWAPLr(d))
#define jit_ntoh_us(d, rs)	jit_op_((d), (rs), RORWir(8, d))

/* Boolean */
#define jit_ltr_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETLr  )
#define jit_ler_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETLEr )
#define jit_gtr_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETGr  )
#define jit_ger_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETGEr )
#define jit_eqr_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETEr  )
#define jit_ner_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETNEr )
#define jit_ltr_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETBr  )
#define jit_ler_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETBEr )
#define jit_gtr_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETAr  )
#define jit_ger_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETAEr )

#define jit_lti_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETLr,  SETSr  )
#define jit_lei_i(d, rs, is)	jit_bool_i ((d), (rs), (is), SETLEr	    )
#define jit_gti_i(d, rs, is)	jit_bool_i ((d), (rs), (is), SETGr 	    )
#define jit_gei_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETGEr, SETNSr )
#define jit_eqi_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETEr,  SETEr  )
#define jit_nei_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETNEr, SETNEr )
#define jit_lti_ui(d, rs, is)	jit_bool_i ((d), (rs), (is), SETBr	    )
#define jit_lei_ui(d, rs, is)	jit_bool_i0((d), (rs), (is), SETBEr, SETEr  )
#define jit_gti_ui(d, rs, is)	jit_bool_i0((d), (rs), (is), SETAr,  SETNEr )
#define jit_gei_ui(d, rs, is)	jit_bool_i0((d), (rs), (is), SETAEr, INCLr  )

/* Jump */
#define jit_bltr_i(label, s1, s2)	jit_bra_r((s1), (s2), JLm(label, 0,0,0) )
#define jit_bler_i(label, s1, s2)	jit_bra_r((s1), (s2), JLEm(label,0,0,0) )
#define jit_bgtr_i(label, s1, s2)	jit_bra_r((s1), (s2), JGm(label, 0,0,0) )
#define jit_bger_i(label, s1, s2)	jit_bra_r((s1), (s2), JGEm(label,0,0,0) )
#define jit_beqr_i(label, s1, s2)	jit_bra_r((s1), (s2), JEm(label, 0,0,0) )
#define jit_bner_i(label, s1, s2)	jit_bra_r((s1), (s2), JNEm(label,0,0,0) )
#define jit_bltr_ui(label, s1, s2)	jit_bra_r((s1), (s2), JBm(label, 0,0,0) )
#define jit_bler_ui(label, s1, s2)	jit_bra_r((s1), (s2), JBEm(label,0,0,0) )
#define jit_bgtr_ui(label, s1, s2)	jit_bra_r((s1), (s2), JAm(label, 0,0,0) )
#define jit_bger_ui(label, s1, s2)	jit_bra_r((s1), (s2), JAEm(label,0,0,0) )
#define jit_bmsr_i(label, s1, s2)	(TESTLrr((s1), (s2)), JNZm(label,0,0,0), _jit.x.pc)
#define jit_bmcr_i(label, s1, s2)	(TESTLrr((s1), (s2)), JZm(label,0,0,0),  _jit.x.pc)
#define jit_boaddr_i(label, s1, s2)	(ADDLrr((s2), (s1)), JOm(label,0,0,0), _jit.x.pc)
#define jit_bosubr_i(label, s1, s2)	(SUBLrr((s2), (s1)), JOm(label,0,0,0), _jit.x.pc)
#define jit_boaddr_ui(label, s1, s2)	(ADDLrr((s2), (s1)), JCm(label,0,0,0), _jit.x.pc)
#define jit_bosubr_ui(label, s1, s2)	(SUBLrr((s2), (s1)), JCm(label,0,0,0), _jit.x.pc)

#define jit_bltr_l(label, s1, s2)	jit_bra_qr((s1), (s2), JLm(label, 0,0,0) )
#define jit_bler_l(label, s1, s2)	jit_bra_qr((s1), (s2), JLEm(label,0,0,0) )
#define jit_bgtr_l(label, s1, s2)	jit_bra_qr((s1), (s2), JGm(label, 0,0,0) )
#define jit_bger_l(label, s1, s2)	jit_bra_qr((s1), (s2), JGEm(label,0,0,0) )
#define jit_beqr_l(label, s1, s2)	jit_bra_qr((s1), (s2), JEm(label, 0,0,0) )
#define jit_bner_l(label, s1, s2)	jit_bra_qr((s1), (s2), JNEm(label,0,0,0) )
#define jit_bltr_ul(label, s1, s2)	jit_bra_qr((s1), (s2), JBm(label, 0,0,0) )
#define jit_bler_ul(label, s1, s2)	jit_bra_qr((s1), (s2), JBEm(label,0,0,0) )
#define jit_bgtr_ul(label, s1, s2)	jit_bra_qr((s1), (s2), JAm(label, 0,0,0) )
#define jit_bger_ul(label, s1, s2)	jit_bra_qr((s1), (s2), JAEm(label,0,0,0) )
#define jit_bmsr_l(label, s1, s2)	(TESTQrr((s1), (s2)), JNZm(label,0,0,0), _jit.x.pc)
#define jit_bmcr_l(label, s1, s2)	(TESTQrr((s1), (s2)), JZm(label,0,0,0),  _jit.x.pc)
#define jit_boaddr_l(label, s1, s2)	(ADDQrr((s2), (s1)), JOm(label,0,0,0), _jit.x.pc)
#define jit_bosubr_l(label, s1, s2)	(SUBQrr((s2), (s1)), JOm(label,0,0,0), _jit.x.pc)
#define jit_boaddr_ul(label, s1, s2)	(ADDQrr((s2), (s1)), JCm(label,0,0,0), _jit.x.pc)
#define jit_bosubr_ul(label, s1, s2)	(SUBQrr((s2), (s1)), JCm(label,0,0,0), _jit.x.pc)

#define jit_blti_i(label, rs, is)	jit_bra_i0((rs), (is), JLm(label, 0,0,0), JSm(label, 0,0,0) )
#define jit_blei_i(label, rs, is)	jit_bra_i ((rs), (is), JLEm(label,0,0,0)		    )
#define jit_bgti_i(label, rs, is)	jit_bra_i ((rs), (is), JGm(label, 0,0,0)		    )
#define jit_bgei_i(label, rs, is)	jit_bra_i0((rs), (is), JGEm(label,0,0,0), JNSm(label,0,0,0) )
#define jit_beqi_i(label, rs, is)	jit_bra_i0((rs), (is), JEm(label, 0,0,0), JEm(label, 0,0,0) )
#define jit_bnei_i(label, rs, is)	jit_bra_i0((rs), (is), JNEm(label,0,0,0), JNEm(label,0,0,0) )
#define jit_blti_ui(label, rs, is)	jit_bra_i ((rs), (is), JBm(label, 0,0,0)		    )
#define jit_blei_ui(label, rs, is)	jit_bra_i0((rs), (is), JBEm(label,0,0,0), JEm(label, 0,0,0) )
#define jit_bgti_ui(label, rs, is)	jit_bra_i0((rs), (is), JAm(label, 0,0,0), JNEm(label,0,0,0) )
#define jit_bgei_ui(label, rs, is)	jit_bra_i ((rs), (is), JAEm(label,0,0,0)		    )
#define jit_boaddi_i(label, rs, is)	(ADDLir((is), (rs)), JOm(label,0,0,0), _jit.x.pc)
#define jit_bosubi_i(label, rs, is)	(SUBLir((is), (rs)), JOm(label,0,0,0), _jit.x.pc)
#define jit_boaddi_ui(label, rs, is)	(ADDLir((is), (rs)), JCm(label,0,0,0), _jit.x.pc)
#define jit_bosubi_ui(label, rs, is)	(SUBLir((is), (rs)), JCm(label,0,0,0), _jit.x.pc)

#define jit_blti_l(label, rs, is)	jit_bra_l0((rs), (is), JLm(label, 0,0,0), JSm(label, 0,0,0) )
#define jit_blei_l(label, rs, is)	jit_bra_l ((rs), (is), JLEm(label,0,0,0)		    )
#define jit_bgti_l(label, rs, is)	jit_bra_l ((rs), (is), JGm(label, 0,0,0)		    )
#define jit_bgei_l(label, rs, is)	jit_bra_l0((rs), (is), JGEm(label,0,0,0), JNSm(label,0,0,0) )
#define jit_beqi_l(label, rs, is)	jit_bra_l0((rs), (is), JEm(label, 0,0,0), JEm(label, 0,0,0) )
#define jit_bnei_l(label, rs, is)	jit_bra_l0((rs), (is), JNEm(label,0,0,0), JNEm(label,0,0,0) )
#define jit_blti_ul(label, rs, is)	jit_bra_l ((rs), (is), JBm(label, 0,0,0)		    )
#define jit_blei_ul(label, rs, is)	jit_bra_l0((rs), (is), JBEm(label,0,0,0), JEm(label, 0,0,0) )
#define jit_bgti_ul(label, rs, is)	jit_bra_l0((rs), (is), JAm(label, 0,0,0), JNEm(label,0,0,0) )
#define jit_bgei_ul(label, rs, is)	jit_bra_l ((rs), (is), JAEm(label,0,0,0)		    )
#define jit_boaddi_l(label, rs, is)	(ADDQir((is), (rs)), JOm(label,0,0,0), _jit.x.pc)
#define jit_bosubi_l(label, rs, is)	(SUBQir((is), (rs)), JOm(label,0,0,0), _jit.x.pc)
#define jit_boaddi_ul(label, rs, is)	(ADDQir((is), (rs)), JCm(label,0,0,0), _jit.x.pc)
#define jit_bosubi_ul(label, rs, is)	(SUBQir((is), (rs)), JCm(label,0,0,0), _jit.x.pc)

#define jit_bmsi_i(label, rs, is)	(jit_reduce(TEST, (is), (rs)), JNZm(label,0,0,0), _jit.x.pc)
#define jit_bmci_i(label, rs, is)	(jit_reduce(TEST, (is), (rs)), JZm(label,0,0,0),  _jit.x.pc)

#define jit_bmsi_l(label, rs, is) jit_bmsi_i(label, rs, is)
#define jit_bmci_l(label, rs, is) jit_bmci_i(label, rs, is)

#define jit_jmpi(label)		(JMPm( ((unsigned long) (label)),	0, 0, 0), _jit.x.pc)
#define jit_calli(label)	(CALLm( ((unsigned long) (label)),	0, 0, 0), _jit.x.pc)
#define jit_callr(reg)		(CALLsr(reg))
#define jit_jmpr(reg)		JMPsr(reg)
#ifdef JIT_X86_64
#define jit_patch_long_at(jump_pc,v)  (*_PSL((jump_pc) - sizeof(long)) = _jit_SL((jit_insn *)(v)))
# define jit_patch_short_at(jump_pc,v)  (*_PSI((jump_pc) - sizeof(int)) = _jit_SI((jit_insn *)(v) - (jump_pc)))
# define jit_patch_branch_at(jump_pc,v) (_jitl.long_jumps ? jit_patch_long_at((jump_pc)-3, v) : jit_patch_short_at(jump_pc, v))
# define jit_patch_ucbranch_at(jump_pc,v) (_jitl.long_jumps ? jit_patch_long_at((jump_pc)-3, v) : jit_patch_short_at(jump_pc, v))
# define jit_ret() (POPQr(_R13), POPQr(_R12), POPQr(_EBX), POPQr(_EBP), RET_())
#else
#define jit_patch_long_at(jump_pc,v)  (*_PSL((jump_pc) - sizeof(long)) = _jit_SL((jit_insn *)(v) - (jump_pc)))
# define jit_patch_branch_at(jump_pc,v)  jit_patch_long_at(jump_pc, v)
# define jit_patch_ucbranch_at(jump_pc,v)  jit_patch_long_at(jump_pc, v)
# define jit_ret() (POPLr(_EDI), POPLr(_ESI), POPLr(_EBX), POPLr(_EBP), RET_())
#endif

/* Memory */
#define jit_ldi_c(d, is)		MOVSBLmr((is), 0,    0,    0, (d))
#define jit_ldr_c(d, rs)		MOVSBLmr(0,    (rs), 0,    0, (d))
#define jit_ldxr_c(d, s1, s2)		MOVSBLmr(0,    (s1), (s2), 1, (d))
#define jit_ldxi_c(d, rs, is)		MOVSBLmr((is), (rs), 0,    0, (d))

#define jit_ldi_uc(d, is)		MOVZBLmr((is), 0,    0,    0, (d))
#define jit_ldr_uc(d, rs)		MOVZBLmr(0,    (rs), 0,    0, (d))
#define jit_ldxr_uc(d, s1, s2)		MOVZBLmr(0,    (s1), (s2), 1, (d))
#define jit_ldxi_uc(d, rs, is)		MOVZBLmr((is), (rs), 0,    0, (d))

#define jit_sti_c(id, rs)               jit_movbrm((rs), (id), 0,    0,    0)
#define jit_str_c(rd, rs)               jit_movbrm((rs), 0,    (rd), 0,    0)
#define jit_stxr_c(d1, d2, rs)          jit_movbrm((rs), 0,    (d1), (d2), 1)
#define jit_stxi_c(id, rd, rs)          jit_movbrm((rs), (id), (rd), 0,    0)

#define jit_ldi_s(d, is)		MOVSWLmr((is), 0,    0,    0, (d))
#define jit_ldr_s(d, rs)		MOVSWLmr(0,    (rs), 0,    0, (d))
#define jit_ldxr_s(d, s1, s2)		MOVSWLmr(0,    (s1), (s2), 1, (d))
#define jit_ldxi_s(d, rs, is)		MOVSWLmr((is), (rs), 0,    0, (d))

#define jit_ldi_us(d, is)		MOVZWLmr((is), 0,    0,    0,  (d))
#define jit_ldr_us(d, rs)		MOVZWLmr(0,    (rs), 0,    0,  (d))
#define jit_ldxr_us(d, s1, s2)		MOVZWLmr(0,    (s1), (s2), 1,  (d))
#define jit_ldxi_us(d, rs, is)		MOVZWLmr((is), (rs), 0,    0,  (d))

#define jit_sti_s(id, rs)		MOVWrm(jit_reg16(rs), (id), 0,    0,    0)
#define jit_str_s(rd, rs)		MOVWrm(jit_reg16(rs), 0,    (rd), 0,    0)
#define jit_stxr_s(d1, d2, rs)		MOVWrm(jit_reg16(rs), 0,    (d1), (d2), 1)
#define jit_stxi_s(id, rd, rs)		MOVWrm(jit_reg16(rs), (id), (rd), 0,    0)

#define jit_ldi_i(d, is)		MOVLmr((is), 0,    0,    0,  (d))
#define jit_ldr_i(d, rs)		MOVLmr(0,    (rs), 0,    0,  (d))
#define jit_ldxr_i(d, s1, s2)		MOVLmr(0,    (s1), (s2), 1,  (d))
#define jit_ldxi_i(d, rs, is)		MOVLmr((is), (rs), 0,    0,  (d))

#define jit_sti_i(id, rs)		MOVLrm((rs), (id), 0,    0,    0)
#define jit_str_i(rd, rs)		MOVLrm((rs), 0,    (rd), 0,    0)
#define jit_stxr_i(d1, d2, rs)		MOVLrm((rs), 0,    (d1), (d2), 1)
#define jit_stxi_i(id, rd, rs)		MOVLrm((rs), (id), (rd), 0,    0)

#define _jit_ldi_l(d, is)		MOVQmr((is), 0,    0,    0,  (d))
#define jit_ldr_l(d, rs)		MOVQmr(0,    (rs), 0,    0,  (d))
#define jit_ldxr_l(d, s1, s2)		MOVQmr(0,    (s1), (s2), 1,  (d))
#define jit_ldxi_l(d, rs, is)		MOVQmr((is), (rs), 0,    0,  (d))

#define _jit_sti_l(id, rs)		MOVQrm((rs), (id), 0,    0,    0)
#define jit_str_l(rd, rs)		MOVQrm((rs), 0,    (rd), 0,    0)
#define jit_stxr_l(d1, d2, rs)		MOVQrm((rs), 0,    (d1), (d2), 1)
#define jit_stxi_l(id, rd, rs)		MOVQrm((rs), (id), (rd), 0,    0)

#ifdef JIT_X86_64
# define jit_ldi_l(d, is) (_u32P((long)(is)) ? _jit_ldi_l(d, is) : (jit_movi_l(d, is), jit_ldr_l(d, d)))
# define jit_sti_l(id, rs) (_u32P((long)(id)) ? _jit_sti_l(id, rs) : (jit_movi_l(JIT_REXTMP, id), MOVQrQm(rs, 0, JIT_REXTMP, 0, 0)))
#else
# define jit_ldi_l(d, is) _jit_ldi_l(d, is)
# define jit_sti_l(id, rs) _jit_sti_l(id, rs)
#endif


/* Extra */
#define jit_nop()			NOP_()

#define _jit_alignment(pc, n)		(((pc ^ _MASK(4)) + 1) & _MASK(n))
#define jit_align(n) 			NOPi(_jit_alignment(_jit_UL(_jit.x.pc), (n)))

#endif /* __lightning_core_h */
