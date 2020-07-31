/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler for the PowerPC
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 1999, 2000, 2001, 2002 Ian Piumarta
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

#ifndef __lightning_asm_h
#define __lightning_asm_h

/* <imm> = [0-9]+ | (.+)	-> add i, one parameter (imm)
 * <reg> = r<imm>		-> add r, one parameter (imm)
 * <mem> = <imm>(<reg>)		-> add m, two parameters (imm,reg)
 * <idx> = <reg>(<reg>)		-> add x, two parameters (reg,reg)
 *
 * `x' operands have two forms. For example `stwu source, rega(regb)'
 * could be written as either
 *	STWUrx(source, rega, regb)
 * or
 *	STWUXrrr(source, rega, regb)
 */



/*** a brief NOTE about halfwords and "shifted" operands
 *
 * LOGICAL insns require UNSIGNED args in 0..65535, whether or not shifted
 *
 * ARITHMETIC insns require SIGNED args in -32768..32767, even when shifted
 * 
 * as a special case: "lis/addis" also accepts UNSIGNED arguments in
 * 0..65535 since it is often used immediately before "ori" to load a 32-bit
 * constant (this is consistent with the GNU rs/6000 and PowerPC assemblers)
 * 
 * thus:	lis	rD, expression@hi
 *		ori	rD, rD, expression@lo	; load 32-bit constant
 */

typedef unsigned int jit_insn;

#ifndef LIGHTNING_DEBUG
#define _cr0	0
#define _cr1	1
#define _cr2	2
#define _cr3	3
#define _cr4	4
#define _cr5	5
#define _cr6	6
#define _cr7	7

#define _lt	0
#define _gt	1
#define _eq	2
#define _so	3
#define _un	3

#define _d16(D)		(_ck_d(16,(_jit_UL(D)-_jit_UL(_jit.x.pc))) & ~3)
#define _d26(D)		(_ck_d(26,(_jit_UL(D)-_jit_UL(_jit.x.pc))) & ~3)

/* primitive instruction forms [1, Section A.4] */

#define _FB(  OP,         BD,AA,LK )    (_jit_I_noinc((_u6(OP)<<26)|                                            _d26(BD)|     (_u1(AA)<<1)|_u1(LK)), _jit.x.pc++, 0)
#define _FBA( OP,         BD,AA,LK )	_jit_I((_u6(OP)<<26)|                                           (_u26(BD)&~3)| (_u1(AA)<<1)|_u1(LK))
#define _BB(   OP,BO,BI,   BD,AA,LK )   (_jit_I_noinc((_u6(OP)<<26)|(_u5(BO)<<21)|(_u5(BI)<<16)|                _d16(BD)|     (_u1(AA)<<1)|_u1(LK)), _jit.x.pc++, 0)
#define _D(   OP,RD,RA,         DD )  	_jit_I((_u6(OP)<<26)|(_u5(RD)<<21)|(_u5(RA)<<16)|                _s16(DD)                          )
#define _Du(  OP,RD,RA,         DD )  	_jit_I((_u6(OP)<<26)|(_u5(RD)<<21)|(_u5(RA)<<16)|                _u16(DD)                          )
#define _Ds(  OP,RD,RA,         DD )  	_jit_I((_u6(OP)<<26)|(_u5(RD)<<21)|(_u5(RA)<<16)|                _su16(DD)                         )
#define _X(   OP,RD,RA,RB,   XO,RC )  	_jit_I((_u6(OP)<<26)|(_u5(RD)<<21)|(_u5(RA)<<16)|( _u5(RB)<<11)|              (_u10(XO)<<1)|_u1(RC))
#define _XL(  OP,BO,BI,      XO,LK )  	_jit_I((_u6(OP)<<26)|(_u5(BO)<<21)|(_u5(BI)<<16)|( _u5(00)<<11)|              (_u10(XO)<<1)|_u1(LK))
#define _XFX( OP,RD,         SR,XO )  	_jit_I((_u6(OP)<<26)|(_u5(RD)<<21)|              (_u10(SR)<<11)|              (_u10(XO)<<1)|_u1(00))
#define _XO(  OP,RD,RA,RB,OE,XO,RC )  	_jit_I((_u6(OP)<<26)|(_u5(RD)<<21)|(_u5(RA)<<16)|( _u5(RB)<<11)|(_u1(OE)<<10)|( _u9(XO)<<1)|_u1(RC))
#define _M(   OP,RS,RA,SH,MB,ME,RC )  	_jit_I((_u6(OP)<<26)|(_u5(RS)<<21)|(_u5(RA)<<16)|( _u5(SH)<<11)|(_u5(MB)<< 6)|( _u5(ME)<<1)|_u1(RC))


/* special purpose registers (form XFX) [1, Section 8.2, page 8-138] */

#define SPR_LR		((8<<5)|(0))

/* +++ intrinsic instructions */

#define ADDrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 266, 0)
#define ADD_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 266, 1)
#define ADDCrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 10, 0)
#define ADDC_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 10, 1)
#define ADDErrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 138, 0)
#define ADDE_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 138, 1)
#define ADDOrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 266, 0)
#define ADDO_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 266, 1)
#define ADDIrri(RD, RA, IMM)		_D	(14, RD, RA, IMM)
#define ADDICrri(RD, RA, IMM)		_D	(12, RD, RA, IMM)
#define ADDIC_rri(RD, RA, IMM)		_D	(13, RD, RA, IMM)
#define ADDISrri(RD, RA, IMM)		_Ds	(15, RD, RA, IMM)

#define ANDrrr(RA, RS, RB)		_X	(31, RS, RA, RB,  28, 0)
#define AND_rrr(RA, RS, RB)		_X	(31, RS, RA, RB,  28, 1)
#define ANDCrrr(RA, RS, RB)		_X	(31, RS, RA, RB,  60, 0)
#define ANDC_rrr(RA, RS, RB)		_X	(31, RS, RA, RB,  60, 1)
#define ANDI_rri(RA, RS, IMM)		_Du	(28, RS, RA, IMM)
#define ANDIS_rri(RA, RS, IMM)		_Du	(29, RS, RA, IMM)

#define Bi(BD)				_FB	(18, BD, 0, 0)
#define BAi(BD)				_FBA	(18, BD, 1, 0)
#define BLi(BD)				_FB     (18, BD, 0, 1)
#define BLAi(BD)			_FBA	(18, BD, 1, 1)

#define BCiii(BO,BI,BD)			_BB	(16, BO, BI, BD, 0, 0)
#define BCAiii(BO,BI,BD)		_BB	(16, BO, BI, BD, 1, 0)
#define BCLiii(BO,BI,BD)		_BB	(16, BO, BI, BD, 0, 1)
#define BCLAiii(BO,BI,BD)		_BB	(16, BO, BI, BD, 1, 1)

#define BCCTRii(BO,BI)			_XL	(19, BO, BI, 528, 0)
#define BCCTRLii(BO,BI)			_XL	(19, BO, BI, 528, 1)

#define BCLRii(BO,BI)			_XL	(19, BO, BI,  16, 0)
#define BCLRLii(BO,BI)			_XL	(19, BO, BI,  16, 1)

#define CMPiirr(CR, LL, RA, RB)		_X	(31, ((CR)<<2)|(LL), RA, RB, 0, 0)
#define CMPIiiri(CR, LL, RA, IMM)	_D	(11, ((CR)<<2)|(LL), RA, IMM)

#define CMPLiirr(CR, LL, RA, RB)	_X	(31, ((CR)<<2)|(LL), RA, RB, 32, 0)
#define CMPLIiiri(CR, LL, RA, IMM)	_D	(10, ((CR)<<2)|(LL), RA, IMM)

#define CRANDiii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB, 257, 0)
#define CRANDCiii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB, 129, 0)
#define CREQViii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB, 289, 0)
#define CRNANDiii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB, 225, 0)
#define CRNORiii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB,  33, 0)
#define CRORiii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB, 449, 0)
#define CRORCiii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB, 417, 0)
#define CRXORiii(CRD,CRA,CRB)		_X	(19, CRD, CRA, CRB, 193, 0)

#define DCBSTrr(RA,RB)			_X	(31, 00, RA, RB,  54, 0)

#define DIVWrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 491, 0)
#define DIVW_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 491, 1)
#define DIVWOrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 491, 0)
#define DIVWO_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 491, 1)

#define DIVWUrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 459, 0)
#define DIVWU_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 459, 1)
#define DIVWUOrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 459, 0)
#define DIVWUO_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 459, 1)

#define EQVrrr(Ra,RS,RB)		_X	(31, RS, RA, RB, 284, 0)
#define EQV_rrr(Ra,RS,RB)		_X	(31, RS, RA, RB, 284, 1)

#define EXTSBrr(RA,RS)			_X	(31, RS, RA,  0, 954, 0)
#define EXTSB_rr(RA,RS)			_X	(31, RS, RA,  0, 954, 1)

#define EXTSHrr(RA,RS)			_X	(31, RS, RA,  0, 922, 0)
#define EXTSH_rr(RA,RS)			_X	(31, RS, RA,  0, 922, 1)

#define ICBIrr(RA,RB)			_X	(31, 00, RA, RB, 982, 0)

#define ISYNC()				_X	(19, 00, 00, 00, 150, 0)

#define LBZrm(RD,ID,RA)			_D	(34, RD, RA, ID)
#define LBZUrm(RD,ID,RA)		_D	(35, RD, RA, ID)
#define LBZUXrrr(RD,RA,RB)		_X	(31, RD, RA, RB, 119, 0)
#define LBZXrrr(RD,RA,RB)		_X	(31, RD, RA, RB,  87, 0)

#define LHArm(RD,ID,RA)			_D	(42, RD, RA, ID)
#define LHAUrm(RD,ID,RA)		_D	(43, RD, RA, ID)
#define LHAUXrrr(RD,RA,RB)		_X	(31, RD, RA, RB, 375, 0)
#define LHAXrrr(RD,RA,RB)		_X	(31, RD, RA, RB, 343, 0)
#define LHBRXrrr(RD,RA,RB)		_X	(31, RD, RA, RB, 790, 0)

#define LHZrm(RD,ID,RA)			_D	(40, RD, RA, ID)
#define LHZUrm(RD,ID,RA)		_D	(41, RD, RA, ID)
#define LHZUXrrr(RD,RA,RB)		_X	(31, RD, RA, RB, 311, 0)
#define LHZXrrr(RD,RA,RB)		_X	(31, RD, RA, RB, 279, 0)

#define LMWrm(RD,ID,RA)			_D	(46, RD, RA, ID)

#define LWBRXrrr(RD,RA,RB)		_X	(31, RD, RA, RB, 534, 0)

#define LWZrm(RD, DISP, RA)		_D	(32, RD, RA, DISP)
#define LWZUrm(RD, DISP, RA)		_D	(33, RD, RA, DISP)
#define LWZUXrrr(RD, RA, RB)		_X	(31, RD, RA, RB,  56, 0)
#define LWZXrrr(RD, RA, RB)		_X	(31, RD, RA, RB,  23, 0)

#define MCRFii(CD,CS)			_X	(19, ((CD)<<2), ((CS)<<2), 0, 0, 0)

#define MFCRr(RD)			_X	(31, RD, 0, 0, 19, 0)
#define MCRXRi(RD) 			_XFX	(31, (RD)<<2,   0, 512)

#define MFSPRri(RD, SPR)		_XFX	(31, RD, (SPR)<<5, 339)
#define MTSPRir(SPR, RS)		_XFX	(31, RS, (SPR)<<5, 467)

#define MULHWrrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 0,  75, 0)
#define MULHW_rrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 0,  75, 1)
#define MULHWUrrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 0,  11, 0)
#define MULHWU_rrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 0,  11, 1)

#define MULLIrri(RD,RA,IM)		_D	(07, RD, RA, IM)

#define MULLWrrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 0, 235, 0)
#define MULLW_rrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 0, 235, 1)
#define MULLWOrrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 1, 235, 0)
#define MULLWO_rrr(RD,RA,RB)		_XO	(31, RD, RA, RB, 1, 235, 1)

#define NANDrrr(RA,RS,RB)		_X	(31, RS, RA, RB, 476, 0)
#define NAND_rrr(RA,RS,RB)		_X	(31, RS, RA, RB, 476, 1)

#define NEGrr(RD,RA)			_XO	(31, RD, RA, 0, 0, 104, 0)
#define NEG_rr(RD,RA)			_XO	(31, RD, RA, 0, 0, 104, 1)
#define NEGOrr(RD,RA)			_XO	(31, RD, RA, 0, 1, 104, 0)
#define NEGO_rr(RD,RA)			_XO	(31, RD, RA, 0, 1, 104, 1)

#define NORrrr(RA,RS,RB)		_X	(31, RS, RA, RB, 124, 0)
#define NOR_rrr(RA,RS,RB)		_X	(31, RS, RA, RB, 124, 1)

#define ORrrr(RA,RS,RB)			_X	(31, RS, RA, RB, 444, 0)
#define OR_rrr(RA,RS,RB)		_X	(31, RS, RA, RB, 444, 1)
#define ORCrrr(RA,RS,RB)		_X	(31, RS, RA, RB, 412, 0)
#define ORC_rrr(RA,RS,RB)		_X	(31, RS, RA, RB, 412, 1)
#define ORIrri(RA,RS,IM)		_Du	(24, RS, RA, IM)
#define ORISrri(RA,RS,IM)		_Du	(25, RS, RA, IM)

#define RLWIMIrriii(RA,RS,SH,MB,ME)	_M	(20, RS, RA, SH, MB, ME, 0)
#define RLWIMI_rriii(RA,RS,SH,MB,ME)	_M	(20, RS, RA, SH, MB, ME, 1)

#define RLWINMrriii(RA,RS,SH,MB,ME)	_M	(21, RS, RA, SH, MB, ME, 0)
#define RLWINM_rriii(RA,RS,SH,MB,ME)	_M	(21, RS, RA, SH, MB, ME, 1)

#define RLWNMrrrii(RA,RS,RB,MB,ME)	_M	(23, RS, RA, RB, MB, ME, 0)
#define RLWNM_rrrii(RA,RS,RB,MB,ME)	_M	(23, RS, RA, RB, MB, ME, 1)

#define SLWrrr(RA,RS,RB)		_X	(31, RS, RA, RB,  24, 0)
#define SLW_rrr(RA,RS,RB)		_X	(31, RS, RA, RB,  24, 1)

#define SRAWrrr(RA,RS,RB)		_X	(31, RS, RA, RB, 792, 0)
#define SRAW_rrr(RA,RS,RB)		_X	(31, RS, RA, RB, 792, 1)

#define SRAWIrri(RD, RS, SH)		_X	(31, RS, RD, SH, 824, 0)
#define SRAWI_rri(RD, RS, SH)		_X	(31, RS, RD, SH, 824, 1)

#define SRWrrr(RA,RS,RB)		_X	(31, RS, RA, RB, 536, 0)
#define SRW_rrr(RA,RS,RB)		_X	(31, RS, RA, RB, 536, 1)

#define STBrm(RS,ID,RA)			_D	(38, RS, RA, ID)
#define STBUrm(RS,ID,RA)		_D	(39, RS, RA, ID)
#define STBUXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 247, 0)
#define STBXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 215, 0)

#define STHrm(RS,ID,RA)			_D	(44, RS, RA, ID)
#define STHUrm(RS,ID,RA)		_D	(45, RS, RA, ID)
#define STHBRXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 918, 0)
#define STHUXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 439, 0)
#define STHXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 407, 0)

#define STMWrm(RS,ID,RA)		_D	(47, RS, RA, ID)

#define STWrm(RS,ID,RA)			_D	(36, RS, RA, ID)
#define STWBRXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 662, 0)
#define STWCXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 150, 0)
#define STWCX_rrr(RS,RA,RB)		_X	(31, RS, RA, RB, 150, 1)
#define STWUrm(RS,ID,RA)		_D	(37, RS, RA, ID)
#define STWUXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 183, 0)
#define STWXrrr(RS,RA,RB)		_X	(31, RS, RA, RB, 151, 0)

#define SUBFrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 40, 0)
#define SUBF_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 40, 1)
#define SUBFrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 40, 0)
#define SUBF_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 40, 1)
#define SUBFErrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 136, 0)
#define SUBFE_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 136, 1)
#define SUBFCrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0,  8, 0)
#define SUBFC_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0,  8, 1)
#define SUBFCOrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1,  8, 0)
#define SUBFCO_rrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1,  8, 1)
#define SUBFICrri(RD, RA, IMM)		_D	(8, RD, RA, IMM)

#define ADDrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 0, 266, 0)
#define ADDOrrr(RD, RA, RB)		_XO	(31, RD, RA, RB, 1, 266, 0)
#define ADDIrri(RD, RA, IMM)		_D	(14, RD, RA, IMM)
#define ADDISrri(RD, RA, IMM)		_Ds	(15, RD, RA, IMM)

#define SYNC()				_X	(31, 00, 00, 00, 598, 0)

#define TWirr(TO,RA,RB)			_X	(31, TO, RA, RB,   4, 0)
#define TWIiri(TO,RA,IM)		_D	(03, TO, RA, IM)

#define XORrrr(RA,RS,RB)		_X	(31, RS, RA, RB, 316, 0)
#define XOR_rrr(RA,RS,RB)		_X	(31, RS, RA, RB, 316, 1)
#define XORIrri(RA,RS,IM)		_Du	(26, RS, RA, IM)
#define XORISrri(RA,RS,IM)		_Du	(27, RS, RA, IM)

/* simplified mnemonics [1, Appendix F] */

#define MOVEIri2(R,H,L)			(LISri(R,H), (L ? ORIrri(R,R,L) : 0))
#define MOVEIri(R,I)			(_siP(16,I) ? LIri(R,I) :	\
					MOVEIri2(R, _HI(I), _LO(I)) )

#define SUBIrri(RD,RA,IM)		ADDIrri(RD,RA,-_LO((IM)))	/* [1, Section F.2.1] */
#define SUBISrri(RD,RA,IM)		ADDISrri(RD,RA,-_LO((IM)))
#define SUBICrri(RD,RA,IM)		ADDICrri(RD,RA,-_LO((IM)))
#define SUBIC_rri(RD,RA,IM)		ADDIC_rri(RD,RA,-_LO((IM)))

#define SUBrrr(RD,RA,RB)		SUBFrrr(RD,RB,RA)	/* [1, Section F.2.2] */
#define SUBOrrr(RD,RA,RB)		SUBFOrrr(RD,RB,RA)
#define SUB_rrr(RD,RA,RB)		SUBF_rrr(RD,RB,RA)
#define SUBCrrr(RD,RA,RB)		SUBFCrrr(RD,RB,RA)
#define SUBCOrrr(RD,RA,RB)		SUBFCOrrr(RD,RB,RA)
#define SUBC_rrr(RD,RA,RB)		SUBFC_rrr(RD,RB,RA)
#define SUBErrr(RD,RA,RB)		SUBFErrr(RD,RB,RA)
#define SUBE_rrr(RD,RA,RB)		SUBFE_rrr(RD,RB,RA)

#define CMPWIiri(C,RA,IM)		CMPIiiri(C,0,RA,IM)	/* [1, Table F-2] */
#define CMPWirr(C,RA,RB)		CMPiirr(C,0,RA,RB)
#define CMPLWIiri(C,RA,IM)		CMPLIiiri(C,0,RA,IM)
#define CMPLWirr(C,RA,RB)		CMPLiirr(C,0,RA,RB)

#define CMPWIri(RA,IM)			CMPWIiri(0,RA,IM)	/* with implicit _cr0 */
#define CMPWrr(RA,RB)			CMPWirr(0,RA,RB)
#define CMPLWIri(RA,IM)			CMPLWIiri(0,RA,IM)
#define CMPLWrr(RA,RB)			CMPLWirr(0,RA,RB)

#define EXTLWIrrii(RA,RS,N,B)		RLWINMrriii(RA, RS,            B,	0,     (N)-1)	/* [1, Table F-3] */
#define EXTRWIrrii(RA,RS,N,B)		RLWINMrriii(RA, RS,      (B)+(N),  32-(N),        31)
#define INSLWIrrii(RA,RS,N,B)		RLWIMIrriii(RA, RS,       32-(B),	B, (B)+(N)-1)
#define INSRWIrrii(RA,RS,N,B)		RLWIMIrriii(RA, RS, 32-((B)+(N)),	B, (B)+(N)-1)
#define ROTLWIrri(RA,RS,N)		RLWINMrriii(RA, RS,            N,	0,        31)
#define ROTRWIrri(RA,RS,N)		RLWINMrriii(RA, RS,       32-(N),	0,        31)
#define ROTLWrrr(RA,RS,RB)		RLWNMrrrii( RA, RS,           RB,	0,        31)
#define SLWIrri(RA,RS,N)		RLWINMrriii(RA, RS,            N,	0,    31-(N))
#define SRWIrri(RA,RS,N)		RLWINMrriii(RA, RS,       32-(N),	N,        31)
#define CLRLWIrri(RA,RS,N)		RLWINMrriii(RA, RS,            0,	N,        31)
#define CLRRWIrri(RA,RS,N)		RLWINMrriii(RA, RS,            0,	0,    31-(N))
#define CLRLSLWIrrii(RA,RS,B,N)		RLWINMrriii(RA, RS,            N, (B)-(N),    31-(N))

#if 0
static int bad_short_jump()
{
  scheme_signal_error("bad short jump");
  return 1;
}
# define NOT_SHORT_JUMPS() (_jitl.long_jumps ? 0 : bad_short_jump()) ,
#else
# define NOT_SHORT_JUMPS() /* empty */
#endif

/* 9 below inverts the branch condition and the branch prediction.
 * This has an incestuous knowledge of JIT_AUX */
#define BC_EXT(A, C, D)  ((_siP(16, _jit_UL(D)-_jit_UL(_jit.x.pc)) && !_jitl.long_jumps) \
  ? BCiii((A), (C), (D)) \
  : (NOT_SHORT_JUMPS() \
     BCiii((A)^9, (C), _jit.x.pc+5), \
     LISri(JIT_AUX,_HI(D)), \
     ORIrri(JIT_AUX,JIT_AUX,_LO(D)), \
     MTLRr(JIT_AUX), BLR() ))

#define B_EXT(D)         ((_siP(16, _jit_UL(D)-_jit_UL(_jit.x.pc)) && !_jitl.long_jumps) \
  ? Bi((D)) \
  : (NOT_SHORT_JUMPS() \
     LISri(JIT_AUX,_HI(D)), \
     ORIrri(JIT_AUX,JIT_AUX,_LO(D)), \
     MTLRr(JIT_AUX), BLR()) )

#define BTii(C,D)			BC_EXT(12, C, D)		/* [1, Table F-5] */
#define BFii(C,D)			BC_EXT( 4, C, D)
#define BDNZi(D)			BCiii(16, 0, D)
#define BDNZTii(C,D)			BC_EXT( 8, C, D)
#define BDNZFii(C,D)			BC_EXT( 0, C, D)
#define BDZi(D)				BCiii(18, 0, D)
#define BDZTii(C,D)			BC_EXT(10, C, D)
#define BDZFii(C,D)			BC_EXT( 2, C, D)
		
#define BCTR()				BCCTRii(20, 0)		/* [1, Table F-6] */
#define BCTRL()				BCCTRLii(20, 0)
		
#define BLR()				BCLRii(20, 0)		/* [1, Table F-6] */
#define BLRL()				BCLRLii(20, 0)
		

#define BLTLRi(CR)			BCLRii(12, ((CR)<<2)+0)	/* [1, Table F-10] */
#define BLELRi(CR)			BCLRii( 4, ((CR)<<2)+1)
#define BEQLRi(CR)			BCLRii(12, ((CR)<<2)+2)
#define BGELRi(CR)			BCLRii( 4, ((CR)<<2)+0)
#define BGTLRi(CR)			BCLRii(12, ((CR)<<2)+1)
#define BNLLRi(CR)			BCLRii( 4, ((CR)<<2)+0)
#define BNELRi(CR)			BCLRii( 4, ((CR)<<2)+2)
#define BNGLRi(CR)			BCLRii( 4, ((CR)<<2)+1)
#define BSOLRi(CR)			BCLRii(12, ((CR)<<2)+3)
#define BNSLRi(CR)			BCLRii( 4, ((CR)<<2)+3)
#define BUNLRi(CR)			BCLRii(12, ((CR)<<2)+3)
#define BNULRi(CR)			BCLRii( 4, ((CR)<<2)+3)
		
#define BLTLRLi(CR)			BCLRLii(12, ((CR)<<2)+0)	/* [1, Table F-10] */
#define BLELRLi(CR)			BCLRLii( 4, ((CR)<<2)+1)
#define BEQLRLi(CR)			BCLRLii(12, ((CR)<<2)+2)
#define BGELRLi(CR)			BCLRLii( 4, ((CR)<<2)+0)
#define BGTLRLi(CR)			BCLRLii(12, ((CR)<<2)+1)
#define BNLLRLi(CR)			BCLRLii( 4, ((CR)<<2)+0)
#define BNELRLi(CR)			BCLRLii( 4, ((CR)<<2)+2)
#define BNGLRLi(CR)			BCLRLii( 4, ((CR)<<2)+1)
#define BSOLRLi(CR)			BCLRLii(12, ((CR)<<2)+3)
#define BNSLRLi(CR)			BCLRLii( 4, ((CR)<<2)+3)
#define BUNLRLi(CR)			BCLRLii(12, ((CR)<<2)+3)
#define BNULRLi(CR)			BCLRLii( 4, ((CR)<<2)+3)
		
#define BLTCTRi(CR)			BCCTRii(12, ((CR)<<2)+0)	/* [1, Table F-10] */
#define BLECTRi(CR)			BCCTRii( 4, ((CR)<<2)+1)
#define BEQCTRi(CR)			BCCTRii(12, ((CR)<<2)+2)
#define BGECTRi(CR)			BCCTRii( 4, ((CR)<<2)+0)
#define BGTCTRi(CR)			BCCTRii(12, ((CR)<<2)+1)
#define BNLCTRi(CR)			BCCTRii( 4, ((CR)<<2)+0)
#define BNECTRi(CR)			BCCTRii( 4, ((CR)<<2)+2)
#define BNGCTRi(CR)			BCCTRii( 4, ((CR)<<2)+1)
#define BSOCTRi(CR)			BCCTRii(12, ((CR)<<2)+3)
#define BNSCTRi(CR)			BCCTRii( 4, ((CR)<<2)+3)
#define BUNCTRi(CR)			BCCTRii(12, ((CR)<<2)+3)
#define BNUCTRi(CR)			BCCTRii( 4, ((CR)<<2)+3)
		
#define BLTCTRLi(CR)			BCCTRLii(12, ((CR)<<2)+0)	/* [1, Table F-10] */
#define BLECTRLi(CR)			BCCTRLii( 4, ((CR)<<2)+1)
#define BEQCTRLi(CR)			BCCTRLii(12, ((CR)<<2)+2)
#define BGECTRLi(CR)			BCCTRLii( 4, ((CR)<<2)+0)
#define BGTCTRLi(CR)			BCCTRLii(12, ((CR)<<2)+1)
#define BNLCTRLi(CR)			BCCTRLii( 4, ((CR)<<2)+0)
#define BNECTRLi(CR)			BCCTRLii( 4, ((CR)<<2)+2)
#define BNGCTRLi(CR)			BCCTRLii( 4, ((CR)<<2)+1)
#define BSOCTRLi(CR)			BCCTRLii(12, ((CR)<<2)+3)
#define BNSCTRLi(CR)			BCCTRLii( 4, ((CR)<<2)+3)
#define BUNCTRLi(CR)			BCCTRLii(12, ((CR)<<2)+3)
#define BNUCTRLi(CR)			BCCTRLii( 4, ((CR)<<2)+3)
		

#define BLTLR()				BLTLRi(0)  	/* with implicit _cr0 */
#define BLELR()				BLELRi(0)  
#define BEQLR()				BEQLRi(0)  
#define BGELR()				BGELRi(0)  
#define BGTLR()				BGTLRi(0)  
#define BNLLR()				BNLLRi(0)  
#define BNELR()				BNELRi(0)  
#define BNGLR()				BNGLRi(0)  
#define BSOLR()				BSOLRi(0)  
#define BNSLR()				BNSLRi(0)  
#define BUNLR()				BUNLRi(0)  
#define BNULR()				BNULRi(0)  
					            
#define BLTLRL()			BLTLRLi(0) 
#define BLELRL()			BLELRLi(0) 
#define BEQLRL()			BEQLRLi(0) 
#define BGELRL()			BGELRLi(0) 
#define BGTLRL()			BGTLRLi(0) 
#define BNLLRL()			BNLLRLi(0) 
#define BNELRL()			BNELRLi(0) 
#define BNGLRL()			BNGLRLi(0) 
#define BSOLRL()			BSOLRLi(0) 
#define BNSLRL()			BNSLRLi(0) 
#define BUNLRL()			BUNLRLi(0) 
#define BNULRL()			BNULRLi(0) 
					            
#define BLTCTR()			BLTCTRi(0) 
#define BLECTR()			BLECTRi(0) 
#define BEQCTR()			BEQCTRi(0) 
#define BGECTR()			BGECTRi(0) 
#define BGTCTR()			BGTCTRi(0) 
#define BNLCTR()			BNLCTRi(0) 
#define BNECTR()			BNECTRi(0) 
#define BNGCTR()			BNGCTRi(0) 
#define BSOCTR()			BSOCTRi(0) 
#define BNSCTR()			BNSCTRi(0) 
#define BUNCTR()			BUNCTRi(0) 
#define BNUCTR()			BNUCTRi(0) 
					            
#define BLTCTRL()			BLTCTRLi(0)
#define BLECTRL()			BLECTRLi(0)
#define BEQCTRL()			BEQCTRLi(0)
#define BGECTRL()			BGECTRLi(0)
#define BGTCTRL()			BGTCTRLi(0)
#define BNLCTRL()			BNLCTRLi(0)
#define BNECTRL()			BNECTRLi(0)
#define BNGCTRL()			BNGCTRLi(0)
#define BSOCTRL()			BSOCTRLi(0)
#define BNSCTRL()			BNSCTRLi(0)
#define BUNCTRL()			BUNCTRLi(0)
#define BNUCTRL()			BNUCTRLi(0)


#define BLTii(C,D)			BC_EXT(12, ((C)<<2)+0, D)	/* [1, Table F-11] */
#define BNLii(C,D)			BC_EXT( 4, ((C)<<2)+0, D)
#define BGEii(C,D)			BC_EXT( 4, ((C)<<2)+0, D)
#define BGTii(C,D)			BC_EXT(12, ((C)<<2)+1, D)
#define BNGii(C,D)			BC_EXT( 4, ((C)<<2)+1, D)
#define BLEii(C,D)			BC_EXT( 4, ((C)<<2)+1, D)
#define BEQii(C,D)			BC_EXT(12, ((C)<<2)+2, D)
#define BNEii(C,D)			BC_EXT( 4, ((C)<<2)+2, D)
#define BSOii(C,D)			BC_EXT(12, ((C)<<2)+3, D)
#define BNSii(C,D)			BC_EXT( 4, ((C)<<2)+3, D)
#define BUNii(C,D)			BC_EXT(12, ((C)<<2)+3, D)
#define BNUii(C,D)			BC_EXT( 4, ((C)<<2)+3, D)

#define BLTi(D)				BLTii(0,D)	/* with implicit _cr0 */
#define BLEi(D)				BLEii(0,D)
#define BEQi(D)				BEQii(0,D)
#define BGEi(D)				BGEii(0,D)
#define BGTi(D)				BGTii(0,D)
#define BNLi(D)				BNLii(0,D)
#define BNEi(D)				BNEii(0,D)
#define BNGi(D)				BNGii(0,D)
#define BSOi(D)				BSOii(0,D)
#define BNSi(D)				BNSii(0,D)
#define BUNi(D)				BUNii(0,D)
#define BNUi(D)				BNUii(0,D)

#define BLTLii(C,D)			BCLiii(12, ((C)<<2)+0, D)	/* [1, Table F-??] */
#define BLELii(C,D)			BCLiii( 4, ((C)<<2)+1, D)
#define BEQLii(C,D)			BCLiii(12, ((C)<<2)+2, D)
#define BGELii(C,D)			BCLiii( 4, ((C)<<2)+0, D)
#define BGTLii(C,D)			BCLiii(12, ((C)<<2)+1, D)
#define BNLLii(C,D)			BCLiii( 4, ((C)<<2)+0, D)
#define BNELii(C,D)			BCLiii( 4, ((C)<<2)+2, D)
#define BNGLii(C,D)			BCLiii( 4, ((C)<<2)+1, D)
#define BSOLii(C,D)			BCLiii(12, ((C)<<2)+3, D)
#define BNSLii(C,D)			BCLiii( 4, ((C)<<2)+3, D)
#define BUNLii(C,D)			BCLiii(12, ((C)<<2)+3, D)
#define BNULii(C,D)			BCLiii( 4, ((C)<<2)+3, D)

#define BLTLi(D)			BLTLii(0,D)	/* with implicit _cr0 */
#define BLELi(D)			BLELii(0,D)
#define BEQLi(D)			BEQLii(0,D)
#define BGELi(D)			BGELii(0,D)
#define BGTLi(D)			BGTLii(0,D)
#define BNLLi(D)			BNLLii(0,D)
#define BNELi(D)			BNELii(0,D)
#define BNGLi(D)			BNGLii(0,D)
#define BSOLi(D)			BSOLii(0,D)
#define BNSLi(D)			BNSLii(0,D)
#define BUNLi(D)			BUNLii(0,D)
#define BNULi(D)			BNULii(0,D)

/* Note: there are many tens of other simplified branches that are not (yet?) defined here */

#define CRSETi(BX)			CREQViii(BX, BX, BX)	/* [1, Table F-15] */
#define CRCLRi(BX)			CRXORiii(BX, BX, BX)
#define CRMOVEii(BX,BY)			CRORiii(BX, BY, BY)
#define CRNOTii(BX,BY)			CRNORiii(BX, BY, BY)
		
#define MTLRr(RS)			MTSPRir(8, RS)		/* [1, Table F-20] */
#define MFLRr(RD)			MFSPRri(RD, 8)
#define MTCTRr(RS)			MTSPRir(9, RS)
#define MFCTRr(RD)			MFSPRri(RD, 9)
#define MTXERr(RS)			MTSPRir(1, RS)
#define MFXERr(RD)			MFSPRri(RD, 1)
		
#define NOP()				ORIrri(0, 0, 0)		/* [1, Section F.9] */
#define LIri(RD,IM)			ADDIrri(RD, 0, IM)
#define LISri(RD,IM)			ADDISrri(RD, 0, IM)
#define LArm(RD,D,RA)			ADDIrri(RD, RA, D)
#define LArrr(RD,RB,RA)			ADDIrrr(RD, RA, RB)
#define MRrr(RA,RS)			ORrrr(RA, RS, RS)
#define NOTrr(RA,RS)			NORrrr(RA, RS, RS)

/* alternative parenthesised forms of extended indexed load/store insns */

#define LBZUrx(RD,RA,RB)		LBZUXrrr(RD,RA,RB)
#define LBZrx(RD,RA,RB)			LBZXrrr(RD,RA,RB)
#define LHAUrx(RD,RA,RB)		LHAUXrrr(RD,RA,RB)
#define LHArx(RD,RA,RB)			LHAXrrr(RD,RA,RB)
#define LHBRrx(RD,RA,RB)		LHBRXrrr(RD,RA,RB)
#define LHZUrx(RD,RA,RB)		LHZUXrrr(RD,RA,RB)
#define LHZrx(RD,RA,RB)			LHZXrrr(RD,RA,RB)
#define LWBRrx(RD,RA,RB)		LWBRXrrr(RD,RA,RB)
#define LWZUrx(RD, RA, RB)		LWZUXrrr(RD, RA, RB)
#define LWZrx(RD, RA, RB)		LWZXrrr(RD, RA, RB)
#define STBUrx(RD,RA,RB)		STBUXrrr(RD,RA,RB)
#define STBrx(RD,RA,RB)			STBXrrr(RD,RA,RB)
#define STHBRrx(RS,RA,RB)		STHBRXrrr(RS,RA,RB)
#define STHUrx(RS,RA,RB)		STHUXrrr(RS,RA,RB)
#define STHrx(RS,RA,RB)			STHXrrr(RS,RA,RB)
#define STWBRrx(RS,RA,RB)		STWBRXrrr(RS,RA,RB)
#define STWCrx(RS,RA,RB)		STWCXrrr(RS,RA,RB)
#define STWCX_rx(RS,RA,RB)		STWCX_rrr(RS,RA,RB)
#define STWUrx(RS,RA,RB)		STWUXrrr(RS,RA,RB)
#define STWrx(RS,RA,RB)			STWXrrr(RS,RA,RB)
#define LArx(RD,RB,RA)			LArrr(RD,RB,RA)	


#define _LO(I)          (_jit_UL(I) & _MASK(16))
#define _HI(I)          (_jit_UL(I) >>     (16))

#define _A(OP,RD,RA,RB,RC,XO,RCx)    _jit_I((_u6(OP)<<26)|(_u5(RD)<<21)|(_u5(RA)<<16)|( _u5(RB)<<11)|_u5(RC)<<6|(_u5(XO)<<1)|_u1(RCx))

#define LFDrri(RD,RA,imm)       _D(50,RD,RA,imm)
#define LFDUrri(RD,RA,imm)      _D(51,RD,RA,imm)
#define LFDUxrrr(RD,RA,RB)      _X(31,RD,RA,RB,631,0)
#define LFDxrrr(RD,RA,RB)       _X(31,RD,RA,RB,599,0)

#define LFSrri(RD,RA,imm)       _D(48,RD,RA,imm)
#define LFSUrri(RD,RA,imm)      _D(49,RD,RA,imm)
#define LFSUxrrr(RD,RA,RB)      _X(31,RD,RA,RB,567,0)
#define LFSxrrr(RD,RA,RB)       _X(31,RD,RA,RB,535,0)

#define STFDrri(RS,RA,imm)      _D(54,RS,RA,imm)
#define STFDUrri(RS,RA,imm)     _D(55,RS,RA,imm)
#define STFDUxrrr(RS,RA,RB)     _X(31,RS,RA,RB,759,0)
#define STFDxrrr(RS,RA,RB)      _X(31,RS,RA,RB,727,0)

#define STFSrri(RS,RA,imm)      _D(52,RS,RA,imm)
#define STFSUrri(RS,RA,imm)     _D(53,RS,RA,imm)
#define STFSUxrrr(RS,RA,RB)     _X(31,RS,RA,RB,695,0)
#define STFSxrrr(RS,RA,RB)      _X(31,RS,RA,RB,663,0)
#define STFIWXrrr(RS,RA,RB)     _X(31,RS,RA,RB,983,0)

#define FADDDrrr(RD,RA,RB)       _A(63,RD,RA,RB,0,21,0)
#define FADDSrrr(RD,RA,RB)       _A(59,RD,RA,RB,0,21,0)
#define FSUBDrrr(RD,RA,RB)       _A(63,RD,RA,RB,0,20,0)
#define FSUBSrrr(RD,RA,RB)       _A(59,RD,RA,RB,0,20,0)
#define FMULDrrr(RD,RA,RC)       _A(63,RD,RA,0,RC,25,0)
#define FMULSrrr(RD,RA,RC)       _A(59,RD,RA,0,RC,25,0)
#define FDIVDrrr(RD,RA,RB)       _A(63,RD,RA,RB,0,18,0)
#define FDIVSrrr(RD,RA,RB)       _A(59,RD,RA,RB,0,25,0)
#define FSQRTDrr(RD,RB)          _A(63,RD,0,RB,0,22,0)
#define FSQRTSrr(RD,RB)          _A(59,RD,0,RB,0,22,0)
#define FSELrrrr(RD,RA,RB,RC)    _A(63,RD,RA,RB,RC,23,0)
#define FCTIWrr(RD,RB)           _X(63,RD,0,RB,14,0)
#define FCTIWZrr(RD,RB)          _X(63,RD,0,RB,15,0)
#define FRSPrr(RD,RB)            _X(63,RD,0,RB,12,0)
#define FABSrr(RD,RB)            _X(63,RD,0,RB,264,0)
#define FNABSrr(RD,RB)           _X(63,RD,0,RB,136,0)
#define FNEGrr(RD,RB)            _X(63,RD,0,RB,40,0)
#define FMOVErr(RD,RB)           _X(63,RD,0,RB,72,0)
#define FCMPOrrr(CR,RA,RB)       _X(63,_u3((CR)<<2),RA,RB,32,0)
#define FCMPUrrr(CR,RA,RB)       _X(63,_u3((CR)<<2),RA,RB,0,0)
#define MTFSFIri(CR,IMM)          _X(63,_u5((CR)<<2),0,_u5((IMM)<<1),134,0)

/*** References:
 *
 * [1] "PowerPC Microprocessor Family: The Programming Environments For 32-Bit Microprocessors", Motorola, 1997.
 */


#endif
#endif /* __ccg_asm_ppc_h */
