/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler for the i386
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

/*	OPCODE	+ i		= immediate operand
 *		+ r		= register operand
 *		+ m		= memory operand (disp,base,index,scale)
 *		+ sr/sm		= a star preceding a register or memory
 */


typedef _uc		jit_insn;

#ifndef LIGHTNING_DEBUG
#define _b00		0
#define _b01		1
#define _b10		2
#define _b11		3

#define _b000		0
#define _b001		1
#define _b010		2
#define _b011		3
#define _b100		4
#define _b101		5
#define _b110		6
#define _b111		7

/*** REGISTERS ***/	/* [size,,number] */


#define _AL		0x10
#define _CL		0x11
#define _DL		0x12
#define _BL		0x13
#define _AH		0x14
#define _CH		0x15
#define _DH		0x16
#define _BH		0x17

#define _AX		0x20
#define _CX		0x21
#define _DX		0x22
#define _BX		0x23
#define _SP		0x24
#define _BP		0x25
#define _SI		0x26
#define _DI		0x27

#define _EAX		0x40
#define _ECX		0x41
#define _EDX		0x42
#define _EBX		0x43
#define _ESP		0x44
#define _EBP		0x45
#define _ESI		0x46
#define _EDI		0x47

#define _ST0		0
#define _ST1		1
#define _ST2		2
#define _ST3		3
#define _ST4		4
#define _ST5		5
#define _ST6		6
#define _ST7		7

#define _rS(R)		((R)>>4)
#define _rN(R)		((R)&0x7)
#define _r0P(R)		((R)==0)

#ifndef _ASM_SAFETY
#define _r1(R)		_rN(R)
#define _r2(R)		_rN(R)
#define _r4(R)		_rN(R)
#else
#define _r1(R)		((_rS(R)==1) ? _rN(R) : JITFAIL( "8-bit register required"))
#define _r2(R)		((_rS(R)==2) ? _rN(R) : JITFAIL("16-bit register required"))
#define _r4(R)		((_rS(R)==4) ? _rN(R) : JITFAIL("32-bit register required"))
#endif

/*** ASSEMBLER ***/

#define _OFF4(D)        (_jit_UL(D) - _jit_UL(_jit.x.pc))
#define _CKD8(D)        _ck_d(8, ((_uc) _OFF4(D)) )

#define _D8(D)          (_jit_B(0), ((*(_PUC(_jit.x.pc)-1))= _CKD8(D)))
#define _D32(D)         (_jit_L(0), ((*(_PUL(_jit.x.pc)-1))= _OFF4(D)))

#ifndef _ASM_SAFETY
# define _M(M)		(M)
# define _r(R)		(R)
# define _m(M)		(M)
# define _s(S)		(S)
# define _i(I)		(I)
# define _b(B)		(B)
# define _noESP(I,OK)	(OK)
#else
# define _M(M)		(((M)>3) ? JITFAIL("internal error: mod = " #M) : (M))
# define _r(R)		(((R)>7) ? JITFAIL("internal error: reg = " #R) : (R))
# define _m(M)		(((M)>7) ? JITFAIL("internal error: r/m = " #M) : (M))
# define _s(S)		(((S)>3) ? JITFAIL("internal error: memory scale = " #S) : (S))
# define _i(I)		(((I)>7) ? JITFAIL("internal error: memory index = " #I) : (I))
# define _b(B)		(((B)>7) ? JITFAIL("internal error: memory base = "  #B) : (B))
# define _noESP(I,OK)	(((I)==_ESP) ? JITFAIL("illegal index register: %esp") : (OK))
#endif

#define _Mrm(Md,R,M)	_jit_B((_M(Md)<<6)|(_r(R)<<3)|_m(M))
#define _SIB(Sc,I, B)	_jit_B((_s(Sc)<<6)|(_i(I)<<3)|_b(B))

#define _SCL(S)		((((S)==1) ? _b00 : \
			 (((S)==2) ? _b01 : \
			 (((S)==4) ? _b10 : \
			 (((S)==8) ? _b11 : JITFAIL("illegal scale: " #S))))))

/* memory subformats - urgh! */

#define _r_D(	R, D	  )	(_Mrm(_b00,_rN(R),_b101 )		             ,_jit_L((long)(D)))
#define _r_0B(	R,   B    )	(_Mrm(_b00,_rN(R),_r4(B))			           )
#define _r_0BIS(R,   B,I,S)	(_Mrm(_b00,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_r4(B))      )
#define _r_1B(	R, D,B    )	(_Mrm(_b01,_rN(R),_r4(B))		             ,_jit_B((long)(D)))
#define _r_1BIS(R, D,B,I,S)	(_Mrm(_b01,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_r4(B)),_jit_B((long)(D)))
#define _r_4B(	R, D,B    )	(_Mrm(_b10,_rN(R),_r4(B))		             ,_jit_L((long)(D)))
#define _r_4IS( R, D,I,S)	(_Mrm(_b00,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_b101 ),_jit_L((long)(D)))
#define _r_4BIS(R, D,B,I,S)	(_Mrm(_b10,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_r4(B)),_jit_L((long)(D)))

#define _r_DB(  R, D,B    )	((_s0P(D) && (B != _EBP) ? _r_0B  (R,  B    ) : (_s8P(D) ? _r_1B(  R,D,B    ) : _r_4B(  R,D,B    ))))
#define _r_DBIS(R, D,B,I,S)	((_s0P(D)		 ? _r_0BIS(R,  B,I,S) : (_s8P(D) ? _r_1BIS(R,D,B,I,S) : _r_4BIS(R,D,B,I,S))))
 
#define _r_X(   R, D,B,I,S)	(_r0P(I) ? (_r0P(B)   ? _r_D   (R,D            )   : \
				           (_ESP==(B) ? _r_DBIS(R,D,_ESP,_ESP,1)   : \
						        _r_DB  (R,D,   B       ))) : \
				 (_r0P(B)	      ? _r_4IS (R,D,	    I,S)   : \
				 (((I)!=_ESP)         ? _r_DBIS(R,D,   B,   I,S)   : \
						        JITFAIL("illegal index register: %esp"))))


/* instruction formats */

/*	 _format						     Opcd	  ModR/M dN(rB,rI,Sc)	  imm... */

#define	 _d16()					   (		  _jit_B(0x66	)				  )
#define	  _O(	     OP				)  (		  _jit_B(  OP	)				  )
#define	  _Or(	     OP,R			)  (		  _jit_B( (OP)|_r(R))				  )
#define	 _OO(	     OP				)  ( _jit_B((OP)>>8), _jit_B( (OP)	)				  )
#define	 _OOr(	     OP,R			)  ( _jit_B((OP)>>8), _jit_B( (OP)|_r(R))				  )
#define	  _Os(	     OP,B			)  (	_s8P(B) ? _jit_B(((OP)|_b10)) : _jit_B(OP)			  )
#define	    _sW(			     W	)  (				       _s8P(W) ? _jit_B(W):_jit_W(W)	  )
#define	    _sL(			     L	)  (				       _s8P(L) ? _jit_B(L):_jit_L(L)	  )
#define	  _O_W(	     OP			    ,W	)  (	    _O	    (  OP  )			      ,_jit_W(W)	  )
#define	  _O_D8(     OP			    ,D	)  (	    _O	    (  OP  )			     ,_D8(D)	  )
#define	  _O_D32(     OP		    ,D	)  (	    _O	    (  OP  )			     ,_D32(D)	  )
#define	 _OO_D32(     OP		    ,D	)  (	   _OO	    (  OP  )			     ,_D32(D)	  )
#define	  _Os_sW(    OP			    ,W	)  (	    _Os	    (  OP,W)			     ,_sW(W)	  )
#define	  _Os_sL(    OP			    ,L	)  (	    _Os	    (  OP,L)			     ,_sL(L)	  )
#define	  _O_W_B(    OP			    ,W,B)  (	    _O	    (  OP  )			      ,_jit_W(W),_jit_B(B))
#define	  _Or_B(     OP,R		    ,B	)  (	    _Or	    (  OP,R)			      ,_jit_B(B)	  )
#define	  _Or_W(     OP,R		    ,W	)  (	    _Or	    (  OP,R)			      ,_jit_W(W)	  )
#define	  _Or_L(     OP,R		    ,L	)  (	    _Or	    (  OP,R)			      ,_jit_L(L)	  )
#define	  _O_Mrm(    OP	 ,MO,R,M		)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    )		  )
#define	 _OO_Mrm(    OP	 ,MO,R,M		)  (	   _OO	    (  OP  ),_Mrm(MO,R,M	    )		  )
#define	  _O_Mrm_B(  OP	 ,MO,R,M	    ,B	)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_B(B)	  )
#define	  _O_Mrm_W(  OP	 ,MO,R,M	    ,W	)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_W(W)	  )
#define	  _O_Mrm_L(  OP	 ,MO,R,M	    ,L	)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_L(L)	  )
#define	 _OO_Mrm_B(  OP	 ,MO,R,M	    ,B	)  (	   _OO	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_B(B)	  )
#define	  _Os_Mrm_sW(OP	 ,MO,R,M	    ,W	)  (	    _Os	    (  OP,W),_Mrm(MO,R,M	    ),_sW(W)	  )
#define	  _Os_Mrm_sL(OP	 ,MO,R,M	    ,L	)  (	    _Os	    (  OP,L),_Mrm(MO,R,M	    ),_sL(L)	  )
#define	  _O_r_X(    OP	    ,R	,MD,MB,MI,MS	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS)		  )
#define	 _OO_r_X(    OP	    ,R	,MD,MB,MI,MS	)  (	   _OO	    (  OP  ),_r_X(   R	,MD,MB,MI,MS)		  )
#define	  _O_r_X_B(  OP	    ,R	,MD,MB,MI,MS,B	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_B(B)	  )
#define	  _O_r_X_W(  OP	    ,R	,MD,MB,MI,MS,W	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_W(W)	  )
#define	  _O_r_X_L(  OP	    ,R	,MD,MB,MI,MS,L	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_L(L)	  )
#define	 _OO_r_X_B(  OP	    ,R	,MD,MB,MI,MS,B	)  (	   _OO	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_B(B)	  )
#define	  _Os_r_X_sW(OP	    ,R	,MD,MB,MI,MS,W	)  (	    _Os	    (  OP,W),_r_X(   R	,MD,MB,MI,MS),_sW(W)	  )
#define	  _Os_r_X_sL(OP	    ,R	,MD,MB,MI,MS,L	)  (	    _Os	    (  OP,L),_r_X(   R	,MD,MB,MI,MS),_sL(L)	  )
#define	  _O_X_B(    OP		,MD,MB,MI,MS,B	)  (	    _O_r_X_B(  OP	    ,0	,MD,MB,MI,MS	 ,B)	  )
#define	  _O_X_W(    OP		,MD,MB,MI,MS,W	)  (	    _O_r_X_W(  OP	    ,0	,MD,MB,MI,MS	 ,W)	  )
#define	  _O_X_L(    OP		,MD,MB,MI,MS,L	)  (	    _O_r_X_L(  OP	    ,0	,MD,MB,MI,MS	 ,L)	  )
#define	 _wO(	     OP				)  (_d16(), _O(	       OP				   )	  )
#define	 _wOr(	     OP,R			)  (_d16(), _Or(       OP,R				   )	  )
#define	 _wOr_W(     OP,R		    ,W	)  (_d16(), _Or_W(     OP,R				 ,W)	  )
#define	 _wOs_sW(    OP			    ,W	)  (_d16(), _Os_sW(    OP				 ,W)	  )
#define	 _wO_Mrm(    OP	 ,MO,R,M		)  (_d16(), _O_Mrm(    OP	 ,MO,R,M		   )	  )
#define _wOO_Mrm(    OP	 ,MO,R,M		)  (_d16(),_OO_Mrm(    OP	 ,MO,R,M		   )	  )
#define	 _wO_Mrm_B(  OP	 ,MO,R,M	    ,B	)  (_d16(), _O_Mrm_B(  OP	 ,MO,R,M		 ,B)	  )
#define _wOO_Mrm_B(  OP	 ,MO,R,M	    ,B	)  (_d16(),_OO_Mrm_B(  OP	 ,MO,R,M		 ,B)	  )
#define	 _wO_Mrm_W(  OP	 ,MO,R,M	    ,W	)  (_d16(), _O_Mrm_W(  OP	 ,MO,R,M		 ,W)	  )
#define	 _wOs_Mrm_sW(OP	 ,MO,R,M	    ,W	)  (_d16(), _Os_Mrm_sW(OP	 ,MO,R,M		 ,W)	  )
#define	 _wO_X_W(    OP		,MD,MB,MI,MS,W	)  (_d16(), _O_X_W(    OP		,MD,MB,MI,MS	 ,W)	  )
#define	 _wO_r_X(    OP	    ,R	,MD,MB,MI,MS	)  (_d16(), _O_r_X(    OP	    ,R	,MD,MB,MI,MS	   )	  )
#define _wOO_r_X(    OP	    ,R	,MD,MB,MI,MS	)  (_d16(),_OO_r_X(    OP	    ,R	,MD,MB,MI,MS	   )	  )
#define	 _wO_r_X_B(  OP	    ,R	,MD,MB,MI,MS,B	)  (_d16(), _O_r_X_B(  OP	    ,R	,MD,MB,MI,MS	 ,B)	  )
#define _wOO_r_X_B(  OP	    ,R	,MD,MB,MI,MS,B	)  (_d16(),_OO_r_X_B(  OP	    ,R	,MD,MB,MI,MS	 ,B)	  )
#define	 _wO_r_X_W(  OP	    ,R	,MD,MB,MI,MS,W	)  (_d16(), _O_r_X_W(  OP	    ,R	,MD,MB,MI,MS	 ,W)	  )
#define	 _wOs_r_X_sW(OP	    ,R	,MD,MB,MI,MS,W	)  (_d16(), _Os_r_X_sW(OP	    ,R	,MD,MB,MI,MS	 ,W)	  )

/* +++ fully-qualified intrinsic instructions */

/*					_format		 Opcd		,Mod ,r	    ,m		,mem=dsp+sib	,imm... */

#define ADCBrr(RS, RD)			_O_Mrm		(0x10		,_b11,_r1(RS),_r1(RD)				)
#define ADCBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x12		     ,_r1(RD)		,MD,MB,MI,MS		)
#define ADCBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x10		     ,_r1(RS)		,MD,MB,MI,MS		)
#define ADCBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b010  ,_r1(RD)			,_su8(IM))
#define ADCBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b010		,MD,MB,MI,MS	,_su8(IM))

#define ADCWrr(RS, RD)			_wO_Mrm		(0x11		,_b11,_r2(RS),_r2(RD)				)
#define ADCWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x13		     ,_r2(RD)		,MD,MB,MI,MS		)
#define ADCWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x11		     ,_r2(RS)		,MD,MB,MI,MS		)
#define ADCWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b010  ,_r2(RD)			,_su16(IM))
#define ADCWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b010		,MD,MB,MI,MS	,_su16(IM))

#define ADCLrr(RS, RD)			_O_Mrm		(0x11		,_b11,_r4(RS),_r4(RD)				)
#define ADCLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x13		     ,_r4(RD)		,MD,MB,MI,MS		)
#define ADCLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x11		     ,_r4(RS)		,MD,MB,MI,MS		)
#define ADCLir(IM, RD)			_Os_Mrm_sL	(0x81		,_b11,_b010  ,_r4(RD)			,IM	)
#define ADCLim(IM, MD, MB, MI, MS)	_Os_r_X_sL	(0x81		     ,_b010		,MD,MB,MI,MS	,IM	)


#define ADDBrr(RS, RD)			_O_Mrm		(0x00		,_b11,_r1(RS),_r1(RD)				)
#define ADDBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x02		     ,_r1(RD)		,MD,MB,MI,MS		)
#define ADDBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x00		     ,_r1(RS)		,MD,MB,MI,MS		)
#define ADDBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b000  ,_r1(RD)			,_su8(IM))
#define ADDBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b000		,MD,MB,MI,MS	,_su8(IM))

#define ADDWrr(RS, RD)			_wO_Mrm		(0x01		,_b11,_r2(RS),_r2(RD)				)
#define ADDWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x03		     ,_r2(RD)		,MD,MB,MI,MS		)
#define ADDWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x01		     ,_r2(RS)		,MD,MB,MI,MS		)
#define ADDWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b000  ,_r2(RD)			,_su16(IM))
#define ADDWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b000		,MD,MB,MI,MS	,_su16(IM))

#define ADDLrr(RS, RD)			_O_Mrm		(0x01		,_b11,_r4(RS),_r4(RD)				)
#define ADDLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x03		     ,_r4(RD)		,MD,MB,MI,MS		)
#define ADDLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x01		     ,_r4(RS)		,MD,MB,MI,MS		)
#define ADDLir(IM, RD)			_Os_Mrm_sL	(0x81		,_b11,_b000  ,_r4(RD)			,IM	)
#define ADDLim(IM, MD, MB, MI, MS)	_Os_r_X_sL	(0x81		     ,_b000		,MD,MB,MI,MS	,IM	)


#define ANDBrr(RS, RD)			_O_Mrm		(0x20		,_b11,_r1(RS),_r1(RD)				)
#define ANDBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x22		     ,_r1(RD)		,MD,MB,MI,MS		)
#define ANDBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x20		     ,_r1(RS)		,MD,MB,MI,MS		)
#define ANDBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b100  ,_r1(RD)			,_su8(IM))
#define ANDBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b100		,MD,MB,MI,MS	,_su8(IM))

#define ANDWrr(RS, RD)			_wO_Mrm		(0x21		,_b11,_r2(RS),_r2(RD)				)
#define ANDWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x23		     ,_r2(RD)		,MD,MB,MI,MS		)
#define ANDWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x21		     ,_r2(RS)		,MD,MB,MI,MS		)
#define ANDWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b100  ,_r2(RD)			,_su16(IM))
#define ANDWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b100		,MD,MB,MI,MS	,_su16(IM))

#define ANDLrr(RS, RD)			_O_Mrm		(0x21		,_b11,_r4(RS),_r4(RD)				)
#define ANDLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x23		     ,_r4(RD)		,MD,MB,MI,MS		)
#define ANDLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x21		     ,_r4(RS)		,MD,MB,MI,MS		)
#define ANDLir(IM, RD)			_Os_Mrm_sL	(0x81		,_b11,_b100  ,_r4(RD)			,IM	)
#define ANDLim(IM, MD, MB, MI, MS)	_Os_r_X_sL	(0x81		     ,_b100		,MD,MB,MI,MS	,IM	)


#define BSWAPLr(R)			_OOr		(0x0fc8,_r4(R)							)


#define BTWir(IM,RD)			_wOO_Mrm_B	(0x0fba		,_b11,_b100  ,_r2(RD)			,_u8(IM))
#define BTWim(IM,MD,MB,MI,MS)		_wOO_r_X_B	(0x0fba		     ,_b100		,MD,MB,MI,MS	,_u8(IM))
#define BTWrr(RS,RD)			_wOO_Mrm	(0x0fa3		,_b11,_r2(RS),_r2(RD)				)
#define BTWrm(RS,MD,MB,MI,MS)		_wOO_r_X	(0x0fa3		     ,_r2(RS)		,MD,MB,MI,MS		)

#define BTLir(IM,RD)			_OO_Mrm_B	(0x0fba		,_b11,_b100  ,_r4(RD)			,_u8(IM))
#define BTLim(IM,MD,MB,MI,MS)		_OO_r_X_B	(0x0fba		     ,_b100		,MD,MB,MI,MS	,_u8(IM))
#define BTLrr(RS,RD)			_OO_Mrm		(0x0fa3		,_b11,_r4(RS),_r4(RD)				)
#define BTLrm(RS,MD,MB,MI,MS)		_OO_r_X		(0x0fa3		     ,_r4(RS)		,MD,MB,MI,MS		)


#define BTCWir(IM,RD)			_wOO_Mrm_B	(0x0fba		,_b11,_b111  ,_r2(RD)			,_u8(IM))
#define BTCWim(IM,MD,MB,MI,MS)		_wOO_r_X_B	(0x0fba		     ,_b111		,MD,MB,MI,MS	,_u8(IM))
#define BTCWrr(RS,RD)			_wOO_Mrm	(0x0fbb		,_b11,_r2(RS),_r2(RD)				)
#define BTCWrm(RS,MD,MB,MI,MS)		_wOO_r_X	(0x0fbb		     ,_r2(RS)		,MD,MB,MI,MS		)

#define BTCLir(IM,RD)			_OO_Mrm_B	(0x0fba		,_b11,_b111  ,_r4(RD)			,_u8(IM))
#define BTCLim(IM,MD,MB,MI,MS)		_OO_r_X_B	(0x0fba		     ,_b111		,MD,MB,MI,MS	,_u8(IM))
#define BTCLrr(RS,RD)			_OO_Mrm		(0x0fbb		,_b11,_r4(RS),_r4(RD)				)
#define BTCLrm(RS,MD,MB,MI,MS)		_OO_r_X		(0x0fbb		     ,_r4(RS)		,MD,MB,MI,MS		)


#define BTRWir(IM,RD)			_wOO_Mrm_B	(0x0fba		,_b11,_b110  ,_r2(RD)			,_u8(IM))
#define BTRWim(IM,MD,MB,MI,MS)		_wOO_r_X_B	(0x0fba		     ,_b110		,MD,MB,MI,MS	,_u8(IM))
#define BTRWrr(RS,RD)			_wOO_Mrm	(0x0fb3		,_b11,_r2(RS),_r2(RD)				)
#define BTRWrm(RS,MD,MB,MI,MS)		_wOO_r_X	(0x0fb3		     ,_r2(RS)		,MD,MB,MI,MS		)

#define BTRLir(IM,RD)			_OO_Mrm_B	(0x0fba		,_b11,_b110  ,_r4(RD)			,_u8(IM))
#define BTRLim(IM,MD,MB,MI,MS)		_OO_r_X_B	(0x0fba		     ,_b110		,MD,MB,MI,MS	,_u8(IM))
#define BTRLrr(RS,RD)			_OO_Mrm		(0x0fb3		,_b11,_r4(RS),_r4(RD)				)
#define BTRLrm(RS,MD,MB,MI,MS)		_OO_r_X		(0x0fb3		     ,_r4(RS)		,MD,MB,MI,MS		)


#define BTSWir(IM,RD)			_wOO_Mrm_B	(0x0fba		,_b11,_b101  ,_r2(RD)			,_u8(IM))
#define BTSWim(IM,MD,MB,MI,MS)		_wOO_r_X_B	(0x0fba		     ,_b101		,MD,MB,MI,MS	,_u8(IM))
#define BTSWrr(RS,RD)			_wOO_Mrm	(0x0fab		,_b11,_r2(RS),_r2(RD)				)
#define BTSWrm(RS,MD,MB,MI,MS)		_wOO_r_X	(0x0fab		     ,_r2(RS)		,MD,MB,MI,MS		)

#define BTSLir(IM,RD)			_OO_Mrm_B	(0x0fba		,_b11,_b101  ,_r4(RD)			,_u8(IM))
#define BTSLim(IM,MD,MB,MI,MS)		_OO_r_X_B	(0x0fba		     ,_b101		,MD,MB,MI,MS	,_u8(IM))
#define BTSLrr(RS,RD)			_OO_Mrm		(0x0fab		,_b11,_r4(RS),_r4(RD)				)
#define BTSLrm(RS,MD,MB,MI,MS)		_OO_r_X		(0x0fab		     ,_r4(RS)		,MD,MB,MI,MS		)


#define CALLm(D,B,I,S)			((_r0P(B) && _r0P(I)) ? _O_D32	(0xe8			,(int)(D)		) : \
								JITFAIL("illegal mode in direct jump"))

#define CALLsr(R)			_O_Mrm	(0xff	,_b11,_b010,_r4(R)			)

#define CALLsm(D,B,I,S)			_O_r_X	(0xff	     ,_b010	,(int)(D),B,I,S		)

#define CBW_()				_O		(0x98								)
#define CLC_()				_O		(0xf8								)
#define CLTD_()				_O		(0x99								)
#define CMC_()				_O		(0xf5								)


#define CMPBrr(RS, RD)			_O_Mrm		(0x38		,_b11,_r1(RS),_r1(RD)				)
#define CMPBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x3a		     ,_r1(RD)		,MD,MB,MI,MS		)
#define CMPBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x38		     ,_r1(RS)		,MD,MB,MI,MS		)
#define CMPBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b111  ,_r1(RD)			,_su8(IM))
#define CMPBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b111		,MD,MB,MI,MS	,_su8(IM))

#define CMPWrr(RS, RD)			_wO_Mrm		(0x39		,_b11,_r2(RS),_r2(RD)				)
#define CMPWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x3b		     ,_r2(RD)		,MD,MB,MI,MS		)
#define CMPWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x39		     ,_r2(RS)		,MD,MB,MI,MS		)
#define CMPWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b111  ,_r2(RD)			,_su16(IM))
#define CMPWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b111		,MD,MB,MI,MS	,_su16(IM))

#define CMPLrr(RS, RD)			_O_Mrm		(0x39		,_b11,_r4(RS),_r4(RD)				)
#define CMPLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x3b		     ,_r4(RD)		,MD,MB,MI,MS		)
#define CMPLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x39		     ,_r4(RS)		,MD,MB,MI,MS		)
#define CMPLir(IM, RD)			_O_Mrm_L	(0x81		,_b11,_b111  ,_r4(RD)			,IM	)
#define CMPLim(IM, MD, MB, MI, MS)	_O_r_X_L	(0x81		     ,_b111		,MD,MB,MI,MS	,IM	)


#define CWD_()				_O		(0x99								)


#define CMPXCHGBrr(RS,RD)		_OO_Mrm		(0x0fb0		,_b11,_r1(RS),_r1(RD)				)
#define CMPXCHGBrm(RS,MD,MB,MI,MS)	_OO_r_X		(0x0fb0		     ,_r1(RS)		,MD,MB,MI,MS		)

#define CMPXCHGWrr(RS,RD)		_wOO_Mrm	(0x0fb1		,_b11,_r2(RS),_r2(RD)				)
#define CMPXCHGWrm(RS,MD,MB,MI,MS)	_wOO_r_X	(0x0fb1		     ,_r2(RS)		,MD,MB,MI,MS		)

#define CMPXCHGLrr(RS,RD)		_OO_Mrm		(0x0fb1		,_b11,_r4(RS),_r4(RD)				)
#define CMPXCHGLrm(RS,MD,MB,MI,MS)	_OO_r_X		(0x0fb1		     ,_r4(RS)		,MD,MB,MI,MS		)


#define DECBr(RD)			_O_Mrm		(0xfe		,_b11,_b001  ,_r1(RD)				)
#define DECBm(MD,MB,MI,MS)		_O_r_X		(0xfe		     ,_b001		,MD,MB,MI,MS		)

#define DECWr(RD)			_wOr		(0x48,_r2(RD)							)
#define DECWm(MD,MB,MI,MS)		_wO_r_X		(0xff		     ,_b001		,MD,MB,MI,MS		)

#define DECLr(RD)			_Or		(0x48,_r4(RD)							)
#define DECLm(MD,MB,MI,MS)		_O_r_X		(0xff		     ,_b001		,MD,MB,MI,MS		)


#define DIVBr(RS)			_O_Mrm		(0xf6		,_b11,_b110  ,_r1(RS)				)
#define DIVBm(MD,MB,MI,MS)		_O_r_X		(0xf6		     ,_b110		,MD,MB,MI,MS		)

#define DIVWr(RS)			_wO_Mrm		(0xf7		,_b11,_b110  ,_r2(RS)				)
#define DIVWm(MD,MB,MI,MS)		_wO_r_X		(0xf7		     ,_b110		,MD,MB,MI,MS		)

#define DIVLr(RS)			_O_Mrm		(0xf7		,_b11,_b110  ,_r4(RS)				)
#define DIVLm(MD,MB,MI,MS)		_O_r_X		(0xf7		     ,_b110		,MD,MB,MI,MS		)


#define ENTERii(W, B)			_O_W_B		(0xc8						  ,_su16(W),_su8(B))
#define HLT_()				_O		(0xf4								)


#define IDIVBr(RS)			_O_Mrm		(0xf6		,_b11,_b111  ,_r1(RS)				)
#define IDIVBm(MD,MB,MI,MS)		_O_r_X		(0xf6		     ,_b111		,MD,MB,MI,MS		)

#define IDIVWr(RS)			_wO_Mrm 	(0xf7		,_b11,_b111  ,_r2(RS)				)
#define IDIVWm(MD,MB,MI,MS)		_wO_r_X 	(0xf7		     ,_b111		,MD,MB,MI,MS		)

#define IDIVLr(RS)			_O_Mrm		(0xf7		,_b11,_b111  ,_r4(RS)				)
#define IDIVLm(MD,MB,MI,MS)		_O_r_X		(0xf7		     ,_b111		,MD,MB,MI,MS		)

#define IMULBr(RS)			_O_Mrm		(0xf6		,_b11,_b101  ,_r1(RS)				)
#define IMULBm(MD,MB,MI,MS)		_O_r_X		(0xf6		     ,_b101		,MD,MB,MI,MS		)

#define IMULWr(RS)			_wO_Mrm 	(0xf7		,_b11,_b101  ,_r2(RS)				)
#define IMULWm(MD,MB,MI,MS)		_wO_r_X 	(0xf7		     ,_b101		,MD,MB,MI,MS		)

#define IMULLr(RS)			_O_Mrm		(0xf7		,_b11,_b101  ,_r4(RS)				)
#define IMULLm(MD,MB,MI,MS)		_O_r_X		(0xf7		     ,_b101		,MD,MB,MI,MS		)


#define IMULWrr(RS,RD)			_wOO_Mrm	(0x0faf		,_b11,_r2(RS),_r2(RD)				)
#define IMULWmr(MD,MB,MI,MS,RD)		_wOO_r_X	(0x0faf		     ,_r2(RD)		,MD,MB,MI,MS		)
#define IMULWirr(IM,RS,RD)		_wOs_Mrm_sW	(0x69		,_b11,_r2(RS),_r2(RD)			,_su16(IM)	)
#define IMULWimr(IM,MD,MB,MI,MS,RD)	_wOs_r_X_sW	(0x69		     ,_r2(RD)		,MD,MB,MI,MS	,_su16(IM)	)

#define IMULLir(IM,RD)			_Os_Mrm_sL	(0x69		,_b11,_r4(RD),_r4(RD)			,IM	)
#define IMULLrr(RS,RD)			_OO_Mrm		(0x0faf		,_b11,_r4(RD),_r4(RS)				)
#define IMULLmr(MD,MB,MI,MS,RD)		_OO_r_X		(0x0faf		     ,_r4(RD)		,MD,MB,MI,MS		)
#define IMULLirr(IM,RS,RD)		_Os_Mrm_sL	(0x69		,_b11,_r4(RS),_r4(RD)			,IM	)
#define IMULLimr(IM,MD,MB,MI,MS,RD)	_Os_r_X_sL	(0x69		     ,_r4(RD)		,MD,MB,MI,MS	,IM	)


#define INCBr(RD)			_O_Mrm		(0xfe		,_b11,_b000  ,_r1(RD)				)
#define INCBm(MD,MB,MI,MS)		_O_r_X		(0xfe		     ,_b000		,MD,MB,MI,MS		)

#define INCWr(RD)			_wOr		(0x40,_r2(RD)							)
#define INCWm(MD,MB,MI,MS)		_wO_r_X		(0xff		     ,_b000		,MD,MB,MI,MS		)

#define INCLr(RD)			_Or		(0x40,_r4(RD)							)
#define INCLm(MD,MB,MI,MS)		_O_r_X		(0xff		     ,_b000		,MD,MB,MI,MS		)


#define INVD_()				_OO		(0x0f08								)
#define INVLPGm(MD, MB, MI, MS)		_OO_r_X		(0x0f01		     ,_b111		,MD,MB,MI,MS		)


#define JCCSim(CC,D,B,I,S)		((_r0P(B) && _r0P(I)) ? _O_D8	(0x70|(CC)		,(int)(D)		) : \
								JITFAIL("illegal mode in conditional jump"))

#define JOSm(D,B,I,S)			JCCSim(0x0,D,B,I,S)
#define JNOSm(D,B,I,S)			JCCSim(0x1,D,B,I,S)
#define JBSm(D,B,I,S)			JCCSim(0x2,D,B,I,S)
#define JNAESm(D,B,I,S)			JCCSim(0x2,D,B,I,S)
#define JNBSm(D,B,I,S)			JCCSim(0x3,D,B,I,S)
#define JAESm(D,B,I,S)			JCCSim(0x3,D,B,I,S)
#define JESm(D,B,I,S)			JCCSim(0x4,D,B,I,S)
#define JZSm(D,B,I,S)			JCCSim(0x4,D,B,I,S)
#define JNESm(D,B,I,S)			JCCSim(0x5,D,B,I,S)
#define JNZSm(D,B,I,S)			JCCSim(0x5,D,B,I,S)
#define JBESm(D,B,I,S)			JCCSim(0x6,D,B,I,S)
#define JNASm(D,B,I,S)			JCCSim(0x6,D,B,I,S)
#define JNBESm(D,B,I,S)			JCCSim(0x7,D,B,I,S)
#define JASm(D,B,I,S)			JCCSim(0x7,D,B,I,S)
#define JSSm(D,B,I,S)			JCCSim(0x8,D,B,I,S)
#define JNSSm(D,B,I,S)			JCCSim(0x9,D,B,I,S)
#define JPSm(D,B,I,S)			JCCSim(0xa,D,B,I,S)
#define JPESm(D,B,I,S)			JCCSim(0xa,D,B,I,S)
#define JNPSm(D,B,I,S)			JCCSim(0xb,D,B,I,S)
#define JPOSm(D,B,I,S)			JCCSim(0xb,D,B,I,S)
#define JLSm(D,B,I,S)			JCCSim(0xc,D,B,I,S)
#define JNGESm(D,B,I,S)			JCCSim(0xc,D,B,I,S)
#define JNLSm(D,B,I,S)			JCCSim(0xd,D,B,I,S)
#define JGESm(D,B,I,S)			JCCSim(0xd,D,B,I,S)
#define JLESm(D,B,I,S)			JCCSim(0xe,D,B,I,S)
#define JNGSm(D,B,I,S)			JCCSim(0xe,D,B,I,S)
#define JNLESm(D,B,I,S)			JCCSim(0xf,D,B,I,S)
#define JGSm(D,B,I,S)			JCCSim(0xf,D,B,I,S)

#define JCCim(CC,D,B,I,S)		((_r0P(B) && _r0P(I)) ? _OO_D32	(0x0f80|(CC)		,(int)(D)		) : \
								JITFAIL("illegal mode in conditional jump"))

#define JOm(D,B,I,S)			JCCim(0x0,D,B,I,S)
#define JNOm(D,B,I,S)			JCCim(0x1,D,B,I,S)
#define JBm(D,B,I,S)			JCCim(0x2,D,B,I,S)
#define JNAEm(D,B,I,S)			JCCim(0x2,D,B,I,S)
#define JNBm(D,B,I,S)			JCCim(0x3,D,B,I,S)
#define JAEm(D,B,I,S)			JCCim(0x3,D,B,I,S)
#define JEm(D,B,I,S)			JCCim(0x4,D,B,I,S)
#define JZm(D,B,I,S)			JCCim(0x4,D,B,I,S)
#define JNEm(D,B,I,S)			JCCim(0x5,D,B,I,S)
#define JNZm(D,B,I,S)			JCCim(0x5,D,B,I,S)
#define JBEm(D,B,I,S)			JCCim(0x6,D,B,I,S)
#define JNAm(D,B,I,S)			JCCim(0x6,D,B,I,S)
#define JNBEm(D,B,I,S)			JCCim(0x7,D,B,I,S)
#define JAm(D,B,I,S)			JCCim(0x7,D,B,I,S)
#define JSm(D,B,I,S)			JCCim(0x8,D,B,I,S)
#define JNSm(D,B,I,S)			JCCim(0x9,D,B,I,S)
#define JPm(D,B,I,S)			JCCim(0xa,D,B,I,S)
#define JPEm(D,B,I,S)			JCCim(0xa,D,B,I,S)
#define JNPm(D,B,I,S)			JCCim(0xb,D,B,I,S)
#define JPOm(D,B,I,S)			JCCim(0xb,D,B,I,S)
#define JLm(D,B,I,S)			JCCim(0xc,D,B,I,S)
#define JNGEm(D,B,I,S)			JCCim(0xc,D,B,I,S)
#define JNLm(D,B,I,S)			JCCim(0xd,D,B,I,S)
#define JGEm(D,B,I,S)			JCCim(0xd,D,B,I,S)
#define JLEm(D,B,I,S)			JCCim(0xe,D,B,I,S)
#define JNGm(D,B,I,S)			JCCim(0xe,D,B,I,S)
#define JNLEm(D,B,I,S)			JCCim(0xf,D,B,I,S)
#define JGm(D,B,I,S)			JCCim(0xf,D,B,I,S)


#define JMPSm(D,B,I,S)			((_r0P(B) && _r0P(I)) ? _O_D8	(0xeb			,(int)(D)		) : \
								JITFAIL("illegal mode in short jump"))

#define JMPm(D,B,I,S)			((_r0P(B) && _r0P(I)) ? _O_D32	(0xe9			,(int)(D)		) : \
								JITFAIL("illegal mode in direct jump"))

#define JMPsr(R)			_O_Mrm	(0xff	,_b11,_b100,_r4(R)			)

#define JMPsm(D,B,I,S)			_O_r_X	(0xff	     ,_b100	,(int)(D),B,I,S		)


#define LAHF_()				_O		(0x9f								)
#define LEALmr(MD, MB, MI, MS, RD)	_O_r_X		(0x8d		     ,_r4(RD)		,MD,MB,MI,MS		)
#define LEAVE_()			_O		(0xc9								)


#define LMSWr(RS)			_OO_Mrm		(0x0f01		,_b11,_b110,_r4(RS)				)
#define LMSWm(MD,MB,MI,MS)		_OO_r_X		(0x0f01		     ,_b110		,MD,MB,MI,MS		)

#define LOOPm(MD,MB,MI,MS)		((_r0P(MB) && _r0P(MI)) ? _O_D8 (0xe2			,MD			) : \
								  JITFAIL("illegal mode in loop"))

#define LOOPEm(MD,MB,MI,MS)		((_r0P(MB) && _r0P(MI)) ? _O_D8 (0xe1			,MD			) : \
								  JITFAIL("illegal mode in loope"))

#define LOOPZm(MD,MB,MI,MS)		((_r0P(MB) && _r0P(MI)) ? _O_D8 (0xe1			,MD			) : \
								  JITFAIL("illegal mode in loopz"))

#define LOOPNEm(MD,MB,MI,MS)		((_r0P(MB) && _r0P(MI)) ? _O_D8 (0xe0			,MD			) : \
								  JITFAIL("illegal mode in loopne"))

#define LOOPNZm(MD,MB,MI,MS)		((_r0P(MB) && _r0P(MI)) ? _O_D8 (0xe0			,MD			) : \
								  JITFAIL("illegal mode in loopnz"))


#define MOVBrr(RS, RD)			_O_Mrm		(0x80		,_b11,_r1(RS),_r1(RD)				)
#define MOVBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x8a		     ,_r1(RD)		,MD,MB,MI,MS		)
#define MOVBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x88		     ,_r1(RS)		,MD,MB,MI,MS		)
#define MOVBir(IM,  R)			_Or_B		(0xb0,_r1(R)						,_su8(IM))
#define MOVBim(IM, MD, MB, MI, MS)	_O_X_B		(0xc6					,MD,MB,MI,MS	,_su8(IM))

#define MOVWrr(RS, RD)			_wO_Mrm		(0x89		,_b11,_r2(RS),_r2(RD)				)
#define MOVWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x8b		     ,_r2(RD)		,MD,MB,MI,MS		)
#define MOVWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x89		     ,_r2(RS)		,MD,MB,MI,MS		)
#define MOVWir(IM,  R)			_wOr_W		(0xb8,_r2(R)						,_su16(IM))
#define MOVWim(IM, MD, MB, MI, MS)	_wO_X_W		(0xc7					,MD,MB,MI,MS	,_su16(IM))

#define MOVLrr(RS, RD)			_O_Mrm		(0x89		,_b11,_r4(RS),_r4(RD)				)
#define MOVLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x8b		     ,_r4(RD)		,MD,MB,MI,MS		)
#define MOVLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x89		     ,_r4(RS)		,MD,MB,MI,MS		)
#define MOVLir(IM,  R)			_Or_L		(0xb8,_r4(R)						,IM	)
#define MOVLim(IM, MD, MB, MI, MS)	_O_X_L		(0xc7					,MD,MB,MI,MS	,IM	)

#define MOVZBLrr(RS, RD)		_OO_Mrm		(0x0fb6		,_b11,_r1(RD),_r1(RS)				)
#define MOVZBLmr(MD, MB, MI, MS, RD)	_OO_r_X		(0x0fb6		     ,_r1(RD)		,MD,MB,MI,MS		)
#define MOVZBWrr(RS, RD)		_wOO_Mrm	(0x0fb6		,_b11,_r2(RD),_r2(RS)				)
#define MOVZBWmr(MD, MB, MI, MS, RD)	_wOO_r_X	(0x0fb6		     ,_r2(RD)		,MD,MB,MI,MS		)
#define MOVZWLrr(RS, RD)		_OO_Mrm		(0x0fb7		,_b11,_r1(RD),_r1(RS)				)
#define MOVZWLmr(MD, MB, MI, MS, RD)	_OO_r_X		(0x0fb7		     ,_r1(RD)		,MD,MB,MI,MS		)

#define MOVSBLrr(RS, RD)		_OO_Mrm		(0x0fbe		,_b11,_r1(RD),_r1(RS)				)
#define MOVSBLmr(MD, MB, MI, MS, RD)	_OO_r_X		(0x0fbe		     ,_r1(RD)		,MD,MB,MI,MS		)
#define MOVSBWrr(RS, RD)		_wOO_Mrm	(0x0fbe		,_b11,_r2(RD),_r2(RS)				)
#define MOVSBWmr(MD, MB, MI, MS, RD)	_wOO_r_X	(0x0fbe		     ,_r2(RD)		,MD,MB,MI,MS		)
#define MOVSWLrr(RS, RD)		_OO_Mrm		(0x0fbf		,_b11,_r1(RD),_r1(RS)				)
#define MOVSWLmr(MD, MB, MI, MS, RD)	_OO_r_X		(0x0fbf		     ,_r1(RD)		,MD,MB,MI,MS		)


#define MULBr(RS)			_O_Mrm		(0xf6		,_b11,_b100  ,_r1(RS)				)
#define MULBm(MD,MB,MI,MS)		_O_r_X		(0xf6		     ,_b100		,MD,MB,MI,MS		)

#define MULWr(RS)			_wO_Mrm 	(0xf7		,_b11,_b100  ,_r2(RS)				)
#define MULWm(MD,MB,MI,MS)		_wO_r_X 	(0xf7		     ,_b100		,MD,MB,MI,MS		)

#define MULLr(RS)			_O_Mrm		(0xf7		,_b11,_b100  ,_r4(RS)				)
#define MULLm(MD,MB,MI,MS)		_O_r_X		(0xf7		     ,_b100		,MD,MB,MI,MS		)


#define NEGBr(RD)			_O_Mrm		(0xf6		,_b11,_b011  ,_r1(RD)				)
#define NEGBm(MD,MB,MI,MS)		_O_r_X		(0xf6		     ,_b011		,MD,MB,MI,MS		)

#define NEGWr(RD)			_wO_Mrm		(0xf7		,_b11,_b011  ,_r2(RD)				)
#define NEGWm(MD,MB,MI,MS)		_wO_r_X		(0xf7		     ,_b011		,MD,MB,MI,MS		)

#define NEGLr(RD)			_O_Mrm		(0xf7		,_b11,_b011  ,_r4(RD)				)
#define NEGLm(MD,MB,MI,MS)		_O_r_X		(0xf7		     ,_b011		,MD,MB,MI,MS		)


#define NOP_()				_O		(0x90								)


#define NOTBr(RD)			_O_Mrm		(0xf6		,_b11,_b010  ,_r1(RD)				)
#define NOTBm(MD,MB,MI,MS)		_O_r_X		(0xf6		     ,_b010		,MD,MB,MI,MS		)

#define NOTWr(RD)			_wO_Mrm		(0xf7		,_b11,_b010  ,_r2(RD)				)
#define NOTWm(MD,MB,MI,MS)		_wO_r_X		(0xf7		     ,_b010		,MD,MB,MI,MS		)

#define NOTLr(RD)			_O_Mrm		(0xf7		,_b11,_b010  ,_r4(RD)				)
#define NOTLm(MD,MB,MI,MS)		_O_r_X		(0xf7		     ,_b010		,MD,MB,MI,MS		)


#define ORBrr(RS, RD)			_O_Mrm		(0x08		,_b11,_r1(RS),_r1(RD)				)
#define ORBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x0a		     ,_r1(RD)		,MD,MB,MI,MS		)
#define ORBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x08		     ,_r1(RS)		,MD,MB,MI,MS		)
#define ORBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b001  ,_r1(RD)			,_su8(IM))
#define ORBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b001		,MD,MB,MI,MS	,_su8(IM))

#define ORWrr(RS, RD)			_wO_Mrm		(0x09		,_b11,_r2(RS),_r2(RD)				)
#define ORWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x0b		     ,_r2(RD)		,MD,MB,MI,MS		)
#define ORWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x09		     ,_r2(RS)		,MD,MB,MI,MS		)
#define ORWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b001  ,_r2(RD)			,_su16(IM))
#define ORWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b001		,MD,MB,MI,MS	,_su16(IM))

#define ORLrr(RS, RD)			_O_Mrm		(0x09		,_b11,_r4(RS),_r4(RD)				)
#define ORLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x0b		     ,_r4(RD)		,MD,MB,MI,MS		)
#define ORLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x09		     ,_r4(RS)		,MD,MB,MI,MS		)
#define ORLir(IM, RD)			_Os_Mrm_sL	(0x81		,_b11,_b001  ,_r4(RD)			,IM	)
#define ORLim(IM, MD, MB, MI, MS)	_Os_r_X_sL	(0x81		     ,_b001		,MD,MB,MI,MS	,IM	)


#define POPWr(RD)			_wOr		(0x58,_r2(RD)							)
#define POPWm(MD,MB,MI,MS)		_wO_r_X		(0x8f		     ,_b000		,MD,MB,MI,MS		)

#define POPLr(RD)			_Or		(0x58,_r4(RD)							)
#define POPLm(MD,MB,MI,MS)		_O_r_X		(0x8f		     ,_b000		,MD,MB,MI,MS		)


#define POPA_()				_wO		(0x61								)
#define POPAD_()			_O		(0x61								)

#define POPF_()				_wO		(0x9d								)
#define POPFD_()			_O		(0x9d								)


#define PUSHWr(R)			_wOr		(0x50,_r2(R)							)
#define PUSHWm(MD,MB,MI,MS)		_wO_r_X		(0xff,		     ,_b110		,MD,MB,MI,MS		)
#define PUSHWi(IM)			_wOs_sW		(0x68							,IM	)

#define PUSHLr(R)			_Or		(0x50,_r4(R)							)
#define PUSHLm(MD,MB,MI,MS)		_O_r_X		(0xff		     ,_b110		,MD,MB,MI,MS		)
#define PUSHLi(IM)			_Os_sL		(0x68							,IM	)


#define PUSHA_()			_wO		(0x60								)
#define PUSHAD_()			_O		(0x60								)

#define PUSHF_()			_O		(0x9c								)
#define PUSHFD_()			_wO		(0x9c								)

#define RET_()				_O		(0xc3								)
#define RETi(IM)			_O_W		(0xc2							,_su16(IM))


#define ROLBir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd0	,_b11,_b000,_r1(RD)				) : \
						_O_Mrm_B	(0xc0	,_b11,_b000,_r1(RD)			,_u8(IM) ) )
#define ROLBim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd0	     ,_b000		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc0	     ,_b000		,MD,MB,MI,MS	,_u8(IM) ) )
#define ROLBrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd2	,_b11,_b000,_r1(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define ROLBrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd2	     ,_b000		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )

#define ROLWir(IM,RD)		(((IM)==1) ?	_wO_Mrm (0xd1	,_b11,_b000,_r2(RD)				) : \
						_wO_Mrm_B	(0xc1	,_b11,_b000,_r2(RD)			,_u8(IM) ) )
#define ROLWim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_wO_r_X (0xd1	     ,_b000		,MD,MB,MI,MS		) : \
						_wO_r_X_B	(0xc1	     ,_b000		,MD,MB,MI,MS	,_u8(IM) ) )
#define ROLWrr(RS,RD)		(((RS)==_CL) ?	_wO_Mrm (0xd3	,_b11,_b000,_r2(RD)				) : \
						JITFAIL ("source register must be CL"					) )
#define ROLWrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_wO_r_X (0xd3	     ,_b000		,MD,MB,MI,MS		) : \
						JITFAIL ("source register must be CL"					) )

#define ROLLir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd1	,_b11,_b000,_r4(RD)				) : \
						_O_Mrm_B	(0xc1	,_b11,_b000,_r4(RD)			,_u8(IM) ) )
#define ROLLim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd1	     ,_b000		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc1	     ,_b000		,MD,MB,MI,MS	,_u8(IM) ) )
#define ROLLrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd3	,_b11,_b000,_r4(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define ROLLrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd3	     ,_b000		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )


#define RORBir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd0	,_b11,_b001,_r1(RD)				) : \
						_O_Mrm_B	(0xc0	,_b11,_b001,_r1(RD)			,_u8(IM) ) )
#define RORBim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd0	     ,_b001		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc0	     ,_b001		,MD,MB,MI,MS	,_u8(IM) ) )
#define RORBrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd2	,_b11,_b001,_r1(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define RORBrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd2	     ,_b001		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )

#define RORWir(IM,RD)		(((IM)==1) ?	_wO_Mrm (0xd1	,_b11,_b001,_r2(RD)				) : \
						_wO_Mrm_B	(0xc1	,_b11,_b001,_r2(RD)			,_u8(IM) ) )
#define RORWim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_wO_r_X (0xd1	     ,_b001		,MD,MB,MI,MS		) : \
						_wO_r_X_B	(0xc1	     ,_b001		,MD,MB,MI,MS	,_u8(IM) ) )
#define RORWrr(RS,RD)		(((RS)==_CL) ?	_wO_Mrm (0xd3	,_b11,_b001,_r2(RD)				) : \
						JITFAIL ("source register must be CL"					) )
#define RORWrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_wO_r_X (0xd3	     ,_b001		,MD,MB,MI,MS		) : \
						JITFAIL ("source register must be CL"					) )

#define RORLir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd1	,_b11,_b001,_r4(RD)				) : \
						_O_Mrm_B	(0xc1	,_b11,_b001,_r4(RD)			,_u8(IM) ) )
#define RORLim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd1	     ,_b001		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc1	     ,_b001		,MD,MB,MI,MS	,_u8(IM) ) )
#define RORLrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd3	,_b11,_b001,_r4(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define RORLrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd3	     ,_b001		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )


#define SAHF_()					_O	(0x9e								)


#define SALBir	SHLBir
#define SALBim	SHLBim
#define SALBrr	SHLBrr
#define SALBrm	SHLBrm
#define SALWir	SHLWir
#define SALWim	SHLWim
#define SALWrr	SHLWrr
#define SALWrm	SHLWrm
#define SALLir	SHLLir
#define SALLim	SHLLim
#define SALLrr	SHLLrr
#define SALLrm	SHLLrm


#define SARBir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd0	,_b11,_b111,_r1(RD)				) : \
						_O_Mrm_B	(0xc0	,_b11,_b111,_r1(RD)			,_u8(IM) ) )
#define SARBim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd0	     ,_b111		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc0	     ,_b111		,MD,MB,MI,MS	,_u8(IM) ) )
#define SARBrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd2	,_b11,_b111,_r1(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SARBrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd2	     ,_b111		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )

#define SARWir(IM,RD)		(((IM)==1) ?	_wO_Mrm (0xd1	,_b11,_b111,_r2(RD)				) : \
						_wO_Mrm_B	(0xc1	,_b11,_b111,_r2(RD)			,_u8(IM) ) )
#define SARWim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_wO_r_X (0xd1	     ,_b111		,MD,MB,MI,MS		) : \
						_wO_r_X_B	(0xc1	     ,_b111		,MD,MB,MI,MS	,_u8(IM) ) )
#define SARWrr(RS,RD)		(((RS)==_CL) ?	_wO_Mrm (0xd3	,_b11,_b111,_r2(RD)				) : \
						JITFAIL ("source register must be CL"					) )
#define SARWrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_wO_r_X (0xd3	     ,_b111		,MD,MB,MI,MS		) : \
						JITFAIL ("source register must be CL"					) )

#define SARLir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd1	,_b11,_b111,_r4(RD)				) : \
						_O_Mrm_B	(0xc1	,_b11,_b111,_r4(RD)			,_u8(IM) ) )
#define SARLim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd1	     ,_b111		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc1	     ,_b111		,MD,MB,MI,MS	,_u8(IM) ) )
#define SARLrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd3	,_b11,_b111,_r4(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SARLrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd3	     ,_b111		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )


#define SBBBrr(RS, RD)			_O_Mrm		(0x18		,_b11,_r1(RS),_r1(RD)				)
#define SBBBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x1a		     ,_r1(RD)		,MD,MB,MI,MS		)
#define SBBBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x18		     ,_r1(RS)		,MD,MB,MI,MS		)
#define SBBBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b011  ,_r1(RD)			,_su8(IM))
#define SBBBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b011		,MD,MB,MI,MS	,_su8(IM))

#define SBBWrr(RS, RD)			_wO_Mrm 	(0x19		,_b11,_r2(RS),_r2(RD)				)
#define SBBWmr(MD, MB, MI, MS, RD)	_wO_r_X 	(0x1b		     ,_r2(RD)		,MD,MB,MI,MS		)
#define SBBWrm(RS, MD, MB, MI, MS)	_wO_r_X 	(0x19		     ,_r2(RS)		,MD,MB,MI,MS		)
#define SBBWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b011  ,_r2(RD)			,_su16(IM))
#define SBBWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b011		,MD,MB,MI,MS	,_su16(IM))

#define SBBLrr(RS, RD)			_O_Mrm		(0x19		,_b11,_r4(RS),_r4(RD)				)
#define SBBLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x1b		     ,_r4(RD)		,MD,MB,MI,MS		)
#define SBBLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x19		     ,_r4(RS)		,MD,MB,MI,MS		)
#define SBBLir(IM, RD)			_Os_Mrm_sL	(0x81		,_b11,_b011  ,_r4(RD)			,IM	)
#define SBBLim(IM, MD, MB, MI, MS)	_Os_r_X_sL	(0x81		     ,_b011		,MD,MB,MI,MS	,IM	)


#define SETCCir(CC,RD)			_OO_Mrm		(0x0f90|(CC)	,_b11,_b000,_r1(RD)				)

#define SETOr(RD)			SETCCir(0x0,RD)
#define SETNOr(RD)			SETCCir(0x1,RD)
#define SETBr(RD)			SETCCir(0x2,RD)
#define SETNAEr(RD)			SETCCir(0x2,RD)
#define SETNBr(RD)			SETCCir(0x3,RD)
#define SETAEr(RD)			SETCCir(0x3,RD)
#define SETEr(RD)			SETCCir(0x4,RD)
#define SETZr(RD)			SETCCir(0x4,RD)
#define SETNEr(RD)			SETCCir(0x5,RD)
#define SETNZr(RD)			SETCCir(0x5,RD)
#define SETBEr(RD)			SETCCir(0x6,RD)
#define SETNAr(RD)			SETCCir(0x6,RD)
#define SETNBEr(RD)			SETCCir(0x7,RD)
#define SETAr(RD)			SETCCir(0x7,RD)
#define SETSr(RD)			SETCCir(0x8,RD)
#define SETNSr(RD)			SETCCir(0x9,RD)
#define SETPr(RD)			SETCCir(0xa,RD)
#define SETPEr(RD)			SETCCir(0xa,RD)
#define SETNPr(RD)			SETCCir(0xb,RD)
#define SETPOr(RD)			SETCCir(0xb,RD)
#define SETLr(RD)			SETCCir(0xc,RD)
#define SETNGEr(RD)			SETCCir(0xc,RD)
#define SETNLr(RD)			SETCCir(0xd,RD)
#define SETGEr(RD)			SETCCir(0xd,RD)
#define SETLEr(RD)			SETCCir(0xe,RD)
#define SETNGr(RD)			SETCCir(0xe,RD)
#define SETNLEr(RD)			SETCCir(0xf,RD)
#define SETGr(RD)			SETCCir(0xf,RD)

#define SETCCim(CC,MD,MB,MI,MS)		_OO_r_X		(0x0f90|(CC)	     ,_b000		,MD,MB,MI,MS		)

#define SETOm(D,B,I,S)			SETCCim(0x0,D,B,I,S)
#define SETNOm(D,B,I,S)			SETCCim(0x1,D,B,I,S)
#define SETBm(D,B,I,S)			SETCCim(0x2,D,B,I,S)
#define SETNAEm(D,B,I,S)		SETCCim(0x2,D,B,I,S)
#define SETNBm(D,B,I,S)			SETCCim(0x3,D,B,I,S)
#define SETAEm(D,B,I,S)			SETCCim(0x3,D,B,I,S)
#define SETEm(D,B,I,S)			SETCCim(0x4,D,B,I,S)
#define SETZm(D,B,I,S)			SETCCim(0x4,D,B,I,S)
#define SETNEm(D,B,I,S)			SETCCim(0x5,D,B,I,S)
#define SETNZm(D,B,I,S)			SETCCim(0x5,D,B,I,S)
#define SETBEm(D,B,I,S)			SETCCim(0x6,D,B,I,S)
#define SETNAm(D,B,I,S)			SETCCim(0x6,D,B,I,S)
#define SETNBEm(D,B,I,S)		SETCCim(0x7,D,B,I,S)
#define SETAm(D,B,I,S)			SETCCim(0x7,D,B,I,S)
#define SETSm(D,B,I,S)			SETCCim(0x8,D,B,I,S)
#define SETNSm(D,B,I,S)			SETCCim(0x9,D,B,I,S)
#define SETPm(D,B,I,S)			SETCCim(0xa,D,B,I,S)
#define SETPEm(D,B,I,S)			SETCCim(0xa,D,B,I,S)
#define SETNPm(D,B,I,S)			SETCCim(0xb,D,B,I,S)
#define SETPOm(D,B,I,S)			SETCCim(0xb,D,B,I,S)
#define SETLm(D,B,I,S)			SETCCim(0xc,D,B,I,S)
#define SETNGEm(D,B,I,S)		SETCCim(0xc,D,B,I,S)
#define SETNLm(D,B,I,S)			SETCCim(0xd,D,B,I,S)
#define SETGEm(D,B,I,S)			SETCCim(0xd,D,B,I,S)
#define SETLEm(D,B,I,S)			SETCCim(0xe,D,B,I,S)
#define SETNGm(D,B,I,S)			SETCCim(0xe,D,B,I,S)
#define SETNLEm(D,B,I,S)		SETCCim(0xf,D,B,I,S)
#define SETGm(D,B,I,S)			SETCCim(0xf,D,B,I,S)


#define SHLBir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd0	,_b11,_b100,_r1(RD)				) : \
						_O_Mrm_B	(0xc0	,_b11,_b100,_r1(RD)			,_u8(IM) ) )
#define SHLBim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd0	     ,_b100		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc0	     ,_b100		,MD,MB,MI,MS	,_u8(IM) ) )
#define SHLBrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd2	,_b11,_b100,_r1(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SHLBrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd2	     ,_b100		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )

#define SHLWir(IM,RD)		(((IM)==1) ?	_wO_Mrm		(0xd1	,_b11,_b100,_r2(RD)				) : \
						_wO_Mrm_B	(0xc1	,_b11,_b100,_r2(RD)			,_u8(IM) ) )
#define SHLWim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_wO_r_X		(0xd1	     ,_b100		,MD,MB,MI,MS		) : \
						_wO_r_X_B	(0xc1	     ,_b100		,MD,MB,MI,MS	,_u8(IM) ) )
#define SHLWrr(RS,RD)		(((RS)==_CL) ?	_wO_Mrm		(0xd3	,_b11,_b100,_r2(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SHLWrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_wO_r_X		(0xd3	     ,_b100		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"					) )

#define SHLLir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd1	,_b11,_b100,_r4(RD)				) : \
						_O_Mrm_B	(0xc1	,_b11,_b100,_r4(RD)			,_u8(IM) ) )
#define SHLLim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd1	     ,_b100		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc1	     ,_b100		,MD,MB,MI,MS	,_u8(IM) ) )
#define SHLLrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd3	,_b11,_b100,_r4(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SHLLrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd3	     ,_b100		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )


#define SHRBir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd0	,_b11,_b101,_r1(RD)				) : \
						_O_Mrm_B	(0xc0	,_b11,_b101,_r1(RD)			,_u8(IM) ) )
#define SHRBim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd0	     ,_b101		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc0	     ,_b101		,MD,MB,MI,MS	,_u8(IM) ) )
#define SHRBrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd2	,_b11,_b101,_r1(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SHRBrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd2	     ,_b101		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )

#define SHRWir(IM,RD)		(((IM)==1) ?	_wO_Mrm		(0xd1	,_b11,_b101,_r2(RD)				) : \
						_wO_Mrm_B	(0xc1	,_b11,_b101,_r2(RD)			,_u8(IM) ) )
#define SHRWim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_wO_r_X		(0xd1	     ,_b101		,MD,MB,MI,MS		) : \
						_wO_r_X_B	(0xc1	     ,_b101		,MD,MB,MI,MS	,_u8(IM) ) )
#define SHRWrr(RS,RD)		(((RS)==_CL) ?	_wO_Mrm		(0xd3	,_b11,_b101,_r2(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SHRWrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_wO_r_X		(0xd3	     ,_b101		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )

#define SHRLir(IM,RD)		(((IM)==1) ?	_O_Mrm		(0xd1	,_b11,_b101,_r4(RD)				) : \
						_O_Mrm_B	(0xc1	,_b11,_b101,_r4(RD)			,_u8(IM) ) )
#define SHRLim(IM,MD,MB,MS,MI)	(((IM)==1) ?	_O_r_X		(0xd1	     ,_b101		,MD,MB,MI,MS		) : \
						_O_r_X_B	(0xc1	     ,_b101		,MD,MB,MI,MS	,_u8(IM) ) )
#define SHRLrr(RS,RD)		(((RS)==_CL) ?	_O_Mrm		(0xd3	,_b11,_b101,_r4(RD)				) : \
						JITFAIL		("source register must be CL"				) )
#define SHRLrm(RS,MD,MB,MS,MI)	(((RS)==_CL) ?	_O_r_X		(0xd3	     ,_b101		,MD,MB,MI,MS		) : \
						JITFAIL		("source register must be CL"				) )


#define STC_()				_O		(0xf9								)


#define SUBBrr(RS, RD)			_O_Mrm		(0x28		,_b11,_r1(RS),_r1(RD)				)
#define SUBBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x2a		     ,_r1(RD)		,MD,MB,MI,MS		)
#define SUBBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x28		     ,_r1(RS)		,MD,MB,MI,MS		)
#define SUBBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b101  ,_r1(RD)			,_su8(IM))
#define SUBBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b101		,MD,MB,MI,MS	,_su8(IM))

#define SUBWrr(RS, RD)			_wO_Mrm		(0x29		,_b11,_r2(RS),_r2(RD)				)
#define SUBWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x2b		     ,_r2(RD)		,MD,MB,MI,MS		)
#define SUBWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x29		     ,_r2(RS)		,MD,MB,MI,MS		)
#define SUBWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b101  ,_r2(RD)			,_su16(IM))
#define SUBWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b101		,MD,MB,MI,MS	,_su16(IM))

#define SUBLrr(RS, RD)			_O_Mrm		(0x29		,_b11,_r4(RS),_r4(RD)				)
#define SUBLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x2b		     ,_r4(RD)		,MD,MB,MI,MS		)
#define SUBLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x29		     ,_r4(RS)		,MD,MB,MI,MS		)
#define SUBLir(IM, RD)			_Os_Mrm_sL	(0x81		,_b11,_b101  ,_r4(RD)			,IM	)
#define SUBLim(IM, MD, MB, MI, MS)	_Os_r_X_sL	(0x81		     ,_b101		,MD,MB,MI,MS	,IM	)


#define TESTBrr(RS, RD)			_O_Mrm		(0x84		,_b11,_r1(RS),_r1(RD)				)
#define TESTBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x84		     ,_r1(RS)		,MD,MB,MI,MS		)
#define TESTBir(IM, RD)			_O_Mrm_B	(0xf6		,_b11,_b000  ,_r1(RD)			,_u8(IM))
#define TESTBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0xf6		     ,_b000		,MD,MB,MI,MS	,_u8(IM))

#define TESTWrr(RS, RD)			_wO_Mrm		(0x85		,_b11,_r2(RS),_r2(RD)				)
#define TESTWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x85		     ,_r2(RS)		,MD,MB,MI,MS		)
#define TESTWir(IM, RD)			_wO_Mrm_W	(0xf7		,_b11,_b000  ,_r2(RD)			,_u16(IM))
#define TESTWim(IM, MD, MB, MI, MS)	_wO_r_X_W	(0xf7		     ,_b000		,MD,MB,MI,MS	,_u16(IM))

#define TESTLrr(RS, RD)			_O_Mrm		(0x85		,_b11,_r4(RS),_r4(RD)				)
#define TESTLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x85		     ,_r4(RS)		,MD,MB,MI,MS		)
#define TESTLir(IM, RD)			_O_Mrm_L	(0xf7		,_b11,_b000  ,_r4(RD)			,IM	)
#define TESTLim(IM, MD, MB, MI, MS)	_O_r_X_L	(0xf7		     ,_b000		,MD,MB,MI,MS	,IM	)


#define XADDBrr(RS,RD)			_OO_Mrm		(0x0fc0		,_b11,_r1(RS),_r1(RD)				)
#define XADDBrm(RS,MD,MB,MI,MS)		_OO_r_X		(0x0fc0		     ,_r1(RS)		,MD,MB,MI,MS		)

#define XADDWrr(RS,RD)			_wOO_Mrm	(0x0fc1		,_b11,_r2(RS),_r2(RD)				)
#define XADDWrm(RS,MD,MB,MI,MS)		_wOO_r_X	(0x0fc1		     ,_r2(RS)		,MD,MB,MI,MS		)

#define XADDLrr(RS,RD)			_OO_Mrm		(0x0fc1		,_b11,_r4(RS),_r4(RD)				)
#define XADDLrm(RS,MD,MB,MI,MS)		_OO_r_X		(0x0fc1		     ,_r4(RS)		,MD,MB,MI,MS		)


#define XCHGBrr(RS,RD)			_O_Mrm		(0x86		,_b11,_r1(RS),_r1(RD)				)
#define XCHGBrm(RS,MD,MB,MI,MS)		_O_r_X		(0x86		     ,_r1(RS)		,MD,MB,MI,MS		)

#define XCHGWrr(RS,RD)			_wO_Mrm		(0x87		,_b11,_r2(RS),_r2(RD)				)
#define XCHGWrm(RS,MD,MB,MI,MS)		_wO_r_X		(0x87		     ,_r2(RS)		,MD,MB,MI,MS		)

#define XCHGLrr(RS,RD)			_O_Mrm		(0x87		,_b11,_r4(RS),_r4(RD)				)
#define XCHGLrm(RS,MD,MB,MI,MS)		_O_r_X		(0x87		     ,_r4(RS)		,MD,MB,MI,MS		)


#define XORBrr(RS, RD)			_O_Mrm		(0x30		,_b11,_r1(RS),_r1(RD)				)
#define XORBmr(MD, MB, MI, MS, RD)	_O_r_X		(0x32		     ,_r1(RD)		,MD,MB,MI,MS		)
#define XORBrm(RS, MD, MB, MI, MS)	_O_r_X		(0x30		     ,_r1(RS)		,MD,MB,MI,MS		)
#define XORBir(IM, RD)			_O_Mrm_B	(0x80		,_b11,_b110  ,_r1(RD)			,_su8(IM))
#define XORBim(IM, MD, MB, MI, MS)	_O_r_X_B	(0x80		     ,_b110		,MD,MB,MI,MS	,_su8(IM))

#define XORWrr(RS, RD)			_wO_Mrm		(0x31		,_b11,_r2(RS),_r2(RD)				)
#define XORWmr(MD, MB, MI, MS, RD)	_wO_r_X		(0x33		     ,_r2(RD)		,MD,MB,MI,MS		)
#define XORWrm(RS, MD, MB, MI, MS)	_wO_r_X		(0x31		     ,_r2(RS)		,MD,MB,MI,MS		)
#define XORWir(IM, RD)			_wOs_Mrm_sW	(0x81		,_b11,_b110  ,_r2(RD)			,_su16(IM))
#define XORWim(IM, MD, MB, MI, MS)	_wOs_r_X_sW	(0x81		     ,_b110		,MD,MB,MI,MS	,_su16(IM))

#define XORLrr(RS, RD)			_O_Mrm		(0x31		,_b11,_r4(RS),_r4(RD)				)
#define XORLmr(MD, MB, MI, MS, RD)	_O_r_X		(0x33		     ,_r4(RD)		,MD,MB,MI,MS		)
#define XORLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x31		     ,_r4(RS)		,MD,MB,MI,MS		)
#define XORLir(IM, RD)			_Os_Mrm_sL	(0x81		,_b11,_b110  ,_r4(RD)			,IM	)
#define XORLim(IM, MD, MB, MI, MS)	_Os_r_X_sL	(0x81		     ,_b110		,MD,MB,MI,MS	,IM	)

/* x87 instructions -- yay, we found a use for octal constants :-) */

#define ESCmi(D,B,I,S,OP)	_O_r_X(0xd8|(OP >> 3), (OP & 7), D,B,I,S)
#define ESCri(RD,OP)		_O_Mrm(0xd8|(OP >> 3), _b11, (OP & 7), RD)

#define ESCrri(RS,RD,OP)	((RS) == _ST0 ? ESCri(RD,(OP|040))			\
				 : (RD) == _ST0 ? ESCri(RS,OP)				\
				 : JITFAIL ("coprocessor instruction without st0"))

#define FLDSm(D,B,I,S)		ESCmi(D,B,I,S,010)     /* fld m32real  */
#define FILDLm(D,B,I,S)		ESCmi(D,B,I,S,030)     /* fild m32int  */
#define FLDLm(D,B,I,S)		ESCmi(D,B,I,S,050)     /* fld m64real  */
#define FILDWm(D,B,I,S)		ESCmi(D,B,I,S,070)     /* fild m16int  */
#define FSTSm(D,B,I,S)		ESCmi(D,B,I,S,012)     /* fst m32real  */
#define FISTLm(D,B,I,S)		ESCmi(D,B,I,S,032)     /* fist m32int  */
#define FSTLm(D,B,I,S)		ESCmi(D,B,I,S,052)     /* fst m64real  */
#define FISTWm(D,B,I,S)		ESCmi(D,B,I,S,072)     /* fist m16int  */
#define FSTPSm(D,B,I,S)		ESCmi(D,B,I,S,013)     /* fstp m32real */
#define FISTPLm(D,B,I,S)	ESCmi(D,B,I,S,033)     /* fistp m32int */
#define FSTPLm(D,B,I,S)		ESCmi(D,B,I,S,053)     /* fstp m64real */
#define FISTPWm(D,B,I,S)	ESCmi(D,B,I,S,073)     /* fistp m16int */
#define FLDTm(D,B,I,S)		ESCmi(D,B,I,S,035)     /* fld m80real  */
#define FILDQm(D,B,I,S)		ESCmi(D,B,I,S,075)     /* fild m64int  */
#define FSTPTm(D,B,I,S)		ESCmi(D,B,I,S,037)     /* fstp m80real */
#define FISTPQm(D,B,I,S)	ESCmi(D,B,I,S,077)     /* fistp m64int */

#define FADDrr(RS,RD)		ESCrri(RS,RD,000)
#define FMULrr(RS,RD)		ESCrri(RS,RD,001)
#define FSUBrr(RS,RD)		ESCrri(RS,RD,004)
#define FSUBRrr(RS,RD)		ESCrri(RS,RD,005)
#define FDIVrr(RS,RD)		ESCrri(RS,RD,006)
#define FDIVRrr(RS,RD)		ESCrri(RS,RD,007)

#define FLDr(RD)		ESCri(RD,010)
#define FXCHr(RD)		ESCri(RD,011)
#define FFREEr(RD)		ESCri(RD,050)
#define FSTr(RD)		ESCri(RD,052)
#define FSTPr(RD)		ESCri(RD,053)
#define FCOMr(RD)		ESCri(RD,002)
#define FCOMPr(RD)		ESCri(RD,003)
#define FCOMIr(RD)		ESCri(RD,036)
#define FCOMIPr(RD)		ESCri(RD,076)
#define FUCOMr(RD)		ESCri(RD,054)
#define FUCOMPr(RD)		ESCri(RD,055)
#define FUCOMIr(RD)		ESCri(RD,035)
#define FUCOMIPr(RD)		ESCri(RD,075)
#define FADDPr(RD)		ESCri(RD,060)
#define FMULPr(RD)		ESCri(RD,061)
#define FSUBPr(RD)		ESCri(RD,064)
#define FSUBRPr(RD)		ESCri(RD,065)
#define FDIVPr(RD)		ESCri(RD,066)
#define FDIVRPr(RD)		ESCri(RD,067)

#define FNSTSWr(RD)		((RD == _AX || RD == _EAX) ? _OO (0xdfe0)		\
				 : JITFAIL ("AX or EAX expected"))
/* N byte NOPs */
#define NOPi(N)		(((  (N)    >= 8) ? (_jit_B(0x8d),_jit_B(0xb4),_jit_B(0x26),_jit_L(0x00),_jit_B(0x90)) : (void) 0), \
			 (( ((N)&7) == 7) ? (_jit_B(0x8d),_jit_B(0xb4),_jit_B(0x26),_jit_L(0x00)) : \
			  ( ((N)&7) == 6) ? (_jit_B(0x8d),_jit_B(0xb6),_jit_L(0x00)) : \
			  ( ((N)&7) == 5) ? (_jit_B(0x90),_jit_B(0x8d),_jit_B(0x74),_jit_B(0x26),_jit_B(0x00)) : \
/* leal 0(,%esi), %esi */ ( ((N)&7) == 4) ? (_jit_B(0x8d),_jit_B(0x74),_jit_B(0x26),_jit_B(0x00)) : \
/* leal (,%esi), %esi */  ( ((N)&7) == 3) ? (_jit_B(0x8d),_jit_B(0x76),_jit_B(0x00)) : \
/* movl %esi, %esi */	  ( ((N)&7) == 2) ? (_jit_B(0x89),_jit_B(0xf6)) : \
			  ( ((N)&7) == 1) ? (_jit_B(0x90)) : \
			  ( ((N)&7) == 0) ? 0 : \
			  JITFAIL(".align argument too large")))


/*** References:										*/
/*												*/
/* [1] "Intel Architecture Software Developer's Manual Volume 1: Basic Architecture",		*/
/*     Intel Corporation 1997.									*/
/*												*/
/* [2] "Intel Architecture Software Developer's Manual Volume 2: Instruction Set Reference",	*/
/*     Intel Corporation 1997.									*/

#endif
#endif /* __lightning_asm_h */

