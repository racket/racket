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

#define _R12            0x4C
#define _R13            0x4D
#define JIT_CALLTMPSTART 0x48
#define JIT_REXTMP       0x4B

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
#define _qrN(R)		((R)&0xF)
#define _r0P(R)		((R)==0)

#ifndef _ASM_SAFETY
#define _r1(R)		_rN(R)
#define _r2(R)		_rN(R)
#define _r4(R)		_rN(R)
#define _r8(R)		_qrN(R)
#else
#define _r1(R)		((_rS(R)==1) ? _rN(R) : JITFAIL( "8-bit register required"))
#define _r2(R)		((_rS(R)==2) ? _rN(R) : JITFAIL("16-bit register required"))
#define _r4(R)		((_rS(R)==4) ? _rN(R) : JITFAIL("32-bit register required"))
#define _r8(R)		((_rS(R)==4) ? _rN(R) : JITFAIL("64-bit register required"))
#endif

/*** ASSEMBLER ***/

#define _OFF4(D)        (_jit_UL(D) - _jit_UL(_jit.x.pc))
#define _CKD8(D)        _ck_d(8, ((_sc) _OFF4(D)) )

#define _D8(D)          (_jit_B(0), ((*(_PUC(_jit.x.pc)-1))= _CKD8(D)))
#define _D32(D)         (_jit_I(0), ((*(_PUI(_jit.x.pc)-1))= _OFF4(D)))

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
#ifdef JIT_X86_64
# define _qMrm(Md,R,M)	_jit_B((_M(Md)<<6)|(_r((R & 0x7))<<3)|_m((M & 0x7)))
#else
# define _qMrm(Md,R,M)  _Mrm(Md,R,M)
#endif

#define _SIB(Sc,I, B)	_jit_B((_s(Sc)<<6)|(_i(I)<<3)|_b(B))

#define _SCL(S)		((((S)==1) ? _b00 : \
			 (((S)==2) ? _b01 : \
			 (((S)==4) ? _b10 : \
			 (((S)==8) ? _b11 : JITFAIL("illegal scale: " #S))))))

/* memory subformats - urgh! */

#ifdef JIT_X86_64
# define _r_D(	R, D	  )	(_Mrm(_b00,_rN(R),_b100 ),_SIB(0,_b100,_b101)	     ,_jit_I((intptr_t)(D)))
# define _r_Q(	R, D	  )	(_qMrm(_b00,_rN(R),_b100 ),_SIB(0,_b100,_b101)        ,_jit_I((intptr_t)(D)))
#else
# define _r_D(	R, D	  )	(_Mrm(_b00,_rN(R),_b101 )		             ,_jit_I((intptr_t)(D)))
# define _r_Q(R, D) _r_D(R, D)
#endif
#define _r_0B(	R,   B    )	(_Mrm(_b00,_rN(R),_r4(B))			           )
#define _r_0BIS(R,   B,I,S)	(_Mrm(_b00,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_r4(B))      )
#define _r_1B(	R, D,B    )	(_Mrm(_b01,_rN(R),_r4(B))		             ,_jit_B((intptr_t)(D)))
#define _r_1BIS(R, D,B,I,S)	(_Mrm(_b01,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_r4(B)),_jit_B((intptr_t)(D)))
#define _r_4B(	R, D,B    )	(_Mrm(_b10,_rN(R),_r4(B))		             ,_jit_I((intptr_t)(D)))
#define _r_4IS( R, D,I,S)	(_Mrm(_b00,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_b101 ),_jit_I((intptr_t)(D)))
#define _r_4BIS(R, D,B,I,S)	(_Mrm(_b10,_rN(R),_b100 ),_SIB(_SCL(S),_r4(I),_r4(B)),_jit_I((intptr_t)(D)))
#define _r_8B(	R, D,B    )	(_qMrm(_b10,_rN(R),_r8(B))		             ,_jit_I((intptr_t)(D)))
#define _r_8IS( R, D,I,S)	(_qMrm(_b00,_rN(R),_b100 ),_SIB(_SCL(S),_r8(I),_b101 ),_jit_I((intptr_t)(D)))
#define _r_8BIS(R, D,B,I,S)	(_qMrm(_b10,_rN(R),_b100 ),_SIB(_SCL(S),_r8(I),_r8(B)),_jit_I((intptr_t)(D)))

#define _r_DB(  R, D,B    )	((_s0P(D) && (B != _EBP) ? _r_0B  (R,  B    ) : (_s8P(D) ? _r_1B(  R,D,B    ) : _r_4B(  R,D,B    ))))
#define _r_DBIS(R, D,B,I,S)	((_s0P(D)		 ? _r_0BIS(R,  B,I,S) : (_s8P(D) ? _r_1BIS(R,D,B,I,S) : _r_4BIS(R,D,B,I,S))))
#define _r_QB(  R, D,B    )	((_s0P(D) && (B != _EBP) ? _r_0B  (R,  B    ) : (_s8P(D) ? _r_1B(  R,D,B    ) : _r_8B(  R,D,B    ))))
#define _r_QBIS(R, D,B,I,S)	((_s0P(D)		 ? _r_0BIS(R,  B,I,S) : (_s8P(D) ? _r_1BIS(R,D,B,I,S) : _r_8BIS(R,D,B,I,S))))
 
#define _r_X(   R, D,B,I,S)	(_r0P(I) ? (_r0P(B)   ? _r_D   (R,D            )   : \
				           (_ESP==(B) ? _r_DBIS(R,D,_ESP,_ESP,1)   : \
						        _r_DB  (R,D,   B       ))) : \
				 (_r0P(B)	      ? _r_4IS (R,D,	    I,S)   : \
				 (((I)!=_ESP)         ? _r_DBIS(R,D,   B,   I,S)   : \
						        JITFAIL("illegal index register: %esp"))))
#define _qr_X(   R, D,B,I,S)	(_r0P(I) ? (_r0P(B)   ? _r_Q   (R,D            )   : \
				           (_ESP==(B) ? _r_QBIS(R,D,_ESP,_ESP,1)   : \
						        _r_QB  (R,D,   B       ))) : \
				 (_r0P(B)	      ? _r_8IS (R,D,	    I,S)   : \
				 (((I)!=_ESP)         ? _r_QBIS(R,D,   B,   I,S)   : \
						        JITFAIL("illegal index register: %esp"))))


/* instruction formats */

/*	 _format						     Opcd	  ModR/M dN(rB,rI,Sc)	  imm... */

#define	 _d16()					   (		  _jit_B(0x66	)				  )
#define	  _O(	     OP				)  (		  _jit_B(  OP	)				  )
#ifdef JIT_X86_64
# define  _REX_(P,R,X,B)                          ( _jit_B(P|((R&0x8)>>1)|((X&0x8)>>2)|((B&0x8)>>3)) )
# define  _REX(R,X,B)                              _REX_(0x48,R,X,B)
# define  _REXd(R,X,B)                             ((B&0x8) ? _REX_(0x40,R,X,B) : 0)
# define  _qO(	     OP, R,X,B			)  ( _REX(R,X,B), _jit_B(  OP	) )
# define  _qOd(	     OP, R,X,B			)  ( _REXd(R,X,B), _jit_B(  OP	) )
#else
# define  _qO(	     OP, R,X,B  		)  _O(OP)
# define  _qOd(	     OP, R,X,B  		)  _O(OP)
#endif
#define	  _Or(	     OP,R			)  (		  _jit_B( (OP)|_r(R))				  )
#ifdef JIT_X86_64
# define  _qOr(	     OP,R			)  ( _REX(0,0,R), _jit_B( (OP)|_r(R&0x7))				  )
# define  _qOdr(     OP,R			)  ( _REXd(0,0,R), _jit_B( (OP)|_r(R&0x7))				  )
#else
# define _qOr(       OP,R                       ) _Or(OP,R) 
# define _qOdr(       OP,R                       ) _Or(OP,R) 
#endif
#define	 _OO(	     OP				)  ( _jit_B((OP)>>8), _jit_B( (OP)	)				  )
#ifdef JIT_X86_64
# define _qOO(OP)  ( _REX(0,0,0), _OO(OP))
#else
# define _qOO(OP) _OO(OP)
#endif
#define	 _OOr(	     OP,R			)  ( _jit_B((OP)>>8), _jit_B( (OP)|_r(R))				  )
#define	  _Os(	     OP,B			)  (	_s8P(B) ? _jit_B(((OP)|_b10)) : _jit_B(OP)			  )
#ifdef JIT_X86_64
# define  _qOs(	     OP, B, R, M	       	)  ( _REX(0, M, R), _Os(OP, B) )
#else
# define  _qOs(	     OP, B, R, M	       	)  _Os(OP, B)
#endif
#define	    _sW(			     W	)  (				       _s8P(W) ? _jit_B(W):_jit_W(W)	  )
#define	    _sL(			     L	)  (				       _s8P(L) ? _jit_B(L):_jit_I(L)	  )
#define	  _O_W(	     OP			    ,W	)  (	    _O	    (  OP  )			      ,_jit_W(W)	  )
#define	  _O_D8(     OP			    ,D	)  (	    _O	    (  OP  )			     ,_D8(D)	  )
#define	  _O_D32(     OP		    ,D	)  (	    _O	    (  OP  )			     ,_D32(D)	  )
#define	 _OO_D32(     OP		    ,D	)  (	   _OO	    (  OP  )			     ,_D32(D)	  )
#define	  _Os_sW(    OP			    ,W	)  (	    _Os	    (  OP,W)			     ,_sW(W)	  )
#define	  _Os_sL(    OP			    ,L	)  (	    _Os	    (  OP,L)			     ,_sL(L)	  )
#define	  _O_W_B(    OP			    ,W,B)  (	    _O	    (  OP  )			      ,_jit_W(W),_jit_B(B))
#define	  _Or_B(     OP,R		    ,B	)  (	    _Or	    (  OP,R)			      ,_jit_B(B)	  )
#define	  _Or_W(     OP,R		    ,W	)  (	    _Or	    (  OP,R)			      ,_jit_W(W)	  )
#define	  _Or_L(     OP,R		    ,L	)  (	    _Or	    (  OP,R)			      ,_jit_I(L)	  )
#define	  _qOr_Q(     OP,R		    ,Q	)  (	   _qOr	    (  OP,R)			      ,_jit_L(Q)	  )
#define	  _O_Mrm(    OP	 ,MO,R,M		)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    )		  )
#define	 _qO_Mrm(    OP	 ,MO,R,M		)  (	    _qO	    (  OP,R,0,M),_qMrm(MO,R,M	    )		  )
#define	 _qOd_Mrm(   OP	 ,MO,R,M		)  (	    _qOd    (  OP,R,0,M),_qMrm(MO,R,M	    )		  )
#define	 _OO_Mrm(    OP	 ,MO,R,M		)  (	   _OO	    (  OP  ),_Mrm(MO,R,M	    )		  )
#define	 _qOO_Mrm(   OP	 ,MO,R,M		)  (	   _qOO	    (  OP  ),_Mrm(MO,R,M	    )		  )
#define	  _O_Mrm_B(  OP	 ,MO,R,M	    ,B	)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_B(B)	  )
#define	 _qO_Mrm_B(  OP	 ,MO,R,M	    ,B	)  (	    _qO	    (  OP,R,0,M),_qMrm(MO,R,M	    ) ,_jit_B(B)	  )
#define	  _O_Mrm_W(  OP	 ,MO,R,M	    ,W	)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_W(W)	  )
#define	  _O_Mrm_L(  OP	 ,MO,R,M	    ,L	)  (	    _O	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_I(L)	  )
#define	 _qO_Mrm_L(  OP	 ,MO,R,M	    ,L	)  (	   _qO	    (  OP,R,0,M),_qMrm(MO,R,M	    ) ,_jit_I(L)	  )
#define	 _qO_Mrm_Q(  OP	 ,MO,R,M	    ,Q	)  (	   _qO	    (  OP,0,0,R),_qMrm(MO,R,M	    ) ,_jit_L(Q)	  )
#define	 _OO_Mrm_B(  OP	 ,MO,R,M	    ,B	)  (	   _OO	    (  OP  ),_Mrm(MO,R,M	    ) ,_jit_B(B)	  )
#define	  _Os_Mrm_sW(OP	 ,MO,R,M	    ,W	)  (	    _Os	    (  OP,W),_Mrm(MO,R,M	    ),_sW(W)	  )
#define	  _Os_Mrm_sL(OP	 ,MO,R,M	    ,L	)  (	    _Os	    (  OP,L),_Mrm(MO,R,M	    ),_sL(L)	  )
#define	 _qOs_Mrm_sL(OP	 ,MO,R,M	    ,L	)  (	   _qOs	    (  OP,L,R,M),_qMrm(MO,R,M	    ),_sL(L)	  )
#define	  _O_r_X(    OP	    ,R	,MD,MB,MI,MS	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS)		  )
#define	 _qO_r_X(    OP	    ,R	,MD,MB,MI,MS	)  (	   _qO	    (  OP,R,0,MS),_qr_X(R,MD,MB,MI,MS)		  )
#define	 _qO_r_XB(   OP	    ,R	,MD,MB,MI,MS	)  (	   _qO	    (  OP,R,0,MB),_qr_X(R,MD,MB,MI,MS)		  )
#define	 _qOd_r_X(   OP	    ,R	,MD,MB,MI,MS	)  (	   _qOd	    (  OP,R,0,MB),_qr_X(R,MD,MB,MI,MS)		  )
#define	 _OO_r_X(    OP	    ,R	,MD,MB,MI,MS	)  (	   _OO	    (  OP  ),_r_X(   R	,MD,MB,MI,MS)		  )
#define	 _qOO_r_X(    OP    ,R	,MD,MB,MI,MS	)  (	   _qOO	    (  OP  ),_r_X(   R	,MD,MB,MI,MS)		  )
#define	  _O_r_X_B(  OP	    ,R	,MD,MB,MI,MS,B	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_B(B)	  )
#define	  _O_r_X_W(  OP	    ,R	,MD,MB,MI,MS,W	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_W(W)	  )
#define	  _O_r_X_L(  OP	    ,R	,MD,MB,MI,MS,L	)  (	    _O	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_I(L)	  )
#define	  _qO_r_X_L( OP	    ,R	,MD,MB,MI,MS,L	)  (	    _qO	    (  OP,R,0,MB),_r_X(   R	,MD,MB,MI,MS) ,_jit_I(L)	  )
#define	 _OO_r_X_B(  OP	    ,R	,MD,MB,MI,MS,B	)  (	   _OO	    (  OP  ),_r_X(   R	,MD,MB,MI,MS) ,_jit_B(B)	  )
#define	  _Os_r_X_sW(OP	    ,R	,MD,MB,MI,MS,W	)  (	    _Os	    (  OP,W),_r_X(   R	,MD,MB,MI,MS),_sW(W)	  )
#define	  _Os_r_X_sL(OP	    ,R	,MD,MB,MI,MS,L	)  (	    _Os	    (  OP,L),_r_X(   R	,MD,MB,MI,MS),_sL(L)	  )
#define	  _O_X_B(    OP		,MD,MB,MI,MS,B	)  (	    _O_r_X_B(  OP	    ,0	,MD,MB,MI,MS	 ,B)	  )
#define	  _O_X_W(    OP		,MD,MB,MI,MS,W	)  (	    _O_r_X_W(  OP	    ,0	,MD,MB,MI,MS	 ,W)	  )
#define	  _O_X_L(    OP		,MD,MB,MI,MS,L	)  (	    _O_r_X_L(  OP	    ,0	,MD,MB,MI,MS	 ,L)	  )
#define	  _qO_X_L(   OP		,MD,MB,MI,MS,L	)  (	    _qO_r_X_L(  OP	    ,0	,MD,MB,MI,MS	 ,L)	  )
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

#define ADDQrr(RS, RD)			_qO_Mrm		(0x01		,_b11,_r8(RS),_r8(RD)				)
#define ADDQir(IM, RD)			_qOs_Mrm_sL	(0x81		,_b11,_b000  ,_r8(RD)			,IM	)

#define ADDQiBr(IM, RD)			_qO_Mrm_B	(0x83		,_b11,_b000  ,_r1(RD)			,_su8(IM))

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

#define ANDQrr(RS, RD)			_qO_Mrm		(0x21		,_b11,_r8(RS),_r8(RD)				)
#define ANDQir(IM, RD)			_qOs_Mrm_sL	(0x81		,_b11,_b100  ,_r8(RD)			,IM	)

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

#ifdef _ASM_SAFETY
# define CALLmL(D,B,I,S)		((_r0P(B) && _r0P(I)) ? _O_D32	(0xe8			,(intptr_t)(D)		) : \
								JITFAIL("illegal mode in direct jump"))
#else
# define CALLmL(D,B,I,S)		_O_D32	(0xe8			,(intptr_t)(D)		)
#endif

#ifdef JIT_X86_64
# define CALLm(D,B,I,S)	                (MOVQir((D), JIT_REXTMP), CALQsr(JIT_REXTMP))
#else
# define CALLm(D,B,I,S)			CALLmL(D,B,I,S)
#endif

#define CALLsr(R)			_O_Mrm	(0xff	,_b11,_b010,_r4(R)			)
#define CALQsr(R)                       _qOd_Mrm(0xff	,_b11,_b010,_r8(R))

#define CALLsm(D,B,I,S)			_O_r_X	(0xff	     ,_b010	,(intptr_t)(D),B,I,S		)

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

#define CMPQrr(RS, RD)			_qO_Mrm		(0x39		,_b11,_r8(RS),_r8(RD)				)
#define CMPQir(IM, RD)			_qO_Mrm_L	(0x81		,_b11,_b111  ,_r8(RD)			,IM	)

#define CWD_()				_O		(0x99								)


#define CMPXCHGBrr(RS,RD)		_OO_Mrm		(0x0fb0		,_b11,_r1(RS),_r1(RD)				)
#define CMPXCHGBrm(RS,MD,MB,MI,MS)	_OO_r_X		(0x0fb0		     ,_r1(RS)		,MD,MB,MI,MS		)

#define CMPXCHGWrr(RS,RD)		_wOO_Mrm	(0x0fb1		,_b11,_r2(RS),_r2(RD)				)
#define CMPXCHGWrm(RS,MD,MB,MI,MS)	_wOO_r_X	(0x0fb1		     ,_r2(RS)		,MD,MB,MI,MS		)

#define CMPXCHGLrr(RS,RD)		_OO_Mrm		(0x0fb1		,_b11,_r4(RS),_r4(RD)				)
#define CMPXCHGLrm(RS,MD,MB,MI,MS)	_OO_r_X		(0x0fb1		     ,_r4(RS)		,MD,MB,MI,MS		)

/* Above variants don't seem to work */
#define CMPXCHGr(RS, RD)          	(_jit_B(0xF), _O_r_X(0xb1 	     ,_r4(RD)		,0,RS,0,0		))
#define CMPXCHGQr(RS, RD)          	(_REX(0, 0, 0), _jit_B(0xF), _O_r_X(0xb1 ,_r4(RD)	,0,RS,0,0		))
#define CMPXCHGWr(RS, RD)          	(_d16(), _jit_B(0xF), _O_r_X(0xb1    ,_r4(RD)		,0,RS,0,0		))

#define LOCK_PREFIX(i) (_jit_B(0xf0), i)

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

#define DIVQr(RS)			_qO_Mrm		(0xf7		,_b11,_b110  ,_r8(RS)				)

#define ENTERii(W, B)			_O_W_B		(0xc8						  ,_su16(W),_su8(B))
#define HLT_()				_O		(0xf4								)


#define IDIVBr(RS)			_O_Mrm		(0xf6		,_b11,_b111  ,_r1(RS)				)
#define IDIVBm(MD,MB,MI,MS)		_O_r_X		(0xf6		     ,_b111		,MD,MB,MI,MS		)

#define IDIVWr(RS)			_wO_Mrm 	(0xf7		,_b11,_b111  ,_r2(RS)				)
#define IDIVWm(MD,MB,MI,MS)		_wO_r_X 	(0xf7		     ,_b111		,MD,MB,MI,MS		)

#define IDIVLr(RS)			_O_Mrm		(0xf7		,_b11,_b111  ,_r4(RS)				)
#define IDIVLm(MD,MB,MI,MS)		_O_r_X		(0xf7		     ,_b111		,MD,MB,MI,MS		)

#define IDIVQr(RS)			_qO_Mrm		(0xf7		,_b11,_b111  ,_r8(RS)				)

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

#define IMULQrr(RS,RD)			_qOO_Mrm	(0x0faf		,_b11,_r4(RD),_r4(RS)				)

#define INCBr(RD)			_O_Mrm		(0xfe		,_b11,_b000  ,_r1(RD)				)
#define INCBm(MD,MB,MI,MS)		_O_r_X		(0xfe		     ,_b000		,MD,MB,MI,MS		)

#define INCWr(RD)			_wOr		(0x40,_r2(RD)							)
#define INCWm(MD,MB,MI,MS)		_wO_r_X		(0xff		     ,_b000		,MD,MB,MI,MS		)

#define INCLr(RD)			_Or		(0x40,_r4(RD)							)
#define INCLm(MD,MB,MI,MS)		_O_r_X		(0xff		     ,_b000		,MD,MB,MI,MS		)


#define INVD_()				_OO		(0x0f08								)
#define INVLPGm(MD, MB, MI, MS)		_OO_r_X		(0x0f01		     ,_b111		,MD,MB,MI,MS		)


#define JCCSim(CC,D,B,I,S)		((_r0P(B) && _r0P(I)) ? _O_D8	(0x70|(CC)		,(intptr_t)(D)		) : \
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

#ifndef JIT_X86_64
# define SUPPORT_TINY_JUMPS
#endif

#ifdef SUPPORT_TINY_JUMPS
# define JCCim_base(CC,nCC,D,B,I,S) ((_r0P(B) && _r0P(I)) ? (_jitl.tiny_jumps \
                                                                 ? _O_D8(0x70|(CC), D) \
                                                                 : _OO_D32	(0x0f80|(CC)		,(intptr_t)(D)		)) : \
								JITFAIL("illegal mode in conditional jump"))
#else
# define JCCim_base(CC,nCC,D,B,I,S) (_OO_D32	(0x0f80|(CC)		,(intptr_t)(D)		))
#endif

#ifdef JIT_X86_64
# define JCCim(CC,nCC,D,B,I,S) (!_jitl.long_jumps \
                                ? JCCim_base(CC,nCC,D,B,I,S)            \
                                : (_O_D8(0x70|(nCC), _jit_UL(_jit.x.pc) + 13), JMPm((intptr_t)D, 0, 0, 0)))
#else
# define JCCim(CC,nCC,D,B,I,S)	JCCim_base(CC,nCC,D,B,I,S)
#endif

#define JOm(D,B,I,S)			JCCim(0x0,0x1,D,B,I,S)
#define JNOm(D,B,I,S)			JCCim(0x1,0x0,D,B,I,S)
#define JBm(D,B,I,S)			JCCim(0x2,0x3,D,B,I,S)
#define JNAEm(D,B,I,S)			JCCim(0x2,0x3,D,B,I,S)
#define JNBm(D,B,I,S)			JCCim(0x3,0x2,D,B,I,S)
#define JAEm(D,B,I,S)			JCCim(0x3,0x2,D,B,I,S)
#define JEm(D,B,I,S)			JCCim(0x4,0x5,D,B,I,S)
#define JZm(D,B,I,S)			JCCim(0x4,0x5,D,B,I,S)
#define JNEm(D,B,I,S)			JCCim(0x5,0x4,D,B,I,S)
#define JNZm(D,B,I,S)			JCCim(0x5,0x4,D,B,I,S)
#define JBEm(D,B,I,S)			JCCim(0x6,0x7,D,B,I,S)
#define JNAm(D,B,I,S)			JCCim(0x6,0x7,D,B,I,S)
#define JNBEm(D,B,I,S)			JCCim(0x7,0x6,D,B,I,S)
#define JAm(D,B,I,S)			JCCim(0x7,0x6,D,B,I,S)
#define JSm(D,B,I,S)			JCCim(0x8,0x9,D,B,I,S)
#define JNSm(D,B,I,S)			JCCim(0x9,0x8,D,B,I,S)
#define JPm(D,B,I,S)			JCCim(0xa,0xb,D,B,I,S)
#define JPEm(D,B,I,S)			JCCim(0xa,0xb,D,B,I,S)
#define JNPm(D,B,I,S)			JCCim(0xb,0xa,D,B,I,S)
#define JPOm(D,B,I,S)			JCCim(0xb,0xa,D,B,I,S)
#define JLm(D,B,I,S)			JCCim(0xc,0xd,D,B,I,S)
#define JNGEm(D,B,I,S)			JCCim(0xc,0xd,D,B,I,S)
#define JNLm(D,B,I,S)			JCCim(0xd,0xc,D,B,I,S)
#define JGEm(D,B,I,S)			JCCim(0xd,0xc,D,B,I,S)
#define JLEm(D,B,I,S)			JCCim(0xe,0xf,D,B,I,S)
#define JNGm(D,B,I,S)			JCCim(0xe,0xf,D,B,I,S)
#define JNLEm(D,B,I,S)			JCCim(0xf,0xe,D,B,I,S)
#define JGm(D,B,I,S)			JCCim(0xf,0xe,D,B,I,S)

#define JCm(D,B,I,S) JBm(D,B,I,S)
#define JNCm(D,B,I,S) JNBm(D,B,I,S)

#define JMPSm(D,B,I,S)			((_r0P(B) && _r0P(I)) ? _O_D8	(0xeb			,(intptr_t)(D)		) : \
								JITFAIL("illegal mode in short jump"))

#ifdef SUPPORT_TINY_JUMPS
# define JMPm_base(D,B,I,S)             ((_r0P(B) && _r0P(I)) ? (_jitl.tiny_jumps \
                                                                 ? _O_D8(0xeB, D) \
                                                                 : _O_D32	(0xe9			,(intptr_t)(D)		)) : \
								JITFAIL("illegal mode in direct jump"))
#else
# define JMPm_base(D,B,I,S)  (_O_D32(0xe9			,(intptr_t)(D)		))
#endif

#ifdef JIT_X86_64
# define JMPm(D,B,I,S) (!_jitl.long_jumps \
                        ? JMPm_base(D,B,I,S)  \
                        : (MOVQir((D), JIT_REXTMP), _qO_Mrm(0xff,_b11,_b100,_r8(JIT_REXTMP))))
#else
# define JMPm(D,B,I,S)	JMPm_base(D,B,I,S)
#endif

#define JMPsr(R)			_O_Mrm	(0xff	,_b11,_b100,_r4(R)			)

#define JMPsm(D,B,I,S)			_O_r_X	(0xff	     ,_b100	,(intptr_t)(D),B,I,S		)


#define LAHF_()				_O		(0x9f								)
#define LEALmr(MD, MB, MI, MS, RD)	_O_r_X		(0x8d		     ,_r4(RD)		,MD,MB,MI,MS		)
#define LEAQmr(MD, MB, MI, MS, RD)	_qO_r_X		(0x8d		     ,_r8(RD)		,MD,MB,MI,MS		)
#define LEAQmQr(MD, MB, MI, MS, RD)	_qO_r_XB	(0x8d		     ,_r8(RD)		,MD,MB,MI,MS		)
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
#define MOVLmQr(MD, MB, MI, MS, RD)	_qOd_r_X	(0x8b		     ,_r8(RD)		,MD,MB,MI,MS		)
#define MOVLrm(RS, MD, MB, MI, MS)	_O_r_X		(0x89		     ,_r4(RS)		,MD,MB,MI,MS		)
#define MOVLQrm(RS, MD, MB, MI, MS)	_qOd_r_X        (0x89		     ,_r8(RS)		,MD,MB,MI,MS		)
#define MOVLir(IM,  R)			_Or_L		(0xb8,_r4(R)						,IM	)
#define MOVLiQr(IM,  R)			(_REXd(0,0,R), MOVLir(IM, R))
#define MOVLim(IM, MD, MB, MI, MS)	_qOd_X_L	(0xc7					,MD,MB,MI,MS	,IM	)

#define MOVQmr(MD, MB, MI, MS, RD)	_qO_r_X		(0x8b		     ,_r8(RD)		,MD,MB,MI,MS		)
#define MOVQmQr(MD, MB, MI, MS, RD)	_qO_r_XB	(0x8b		     ,_r8(RD)		,MD,MB,MI,MS		)
#define MOVQrm(RS, MD, MB, MI, MS)	_qOd_r_X	(0x89		     ,_r8(RS)		,MD,MB,MI,MS		)
#define MOVQrQm(RS, MD, MB, MI, MS)	_qO_r_XB     	(0x89		     ,_r8(RS)		,MD,MB,MI,MS		)
#define MOVQir(IM,  R)			_qOr_Q	        (0xb8,_r8(R)			,IM	)
#define MOVQim(IM, MD, MB, MI, MS)	_qO_X_L         (0xc7					,MD,MB,MI,MS	,IM	)

#define MOVQrr(RS, RD)			_qO_Mrm		(0x89		,_b11,_r8(RS),_r8(RD)				)

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

#define MOVSWQmr(MD, MB, MI, MS, RD)	_qOO_r_X	(0x0fbf		     ,_r1(RD)		,MD,MB,MI,MS		)

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

#define NEGQr(RD)			_qO_Mrm		(0xf7		,_b11,_b011  ,_r8(RD)				)

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

#define ORQrr(RS, RD)			_qO_Mrm		(0x09		,_b11,_r8(RS),_r8(RD)				)
#define ORQir(IM, RD)			_qOs_Mrm_sL	(0x81		,_b11,_b001  ,_r8(RD)			,IM	)

#define POPWr(RD)			_wOr		(0x58,_r2(RD)							)
#define POPWm(MD,MB,MI,MS)		_wO_r_X		(0x8f		     ,_b000		,MD,MB,MI,MS		)

#define POPLr(RD)			_Or		(0x58,_r4(RD)							)
#define POPLm(MD,MB,MI,MS)		_O_r_X		(0x8f		     ,_b000		,MD,MB,MI,MS		)

#define POPQr(RD)			_qOdr		(0x58,_r8(RD)							)


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

#define PUSHQr(R)			_qOdr		(0x50,_r8(R)							)

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
#define SALQir	SHLQir
#define SALQim	SHLQim
#define SALQrr	SHLQrr
#define SALQrm	SHLQrm


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

#define SARQir(IM,RD)		(((IM)==1) ?	_qO_Mrm		(0xd1	,_b11,_b111,_r8(RD)				) : \
						_qO_Mrm_B	(0xc1	,_b11,_b111,_r4(RD)			,_u8(IM) ) )
#define SARQrr(RS,RD)		(((RS)==_CL) ?	_qO_Mrm		(0xd3	,_b11,_b111,_r8(RD)				) : \
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

#define SHLQir(IM,RD)		(((IM)==1) ?	_qO_Mrm		(0xd1	,_b11,_b100,_r8(RD)				) : \
						_qO_Mrm_B	(0xc1	,_b11,_b100,_r8(RD)			,_u8(IM) ) )
#define SHLQrr(RS,RD)		(((RS)==_CL) ?	_qO_Mrm		(0xd3	,_b11,_b100,_r8(RD)				) : \
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

#define SHRQir(IM,RD)		(((IM)==1) ?	_qO_Mrm		(0xd1	,_b11,_b101,_r8(RD)				) : \
						_qO_Mrm_B	(0xc1	,_b11,_b101,_r8(RD)			,_u8(IM) ) )
#define SHRQrr(RS,RD)		(((RS)==_CL) ?	_qO_Mrm		(0xd3	,_b11,_b101,_r8(RD)				) : \
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

#define SUBQrr(RS, RD)			_qO_Mrm		(0x29		,_b11,_r8(RS),_r8(RD)				)
#define SUBQir(IM, RD)			_qOs_Mrm_sL	(0x81		,_b11,_b101  ,_r8(RD)			,IM	)

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

#define TESTQrr(RS, RD)			_qO_Mrm		(0x85		,_b11,_r8(RS),_r8(RD)				)
#define TESTQir(IM, RD)			_qO_Mrm_L	(0xf7		,_b11,_b000  ,_r8(RD)			,IM	)


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

#define XORQrr(RS, RD)			_qO_Mrm		(0x31		,_b11,_r8(RS),_r8(RD)				)
#define XORQir(IM, RD)			_qOs_Mrm_sL	(0x81		,_b11,_b110  ,_r8(RD)			,IM	)

/* x87 instructions -- yay, we found a use for octal constants :-) */
#ifdef JIT_X86_64
#define ESCmi(D,B,I,S,OP)	_qOd_r_X(0xd8|(OP >> 3), (OP & 7), D,B,I,S)
#else
#define ESCmi(D,B,I,S,OP)	_O_r_X(0xd8|(OP >> 3), (OP & 7), D,B,I,S)
#endif
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
#ifdef JIT_X86_64
# define FISTPQm(D,B,I,S)	ESCmi(D,B,I,S,077)     /* fistp m64int */
#else
# define FISTPQm(D,B,I,S)	FISTPLm(D,B,I,S)
#endif

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
#define FCOMPPr(RD)		ESCri(RD,073)
#define FCOMIr(RD)		ESCri(RD,036)
#define FCOMIPr(RD)		ESCri(RD,076)
#define FUCOMr(RD)		ESCri(RD,054)
#define FUCOMPr(RD)		ESCri(RD,055)
#define FUCOMPPr(RD)		ESCri(RD,025)
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

#define FLDCWm(D, B, I, S) _O_r_X(0xd9, 5, D,B,I,S)
#define FNSTCWm(D, B, I, S) _O_r_X(0xd9, 7, D,B,I,S)

/* N byte NOPs */
#define NOPi(N)		(((  (N)    >= 8) ? (_jit_B(0x8d),_jit_B(0xb4),_jit_B(0x26),_jit_I(0x00),_jit_B(0x90)) : (void) 0), \
			 (( ((N)&7) == 7) ? (_jit_B(0x8d),_jit_B(0xb4),_jit_B(0x26),_jit_I(0x00)) : \
			  ( ((N)&7) == 6) ? (_jit_B(0x8d),_jit_B(0xb6),_jit_I(0x00)) : \
			  ( ((N)&7) == 5) ? (_jit_B(0x90),_jit_B(0x8d),_jit_B(0x74),_jit_B(0x26),_jit_B(0x00)) : \
/* leal 0(,%esi), %esi */ ( ((N)&7) == 4) ? (_jit_B(0x8d),_jit_B(0x74),_jit_B(0x26),_jit_B(0x00)) : \
/* leal (,%esi), %esi */  ( ((N)&7) == 3) ? (_jit_B(0x8d),_jit_B(0x76),_jit_B(0x00)) : \
/* movl %esi, %esi */	  ( ((N)&7) == 2) ? (_jit_B(0x89),_jit_B(0xf6)) : \
			  ( ((N)&7) == 1) ? (_jit_B(0x90)) : \
			  ( ((N)&7) == 0) ? 0 : \
			  JITFAIL(".align argument too large")))

/* --- Media 128-bit instructions ------------------------------------------ */

typedef enum {
    X86_SSE_MOV		= 0x10,
    X86_SSE_MOVLP	= 0x12,
    X86_SSE_MOVHP	= 0x16,
    X86_SSE_MOVA	= 0x28,
    X86_SSE_CVTIS	= 0x2a,
    X86_SSE_CVTTSI	= 0x2c,
    X86_SSE_CVTSI	= 0x2d,
    X86_SSE_UCOMI	= 0x2e,
    X86_SSE_COMI	= 0x2f,
    X86_SSE_ROUND	= 0x3a,
    X86_SSE_SQRT	= 0x51,
    X86_SSE_RSQRT	= 0x52,
    X86_SSE_RCP		= 0x53,
    X86_SSE_AND		= 0x54,
    X86_SSE_ANDN	= 0x55,
    X86_SSE_OR		= 0x56,
    X86_SSE_XOR		= 0x57,
    X86_SSE_ADD		= 0x58,
    X86_SSE_MUL		= 0x59,
    X86_SSE_CVTSD	= 0x5a,
    X86_SSE_CVTDT	= 0x5b,
    X86_SSE_SUB		= 0x5c,
    X86_SSE_MIN		= 0x5d,
    X86_SSE_DIV		= 0x5e,
    X86_SSE_MAX		= 0x5f,
    X86_SSE_X2G		= 0x6e,
    X86_SSE_EQB		= 0x74,
    X86_SSE_EQW		= 0x75,
    X86_SSE_EQD		= 0x76,
    X86_SSE_G2X		= 0x7e,
    X86_SSE_MOV2	= 0xd6
} x86_sse_t;


#define _BIT(X)			(!!(X))
#define _rR(R)			((R) & 0x0f)
#define _rX(R)                  _rN(R)
#define _rXP(R)			((R) > 0 && _rR(R) > 7)
#define _SCL1 _b00

#define _rA(R)		_r4(R)

#define _RSP 0x54

#define _i_X(op, md, rb, ri, ms) _r_X(op, md, rb, ri, ms)

#define _f_X(rd, md, rb, ri, ms) _i_X((int)_rX(rd), md, rb, ri, ms)

#ifdef JIT_X86_64
# define x86_REXwrxb(l, w, r, x, b)  \
  (((l) || (((int)(w) << 3) | (((int)(r)) << 2) | (((int)(x)) << 1) | ((int)(b)))) \
   ? _jit_B(0x40 | (((int)(w) << 3) | (((int)(r)) << 2) | (((int)(x)) << 1) | ((int)(b)))) \
   : (void)0)
#else
# define x86_REXwrxb(l, w, r, x, b) (void)0
#endif

#define x86_REXwrx_(l, w, r, x, mr) x86_REXwrxb(l, w, r, x, _BIT(_rXP(mr)))
#define x86_REXw_x_(l, w, r, x, mr) x86_REXwrx_(l, w, _BIT(_rXP(r)), x, mr)
#define x86_rex_l_rr(rr, mr) x86_REXw_x_(0, 0, rr, 0, mr)
#define x86_rex_l_mr(rb, ri, rd) x86_REXw_x_(0, 0, rd, _BIT(_rXP(ri)), rb)
#define x86_rex_l_rm(rs, rb, ri) x86_rex_l_mr(rb, ri, rs)


#define _rex_ff_rr(rr, mr) x86_rex_l_rr(rr, mr)
#define _rex_if_rr(rr, mr) x86_rex_l_rr(rr, mr)
#define _rex_fi_rr(rr, mr) x86_rex_l_rr(rr, mr)
#define _rex_if_mr(rb, ri, rd) x86_rex_l_mr(rb, ri, rd)
#define _rex_fi_rm(rs, rb, ri) x86_rex_l_rm(rs, rb, ri)

#define __sse_ff_rr(op, rs, rd)	(_rex_ff_rr(rd, rs), _O(0x0f), _O(op), _Mrm(_b11, _rX(rd), _rX(rs)))

#define __sse_id_rr(op, rs, rd) __sse_if_rr(op, rs, rd)
#define __sse_if_rr(op, rs, rd)	(_rex_if_rr(rd, rs), _O(0x0f), _O(op), _Mrm(_b11, _rA(rd), _rX(rs)))

#define __sse_di_rr(op, rs, rd) __sse_fi_rr(op, rs, rd)
#define __sse_fi_rr(op, rs, rd) (_rex_fi_rr(rd, rs), _O(0x0f), _O(op), _Mrm(_b11, _rX(rd), _rA(rs)))

#define __sse_id_mr(op, md, rb, mi, ms, rd) __sse_if_mr(op, md, rb, mi, ms, rd)
#define __sse_if_mr(op, md, rb, ri, ms, rd) (_rex_if_mr(rb, ri, rd), _O(0x0f), _O(op), _f_X(rd, md, rb, ri, ms))

#define __sse_di_rm(op, rs, md, rb, mi, ms) __sse_fi_rm(op, rs, md, rb, mi, ms)
#define __sse_fi_rm(op, rs, md, rb, ri, ms) (_rex_fi_rm(rs, rb, ri), _O(0x0f), _O(op), _f_X(rs, md, rb, ri, ms))

#define __sse1_di_rm(op, rs, md, mb, mi, ms) __sse1_fi_rm(op, rs, md, mb, mi, ms)
#define __sse1_fi_rm(op, rs, md, rb, ri, ms) (_rex_fi_rm(rs, rb, ri), _O(0x0f), _O(0x01 | op), _f_X(rs, md, rb, ri, ms))

#define _sse_ff_rr(px, op, rs, rd) (_jit_B(px), __sse_ff_rr(op, rs, rd))

#define _sse_id_rr(px, op, rs, rd) _sse_if_rr(px, op, rs, rd)
#define _sse_if_rr(px, op, rs, rd) (_jit_B(px), __sse_if_rr(op, rs, rd))

#define _sse_di_rr(px, op, rs, rd) _sse_fi_rr(px, op, rs, rd)
#define _sse_fi_rr(px, op, rs, rd) (_jit_B(px), __sse_fi_rr(op, rs, rd))

#define _sse_id_mr(px, op, md, rb, mi, ms, rd) _sse_if_mr(px, op, md, rb, mi, ms, rd)
#define _sse_if_mr(px, op, md, rb, ri, ms, rd) (_jit_B(px), __sse_if_mr(op, md, rb, ri, ms, rd))

#define _sse_di_rm(px, op, rs, md, rb, mi, ms) _sse_fi_rm(px, op, rs, md, rb, mi, ms)
#define _sse_fi_rm(px, op, rs, md, rb, ri, ms) (_jit_B(px), __sse_fi_rm(op, rs, md, rb, ri, ms))

#define _sse1_di_rm(px, op, rs, md, mb, mi, ms) _sse1_fi_rm(px, op, rs, md, mb, mi, ms)
#define _sse1_fi_rm(px, op, rs, md, rb, ri, ms) (_jit_B(px), __sse1_fi_rm(op, rs, md, rb, ri, ms))

#define _SSEPSrr(OP,RS,RD)		__sse_ff_rr (      OP, RS, RD)
#define _SSEPSmr(OP,MD,MB,MI,MS,RD)	__sse_if_mr (      OP, MD, MB, MI, MS, RD)
#define _SSEPSrm(OP,RS,MD,MB,MI,MS)	__sse_fi_rm (      OP, RS, MD, MB, MI, MS)
#define _SSEPS1rm(OP,RS,MD,MB,MI,MS)	__sse1_fi_rm(      OP, RS, MD, MB, MI, MS)

#define _SSEPDrr(OP,RS,RD)		 _sse_ff_rr (0x66, OP, RS, RD)
#define _SSEPDmr(OP,MD,MB,MI,MS,RD)	 _sse_if_mr (0x66, OP, MD, MB, MI, MS, RD)
#define _SSEPDrm(OP,RS,MD,MB,MI,MS)	 _sse_fi_rm (0x66, OP, RS, MD, MB, MI, MS)
#define _SSEPD1rm(OP,RS,MD,MB,MI,MS)	 _sse1_fi_rm(0x66, OP, RS, MD, MB, MI, MS)

#define _SSESSrr(OP,RS,RD)		 _sse_ff_rr (0xf3, OP, RS, RD)
#define _SSESSmr(OP,MD,MB,MI,MS,RD)	 _sse_if_mr (0xf3, OP, MD, MB, MI, MS, RD)
#define _SSESSrm(OP,RS,MD,MB,MI,MS)	 _sse_fi_rm (0xf3, OP, RS, MD, MB, MI, MS)
#define _SSESS1rm(OP,RS,MD,MB,MI,MS)	 _sse1_fi_rm(0xf3, OP, RS, MD, MB, MI, MS)

#define _SSESDrr(OP,RS,RD)		 _sse_ff_rr (0xf2, OP, RS, RD)
#define _SSESDmr(OP,MD,MB,MI,MS,RD)	 _sse_if_mr (0xf2, OP, MD, MB, MI, MS, RD)
#define _SSESDrm(OP,RS,MD,MB,MI,MS)	 _sse_fi_rm (0xf2, OP, RS, MD, MB, MI, MS)
#define _SSESD1rm(OP,RS,MD,MB,MI,MS)	 _sse1_fi_rm(0xf2, OP, RS, MD, MB, MI, MS)

#define _NOREG 0

/* SSE */
#define LDMXCSRmr(MD, MB, MI, MS)					\
    (_REXLmr(MB, MI, _NOREG),						\
     _O(0x0f),								\
     _O(0xae),								\
     _i_X(_b10, MD, MB, MI, MS))
#define STMXCSRrm(MD, MB, MI, MS)					\
    (_REXLrm(_NOREG, MI, MB),						\
     _O(0x0f),								\
     _O(0xae),								\
     _i_X(_b11, MD, MB, MI, MS))

/* SSE2 */
#define ADDPSrr(RS, RD)			_SSEPSrr(X86_SSE_ADD, RS, RD)
#define ADDPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_ADD, MD, MB, MI, MS, RD)
#define ADDPDrr(RS, RD)			_SSEPDrr(X86_SSE_ADD, RS, RD)
#define ADDPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_ADD, MD, MB, MI, MS, RD)

/* SSE */
#define ADDSSrr(RS, RD)			_SSESSrr(X86_SSE_ADD, RS, RD)
#define ADDSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_ADD, MD, MB, MI, MS, RD)

/* SSE2 */
#define ADDSDrr(RS, RD)			_SSESDrr(X86_SSE_ADD, RS, RD)
#define ADDSDmr(MD, MB, MI, MS, RD)	_SSESDmr(X86_SSE_ADD, MD, MB, MI, MS, RD)

/* SSE */
#define ANDNPSrr(RS, RD)		_SSEPSrr(X86_SSE_ANDN, RS, RD)
#define ANDNPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_ANDN, MD, MB, MI, MS, RD)

/* SSE2 */
#define ANDNPDrr(RS, RD)		_SSEPDrr(X86_SSE_ANDN, RS, RD)
#define ANDNPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_ANDN, MD, MB, MI, MS, RD)

/* SSE */
#define ANDNSSrr			ANDNPSrr
#define ANDNSSmr			ANDNPSrr

/* SSE2 */
#define ANDNSDrr			ANDNPDrr
#define ANDNSDmr			ANDNPDrr

/* SSE */
#define ANDPSrr(RS, RD)			_SSEPSrr(X86_SSE_AND, RS, RD)
#define ANDPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_AND, MD, MB, MI, MS, RD)

/* SSE2 */
#define ANDPDrr(RS, RD)			_SSEPDrr(X86_SSE_AND, RS, RD)
#define ANDPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_AND, MD, MB, MI, MS, RD)

/* SSE */
#define ANDSSrr				ANDPSrr
#define ANDSSmr				ANDPSrr

/* SSE2 */
#define ANDSDrr				ANDPDrr
#define ANDSDmr				ANDPDrr

/* SSE */
#define DIVPSrr(RS, RD)			_SSEPSrr(X86_SSE_DIV, RS, RD)
#define DIVPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_DIV, MD, MB, MI, MS, RD)

/* SSE2 */
#define DIVPDrr(RS, RD)			_SSEPDrr(X86_SSE_DIV, RS, RD)
#define DIVPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_DIV, MD, MB, MI, MS, RD)

/* SSE */
#define DIVSSrr(RS, RD)			_SSESSrr(X86_SSE_DIV, RS, RD)
#define DIVSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_DIV, MD, MB, MI, MS, RD)

/* SSE2 */
#define DIVSDrr(RS, RD)			_SSESDrr(X86_SSE_DIV, RS, RD)
#define DIVSDmr(MD, MB, MI, MS, RD)	_SSESDmr(X86_SSE_DIV, MD, MB, MI, MS, RD)

/* SSE */
#define MAXPSrr(RS, RD)			_SSEPSrr(X86_SSE_MAX, RS, RD)
#define MAXPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_MAX, MD, MB, MI, MS, RD)

/* SSE2 */
#define MAXPDrr(RS, RD)			_SSEPDrr(X86_SSE_MAX, RS, RD)
#define MAXPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_MAX, MD, MB, MI, MS, RD)

/* SSE */
#define MAXSSrr(RS, RD)			_SSESSrr(X86_SSE_MAX, RS, RD)
#define MAXSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_MAX, MD, MB, MI, MS, RD)

/* SSE2 */
#define MAXSDrr(RS, RD)			_SSESDrr(X86_SSE_MAX, RS, RD)
#define MAXSDmr(MD, MB, MI, MS, RD)	_SSESDmr(X86_SSE_MAX, MD, MB, MI, MS, RD)

/* SSE */
#define MINPSrr(RS, RD)			_SSEPSrr(X86_SSE_MIN, RS, RD)
#define MINPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_MIN, MD, MB, MI, MS, RD)

/* SSE2 */
#define MINPDrr(RS, RD)			_SSEPDrr(X86_SSE_MIN, RS, RD)
#define MINPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_MIN, MD, MB, MI, MS, RD)

/* SSE */
#define MINSSrr(RS, RD)			_SSESSrr(X86_SSE_MIN, RS, RD)
#define MINSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_MIN, MD, MB, MI, MS, RD)

/* SSE2 */
#define MINSDrr(RS, RD)			_SSESDrr(X86_SSE_MIN, RS, RD)
#define MINSDmr(MD, MB, MI, MS, RD)	_SSESDmr(X86_SSE_MIN, MD, MB, MI, MS, RD)

/* SSE */
#define MULPSrr(RS, RD)			_SSEPSrr(X86_SSE_MUL, RS, RD)
#define MULPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_MUL, MD, MB, MI, MS, RD)

/* SSE2 */
#define MULPDrr(RS, RD)			_SSEPDrr(X86_SSE_MUL, RS, RD)
#define MULPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_MUL, MD, MB, MI, MS, RD)

/* SSE */
#define MULSSrr(RS, RD)			_SSESSrr(X86_SSE_MUL, RS, RD)
#define MULSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_MUL, MD, MB, MI, MS, RD)

/* SSE2 */
#define MULSDrr(RS, RD)			_SSESDrr(X86_SSE_MUL, RS, RD)
#define MULSDmr(MD, MB, MI, MS, RD)	_SSESDmr(X86_SSE_MUL, MD, MB, MI, MS, RD)

/* SSE */
#define ORPSrr(RS, RD)			_SSEPSrr(X86_SSE_OR, RS, RD)
#define ORPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_OR, MD, MB, MI, MS, RD)

/* SSE2 */
#define ORPDrr(RS, RD)			_SSEPDrr(X86_SSE_OR, RS, RD)
#define ORPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_OR, MD, MB, MI, MS, RD)

/* SSE */
#define ORSSrr				ORPSrr
#define ORSSmr				ORPSrr

/* SSE2 */
#define ORSDrr				ORPDrr
#define ORSDmr				ORPDrr

/* SSE */
#define RCPPSrr(RS, RD)			_SSEPSrr(X86_SSE_RCP, RS, RD)
#define RCPPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_RCP, MD, MB, MI, MS, RD)
#define RCPSSrr(RS, RD)			_SSESSrr(X86_SSE_RCP, RS, RD)
#define RCPSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_RCP, MD, MB, MI, MS, RD)

/* SSE */
#define RSQRTPSrr(RS, RD)		_SSEPSrr(X86_SSE_RSQRT, RS, RD)
#define RSQRTPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_RSQRT, MD, MB, MI, MS, RD)
#define RSQRTSSrr(RS, RD)		_SSESSrr(X86_SSE_RSQRT, RS, RD)
#define RSQRTSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_RSQRT, MD, MB, MI, MS, RD)

/* SSE */
#define SQRTPSrr(RS, RD)		_SSEPSrr(X86_SSE_SQRT, RS, RD)
#define SQRTPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_SQRT, MD, MB, MI, MS, RD)

/* SSE2 */
#define SQRTPDrr(RS, RD)		_SSEPDrr(X86_SSE_SQRT, RS, RD)
#define SQRTPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_SQRT, MD, MB, MI, MS, RD)

/* SSE */
#define SQRTSSrr(RS, RD)		_SSESSrr(X86_SSE_SQRT, RS, RD)
#define SQRTSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_SQRT, MD, MB, MI, MS, RD)

/* SSE2 */
#define SQRTSDrr(RS, RD)		_SSESDrr(X86_SSE_SQRT, RS, RD)
#define SQRTSDmr(MD, MB, MI, MS, RD)	_SSESDmr(X86_SSE_SQRT, MD, MB, MI, MS, RD)

/* SSE */
#define SUBPSrr(RS, RD)			_SSEPSrr(X86_SSE_SUB, RS, RD)
#define SUBPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_SUB, MD, MB, MI, MS, RD)

/* SSE2 */
#define SUBPDrr(RS, RD)			_SSEPDrr(X86_SSE_SUB, RS, RD)
#define SUBPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_SUB, MD, MB, MI, MS, RD)

/* SSE */
#define SUBSSrr(RS, RD)			_SSESSrr(X86_SSE_SUB, RS, RD)
#define SUBSSmr(MD, MB, MI, MS, RD)	_SSESSmr(X86_SSE_SUB, MD, MB, MI, MS, RD)

/* SSE2 */
#define SUBSDrr(RS, RD)			_SSESDrr(X86_SSE_SUB, RS, RD)
#define SUBSDmr(MD, MB, MI, MS, RD)	_SSESDmr(X86_SSE_SUB, MD, MB, MI, MS, RD)

/* SSE */
#define XORPSrr(RS, RD)			_SSEPSrr(X86_SSE_XOR, RS, RD)
#define XORPSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_XOR, MD, MB, MI, MS, RD)

/* SSE2 */
#define XORPDrr(RS, RD)			_SSEPDrr(X86_SSE_XOR, RS, RD)
#define XORPDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_XOR, MD, MB, MI, MS, RD)

/* SSE */
#define XORSSrr				XORPSrr
#define XORSSmr				XORPSrr

/* SSE2 */
#define XORSDrr				XORPDrr
#define XORSDmr				XORPDrr

/* No prefixes here.  */
/* SSE */
#define COMISSrr(RS, RD)		_SSEPSrr(X86_SSE_COMI, RS, RD)
#define COMISSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_COMI, MD, MB, MI, MS, RD)

/* SSE2 */
#define COMISDrr(RS, RD)		_SSEPDrr(X86_SSE_COMI, RS, RD)
#define COMISDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_COMI, MD, MB, MI, MS, RD)

/* No prefixes here.  */
/* SSE */
#define UCOMISSrr(RS, RD)		_SSEPSrr(X86_SSE_UCOMI, RS, RD)
#define UCOMISSmr(MD, MB, MI, MS, RD)	_SSEPSmr(X86_SSE_UCOMI, MD, MB, MI, MS, RD)

/* SSE2 */
#define UCOMISDrr(RS, RD)		_SSEPDrr(X86_SSE_UCOMI, RS, RD)
#define UCOMISDmr(MD, MB, MI, MS, RD)	_SSEPDmr(X86_SSE_UCOMI, MD, MB, MI, MS, RD)

/* SSE */
#define MOVSSrr(RS, RD)			_SSESSrr (X86_SSE_MOV, RS, RD)
#define MOVSSmr(MD, MB, MI, MS, RD)	_SSESSmr (X86_SSE_MOV, MD, MB, MI, MS, RD)
#define MOVSSrm(RS, MD, MB, MI, MS)	_SSESS1rm(X86_SSE_MOV, RS, MD, MB, MI, MS)

/* SSE2 */
#define MOVSDrr(RS, RD)			_SSESDrr (X86_SSE_MOV, RS, RD)
#define MOVSDmr(MD, MB, MI, MS, RD)	_SSESDmr (X86_SSE_MOV, MD, MB, MI, MS, RD)
#define MOVSDrm(RS, MD, MB, MI, MS)	_SSESD1rm(X86_SSE_MOV, RS, MD, MB, MI, MS)

/* SSE */
#define MOVAPSrr(RS, RD)		_SSEPSrr (X86_SSE_MOVA, RS, RD)
#define MOVAPSmr(MD, MB, MI, MS, RD)	_SSEPSmr (X86_SSE_MOVA, MD, MB, MI, MS, RD)
#define MOVAPSrm(RS, MD, MB, MI, MS)	_SSEPS1rm(X86_SSE_MOVA, RS, MD, MB, MI, MS)

/* SSE2 */
#define MOVAPDrr(RS, RD)		_SSEPDrr (X86_SSE_MOVA, RS, RD)
#define MOVAPDmr(MD, MB, MI, MS, RD)	_SSEPDmr (X86_SSE_MOVA, MD, MB, MI, MS, RD)
#define MOVAPDrm(RS, MD, MB, MI, MS)	_SSEPD1rm(X86_SSE_MOVA, RS, MD, MB, MI, MS)

/* SSE */
#define CVTPS2PIrr(RS, RD)		__sse_ff_rr(      X86_SSE_CVTSI, RS, RD)
#define CVTPS2PImr(MD, MB, MI, MS, RD)	__sse_if_mr(      X86_SSE_CVTSI, MD, MB, MI, MS, RD)

/* SSE2 */
#define CVTPD2PIrr(RS, RD)		 _sse_ff_rr(0x66, X86_SSE_CVTSI, RS, RD)
#define CVTPD2PImr(MD, MB, MI, MS, RD)	 _sse_id_mr(0x66, X86_SSE_CVTSI, MD, MB, MI, MS, RD)

/* SSE */
#define CVTPI2PSrr(RS, RD)		__sse_ff_rr(      X86_SSE_CVTIS, RS, RD)
#define CVTPI2PSmr(MD, MB, MI, MS, RD)	__sse_if_mr(      X86_SSE_CVTIS, MD, MB, MI, MS, RD)

/* SSE2 */
#define CVTPI2PDrr(RS, RD)		 _sse_ff_rr(0x66, X86_SSE_CVTIS, RS, RD)
#define CVTPI2PDmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0x66, X86_SSE_CVTIS, MD, MB, MI, MS, RD)

/* SSE2 */
#define CVTPS2PDrr(RS, RD)		__sse_ff_rr(      X86_SSE_CVTSD, RS, RD)
#define CVTPS2PDmr(MD, MB, MI, MS, RD)	__sse_if_mr(      X86_SSE_CVTSD, MD, MB, MI, MS, RD)
#define CVTPD2PSrr(RS, RD)		 _sse_ff_rr(0x66, X86_SSE_CVTSD, RS, RD)
#define CVTPD2PSmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0x66, X86_SSE_CVTSD, MD, MB, MI, MS, RD)

/* SSE2 */
#define CVTSS2SDrr(RS, RD)		 _sse_ff_rr(0xf3, X86_SSE_CVTSD, RS, RD)
#define CVTSS2SDmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0xf3, X86_SSE_CVTSD, MD, MB, MI, MS, RD)
#define CVTSD2SSrr(RS, RD)		 _sse_ff_rr(0xf2, X86_SSE_CVTSD, RS, RD)
#define CVTSD2SSmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0xf2, X86_SSE_CVTSD, MD, MB, MI, MS, RD)

/* SSE */
#define CVTTSS2SILrr(RS, RD)		 _sse_id_rr(0xf3, X86_SSE_CVTTSI, RS, RD)
#define CVTTSS2SILmr(MD, MB, MI, MS, RD) _sse_id_mr(0xf3, X86_SSE_CVTTSI, MD, MB, MI, MS, RD)

/* SSE2 */
#define CVTTSD2SILrr(RS, RD)		 _sse_id_rr(0xf2, X86_SSE_CVTTSI, RS, RD)
#define CVTTSD2SILmr(MD, MB, MI, MS, RD) _sse_id_mr(0xf2, X86_SSE_CVTTSI, MD, MB, MI, MS, RD)

/* SSE */
#define CVTSS2SILrr(RS, RD)		 _sse_if_rr(0xf3, X86_SSE_CVTSI, RS, RD)
#define CVTSS2SILmr(MD, MB, MI, MS, RD)	 _sse_if_mr(0xf3, X86_SSE_CVTSI, MD, MB, MI, MS, RD)

/* SSE2 */
#define CVTSD2SILrr(RS, RD)		 _sse_id_rr(0xf2, X86_SSE_CVTSI, RS, RD)
#define CVTSD2SILmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0xf2, X86_SSE_CVTSI, MD, MB, MI, MS, RD)

/* SSE */
#define CVTSI2SSLrr(RS, RD)		 _sse_fi_rr(0xf3, X86_SSE_CVTIS, RS, RD)
#define CVTSI2SSLmr(MD, MB, MI, MS, RD)	 _sse_if_mr(0xf3, X86_SSE_CVTIS, MD, MB, MI, MS, RD)

/* SSE2 */
#define CVTSI2SDLrr(RS, RD)		 _sse_di_rr(0xf2, X86_SSE_CVTIS, RS, RD)
#define CVTSI2SDLmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0xf2, X86_SSE_CVTIS, MD, MB, MI, MS, RD)

/* SSE2 */
#define MOVDLXrr(RS, RD)		 _sse_di_rr(0x66, X86_SSE_X2G, RS, RD)
#define MOVDLXmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0x66, X86_SSE_X2G, MD, MB, MI, MS, RD)

/* SSE2 */
#define MOVDXLrr(RS, RD)		 _sse_ff_rr(0x66, X86_SSE_G2X, RS, RD)
#define MOVDXLrm(RS, MD, MB, MI, MS)	 _sse_di_rm(0x66, X86_SSE_G2X, RS, MD, MB, MI, MS)

/* SSE */
#define MOVDLMrr(RS, RD)		__sse_ff_rr(      X86_SSE_X2G, RS, RD)
#define MOVDLMmr(MD, MB, MI, MS, RD)	__sse_id_mr(      X86_SSE_X2G, MD, MB, MI, MS, RD)

/* SSE */
#define MOVDMLrr(RS, RD)		__sse_ff_rr(      X86_SSE_G2X, RS, RD)
#define MOVDMLrm(RS, MD, MB, MI, MS)	__sse_fi_rm(      X86_SSE_G2X, RS, MD, MB, MI, MS)

/* SSE3 */
#define MOVDQ2Qrr(RS, RD)		 _sse_ff_rr(0xf2, X86_SSE_MOV2, RS, RD)
#define MOVQ2DQrr(RS, RD)		 _sse_ff_rr(0xf3, X86_SSE_MOV2, RS, RD)

/* SSE */
#define MOVHLPSrr(RS, RD)		__sse_ff_rr(      X86_SSE_MOVLP, RS, RD)
#define MOVLHPSrr(RS, RD)		__sse_ff_rr(      X86_SSE_MOVHP, RS, RD)

/* SSE2 */
#define MOVDQArr(RS, RD)		 _sse_ff_rr(0x66, 0x6f, RS, RD)
#define MOVDQAmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0x66, 0x6f, MD, MB, MI, MS, RD)
#define MOVDQArm(RS, MD, MB, MI, MS)	 _sse_di_rm(0x66, 0x7f, RS, MD, MB, MI, MS)

/* SSE2 */
#define MOVDQUrr(RS, RD)		 _sse_ff_rr(0xf3, 0x6f, RS, RD)
#define MOVDQUmr(MD, MB, MI, MS, RD)	 _sse_id_mr(0xf3, 0x6f, MD, MB, MI, MS, RD)
#define MOVDQUrm(RS, MD, MB, MI, MS)	 _sse_di_rm(0xf3, 0x7f, RS, MD, MB, MI, MS)

/* SSE2 */
#define MOVHPDmr(MD, MB, MI, MS, RD)	 _sse_id_mr (0x66, X86_SSE_MOVHP, MD, MB, MI, MS, RD)
#define MOVHPDrm(RS, MD, MB, MI, MS)	 _sse1_di_rm(0x66, X86_SSE_MOVHP, RS, MD, MB, MI, MS)

/* SSE */
#define MOVHPSmr(MD, MB, MI, MS, RD)	__sse_if_mr (      X86_SSE_MOVHP, MD, MB, MI, MS, RD)
#define MOVHPSrm(RS, MD, MB, MI, MS)	__sse1_fi_rm(      X86_SSE_MOVHP, RS, MD, MB, MI, MS)

/* SSE2 */
#define MOVLPDmr(MD, MB, MI, MS, RD)	 _sse_id_mr (0x66, X86_SSE_MOVLP, MD, MB, MI, MS, RD)
#define MOVLPDrm(RS, MD, MB, MI, MS)	 _sse1_di_rm(0x66, X86_SSE_MOVLP, RS, MD, MB, MI, MS)

/* SSE */
#define MOVLPSmr(MD, MB, MI, MS, RD)	__sse_if_mr (      X86_SSE_MOVLP, MD, MB, MI, MS, RD)
#define MOVLPSrm(RS, MD, MB, MI, MS)	__sse1_fi_rm(      X86_SSE_MOVLP, RS, MD, MB, MI, MS)

/* FIXME 0x66 prefix actually required to modify 128 bits register */
/* SSE or SSE2 with 0x66 prefix */
#define PCMPEQBrr(RS, RD)						\
    _sse_ff_rr(0x66, X86_SSE_EQB, RS, RD)
#define PCMPEQBrm(RS, MD, MB, MI, MS)					\
    _sse_if_mr(0x66, X86_SSE_EQB, MD, MB, MI, MS, RD)
#define PCMPEQWrr(RS, RD)						\
    _sse_ff_rr(0x66, X86_SSE_EQW, RS, RD)
#define PCMPEQWrm(RS, MD, MB, MI, MS)					\
    _sse_if_mr(0x66, X86_SSE_EQW, MD, MB, MI, MS, RD)
#define PCMPEQLrr(RS, RD)						\
    _sse_ff_rr(0x66, X86_SSE_EQD, RS, RD)
#define PCMPEQLrm(RS, MD, MB, MI, MS)					\
    _sse_if_mr(0x66, X86_SSE_EQD, MD, MB, MI, MS, RD)

/* SSE2 with 0x66 prefix, SSE otherwise */
#define PSRLWrr(RS, RD)							\
    _sse_ff_rr(0x66, 0xd1, RS, RD)
#define PSRLWrm(RS, MD, MB, MI, MS)					\
    _sse_if_mr(0x66, 0xd1, MD, MB, MI, MS, RD)
#define PSRLWir(IM, RD)							\
    (_O(0x66),								\
     _REXLrr(_NOREG, RD),						\
     _O(0x0f),								\
     _O(0x71),								\
     _Mrm(_b11, _b10, _rX(RD)),						\
     _O(IM))

/* SSE2 with 0x66 prefix, SSE otherwise */
#define PSRLLrr(RS, RD)							\
    _sse_ff_rr(0x66, 0xd2, RS, RD)
#define PSRLLrm(RS, MD, MB, MI, MS)					\
    _sse_id_mr(0x66, 0xd2, MD, MB, MI, MS, RD)
#define PSRLLir(IM, RD)							\
    (_O(0x66),								\
     _rex_if_rr(_NOREG, RD),						\
     _O(0x0f),								\
     _O(0x72),								\
     _Mrm(_b11, _b10, _rX(RD)),						\
     _O(IM))

/* SSE2 */
#define PSRLQrr(RS, RD)							\
    _sse_ff_rr(0x66, 0xd3, RS, RD)
#define PSRLQrm(RS, MD, MB, MI, MS)					\
    _sse_id_mr(0x66, 0xd3, MD, MB, MI, MS, RD)
#define PSRLQir(IM, RD)							\
    (_O(0x66),								\
     _rex_if_rr(_NOREG, RD),						\
     _O(0x0f),								\
     _O(0x73),								\
     _Mrm(_b11, _b10, _rX(RD)),						\
     _O(IM))

/* SSE4.1 */
#define ROUNDSSrri(RS, RD, IM)						\
    (_O(0x66), _rex_ff_rr(RD, RS), _OO(0xf00|X86_SSE_ROUND), _O(0x0a),	\
     _Mrm(_b11, _rX(RD), _rX(RS)), _O(IM))
#define ROUNDSDrri(RS, RD, IM)						\
    (_O(0x66), _rex_ff_rr(RD, RS), _OO(0xf00|X86_SSE_ROUND), _O(0x0b),	\
     _Mrm(_b11, _rX(RD), _rX(RS)), _O(IM))
#define PCMPEQQrr(RS, RD)						\
    (_O(0x66), _rex_ff_rr(RD, RS), _OO(0x0f38), _O(0x29),		\
     _Mrm(_b11, _rX(RD), _rX(RS)))


#ifdef JIT_X86_64

#define _rex_q_rr(rr, mr) x86_REXw_x_(0, 1, rr, 0, mr)
#define _rex_dl_rr(rr, mr) _rex_q_rr(rr, mr)
#define _rex_ld_rr(rr, mr) _rex_q_rr(rr, mr)

#define __sse_lf_rr(op, rs, rd) __sse_ld_rr(op, rs, rd)
#define __sse_ld_rr(op, rs, rd) (_rex_ld_rr(rd, rs), _O(0x0f), _O(op), _Mrm(_b11, _rA(rd), _rX(rs)))

#define __sse_fl_rr(op, rs, rd) __sse_dl_rr(op, rs, rd)
#define __sse_dl_rr(op, rs, rd) (_rex_dl_rr(rd, rs), _O(0x0f), _O(op), _Mrm(_b11, _rX(rd), _rA(rs)))

#define _sse_lf_rr(px, op, rs, rd) _sse_ld_rr(px, op, rs, rd)
#define _sse_ld_rr(px, op, rs, rd) (_jit_B(px), __sse_ld_rr(op, rs, rd))

#define _sse_fl_rr(px, op, rs, rd) _sse_dl_rr(px, op, rs, rd)
#define _sse_dl_rr(px, op, rs, rd) (_jit_B(px), __sse_dl_rr(op, rs, rd))

#define CVTTSD2SIQrr(RS, RD)		 _sse_lf_rr(0xf2, X86_SSE_CVTTSI, RS, RD)
#define CVTSI2SDQrr(RS, RD)		 _sse_dl_rr(0xf2, X86_SSE_CVTIS, RS, RD)
#define MOVDQXrr(RS, RD)		 _sse_dl_rr(0x66, X86_SSE_X2G, RS, RD)

#endif

/*** References:										*/
/*												*/
/* [1] "Intel Architecture Software Developer's Manual Volume 1: Basic Architecture",		*/
/*     Intel Corporation 1997.									*/
/*												*/
/* [2] "Intel Architecture Software Developer's Manual Volume 2: Instruction Set Reference",	*/
/*     Intel Corporation 1997.									*/

#endif
#endif /* __lightning_asm_h */

