/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer (arm version)
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright 2011 Free Software Foundation, Inc.
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
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 ***********************************************************************/

#ifndef __lightning_core_arm_h
#define __lightning_core_arm_h

#define _jitl		_jitp->jitl

#define JIT_R_NUM			4
#define JIT_R(i) i

#define JIT_V_NUM			4
static const jit_gpr_t
jit_v_order[JIT_V_NUM] = {
  _R4, _R5, _R6, _R7
};
#define JIT_V(i)			jit_v_order[i]

#define JIT_R0				_R0
#define JIT_R1				_R1
#define JIT_R2				_R2
#define JIT_V0				_R4
#define JIT_V1				_R5
#define JIT_V2				_R6

#define jit_no_set_flags()		jit_flags.no_set_flags

#if TARGET_OS_IPHONE
# define USE_SEPARATE_DIV_AND_MOD
#endif

#ifdef USE_SEPARATE_DIV_AND_MOD
extern int __divsi3(int, int);
extern int __udivsi3(int, int);
extern int __modsi3(int, int);
extern int __umodsi3(int, int);
#else
extern int	__aeabi_idivmod(int, int);
extern unsigned	__aeabi_uidivmod(unsigned, unsigned);
#endif

#define jit_nop(n)			arm_nop(_jitp, n)
__jit_inline void
arm_nop(jit_state_t _jitp, int n)
{
  jit_assert(n >= 0);
  if (jit_thumb_p()) {
    for (; n > 0; n -= 2)
      T1_NOP();
  }
  else {
    for (; n > 0; n -= 4)
      _NOP();
  }
}

#define jit_movr_i(r0, r1)		arm_movr_i(_jitp, r0, r1)
__jit_inline void
arm_movr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (r0 != r1) {
    if (jit_thumb_p())
      T1_MOV(r0, r1);
    else
      _MOV(r0, r1);
  }
}

#define jit_movi_i(r0, i0)		arm_movi_i(_jitp, r0, i0)
__jit_inline void
arm_movi_i(jit_state_t _jitp, jit_gpr_t r0, int i0)
{
  int		i;
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && r0 < 8 && !(i0 & 0xffffff80))
      T1_MOVI(r0, i0);
    else if ((i = encode_thumb_immediate(i0)) != -1)
      T2_MOVI(r0, i);
    else if ((i = encode_thumb_immediate(~i0)) != -1)
      T2_MVNI(r0, i);
    else {
      T2_MOVWI(r0, _jit_US(i0));
      if (i0 & 0xffff0000)
        T2_MOVTI(r0, _jit_US((unsigned)i0 >> 16));
    }
  }
  else {
    if (jit_armv6t_p() && !(i0 & 0xffff0000))
      _MOVWI(r0, i0);
    else if ((i = encode_arm_immediate(i0)) != -1)
      _MOVI(r0, i);
    else if ((i = encode_arm_immediate(~i0)) != -1)
      _MVNI(r0, i);
    else if (jit_armv6t_p()) {
      _MOVWI(r0, _jit_US(i0));
      if ((i0 & 0xffff0000))
        _MOVTI(r0, _jit_US((unsigned)i0 >> 16));
    }
    else {
      int     p0, p1, p2, p3, q0, q1, q2, q3;
      p0 = i0 & 0x000000ff;   p1 = i0 & 0x0000ff00;
      p2 = i0 & 0x00ff0000;   p3 = i0 & 0xff000000;
      i0 = ~i0;
      q0 = i0 & 0x000000ff;   q1 = i0 & 0x0000ff00;
      q2 = i0 & 0x00ff0000;   q3 = i0 & 0xff000000;
      if (!!p0 + !!p1 + !!p2 + !!p3 <= !!q0 + !!q1 + !!q2 + !!q3) {
        /* prefer no inversion on tie */
        if (p3) {
          _MOVI(r0, encode_arm_immediate(p3));
          if (p2) _ORRI(r0, r0, encode_arm_immediate(p2));
          if (p1) _ORRI(r0, r0, encode_arm_immediate(p1));
          if (p0) _ORRI(r0, r0, p0);
        }
        else if (p2) {
          _MOVI(r0, encode_arm_immediate(p2));
          if (p1) _ORRI(r0, r0, encode_arm_immediate(p1));
          if (p0) _ORRI(r0, r0, p0);
        }
        else {
          _MOVI(r0, encode_arm_immediate(p1));
          _ORRI(r0, r0, p0);
        }
      }
      else {
        if (q3) {
          _MVNI(r0, encode_arm_immediate(q3));
          if (q2) _EORI(r0, r0, encode_arm_immediate(q2));
          if (q1) _EORI(r0, r0, encode_arm_immediate(q1));
          if (q0) _EORI(r0, r0, q0);
        }
        else if (q2) {
          _MVNI(r0, encode_arm_immediate(q2));
          if (q1) _EORI(r0, r0, encode_arm_immediate(q1));
          if (q0) _EORI(r0, r0, q0);
        }
        else {
          _MVNI(r0, encode_arm_immediate(q1));
          _EORI(r0, r0, q0);
        }
      }
    }
  }
}

#define jit_movi_p(r0, i0)		arm_movi_p(_jitp, r0, i0)
__jit_inline jit_insn *
arm_movi_p(jit_state_t _jitp, jit_gpr_t r0, void *i0)
{
  jit_insn	*l;
  int		 im, q0, q1, q2, q3;
  im = (int)i0;
  if (jit_thumb_p()) {
    l = _jitp->x.pc+1;
    T2_MOVWI(r0, _jit_US((int)i0));
    T2_MOVTI(r0, _jit_US((int)i0 >> 16));
  }
  else {
    l = _jitp->x.pc;
    if (jit_armv6t_p()) {
      _MOVWI(r0, _jit_US((unsigned)i0));
      _MOVTI(r0, _jit_US((unsigned)i0 >> 16));
    }
    else {
      q0 = im & 0x000000ff;	q1 = im & 0x0000ff00;
      q2 = im & 0x00ff0000;	q3 = im & 0xff000000;
      _MOVI(r0, encode_arm_immediate(q3));
      _ORRI(r0, r0, encode_arm_immediate(q2));
      _ORRI(r0, r0, encode_arm_immediate(q1));
      _ORRI(r0, r0, q0);
    }
  }
  return (l);
}

#define jit_patchable_movi_p(i0, i1) jit_movi_p(i0, i1)

#define jit_patch_movi(i0, i1)		arm_patch_movi(i0, i1)
__jit_inline void
arm_patch_movi(jit_insn *i0, void *i1)
{
  union {
    short		*s;
    int		*i;
    void		*v;
  } u;
  jit_thumb_t		 thumb;
  unsigned int	 im;
  int			 q0, q1, q2, q3;
  im = (unsigned int)i1;
  if (jit_thumb_p()) {
    jit_assert((long)i0 & 0x1);
    u.v = i0 - 1;
    q0 = (im & 0xf000) << 4;
    q1 = (im & 0x0800) << 15;
    q2 = (im & 0x0700) << 4;
    q3 =  im & 0x00ff;
    code2thumb(thumb.s[0], thumb.s[1], u.s[0], u.s[1]);
    jit_assert(   (thumb.i & 0xfbf00000) == THUMB2_MOVWI);
    thumb.i = (thumb.i & 0xfbf00f00) | q0 | q1 | q2 | q3;
    thumb2code(thumb.s[0], thumb.s[1], u.s[0], u.s[1]);
    im >>= 16;
    q0 = (im & 0xf000) << 4;
    q1 = (im & 0x0800) << 15;
    q2 = (im & 0x0700) << 4;
    q3 =  im & 0x00ff;
    code2thumb(thumb.s[0], thumb.s[1], u.s[2], u.s[3]);
    jit_assert(   (thumb.i & 0xfbf00000) == THUMB2_MOVTI);
    thumb.i = (thumb.i & 0xfbf00f00) | q0 | q1 | q2 | q3;
    thumb2code(thumb.s[0], thumb.s[1], u.s[2], u.s[3]);
  }
  else {
    u.v = i0;
    if (jit_armv6t_p()) {
      q0 =  im &      0xfff;
      q1 = (im &     0xf000) <<  4;
      q2 = (im &  0xfff0000) >> 16;
      q3 = (im & 0xf0000000) >> 12;
      jit_assert(  (u.i[0] & 0x0ff00000) == (ARM_MOVWI));
      jit_assert(  (u.i[1] & 0x0ff00000) == (ARM_MOVTI));
      u.i[0] = (u.i[0] & 0xfff0f000) | q1 | q0;
      u.i[1] = (u.i[1] & 0xfff0f000) | q3 | q2;
    }
    else {
      q0 = im & 0x000000ff;	q1 = im & 0x0000ff00;
      q2 = im & 0x00ff0000;	q3 = im & 0xff000000;
      jit_assert(  (u.i[0] & 0x0ff00000) == (ARM_MOV|ARM_I));
      u.i[0] = (u.i[0] & 0xfffff000) | encode_arm_immediate(q3);
      jit_assert(  (u.i[1] & 0x0ff00000) == (ARM_ORR|ARM_I));
      u.i[1] = (u.i[1] & 0xfffff000) | encode_arm_immediate(q2);
      jit_assert(  (u.i[2] & 0x0ff00000) == (ARM_ORR|ARM_I));
      u.i[2] = (u.i[2] & 0xfffff000) | encode_arm_immediate(q1);
      jit_assert(  (u.i[3] & 0x0ff00000) == (ARM_ORR|ARM_I));
      u.i[3] = (u.i[3] & 0xfffff000) | encode_arm_immediate(q0);
    }
  }
}

#define jit_patch_calli(i0, i1)		arm_patch_at(_jitp, i0, i1)
#define jit_patch_at(jump, label)	arm_patch_at(_jitp, jump, label)
__jit_inline void
arm_patch_at(jit_state_t _jitp, jit_insn *jump, jit_insn *label)
{
  long		 d;
  union {
    short		*s;
    int		*i;
    void		*v;
  } u;
  jit_thumb_t		 thumb;
  u.v = jump;
  if (jit_thumb_p()) {
    jit_assert((long)u.v & 0x1);
    jit_assert((long)label & 0x1);
    u.v = ((char *)u.v - 1);
#if 0
    /* this actually matches other patterns, so cannot patch
     * automatically short jumps */
    if ((u.s[0] & THUMB_CC_B) == THUMB_CC_B) {
      jit_assert(_s8P(d));
      u.s[0] = (u.s[0] & 0xff00) | (d & 0xff);
    }
    else if ((u.s[0] & THUMB_B) == THUMB_B) {
      jit_assert(_s11P(d));
      u.s[0] = (u.s[0] & 0xf800) | (d & 0x7ff);
    }
    else
#endif
      {
        code2thumb(thumb.s[0], thumb.s[1], u.s[0], u.s[1]);
        if ((thumb.i & THUMB2_B) == THUMB2_B) {
          d = (((long)label - (long)jump) >> 1) - 2;
          jit_assert(_s24P(d));
          thumb.i = THUMB2_B | encode_thumb_jump(d);
          thumb2code(thumb.s[0], thumb.s[1], u.s[0], u.s[1]);
        }
        else if ((thumb.i & THUMB2_B) == THUMB2_CC_B) {
          d = (((long)label - (long)jump) >> 1) - 2;
          jit_assert(_s20P(d));
          thumb.i = THUMB2_CC_B | (thumb.i & 0x3c00000) |
            encode_thumb_cc_jump(d);
          thumb2code(thumb.s[0], thumb.s[1], u.s[0], u.s[1]);
        }
        else if ((thumb.i & 0xfbf08000) == THUMB2_MOVWI)
          jit_patch_movi(jump, label);
        else
          jit_assert(!"handled branch opcode");
      }
  }
  else {
    /* 0x0e000000 because 0x01000000 is (branch&) link modifier */
    if ((u.i[0] & 0x0e000000) == ARM_B) {
      d = (((long)label - (long)jump) >> 2) - 2;
      jit_assert(_s24P(d));
      u.i[0] = (u.i[0] & 0xff000000) | (d & 0x00ffffff);
    }
    else if (( jit_armv6t_p() && (u.i[0] & 0x0ff00000) == ARM_MOVWI) ||
             (!jit_armv6t_p() && (u.i[0] & 0x0ff00000) == (ARM_MOV|ARM_I)))
      jit_patch_movi(jump, label);
    else
      jit_assert(!"handled branch opcode");
  }
}

#define jit_notr_i(r0, r1)		arm_notr_i(_jitp, r0, r1)
__jit_inline void
arm_notr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1) < 8)
      T1_NOT(r0, r1);
    else
      T2_NOT(r0, r1);
  }
  else
    _NOT(r0, r1);
}

#define jit_negr_i(r0, r1)		arm_negr_i(_jitp, r0, r1)
__jit_inline void
arm_negr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1) < 8)
      T1_RSBI(r0, r1);
    else
      T2_RSBI(r0, r1, 0);
  }
  else
    _RSBI(r0, r1, 0);
}

#define jit_addr_i(r0, r1, r2)		arm_addr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_addr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8)
      T1_ADD(r0, r1, r2);
    else if (r0 == r1 || r0 == r2)
      T1_ADDX(r0, r0 == r1 ? r2 : r1);
    else
      T2_ADD(r0, r1, r2);
  }
  else
    _ADD(r0, r1, r2);
}

#define jit_addi_i(r0, r1, i0)		arm_addi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_addi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1) < 8 && !(i0 & ~7))
      T1_ADDI3(r0, r1, i0);
    else if (!jit_no_set_flags() && (r0|r1) < 8 && !(-i0 & ~7))
      T1_SUBI3(r0, r1, -i0);
    else if (!jit_no_set_flags() && r0 < 8 && r0 == r1 && !(i0 & ~0xff))
      T1_ADDI8(r0, i0);
    else if (!jit_no_set_flags() && r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
      T1_SUBI8(r0, -i0);
    else if ((i = encode_thumb_immediate(i0)) != -1)
      T2_ADDI(r0, r1, i);
    else if ((i = encode_thumb_immediate(-i0)) != -1)
      T2_SUBI(r0, r1, i);
    else if ((i = encode_thumb_word_immediate(i0)) != -1)
      T2_ADDWI(r0, r1, i);
    else if ((i = encode_thumb_word_immediate(-i0)) != -1)
      T2_SUBWI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_ADD(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _ADDI(r0, r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _SUBI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _ADD(r0, r1, reg);
    }
  }
}

#define jit_addcr_ui(r0, r1, r2)	arm_addcr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_addcr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    /* thumb auto set carry if not inside IT block */
    if ((r0|r1|r2) < 8)
      T1_ADD(r0, r1, r2);
    else
      T2_ADDS(r0, r1, r2);
  }
  else
    _ADDS(r0, r1, r2);
}

#define jit_addci_ui(r0, r1, i0)	arm_addci_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_addci_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && !(i0 & ~7))
      T1_ADDI3(r0, r1, i0);
    else if ((r0|r1) < 8 && !(-i0 & ~7))
      T1_SUBI3(r0, r1, -i0);
    else if (r0 < 8 && r0 == r1 && !(i0 & ~0xff))
      T1_ADDI8(r0, i0);
    else if (r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
      T1_SUBI8(r0, -i0);
    else if ((i = encode_thumb_immediate(i0)) != -1)
      T2_ADDSI(r0, r1, i);
    else if ((i = encode_thumb_immediate(-i0)) != -1)
      T2_SUBSI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_ADDS(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _ADDSI(r0, r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _SUBSI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _ADDS(r0, r1, reg);
    }
  }
}

#define jit_addxr_ui(r0, r1, r2)	arm_addxr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_addxr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  /* keep setting carry because don't know last ADC */
  if (jit_thumb_p()) {
    /* thumb auto set carry if not inside IT block */
    if ((r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
      T1_ADC(r0, r0 == r1 ? r2 : r1);
    else
      T2_ADCS(r0, r1, r2);
  }
  else
    _ADCS(r0, r1, r2);
}

#define jit_addxi_ui(r0, r1, i0)	arm_addxi_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_addxi_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if ((i = encode_thumb_immediate(i0)) != -1)
      T2_ADCSI(r0, r1, i);
    else if ((i = encode_thumb_immediate(-i0)) != -1)
      T2_SBCSI(r0, r1, i);
    else {
      int		no_set_flags = jit_no_set_flags();
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_no_set_flags() = 1;
      jit_movi_i(reg, i0);
      jit_no_set_flags() = no_set_flags;
      T2_ADCS(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _ADCSI(r0, r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _SBCSI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _ADCS(r0, r1, reg);
    }
  }
}

#define jit_subr_i(r0, r1, r2)		arm_subr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_subr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8)
      T1_SUB(r0, r1, r2);
    else
      T2_SUB(r0, r1, r2);
  }
  else
    _SUB(r0, r1, r2);
}

#define jit_subi_i(r0, r1, i0)		arm_subi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_subi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1) < 8 && !(i0 & ~7))
      T1_SUBI3(r0, r1, i0);
    else if (!jit_no_set_flags() && (r0|r1) < 8 && !(-i0 & ~7))
      T1_ADDI3(r0, r1, -i0);
    else if (!jit_no_set_flags() && r0 < 8 && r0 == r1 && !(i0 & ~0xff))
      T1_SUBI8(r0, i0);
    else if (!jit_no_set_flags() && r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
      T1_ADDI8(r0, -i0);
    else if ((i = encode_thumb_immediate(i0)) != -1)
      T2_SUBI(r0, r1, i);
    else if ((i = encode_thumb_immediate(-i0)) != -1)
      T2_ADDI(r0, r1, i);
    else if ((i = encode_thumb_word_immediate(i0)) != -1)
      T2_SUBWI(r0, r1, i);
    else if ((i = encode_thumb_word_immediate(-i0)) != -1)
      T2_ADDWI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_SUB(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _SUBI(r0, r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _ADDI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _SUB(r0, r1, reg);
    }
  }
}

#define jit_subcr_ui(r0, r1, r2)	arm_subcr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_subcr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    /* thumb auto set carry if not inside IT block */
    if ((r0|r1|r2) < 8)
      T1_SUB(r0, r1, r2);
    else
      T2_SUBS(r0, r1, r2);
  }
  else
    _SUBS(r0, r1, r2);
}

#define jit_subci_ui(r0, r1, i0)	arm_subci_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_subci_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && !(i0 & ~7))
      T1_SUBI3(r0, r1, i0);
    else if ((r0|r1) < 8 && !(-i0 & ~7))
      T1_ADDI3(r0, r1, -i0);
    else if (r0 < 8 && r0 == r1 && !(i0 & ~0xff))
      T1_SUBI8(r0, i0);
    else if (r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
      T1_ADDI8(r0, -i0);
    else if ((i = encode_thumb_immediate(i0)) != -1)
      T2_SUBSI(r0, r1, i);
    else if ((i = encode_thumb_immediate(-i0)) != -1)
      T2_ADDSI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_SUBS(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _SUBSI(r0, r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _ADDSI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _SUBS(r0, r1, reg);
    }
  }
}

#define jit_subxr_ui(r0, r1, r2)	arm_subxr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_subxr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  /* keep setting carry because don't know last ADC */
  if (jit_thumb_p()) {
    /* thumb auto set carry if not inside IT block */
    if ((r0|r1|r2) < 8 && r0 == r1)
      T1_SBC(r0, r2);
    else
      T2_SBCS(r0, r1, r2);
  }
  else
    _SBCS(r0, r1, r2);
}

#define jit_subxi_ui(r0, r1, i0)	arm_subxi_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_subxi_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if ((i = encode_thumb_immediate(i0)) != -1)
      T2_SBCSI(r0, r1, i);
    else if ((i = encode_thumb_immediate(-i0)) != -1)
      T2_ADCSI(r0, r1, i);
    else {
      int		no_set_flags = jit_no_set_flags();
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_no_set_flags() = 1;
      jit_movi_i(reg, i0);
      jit_no_set_flags() = no_set_flags;
      T2_SBCS(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _SBCSI(r0, r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _ADCSI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _SBCS(r0, r1, reg);
    }
  }
}

#define jit_rsbr_i(r0, r1, r2)		arm_rsbr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_rsbr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p())
    T2_RSB(r0, r1, r2);
  else
    _RSB(r0, r1, r2);
}

#define jit_rsbi_i(r0, r1, i0)		arm_rsbi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_rsbi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if (i0 == 0)
      arm_negr_i(_jitp, r0, r1);
    else if ((i = encode_thumb_immediate(i0)) != -1)
      T2_RSBI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_RSB(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _RSBI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _RSB(r0, r1, reg);
    }
  }
}

#define jit_mulr_i(r0, r1, r2)		arm_mulr_i(_jitp, r0, r1, r2)
#define jit_mulr_ui(r0, r1, r2)		arm_mulr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_mulr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && r0 == r2 && (r0|r1) < 8)
      T1_MUL(r0, r1);
    else if (!jit_no_set_flags() && r0 == r1 && (r0|r2) < 8)
      T1_MUL(r0, r2);
    else
      T2_MUL(r0, r1, r2);
  }
  else {
    if (r0 == r1 && !jit_armv6_p()) {
      if (r0 != r2)
        _MUL(r0, r2, r1);
      else {
        _MOV(JIT_TMP, r1);
        _MUL(r0, JIT_TMP, r2);
      }
    }
    else
      _MUL(r0, r1, r2);
  }
}

#define jit_muli_i(r0, r1, i0)		arm_muli_i(_jitp, r0, r1, i0)
#define jit_muli_ui(r0, r1, i0)		arm_muli_i(_jitp, r0, r1, i0)
__jit_inline void
arm_muli_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t	reg;
  reg = r0 != r1 ? r0 : JIT_TMP;
  jit_movi_i(reg, i0);
  arm_mulr_i(_jitp, r0, r1, reg);
}

#define jit_hmulr_i(r0, r1, r2)		arm_hmulr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_hmulr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p())
    T2_SMULL(JIT_TMP, r0, r1, r2);
  else {
    if (r0 == r1 && !jit_armv6_p()) {
      jit_assert(r2 != JIT_TMP);
      _SMULL(JIT_TMP, r0, r2, r1);
    }
    else
      _SMULL(JIT_TMP, r0, r1, r2);
  }
}

#define jit_hmuli_i(r0, r1, i0)		arm_hmuli_i(_jitp, r0, r1, i0)
__jit_inline void
arm_hmuli_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    jit_assert(r0 != JIT_TMP);
    jit_movi_i(JIT_TMP, i0);
    T2_SMULL(JIT_TMP, r0, r1, JIT_TMP);
  }
  else {
    if (r0 != r1 || jit_armv6_p()) {
      jit_movi_i(JIT_TMP, i0);
      _SMULL(JIT_TMP, r0, r1, JIT_TMP);
    }
    else {
      if (r0 != _R0)	    reg = _R0;
      else if (r0 != _R1)     reg = _R1;
      else if (r0 != _R2)     reg = _R2;
      else		    reg = _R3;
      _PUSH(1<<reg);
      jit_movi_i(reg, i0);
      _SMULL(JIT_TMP, r0, r1, reg);
      _POP(1<<reg);
    }
  }
}

#define jit_hmulr_ui(r0, r1, r2)	arm_hmulr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_hmulr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p())
    T2_UMULL(JIT_TMP, r0, r1, r2);
  else {
    if (r0 == r1 && !jit_armv6_p()) {
      jit_assert(r2 != JIT_TMP);
      _UMULL(JIT_TMP, r0, r2, r1);
    }
    else
      _UMULL(JIT_TMP, r0, r1, r2);
  }
}

#define jit_hmuli_ui(r0, r1, i0)	arm_hmuli_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_hmuli_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    jit_assert(r0 != JIT_TMP);
    jit_movi_i(JIT_TMP, i0);
    T2_UMULL(JIT_TMP, r0, r1, JIT_TMP);
  }
  else {
    if (r0 != r1 || jit_armv6_p()) {
      jit_movi_i(JIT_TMP, i0);
      _UMULL(JIT_TMP, r0, r1, JIT_TMP);
    }
    else {
      if (r0 != _R0)	    reg = _R0;
      else if (r0 != _R1)     reg = _R1;
      else if (r0 != _R2)     reg = _R2;
      else		    reg = _R3;
      _PUSH(1<<reg);
      jit_movi_i(reg, i0);
      _UMULL(JIT_TMP, r0, r1, reg);
      _POP(1<<reg);
    }
  }
}

static void
arm_divmod(jit_state_t _jitp, int div, int sign,
	   jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  int			 l;
  void		*p;
  l = 0xf;
  if ((int)r0 < 4)
    /* bogus extra push to align at 8 bytes */
    l = (l & ~(1 << r0)) | 0x10;
  if (jit_thumb_p())
    T1_PUSH(l);
  else
    _PUSH(l);
  if (r1 == _R1 && r2 == _R0) {
    jit_movr_i(JIT_FTMP, _R0);
    jit_movr_i(_R0, _R1);
    jit_movr_i(_R1, JIT_FTMP);
  }
  else if (r2 == _R0) {
    jit_movr_i(_R1, r2);
    jit_movr_i(_R0, r1);
  }
  else {
    jit_movr_i(_R0, r1);
    jit_movr_i(_R1, r2);
  }
#ifdef USE_SEPARATE_DIV_AND_MOD
  if (div) {
    if (sign) p = __divsi3;
    else p = __udivsi3;
  } else {
    if (sign) p = __modsi3;
    else p = __umodsi3;
  }
#else
  if (sign)		p = __aeabi_idivmod;
  else		p = __aeabi_uidivmod;
#endif
  {
    jit_movi_i(JIT_FTMP, (int)p);
    if (jit_thumb_p())
      T1_BLX(JIT_FTMP);
    else
      _BLX(JIT_FTMP);
  }
#ifdef USE_SEPARATE_DIV_AND_MOD
  jit_movr_i(r0, _R0);
#else
  if (div)
    jit_movr_i(r0, _R0);
  else
    jit_movr_i(r0, _R1);
#endif
  if (jit_thumb_p())
    T1_POP(l);
  else
    _POP(l);
}

#define jit_divr_i(r0, r1, r2)		arm_divr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_divr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_armv7r_p() && jit_thumb_p())
    T2_SDIV(r0, r1, r2);
  else
    arm_divmod(_jitp, 1, 1, r0, r1, r2);
}

#define jit_divi_i(r0, r1, i0)		arm_divi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_divi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_movi_i(JIT_TMP, i0);
  arm_divr_i(_jitp, r0, r1, JIT_TMP);
}

#define jit_divr_ui(r0, r1, r2)		arm_divr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_divr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_armv7r_p() && jit_thumb_p())
    T2_UDIV(r0, r1, r2);
  else
    arm_divmod(_jitp, 1, 0, r0, r1, r2);
}

#define jit_divi_ui(r0, r1, i0)		arm_divi_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_divi_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, unsigned int i0)
{
  jit_movi_i(JIT_TMP, i0);
  arm_divr_ui(_jitp, r0, r1, JIT_TMP);
}

#define jit_modr_i(r0, r1, r2)		arm_modr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_modr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  arm_divmod(_jitp, 0, 1, r0, r1, r2);
}

#define jit_modi_i(r0, r1, i0)		arm_modi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_modi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_movi_i(JIT_TMP, i0);
  arm_modr_i(_jitp, r0, r1, JIT_TMP);
}

#define jit_modr_ui(r0, r1, r2)		arm_modr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_modr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  arm_divmod(_jitp, 0, 0, r0, r1, r2);
}

#define jit_modi_ui(r0, r1, i0)		arm_modi_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_modi_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_movi_i(JIT_TMP, i0);
  arm_modr_ui(_jitp, r0, r1, JIT_TMP);
}

#define jit_andr_i(r0, r1, r2)		arm_andr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_andr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
      T1_AND(r0, r0 == r1 ? r2 : r1);
    else
      T2_AND(r0, r1, r2);
  }
  else
    _AND(r0, r1, r2);
}

#define jit_andi_i(r0, r1, i0)		arm_andi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_andi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if ((i = encode_thumb_immediate(i0)) != -1)
      T2_ANDI(r0, r1, i);
    else if ((i = encode_thumb_immediate(~i0)) != -1)
      T2_BICI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_AND(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _ANDI(r0, r1, i);
    else if ((i = encode_arm_immediate(~i0)) != -1)
      _BICI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _AND(r0, r1, reg);
    }
  }
}

#define jit_orr_i(r0, r1, r2)		arm_orr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_orr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
      T1_ORR(r0, r0 == r1 ? r2 : r1);
    else
      T2_ORR(r0, r1, r2);
  }
  else
    _ORR(r0, r1, r2);
}

#define jit_ori_i(r0, r1, i0)		arm_ori_i(_jitp, r0, r1, i0)
__jit_inline void
arm_ori_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if ((i = encode_thumb_immediate(i0)) != -1)
      T2_ORRI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_ORR(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _ORRI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _ORR(r0, r1, reg);
    }
  }
}

#define jit_xorr_i(r0, r1, r2)		arm_xorr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_xorr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
      T1_EOR(r0, r0 == r1 ? r2 : r1);
    else
      T2_EOR(r0, r1, r2);
  }
  else
    _EOR(r0, r1, r2);
}

#define jit_xori_i(r0, r1, i0)		arm_xori_i(_jitp, r0, r1, i0)
__jit_inline void
arm_xori_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if ((i = encode_thumb_immediate(i0)) != -1)
      T2_EORI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      T2_EOR(r0, r1, reg);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _EORI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _EOR(r0, r1, reg);
    }
  }
}

#define jit_lshr_i(r0, r1, r2)		arm_lshr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_lshr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8 && r0 == r1)
      T1_LSL(r0, r2);
    else
      T2_LSL(r0, r1, r2);
  }
  else
    _LSL(r0, r1, r2);
}

#define jit_lshi_i(r0, r1, i0)		arm_lshi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_lshi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_assert(i0 >= 0 && i0 <= 31);
  if (i0 == 0)
    jit_movr_i(r0, r1);
  else if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1) < 8)
      T1_LSLI(r0, r1, i0);
    else
      T2_LSLI(r0, r1, i0);
  }
  else
    _LSLI(r0, r1, i0);
}

#define jit_rshr_i(r0, r1, r2)		arm_rshr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_rshr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8 && r0 == r1)
      T1_ASR(r0, r2);
    else
      T2_ASR(r0, r1, r2);
  }
  else
    _ASR(r0, r1, r2);
}

#define jit_rshi_i(r0, r1, i0)		arm_rshi_i(_jitp, r0, r1, i0)
__jit_inline void
arm_rshi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_assert(i0 >= 0 && i0 <= 31);
  if (i0 == 0)
    jit_movr_i(r0, r1);
  else if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1) < 8)
      T1_ASRI(r0, r1, i0);
    else
      T2_ASRI(r0, r1, i0);
  }
  else
    _ASRI(r0, r1, i0);
}

#define jit_rshr_ui(r0, r1, r2)		arm_rshr_ui(_jitp, r0, r1, r2)
__jit_inline void
arm_rshr_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1|r2) < 8 && r0 == r1)
      T1_LSR(r0, r2);
    else
      T2_LSR(r0, r1, r2);
  }
  else
    _LSR(r0, r1, r2);
}

#define jit_rshi_ui(r0, r1, i0)		arm_rshi_ui(_jitp, r0, r1, i0)
__jit_inline void
arm_rshi_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_assert(i0 >= 0 && i0 <= 31);
  if (i0 == 0)
    jit_movr_i(r0, r1);
  else if (jit_thumb_p()) {
    if (!jit_no_set_flags() && (r0|r1) < 8)
      T1_LSRI(r0, r1, i0);
    else
      T2_LSRI(r0, r1, i0);
  }
  else
    _LSRI(r0, r1, i0);
}

__jit_inline void
arm_ccr(jit_state_t _jitp, int ct, int cf,
	jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    jit_assert((ct ^ cf) >> 28 == 1);
    if ((r1|r2) < 8)
      T1_CMP(r1, r2);
    else if ((r1&r2) & 8)
      T1_CMPX(r1, r2);
    else
      T2_CMP(r1, r2);
    _ITE(ct);
    if (r0 < 8) {
      T1_MOVI(r0, 1);
      T1_MOVI(r0, 0);
    }
    else {
      T2_MOVI(r0, 1);
      T2_MOVI(r0, 0);
    }
  }
  else {
    _CMP(r1, r2);
    _CC_MOVI(ct, r0, 1);
    _CC_MOVI(cf, r0, 0);
  }
}
__jit_inline void
arm_cci(jit_state_t _jitp, int ct, int cf,
	jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p()) {
    if (r1 < 7 && !(i0 & 0xffffff00))
      T1_CMPI(r1, i0);
    else if ((i = encode_thumb_immediate(i0)) != -1)
      T2_CMPI(r1, i);
    else if ((i = encode_thumb_immediate(-i0)) != -1)
      T2_CMNI(r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      arm_ccr(_jitp, ct, cf, r0, r1, reg);
      return;
    }
    _ITE(ct);
    if (r0 < 8) {
      T1_MOVI(r0, 1);
      T1_MOVI(r0, 0);
    }
    else {
      T2_MOVI(r0, 1);
      T2_MOVI(r0, 0);
    }
  }
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _CMPI(r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _CMNI(r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _CMP(r1, reg);
    }
    _CC_MOVI(ct, r0, 1);
    _CC_MOVI(cf, r0, 0);
  }
}
#define jit_ltr_i(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_LT,ARM_CC_GE,r0,r1,r2)
#define jit_lti_i(r0, r1, i0)	arm_cci(_jitp,ARM_CC_LT,ARM_CC_GE,r0,r1,i0)
#define jit_ltr_ui(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_LO,ARM_CC_HS,r0,r1,r2)
#define jit_lti_ui(r0, r1, i0)	arm_cci(_jitp,ARM_CC_LO,ARM_CC_HS,r0,r1,i0)
#define jit_ler_i(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_LE,ARM_CC_GT,r0,r1,r2)
#define jit_lei_i(r0, r1, i0)	arm_cci(_jitp,ARM_CC_LE,ARM_CC_GT,r0,r1,i0)
#define jit_ler_ui(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_LS,ARM_CC_HI,r0,r1,r2)
#define jit_lei_ui(r0, r1, i0)	arm_cci(_jitp,ARM_CC_LS,ARM_CC_HI,r0,r1,i0)
#define jit_eqr_i(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_EQ,ARM_CC_NE,r0,r1,r2)
#define jit_eqi_i(r0, r1, i0)	arm_cci(_jitp,ARM_CC_EQ,ARM_CC_NE,r0,r1,i0)
#define jit_ger_i(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_GE,ARM_CC_LT,r0,r1,r2)
#define jit_gei_i(r0, r1, i0)	arm_cci(_jitp,ARM_CC_GE,ARM_CC_LT,r0,r1,i0)
#define jit_ger_ui(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_HS,ARM_CC_LO,r0,r1,r2)
#define jit_gei_ui(r0, r1, i0)	arm_cci(_jitp,ARM_CC_HS,ARM_CC_LO,r0,r1,i0)
#define jit_gtr_i(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_GT,ARM_CC_LE,r0,r1,r2)
#define jit_gti_i(r0, r1, i0)	arm_cci(_jitp,ARM_CC_GT,ARM_CC_LE,r0,r1,i0)
#define jit_gtr_ui(r0, r1, r2)	arm_ccr(_jitp,ARM_CC_HI,ARM_CC_LS,r0,r1,r2)
#define jit_gti_ui(r0, r1, i0)	arm_cci(_jitp,ARM_CC_HI,ARM_CC_LS,r0,r1,i0)

#define jit_ner_i(r0, r1, r2)		arm_ner_i(_jitp, r0, r1, r2)
__jit_inline void
arm_ner_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p())
    arm_ccr(_jitp, ARM_CC_NE, ARM_CC_EQ, r0, r1, r2);
  else {
    _SUBS(r0, r1, r2);
    _CC_MOVI(ARM_CC_NE, r0, 1);
  }
}

#define jit_nei_i(r0, r1, i0)		arm_nei_i(_jitp, r0, r1, i0)
__jit_inline void
arm_nei_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  int		i;
  jit_gpr_t	reg;
  if (jit_thumb_p())
    arm_cci(_jitp, ARM_CC_NE, ARM_CC_EQ, r0, r1, i0);
  else {
    if ((i = encode_arm_immediate(i0)) != -1)
      _SUBSI(r0, r1, i);
    else if ((i = encode_arm_immediate(-i0)) != -1)
      _ADDSI(r0, r1, i);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _SUBS(r0, r1, reg);
    }
    _CC_MOVI(ARM_CC_NE, r0, 1);
  }
}

#define jit_jmpr(r0)			arm_jmpr(_jitp, r0)
__jit_inline void
arm_jmpr(jit_state_t _jitp, jit_gpr_t r0)
{
  if (jit_thumb_p())
    T1_BX(r0);
  else
    _BX(r0);
}

__jit_inline jit_insn *
arm_branch(jit_state_t _jitp, int cc, jit_insn *i0)
{
  jit_insn	*l;
  long	 d;

  l = _jitp->x.pc;
  d = (((long)i0 - (long)l) >> 2) - 2;
  
  if (!_jitl.long_jumps && _s24P(d)) {
    _CC_B(cc, d & 0x00ffffff);
  } else {
    int im = (int)i0;
    jit_assert(_jitl.long_jumps);
    if (jit_armv6t_p()) {
      _CC_MOVWI(cc, JIT_TMP, _jit_US(im));
      _CC_MOVTI(cc, JIT_TMP, _jit_US((unsigned)im >> 16));
    } else {
      int     q0, q1, q2, q3;

      q0 = im & 0x000000ff;	q1 = im & 0x0000ff00;
      q2 = im & 0x00ff0000;	q3 = im & 0xff000000;
      _CC_MOVI(cc, JIT_TMP, encode_arm_immediate(q3));
      _CC_ORRI(cc, JIT_TMP, JIT_TMP, encode_arm_immediate(q2));
      _CC_ORRI(cc, JIT_TMP, JIT_TMP, encode_arm_immediate(q1));
      _CC_ORRI(cc, JIT_TMP, JIT_TMP, q0);
    }
    _CC_MOV(cc, _R15, JIT_TMP);
  }

  return l;
}

__jit_inline int invert_cc(int cc)
{
  return cc ^ ARM_CC_NE;
}

__jit_inline jit_insn *
thumb_branch(jit_state_t _jitp, int cc, jit_insn *i0)
{
  jit_insn	*l;
  long	 d;

  jit_assert((long)i0 & 0x1);

  /* use only thumb2 conditional as does not know if will be patched */
  l = _jitp->x.pc+1;
  d = (((long)i0 - (long)l) >> 1) - 2;
  if (!_jitl.long_jumps && _s20P(d)) {
    if (cc == ARM_CC_AL)
      T2_B(encode_thumb_jump(d));
    else
      T2_CC_B(cc, encode_thumb_cc_jump(d));
  } else {
    jit_assert(_jitl.long_jumps);
    jit_movi_p(JIT_TMP, i0);
    if (cc != ARM_CC_AL) {
      cc = invert_cc(cc);
      T2_CC_B(cc, 1);
    }
    T1_MOV(_R15, JIT_TMP);
  }

  return l;
}

#define jit_jmpi(i0)			arm_jmpi(_jitp, i0)
__jit_inline jit_insn *
arm_jmpi(jit_state_t _jitp, void *i0)
{
  jit_insn	*l;
  if (jit_thumb_p()) {
    l = thumb_branch(_jitp, ARM_CC_AL, i0);
  }
  else {
    l = arm_branch(_jitp, ARM_CC_AL, i0);
  }
  return (l);
}

__jit_inline jit_insn *
arm_bccr(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  jit_insn *l;

  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_CMP(r0, r1);
    else if ((r0&r1) & 8)
      T1_CMPX(r0, r1);
    else
      T2_CMP(r0, r1);
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    _CMP(r0, r1);
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}
__jit_inline jit_insn *
arm_bcci(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, int i1)
{
  jit_insn	*l;
  int		 i;
  if (jit_thumb_p()) {
    if (r0 < 7 && !(i1 & 0xffffff00))
      T1_CMPI(r0, i1);
    else if ((i = encode_thumb_immediate(i1)) != -1)
      T2_CMPI(r0, i);
    else if ((i = encode_thumb_immediate(-i1)) != -1)
      T2_CMNI(r0, i);
    else {
      jit_movi_i(JIT_TMP, i1);
      T2_CMP(r0, JIT_TMP);
    }
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    if ((i = encode_arm_immediate(i1)) != -1)
      _CMPI(r0, i);
    else if ((i = encode_arm_immediate(-i1)) != -1)
      _CMNI(r0, i);
    else {
      jit_movi_i(JIT_TMP, i1);
      _CMP(r0, JIT_TMP);
    }
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}
#define jit_bltr_i(i0, r0, r1)		arm_bccr(_jitp, ARM_CC_LT, i0, r0, r1)
#define jit_blti_i(i0, r0, i1)		arm_bcci(_jitp, ARM_CC_LT, i0, r0, i1)
#define jit_bltr_ui(i0, r0, r1) 	arm_bccr(_jitp, ARM_CC_LO, i0, r0, r1)
#define jit_blti_ui(i0, r0, i1) 	arm_bcci(_jitp, ARM_CC_LO, i0, r0, i1)
#define jit_bler_i(i0, r0, r1)		arm_bccr(_jitp, ARM_CC_LE, i0, r0, r1)
#define jit_blei_i(i0, r0, i1)		arm_bcci(_jitp, ARM_CC_LE, i0, r0, i1)
#define jit_bler_ui(i0, r0, r1) 	arm_bccr(_jitp, ARM_CC_LS, i0, r0, r1)
#define jit_blei_ui(i0, r0, i1) 	arm_bcci(_jitp, ARM_CC_LS, i0, r0, i1)
#define jit_beqr_i(i0, r0, r1)		arm_bccr(_jitp, ARM_CC_EQ, i0, r0, r1)
#define jit_beqi_i(i0, r0, i1)		arm_bcci(_jitp, ARM_CC_EQ, i0, r0, i1)
#define jit_bger_i(i0, r0, r1)		arm_bccr(_jitp, ARM_CC_GE, i0, r0, r1)
#define jit_bgei_i(i0, r0, i1)		arm_bcci(_jitp, ARM_CC_GE, i0, r0, i1)
#define jit_bger_ui(i0, r0, r1) 	arm_bccr(_jitp, ARM_CC_HS, i0, r0, r1)
#define jit_bgei_ui(i0, r0, i1) 	arm_bcci(_jitp, ARM_CC_HS, i0, r0, i1)
#define jit_bgtr_i(i0, r0, r1)		arm_bccr(_jitp, ARM_CC_GT, i0, r0, r1)
#define jit_bgti_i(i0, r0, i1)		arm_bcci(_jitp, ARM_CC_GT, i0, r0, i1)
#define jit_bgtr_ui(i0, r0, r1) 	arm_bccr(_jitp, ARM_CC_HI, i0, r0, r1)
#define jit_bgti_ui(i0, r0, i1) 	arm_bcci(_jitp, ARM_CC_HI, i0, r0, i1)
#define jit_bner_i(i0, r0, r1)		arm_bccr(_jitp, ARM_CC_NE, i0, r0, r1)
#define jit_bnei_i(i0, r0, i1)		arm_bcci(_jitp, ARM_CC_NE, i0, r0, i1)

__jit_inline jit_insn *
arm_baddr(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  jit_insn	*l;

  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_ADD(r0, r0, r1);
    else
      T2_ADDS(r0, r0, r1);
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    _ADDS(r0, r0, r1);
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}
__jit_inline jit_insn *
arm_baddi(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, int i1)
{
  jit_insn	*l;
  int		 i;
  if (jit_thumb_p()) {
    if (r0 < 8 && !(i1 & ~7))
      T1_ADDI3(r0, r0, i1);
    else if (r0 < 8 && !(-i1 & ~7))
      T1_SUBI3(r0, r0, -i1);
    else if (r0 < 8 && !(i1 & ~0xff))
      T1_ADDI8(r0, i1);
    else if (r0 < 8 && !(-i1 & ~0xff))
      T1_SUBI8(r0, -i1);
    else if ((i = encode_thumb_immediate(i1)) != -1)
      T2_ADDSI(r0, r0, i);
    else if ((i = encode_thumb_immediate(-i1)) != -1)
      T2_SUBSI(r0, r0, i);
    else {
      jit_movi_i(JIT_TMP, i1);
      T2_ADDS(r0, r0, JIT_TMP);
    }
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    if ((i = encode_arm_immediate(i1)) != -1)
      _ADDSI(r0, r0, i);
    else if ((i = encode_arm_immediate(-i1)) != -1)
      _SUBSI(r0, r0, i);
    else {
      jit_movi_i(JIT_TMP, i1);
      _ADDS(r0, r0, JIT_TMP);
    }
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}
#define jit_boaddr_i(i0, r0, r1)	arm_baddr(_jitp, ARM_CC_VS, i0, r0, r1)
#define jit_boaddi_i(i0, r0, i1)	arm_baddi(_jitp, ARM_CC_VS, i0, r0, i1)
#define jit_boaddr_ui(i0, r0, r1)	arm_baddr(_jitp, ARM_CC_HS, i0, r0, r1)
#define jit_boaddi_ui(i0, r0, i1)	arm_baddi(_jitp, ARM_CC_HS, i0, r0, i1)

__jit_inline jit_insn *
arm_bsubr(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  jit_insn	*l;
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_SUB(r0, r0, r1);
    else
      T2_SUBS(r0, r0, r1);
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    _SUBS(r0, r0, r1);
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}

__jit_inline jit_insn *
arm_bsubi(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, int i1)
{
  jit_insn	*l;
  int		 i;
  if (jit_thumb_p()) {
    if (r0 < 8 && !(i1 & ~7))
      T1_SUBI3(r0, r0, i1);
    else if (r0 < 8 && !(-i1 & ~7))
      T1_ADDI3(r0, r0, -i1);
    else if (r0 < 8 && !(i1 & ~0xff))
      T1_SUBI8(r0, i1);
    else if (r0 < 8 && !(-i1 & ~0xff))
      T1_ADDI8(r0, -i1);
    else if ((i = encode_thumb_immediate(i1)) != -1)
      T2_SUBSI(r0, r0, i);
    else if ((i = encode_thumb_immediate(-i1)) != -1)
      T2_ADDSI(r0, r0, i);
    else {
      jit_movi_i(JIT_TMP, i1);
      T2_SUBS(r0, r0, JIT_TMP);
    }
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    if ((i = encode_arm_immediate(i1)) != -1)
      _SUBSI(r0, r0, i);
    else if ((i = encode_arm_immediate(-i1)) != -1)
      _ADDSI(r0, r0, i);
    else {
      jit_movi_i(JIT_TMP, i1);
      _SUBS(r0, r0, JIT_TMP);
    }
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}
#define jit_bosubr_i(i0, r0, r1)	arm_bsubr(_jitp, ARM_CC_VS, i0, r0, r1)
#define jit_bosubi_i(i0, r0, i1)	arm_bsubi(_jitp, ARM_CC_VS, i0, r0, i1)
#define jit_bosubr_ui(i0, r0, r1)	arm_bsubr(_jitp, ARM_CC_LO, i0, r0, r1)
#define jit_bosubi_ui(i0, r0, i1)	arm_bsubi(_jitp, ARM_CC_LO, i0, r0, i1)

#define jit_bomulr_i(a, r0, r1)		arm_bomulr_i(_jitp, a, r0, r0, r1)
#define jit_bomulr_ui(a, r0, r1)		arm_bomulr_i(_jitp, a, r0, r0, r1)
__jit_inline jit_insn *
arm_bomulr_i(jit_state_t _jitp, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  jit_insn	*l;

  if (jit_thumb_p()) {
    T2_SMULL(r0, JIT_TMP, r1, r2);
    T1_ASRI(JIT_TMP2, r0, 31);
    T1_CMP(JIT_TMP, JIT_TMP2);
    l = thumb_branch(_jitp, ARM_CC_NE, i0);
  } else {
    if (r0 == r1 && !jit_armv6_p()) {
      jit_assert(r2 != JIT_TMP);
      _SMULL(r0, JIT_TMP, r2, r1);
    }
    else
      _SMULL(r0, JIT_TMP, r1, r2);
    _CC_CMPSH(ARM_CC_AL, JIT_TMP, r0, ARM_ASR, 31);
    l = arm_branch(_jitp, ARM_CC_NE, i0);
  }

  return l;
}

__jit_inline jit_insn *
arm_bmxr(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, jit_gpr_t r1)
{
  jit_insn	*l;
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_TST(r0, r1);
    else
      T2_TST(r0, r1);
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    if (jit_armv5_p())
      _TST(r0, r1);
    else
      _ANDS(JIT_TMP, r0, r1);
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}
__jit_inline jit_insn *
arm_bmxi(jit_state_t _jitp, int cc, jit_insn *i0, jit_gpr_t r0, int i1)
{
  jit_insn	*l;
  int		 i;
  if (jit_thumb_p()) {
    if ((i = encode_thumb_immediate(i1)) != -1)
      T2_TSTI(r0, i);
    else {
      jit_movi_i(JIT_TMP, i1);
      T2_TST(r0, JIT_TMP);
    }
    l = thumb_branch(_jitp, cc, i0);
  }
  else {
    if (jit_armv5_p()) {
      if ((i = encode_arm_immediate(i1)) != -1)
        _TSTI(r0, i);
      else {
        jit_movi_i(JIT_TMP, i1);
        _TST(r0, JIT_TMP);
      }
    }
    else {
      if ((i = encode_arm_immediate(i1)) != -1)
        _ANDSI(JIT_TMP, r0, i);
      else if ((i = encode_arm_immediate(~i1)) != -1)
        _BICSI(JIT_TMP, r0, i);
      else {
        jit_movi_i(JIT_TMP, i1);
        _ANDS(JIT_TMP, r0, JIT_TMP);
      }
    }
    l = arm_branch(_jitp, cc, i0);
  }
  return (l);
}
#define jit_bmsr_i(i0, r0, r1)		arm_bmxr(_jitp, ARM_CC_NE, i0, r0, r1)
#define jit_bmsi_i(i0, r0, i1)		arm_bmxi(_jitp, ARM_CC_NE, i0, r0, i1)
#define jit_bmcr_i(i0, r0, r1)		arm_bmxr(_jitp, ARM_CC_EQ, i0, r0, r1)
#define jit_bmci_i(i0, r0, i1)		arm_bmxi(_jitp, ARM_CC_EQ, i0, r0, i1)

#define jit_ldr_c(r0, r1)		arm_ldr_c(_jitp, r0, r1)
__jit_inline void
arm_ldr_c(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_LDRSBI(r0, r1, 0);
  else
    _LDRSBI(r0, r1, 0);
}

#define jit_ldi_c(r0, i0)		arm_ldi_c(_jitp, r0, i0)
__jit_inline void
arm_ldi_c(jit_state_t _jitp, jit_gpr_t r0, void *i0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_LDRSBI(r0, JIT_TMP, 0);
  else
    _LDRSBI(r0, JIT_TMP, 0);
}

#define jit_ldxr_c(r0, r1, r2)		arm_ldxr_c(_jitp, r0, r1, r2)
__jit_inline void
arm_ldxr_c(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_LDRSB(r0, r1, r2);
    else
      T2_LDRSB(r0, r1, r2);
  }
  else
    _LDRSB(r0, r1, r2);
}

#define jit_ldxi_c(r0, r1, i0)		arm_ldxi_c(_jitp, r0, r1, i0)
__jit_inline void
arm_ldxi_c(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t		reg;
  if (jit_thumb_p()) {
    if (i0 >= 0 && i0 <= 255)
      T2_LDRSBI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_LDRSBIN(r0, r1, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_LDRSBWI(r0, r1, i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      if ((r0|r1|reg) < 8)
        T1_LDRSB(r0, r1, reg);
      else
        T2_LDRSB(r0, r1, reg);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 255)
      _LDRSBI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      _LDRSBIN(r0, r1, -i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _LDRSB(r0, r1, reg);
    }
  }
}

#define jit_ldr_uc(r0, r1)		arm_ldr_uc(_jitp, r0, r1)
__jit_inline void
arm_ldr_uc(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_LDRBI(r0, r1, 0);
  else
    _LDRBI(r0, r1, 0);
}

#define jit_ldi_uc(r0, i0)		arm_ldi_uc(_jitp, r0, i0)
__jit_inline void
arm_ldi_uc(jit_state_t _jitp, jit_gpr_t r0, void *i0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_LDRBI(r0, JIT_TMP, 0);
  else
    _LDRBI(r0, JIT_TMP, 0);
}

#define jit_ldxr_uc(r0, r1, r2)		arm_ldxr_uc(_jitp, r0, r1, r2)
__jit_inline void
arm_ldxr_uc(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_LDRB(r0, r1, r2);
    else
      T2_LDRB(r0, r1, r2);
  }
  else
    _LDRB(r0, r1, r2);
}

#define jit_ldxi_uc(r0, r1, i0)		arm_ldxi_uc(_jitp, r0, r1, i0)
__jit_inline void
arm_ldxi_uc(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t		reg;
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && i0 >= 0 && i0 < 0x20)
      T1_LDRBI(r0, r1, i0);
    else if (i0 >= 0 && i0 <= 255)
      T2_LDRBI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_LDRBIN(r0, r1, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_LDRBWI(r0, r1, i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      if ((r0|r1|reg) < 8)
        T1_LDRB(r0, r1, reg);
      else
        T2_LDRB(r0, r1, reg);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 4095)
      _LDRBI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -4095)
      _LDRBIN(r0, r1, -i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _LDRB(r0, r1, reg);
    }
  }
}

#define jit_ldr_s(r0, r1)		arm_ldr_s(_jitp, r0, r1)
__jit_inline void
arm_ldr_s(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_LDRSHI(r0, r1, 0);
  else
    _LDRSHI(r0, r1, 0);
}

#define jit_ldi_s(r0, i0)		arm_ldi_s(_jitp, r0, i0)
__jit_inline void
arm_ldi_s(jit_state_t _jitp, jit_gpr_t r0, void *i0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_LDRSHI(r0, JIT_TMP, 0);
  else
    _LDRSHI(r0, JIT_TMP, 0);
}

#define jit_ldxr_s(r0, r1, r2)		arm_ldxr_s(_jitp, r0, r1, r2)
__jit_inline void
arm_ldxr_s(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_LDRSH(r0, r1, r2);
    else
      T2_LDRSH(r0, r1, r2);
  }
  else
    _LDRSH(r0, r1, r2);
}

#define jit_ldxi_s(r0, r1, i0)		arm_ldxi_s(_jitp, r0, r1, (int)i0)
__jit_inline void
arm_ldxi_s(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t		reg;
  if (jit_thumb_p()) {
    if (i0 >= 0 && i0 <= 255)
      T2_LDRSHI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_LDRSHIN(r0, r1, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_LDRSHWI(r0, r1, i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      if ((r0|r1|reg) < 8)
        T1_LDRSH(r0, r1, reg);
      else
        T2_LDRSH(r0, r1, reg);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 255)
      _LDRSHI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      _LDRSHIN(r0, r1, -i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _LDRSH(r0, r1, reg);
    }
  }

}
#define jit_ldr_us(r0, r1)		arm_ldr_us(_jitp, r0, r1)
__jit_inline void
arm_ldr_us(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_LDRHI(r0, r1, 0);
  else
    _LDRHI(r0, r1, 0);
}

#define jit_ldi_us(r0, i0)		arm_ldi_us(_jitp, r0, i0)
__jit_inline void
arm_ldi_us(jit_state_t _jitp, jit_gpr_t r0, void *i0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_LDRHI(r0, JIT_TMP, 0);
  else
    _LDRHI(r0, JIT_TMP, 0);
}

#define jit_ldxr_us(r0, r1, r2)		arm_ldxr_us(_jitp, r0, r1, r2)
__jit_inline void
arm_ldxr_us(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_LDRH(r0, r1, r2);
    else
      T2_LDRH(r0, r1, r2);
  }
  else
    _LDRH(r0, r1, r2);
}

#define jit_ldxi_us(r0, r1, i0)		arm_ldxi_us(_jitp, r0, r1, i0)
__jit_inline void
arm_ldxi_us(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t		reg;
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 1) && (i0 >> 1) < 0x20)
      T1_LDRHI(r0, r1, i0 >> 1);
    else if (i0 >= 0 && i0 <= 255)
      T2_LDRHI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_LDRHIN(r0, r1, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_LDRHWI(r0, r1, i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      if ((r0|r1|reg) < 8)
        T1_LDRH(r0, r1, reg);
      else
        T2_LDRH(r0, r1, reg);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 255)
      _LDRHI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      _LDRHIN(r0, r1, -i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _LDRH(r0, r1, reg);
    }
  }
}

#define jit_ldr_i(r0, r1)		arm_ldr_i(_jitp, r0, r1)
__jit_inline void
arm_ldr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_LDRI(r0, r1, 0);
  else
    _LDRI(r0, r1, 0);
}

#define jit_ldi_i(r0, i0)		arm_ldi_i(_jitp, r0, i0)
__jit_inline void
arm_ldi_i(jit_state_t _jitp, jit_gpr_t r0, void *i0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_LDRI(r0, JIT_TMP, 0);
  else
    _LDRI(r0, JIT_TMP, 0);
}

#define jit_ldxr_i(r0, r1, r2)		arm_ldxr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_ldxr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_LDR(r0, r1, r2);
    else
      T2_LDR(r0, r1, r2);
  }
  else
    _LDR(r0, r1, r2);
}

#define jit_ldxi_i(r0, r1, i0)		arm_ldxi_i(_jitp, r0, r1, (int)i0)
__jit_inline void
arm_ldxi_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, int i0)
{
  jit_gpr_t		reg;
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 3) && (i0 >> 2) < 0x20)
      T1_LDRI(r0, r1, i0 >> 2);
    else if (r1 == _R13 && r0 < 8 &&
             i0 >= 0 && !(i0 & 3) && (i0 >> 2) <= 255)
      T1_LDRISP(r0, i0 >> 2);
    else if (i0 >= 0 && i0 <= 255)
      T2_LDRI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_LDRIN(r0, r1, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_LDRWI(r0, r1, i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      if ((r0|r1|reg) < 8)
        T1_LDR(r0, r1, reg);
      else
        T2_LDR(r0, r1, reg);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 4095)
      _LDRI(r0, r1, i0);
    else if (i0 < 0 && i0 >= -4095)
      _LDRIN(r0, r1, -i0);
    else {
      reg = r0 != r1 ? r0 : JIT_TMP;
      jit_movi_i(reg, i0);
      _LDR(r0, r1, reg);
    }
  }
}

#define jit_str_c(r0, r1)		arm_str_c(_jitp, r0, r1)
__jit_inline void
arm_str_c(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_STRBI(r1, r0, 0);
  else
    _STRBI(r1, r0, 0);
}

#define jit_sti_c(r0, i0)		arm_sti_c(_jitp, r0, i0)
__jit_inline void
arm_sti_c(jit_state_t _jitp, void *i0, jit_gpr_t r0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_STRBI(r0, JIT_TMP, 0);
  else
    _STRBI(r0, JIT_TMP, 0);
}

#define jit_stxr_c(r0, r1, r2)		arm_stxr_c(_jitp, r0, r1, r2)
__jit_inline void
arm_stxr_c(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_STRB(r2, r1, r0);
    else
      T2_STRB(r2, r1, r0);
  }
  else
    _STRB(r2, r1, r0);
}

#define jit_stxi_c(r0, r1, i0)		arm_stxi_c(_jitp, r0, r1, i0)
__jit_inline void
arm_stxi_c(jit_state_t _jitp, int i0, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && i0 >= 0 && i0 < 0x20)
      T1_STRBI(r1, r0, i0);
    else if (i0 >= 0 && i0 <= 255)
      T2_STRBI(r1, r0, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_STRBIN(r1, r0, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_STRBWI(r1, r0, i0);
    else {
      jit_movi_i(JIT_TMP, (int)i0);
      if ((r0|r1|JIT_TMP) < 8)
        T1_STRB(r1, r0, JIT_TMP);
      else
        T2_STRB(r1, r0, JIT_TMP);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 4095)
      _STRBI(r1, r0, i0);
    else if (i0 < 0 && i0 >= -4095)
      _STRBIN(r1, r0, -i0);
    else {
      jit_movi_i(JIT_TMP, i0);
      _STRB(r1, r0, JIT_TMP);
    }
  }
}

#define jit_str_s(r0, r1)		arm_str_s(_jitp, r0, r1)
__jit_inline void
arm_str_s(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_STRHI(r1, r0, 0);
  else
    _STRHI(r1, r0, 0);
}

#define jit_sti_s(r0, i0)		arm_sti_s(_jitp, r0, i0)
__jit_inline void
arm_sti_s(jit_state_t _jitp, void *i0, jit_gpr_t r0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_STRHI(r0, JIT_TMP, 0);
  else
    _STRHI(r0, JIT_TMP, 0);
}

#define jit_stxr_s(r0, r1, r2)		arm_stxr_s(_jitp, r0, r1, r2)
__jit_inline void
arm_stxr_s(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_STRH(r2, r1, r0);
    else
      T2_STRH(r2, r1, r0);
  }
  else
    _STRH(r2, r1, r0);
}

#define jit_stxi_s(r0, r1, i0)		arm_stxi_s(_jitp, (int)r0, r1, i0)
__jit_inline void
arm_stxi_s(jit_state_t _jitp, int i0, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 1) && (i0 >> 1) < 0x20)
      T1_STRHI(r1, r0, i0 >> 1);
    else if (i0 >= 0 && i0 <= 255)
      T2_STRHI(r1, r0, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_STRHIN(r1, r0, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_STRHWI(r1, r0, i0);
    else {
      jit_movi_i(JIT_TMP, (int)i0);
      if ((r0|r1|JIT_TMP) < 8)
        T1_STRH(r1, r0, JIT_TMP);
      else
        T2_STRH(r1, r0, JIT_TMP);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 255)
      _STRHI(r1, r0, i0);
    else if (i0 < 0 && i0 >= -255)
      _STRHIN(r1, r0, -i0);
    else {
      jit_movi_i(JIT_TMP, i0);
      _STRH(r1, r0, JIT_TMP);
    }
  }
}

#define jit_str_i(r0, r1)		arm_str_i(_jitp, r0, r1)
__jit_inline void
arm_str_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p())
    T2_STRI(r1, r0, 0);
  else
    _STRI(r1, r0, 0);
}

#define jit_sti_i(r0, i0)		arm_sti_i(_jitp, r0, i0)
__jit_inline void
arm_sti_i(jit_state_t _jitp, void *i0, jit_gpr_t r0)
{
  jit_movi_i(JIT_TMP, (int)i0);
  if (jit_thumb_p())
    T2_STRI(r0, JIT_TMP, 0);
  else
    _STRI(r0, JIT_TMP, 0);
}

#define jit_stxr_i(r0, r1, r2)		arm_stxr_i(_jitp, r0, r1, r2)
__jit_inline void
arm_stxr_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1, jit_gpr_t r2)
{
  if (jit_thumb_p()) {
    if ((r0|r1|r2) < 8)
      T1_STR(r2, r1, r0);
    else
      T2_STR(r2, r1, r0);
  }
  else
    _STR(r2, r1, r0);
}

#define jit_stxi_i(r0, r1, i0)		arm_stxi_i(_jitp, (int)r0, r1, i0)
__jit_inline void
arm_stxi_i(jit_state_t _jitp, int i0, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 3) && (i0 >> 2) < 0x20)
      T1_STRI(r1, r0, i0 >> 2);
    else if (r0 == _R13 && r1 < 8 &&
             i0 >= 0 && !(i0 & 3) && (i0 >> 2) <= 255)
      T1_STRISP(r1, i0 >> 2);
    else if (i0 >= 0 && i0 <= 255)
      T2_STRI(r1, r0, i0);
    else if (i0 < 0 && i0 >= -255)
      T2_STRIN(r1, r0, -i0);
    else if (i0 >= 0 && i0 <= 4095)
      T2_STRWI(r1, r0, i0);
    else {
      jit_movi_i(JIT_TMP, (int)i0);
      if ((r0|r1|JIT_TMP) < 8)
        T1_STR(r1, r0, JIT_TMP);
      else
        T2_STR(r1, r0, JIT_TMP);
    }
  }
  else {
    if (i0 >= 0 && i0 <= 4095)
      _STRI(r1, r0, i0);
    else if (i0 < 0 && i0 >= -4095)
      _STRIN(r1, r0, -i0);
    else {
      jit_movi_i(JIT_TMP, i0);
      _STR(r1, r0, JIT_TMP);
    }
  }
}

#if __BYTE_ORDER == __LITTLE_ENDIAN
/* inline glibc htons (without register clobber) */
#define jit_ntoh_us(r0, r1)		arm_ntoh_us(_jitp, r0, r1)
__jit_inline void
arm_ntoh_us(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_REV16(r0, r1);
    else
      T2_REV16(r0, r1);
  }
  else {
    if (jit_armv6_p())
      _REV16(r0, r1);
    else {
      _LSLI(JIT_TMP, r1, 24);
      _LSRI(r0, r1, 8);
      _ORR_SI(r0, r0, JIT_TMP, ARM_LSR, 16);
    }
  }
}

/* inline glibc htonl (without register clobber) */
#define jit_ntoh_ui(r0, r1)		arm_ntoh_ui(_jitp, r0, r1)
__jit_inline void
arm_ntoh_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_REV(r0, r1);
    else
      T2_REV(r0, r1);
  }
  else {
    if (jit_armv6_p())
      _REV(r0, r1);
    else {
      _EOR_SI(JIT_TMP, r1, r1, ARM_ROR, 16);
      _LSRI(JIT_TMP, JIT_TMP, 8);
      _BICI(JIT_TMP, JIT_TMP, encode_arm_immediate(0xff00));
      _EOR_SI(r0, JIT_TMP, r1, ARM_ROR, 8);
    }
  }
}
#endif

#define jit_extr_c_i(r0, r1)		arm_extr_c_i(_jitp, r0, r1)
__jit_inline void
arm_extr_c_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_SXTB(r0, r1);
    else
      T2_SXTB(r0, r1);
  }
  else {
    if (jit_armv6_p())
      _SXTB(r0, r1);
    else {
      _LSLI(r0, r1, 24);
      _ASRI(r0, r0, 24);
    }
  }
}

#define jit_extr_c_ui(r0, r1)		arm_extr_c_ui(_jitp, r0, r1)
__jit_inline void
arm_extr_c_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_UXTB(r0, r1);
    else
      T2_UXTB(r0, r1);
  }
  else {
    if (jit_armv6_p())
      _UXTB(r0, r1);
    else
      _ANDI(r0, r1, 0xff);
  }
}

#define jit_extr_s_i(r0, r1)		arm_extr_s_i(_jitp, r0, r1)
__jit_inline void
arm_extr_s_i(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_SXTH(r0, r1);
    else
      T2_SXTH(r0, r1);
  }
  else {
    if (jit_armv6_p())
      _SXTH(r0, r1);
    else {
      _LSLI(r0, r1, 16);
      _ASRI(r0, r0, 16);
    }
  }
}

#define jit_extr_s_ui(r0, r1)		arm_extr_s_ui(_jitp, r0, r1)
__jit_inline void
arm_extr_s_ui(jit_state_t _jitp, jit_gpr_t r0, jit_gpr_t r1)
{
  if (jit_thumb_p()) {
    if ((r0|r1) < 8)
      T1_UXTH(r0, r1);
    else
      T2_UXTH(r0, r1);
  }
  else {
    if (jit_armv6_p())
      _UXTH(r0, r1);
    else {
      /* _ANDI(r0, r1, 0xffff) needs more instructions */
      _LSLI(r0, r1, 16);
      _LSRI(r0, r0, 16);
    }
  }
}

#define JIT_NEXT_FP_OFFSET              8
#define JIT_FRAME_EXTRA_SPACE           16
#define JIT_FRAME_EXTRA_SPACE_OFFSET    (-JIT_FRAME_EXTRA_SPACE)
#define JIT_ARG_STACK_OFFSET            0

#define jit_prolog(n)			arm_prolog(_jitp, n)
__jit_inline void
arm_prolog(jit_state_t _jitp, int i0)
{
  if (jit_thumb_p()) {
    T2_PUSH(/* arguments (should keep state and only save "i0" registers) */
            (1<<_R0)|(1<<_R1)|(1<<_R2)|(1<<_R3)|
            (1<<_R4)|(1<<_R5)|(1<<_R6)|(1<<_R7)|
            /* previous fp and return address */
            (1<<JIT_FP)|(1<<JIT_LR));
    T2_MOV(JIT_FP, JIT_SP);
  }
  else {
    _PUSH(/* arguments (should keep state and only save "i0" registers) */
          (1<<_R0)|(1<<_R1)|(1<<_R2)|(1<<_R3)|
          (1<<_R4)|(1<<_R5)|(1<<_R6)|(1<<_R7)|
          /* previous fp and return address */
          (1<<JIT_FP)|(1<<JIT_LR));
    _MOV(JIT_FP, JIT_SP);
  }

  _jitl.nextarg_get = _jitl.nextarg_getf = 0;

  if (jit_swf_p()) {
    /* 6 soft double precision float registers */
    jit_subi_i(JIT_SP, JIT_SP, JIT_FRAME_EXTRA_SPACE + 48);
  } else if (JIT_FRAME_EXTRA_SPACE) {
    jit_subi_i(JIT_SP, JIT_SP, JIT_FRAME_EXTRA_SPACE);
  }
}

#define jit_callr(r0)			arm_callr(_jitp, r0)
static void
arm_callr(jit_state_t _jitp, jit_gpr_t r0)
{
  if (jit_thumb_p())
    T1_BLX(r0);
  else
    _BLX(r0);
}

#define jit_calli(i0)			arm_calli(_jitp, i0)
__jit_inline jit_insn *
arm_calli(jit_state_t _jitp, void *i0)
{
  jit_insn	*l;
  l = _jitp->x.pc;
  jit_movi_p(JIT_TMP, i0);
  if (jit_thumb_p()) {
    l++;
    T1_BLX(JIT_TMP);
  } else
    _BLX(JIT_TMP);
  return (l);
}

#define jit_short_calli(i0)			arm_short_calli(_jitp, i0)
__jit_inline jit_insn *
arm_short_calli(jit_state_t _jitp, void *i0)
{
  jit_insn	*l;
  long	 d;

  if (jit_thumb_p()) {
    jit_assert((long)i0 & 0x1);
    l = _jitp->x.pc+1;
    d = (((long)i0 - (long)l) >> 1) - 2;
    T2_BLI(encode_thumb_jump(d));
  } else {
    l = _jitp->x.pc;
    d = (((long)i0 - (long)l) >> 2) - 2;
    _BLI(d & 0x00ffffff);
  }

  return (l);
}

#define jit_prepare_i(i0)		arm_prepare_i(_jitp, i0)
__jit_inline void
arm_prepare_i(jit_state_t _jitp, int i0)
{
  jit_assert(i0 >= 0 && !_jitl.nextarg_put);
}

#define jit_arg_c()			arm_arg_i(_jitp)
#define jit_arg_uc()			arm_arg_i(_jitp)
#define jit_arg_s()			arm_arg_i(_jitp)
#define jit_arg_us()			arm_arg_i(_jitp)
#define jit_arg_i()			arm_arg_i(_jitp)
#define jit_arg_ui()			arm_arg_i(_jitp)
#define jit_arg_l()			arm_arg_i(_jitp)
#define jit_arg_ul()			arm_arg_i(_jitp)
#define jit_arg_p()			arm_arg_i(_jitp)
__jit_inline int
arm_arg_i(jit_state_t _jitp)
{
  int		ofs = _jitl.nextarg_get++;
  return (ofs);
}

#define jit_getarg_c(r0, i0)		arm_getarg_c(_jitp, r0, i0)
__jit_inline void
arm_getarg_c(jit_state_t _jitp, jit_gpr_t r0, int i0)
{
  if (i0 < 4)
    i0 <<= 2;
#if __BYTE_ORDER == __BIG_ENDIAN
  i0 += sizeof(int) - sizeof(char);
#endif
  jit_ldxi_c(r0, JIT_FP, i0);
}

#define jit_getarg_uc(r0, i0)		arm_getarg_uc(_jitp, r0, i0)
__jit_inline void
arm_getarg_uc(jit_state_t _jitp, jit_gpr_t r0, int i0)
{
  if (i0 < 4)
    i0 <<= 2;
#if __BYTE_ORDER == __BIG_ENDIAN
  i0 += sizeof(int) - sizeof(char);
#endif
  jit_ldxi_uc(r0, JIT_FP, i0);
}

#define jit_getarg_s(r0, i0)		arm_getarg_s(_jitp, r0, i0)
__jit_inline void
arm_getarg_s(jit_state_t _jitp, jit_gpr_t r0, int i0)
{
  if (i0 < 4)
    i0 <<= 2;
#if __BYTE_ORDER == __BIG_ENDIAN
  i0 += sizeof(int) - sizeof(short);
#endif
  jit_ldxi_s(r0, JIT_FP, i0);
}

#define jit_getarg_us(r0, i0)		arm_getarg_us(_jitp, r0, i0)
__jit_inline void
arm_getarg_us(jit_state_t _jitp, jit_gpr_t r0, int i0)
{
  if (i0 < 4)
    i0 <<= 2;
#if __BYTE_ORDER == __BIG_ENDIAN
  i0 += sizeof(int) - sizeof(short);
#endif
  jit_ldxi_us(r0, JIT_FP, i0);
}

#define jit_getarg_i(r0, i0)		arm_getarg_i(_jitp, r0, i0)
#define jit_getarg_ui(r0, i0)		arm_getarg_i(_jitp, r0, i0)
#define jit_getarg_l(r0, i0)		arm_getarg_i(_jitp, r0, i0)
#define jit_getarg_ul(r0, i0)		arm_getarg_i(_jitp, r0, i0)
#define jit_getarg_p(r0, i0)		arm_getarg_i(_jitp, r0, i0)
__jit_inline void
arm_getarg_i(jit_state_t _jitp, jit_gpr_t r0, int i0)
{
  /* arguments are saved in prolog */
  if (i0 < 4)
    i0 <<= 2;
  jit_ldxi_i(r0, JIT_FP, i0);
}

#define jit_pusharg_i(r0)		arm_pusharg_i(_jitp, r0)
__jit_inline void
arm_pusharg_i(jit_state_t _jitp, jit_gpr_t r0)
{
  int		ofs = _jitl.nextarg_put++;
  jit_stxi_i((ofs <<2) + JIT_ARG_STACK_OFFSET, JIT_FP, r0);
}

#define jit_normal_pushonlyarg_i(r0)  arm_normal_pushonlyarg_i(_jitp, r0)
__jit_inline void
arm_normal_pushonlyarg_i(jit_state_t _jitp, jit_gpr_t r0)
{
  jit_movr_i(JIT_R0, r0);
}

static void
arm_patch_arguments(jit_state_t _jitp)
{
  int ofs = _jitl.nextarg_put;
  int reg = 0;
  _jitl.nextarg_put = 0;
  while (ofs) {
    --ofs;
    jit_ldxi_i(JIT_R(reg), JIT_FP, (ofs <<2) + JIT_ARG_STACK_OFFSET);
    reg++;
  }
}

#define jit_finishr(rs)			arm_finishr(_jitp, rs)
__jit_inline void
arm_finishr(jit_state_t _jitp, jit_gpr_t r0)
{
  if (r0 < 4) {
    jit_movr_i(JIT_TMP, r0);
    r0 = JIT_TMP;
  }
  arm_patch_arguments(_jitp);
  jit_callr(r0);
}

#define jit_finish(i0)			arm_finishi(_jitp, i0)
__jit_inline jit_insn *
arm_finishi(jit_state_t _jitp, void *i0)
{
  arm_patch_arguments(_jitp);
  return (jit_calli(i0));
}

#define jit_normal_finish(i0)		arm_normal_finishi(_jitp, i0)
__jit_inline jit_insn *
arm_normal_finishi(jit_state_t _jitp, void *i0)
{
  return (jit_calli(i0));
}

#define jit_retval_i(r0)		jit_movr_i(r0, JIT_RET)
#define jit_ret()			arm_ret(_jitp)
__jit_inline void
arm_ret(jit_state_t _jitp)
{
  /* do not restore arguments */
  jit_addi_i(JIT_SP, JIT_FP, 16);
  if (jit_thumb_p())
    T2_POP(/* callee save */
           (1<<_R4)|(1<<_R5)|(1<<_R6)|(1<<_R7)|
           /* previous fp and return address */
           (1<<JIT_FP)|(1<<JIT_PC));
  else
    _POP(/* callee save */
         (1<<_R4)|(1<<_R5)|(1<<_R6)|(1<<_R7)|
         /* previous fp and return address */
         (1<<JIT_FP)|(1<<JIT_PC));
  if (jit_thumb_p() && ((int)_jitp->x.pc & 2))
    T1_NOP();
}

# define jit_pushr_i(r0)		arm_pushr_i(_jitp, r0)
__jit_inline void
arm_pushr_i(jit_state_t _jitp, jit_gpr_t r0)
{
  _PUSH(1<<r0);
}

# define jit_popr_i(r0)			arm_popr_i(_jitp, r0)
__jit_inline void
arm_popr_i(jit_state_t _jitp, jit_gpr_t r0)
{
  _POP(1<<r0);
}

#define jit_stixi_l(id, rd, is) (jit_movi_l(JIT_TMP, is), jit_stxi_l(id, rd, JIT_TMP))
#define jit_stixi_p(id, rd, is) jit_stixi_l(id, rd, (long)is) 

#define jit_stir_l(rd, is) (jit_movi_l(JIT_TMP, (intptr_t)(is)), jit_str_l(rd, JIT_TMP))
#define jit_stir_p(rd, is) jit_stir_l(rd, is)

#endif /* __lightning_core_arm_h */
