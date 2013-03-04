#ifndef FP_EXTFPU_H
#define FP_EXTFPU_H
/* We really must map the x87 stack onto a flat register file.  In practice,
   we can provide something sensible and make it work on the x86 using the
   stack like a file of eight registers.

   We use six or seven registers so as to have some freedom
   for floor, ceil, round, (and log, tan, atn and exp).

   Not hard at all, basically play with FXCH.  FXCH is mostly free,
   so the generated code is not bad.  Of course we special case when one
   of the operands turns out to be ST0.

   Here are the macros that actually do the trick.  */

#define JIT_FPU_FPR_NUM	       6
#define JIT_FPU_FPR(i)	       (i)

#define JIT_FPU_FPR0			JIT_FPU_FPR(0)
#define JIT_FPU_FPR1			JIT_FPU_FPR(1)
#define JIT_FPU_FPR2			JIT_FPU_FPR(2)
#define JIT_FPU_FPR3			JIT_FPU_FPR(3)
#define JIT_FPU_FPR4			JIT_FPU_FPR(4)
#define JIT_FPU_FPR5			JIT_FPU_FPR(5)

#define jit_fpu_fxch(rs, op)       (((rs) != 0 ? FXCHr(rs) : (void)0),  \
                                    op, ((rs) != 0 ? FXCHr(rs) : (void)0))

#define jit_fpu_fp_unary(rd, s1, op)            \
  ((rd) == (s1) ? jit_fpu_fxch ((rd), op)       \
   : (rd) == 0 ? (FSTPr (0), FLDr ((s1)-1), op) \
   : (FLDr ((s1)), op, FSTPr ((rd))))

#define jit_fpu_fp_binary(rd, s1, s2, op, opr)                          \
  ((rd) == (s1) ?                                                       \
   ((s2) == 0 ? opr(0, (rd))                                            \
    : (s2) == (s1) ? jit_fpu_fxch((rd), op(0, 0))                       \
    : jit_fpu_fxch((rd), op((s2), 0)))                                  \
   : (rd) == (s2) ? ((s1) == 0 ? op((s1), (s2)) : jit_fpu_fxch((s2), opr((s1), 0))) \
   : (FLDr (s1), op(0, (s2)+1), FSTPr((rd)+1)))

#define jit_fpu_addr_d(rd,s1,s2)    jit_fpu_fp_binary((rd),(s1),(s2),FADDrr,FADDrr)
#define jit_fpu_subr_d(rd,s1,s2)    jit_fpu_fp_binary((rd),(s1),(s2),FSUBrr,FSUBRrr)
#define jit_fpu_subrr_d(rd,s1,s2)   jit_fpu_fp_binary((rd),(s1),(s2),FSUBRrr,FSUBrr)
#define jit_fpu_mulr_d(rd,s1,s2)    jit_fpu_fp_binary((rd),(s1),(s2),FMULrr,FMULrr)
#define jit_fpu_divr_d(rd,s1,s2)    jit_fpu_fp_binary((rd),(s1),(s2),FDIVrr,FDIVRrr)
#define jit_fpu_divrr_d(rd,s1,s2)   jit_fpu_fp_binary((rd),(s1),(s2),FDIVRrr,FDIVrr)

#define jit_fpu_abs_d(rd,rs)       jit_fpu_fp_unary ((rd), (rs), _OO (0xd9e1))
#define jit_fpu_negr_d(rd,rs)      jit_fpu_fp_unary ((rd), (rs), _OO (0xd9e0))
#define jit_fpu_sqrt_d(rd,rs)      jit_fpu_fp_unary ((rd), (rs), _OO (0xd9fa))

#define jit_fpu_addr_d_fppop(rd,s1,s2)  (FADDPr(1))
#define jit_fpu_subr_d_fppop(rd,s1,s2)  (FSUBPr(1))
#define jit_fpu_subrr_d_fppop(rd,s1,s2) (FSUBRPr(1))
#define jit_fpu_mulr_d_fppop(rd,s1,s2)  (FMULPr(1))
#define jit_fpu_divr_d_fppop(rd,s1,s2)  (FDIVPr(1))
#define jit_fpu_divrr_d_fppop(rd,s1,s2) (FDIVRPr(1))
#define jit_fpu_negr_d_fppop(rd,rs)     ( _OO (0xd9e0))
#define jit_fpu_abs_d_fppop(rd,rs)      ( _OO (0xd9e1))
#define jit_fpu_sqrt_d_fppop(rd,rs)     ( _OO (0xd9fa))

/* Except for memory loads and stores, _ld typed operations
   are implemented the same as _d typed operations. Set the
   FPU's precision to actually get one or the other. */

#define jit_fpu_addr_ld(rd,s1,s2) jit_fpu_addr_d(rd,s1,s2)
#define jit_fpu_subr_ld(rd,s1,s2) jit_fpu_subr_d(rd,s1,s2)
#define jit_fpu_subrr_ld(rd,s1,s2) jit_fpu_subrr_d(rd,s1,s2)
#define jit_fpu_mulr_ld(rd,s1,s2) jit_fpu_mulr_d(rd,s1,s2)
#define jit_fpu_ldivr_ld(rd,s1,s2) jit_fpu_ldivr_d(rd,s1,s2)
#define jit_fpu_ldivrr_ld(rd,s1,s2) jit_fpu_ldivrr_d(rd,s1,s2)

#define jit_fpu_abs_ld(rd,rs) jit_fpu_abs_d(rd,rs)
#define jit_fpu_negr_ld(rd,rs) jit_fpu_negr_d(rd,rs)
#define jit_fpu_sqrt_ld(rd,rs) jit_fpu_sqrt_d(rd,rs)

#define jit_fpu_addr_ld_fppop(rd,s1,s2)  jit_fpu_addr_d_fppop(rd,s1,s2)
#define jit_fpu_subr_ld_fppop(rd,s1,s2)  jit_fpu_subr_d_fppop(rd,s1,s2)
#define jit_fpu_subrr_ld_fppop(rd,s1,s2) jit_fpu_subrr_d_fppop(rd,s1,s2)
#define jit_fpu_mulr_ld_fppop(rd,s1,s2)  jit_fpu_mulr_d_fppop(rd,s1,s2)
#define jit_fpu_divr_ld_fppop(rd,s1,s2)  jit_fpu_divr_d_fppop(rd,s1,s2)
#define jit_fpu_divrr_ld_fppop(rd,s1,s2) jit_fpu_divrr_d_fppop(rd,s1,s2)
#define jit_fpu_negr_ld_fppop(rd,rs)     jit_fpu_negr_d_fppop(rd,rs)
#define jit_fpu_abs_ld_fppop(rd,rs)      jit_fpu_abs_d_fppop(rd,rs)
#define jit_fpu_sqrt_ld_fppop(rd,rs)     jit_fpu_sqrt_d_fppop(rd,rs)

/* - moves:

   move FPR0 to FPR3
   FST  ST3

   move FPR3 to FPR0
   FXCH ST3
   FST  ST3

   move FPR3 to FPR1
   FLD  ST1
   FST  ST4   Stack is rotated, so FPRn becomes STn+1 */

#define jit_fpu_movr_d(rd,s1)                   \
  ((s1) == (rd) ? 0                             \
   : (rd) == 0 ? (FSTPr(0), FSTr (((s1)-1)))    \
   : (FLDr ((s1)), FSTPr ((rd)+1)))

#define jit_fpu_movr_d_rel(rd,s1) ((rd < s1) ? (FSTPr(0), FLDr(0)) : (FSTr(1)))
#define jit_fpu_movr_d_fppush(rd,s1) (FLDr(s1))

#define jit_fpu_movr_ld(rd,s1)        jit_fpu_movr_d(rd,s1)
#define jit_fpu_movr_ld_rel(rd,s1)    jit_fpu_movr_d_rel(rd,s1) 
#define jit_fpu_movr_ld_fppush(rd,s1) jit_fpu_movr_d_fppush(rd,s1)

/* - loads:

   load into FPR0
   FSTP ST0
   FLD  [FUBAR]

   load into FPR3
   FSTP ST3     Save old st0 into destination register
   FLD  [FUBAR]
   FXCH ST3     Get back old st0

   (and similarly for immediates, using the stack) */

#define jit_fpu_movi_f(rd,immf)                 \
  (_O (0x68),                                   \
   *((float *) _jit.x.pc) = (float) immf,       \
   _jit.x.uc_pc += sizeof (float),              \
   jit_fpu_ldr_f((rd), _ESP),                   \
   ADDQir(4, _ESP))

union jit_fpu_double_imm {
  double d;
  int i[2];
};

#ifdef JIT_X86_64
# define jit_fpu_double_as_long(v) (*(double *)(_jit.x.uc_pc) = v, *(intptr_t *)(_jit.x.uc_pc))
# define _jit_fpu_push_d(immd)                          \
  (MOVQir(jit_fpu_double_as_long(immd), JIT_REXTMP),    \
   PUSHQr(JIT_REXTMP))
# define FPX() (void)0 /* don't need _REX(0,0,0), apparently */
#else
# define _jit_fpu_push_d(immd)                                          \
  (_O (0x68),                                                           \
   _jit.x.uc_pc[4] = 0x68,                                              \
   ((union jit_fpu_double_imm *) (_jit.x.uc_pc + 5))->d = (double) immd,    \
   *((int *) _jit.x.uc_pc) = ((union jit_fpu_double_imm *) (_jit.x.uc_pc + 5))->i[1], \
   _jit.x.uc_pc += 9)
# define FPX() ((void) 0)
#endif

#define jit_fpu_movi_d(rd,immd)                 \
  (_jit_fpu_push_d(immd),                       \
   jit_fpu_ldr_d((rd), _ESP),                   \
   ADDQir(8, _ESP))

#ifdef JIT_X86_64
#define jit_fpu_ldi_d_fppush(rd, is)            \
  (MOVQrr(JIT_R0, JIT_REXTMP),                  \
   MOVQir(((intptr_t)is), JIT_R0),              \
   jit_fpu_ldr_d_fppush(rd, JIT_R0),            \
   MOVQrr(JIT_REXTMP, JIT_R0))

#define jit_fpu_ldi_ld_fppush(rd, is)           \
  (MOVQrr(JIT_R0, JIT_REXTMP),                  \
   MOVQir(((intptr_t)is), JIT_R0),              \
   jit_fpu_ldr_ld_fppush(rd, JIT_R0),           \
   MOVQrr(JIT_REXTMP, JIT_R0))
#else
#define jit_fpu_ldi_f(rd, is)                           \
  ((rd) == 0 ? (FSTPr (0), FLDSm((is), 0, 0, 0))        \
   : (FLDSm((is), 0, 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldi_d(rd, is)                           \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDLm((is), 0, 0, 0)) \
   : (FPX(), FLDLm((is), 0, 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldi_d_fppush(rd, is) (FPX(), FLDLm((is), 0, 0, 0))

#define jit_fpu_ldi_ld(rd, is)                          \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDTm((is), 0, 0, 0)) \
     : (FPX(), FLDTm((is), 0, 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldi_ld_fppush(rd, is) (FPX(), FLDTm((is), 0, 0, 0))
#endif

#define jit_fpu_ldr_f(rd, rs)                           \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDSm(0, (rs), 0, 0)) \
   : (FPX(), FLDSm(0, (rs), 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldr_d(rd, rs)                           \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDLm(0, (rs), 0, 0)) \
   : (FPX(), FLDLm(0, (rs), 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldr_d_fppush(rd, rs) (FPX(), FLDLm(0, (rs), 0, 0))

#define jit_fpu_ldr_ld(rd, rs)                          \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDTm(0, (rs), 0, 0)) \
   : (FPX(), FLDTm(0, (rs), 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldr_ld_fppush(rd, rs) (FPX(), FLDTm(0, (rs), 0, 0))

#define jit_fpu_ldxi_f(rd, rs, is)                              \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDSm((is), (rs), 0, 0))	\
   : (FPX(), FLDSm((is), (rs), 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldxi_d(rd, rs, is)                              \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDLm((is), (rs), 0, 0))	\
   : (FPX(), FLDLm((is), (rs), 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldxi_d_fppush(rd, rs, is) (FPX(), FLDLm((is), (rs), 0, 0))

#define jit_fpu_ldxi_ld(rd, rs, is)                             \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDTm((is), (rs), 0, 0))	\
   : (FPX(), FLDTm((is), (rs), 0, 0), FSTPr ((rd) + 1)))

#define jit_fpu_ldxi_ld_fppush(rd, rs, is) (FPX(), FLDTm((is), (rs), 0, 0))

#define jit_fpu_ldxr_f(rd, s1, s2)                              \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDSm(0, (s1), (s2), 1))	\
   : (FPX(), FLDSm(0, (s1), (s2), 1), FSTPr ((rd) + 1)))

#define jit_fpu_ldxr_d(rd, s1, s2)                              \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDLm(0, (s1), (s2), 1))	\
   : (FPX(), FLDLm(0, (s1), (s2), 1), FSTPr ((rd) + 1)))

#define jit_fpu_ldxr_d_fppush(rd, s1, s2) (FPX(), FLDLm(0, (s1), (s2), 1))

#define jit_fpu_ldxr_ld(rd, s1, s2)                             \
  ((rd) == 0 ? (FSTPr (0), FPX(), FLDTm(0, (s1), (s2), 1))	\
     : (FPX(), FLDTm(0, (s1), (s2), 1), FSTPr ((rd) + 1)))

#define jit_fpu_ldxr_ld_fppush(rd, s1, s2) (FPX(), FLDTm(0, (s1), (s2), 1))

#define jit_fpu_extr_i_d(rd, rs)   (PUSHLr((rs)),                       \
                                    ((rd) == 0 ? (FSTPr (0), FILDLm(0, _ESP, 0, 0)) \
                                     : (FILDLm(0, _ESP, 0, 0), FSTPr ((rd) + 1))), \
                                    POPLr((rs)))

#define jit_fpu_extr_i_d_fppush(rd, rs)                 \
  (PUSHLr((rs)), FILDLm(0, _ESP, 0, 0), POPLr((rs)))
#define jit_fpu_extr_i_ld_fppush(rd, rs)                 \
  (PUSHLr((rs)), FILDLm(0, _ESP, 0, 0), POPLr((rs)))
  #ifdef JIT_X86_64
# define jit_fpu_extr_l_d_fppush(rd, rs)                \
  (PUSHQr((rs)), FILDQm(0, _ESP, 0, 0), POPQr((rs)))

# define jit_fpu_extr_l_ld_fppush(rd, rs)                \
  (PUSHQr((rs)), FILDQm(0, _ESP, 0, 0), POPQr((rs)))
#else
# define jit_fpu_extr_l_d_fppush(rd, rs) jit_fpu_extr_i_d_fppush(rd, rs)

# define jit_fpu_extr_l_ld_fppush(rd, rs) jit_fpu_extr_i_ld_fppush(rd, rs)
#endif


#define jit_fpu_stxi_f(id, rd, rs) jit_fpu_fxch ((rs), FPX(), FSTSm((id), (rd), 0, 0))
#define jit_fpu_stxr_f(d1, d2, rs) jit_fpu_fxch ((rs), FPX(), FSTSm(0, (d1), (d2), 1))
#define jit_fpu_stxi_d(id, rd, rs) jit_fpu_fxch ((rs), FPX(), FSTLm((id), (rd), 0, 0))
#define jit_fpu_stxr_d(d1, d2, rs) jit_fpu_fxch ((rs), FPX(), FSTLm(0, (d1), (d2), 1))

#ifdef JIT_X86_64
#define jit_fpu_sti_d_fppop(is, rd)             \
  (MOVQrr(JIT_R0, JIT_REXTMP),                  \
   MOVQir(((intptr_t)is), JIT_R0),              \
   jit_fpu_str_d_fppop(JIT_R0, rd),             \
   MOVQrr(JIT_REXTMP, JIT_R0))

#define jit_fpu_sti_ld_fppop(is, rd)            \
  (MOVQrr(JIT_R0, JIT_REXTMP),                  \
   MOVQir(((intptr_t)is), JIT_R0),              \
   jit_fpu_str_ld_fppop(JIT_R0, rd),            \
   MOVQrr(JIT_REXTMP, JIT_R0))

#else
#define jit_fpu_sti_f(id, rs)      jit_fpu_fxch ((rs), FPX(), FSTSm((id), 0,    0, 0))
#define jit_fpu_str_f(rd, rs)      jit_fpu_fxch ((rs), FPX(), FSTSm(0,    (rd), 0, 0))
#define jit_fpu_sti_d(id, rs)      jit_fpu_fxch ((rs), FPX(), FSTLm((id), 0,    0, 0))
#define jit_fpu_str_d(rd, rs)      jit_fpu_fxch ((rs), FPX(), FSTLm(0,    (rd), 0, 0))

#define jit_fpu_sti_d_fppop(id, rs)   (FPX(), FSTPLm((id), 0,    0, 0))

#define jit_fpu_sti_ld_fppop(id, rs)  (FPX(), FSTPTm((id), 0,    0, 0))
#endif

#define jit_fpu_stxi_d_fppop(id, rd, rs) (FPX(), FSTPLm((id), (rd), 0, 0))
#define jit_fpu_str_d_fppop(rd, rs)      (FPX(), FSTPLm(0,    (rd), 0, 0))
#define jit_fpu_stxr_d_fppop(d1, d2, rs) (FPX(), FSTPLm(0,    (d1), (d2), 1))

#define jit_fpu_stxi_ld_fppop(id, rd, rs) (FPX(), FSTPTm((id), (rd), 0, 0))
#define jit_fpu_str_ld_fppop(rd, rs)      (FPX(), FSTPTm(0,    (rd), 0, 0))
#define jit_fpu_stxr_ld_fppop(d1, d2, rs) (FPX(), FSTPTm(0,    (d1), (d2), 1))

/* Assume round to near mode */
#define jit_fpu_floorr_d_i(rd, rs)                              \
  (FLDr (rs), jit_fpu_floor2((rd), ((rd) == _EDX ? _EAX : _EDX)))

#define jit_fpu_ceilr_d_i(rd, rs)                               \
  (FLDr (rs), jit_fpu_ceil2((rd), ((rd) == _EDX ? _EAX : _EDX)))

#define jit_fpu_truncr_d_i(rd, rs)                              \
  (FLDr (rs), jit_fpu_trunc2((rd), ((rd) == _EDX ? _EAX : _EDX)))

#define jit_fpu_calc_diff(ofs)                  \
  FISTLm(ofs, _ESP, 0, 0),                      \
    FILDLm(ofs, _ESP, 0, 0),                    \
    FSUBRPr(1),                                 \
    FSTPSm(4+ofs, _ESP, 0, 0)                   \

/* The real meat */
#define jit_fpu_floor2(rd, aux)                                         \
  (PUSHLr(aux),                                                         \
   SUBLir(8, _ESP),                                                     \
   jit_fpu_calc_diff(0),                                                \
   POPLr(rd),			/* floor in rd */                       \
   POPLr(aux),			/* x-round(x) in aux */                 \
   ADDLir(0x7FFFFFFF, aux),	/* carry if x-round(x) < -0 */          \
   SBBLir(0, rd),			/* subtract 1 if carry */       \
   POPLr(aux))

#define jit_fpu_ceil2(rd, aux)                          \
  (PUSHLr(aux),                                         \
   SUBLir(8, _ESP),                                     \
   jit_fpu_calc_diff(0),                                \
   POPLr(rd),			/* floor in rd */       \
   POPLr(aux),			/* x-round(x) in aux */ \
   TESTLrr(aux, aux),                                   \
   SETGr(jit_reg8(aux)),                                \
   SHRLir(1, aux),                                      \
   ADCLir(0, rd),                                       \
   POPLr(aux))

/* a mingling of the two above */
#define jit_fpu_trunc2(rd, aux)			\
  (PUSHLr(aux),                                 \
   SUBLir(12, _ESP),                            \
   FSTSm(0, _ESP, 0, 0),			\
   jit_fpu_calc_diff(4),			\
   POPLr(aux),                                  \
   POPLr(rd),                                   \
   TESTLrr(aux, aux),                           \
   POPLr(aux),                                  \
   JSSm(_jit.x.pc + 11, 0, 0, 0),		\
   ADDLir(0x7FFFFFFF, aux),	/* 6 */         \
   SBBLir(0, rd),			/* 3 */ \
   JMPSm(_jit.x.pc + 10, 0, 0, 0),	/* 2 */ \
   TESTLrr(aux, aux),		/* 2 */         \
   SETGr(jit_reg8(aux)),		/* 3 */ \
   SHRLir(1, aux),			/* 2 */ \
   ADCLir(0, rd),			/* 3 */ \
   POPLr(aux))

/* the easy one */
#define jit_fpu_roundr_d_i(rd, rs)              \
  (PUSHLr(_EAX),                                \
   jit_fpu_fxch ((rs), FISTPLm(0, _ESP, 0, 0)), \
   POPLr((rd)))

#define jit_fpu_roundr_ld_i(rd, rs) jit_fpu_roundr_d_i(rd, rs)

#define jit_fpu_roundr_d_l(rd, rs)              \
  (PUSHQr(_EAX),                                \
   jit_fpu_fxch ((rs), FISTPQm(0, _ESP, 0, 0)), \
   POPQr((rd)))

#define jit_fpu_roundr_ld_l(rd, rs) jit_fpu_roundr_d_l(rd, rs)

#define jit_fpu_roundr_d_l_fppop(rd, rs)        \
  (PUSHQr(_EAX),                                \
   FISTPQm(0, _ESP, 0, 0),                      \
   POPQr((rd)))

#define jit_fpu_roundr_ld_l_fppop(rd, rs) jit_fpu_roundr_d_l_fppop(rd, rs)

#define jit_fpu_fp_test(d, s1, s2, n, _and, res)                        \
  (((s1) == 0 ? FUCOMr((s2)) : (FLDr((s1)), FUCOMPr((s2) + 1))),        \
   ((d) != _EAX ? MOVLrr(_EAX, (d)) : 0),                               \
   FNSTSWr(_EAX),                                                       \
   SHRLir(n, _EAX),                                                     \
   ((_and) ? ANDLir((_and), _EAX) : MOVLir(0, _EAX)),                   \
   res,                                                                 \
   ((d) != _EAX ? _O (0x90 + ((d) & 7)) : 0))     /* xchg */

#define jit_fpu_fp_btest(d, s1, s2, n, _and, cmp, res)          \
  (((s1) == 0 ? FCOMr((s2)) : (FLDr((s1)), FUCOMPr((s2) + 1))), \
   (_jitl.r0_can_be_tmp ? 0 : PUSHQr(_EAX)),                    \
   FNSTSWr(_EAX),                                               \
   SHRLir(n, _EAX),                                             \
   (void)((_and) ? ANDLir ((_and), _EAX) : 0),                  \
   ((cmp) ? CMPLir ((cmp), _AL) : 0),                           \
   (void) (_jitl.r0_can_be_tmp ? 0 : POPQr(_EAX)),              \
   res ((d), 0, 0, 0), _jit.x.pc)

#define jit_fpu_fp_test_fppop(d, n, _and, res)                  \
  (FUCOMPPr(1),                                                 \
   ((d) != _EAX ? MOVLrr(_EAX, (d)) : 0),                       \
   FNSTSWr(_EAX),                                               \
   SHRLir(n, _EAX),                                             \
   ((_and) ? ANDLir((_and), _EAX) : MOVLir(0, _EAX)),           \
   res,                                                         \
   ((d) != _EAX ? _O (0x90 + ((d) & 7)) : 0))     /* xchg */

#define jit_fpu_fp_btest_fppop(d, n, _and, cmp, res)    \
  (FUCOMPPr(1),                                         \
   (_jitl.r0_can_be_tmp ? 0 : PUSHQr(_EAX)),            \
   FNSTSWr(_EAX),                                       \
   SHRLir(n, _EAX),                                     \
   (void)((_and) ? ANDLir ((_and), _EAX) : 0),          \
   (void)((cmp) ? CMPLir ((cmp), _AL) : 0),             \
   (void) (_jitl.r0_can_be_tmp ? 0 : POPQr(_EAX)),      \
   res ((d), 0, 0, 0), _jit.x.pc)

#define jit_fpu_fp_btest_fppop(d, n, _and, cmp, res)    \
  (FUCOMPPr(1),                                         \
   (_jitl.r0_can_be_tmp ? 0 : PUSHQr(_EAX)),            \
   FNSTSWr(_EAX),                                       \
   SHRLir(n, _EAX),                                     \
   (void)((_and) ? ANDLir ((_and), _EAX) : 0),          \
   (void)((cmp) ? CMPLir ((cmp), _AL) : 0),             \
   (void) (_jitl.r0_can_be_tmp ? 0 : POPQr(_EAX)),      \
   res ((d), 0, 0, 0), _jit.x.pc)

#define jit_fpu_fp_btest_fppop_2(d, res)                        \
  (FUCOMIPr(1), FSTPr(0), res ((d), 0, 0, 0), _jit.x.pc)

#define jit_fpu_nothing_needed(x)

/* After FNSTSW we have 1 if <, 40 if =, 0 if >, 45 if unordered.  Here
   is how to map the values of the status word's high byte to the
   conditions.

   <     =     >     unord    valid values    condition
   gt     no    no    yes   no       0               STSW & 45 == 0
   lt     yes   no    no    no       1               STSW & 45 == 1
   eq     no    yes   no    no       40              STSW & 45 == 40
   unord  no    no    no    yes      45              bit 2 == 1

   ge     no    yes   no    no       0, 40           bit 0 == 0
   unlt   yes   no    no    yes      1, 45           bit 0 == 1
   ltgt   yes   no    yes   no       0, 1            bit 6 == 0
   uneq   no    yes   no    yes      40, 45          bit 6 == 1
   le     yes   yes   no    no       1, 40           odd parity for STSW & 41
   ungt   no    no    yes   yes      0, 45           even parity for STSW & 41

   unle   yes   yes   no    yes      1, 40, 45       STSW & 45 != 0
   unge   no    yes   yes   yes      0, 40, 45       STSW & 45 != 1
   ne     yes   no    yes   yes      0, 1, 45        STSW & 45 != 40
   ord    yes   yes   yes   no       0, 1, 40        bit 2 == 0

   lt, le, ungt, unge are actually computed as gt, ge, unlt, unle with
   the operands swapped; it is more efficient this way.  */

#define jit_fpu_gtr_d(d, s1, s2)            jit_fpu_fp_test((d), (s1), (s2), 8, 0x45, SETZr (_AL))
#define jit_fpu_ger_d(d, s1, s2)            jit_fpu_fp_test((d), (s1), (s2), 9, 0, SBBBir (-1, _AL))
#define jit_fpu_unler_d(d, s1, s2)          jit_fpu_fp_test((d), (s1), (s2), 8, 0x45, SETNZr (_AL))
#define jit_fpu_unltr_d(d, s1, s2)          jit_fpu_fp_test((d), (s1), (s2), 9, 0, ADCBir (0, _AL))
#define jit_fpu_ltr_d(d, s1, s2)            jit_fpu_fp_test((d), (s2), (s1), 8, 0x45, SETZr (_AL))
#define jit_fpu_ler_d(d, s1, s2)            jit_fpu_fp_test((d), (s2), (s1), 9, 0, SBBBir (-1, _AL))
#define jit_fpu_unger_d(d, s1, s2)          jit_fpu_fp_test((d), (s2), (s1), 8, 0x45, SETNZr (_AL))
#define jit_fpu_ungtr_d(d, s1, s2)          jit_fpu_fp_test((d), (s2), (s1), 9, 0, ADCBir (0, _AL))
#define jit_fpu_eqr_d(d, s1, s2)            jit_fpu_fp_test((d), (s1), (s2), 8, 0x45, (CMPBir (0x40, _AL), SETEr (_AL)))
#define jit_fpu_ner_d(d, s1, s2)            jit_fpu_fp_test((d), (s1), (s2), 8, 0x45, (CMPBir (0x40, _AL), SETNEr (_AL)))
#define jit_fpu_ltgtr_d(d, s1, s2)          jit_fpu_fp_test((d), (s1), (s2), 15, 0, SBBBir (-1, _AL))
#define jit_fpu_uneqr_d(d, s1, s2)          jit_fpu_fp_test((d), (s1), (s2), 15, 0, ADCBir (0, _AL))
#define jit_fpu_ordr_d(d, s1, s2)           jit_fpu_fp_test((d), (s1), (s2), 11, 0, SBBBir (-1, _AL))
#define jit_fpu_unordr_d(d, s1, s2)         jit_fpu_fp_test((d), (s1), (s2), 11, 0, ADCBir (0, _AL))
#define jit_fpu_bgtr_d(d, s1, s2)           jit_fpu_fp_btest((d), (s1), (s2), 8, 0x45, 0, JZm)
#define jit_fpu_bger_d(d, s1, s2)           jit_fpu_fp_btest((d), (s1), (s2), 9, 0, 0, JNCm)
#define jit_fpu_bger_ld(d, s1, s2) jit_fpu_bger_d(d, s1, s2)
#define jit_fpu_bantigtr_d(d, s1, s2)       jit_fpu_fp_btest((d), (s1), (s2), 8, 0x45, 0, JNZm)
#define jit_fpu_bantiger_d(d, s1, s2)       jit_fpu_fp_btest((d), (s1), (s2), 9, 0, 0, JCm)
#define jit_fpu_bunler_d(d, s1, s2)         jit_fpu_fp_btest((d), (s1), (s2), 8, 0x45, 0, JNZm)
#define jit_fpu_bunltr_d(d, s1, s2)         jit_fpu_fp_btest((d), (s1), (s2), 9, 0, 0, JCm)
#define jit_fpu_bltr_d(d, s1, s2)           jit_fpu_fp_btest((d), (s2), (s1), 8, 0x45, 0, JZm)

#define jit_fpu_bltr_ld(d, s1, s2) jit_fpu_bltr_d(d, s1, s2)

#define jit_fpu_bler_d(d, s1, s2)           jit_fpu_fp_btest((d), (s2), (s1), 9, 0, 0, JNCm)
#define jit_fpu_bantiltr_d(d, s1, s2)       jit_fpu_fp_btest((d), (s2), (s1), 8, 0x45, 0, JNZm)
#define jit_fpu_bantiler_d(d, s1, s2)       jit_fpu_fp_btest((d), (s2), (s1), 9, 0, 0, JCm)
#define jit_fpu_bunger_d(d, s1, s2)         jit_fpu_fp_btest((d), (s2), (s1), 8, 0x45, 0, JNZm)
#define jit_fpu_bungtr_d(d, s1, s2)         jit_fpu_fp_btest((d), (s2), (s1), 9, 0, 0, JCm)
#define jit_fpu_beqr_d(d, s1, s2)           jit_fpu_fp_btest((d), (s1), (s2), 8, 0x45, 0x40, JZm)

#define jit_fpu_beqr_ld(d, s1, s2) jit_fpu_beqr_d(d, s1, s2)

#define jit_fpu_bantieqr_d(d, s1, s2)       jit_fpu_fp_btest((d), (s1), (s2), 8, 0x45, 0x40, JNZm)

#define jit_fpu_bantieqr_ld(d, s1, s2) jit_fpu_bantieqr_d(d, s1, s2)

#define jit_fpu_bner_d(d, s1, s2)           jit_fpu_fp_btest((d), (s1), (s2), 8, 0x45, 0x40, JNZm)
#define jit_fpu_bltgtr_d(d, s1, s2)         jit_fpu_fp_btest((d), (s1), (s2), 15, 0, 0, JNCm)
#define jit_fpu_buneqr_d(d, s1, s2)         jit_fpu_fp_btest((d), (s1), (s2), 15, 0, 0, JCm)
#define jit_fpu_bordr_d(d, s1, s2)          jit_fpu_fp_btest((d), (s1), (s2), 11, 0, 0, JNCm)
#define jit_fpu_bunordr_d(d, s1, s2)        jit_fpu_fp_btest((d), (s1), (s2), 11, 0, 0, JCm)

#define jit_fpu_bger_d_fppop(d, s1, s2)       jit_fpu_fp_btest_fppop((d), 9, 0, 0, JNCm)
#define jit_fpu_bger_ld_fppop(d, s1, s2)      jit_fpu_bger_d_fppop(d, s1, s2)
/* #define jit_fpu_bantiger_d_fppop(d, s1, s2)   jit_fpu_fp_btest_fppop((d), 9, 0, 0, JCm) */
#define jit_fpu_bantiger_d_fppop(d, s1, s2)   jit_fpu_fp_btest_fppop_2((d), JBm)
#define jit_fpu_bantiger_ld_fppop(d, s1, s2)  jit_fpu_bantiger_d_fppop(d, s1, s2)
#define jit_fpu_bler_d_fppop(d, s1, s2)       (FXCHr(1), jit_fpu_bger_d_fppop(d, s1, s2))
#define jit_fpu_bler_ld_fppop(d, s1, s2)      jit_fpu_bler_d_fppop(d, s1, s2)
#define jit_fpu_bantiler_d_fppop(d, s1, s2)   (FXCHr(1), jit_fpu_bantiger_d_fppop(d, s1, s2))
#define jit_fpu_bantiler_ld_fppop(d, s1, s2)  jit_fpu_bantiler_d_fppop(d, s1, s2)

#define jit_fpu_bgtr_d_fppop(d, s1, s2)       jit_fpu_fp_btest_fppop((d), 8, 0x45, 0, JZm)
#define jit_fpu_bgtr_ld_fppop(d, s1, s2)      jit_fpu_bgtr_d_fppop(d, s1, s2)
/* #define jit_fpu_bantigtr_d_fppop(d, s1, s2)   jit_fpu_fp_btest_fppop((d), 8, 0x45, 0, JNZm) */
#define jit_fpu_bantigtr_d_fppop(d, s1, s2)   jit_fpu_fp_btest_fppop_2((d), JBEm)
#define jit_fpu_bantigtr_ld_fppop(d, s1, s2)  jit_fpu_bantigtr_d_fppop(d, s1, s2)

#define jit_fpu_bltr_d_fppop(d, s1, s2)       (FXCHr(1), jit_fpu_bgtr_d_fppop(d, s1, s2))
#define jit_fpu_bltr_ld_fppop(d, s1, s2)      jit_fpu_bltr_d_fppop(d, s1, s2)

#define jit_fpu_bantiltr_d_fppop(d, s1, s2)   (FXCHr(1), jit_fpu_bantigtr_d_fppop(d, s1, s2))
#define jit_fpu_bantiltr_ld_fppop(d, s1, s2)  jit_fpu_bantiltr_d_fppop(d, s1, s2)

#define jit_fpu_beqr_d_fppop(d, s1, s2)       jit_fpu_fp_btest_fppop((d), 8, 0x45, 0x40, JZm)
#define jit_fpu_beqr_ld_fppop(d, s1, s2)      jit_fpu_beqr_d_fppop(d, s1, s2)
#define jit_fpu_bantieqr_d_fppop(d, s1, s2)   jit_fpu_fp_btest_fppop((d), 8, 0x45, 0x40, JNZm)
#define jit_fpu_bantieqr_ld_fppop(d, s1, s2)  jit_fpu_bantieqr_d_fppop(d, s1, s2)
/* Doesn't work right with +nan.0: */
/* #define jit_bantieqr_d_fppop(d, s1, s2)   jit_fp_btest_fppop_2((d), JNZm) */

#define jit_fpu_getarg_f(rd, ofs)        jit_fpu_ldxi_f((rd), JIT_FP,(ofs))
#define jit_fpu_getarg_d(rd, ofs)        jit_fpu_ldxi_d((rd), JIT_FP,(ofs))
#define jit_fpu_pusharg_d(rs)            (jit_fpu_subi_i(JIT_SP,JIT_SP,sizeof(double)), jit_fpu_str_d(JIT_SP,(rs)))
#define jit_fpu_pusharg_f(rs)            (jit_fpu_subi_i(JIT_SP,JIT_SP,sizeof(float)), jit_fpu_str_f(JIT_SP,(rs)))
#define jit_fpu_retval_d(op1)            jit_fpu_movr_d(0, (op1))

#define jit_set_fp_control(v) (PUSHWi(v), FLDCWm(0, JIT_SP, 0, 0), jit_addi_p(JIT_SP,JIT_SP,2))

#if 0
#define jit_sin()	_OO(0xd9fe)			/* fsin */
#define jit_cos()	_OO(0xd9ff)			/* fcos */
#define jit_tan()	(_OO(0xd9f2), 			/* fptan */ \
			 FSTPr(0))			/* fstp st */
#define jit_atn()	(_OO(0xd9e8), 			/* fld1 */ \
			 _OO(0xd9f3))			/* fpatan */
#define jit_exp()	(_OO(0xd9ea), 			/* fldl2e */ \
			 FMULPr(1), 			/* fmulp */ \
			 _OO(0xd9c0),			/* fld st */ \
			 _OO(0xd9fc),		 	/* frndint */ \
			 _OO(0xdce9), 			/* fsubr */ \
			 FXCHr(1), 			/* fxch st(1) */ \
			 _OO(0xd9f0), 			/* f2xm1 */ \
			 _OO(0xd9e8), 			/* fld1 */ \
			 _OO(0xdec1), 			/* faddp */ \
			 _OO(0xd9fd), 			/* fscale */ \
			 FSTPr(1))			/* fstp st(1) */
#define jit_log()	(_OO(0xd9ed), 			/* fldln2 */ \
			 FXCHr(1), 			/* fxch st(1) */ \
			 _OO(0xd9f1))			/* fyl2x */
#endif

#endif
