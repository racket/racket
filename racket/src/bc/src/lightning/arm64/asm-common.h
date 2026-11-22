/******************************** -*- C -*- ****************************
 *
 *	Dynamic assembler support (AArch64 version)
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002, 2011, 2013-2023, 2026
 *   Free Software Foundation, Inc.
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
 * Authors:
 *	Paolo Bonzini (original lightning 1.x common layer)
 *	Paulo Cesar Pereira de Andrade (ARM and AArch64 ports)
 *
 * This AArch64 variant for Racket BC follows the structure of the
 * lightning 1.x-style ARM port in "../arm", and its instruction
 * encodings are derived from the AArch64 port in GNU lightning 2.x
 * ("lib/jit_aarch64-cpu.c" and "lib/jit_aarch64-fpu.c").
 ***********************************************************************/


#ifndef __lightning_asm_common_h_
#define __lightning_asm_common_h_

#if 1
# include <assert.h>
# define jit_assert(x) assert(x)
#else
# define jit_assert(x) /* empty */
#endif

#include <stdint.h>

#ifndef __WORDSIZE
# define __WORDSIZE 64
#endif

#define __jit_constructor /* empty */
#define __jit_inline XFORM_NONGCING MZ_INLINE static

typedef signed char		_sc;
typedef unsigned char		_uc, jit_insn;
typedef unsigned short		_us;
typedef unsigned int		_ui;
/* Use intptr_t, since `long` is 32 bits on Windows: */
typedef intptr_t		_sl;
typedef uintptr_t		_ul;
typedef struct jit_local_state	jit_local_state;

typedef struct jit_state jit_state;

struct jit_local_state {
  int		 long_jumps;
  int		 nextarg_get;	/* counts jit_arg_*() */
  int		 nextarg_put;	/* counts jit_pusharg_*() */
  jit_insn	*block_start;	/* set by jit_set_ip() */
};

typedef struct jit_state {
  union {
    jit_insn	*pc;
    _uc		*uc_pc;
    _us		*us_pc;
    _ui		*ui_pc;
    _ul		*ul_pc;
  } x;
  struct jit_fp	*fp;
  jit_local_state	 jitl;
} jit_state_t[1];

#ifndef _ASM_SAFETY
#define JITFAIL(MSG) 0
#else
#if (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L) || (defined __GNUC__ && (__GNUC__ == 3 ? __GNUC_MINOR__ >= 2 : __GNUC__ > 3))
#define JITFAIL(MSG) jit_fail(MSG, __FILE__, __LINE__, __func__)
#else
#define JITFAIL(MSG) jit_fail(MSG, __FILE__, __LINE__, "(unknown)")
#endif
#endif

#ifdef __GNUC__
#define JIT_UNUSED		__attribute__((__unused__))
#else
#define JIT_UNUSED
#endif

static int jit_fail(const char *, const char*, int, const char *) JIT_UNUSED;

#define _jit_UC(X)	((_uc  )(X))
#define _jit_US(X)	((_us  )(X))
#define _jit_UI(X)	((_ui  )(X))
#define _jit_SI(X)	((int  )(X))
#define _jit_SL(X)	((_sl  )(X))
#define _jit_UL(X)	((_ul  )(X))
# define _PUC(X)	((_uc *)(X))
# define _PUS(X)	((_us *)(X))
# define _PUI(X)	((_ui *)(X))
# define _PSI(X)	((int *)(X))
# define _PSL(X)	((_sl *)(X))
# define _PUL(X)	((_ul *)(X))

/* All AArch64 instructions are 32 bits: */
#define _jit_I(I)         ((void)((*_jitp->x.ui_pc++)= _jit_UI((I))))

#define _MASK(N)	((uint64_t)(((uint64_t)1<<(N)))-1)
#define _siP(N,I)	(!((((uint64_t)(I))^(((uint64_t)(I))<<1))&~_MASK(N)))
#define _uiP(N,I)	(!(((uint64_t)(I))&~_MASK(N)))

#define _s9P(I)		_siP(9,I)
#define _s19P(I)	_siP(19,I)
#define _s26P(I)	_siP(26,I)
#define _s32P(I)	_siP(32,I)
#define _u12P(I)	_uiP(12,I)
#define _u16P(I)	_uiP(16,I)
#define _u32P(I)	_uiP(32,I)

#endif /* __lightning_asm_common_h */
