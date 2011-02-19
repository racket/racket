/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer inline functions (PowerPC)
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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



#ifndef __lightning_funcs_h
#define __lightning_funcs_h

#ifndef DEFINE_LIGHTNING_FUNCS_STATIC
# define DEFINE_LIGHTNING_FUNCS_STATIC static
#endif

#ifdef SUPPRESS_LIGHTNING_FUNCS
void jit_notify_freed_code(void);
void jit_flush_code(void *start, void *end);
void _jit_prolog(jit_state *jit, int n);
void _jit_epilog(jit_state *jit);
#else

DEFINE_LIGHTNING_FUNCS_STATIC void
jit_notify_freed_code(void)
{
}

#if !defined(__GNUC__) && !defined(__GNUG__)
#error Go get GNU C, I do not know how to flush the cache
#error with this compiler.
#else
DEFINE_LIGHTNING_FUNCS_STATIC void
jit_flush_code(void *start, void *end)
{
#ifndef LIGHTNING_CROSS
  register char *ddest, *idest;

  static int cache_line_size;
  if (cache_line_size == 0) {
    char buffer[8192];
    int i, probe;

    /* Find out the size of a cache line by zeroing one */
    memset(buffer, 0xFF, 8192);
    __asm__ __volatile__ ("dcbz 0,%0" : : "r"(buffer + 4096));

    /* Probe for the beginning of the cache line. */
    for(i = 0, probe = 4096; probe; probe >>= 1)
      if (buffer[i | probe] != 0x00)
        i |= probe;

    /* i is now just before the start of the cache line */
    i++;
    for(cache_line_size = 1; i + cache_line_size < 8192; cache_line_size <<= 1)
      if (buffer[i + cache_line_size] != 0x00)
        break;
  }

  start -= ((long) start) & (cache_line_size - 1);
  end -= ((long) end - 1) & (cache_line_size - 1);

  /* Force data cache write-backs */
  for (ddest = (char *) start; ddest <= (char *) end; ddest += cache_line_size) {
    __asm__ __volatile__ ("dcbst 0,%0" : : "r"(ddest));
  }
  __asm__ __volatile__ ("sync" : : );

  /* Now invalidate the instruction cache */
  for (idest = (char *) start; idest <= (char *) end; idest += cache_line_size) {
    __asm__ __volatile__ ("icbi 0,%0" : : "r"(idest));
  }
  __asm__ __volatile__ ("isync" : : );
#endif /* !LIGHTNING_CROSS */
}
#endif /* __GNUC__ || __GNUG__ */

#define _jit (*jit)

DEFINE_LIGHTNING_FUNCS_STATIC void
_jit_epilog(jit_state *jit)
{
  int n = _jitl.nbArgs;
  int frame_size, ofs;
  int first_saved_reg = JIT_AUX - n;
  int num_saved_regs = 32 - first_saved_reg;

  frame_size = 24 + 32 + 12 + num_saved_regs * 4;	/* r24..r31 + args		   */
  frame_size += 15;			/* the stack must be quad-word     */
  frame_size &= ~15;			/* aligned			   */

#ifdef _CALL_DARWIN
  LWZrm(0, frame_size + 8, 1);	/* lwz   r0, x+8(r1)  (ret.addr.)  */
#else
  LWZrm(0, frame_size + 4, 1);	/* lwz   r0, x+4(r1)  (ret.addr.)  */
#endif
  MTLRr(0);				/* mtspr LR, r0			   */

  ofs = frame_size - num_saved_regs * 4;
  LMWrm(first_saved_reg, ofs, 1);	/* lmw   rI, ofs(r1)		   */
  ADDIrri(1, 1, frame_size);		/* addi  r1, r1, x		   */
  BLR();				/* blr				   */
}

/* Emit a prolog for a function.
   Upon entrance to the trampoline:
     - LR      = address where the real code for the function lies
     - R3-R8   = parameters
   Upon finishing the trampoline:
     - R0      = return address for the function
     - R25-R20 = parameters (order is reversed, 1st argument is R25)
  
   The +32 in frame_size computation is to account for the parameter area of
   a function frame. The +12 is to make room for three local variables (a
   Racket-specific change).

   On PPC the frame must have space to host the arguments of any callee.
   However, as it currently stands, the argument to jit_trampoline (n) is
   the number of arguments of the caller we generate. Therefore, the
   callee can overwrite a part of the stack (saved register area when it
   flushes its own parameter on the stack). The addition of a constant 
   offset = 32 is enough to hold eight 4 bytes arguments.  This is less
   than perfect but is a reasonable work around for now. 
   Better solution must be investigated.  */
DEFINE_LIGHTNING_FUNCS_STATIC void
_jit_prolog(jit_state *jit, int n)
{
  int frame_size;
  int ofs;
  int first_saved_reg = JIT_AUX - n;
  int num_saved_regs = 32 - first_saved_reg;

  _jitl.nextarg_geti = 3;
  _jitl.nextarg_getd = 1;
  _jitl.nbArgs = n;

  frame_size = 24 + 32 + 12 + num_saved_regs * 4;	/* r27..r31 + args		   */
  frame_size += 15;			/* the stack must be quad-word     */
  frame_size &= ~15;			/* aligned			   */

  MFLRr(0);
  STWUrm(1, -frame_size, 1);		/* stwu  r1, -x(r1)		   */

  ofs = frame_size - num_saved_regs * 4;
  STMWrm(first_saved_reg, ofs, 1);		/* stmw  rI, ofs(r1)		   */
#ifdef _CALL_DARWIN
  STWrm(0, frame_size + 8, 1);		/* stw   r0, x+8(r1)		   */
#else
  STWrm(0, frame_size + 4, 1);		/* stw   r0, x+4(r1)		   */
#endif

#if 0
  for (i = 0; i < n; i++)
    MRrr(JIT_AUX-1-i, 3+i);		/* save parameters below r24	   */
#endif
}

#undef _jit

#endif

#endif /* __lightning_funcs_h */
