/* Sparc fix borrowed from SCM, so here's the copyright:  */
/* Scheme implementation intended for JACAL.
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Aubrey Jaffer. */
/* James Clark came up with this neat one instruction fix for
   continuations on the SPARC.  It flushes the register windows so
   that all the state of the process is contained in the stack. */
#ifdef FLUSH_SPARC_REGISTER_WINDOWS
#define FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
#define FLUSH_REGISTER_WINDOWS /* empty */
#endif

/* If stack is not longword aligned then */

/* #define SHORT_ALIGN */
#ifdef THINK_C
#define SHORT_ALIGN
#endif
#ifdef MSDOS
#define SHORT_ALIGN
#endif
#ifdef atarist
#define SHORT_ALIGN
#endif

/* If stacks grow up then */

 /* STACK_GROWS_UP means that deeper stack values have higher
     numbered addresses.
    STACK_GROWS_DOWN means that deeper stack values have lower
     numbered addresses. This is usually the case (Sparc and
     Intel platforms, for example, use this). */

#if STACK_DIRECTION > 0
# define STACK_GROWS_UP
#else
# define STACK_GROWS_DOWN
#endif

#ifdef STACK_GROWS_UP
# define STK_COMP(a,b) ((a) > (b))
# define STK_DIFF(a, b) ((b) - (a))
#else
# define STK_COMP(a,b) ((a) < (b))
# define STK_DIFF(a, b) ((a) - (b))
#endif

#ifndef STACK_SAFETY_MARGIN
# ifdef SIXTY_FOUR_BIT_INTEGERS
#  define STACK_SAFETY_MARGIN 100000
# else
#  define STACK_SAFETY_MARGIN 50000
# endif
#endif
