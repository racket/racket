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

#if !defined(STACK_GROWS_UP) && !defined(STACK_GROWS_DOWN)
# define STACK_GROWS_UNKNOWN
#endif

#ifdef STACK_GROWS_UP
# define STK_COMP(a,b) ((a) > (b))
#else
# ifdef STACK_GROWS_DOWN
#  define STK_COMP(a,b) ((a) < (b))
# else
#  define STK_COMP(a,b) (scheme_stack_grows_up == ((a) > (b)))
# endif
#endif

#ifdef SHORT_ALIGN
typedef short stack_val;
#else
typedef long stack_val;
#endif

extern int scheme_stack_grows_up;

#ifndef STACK_SAFETY_MARGIN
# define STACK_SAFETY_MARGIN 50000
#endif
