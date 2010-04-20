/* 
 * Copyright (c) 2007 by NEC LE-IT:		  All rights reserved.
 * A transcription of ARMv6 atomic operations for the ARM Realview Toolchain.
 * This code works with armcc from RVDS 3.1
 * This is based on work in gcc/arm.h by
 *   Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 *   Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 *   Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 * 
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */
#include "../read_ordered.h"
#include "../test_and_set_t_is_ao_t.h" /* Probably suboptimal */

#if __TARGET_ARCH_ARM < 6
Dont use with ARM instruction sets lower than v6
#endif

/* NEC LE-IT: ARMv6 is the first architecture providing support for simple LL/SC
 * A data memory barrier must be raised via CP15 command (see documentation).	
 * 											
 * ARMv7 is compatible to ARMv6 but has a simpler command for issuing a 		
 * memory barrier (DMB). Raising it via CP15 should still work as told me by the
 * support engineers. If it turns out to be much quicker than we should implement
 * custom code for ARMv7 using the asm { dmb } command.					
 *
 * If only a single processor is used, we can define AO_UNIPROCESSOR
 * and do not need to access CP15 for ensuring a DMB at all.
*/

AO_INLINE void
AO_nop_full()
{
# ifndef AO_UNIPROCESSOR
    unsigned int dest=0;
    /* issue an data memory barrier (keeps ordering of memory transactions	*/
    /* before and after this operation)						*/
	__asm { mcr p15,0,dest,c7,c10,5 } ;
# endif
}

#define AO_HAVE_nop_full

AO_INLINE AO_t
AO_load(volatile AO_t *addr)
{
  /* Cast away the volatile in case it adds fence semantics.		*/
  return (*(AO_t *)addr);
}
#define AO_HAVE_load

/* NEC LE-IT: atomic "store" - according to ARM documentation this is
 * the only safe way to set variables also used in LL/SC environment.
 * A direct write won't be recognized by the LL/SC construct in other CPUs.
 *
 * HB: Based on subsequent discussion, I think it would be OK to use an
 * ordinary store here if we knew that interrupt handlers always cleared
 * the reservation.  They should, but there is some doubt that this is
 * currently always the case for e.g. Linux.
*/
AO_INLINE void AO_store(volatile AO_t *addr, AO_t value)
{
	unsigned long tmp;
	
retry:
__asm {	
		ldrex	tmp, [addr]
		strex	tmp, value, [addr]
		teq	tmp, #0
		bne	retry
	  };
}
#define AO_HAVE_store

/* NEC LE-IT: replace the SWAP as recommended by ARM:

   "Applies to: ARM11 Cores
	Though the SWP instruction will still work with ARM V6 cores, it is recommended
	to use the new V6 synchronization instructions. The SWP instruction produces
	locked read and write accesses which are atomic, i.e. another operation cannot
	be done between these locked accesses which ties up external bus (AHB,AXI)
	bandwidth and can increase worst case interrupt latencies. LDREX,STREX are
	more flexible, other instructions can be done between the LDREX and STREX accesses. 
   "
*/
AO_INLINE AO_TS_t
AO_test_and_set(volatile AO_TS_t *addr) {
	
	AO_TS_t oldval;
	unsigned long tmp;
	unsigned long one = 1;
retry:
__asm {	
		ldrex	oldval, [addr]
		strex	tmp, one, [addr]
		teq		tmp, #0
		bne	retry
	  }

	return oldval;
}

#define AO_HAVE_test_and_set

/* NEC LE-IT: fetch and add for ARMv6 */
AO_INLINE AO_t
AO_fetch_and_add(volatile AO_t *p, AO_t incr)
{
	unsigned long tmp,tmp2;
	AO_t result;

retry:
__asm {
	ldrex	result, [p]
	add     tmp, incr, result
	strex	tmp2, tmp, [p]
	teq	tmp2, #0
	bne	retry }

	return result;
}

#define AO_HAVE_fetch_and_add

/* NEC LE-IT: fetch and add1 for ARMv6 */
AO_INLINE AO_t
AO_fetch_and_add1(volatile AO_t *p)
{
	unsigned long tmp,tmp2;
	AO_t result;

retry:
__asm {
	ldrex	result, [p]
	add     tmp, result, #1
	strex	tmp2, tmp, [p]
	teq		tmp2, #0
	bne	retry
	}

	return result;
}

#define AO_HAVE_fetch_and_add1

/* NEC LE-IT: fetch and sub for ARMv6 */
AO_INLINE AO_t
AO_fetch_and_sub1(volatile AO_t *p)
{
	unsigned long tmp,tmp2;
	AO_t result;

retry:
__asm {
	ldrex	result, [p]
	sub     tmp, result, #1
	strex	tmp2, tmp, [p]
	teq		tmp2, #0
	bne	retry
	}

	return result;
}

#define AO_HAVE_fetch_and_sub1

/* NEC LE-IT: compare and swap */
/* Returns nonzero if the comparison succeeded. */
AO_INLINE int
AO_compare_and_swap(volatile AO_t *addr,
		  	     	AO_t old_val, AO_t new_val) 
{
  	 AO_t result,tmp;

retry:
__asm__ {
	ldrex	tmp, [addr]
	mov		result, #2
	teq		tmp, old_val
	strexeq	result, new_val, [addr]
	teq		result, #1
	beq		retry
	}
	
	return (result^2)>>1;
}
#define AO_HAVE_compare_and_swap

#endif // __TARGET_ARCH_ARM
