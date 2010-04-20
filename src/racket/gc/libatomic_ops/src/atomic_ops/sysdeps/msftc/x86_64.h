/*
 * Copyright (c) 2003 Hewlett-Packard Development Company, L.P.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. 
 */

/* The following really assume we have a 486 or better. */
/* If ASSUME_WINDOWS98 is defined, we assume Windows 98 or newer.	*/

#include "../all_aligned_atomic_load_store.h"

/* Real X86 implementations, except for some old WinChips, appear	*/
/* to enforce ordering between memory operations, EXCEPT that a later	*/
/* read can pass earlier writes, presumably due to the visible		*/
/* presence of store buffers.						*/
/* We ignore both the WinChips, and the fact that the official specs	*/
/* seem to be much weaker (and arguably too weak to be usable).		*/

#include "../ordered_except_wr.h"

#if 0
FIXME: Need to reimplement testandset

#include "../test_and_set_t_is_char.h"

#else

#include "../test_and_set_t_is_ao_t.h"

#endif

#include <windows.h>
	/* Seems like over-kill, but that's what MSDN recommends.	*/
	/* And apparently winbase.h is not always self-contained.	*/


#include <intrin.h>

#pragma intrinsic (_ReadWriteBarrier)

#ifdef __cplusplus
extern "C" {
#endif

LONGLONG __cdecl _InterlockedIncrement64(LONGLONG volatile *Addend);
LONGLONG __cdecl _InterlockedDecrement64(LONGLONG volatile *Addend);
LONGLONG __cdecl _InterlockedExchangeAdd64(LONGLONG volatile* Target,
					   LONGLONG Addend);
LONGLONG __cdecl _InterlockedExchange64(LONGLONG volatile* Target,
				        LONGLONG Value);
LONGLONG __cdecl _InterlockedCompareExchange64(LONGLONG volatile* Dest,
                                               LONGLONG Exchange,
					       LONGLONG Comp);

#ifdef __cplusplus
}
#endif

#pragma intrinsic (_InterlockedIncrement64)
#pragma intrinsic (_InterlockedDecrement64)
#pragma intrinsic (_InterlockedExchange64)
#pragma intrinsic (_InterlockedExchangeAdd64)
#pragma intrinsic (_InterlockedCompareExchange64)

/* As far as we can tell, the lfence and sfence instructions are not	*/
/* currently needed or useful for cached memory accesses.		*/

/* Unfortunately mfence doesn't exist everywhere. 		*/
/* IsProcessorFeaturePresent(PF_COMPARE_EXCHANGE128) is		*/
/* probably a conservative test for it?				*/

#if defined(AO_USE_PENTIUM4_INSTRS)

AO_INLINE void
AO_nop_full()
{
  __asm { mfence }
}

#define AO_HAVE_nop_full

#else

/* We could use the cpuid instruction.  But that seems to be slower 	*/
/* than the default implementation based on test_and_set_full.  Thus	*/
/* we omit that bit of misinformation here.				*/

#endif

AO_INLINE AO_t
AO_fetch_and_add_full (volatile AO_t *p, AO_t incr)
{
  return _InterlockedExchangeAdd64((LONGLONG volatile *)p, (LONGLONG)incr);
}

#define AO_HAVE_fetch_and_add_full

AO_INLINE AO_t
AO_fetch_and_add1_full (volatile AO_t *p)
{
  return _InterlockedIncrement64((LONGLONG volatile *)p) - 1;
}

#define AO_HAVE_fetch_and_add1_full

AO_INLINE AO_t
AO_fetch_and_sub1_full (volatile AO_t *p)
{
  return _InterlockedDecrement64((LONGLONG volatile *)p) + 1;
}

#define AO_HAVE_fetch_and_sub1_full

AO_INLINE int
AO_compare_and_swap_full(volatile AO_t *addr,
		  	 AO_t old, AO_t new_val) 
{
    return _InterlockedCompareExchange64((LONGLONG volatile *)addr,
                                         (LONGLONG)new_val, (LONGLONG)old)
	   == (LONGLONG)old;
}

#define AO_HAVE_compare_and_swap_full

#if 0
FIXME: (__asm not supported)
AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr)
{
    __asm
    {
	mov	eax,AO_TS_SET		;
	mov	ebx,addr		;
	xchg	byte ptr [ebx],al	;
    }
}

#define AO_HAVE_test_and_set_full

FIXME: (__asm not supported)
NEC LE-IT: Don't have a working Win64 environment here at the moment.
AO_compare_double_and_swap_double_full needs implementation for Win64
But there is no _InterlockedCompareExchange128 in the WinAPI, so we
need basically whats given below.
Also see gcc/x86_64.h for partial old opteron workaround:

#ifndef AO_CASDOUBLE_MISSING

AO_INLINE int
AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
				       AO_t old_val1, AO_t old_val2,
		                       AO_t new_val1, AO_t new_val2)
{
	char result;
	__asm
	{
		mov	rdx,QWORD PTR [old_val]
		mov	rax,QWORD PTR [old_val + 8]
		mov	rcx,QWORD PTR [new_val]
		mov	rbx,QWORD PTR [new_val + 8]
		lock cmpxchg16b	[addr]
		setz result;
	}
	return result;
}
#endif // AO_CASDOUBLE_MISSING
#define AO_HAVE_compare_double_and_swap_double_full

#endif /* 0 */

