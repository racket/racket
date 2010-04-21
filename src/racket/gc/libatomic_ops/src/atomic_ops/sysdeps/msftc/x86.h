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

/* The following really assume we have a 486 or better. 		*/
/* If ASSUME_WINDOWS98 is defined, we assume Windows 98 or newer.	*/
/* If ASSUME_VISTA is defined, we assume Windows Server 2003, Vista	*/
/* or later.								*/

#include "../all_aligned_atomic_load_store.h"

/* Real X86 implementations, except for some old WinChips, appear	*/
/* to enforce ordering between memory operations, EXCEPT that a later	*/
/* read can pass earlier writes, presumably due to the visible		*/
/* presence of store buffers.						*/
/* We ignore both the WinChips, and the fact that the official specs	*/
/* seem to be much weaker (and arguably too weak to be usable).		*/

#include "../ordered_except_wr.h"

#include "../test_and_set_t_is_char.h"

#include <windows.h>
	/* Seems like over-kill, but that's what MSDN recommends.	*/
	/* And apparently winbase.h is not always self-contained.	*/

#if _MSC_VER < 1310

#define _InterlockedIncrement       InterlockedIncrement
#define _InterlockedDecrement       InterlockedDecrement
#define _InterlockedExchange        InterlockedExchange 
#define _InterlockedExchangeAdd     InterlockedExchangeAdd
#define _InterlockedCompareExchange InterlockedCompareExchange

#else

#if _MSC_VER >= 1400
#include <intrin.h>

#pragma intrinsic (_ReadWriteBarrier)

#else
#ifdef __cplusplus
extern "C" {
#endif

LONG __cdecl _InterlockedIncrement(LONG volatile *Addend);
LONG __cdecl _InterlockedDecrement(LONG volatile *Addend);
LONG __cdecl _InterlockedExchangeAdd(LONG volatile* Target, LONG Addend);
LONG __cdecl _InterlockedExchange(LONG volatile* Target, LONG Value);
LONG __cdecl _InterlockedCompareExchange(LONG volatile* Dest,
                                         LONG Exchange, LONG Comp);

#ifdef __cplusplus
}
#endif
#endif /* _MSC_VER >= 1400 */

#pragma intrinsic (_InterlockedIncrement)
#pragma intrinsic (_InterlockedDecrement)
#pragma intrinsic (_InterlockedExchange)
#pragma intrinsic (_InterlockedExchangeAdd)
#pragma intrinsic (_InterlockedCompareExchange)

#endif /* _MSC_VER < 1310 */

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
  return _InterlockedExchangeAdd((LONG volatile*)p, (LONG)incr);
}

#define AO_HAVE_fetch_and_add_full

AO_INLINE AO_t
AO_fetch_and_add1_full (volatile AO_t *p)
{
  return _InterlockedIncrement((LONG volatile *)p) - 1;
}

#define AO_HAVE_fetch_and_add1_full

AO_INLINE AO_t
AO_fetch_and_sub1_full (volatile AO_t *p)
{
  return _InterlockedDecrement((LONG volatile *)p) + 1;
}

#define AO_HAVE_fetch_and_sub1_full

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

#ifdef AO_ASSUME_WINDOWS98
/* Returns nonzero if the comparison succeeded. */
AO_INLINE int
AO_compare_and_swap_full(volatile AO_t *addr,
		  	 AO_t old, AO_t new_val) 
{
    return _InterlockedCompareExchange((LONG volatile *)addr,
                                       (LONG)new_val, (LONG)old)
	   == (LONG)old;
}

#define AO_HAVE_compare_and_swap_full
#endif /* ASSUME_WINDOWS98 */

#ifdef _WIN64
#  error wrong architecture
#endif

#ifdef ASSUME_VISTA
/* NEC LE-IT: whenever we run on a pentium class machine we have that
 * certain function */

#include "../standard_ao_double_t.h"
#pragma intrinsic (_InterlockedCompareExchange64)
/* Returns nonzero if the comparison succeeded. */
AO_INLINE int
AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
        			       AO_t old_val1, AO_t old_val2,
               			       AO_t new_val1, AO_t new_val2) 
{
    __int64 oldv = (__int64)old_val2 | ((__int64)old_val1 << 32);
    __int64 newv = (__int64)new_val2 | ((__int64)new_val1 << 32);
    return _InterlockedCompareExchange64((__int64 volatile *)addr,
                                       newv, oldv) == oldv;
}
#define AO_HAVE_compare_double_and_swap_double_full

#ifdef __cplusplus
AO_INLINE int
AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
		  		       AO_double_t old_val,
				       AO_double_t new_val) 
{
    return _InterlockedCompareExchange64((__int64 volatile *)addr,
		new_val.AO_whole, old_val.AO_whole) == old_val.AO_whole;
}
#define AO_HAVE_double_compare_and_swap_full
#endif // __cplusplus
#endif /* ASSUME_VISTA */

#include "../ao_t_is_int.h"
