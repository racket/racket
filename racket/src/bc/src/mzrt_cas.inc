XFORM_NONGCING static MZ_INLINE int mz_MZRT_CAS(volatile mz_CAS_T *addr, mz_CAS_T old, mz_CAS_T new_val) 
  XFORM_SKIP_PROC 
{
#ifdef MZ_CAS_AVAILABLE
  return __sync_bool_compare_and_swap(addr, old, new_val);
#elif defined(__GNUC__)
# if defined(__i386__) || defined(__x86_64__)
#  ifdef mz_CAS_64
#   define CAS_I_SIZE "q"
#  elif defined(mz_CAS_16)
#   define CAS_I_SIZE ""
#  elif defined(mz_CAS_32)
#   define CAS_I_SIZE "l"
#  endif
    char result;
    __asm__ __volatile__("lock; cmpxchg" CAS_I_SIZE " %3, %0; setz %1"
        : "=m"(*addr), "=q"(result)
        : "m"(*addr), "r" (new_val), "a"(old)
        : "memory");
    return (int) result;
#  undef CAS_I_SIZE
# elif defined(__POWERPC__) || defined(__powerpc__) || defined(__ppc__) || defined(__PPC__)  \
  || defined(__powerpc64__) || defined(__ppc64__)
#  ifdef mz_CAS_64
#   define CAS_I_SIZE "d"
#  elif defined(mz_CAS_16)
#   define CAS_I_SIZE "h"
#  elif defined(mz_CAS_32)
#   define CAS_I_SIZE "w"
#  endif
    mz_CAS_T oldval;
    int result = 0;
    /* This code is based on Boehm GC's libatomic */
    __asm__ __volatile__(
                         "1:l" CAS_I_SIZE "arx %0,0,%2\n" /* load and reserve */
                         "cmp" CAS_I_SIZE " %0, %4\n"                  /* if load is not equal to  */
                         "bne 2f\n"                       /*   old, fail */
                         "st" CAS_I_SIZE "cx. %3,0,%2\n"  /* else store conditional */
                         "bne- 1b\n"                      /* retry if lost reservation */
                         "li %1,1\n"                      /* result = 1;     */
                         "2:\n"
                         : "=&r"(oldval), "=&r"(result)
                         : "r"(addr), "r"(new_val), "r"(old), "1"(result)
                         : "memory", "cc");
    
    return result;
#  undef CAS_I_SIZE
# else
#  error mzrt_cas not defined on this platform
# endif

#elif defined(_MSC_VER)
# ifdef mz_CAS_64
  return _InterlockedCompareExchange64((__int64 *)addr, (__int64)new_val, (__int64)old) == (__int64)old;
# elif defined(mz_CAS_16)
  return _InterlockedCompareExchange16((short *)addr, (short)new_val, (short)old) == (short)old;
# else
  return _InterlockedCompareExchange((long *)addr, (long)new_val, (long)old) == (long)old;
# endif
#else
# error mzrt_cas not defined on this platform
#endif
}
