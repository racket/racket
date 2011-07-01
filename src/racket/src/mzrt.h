#ifndef MZRT_H
#define MZRT_H

#ifdef MZ_USE_MZRT

/****************** ATOMIC OPERATIONS ************************************/
/* mzrt_atomic_ops.c */
#ifdef _MSC_VER
typedef unsigned int uint32_t;
#else
# include <stdint.h>
#endif

/****************** SIGNAL HANDLING ***************************************/
/* mzrt.c */
void mzrt_set_segfault_debug_handler();
void mzrt_set_user_break_handler(void (*user_break_handler)(int));


/****************** PROCESS WEIGHT THREADS ********************************/

#if (defined(__WIN32__) || defined(WIN32) || defined(_WIN32))
# include <winsock2.h>
typedef HANDLE mzrt_thread_id;
#else
#include <pthread.h>
typedef pthread_t mzrt_thread_id;
#endif


typedef struct mz_proc_thread {
  mzrt_thread_id threadid;
  int refcount;
} mz_proc_thread;


typedef void *(*mz_proc_thread_start)(void*);

mz_proc_thread* mzrt_proc_first_thread_init();
mz_proc_thread* mz_proc_thread_create(mz_proc_thread_start, void* data);
mz_proc_thread* mz_proc_thread_create_w_stacksize(mz_proc_thread_start, void* data, intptr_t stacksize);
void *mz_proc_thread_wait(mz_proc_thread *thread);
int mz_proc_thread_detach(mz_proc_thread *thread);
void mz_proc_thread_exit(void *rc);

void mzrt_sleep(int seconds);

mzrt_thread_id mz_proc_thread_self();
mzrt_thread_id mz_proc_thread_id(mz_proc_thread* thread);

/****************** THREAD RWLOCK ******************************************/
/* mzrt_rwlock_*.c */
typedef struct mzrt_rwlock mzrt_rwlock; /* OPAQUE DEFINITION */
int mzrt_rwlock_create(mzrt_rwlock **lock);
int mzrt_rwlock_rdlock(mzrt_rwlock *lock);
int mzrt_rwlock_wrlock(mzrt_rwlock *lock);
int mzrt_rwlock_tryrdlock(mzrt_rwlock *lock);
int mzrt_rwlock_trywrlock(mzrt_rwlock *lock);
int mzrt_rwlock_unlock(mzrt_rwlock *lock);
int mzrt_rwlock_destroy(mzrt_rwlock *lock);

/****************** THREAD MUTEX ******************************************/
typedef struct mzrt_mutex mzrt_mutex; /* OPAQUE DEFINITION */
int mzrt_mutex_create(mzrt_mutex **mutex);
int mzrt_mutex_lock(mzrt_mutex *mutex);
int mzrt_mutex_trylock(mzrt_mutex *mutex);
int mzrt_mutex_unlock(mzrt_mutex *mutex);
int mzrt_mutex_destroy(mzrt_mutex *mutex);

/****************** THREAD COND *******************************************/
typedef struct mzrt_cond mzrt_cond; /* OPAQUE DEFINITION */
int mzrt_cond_create(mzrt_cond **cond);
int mzrt_cond_wait(mzrt_cond *cond, mzrt_mutex *mutex);
int mzrt_cond_timedwait(mzrt_cond *cond, mzrt_mutex *mutex, intptr_t seconds, intptr_t nanoseconds);
int mzrt_cond_signal(mzrt_cond *cond);
int mzrt_cond_broadcast(mzrt_cond *cond);
int mzrt_cond_destroy(mzrt_cond *cond);

/****************** THREAD SEMA ******************************************/
typedef struct mzrt_sema mzrt_sema; /* OPAQUE DEFINITION */
int mzrt_sema_create(mzrt_sema **sema, int init);
int mzrt_sema_post(mzrt_sema *sema);
int mzrt_sema_wait(mzrt_sema *sema);
int mzrt_sema_destroy(mzrt_sema *sema);

/****************** Compare and Swap *******************************/

static MZ_INLINE int mzrt_cas(volatile size_t *addr, size_t old, size_t new_val) {
#if defined(__GNUC__) && !defined(__INTEL_COMPILER) && __GNUC__ <= 4 && __GNUC_MINOR__ < 1
# if defined(__i386__)
    char result;
    __asm__ __volatile__("lock; cmpxchgl %3, %0; setz %1"
        : "=m"(*addr), "=q"(result)
        : "m"(*addr), "r" (new_val), "a"(old)
        : "memory");
    return (int) result;
# elif defined(__x86_64__)
    char result;
    __asm__ __volatile__("lock; cmpxchgq %3, %0; setz %1"
        : "=m"(*addr), "=q"(result)
        : "m"(*addr), "r" (new_val), "a"(old)
        : "memory");
    return (int) result;
# elif defined(__POWERPC__) || defined(__powerpc__) || defined(__ppc__) || defined(__PPC__)  \
  || defined(__powerpc64__) || defined(__ppc64__)
    size_t oldval;
    int result = 0;
#  if defined(__powerpc64__) || defined(__ppc64__) || defined(__64BIT__)
#   define CAS_I_SIZE "d"
#  else
#   define CAS_I_SIZE "w"
#  endif
    /* This code is based on Boehm GC's libatomic */
    __asm__ __volatile__(
                         "1:l" CAS_I_SIZE "arx %0,0,%2\n" /* load and reserve */
                         "cmpw %0, %4\n"                  /* if load is not equal to  */
                         "bne 2f\n"                       /*   old, fail */
                         "st" CAS_I_SIZE "cx. %3,0,%2\n"  /* else store conditional */
                         "bne- 1b\n"                      /* retry if lost reservation */
                         "li %1,1\n"                      /* result = 1;     */
                         "2:\n"
                         : "=&r"(oldval), "=&r"(result)
                         : "r"(addr), "r"(new_val), "r"(old), "1"(result)
                         : "memory", "cc");
    
    return result;
# else
#  error mzrt_cas not defined on this platform
# endif

#elif defined(__GNUC__) && !defined(__INTEL_COMPILER)
  return __sync_bool_compare_and_swap(addr, old, new_val);
#elif defined(_MSC_VER)
# if defined(_AMD64_)
  return _InterlockedCompareExchange64((LONGLONG volatile *)addr, (LONGLONG)new_val, (LONGLONG)old) == (LONGLONG)old;
# elif _M_IX86 >= 400
  return _InterlockedCompareExchange((LONG volatile *)addr, (LONG)new_val, (LONG)old) == (LONG)old;
# endif
#else
# error mzrt_cas not defined on this platform
#endif
}

static MZ_INLINE void mzrt_ensure_max_cas(uintptr_t *atomic_val, uintptr_t len) {
  int set = 0;
  while(!set) {
    uintptr_t old_val = *atomic_val;
    if (len > old_val) {
      set = !mzrt_cas((size_t *)atomic_val, old_val, len);
    }
    else {
      set = 1;
    }
  }
}
#endif

#endif
