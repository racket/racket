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

MZ_INLINE uint32_t mzrt_atomic_add_32(volatile unsigned int *counter, unsigned int value);
MZ_INLINE uint32_t mzrt_atomic_incr_32(volatile unsigned int *counter);


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
  struct pt_mbox *mbox;
} mz_proc_thread;


typedef void *(*mz_proc_thread_start)(void*);

mz_proc_thread* mzrt_proc_first_thread_init();
mz_proc_thread* mz_proc_thread_create(mz_proc_thread_start, void* data);
mz_proc_thread* mz_proc_thread_create_w_stacksize(mz_proc_thread_start, void* data, long stacksize);
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
int mzrt_cond_timedwait(mzrt_cond *cond, mzrt_mutex *mutex, long seconds, long nanoseconds);
int mzrt_cond_signal(mzrt_cond *cond);
int mzrt_cond_broadcast(mzrt_cond *cond);
int mzrt_cond_destroy(mzrt_cond *cond);

/****************** THREAD SEMA ******************************************/
typedef struct mzrt_sema mzrt_sema; /* OPAQUE DEFINITION */
int mzrt_sema_create(mzrt_sema **sema, int init);
int mzrt_sema_post(mzrt_sema *sema);
int mzrt_sema_wait(mzrt_sema *sema);
int mzrt_sema_destroy(mzrt_sema *sema);

/****************** PROCESS THREAD MAIL BOX *******************************/
typedef struct pt_mbox_msg {
  int     type;
  void    *payload;
  struct pt_mbox *origin;
} pt_mbox_msg;

typedef struct pt_mbox {
  struct pt_mbox_msg queue[5];
  int count;
  int in;
  int out;
  mzrt_mutex *mutex;
  mzrt_cond *nonempty;
  mzrt_cond *nonfull;
} pt_mbox;

pt_mbox *pt_mbox_create();
void pt_mbox_send(pt_mbox *mbox, int type, void *payload, pt_mbox *origin);
void pt_mbox_recv(pt_mbox *mbox, int *type, void **payload, pt_mbox **origin);
void pt_mbox_send_recv(pt_mbox *mbox, int type, void *payload, pt_mbox *origin, int *return_type, void **return_payload);
void pt_mbox_destroy(pt_mbox *mbox);

static MZ_INLINE int mzrt_cas(volatile size_t *addr, size_t old, size_t new_val) {
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
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
# elif defined(__powerpc__) || defined(__ppc__) || defined(__PPC__) \
     || defined(__powerpc64__) || defined(__ppc64__)

#  if defined(__powerpc64__) || defined(__ppc64__) || defined(__64BIT__)
/* FIXME: Completely untested.  */
  AO_t oldval;
  int result = 0;

  __asm__ __volatile__(
               "1:ldarx %0,0,%2\n"   /* load and reserve              */
               "cmpd %0, %4\n"      /* if load is not equal to  */
               "bne 2f\n"            /*   old, fail     */
               "stdcx. %3,0,%2\n"    /* else store conditional         */
               "bne- 1b\n"           /* retry if lost reservation      */
         "li %1,1\n"       /* result = 1;     */
               "2:\n"
              : "=&r"(oldval), "=&r"(result)
              : "r"(addr), "r"(new_val), "r"(old), "1"(result)
              : "memory", "cc");

  return result;
#  else
  AO_t oldval;
  int result = 0;

  __asm__ __volatile__(
               "1:lwarx %0,0,%2\n"   /* load and reserve              */
               "cmpw %0, %4\n"      /* if load is not equal to  */
               "bne 2f\n"            /*   old, fail     */
               "stwcx. %3,0,%2\n"    /* else store conditional         */
               "bne- 1b\n"           /* retry if lost reservation      */
         "li %1,1\n"       /* result = 1;     */
               "2:\n"
              : "=&r"(oldval), "=&r"(result)
              : "r"(addr), "r"(new_val), "r"(old), "1"(result)
              : "memory", "cc");

  return result;
#  endif
# else
# error mzrt_cas not defined on this platform
# endif
#elif defined(_MSC_VER)
# if defined(_AMD64_)
  return _InterlockedCompareExchange64((LONGLONG volatile *)addr, (LONGLONG)new_val, (LONGLONG)old) == (LONGLONG)old
# elif _M_IX86 >= 400
  return _InterlockedCompareExchange((LONG volatile *)addr, (LONG)new_val, (LONG)old) == (LONG)old;
# endif
#else
# error mzrt_cas not defined on this platform
#endif
}

static MZ_INLINE void mzrt_ensure_max_cas(unsigned long *atomic_val, unsigned long len) {
  int set = 0;
  while(!set) {
    unsigned long old_val = *atomic_val;
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
