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
typedef DWORD mzrt_os_thread_id;
#else
#include <pthread.h>
typedef pthread_t mzrt_thread_id;
typedef pthread_t mzrt_os_thread_id;
#endif


typedef struct mz_proc_thread {
  mzrt_thread_id threadid;
  int refcount;
#if (defined(__WIN32__) || defined(WIN32) || defined(_WIN32))
  void *res;
#endif
} mz_proc_thread;


typedef void *(*mz_proc_thread_start)(void*);

mz_proc_thread* mzrt_proc_first_thread_init();
mz_proc_thread* mz_proc_thread_create(mz_proc_thread_start, void* data);
mz_proc_thread* mz_proc_thread_create_w_stacksize(mz_proc_thread_start, void* data, intptr_t stacksize);
void *mz_proc_thread_wait(mz_proc_thread *thread);
int mz_proc_thread_detach(mz_proc_thread *thread);
void mz_proc_thread_exit(void *rc);

void mzrt_sleep(int seconds);

mzrt_os_thread_id mz_proc_os_thread_self();
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

#if defined(MZ_USE_PLACES) || defined(MZ_USE_FUTURES)

#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-function"
#endif

#define mz_CAS_T uintptr_t
#ifdef SIXTY_FOUR_BIT_INTEGERS
# define mz_CAS_64
#else
# define mz_CAS_32
#endif
#define mz_MZRT_CAS mzrt_cas
#include "mzrt_cas.inc"
#undef mz_CAS_T
#ifdef SIXTY_FOUR_BIT_INTEGERS
# undef mz_CAS_64
#else
# undef mz_CAS_32
#endif
#undef mz_MZRT_CAS

#define mz_CAS_T short
#define mz_CAS_16
#define mz_MZRT_CAS mzrt_cas16
#include "mzrt_cas.inc"
#undef mz_CAS_T
#undef mz_CAS_16
#undef mz_MZRT_CAS

static MZ_INLINE void mzrt_ensure_max_cas(uintptr_t *atomic_val, uintptr_t len) {
  int set = 0;
  while(!set) {
    uintptr_t old_val = *atomic_val;
    if (len > old_val) {
      set = !mzrt_cas(atomic_val, old_val, len);
    }
    else {
      set = 1;
    }
  }
}

#ifdef __clang__
# pragma clang diagnostic pop
#endif

/* end defined(MZ_USE_PLACES) || defined(MZ_USE_FUTURES) */
#endif

/****************************************************************/

/* end MZ_USE_MZRT */
#endif

#endif
