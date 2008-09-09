#ifndef MZRT_H
#define MZRT_H

#ifdef MZ_USE_PLACES

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
/* mzrt_threads.c */
typedef struct mz_proc_thread mz_proc_thread; /* OPAQUE DEFINITION */

#ifdef WIN32
typedef DWORD (WINAPI *mz_proc_thread_start)(void*);
#else
typedef void *(mz_proc_thread_start)(void*);
#endif

mz_proc_thread* mz_proc_thread_create(mz_proc_thread_start*, void* data);
void *mz_proc_thread_wait(mz_proc_thread *thread);

void mzrt_sleep(int seconds);

int mz_proc_thread_self();
int mz_proc_thread_id(mz_proc_thread* thread);

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


#endif

#endif
