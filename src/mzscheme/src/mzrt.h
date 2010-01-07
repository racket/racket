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

#ifdef WIN32
typedef HANDLE mzrt_thread_id;
#else
typedef pthread_t mzrt_thread_id;
#endif


typedef struct mz_proc_thread {
  mzrt_thread_id threadid;
  struct pt_mbox *mbox;
} mz_proc_thread;


#ifdef WIN32
typedef DWORD (WINAPI *mz_proc_thread_start)(void*);
#else
typedef void *(mz_proc_thread_start)(void*);
#endif

mz_proc_thread* mzrt_proc_first_thread_init();
mz_proc_thread* mz_proc_thread_create(mz_proc_thread_start*, void* data);
void *mz_proc_thread_wait(mz_proc_thread *thread);
int mz_proc_thread_detach(mz_proc_thread *thread);

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

#endif

#endif
