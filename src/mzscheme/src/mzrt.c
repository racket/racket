#include "schpriv.h"

#ifdef MZ_USE_PLACES

/************************************************************************/
/************************************************************************/
/************************************************************************/

#include "mzrt.h"
#include "schgc.h"

#ifdef MZ_XFORM
START_XFORM_SUSPEND;
#endif

/* std C headers */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <../sconfig.h>

/* platform headers */
#ifdef WIN32
# include <windows.h>
#else
# include <pthread.h>
# include <signal.h>
# include <unistd.h>
# include <time.h>
# if defined(UNIX_LIMIT_STACK) || defined(UNIX_LIMIT_FDSET_SIZE)
#   include <signal.h>
#   include <sys/time.h>
#   include <sys/resource.h>
# endif
#endif

void mzrt_set_user_break_handler(void (*user_break_handler)(int))
{
#ifdef WIN32
#else
  signal(SIGINT, user_break_handler);
#endif
}

static void segfault_handler(int signal_num) {
#ifdef WIN32
#else
  pid_t pid = getpid();
  char buffer[500];
  char buf[500];
  signal(SIGSEGV, segfault_handler);

  fprintf(stderr, "%i %i resume(r)/gdb(d)/exit(e)?\n", signal_num, pid);
  fflush(stderr);

  while(read(fileno(stdin), buf, 100) <= 0){
    if(errno != EINTR){
      fprintf(stderr, "\nCould not read response, sleeping for 20 seconds.\n");
    }
    switch(buf[0]) {
      case 'r':
        return;
        break;
      case 'd':
        snprintf(buffer, 500, "xterm -e gdb ./mzschemecgc %d &", pid);
        fprintf(stderr, "%i %i Launching GDB", signal_num, pid);
        system(buffer);
        break;
      case 'e':
      default:
        exit(1);
        break;
    }
  }
#endif
}

void mzrt_set_segfault_debug_handler()
{
#ifdef WIN32
#else
  signal(SIGSEGV, segfault_handler);
#endif
}

void mzrt_sleep(int seconds)
{
#ifdef WIN32
#else
  struct timespec set;
  struct timespec rem;
  set.tv_sec  = seconds;
  set.tv_nsec = 0;
  rem.tv_sec  = 0;
  rem.tv_nsec = 0;
  while ((-1 == nanosleep(&set, &rem))) {
    //fprintf(stderr, "%i %i INITIAL\n", set.tv_sec, set.tv_nsec);
    //fprintf(stderr, "%i %i LEFT\n", rem.tv_sec, rem.tv_nsec);
    set = rem;
    //fprintf(stderr, "%i %i NOW\n", set.tv_sec, set.tv_nsec);
  }
#endif
}

#ifdef MZ_XFORM
END_XFORM_SUSPEND;
#endif

/***********************************************************************/
/*                Atomic Ops                                           */
/***********************************************************************/

MZ_INLINE uint32_t mzrt_atomic_add_32(volatile unsigned int *counter, unsigned int value) {
#ifdef WIN32
# if defined(__MINGW32__)
  return InterlockedExchangeAdd((long *)counter, value);
# else
  return InterlockedExchangeAdd(counter, value);
# endif

#elif defined (__GNUC__) && (defined(__i386__) || defined(__x86_64__))
  asm volatile ("lock; xaddl %0,%1"
      : "=r" (value), "=m" (*counter)
      : "0" (value), "m" (*counter)
      : "memory", "cc");
  return value;
#else
#error !!!Atomic ops not provided!!!
#endif
}

/* returns the pre-incremented value */
MZ_INLINE uint32_t mzrt_atomic_incr_32(volatile unsigned int *counter) {
  return mzrt_atomic_add_32(counter, 1);
}

/***********************************************************************/
/*                Threads                                              */
/***********************************************************************/

struct mz_proc_thread {
#ifdef WIN32
  HANDLE threadid;
#else
  pthread_t threadid;
#endif
};

int mz_proc_thread_self() {
#ifdef WIN32
#error !!!mz_proc_thread_id not implemented!!!
#else
  return (int) pthread_self();
#endif
}

int mz_proc_thread_id(mz_proc_thread* thread) {

  return (int) thread->threadid;
}


mz_proc_thread* mz_proc_thread_create(mz_proc_thread_start start_proc, void* data) {
  mz_proc_thread *thread = (mz_proc_thread*)malloc(sizeof(mz_proc_thread));
#ifdef WIN32
#   ifndef MZ_PRECISE_GC
  thread->threadid = CreateThread(NULL, 0, start_proc, data, 0, NULL);
#   else
  thread->threadid = CreateThread(NULL, 0, start_proc, data, 0, NULL);
#   endif
#else
#   ifndef MZ_PRECISE_GC
  GC_pthread_create(&thread->threadid, NULL, start_proc, data);
#   else
  pthread_create(&thread->threadid, NULL, start_proc, data);
#   endif
#endif
  return thread;
}

void * mz_proc_thread_wait(mz_proc_thread *thread) {
#ifdef WIN32
  DWORD rc;
  WaitForSingleObject(thread->threadid,INFINITE);
  GetExitCodeThread(thread->threadid, &rc);
  return (void *) rc;
#else
  void *rc;
#   ifndef MZ_PRECISE_GC
  GC_pthread_join(thread->threadid, &rc);
#   else
  pthread_join(thread->threadid, &rc);
#   endif
  return rc;
#endif
}

/***********************************************************************/
/*                RW Lock                                              */
/***********************************************************************/

/* Unix **************************************************************/

#ifndef WIN32

#ifdef MZ_XFORM
START_XFORM_SUSPEND;
#endif

struct mzrt_rwlock {
  pthread_rwlock_t lock;
};

int mzrt_rwlock_create(mzrt_rwlock **lock) {
  *lock = malloc(sizeof(mzrt_rwlock));
  return pthread_rwlock_init(&(*lock)->lock, NULL);
}

int mzrt_rwlock_rdlock(mzrt_rwlock *lock) {
  return pthread_rwlock_rdlock(&lock->lock);
}

int mzrt_rwlock_wrlock(mzrt_rwlock *lock) {
  return pthread_rwlock_wrlock(&lock->lock);
}

int mzrt_rwlock_tryrdlock(mzrt_rwlock *lock) {
  return pthread_rwlock_tryrdlock(&lock->lock);
}

int mzrt_rwlock_trywrlock(mzrt_rwlock *lock) {
  return pthread_rwlock_trywrlock(&lock->lock);
}
int mzrt_rwlock_unlock(mzrt_rwlock *lock) {
  return pthread_rwlock_unlock(&lock->lock);
}

int mzrt_rwlock_destroy(mzrt_rwlock *lock) {
  return pthread_rwlock_destroy(&lock->lock);
}

struct mzrt_mutex {
  pthread_mutex_t mutex;
};

int mzrt_mutex_create(mzrt_mutex **mutex) {
  *mutex = malloc(sizeof(mzrt_mutex));
  return pthread_mutex_init(&(*mutex)->mutex, NULL);
}

int mzrt_mutex_lock(mzrt_mutex *mutex) {
  return pthread_mutex_lock(&mutex->mutex);
}

int mzrt_mutex_trylock(mzrt_mutex *mutex) {
  return pthread_mutex_trylock(&mutex->mutex);
}

int mzrt_mutex_unlock(mzrt_mutex *mutex) {
  return pthread_mutex_unlock(&mutex->mutex);
}

int mzrt_mutex_destroy(mzrt_mutex *mutex) {
  return pthread_mutex_destroy(&mutex->mutex);
}


#ifdef MZ_XFORM
END_XFORM_SUSPEND;
#endif

#endif

/* Windows **************************************************************/

#ifdef WIN32

#ifdef MZ_XFORM
START_XFORM_SUSPEND;
#endif

typedef struct mzrt_rwlock {
  HANDLE readEvent;
  HANDLE writeMutex;
  unsigned long readers;
} mzrt_rwlock;

int mzrt_rwlock_create(mzrt_rwlock **lock) {
  *lock = malloc(sizeof(mzrt_rwlock));
  (*lock)->readers = 0;
  /* CreateEvent(LPSECURITY_ATTRIBUTES, manualReset, initiallySignaled, LPCSTR name) */
  if (! ((*lock)->readEvent = CreateEvent(NULL, TRUE, FALSE, NULL)))
    return 0;
  if (! ((*lock)->writeMutex = CreateMutex(NULL, FALSE, NULL)))
    return 0;

  return 1;
}

static int get_win32_os_error() {
  return 0;
}

static int mzrt_rwlock_rdlock_worker(mzrt_rwlock *lock, DWORD millis) {
  DWORD rc = WaitForSingleObject(lock->writeMutex, millis);
  if (rc == WAIT_FAILED || rc == WAIT_TIMEOUT );
    return 0;

  InterlockedIncrement(&lock->readers);

  if (! ResetEvent(lock->readEvent))
    return 0;

  if (!ReleaseMutex(lock->writeMutex))
    return 0;

  return 1;
}

static int mzrt_rwlock_wrlock_worker(mzrt_rwlock *lock, DWORD millis) {
  DWORD rc = WaitForSingleObject(lock->writeMutex, millis);
  if (rc == WAIT_FAILED || rc == WAIT_TIMEOUT );
    return 0;

  if (lock->readers) {
    if (millis) {
      rc = WaitForSingleObject(lock->readEvent, millis);
    }
    else {
      rc = WAIT_TIMEOUT;
    }

    if (rc == WAIT_FAILED || rc == WAIT_TIMEOUT );
      return 0;
  }

  return 1;
}

int mzrt_rwlock_rdlock(mzrt_rwlock *lock) {
  return mzrt_rwlock_rdlock_worker(lock, INFINITE);
}

int mzrt_rwlock_wrlock(mzrt_rwlock *lock) {
  return mzrt_rwlock_wrlock_worker(lock, INFINITE);
}

int mzrt_rwlock_tryrdlock(mzrt_rwlock *lock) {
  return mzrt_rwlock_rdlock_worker(lock, 0);
}

int mzrt_rwlock_trywrlock(mzrt_rwlock *lock) {
  return mzrt_rwlock_wrlock_worker(lock, 0);
}

int mzrt_rwlock_unlock(mzrt_rwlock *lock) {
  DWORD rc = 0;
  if (!ReleaseMutex(lock->writeMutex)) {
    rc = get_win32_os_error();
  }

  if (rc == ERROR_NOT_OWNER) {
    if (lock->readers && !InterlockedDecrement(&lock->readers) && !SetEvent(lock->readEvent)) {
      rc = get_win32_os_error();
    }
    else {
      rc = 0;
    }
  }

  return !rc;
}

int mzrt_rwlock_destroy(mzrt_rwlock *lock) {
  int rc = 1;
  rc &= CloseHandle(lock->readEvent);
  rc &= CloseHandle(lock->writeMutex);
  return rc; 
}

struct mzrt_mutex {
  CRITICAL_SECTION critical_section;
};

int mzrt_mutex_create(mzrt_mutex **mutex) {
  *mutex = malloc(sizeof(mzrt_mutex));
  InitializeCriticalSection(&(*mutex)->critical_section);
  return 0;
}

int mzrt_mutex_lock(mzrt_mutex *mutex) {
  EnterCriticalSection(&(*mutex)->critical_section);
  return 0;
}

int mzrt_mutex_trylock(mzrt_mutex *mutex) {
  if (!TryEnterCriticalSection(&(*mutex)->critical_section))
    return 1;
  return 0;
}

int mzrt_mutex_unlock(mzrt_mutex *mutex) {
  LeaveCriticalSection(&(*mutex)->critical_section);
  return 0;
}

int mzrt_mutex_destroy(mzrt_mutex *mutex) {
  DeleteCriticalSection(&(*mutex)->critical_section);
  return 0;
}

#ifdef MZ_XFORM
END_XFORM_SUSPEND;
#endif

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#endif
