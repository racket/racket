/*
  Racket
  Copyright (c) 2009-2013 PLT Design Inc.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.
*/

#include "schpriv.h"

#ifdef MZ_USE_MZRT

/************************************************************************/
/************************************************************************/
/************************************************************************/
#include "schgc.h"

THREAD_LOCAL_DECL(mz_proc_thread *proc_thread_self);

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
# include <process.h>
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

/* Define this is we need CGC support for threads. This was needed
   when we tried to make places work with the Boehm GC, but since that has
   other problems (notably disappearing links), we have given up on
   having threads cooperate with CGC. */
/* #define NEED_GC_THREAD_OPS */

#ifdef NEED_GC_THREAD_OPS
int GC_pthread_join(pthread_t thread, void **retval);
int GC_pthread_create(pthread_t *thread, const pthread_attr_t *attr, void *(*start_routine)(void*), void * arg);
int GC_pthread_detach(pthread_t thread);
#endif

void mzrt_set_user_break_handler(void (*user_break_handler)(int))
{
#ifdef WIN32
#else
  signal(SIGINT, user_break_handler);
#endif
}

static void rungdb() {
#ifdef WIN32
#else
  pid_t pid = getpid();
  char outbuffer[100];
  char inbuffer[10];

  fprintf(stderr, "pid # %i resume(r)/gdb(d)/exit(e)?\n", pid);
  fflush(stderr);

  while(1) {
    while(read(fileno(stdin), inbuffer, 10) <= 0){
      if(errno != EINTR){
        fprintf(stderr, "Error detected %i\n", errno);
      }
    }
    switch(inbuffer[0]) {
      case 'r':
        return;
        break;
      case 'd':
        snprintf(outbuffer, 100, "xterm -e gdb ./racket3m %d &", pid);
        fprintf(stderr, "%s\n", outbuffer);
        if(system(outbuffer)) 
          fprintf(stderr, "system failed\n");
        break;
      case 'e':
      default:
        exit(1);
        break;
    }
  }
#endif
}

#ifndef WIN32
static void segfault_handler(int signal_num) {
  pid_t pid = getpid();
  fprintf(stderr, "sig# %i pid# %i\n", signal_num, pid);
  rungdb();
}
#endif

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
  Sleep(seconds * 1000);
#else
  struct timespec set;
  struct timespec rem;
  set.tv_sec  = seconds;
  set.tv_nsec = 0;
  rem.tv_sec  = 0;
  rem.tv_nsec = 0;
  while ((-1 == nanosleep(&set, &rem))) {
    /* fprintf(stderr, "%i %i INITIAL\n", set.tv_sec, set.tv_nsec); */
    /* fprintf(stderr, "%i %i LEFT\n", rem.tv_sec, rem.tv_nsec); */
    set = rem;
    /* fprintf(stderr, "%i %i NOW\n", set.tv_sec, set.tv_nsec); */
  }
#endif
}

/***********************************************************************/
/*                Threads                                              */
/***********************************************************************/
typedef struct mzrt_thread_stub_data {
  mz_proc_thread_start start_proc;
  void *data;
  mz_proc_thread *thread;
} mzrt_thread_stub_data;

void *mzrt_thread_stub(void *data){
  mzrt_thread_stub_data *stub_data  = (mzrt_thread_stub_data*) data;
  mz_proc_thread_start start_proc     = stub_data->start_proc;
  void *start_proc_data               = stub_data->data;
  void* res;

  scheme_init_os_thread();

  proc_thread_self = stub_data->thread;

  free(data);

  res = start_proc(start_proc_data);

  if (!--proc_thread_self->refcount)
    free(proc_thread_self);

  scheme_done_os_thread();

  return res;
}

#ifdef WIN32
DWORD WINAPI mzrt_win_thread_stub(void *data)
{
  return (DWORD)mzrt_thread_stub(data);
}
#endif


mzrt_thread_id mz_proc_thread_self() {
#ifdef WIN32
  return GetCurrentThread();
#else
  return pthread_self();
#endif
}

mzrt_thread_id mz_proc_thread_id(mz_proc_thread* thread) {

  return thread->threadid;
}

mz_proc_thread* mzrt_proc_first_thread_init() {
  /* initialize mz_proc_thread struct for first thread that wasn't created with mz_proc_thread_create */
  mz_proc_thread *thread = (mz_proc_thread*)malloc(sizeof(mz_proc_thread));
  thread->threadid  = mz_proc_thread_self();
  proc_thread_self  = thread;
  thread->refcount = 1;
  return thread;
}

mz_proc_thread* mz_proc_thread_create_w_stacksize(mz_proc_thread_start start_proc, void* data, intptr_t stacksize)
{
  mz_proc_thread *thread = (mz_proc_thread*)malloc(sizeof(mz_proc_thread));
  mzrt_thread_stub_data *stub_data;
  int ok;

#   ifndef WIN32
  pthread_attr_t *attr;
  pthread_attr_t attr_storage;

  if (stacksize) {
    attr = &attr_storage;
    pthread_attr_init(attr);
    pthread_attr_setstacksize(attr, stacksize); /*8MB*/
  } else
    attr = NULL;
#   endif

  thread->refcount = 2;

  stub_data = (mzrt_thread_stub_data*)malloc(sizeof(mzrt_thread_stub_data));

  stub_data->start_proc = start_proc;
  stub_data->data       = data;
  stub_data->thread     = thread;
#   ifdef WIN32
  thread->threadid = (HANDLE)_beginthreadex(NULL, stacksize, mzrt_win_thread_stub, stub_data, 0, NULL);
  ok = (thread->threadid != -1L);
#   else
#    ifdef NEED_GC_THREAD_OPS
  ok = !GC_pthread_create(&thread->threadid, attr, mzrt_thread_stub, stub_data);
#    else
  ok = !pthread_create(&thread->threadid, attr, mzrt_thread_stub, stub_data);
#    endif
#   endif

  if (!ok) {
    free(thread);
    free(stub_data);
    return NULL;
  }

  return thread;
}

mz_proc_thread* mz_proc_thread_create(mz_proc_thread_start start_proc, void* data) {
  intptr_t stacksize;

#if defined(OS_X) || defined(linux)
  stacksize = 8*1024*1024;
#else
  stacksize = 0;
#endif

  return mz_proc_thread_create_w_stacksize(start_proc, data, stacksize);
}

void * mz_proc_thread_wait(mz_proc_thread *thread) {
  void *rc;
#ifdef WIN32
  DWORD rcw;
  WaitForSingleObject(thread->threadid,INFINITE);
  GetExitCodeThread(thread->threadid, &rcw);
  rc = (void *)rcw;
  CloseHandle(thread->threadid);
#else
#   ifdef NEED_GC_THREAD_OPS
  GC_pthread_join(thread->threadid, &rc);
#   else
  pthread_join(thread->threadid, &rc);
#   endif
#endif

  if (!--thread->refcount)
    free(thread);
  
  return rc;
}

int mz_proc_thread_detach(mz_proc_thread *thread) {
  int rc;
#ifdef WIN32
  rc = CloseHandle(thread->threadid);
#else
#   ifdef NEED_GC_THREAD_OPS
  rc = GC_pthread_detach(thread->threadid);
#   else
  rc = pthread_detach(thread->threadid);
#   endif
#endif

  if (!--thread->refcount)
    free(thread);

  return rc;
}

void mz_proc_thread_exit(void *rc) {
#ifdef WIN32
  _endthreadex((unsigned)rc);
#else
#   ifndef MZ_PRECISE_GC
  pthread_exit(rc);
#   else
  pthread_exit(rc);
#   endif
#endif
}

/***********************************************************************/
/*                RW Lock                                              */
/***********************************************************************/

/* Unix **************************************************************/

#ifndef WIN32

# ifdef HAVE_PTHREAD_RWLOCK

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

# else

struct mzrt_rwlock {
  pthread_mutex_t m;
  pthread_cond_t cr, cw;
  int readers, writers, write_waiting;
};

int mzrt_rwlock_create(mzrt_rwlock **lock) {
  int err;

  *lock = malloc(sizeof(mzrt_rwlock));
  err = pthread_mutex_init(&(*lock)->m, NULL);
  if (err) { free(*lock); return err; }
  err = pthread_cond_init(&(*lock)->cr, NULL);
  if (err) { free(*lock); return err; }
  err = pthread_cond_init(&(*lock)->cw, NULL);
  if (err) { free(*lock); return err; }
  return err;
}

static int rwlock_rdlock(mzrt_rwlock *lock, int just_try) {
  int err;

  err = pthread_mutex_lock(&lock->m);
  if (err) return err;
  while (lock->writers || lock->write_waiting) {
    if (just_try) {
      err = pthread_mutex_unlock(&lock->m);
      if (err) return err;
      return EBUSY;
    } else {
      err = pthread_cond_wait(&lock->cr, &lock->m);
      if (err)
        return err;
    }
  }
  lock->readers++;
  return pthread_mutex_unlock(&lock->m);
}

int mzrt_rwlock_rdlock(mzrt_rwlock *lock) {
  return rwlock_rdlock(lock, 0);
}

static int rwlock_wrlock(mzrt_rwlock *lock, int just_try) {
  int err;

  err = pthread_mutex_lock(&lock->m);
  if (err) return err;
  while (lock->writers || lock->readers) {
    if (just_try) {
      err = pthread_mutex_unlock(&lock->m);
      if (err) return err;
      return EBUSY;
    } else {
      lock->write_waiting++;
      err = pthread_cond_wait(&lock->cw, &lock->m);
      --lock->write_waiting;
      if (err)
        return err;
    }
  }
  lock->writers++;
  return pthread_mutex_unlock(&lock->m);
}

int mzrt_rwlock_wrlock(mzrt_rwlock *lock) {
  return rwlock_wrlock(lock, 0);
}

int mzrt_rwlock_tryrdlock(mzrt_rwlock *lock) {
  return rwlock_rdlock(lock, 1);
}

int mzrt_rwlock_trywrlock(mzrt_rwlock *lock) {
  return rwlock_wrlock(lock, 1);
}

int mzrt_rwlock_unlock(mzrt_rwlock *lock) {
  int err;

  err = pthread_mutex_lock(&lock->m);
  if (err) return err;

  if (lock->readers)
    --lock->readers; /* must have been a read lock */
  else
    --lock->writers;

  if (lock->write_waiting)
    err = pthread_cond_signal(&lock->cw);
  else
    err = pthread_cond_broadcast(&lock->cr);
  if (err) return err;
  
  return pthread_mutex_unlock(&lock->m);
}

int mzrt_rwlock_destroy(mzrt_rwlock *lock) {
  pthread_mutex_destroy(&lock->m);
  pthread_cond_destroy(&lock->cr);
  pthread_cond_destroy(&lock->cw);
  free(lock);

  return 0;
}

# endif

struct mzrt_mutex {
  pthread_mutex_t mutex;
};

int mzrt_mutex_create(mzrt_mutex **mutex) {
  *mutex = malloc(sizeof(struct mzrt_mutex));
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

struct mzrt_cond {
  pthread_cond_t cond;
};

int mzrt_cond_create(mzrt_cond **cond) {
  *cond = malloc(sizeof(struct mzrt_cond));
  return pthread_cond_init(&(*cond)->cond, NULL);
}

int mzrt_cond_wait(mzrt_cond *cond, mzrt_mutex *mutex) {
  return pthread_cond_wait(&cond->cond, &mutex->mutex);
}

int mzrt_cond_timedwait(mzrt_cond *cond, mzrt_mutex *mutex, intptr_t seconds, intptr_t nanoseconds) {
  struct timespec timeout;
  timeout.tv_sec  = seconds;
  timeout.tv_nsec = nanoseconds;
  return pthread_cond_timedwait(&cond->cond, &mutex->mutex, &timeout);
}

int mzrt_cond_signal(mzrt_cond *cond) {
  return pthread_cond_signal(&cond->cond);
}

int mzrt_cond_broadcast(mzrt_cond *cond) {
  return pthread_cond_broadcast(&cond->cond);
}

int mzrt_cond_destroy(mzrt_cond *cond) {
  return pthread_cond_destroy(&cond->cond);
}

struct mzrt_sema {
  int ready;
  pthread_mutex_t m;
  pthread_cond_t c;
};

int mzrt_sema_create(mzrt_sema **_s, int v)
{
  mzrt_sema *s;
  int err;

  s = (mzrt_sema *)malloc(sizeof(mzrt_sema));
  err = pthread_mutex_init(&s->m, NULL);
  if (err) { 
    free(s); 
    return err; 
  }
  err = pthread_cond_init(&s->c, NULL);
  if (err) { 
    pthread_mutex_destroy(&s->m);
    free(s); 
    return err; 
  }
  s->ready = v;
  *_s = s;

  return 0;
}

int mzrt_sema_wait(mzrt_sema *s)
{
  pthread_mutex_lock(&s->m);
  while (!s->ready) {
    pthread_cond_wait(&s->c, &s->m);
  }
  --s->ready;
  pthread_mutex_unlock(&s->m);

  return 0;
}

int mzrt_sema_post(mzrt_sema *s)
{
  pthread_mutex_lock(&s->m);
  s->ready++;
  pthread_cond_signal(&s->c);
  pthread_mutex_unlock(&s->m);

  return 0;
}

int mzrt_sema_destroy(mzrt_sema *s)
{
  pthread_mutex_destroy(&s->m);
  pthread_cond_destroy(&s->c);
  free(s);

  return 0;
}

#endif

/* Windows **************************************************************/

#ifdef WIN32

typedef struct mzrt_rwlock {
  HANDLE readEvent;
  HANDLE writeMutex;
  LONG readers;
};

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
  EnterCriticalSection(&mutex->critical_section);
  return 0;
}

int mzrt_mutex_trylock(mzrt_mutex *mutex) {
  /* FIXME: TryEnterCriticalSection() requires NT:
     if (!TryEnterCriticalSection(&mutex->critical_section))
       return 1; */
  return 0;
}

int mzrt_mutex_unlock(mzrt_mutex *mutex) {
  LeaveCriticalSection(&mutex->critical_section);
  return 0;
}

int mzrt_mutex_destroy(mzrt_mutex *mutex) {
  DeleteCriticalSection(&mutex->critical_section);
  return 0;
}

struct mzrt_cond {
  int nothing;
};

int mzrt_cond_create(mzrt_cond **cond) {
  return 0;
}

int mzrt_cond_wait(mzrt_cond *cond, mzrt_mutex *mutex) {
  return 0;
}

int mzrt_cond_timedwait(mzrt_cond *cond, mzrt_mutex *mutex, intptr_t secs, intptr_t nsecs) {
  return 0;
}

int mzrt_cond_signal(mzrt_cond *cond) {
  return 0;
}

int mzrt_cond_broadcast(mzrt_cond *cond) {
  return 0;
}

int mzrt_cond_destroy(mzrt_cond *cond) {
  return 0;
}

struct mzrt_sema {
  HANDLE ws;
};

int mzrt_sema_create(mzrt_sema **_s, int v)
{
  mzrt_sema *s;
  HANDLE ws;

  s = (mzrt_sema *)malloc(sizeof(mzrt_sema));
  ws = CreateSemaphore(NULL, v, 32000, NULL);
  s->ws = ws;
  *_s = s;

  return 0;
}

int mzrt_sema_wait(mzrt_sema *s)
{
  WaitForSingleObject(s->ws, INFINITE);
  return 0;
}

int mzrt_sema_post(mzrt_sema *s)
{
  ReleaseSemaphore(s->ws, 1, NULL);  
  return 0;
}

int mzrt_sema_destroy(mzrt_sema *s)
{
  CloseHandle(s->ws);
  free(s);

  return 0;
}

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#ifdef MZ_XFORM
END_XFORM_SUSPEND;
#endif

#endif
