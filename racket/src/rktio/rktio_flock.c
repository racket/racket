#include "rktio.h"
#include "rktio_private.h"
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#if defined(RKTIO_SYSTEM_UNIX) && !defined(RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS)
# define USE_FLOCK_FOR_FILE_LOCKS
#endif

#ifdef USE_FLOCK_FOR_FILE_LOCKS
#include <sys/file.h>
#endif

#if defined(RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS)
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
typedef struct pair_t { int car, cdr; } pair_t;
#endif

int rktio_file_lock_try(rktio_t *rktio, rktio_fd_t *rfd, int excl)
{
#ifdef RKTIO_SYSTEM_UNIX
# ifdef USE_FLOCK_FOR_FILE_LOCKS
  {
    intptr_t fd = rktio_fd_system_fd(rktio, rfd);
    int ok;

    do {
      ok = flock(fd, (excl ? LOCK_EX : LOCK_SH) | LOCK_NB);
    } while ((ok == -1) && (errno == EINTR));

    if (ok == 0)
      return RKTIO_LOCK_ACQUIRED;

    if (errno == EWOULDBLOCK)
      return 0;

    get_posix_error();
    return RKTIO_LOCK_ERROR;
  }
# elif defined(RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS)
  /* An lockf() is cancelled if *any* file descriptor to the same file
     is closed within the same process. We avoid that problem by forking
     a new process whose only job is to use lockf(). */
  {
    intptr_t fd = rktio_fd_system_fd(rktio, rfd);
    int ifds[2], ofds[2], cr;

    if (rktio->locked_fd_process_map)
      if (rktio_hash_get(rktio->locked_fd_process_map, fd))
        /* already have a lock */
        return RKTIO_LOCK_ACQUIRED;

    if (!pipe(ifds)) {
      if (!pipe(ofds)) {
        int pid;

#ifdef SUBPROCESS_USE_FORK1
        pid = fork1();
#else
        pid = fork();
#endif
        
        if (pid > 0) {
          /* Original process: */
          int errid = 0;

          rktio_reliably_close(ifds[1]);
          rktio_reliably_close(ofds[0]);

          do{
            cr = read(ifds[0], &errid, sizeof(int));
          } while ((cr == -1) && (errno == EINTR));
          if (cr == -1)
            errid = errno;

          rktio_reliably_close(ifds[0]);

          if (errid) {
            rktio_reliably_close(ofds[1]);
            
            if (errid == EAGAIN)
              return 0;
            else {
              errno = errid;
              get_posix_error();
              return RKTIO_LOCK_ERROR;
            }
          } else {
            pair_t *pr;
            
            /* got lock; record fd -> pipe mapping */
            if (!rktio->locked_fd_process_map) {
              rktio->locked_fd_process_map = rktio_hash_new();
            }

            pr = malloc(sizeof(pair_t));
            pr->car = ofds[1];
            pr->cdr = pid;
          
            rktio_hash_set(rktio->locked_fd_process_map, fd, pr);
            return RKTIO_LOCK_ACQUIRED;
          }
        } else if (!pid) {
          /* Child process */
          int ok = 0;
          struct flock fl;

          rktio_reliably_close(ifds[0]);
          rktio_reliably_close(ofds[1]);
          rktio_close_fds_after_fork(ifds[1], ofds[0], fd);
   
          fl.l_start = 0;
          fl.l_len = 0;
          fl.l_type = (excl ? F_WRLCK : F_RDLCK);
          fl.l_whence = SEEK_SET;
          fl.l_pid = getpid();

          if (!fcntl(fd, F_SETLK, &fl)) {
            /* report success: */
            do {
              cr = write(ifds[1], &ok, sizeof(int));
            } while ((cr == -1) && (errno == EINTR));
            /* wait until a signal to exit: */
            do {
              cr = read(ofds[0], &ok, sizeof(int));
            } while ((cr == -1) && (errno == EINTR));
          }

          if (!ok) {
            int errid = errno;
            do {
              cr = write(ifds[1], &errid, sizeof(int));
            } while ((cr == -1) && (errno == EINTR));
          }
          _exit(0);
        } else {
          /* Child process creation failed */
          get_posix_error();
          int i;
          for (i = 0; i < 2; i++) {
            rktio_reliably_close(ifds[i]);
            rktio_reliably_close(ofds[i]);
          }
          return RKTIO_LOCK_ERROR;
        }
      } else {
        /* Second pipe creation failed */
        int i;
        get_posix_error();
        for (i = 0; i < 2; i++) {
          rktio_reliably_close(ifds[i]);
        }
        return RKTIO_LOCK_ERROR;
      }
    } else {
      /* First pipe creation failed */
      get_posix_error();
      return RKTIO_LOCK_ERROR;
    }
  }
# else
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return RKTIO_LOCK_ERROR;
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    HANDLE fd = (HANDLE)rktio_fd_system_fd(rktio, rfd);
    OVERLAPPED o;
    int errid;

# define LOCK_ALL_FILE_LO 0
# define LOCK_ALL_FILE_HI 0x10000

    memset(&o, 0, sizeof(OVERLAPPED));
    if (LockFileEx(fd,
		   (LOCKFILE_FAIL_IMMEDIATELY
		    | (excl ? LOCKFILE_EXCLUSIVE_LOCK : 0)),
		   0, 
		   LOCK_ALL_FILE_LO, LOCK_ALL_FILE_HI,
		   &o))
      return RKTIO_LOCK_ACQUIRED;

    errid = GetLastError();
    if (errid == ERROR_LOCK_VIOLATION)
      return 0;
    else {
      get_windows_error();
      return RKTIO_LOCK_ERROR;
    }
  }
#endif
}

#ifdef RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS
void rktio_release_lockf(rktio_t *rktio, int fd)
{
  if (rktio->locked_fd_process_map) {
    pair_t *pr;
    pr = rktio_hash_get(rktio->locked_fd_process_map, fd);
    if (pr) {
      int fd2, pid, status;

      fd2 = pr->car;
      pid = pr->cdr;
      rktio_hash_remove(rktio->locked_fd_process_map, fd, 0);
      free(pr);

      rktio_reliably_close(fd2); /* makes the fork()ed process exit */
      waitpid(pid, &status, 0);
    }
  }
}
#endif

rktio_ok_t rktio_file_unlock(rktio_t *rktio, rktio_fd_t *rfd)
{
  intptr_t fd = rktio_fd_system_fd(rktio, rfd);
  int ok;

#ifdef RKTIO_SYSTEM_UNIX
# ifdef USE_FLOCK_FOR_FILE_LOCKS
  do {
    ok = flock(fd, LOCK_UN);
  } while ((ok == -1) && (errno == EINTR));
  ok = !ok;
  if (!ok) get_posix_error();
# elif defined(RKTIO_USE_FCNTL_AND_FORK_FOR_FILE_LOCKS)
  rktio_release_lockf(rktio, fd);
  ok = 1;
# else
  ok = 0;
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  ok = UnlockFile((HANDLE)fd, 0, 0, LOCK_ALL_FILE_LO, LOCK_ALL_FILE_HI);
  if (!ok)
    get_windows_error();
#endif

  return ok;
}
