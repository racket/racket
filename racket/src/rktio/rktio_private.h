#if ((defined(_MSC_VER) || defined(__MINGW32__))                        \
     && (defined(__WIN32__) || defined(WIN32) || defined(_WIN32)))
# define RKTIO_SYSTEM_WINDOWS
#else
# define RKTIO_SYSTEM_UNIX
#endif


#ifdef RKTIO_SYSTEM_WINDOWS
# include <windows.h>
#endif

#if RKTIO_SYSTEM_WINDOWS
# define USE_FAR_RKTIO_FDCALLS
#endif
#ifdef USE_DYNAMIC_FDSET_SIZE
# define USE_FAR_RKTIO_FDCALLS
#endif
#ifdef HAVE_POLL_SYSCALL
# define USE_FAR_RKTIO_FDCALLS
#endif

/*========================================================================*/
/* Globals, as gathered into `rktio_t`                                    */
/*========================================================================*/

struct rktio_t {
  intptr_t errid;
  int errkind;

#ifdef RKTIO_SYSTEM_UNIX
  struct group_member_cache_entry_t *group_member_cache;
  int external_event_fd;
  int put_external_event_fd;
  int long_term_poll_set_fd;
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  int windows_nt_or_later;
  HANDLE break_semaphore;
  int wsr_size = 0;
  struct rktio_socket_t *wsr_array;
#endif
#ifdef USE_FAR_RKTIO_FDCALLS
  /* A single fdset that can be reused for immediate actions: */
  struct rktio_poll_set_t *rktio_global_poll_set;
#endif

#if defined(RKTIO_SYSTEM_WINDOWS) || defined(RKTIO_USE_PTHREADS)
  int ghbn_started, ghbn_run;
  struct rktio_addr_lookup_t *ghbn_requests;
# ifdef RKTIO_USE_PTHREADS
  HANDLE ghbn_th;
  pthread_mutex_t ghbn_lock;
  pthread_cond_t ghbn_start;
# endif
# ifdef RKTIO_SYSTEM_WINDOWS
  pthread_t ghbn_th;
  HANDLE ghbn_lock;
  HANDLE ghbn_start;
# endif
#endif
};

/*========================================================================*/
/* Poll sets                                                              */
/*========================================================================*/

typedef struct rktio_poll_set_t rktio_poll_set_t;

void rktio_alloc_global_poll_set(rktio_t *rktio);
void rktio_free_global_poll_set(rktio_t *rktio);
int rktio_initialize_signal(rktio_t *rktio);

#ifdef USE_FAR_RKTIO_FDCALLS

rktio_poll_set_t *rktio_get_fdset(rktio_poll_set_t *fdarray, int pos);
void rktio_fdzero(rktio_poll_set_t *fd);
void rktio_fdset(rktio_poll_set_t *fd, int n);
void rktio_fdclr(rktio_poll_set_t *fd, int n);
int rktio_fdisset(rktio_poll_set_t *fd, int n);
  
# define DECL_FDSET(n, c) fd_set *n
# define INIT_DECL_FDSET(r, w, e) { \
   r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 0 ); \
   w = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 1 ); \
   e = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 2 ); \
 }
# define INIT_DECL_RD_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 0 )
# define INIT_DECL_WR_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 1 )
# define INIT_DECL_ER_FDSET(r) r = RKTIO_GET_FDSET(rktio->rktio_global_poll_set, 2 )

# define RKTIO_GET_FDSET(p, n) rktio_get_fdset(p, n)
# define RKTIO_FD_ZERO(p) rktio_fdzero(p)
# define RKTIO_FD_SET(n, p) rktio_fdset(p, n)
# define RKTIO_FD_CLR(n, p) rktio_fdclr(p, n)
# define RKTIO_FD_ISSET(n, p) rktio_fdisset(p, n)

# if !defined(HAVE_POLL_SYSCALL) && !defined(RKTIO_SYSTEM_WINDOWS)
#  define RKTIO_FDS(p) ((fd_set *)fds)
# endif

#else

#include <sys/select.h>
struct rktio_poll_set_t { fd_set data; };

# define DECL_FDSET(n, c) rktio_poll_set_t n[c]
# define INIT_DECL_FDSET(r, w, e) /* empty */
# define INIT_DECL_RD_FDSET(r) /* empty */
# define INIT_DECL_WR_FDSET(r) /* empty */
# define INIT_DECL_ER_FDSET(r) /* empty */

# define RKTIO_FDS(p) (&(p)->data)

# define RKTIO_GET_FDSET(p, n) ((p)+(n))
# define RKTIO_FD_ZERO(p) FD_ZERO(RKTIO_FDS(p))
# define RKTIO_FD_SET(n, p) FD_SET(n, RKTIO_FDS(p))
# define RKTIO_FD_CLR(n, p) FD_CLR(n, RKTIO_FDS(p))
# define RKTIO_FD_ISSET(n, p) FD_ISSET(n, RKTIO_FDS(p))

#endif

void rktio_merge_fd_sets(rktio_poll_set_t *fds, rktio_poll_set_t *src_fds);
void rktio_clean_fd_set(rktio_poll_set_t *fds);
int rktio_get_fd_limit(rktio_poll_set_t *fds);

#if defined(HAVE_KQUEUE_SYSCALL) || defined(HAVE_EPOLL_SYSCALL)
int rktio_ltps_get_fd(rktio_ltps_t *lt);
#else
rktio_poll_set_t *rktio_ltps_get_fd_set(rktio_ltps_t *lt); 
#endif

#if defined(HAVE_POLL_SYSCALL)
int rktio_get_poll_count(rktio_poll_set_t *fds);
struct pollfd *rktio_get_poll_fd_array(rktio_poll_set_t *fds);
#endif

/*========================================================================*/
/* Network                                                                */
/*========================================================================*/

int rktio_socket_close(rktio_t *rktio, rktio_fd_t *rfd);

int rktio_socket_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd);
int rktio_socket_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd);

intptr_t rktio_socket_write(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len);
intptr_t rktio_socket_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len);
  
void rktio_free_ghbn(rktio_t *rktio);

const char *rktio_gai_strerror(int errnum);
  
/*========================================================================*/
/* Misc                                                                   */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS
# define MSC_IZE(n) _ ## n
# define MSC_W_IZE(n) _w ## n
# define MSC_WIDE_PATH_temp(n) WIDE_PATH_temp(n)
#else
# define MSC_IZE(n) n
# define MSC_W_IZE(n) MSC_IZE(n)
# define MSC_WIDE_PATH_temp(n) n
#endif

void rktio_get_posix_error(rktio_t *rktio);
#define get_posix_error() rktio_get_posix_error(rktio)

void rktio_set_racket_error(rktio_t *rktio, int errid);
#define set_racket_error(e) rktio_set_racket_error(rktio, e)

#ifdef RKTIO_SYSTEM_WINDOWS
void rktio_get_windows_error(rktio_t *rktio);
# define get_windows_error() rktio_get_windows_error(rktio)
#endif

#if defined(USE_FCNTL_O_NONBLOCK)
# define RKTIO_NONBLOCKING O_NONBLOCK
#else
# define RKTIO_NONBLOCKING FNDELAY
#endif
