#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#ifdef HAVE_POLL_SYSCALL
# include <poll.h>
#endif

#ifdef RKTIO_SYSTEM_UNIX
# include <unistd.h>
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# include <sys/types.h>
# include <fcntl.h>
# include <errno.h>
# define TCP_SOCKSENDBUF_SIZE 32768
# define NOT_WINSOCK(x) (x)
# define SOCK_ERRNO() errno
# define WAS_EAGAIN(e) ((e == EWOULDBLOCK) || (e == EAGAIN) || (e == EINPROGRESS) || (e == EALREADY))
# define WAS_ECONNREFUSED(e) (e == ECONNREFUSED)
# define WAS_EBADADDRESS(e) (e == EINVAL)
# define WAS_WSAEMSGSIZE(e) 0
# define RKTIO_AFNOSUPPORT EAFNOSUPPORT

#define RKTIO_SHUT_RD SHUT_RD
#define RKTIO_SHUT_WR SHUT_WR

#define RKTIO_SOCKS(s) s

typedef intptr_t rktio_socket_t;
typedef unsigned int rktio_sockopt_len_t;

# define INVALID_SOCKET (-1)

static int closesocket(rktio_socket_t s) {
  return rktio_reliably_close_err(s);
}

typedef struct sockaddr_in rktio_unspec_address;
#define REGISTER_SOCKET(s) /**/
#define UNREGISTER_SOCKET(s) /**/

#endif

#ifdef CANT_SET_SOCKET_BUFSIZE
# undef SET_SOCKET_BUFFSIZE_ON_CREATE
#endif

#ifdef SET_SOCKET_BUFFSIZE_ON_CREATE
# define RKTIO_WHEN_SET_SOCKBUF_SIZE(x) x
#else
# define RKTIO_WHEN_SET_SOCKBUF_SIZE(x) /* empty */
#endif

#ifdef RKTIO_SYSTEM_WINDOWS
# include <process.h>
# include <winsock2.h>
# include <ws2tcpip.h>
# ifndef __MINGW32__
#  include <wspiapi.h>
# else
typedef int (WINAPI*gai_t)(const char*, const char*, const struct rktio_addrinfo_t *, struct rktio_addrinfo_t **);
typedef void (WINAPI*fai_t)(struct rktio_addrinfo_t *ai);
# endif
struct SOCKADDR_IN {
  short sin_family;
  unsigned short sin_port;
  struct in_addr sin_addr;
  char sin_zero[8];
};
# define NOT_WINSOCK(x) 0
# define SOCK_ERRNO() WSAGetLastError()
# define WAS_EAGAIN(e) ((e == WSAEWOULDBLOCK) || (e == WSAEINPROGRESS))
# define WAS_WSAEMSGSIZE(e) (e == WSAEMSGSIZE)
# define WAS_ECONNREFUSED(e) (e == WSAECONNREFUSED)
# define WAS_EBADADDRESS(e) 0
# define RKTIO_AFNOSUPPORT WSAEAFNOSUPPORT

#define RKTIO_SHUT_RD SD_RECEIVE
#define RKTIO_SHUT_WR SD_SEND

typedef SOCKET rktio_socket_t;
typedef int rktio_sockopt_len_t;

/* Avoid warnings, since select() first argument is ignored: */
#define RKTIO_SOCKS(s) 0

typedef struct SOCKADDR_IN rktio_unspec_address;
# define REGISTER_SOCKET(s) winsock_remember(s)
# define UNREGISTER_SOCKET(s) winsock_forget(s)

# ifdef errno
#  undef errno
# endif
# define errno [do not use errno with winsock]

#endif

static void do_get_socket_error(rktio_t *rktio) {
  rktio->errid = SOCK_ERRNO();
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio->errkind = RKTIO_ERROR_KIND_WINDOWS;
#else
  rktio->errkind = RKTIO_ERROR_KIND_POSIX;
#endif
}
#define get_socket_error() do_get_socket_error(rktio)

static void do_set_socket_error(rktio_t *rktio, int errid) {
  rktio->errid = errid;
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio->errkind = RKTIO_ERROR_KIND_WINDOWS;
#else
  rktio->errkind = RKTIO_ERROR_KIND_POSIX;
#endif
}
#define set_socket_error(errid) do_set_socket_error(rktio, errid)

static void do_set_gai_error(rktio_t *rktio, int errid) {
  rktio->errid = errid;
  rktio->errkind = RKTIO_ERROR_KIND_GAI;
}
#define set_gai_error(err) do_set_gai_error(rktio, err)

#define TCP_BUFFER_SIZE 4096

struct rktio_udp_t {
  rktio_socket_t s;
  char bound, connected;
};


#ifdef RKTIO_SYSTEM_WINDOWS
# define DECL_SOCK_FDSET(n) fd_set n[1]
# define INIT_DECL_SOCK_FDSET(r, w, e) /* empty */
# define INIT_DECL_SOCK_RD_FDSET(r) /* empty */
# define INIT_DECL_SOCK_WR_FDSET(r) /* empty */
# define INIT_DECL_SOCK_ER_FDSET(r) /* empty */
# define RKTIO_SOCK_FD_ZERO(p) FD_ZERO(p)
# define RKTIO_SOCK_FD_SET(n, p) FD_SET(n, p)
# define RKTIO_SOCK_FD_CLR(n, p) FD_CLR(n, p)
# define RKTIO_SOCK_FD_ISSET(n, p) FD_ISSET(n, p)
# define RKTIO_SOCK_FDS(p) (p)
#else
# define DECL_SOCK_FDSET(n) DECL_FDSET(n, 1)
# define INIT_DECL_SOCK_FDSET(r, w, e) INIT_DECL_FDSET(r, w, e)
# define INIT_DECL_SOCK_RD_FDSET(r) INIT_DECL_RD_FDSET(r)
# define INIT_DECL_SOCK_WR_FDSET(r) INIT_DECL_WR_FDSET(r)
# define INIT_DECL_SOCK_ER_FDSET(r) INIT_DECL_ER_FDSET(r)
# define RKTIO_SOCK_FD_ZERO(p) RKTIO_FD_ZERO(p)
# define RKTIO_SOCK_FD_SET(n, p) RKTIO_FD_SET(n, p)
# define RKTIO_SOCK_FD_CLR(n, p) RKTIO_FD_CLR(n, p)
# define RKTIO_SOCK_FD_ISSET(n, p) RKTIO_FD_ISSET(n, p)
# define RKTIO_SOCK_FDS(p) RKTIO_FDS(p)
#endif

/*========================================================================*/
/* Host address lookup, including asynchronous-lookup support             */
/*========================================================================*/

# ifdef PROTOENT_IS_INT
#  define PROTO_P_PROTO PROTOENT_IS_INT
# else
static struct protoent *proto;
#  define PROTO_P_PROTO (proto ? proto->p_proto : 0)
# endif

# ifndef RKTIO_PF_INET
#  define RKTIO_PF_INET PF_INET
# endif

/* For getting connection names: */
#define RKTIO_SOCK_NAME_MAX_LEN 256
#define RKTIO_SOCK_HOST_NAME_MAX_LEN 64
#define RKTIO_SOCK_SVC_NAME_MAX_LEN 32

#if defined(HAVE_GETADDRINFO) || defined(__MINGW32__)
struct rktio_addrinfo_t {
  struct addrinfo ai;
};
# define RKTIO_AS_ADDRINFO(x) (&(x)->ai)
# define RKTIO_AS_ADDRINFO_PTR(xp) ((struct addrinfo **)(xp))
#else
struct rktio_addrinfo_t {
  int ai_flags;
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  size_t  ai_addrlen;
  struct sockaddr *ai_addr; 
  struct rktio_addrinfo_t *ai_next;
};
# define RKTIO_AS_ADDRINFO(x) x
# define RKTIO_AS_ADDRINFO_PTR(x) x
#endif

#if defined(__MINGW32__) && !defined(HAVE_GETADDRINFO)
/* Although `configure` didn't discover it, we do have getaddrinfo()
   from Winsock */
# define HAVE_GETADDRINFO
#endif

/*****************************************************************/
/* Fallback using gethostbyname where getddrinfo isn't available */

#ifdef HAVE_GETADDRINFO
# define rktio_AI_PASSIVE AI_PASSIVE 
# define do_getaddrinfo(n, s, h, res) getaddrinfo(n, s, RKTIO_AS_ADDRINFO(h), RKTIO_AS_ADDRINFO_PTR(res))
# define do_freeaddrinfo freeaddrinfo
# ifdef RKTIO_SYSTEM_WINDOWS
#  define do_gai_strerror gai_strerrorA
# else
#  define do_gai_strerror gai_strerror
# endif
#else
# define rktio_AI_PASSIVE 0
static int do_getaddrinfo(const char *nodename, const char *servname,
			  const struct rktio_addrinfo_t *hints, struct rktio_addrinfo_t **res)
{
  struct hostent *h;

  if (nodename)
    h = gethostbyname(nodename);
  else
    h = NULL;

  if (h || !nodename) {
    struct rktio_addrinfo_t *ai;
    struct sockaddr_in *sa;
    int j, id = 0;

    ai = malloc(sizeof(struct rktio_addrinfo_t));
    sa = malloc(sizeof(struct sockaddr_in));
    ai->ai_addr = (struct sockaddr *)sa;

    ai->ai_addrlen = sizeof(struct sockaddr_in);
    if (servname) {
      for (j = 0; servname[j]; j++) {
	id = (id * 10) + (servname[j] - '0');
      }
    }

    ai->ai_family = RKTIO_PF_INET;
    ai->ai_socktype = hints->ai_socktype;
    ai->ai_protocol = hints->ai_protocol;
    ai->ai_next = NULL;

    sa->sin_family = (id ? AF_INET : AF_UNSPEC);
    j = htons(id);
    sa->sin_port = j;
    memset(&(sa->sin_addr), 0, sizeof(sa->sin_addr));
    memset(&(sa->sin_zero), 0, sizeof(sa->sin_zero));
    if (h)
      memcpy(&sa->sin_addr, h->h_addr_list[0], h->h_length); 
    
    *res = ai;
    return 0;
  }
  return h_errno;
}

void do_freeaddrinfo(struct rktio_addrinfo_t *ai)
{
  free(ai->ai_addr);
  free(ai);
}

const char *do_gai_strerror(int ecode)
{
  return hstrerror(ecode);
}
#endif

/*******************************************************************/
/* Running getddrinfo in a separate thread to make it asynchronous */

struct rktio_addrinfo_lookup_t {
  int mode;

  char *name, *svc;
  struct rktio_addrinfo_t *hints;
  
#if defined(RKTIO_SYSTEM_WINDOWS) || defined(RKTIO_USE_PTHREADS)
  /* For delivering the result: */
  struct rktio_addrinfo_t *result;
  int err;

  /* For signaling that the result is ready: */
# ifdef RKTIO_SYSTEM_WINDOWS
  HANDLE done_sema;
# else
  int done_fd[2];
# endif

  /* For chaining active requests: */
  struct rktio_addrinfo_lookup_t *next;
#endif
};

static void free_lookup(rktio_addrinfo_lookup_t *lookup)
{
#if defined(RKTIO_SYSTEM_WINDOWS) || defined(RKTIO_USE_PTHREADS)
  if (lookup->result)
    do_freeaddrinfo(RKTIO_AS_ADDRINFO(lookup->result));
#endif

  if (lookup->name)
    free(lookup->name);
  if (lookup->svc)
    free(lookup->svc);
  free(lookup->hints);
  free(lookup);
}

static void init_lookup(rktio_addrinfo_lookup_t *lookup)
{
#if defined(RKTIO_SYSTEM_WINDOWS) || defined(RKTIO_USE_PTHREADS)
  lookup->result = NULL;
#endif
}

#if defined(RKTIO_SYSTEM_WINDOWS) || defined(RKTIO_USE_PTHREADS)

# ifdef RKTIO_SYSTEM_WINDOWS
#  ifdef __BORLANDC__
#   define RKTIO_LPTHREAD_START_ROUTINE unsigned int (__stdcall*)(void*)
#  else
#   define RKTIO_LPTHREAD_START_ROUTINE LPTHREAD_START_ROUTINE
#  endif
# else
#  include <pthread.h>
#   define RKTIO_LPTHREAD_START_ROUTINE void *(*)(void *)
# endif

#define GHBN_WAIT        1
#define GHBN_DONE        2
#define GHBN_ABANDONED   3

# ifdef RKTIO_SYSTEM_WINDOWS

static void ghbn_lock(rktio_t *rktio)
{
  WaitForSingleObject(rktio->ghbn_lock, INFINITE);
}

static void ghbn_unlock(rktio_t *rktio)
{
  ReleaseSemaphore(rktio->ghbn_lock, 1, NULL);
}

static void ghbn_wait(rktio_t *rktio)
{
  ghbn_unlock(rktio);
  WaitForSingleObject(rktio->ghbn_start, INFINITE);
  ghbn_lock(rktio);
}

static void ghbn_signal(rktio_t *rktio)
{
  ReleaseSemaphore(rktio->ghbn_start, 1, NULL);
}

static void ghbn_wait_exit(rktio_t *rktio)
{
  WaitForSingleObject(rktio->ghbn_th, INFINITE);
}

# else

static void ghbn_lock(rktio_t *rktio)
{
  pthread_mutex_lock(&rktio->ghbn_lock);
}

static void ghbn_unlock(rktio_t *rktio)
{
  pthread_mutex_unlock(&rktio->ghbn_lock);
}

static void ghbn_wait(rktio_t *rktio)
{
  pthread_cond_wait(&rktio->ghbn_start, &rktio->ghbn_lock);
}

static void ghbn_signal(rktio_t *rktio)
{
  pthread_cond_signal(&rktio->ghbn_start);
}

static void ghbn_wait_exit(rktio_t *rktio)
{
  pthread_join(rktio->ghbn_th, NULL);
}

# endif

static intptr_t getaddrinfo_in_thread(void *_data)
{
  rktio_t *rktio = (rktio_t *)_data;
  rktio_addrinfo_lookup_t *lookup;
  rktio_addrinfo_t *result;
  int err;

  ghbn_lock(rktio);
  while (rktio->ghbn_run) {
    lookup = rktio->ghbn_requests;
    if (lookup) {
      rktio->ghbn_requests = lookup->next;
      ghbn_unlock(rktio);

      /* Handle one lookup request: */
      err = do_getaddrinfo(lookup->name, lookup->svc, lookup->hints, &result);
      lookup->err = err;
      if (!err)
        lookup->result = result;

      ghbn_lock(rktio);

# ifdef RKTIO_SYSTEM_WINDOWS
      ReleaseSemaphore(lookup->done_sema, 1, NULL);  
# else
      {
        long v = 1;
        do {
          err = write(lookup->done_fd[1], &v, sizeof(v));
        } while ((err == -1) && (errno == EINTR));
        rktio_reliably_close(lookup->done_fd[1]);
      }
# endif

      if (lookup->mode == GHBN_ABANDONED) {
# ifdef RKTIO_SYSTEM_WINDOWS
        CloseHandle(lookup->done_sema);
# else
        rktio_reliably_close(lookup->done_fd[0]);
# endif
        free_lookup(lookup);
      }
    } else {
      ghbn_wait(rktio);
    }
  }

  ghbn_unlock(rktio);
  
  return 0;
}

# ifdef RKTIO_SYSTEM_WINDOWS

static unsigned int WINAPI win_getaddrinfo_in_thread(void *_data)
{
  return (unsigned int)getaddrinfo_in_thread(_data);
}

static int ghbn_init(rktio_t *rktio)
{
  rktio->ghbn_lock = CreateSemaphore(NULL, 1, 1, NULL);
  if (!rktio->ghbn_lock) {
    get_windows_error();
    return 0;
  }
  rktio->ghbn_start = CreateSemaphore(NULL, 0, 1, NULL);
  if (!rktio->ghbn_start) {
    get_windows_error();
    return 0;
  }
  rktio->ghbn_th = (HANDLE)_beginthreadex(NULL, 5000, 
                                          win_getaddrinfo_in_thread,
                                          rktio, 0, NULL);
  if (rktio->ghbn_th == INVALID_HANDLE_VALUE) {
    get_posix_error();
    return 0;
  }
  
  return 1;
}

# else

static int ghbn_init(rktio_t *rktio)
{
  if (pthread_mutex_init(&rktio->ghbn_lock, NULL)) {
    get_posix_error();
    return 0;
  }
  if (pthread_cond_init(&rktio->ghbn_start, NULL)) {
    get_posix_error();
    return 0;
  }
  if (pthread_create(&rktio->ghbn_th, NULL, 
                     (RKTIO_LPTHREAD_START_ROUTINE)getaddrinfo_in_thread,
                     rktio)) {
    return 0;
  }
  return 1;
}

# endif

void rktio_free_ghbn(rktio_t *rktio)
{
  if (rktio->ghbn_started) {
    ghbn_lock(rktio);
    rktio->ghbn_run = 0;
    ghbn_signal(rktio);
    ghbn_unlock(rktio);
    ghbn_wait_exit(rktio);
  }
}

static rktio_addrinfo_lookup_t *start_lookup(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  lookup->mode = GHBN_WAIT;
  
  if (!rktio->ghbn_started) {
    rktio->ghbn_run = 1;
    if (!ghbn_init(rktio))
      return NULL;
    rktio->ghbn_started = 1;
  }

# ifdef RKTIO_SYSTEM_WINDOWS
  {
    lookup->done_sema = CreateSemaphore(NULL, 0, 1, NULL);
    if (!lookup->done_sema) {
      get_windows_error();
      free_lookup(lookup);
      return NULL;
    }
  }
# else
  if (pipe(lookup->done_fd)) {
    get_posix_error();
    free_lookup(lookup);
    return NULL;
  } else {
    fcntl(lookup->done_fd[0], F_SETFL, RKTIO_NONBLOCKING);
  }
# endif

  ghbn_lock(rktio);
  lookup->next = rktio->ghbn_requests;
  rktio->ghbn_requests = lookup;
  ghbn_signal(rktio);
  ghbn_unlock(rktio);

  return lookup;
}

int rktio_poll_addrinfo_lookup_ready(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  int done = 0;
  
  ghbn_lock(rktio);

  if (lookup->mode == GHBN_DONE) {
    ghbn_unlock(rktio);
    return RKTIO_POLL_READY;
  }

# ifdef RKTIO_SYSTEM_WINDOWS
  if (WaitForSingleObject(lookup->done_sema, 0) == WAIT_OBJECT_0) {
    CloseHandle(lookup->done_sema);
    done = 1;
  }
# else
  {
    long v;
    int cr;
    do {
      cr = read(lookup->done_fd[0], &v, sizeof(long));
    } while ((cr == -1) && (errno == EINTR));
    if (cr > 0) {
      rktio_reliably_close(lookup->done_fd[0]);
      done = 1;
    }
  }
# endif

  if (done)
    lookup->mode = GHBN_DONE;

  ghbn_unlock(rktio);

  return (done ? RKTIO_POLL_READY : 0);
}

void rktio_poll_add_addrinfo_lookup(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup, rktio_poll_set_t *fds)
{
  ghbn_lock(rktio);
  
  if (lookup->mode != GHBN_WAIT) {
    ghbn_unlock(rktio);
    rktio_poll_set_add_nosleep(rktio, fds);
    return;
  }
  
  ghbn_unlock(rktio);

# ifdef RKTIO_SYSTEM_WINDOWS
  rktio_poll_set_add_handle(rktio, (intptr_t)lookup->done_sema, fds, 1);
# else
  {
    rktio_poll_set_t *fds2;
    
    fds2 = RKTIO_GET_FDSET(fds, 2);
    
    RKTIO_FD_SET(lookup->done_fd[0], fds);
    RKTIO_FD_SET(lookup->done_fd[0], fds2);
  }
# endif
}

void rktio_addrinfo_lookup_stop(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  ghbn_lock(rktio);
  if (lookup->mode != GHBN_DONE) {
    lookup->mode = GHBN_ABANDONED;
    ghbn_unlock(rktio);
  } else {
    ghbn_unlock(rktio);
# ifdef RKTIO_SYSTEM_WINDOWS
    CloseHandle(lookup->done_sema);
# else
    rktio_reliably_close(lookup->done_fd[0]);
# endif
    free_lookup(lookup);
  }
}

rktio_addrinfo_t *rktio_addrinfo_lookup_get(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  rktio_addrinfo_t *addr = NULL;

  if (lookup->err)
    set_gai_error(lookup->err);
  else {
    addr = lookup->result;
    lookup->result = NULL; /* so it's not freed */
  }

  free_lookup(lookup);

  return addr;
}

#else

void rktio_free_ghbn(rktio_t *rktio)
{
}

static rktio_addrinfo_lookup_t *start_lookup(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  return lookup;
}

int rktio_poll_addrinfo_lookup_ready(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  return RKTIO_POLL_READY;
}

void rktio_poll_add_addrinfo_lookup(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup, rktio_poll_set_t *fds)
{
  rktio_poll_set_add_nosleep(rktio, fds);
}

void rktio_addrinfo_lookup_stop(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  free_lookup(lookup);
}

rktio_addrinfo_t *rktio_addrinfo_lookup_get(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup)
{
  int err;
  rktio_addrinfo_t *result;

  err = do_getaddrinfo(lookup->name, lookup->svc, lookup->hints, &result);
  if (err)
    set_gai_error(err);
  
  free_lookup(lookup);

  if (err)
    return NULL;
  else
    return result;
}

#endif

int rktio_get_ipv4_family(rktio_t *rktio)
{
  return RKTIO_PF_INET;
}

rktio_addrinfo_lookup_t *rktio_start_addrinfo_lookup(rktio_t *rktio,
                                                     const char *hostname, int portno,
                                                     int family, int passive, int tcp)
{
  rktio_addrinfo_lookup_t *lookup;
  char buf[32], *service;
  struct rktio_addrinfo_t *hints;

  if (portno >= 0) {
    service = buf;
    sprintf(buf, "%d", portno);
  } else
    service = NULL;
  
  if (!hostname && !service) {
    set_racket_error(RKTIO_ERROR_HOST_AND_PORT_BOTH_UNSPECIFIED);
    return NULL;
  }

  hints = malloc(sizeof(rktio_addrinfo_t));
  memset(hints, 0, sizeof(struct rktio_addrinfo_t));
  RKTIO_AS_ADDRINFO(hints)->ai_family = ((family < 0) ? PF_UNSPEC : family);
  if (passive) {
    RKTIO_AS_ADDRINFO(hints)->ai_flags |= rktio_AI_PASSIVE;
  }
  if (tcp) {
    RKTIO_AS_ADDRINFO(hints)->ai_socktype = SOCK_STREAM;
# ifndef PROTOENT_IS_INT
    if (!proto) {
      proto = getprotobyname("tcp");
    }
# endif
    RKTIO_AS_ADDRINFO(hints)->ai_protocol= PROTO_P_PROTO;
  } else {
    RKTIO_AS_ADDRINFO(hints)->ai_socktype = SOCK_DGRAM;
  }

  lookup = malloc(sizeof(rktio_addrinfo_lookup_t));
  lookup->name = (hostname ? MSC_IZE(strdup)(hostname) : NULL);
  lookup->svc = (service ? MSC_IZE(strdup)(service) : NULL);
  lookup->hints = hints;
  init_lookup(lookup);
 
  return start_lookup(rktio, lookup);
}

void rktio_addrinfo_free(rktio_t *rktio, rktio_addrinfo_t *a)
{
  do_freeaddrinfo(RKTIO_AS_ADDRINFO(a));
}

const char *rktio_gai_strerror(int errnum)
{
  return do_gai_strerror(errnum);
}

/*========================================================================*/
/* Winsock management                                                     */
/*========================================================================*/

#ifdef RKTIO_SYSTEM_WINDOWS

static HANDLE winsock_sema;
static int winsock_started = 0;
static int wsr_size = 0;
static rktio_socket_t *wsr_array;

static void winsock_remember(rktio_socket_t s)
{
  int i, new_size;
  rktio_socket_t *naya;

  WaitForSingleObject(winsock_sema, INFINITE);

  for (i = 0; i < wsr_size; i++) {
    if (!wsr_array[i]) {
      wsr_array[i] = s;
      break;
    }
  }

  if (i >= wsr_size) {
    if (!wsr_size) {
      new_size = 32;
    } else
      new_size = 2 * wsr_size;

    naya = malloc(sizeof(rktio_socket_t) * new_size);
    for (i = 0; i < wsr_size; i++) {
      naya[i] = wsr_array[i];
    }

    naya[wsr_size] = s;

    if (wsr_array) free(wsr_array);

    wsr_array = naya;
    wsr_size = new_size;
  }  

  ReleaseSemaphore(winsock_sema, 1, NULL);
}

static void winsock_forget(rktio_socket_t s)
{
  int i;

  WaitForSingleObject(winsock_sema, INFINITE);

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i] == s) {
      wsr_array[i] = (rktio_socket_t)NULL;
      break;
    }
  }

  ReleaseSemaphore(winsock_sema, 1, NULL);
}

int rktio_winsock_init(rktio_t *rktio)
{
  if (!winsock_sema) {
    winsock_sema = CreateSemaphore(NULL, 1, 1, NULL);
  }

  WaitForSingleObject(winsock_sema, INFINITE);

  if (!winsock_started) {
    WSADATA data;
    if (!WSAStartup(MAKEWORD(1, 1), &data)) {
      winsock_started = 1;
    } else {
      get_windows_error();
      ReleaseSemaphore(winsock_sema, 1, NULL);
      return 0;
    }
  } else
    winsock_started++;
  
  ReleaseSemaphore(winsock_sema, 1, NULL);

  return 1;
}

void rktio_winsock_done(rktio_t *rktio)
{
  int i;

  WaitForSingleObject(winsock_sema, INFINITE);

  if (!--winsock_started) {
    for (i = 0; i < wsr_size; i++) {
      if (wsr_array[i]) {
        closesocket(wsr_array[i]);
        wsr_array[i] = (rktio_socket_t)NULL;
      }
    }

    wsr_size = 0;
    wsr_array = NULL;

    WSACleanup();
  }

  ReleaseSemaphore(winsock_sema, 1, NULL);
}
#endif

/*========================================================================*/
/* TCP sockets                                                            */
/*========================================================================*/

rktio_socket_t rktio_fd_socket(rktio_t *rktio, rktio_fd_t *rfd)
{
  return (rktio_socket_t)rktio_fd_system_fd(rktio, rfd);
}

void rktio_socket_init(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);

#ifdef RKTIO_SYSTEM_UNIX
  fcntl(s, F_SETFL, RKTIO_NONBLOCKING);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    unsigned long ioarg = 1;
    ioctlsocket(s, FIONBIO, &ioarg);
  }
#endif

  if (rktio_fd_is_udp(rktio, rfd)) {
#ifdef RKTIO_SYSTEM_UNIX
# ifdef SO_BROADCAST
    {
      int bc = 1;
      setsockopt(s, SOL_SOCKET, SO_BROADCAST, &bc, sizeof(bc));
    }
# endif
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
    {
      BOOL bc = 1;
      setsockopt(s, SOL_SOCKET, SO_BROADCAST, (char *)(&bc), sizeof(BOOL));
    }
#endif
  }
}

int rktio_socket_close(rktio_t *rktio, rktio_fd_t *rfd, int set_error)
{
#ifdef RKTIO_SYSTEM_UNIX
  if (set_error)
    return rktio_close(rktio, rfd);
  else {
    rktio_close_noerr(rktio, rfd);
    return 1;
  }
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  int err;
  UNREGISTER_SOCKET(s);
  err = closesocket(s);
  if (!err)
    get_socket_error();
  free(rfd);

  return !err;
#endif
}

void rktio_socket_own(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  REGISTER_SOCKET(s);
#endif
}

void rktio_socket_forget_owned(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  UNREGISTER_SOCKET(s);
#endif
}

int rktio_socket_shutdown(rktio_t *rktio, rktio_fd_t *rfd, int mode)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  
  if (shutdown(s, ((mode == RKTIO_SHUTDOWN_READ) ? RKTIO_SHUT_RD : RKTIO_SHUT_WR))) {
    get_socket_error();
    return 0;
  }
  
  return 1;
}

int rktio_socket_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return rktio_poll_write_ready(rktio, rfd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    rktio_socket_t s = rktio_fd_socket(rktio, rfd);
    DECL_SOCK_FDSET(writefds);
    DECL_SOCK_FDSET(exnfds);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_SOCK_WR_FDSET(writefds);
    INIT_DECL_SOCK_ER_FDSET(exnfds);
    
    RKTIO_SOCK_FD_ZERO(writefds);
    RKTIO_SOCK_FD_SET(s, writefds);
    RKTIO_SOCK_FD_ZERO(exnfds);
    RKTIO_SOCK_FD_SET(s, exnfds);
    
    sr = select(RKTIO_SOCKS(s + 1), NULL, writefds, exnfds, &time);

    if (sr == -1) {
      get_socket_error();
      return RKTIO_POLL_ERROR;
    } else if (sr)
      return RKTIO_POLL_READY;
    else
      return 0;
  }
#endif
}

int rktio_socket_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return rktio_poll_read_ready(rktio, rfd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    rktio_socket_t s = rktio_fd_socket(rktio, rfd);
    DECL_SOCK_FDSET(readfds);
    DECL_SOCK_FDSET(exnfds);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_SOCK_WR_FDSET(readfds);
    INIT_DECL_SOCK_ER_FDSET(exnfds);
    
    RKTIO_SOCK_FD_ZERO(readfds);
    RKTIO_SOCK_FD_SET(s, readfds);
    RKTIO_SOCK_FD_ZERO(exnfds);
    RKTIO_SOCK_FD_SET(s, exnfds);
    
    sr = select(RKTIO_SOCKS(s + 1), readfds, NULL, exnfds, &time);
    
    if (sr == -1) {
      get_socket_error();
      return RKTIO_POLL_ERROR;
    } else if (sr)
      return RKTIO_POLL_READY;
    else
      return 0;
  }
#endif
}

rktio_fd_t *rktio_socket_dup(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return rktio_dup(rktio, rfd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  rktio_socket_t nsocket;
  intptr_t rc;
  WSAPROTOCOL_INFO protocolInfo;
  rc = WSADuplicateSocket(s, GetCurrentProcessId(), &protocolInfo);
  if (rc) {
    get_socket_error();
    return NULL;
  }
  nsocket = WSASocket(FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, &protocolInfo, 0, WSA_FLAG_OVERLAPPED);
  if (nsocket == INVALID_SOCKET) {
    get_socket_error();
    return NULL;
  }
  return rktio_system_fd(rktio, nsocket, rktio_fd_modes(rktio, rfd) | RKTIO_OPEN_OWN);
#endif
}

intptr_t rktio_socket_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  int rn;
  
  do {
    rn = recv(s, buffer, len, 0);
  } while ((rn == -1) && NOT_WINSOCK(errno == EINTR));

  if (rn > 0)
    return rn;
  else if (rn == 0)
    return RKTIO_READ_EOF;
  else {
    int err = SOCK_ERRNO();
    if (WAS_EAGAIN(err))
      return 0;
    else {
      get_socket_error();
      return RKTIO_READ_ERROR;
    }
  }
}

static intptr_t do_socket_write(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t len,
                                /* for UDP sendto: */
                                rktio_addrinfo_t *addr)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  intptr_t sent;
  int errid = 0;

#ifdef RKTIO_SYSTEM_WINDOWS
# define SEND_BAD_MSG_SIZE(e) (e == WSAEMSGSIZE)
#else    
# ifdef SEND_IS_NEVER_TOO_BIG
#  define SEND_BAD_MSG_SIZE(errid) 0
# else
#  define SEND_BAD_MSG_SIZE(errid) (errid == EMSGSIZE)
# endif
#endif

  while (1) {
    if (addr) {
      /* Use first address that dosn't result in a bad-address error: */
      for (; addr; addr = (rktio_addrinfo_t *)RKTIO_AS_ADDRINFO(addr)->ai_next) {
        do {
          sent = sendto(s, buffer, len, 0,
                        RKTIO_AS_ADDRINFO(addr)->ai_addr, RKTIO_AS_ADDRINFO(addr)->ai_addrlen);
        } while ((sent == -1) && NOT_WINSOCK(errno == EINTR));
        if (sent >= 0)
          break;
        errid = SOCK_ERRNO();
        if (!WAS_EBADADDRESS(errid))
          break;
      }
    } else {
      do {
        sent = send(s, buffer, len, 0);
      } while ((sent == -1) && NOT_WINSOCK(errno == EINTR));

      if (sent == -1)
        errid = SOCK_ERRNO();
    }
    
    if (sent >= 0)
      return sent;
    
    if (WAS_EAGAIN(errid))
      return 0;
    else if (SEND_BAD_MSG_SIZE(errid) && (len > 1)) {
      /* split the message and try again: */
      len >>= 1;
    } else {
      get_socket_error();
      return RKTIO_WRITE_ERROR;
    }
  }
}

intptr_t rktio_socket_write(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t len)
{
  return do_socket_write(rktio, rfd, buffer, len, NULL);
}

/*========================================================================*/
/* TCP connect                                                            */
/*========================================================================*/

struct rktio_connect_t {
  int inprogress;
  rktio_fd_t *trying_fd;
  rktio_addrinfo_t *dest, *src;
  rktio_addrinfo_t *addr; /* walking through dest */
};

static rktio_connect_t *try_connect(rktio_t *rktio, rktio_connect_t *conn);

rktio_connect_t *rktio_start_connect(rktio_t *rktio, rktio_addrinfo_t *dest, rktio_addrinfo_t *src)
{
  rktio_connect_t *conn;

  conn = malloc(sizeof(rktio_connect_t));
  conn->dest = dest;
  conn->src = src;
  conn->addr = dest;

  return try_connect(rktio, conn);
}

static rktio_connect_t *try_connect(rktio_t *rktio, rktio_connect_t *conn)
{
  struct rktio_addrinfo_t *addr;
  rktio_socket_t s;
  
  addr = conn->addr;
  s = socket(RKTIO_AS_ADDRINFO(addr)->ai_family,
             RKTIO_AS_ADDRINFO(addr)->ai_socktype,
             RKTIO_AS_ADDRINFO(addr)->ai_protocol);
  if (s != INVALID_SOCKET) {
    int status, inprogress;
    if (!conn->src
        || !bind(s, RKTIO_AS_ADDRINFO(conn->src)->ai_addr, RKTIO_AS_ADDRINFO(conn->src)->ai_addrlen)) {
#ifdef RKTIO_SYSTEM_WINDOWS
      unsigned long ioarg = 1;
      ioctlsocket(s, FIONBIO, &ioarg);
#else
      RKTIO_WHEN_SET_SOCKBUF_SIZE(int size = TCP_SOCKSENDBUF_SIZE);
      fcntl(s, F_SETFL, RKTIO_NONBLOCKING);
      RKTIO_WHEN_SET_SOCKBUF_SIZE(setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int)));
#endif

      status = connect(s, RKTIO_AS_ADDRINFO(addr)->ai_addr, RKTIO_AS_ADDRINFO(addr)->ai_addrlen);

#ifdef RKTIO_SYSTEM_UNIX
      if (status)
        status = errno;
      if (status == EINTR)
        status = EINPROGRESS;
	
      inprogress = (status == EINPROGRESS);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
      if (status)
        status = WSAGetLastError();
        
      inprogress = (status == WSAEWOULDBLOCK);
#endif

      conn->trying_fd = rktio_system_fd(rktio,
					(intptr_t)s,
					RKTIO_OPEN_SOCKET | RKTIO_OPEN_READ | RKTIO_OPEN_WRITE | RKTIO_OPEN_OWN);
      conn->inprogress = inprogress;

      return conn;
    }
  }
  
  get_socket_error();
  return NULL;
}

int rktio_poll_connect_ready(rktio_t *rktio, rktio_connect_t *conn)
{
  if (conn->inprogress)
    return rktio_socket_poll_write_ready(rktio, conn->trying_fd);
  else
    return RKTIO_POLL_READY;
}

void rktio_poll_add_connect(rktio_t *rktio, rktio_connect_t *conn, rktio_poll_set_t *fds)
{
  if (conn->inprogress)
    rktio_poll_add(rktio, conn->trying_fd, fds, RKTIO_POLL_WRITE);
  else
    rktio_poll_set_add_nosleep(rktio, fds);
}

static void conn_free(rktio_connect_t *conn)
{
  free(conn);
}

rktio_fd_t *rktio_connect_finish(rktio_t *rktio, rktio_connect_t *conn)
{
  rktio_fd_t *rfd = conn->trying_fd;
  
  if (conn->inprogress) {
    /* Check whether connect succeeded, or get error: */
    int errid;
    unsigned status;
    rktio_sockopt_len_t so_len = sizeof(status);
    rktio_socket_t s = rktio_fd_socket(rktio, rfd);
    if (getsockopt(s, SOL_SOCKET, SO_ERROR, (void *)&status, &so_len) != 0) {
      errid = SOCK_ERRNO();
    } else
      errid = status;
#ifdef RKTIO_SYSTEM_WINDOWS
    if (!rktio->windows_nt_or_later && !errid) {
      /* getsockopt() seems not to work in Windows 95, so use the
         result from select(), which seems to reliably detect an error condition */
      if (rktio_poll_connect_ready(rktio, conn) == RKTIO_POLL_ERROR) {
        errid = WSAECONNREFUSED; /* guess! */
      }
    }
#endif

    if (errid) {
      rktio_close(rktio, rfd);
      if (RKTIO_AS_ADDRINFO(conn->addr)->ai_next) {
        /* try the next one */
        conn->addr = (rktio_addrinfo_t *)RKTIO_AS_ADDRINFO(conn->addr)->ai_next;
        if (try_connect(rktio, conn)) {
          set_racket_error(RKTIO_ERROR_CONNECT_TRYING_NEXT);
          return NULL;
        } else {
          /* error was set by try_error() */
          conn_free(conn);
          return NULL;
        }
      } else {
        set_socket_error(errid);
        conn_free(conn);
        return NULL;
      }
    }
  }

  conn_free(conn);

  return rfd;
}

void rktio_connect_stop(rktio_t *rktio, rktio_connect_t *conn)
{
  rktio_close(rktio, conn->trying_fd);

  conn_free(conn);
}

rktio_fd_t *rktio_connect_trying(rktio_t *rktio, rktio_connect_t *conn)
{
  return conn->trying_fd;
}

/*========================================================================*/
/* TCP listener and accept                                                */
/*========================================================================*/

struct rktio_listener_t {
  int count;
# ifdef HAVE_POLL_SYSCALL
  struct pollfd *pfd;
# endif
  rktio_socket_t s[];
};

static int get_no_portno(rktio_t *rktio, rktio_socket_t socket);

rktio_listener_t *rktio_listen(rktio_t *rktio, rktio_addrinfo_t *src, int backlog, int reuse)
{
  {
    rktio_addrinfo_t *addr;
    int count = 0, pos = 0;
    rktio_listener_t *l = NULL;
#ifdef RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
    int any_v4 = 0, any_v6 = 0;
#endif

    for (addr = src; addr; addr = (rktio_addrinfo_t *)RKTIO_AS_ADDRINFO(addr)->ai_next) {
#ifdef RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
      if (RKTIO_AS_ADDRINFO(addr)->ai_family == RKTIO_PF_INET)
	any_v4 = 1;
      else if (RKTIO_AS_ADDRINFO(addr)->ai_family == PF_INET6)
	any_v6 = 1;
#endif
      count++;
    }

    {
      rktio_socket_t s;
#ifdef RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
      /* Try IPv6 listeners first, so we can retry and use just IPv4 if
	 IPv6 doesn't work right. */
      int v6_loop = (any_v6 && any_v4), skip_v6 = 0;
#endif
      int first_time = 1;
      int first_was_zero = 0;
      unsigned short no_port = 0;

      for (addr = src; addr; ) {
#ifdef RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
	if ((v6_loop && (RKTIO_AS_ADDRINFO(addr)->ai_family != PF_INET6))
	    || (skip_v6 && (RKTIO_AS_ADDRINFO(addr)->ai_family == PF_INET6))) {
	  addr = (rktio_addrinfo_t *)RKTIO_AS_ADDRINFO(addr)->ai_next;
	  if (v6_loop && !addr) {
	    v6_loop = 0;
	    skip_v6 = 1;
	    addr = src;
	  }
	  continue;
	}
#endif

	s = socket(RKTIO_AS_ADDRINFO(addr)->ai_family,
                   RKTIO_AS_ADDRINFO(addr)->ai_socktype,
                   RKTIO_AS_ADDRINFO(addr)->ai_protocol);
        if (s != INVALID_SOCKET)
          get_socket_error();

#ifdef RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
	if (s == INVALID_SOCKET) {
	  /* Maybe it failed because IPv6 is not available: */
	  if ((RKTIO_AS_ADDRINFO(addr)->ai_family == PF_INET6) && (errno == EAFNOSUPPORT)) {
	    if (any_v4 && !pos) {
	      /* Let client known that maybe we can make it work with just IPv4. */
	      set_racket_error(RKTIO_ERROR_TRY_AGAIN_WITH_IPV4);
	    }
	  }
	}
	if (s != INVALID_SOCKET) {
	  if (any_v4 && (RKTIO_AS_ADDRINFO(addr)->ai_family == PF_INET6)) {
	    int ok;
# ifdef IPV6_V6ONLY
	    int on = 1;
	    ok = setsockopt(s, IPPROTO_IPV6, IPV6_V6ONLY, &on, sizeof(on));
# else
	    ok = -1;
# endif
	    if (ok) {
	      if (!pos) {
		/* IPV6_V6ONLY doesn't work */
                set_racket_error(RKTIO_ERROR_TRY_AGAIN_WITH_IPV4);
                s = INVALID_SOCKET;
	      } else {
                get_socket_error();
		closesocket(s);
		s = INVALID_SOCKET;
	      }
	    }
	  }
	}
#endif

	if (s != INVALID_SOCKET) {
#ifdef RKTIO_SYSTEM_WINDOWS
	  unsigned long ioarg = 1;
	  ioctlsocket(s, FIONBIO, &ioarg);
#else
	  fcntl(s, F_SETFL, RKTIO_NONBLOCKING);
#endif

	  if (reuse) {
	    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)(&reuse), sizeof(int));
	  }
      
          if (first_was_zero) {
            ((struct sockaddr_in *)RKTIO_AS_ADDRINFO(addr)->ai_addr)->sin_port = no_port;
          }
	  if (!bind(s, RKTIO_AS_ADDRINFO(addr)->ai_addr, RKTIO_AS_ADDRINFO(addr)->ai_addrlen)) {
            if (first_time) {
              if (((struct sockaddr_in *)RKTIO_AS_ADDRINFO(addr)->ai_addr)->sin_port == 0) {
                no_port = get_no_portno(rktio, s);
                first_was_zero = 1;
		if (no_port < 0) {
		  closesocket(s);
		  break;
		}
              }
              first_time = 0;
            }

	    if (!listen(s, backlog)) {
	      if (!pos) {
		l = malloc(sizeof(rktio_listener_t) + (count * sizeof(rktio_socket_t)));
		l->count = count;
# ifdef HAVE_POLL_SYSCALL
                {
                  struct pollfd *pfd;
                  pfd = malloc(sizeof(struct pollfd) * count);
                  l->pfd = pfd;
                }
# endif
	      }
# ifdef HAVE_POLL_SYSCALL
              l->pfd[pos].fd = s;
              l->pfd[pos].events = POLLIN;
# endif
	      l->s[pos++] = s;
	    
	      REGISTER_SOCKET(s);

	      if (pos == count) {
		return l;
	      }
	    } else {
              get_socket_error();
	      closesocket(s);
	      break;
	    }
	  } else {
            get_socket_error();
	    closesocket(s);
	    break;
	  }
	} else {
          break;
        }

	addr = (rktio_addrinfo_t *)RKTIO_AS_ADDRINFO(addr)->ai_next;

#ifdef RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
	if (!addr && v6_loop) {
	  v6_loop = 0;
	  skip_v6 = 1;
	  addr = src;
	}
#endif
      }

      if (l) {
        l->count = pos;
        rktio_listen_stop(rktio, l);
      }

      return NULL;
    }
  }
}

static int get_no_portno(rktio_t *rktio, rktio_socket_t socket)
{
  char here[RKTIO_SOCK_NAME_MAX_LEN];
  struct sockaddr_in *addr_in;
  rktio_sockopt_len_t l = sizeof(here);
  unsigned short no_port;

  if (getsockname(socket, (struct sockaddr *)here, &l)) {
    get_socket_error();
    return -1;
  }

  /* don't use ntohs, since the result is put back into another sin_port: */
  addr_in = (struct sockaddr_in *)here;
  no_port = addr_in->sin_port;
  return no_port;
}

void rktio_listen_stop(rktio_t *rktio, rktio_listener_t *l)
{
  int i;
  rktio_socket_t s;
  
  for (i = 0; i < l->count; i++) {
    s = l->s[i];
    UNREGISTER_SOCKET(s);
    closesocket(s);
  }

#ifdef HAVE_POLL_SYSCALL
  free(l->pfd);
#endif
  
  free(l);
}

static int do_poll_accept_ready(rktio_t *rktio, rktio_listener_t *listener, int report_which)
{
  int sr, i;

#ifdef HAVE_POLL_SYSCALL
  do {
    sr = poll(listener->pfd, listener->count, 0);
  } while ((sr == -1) && (errno == EINTR));

  if (sr > 0) {
    if (report_which) {
      for (i = listener->count; i--; ) {
        if (listener->pfd[i].revents)
          return i + 1;
      }
    } else
      return RKTIO_POLL_READY;
  }

  if (sr == -1) {
    get_socket_error();
    return RKTIO_POLL_ERROR;
  } else
    return 0;
#else
  rktio_socket_t s, mx;
  DECL_SOCK_FDSET(readfds);
  DECL_SOCK_FDSET(exnfds);
  struct timeval time = {0, 0};

  INIT_DECL_SOCK_RD_FDSET(readfds);
  INIT_DECL_SOCK_ER_FDSET(exnfds);

  RKTIO_SOCK_FD_ZERO(readfds);
  RKTIO_SOCK_FD_ZERO(exnfds);

  mx = 0;
  for (i = 0; i < listener->count; i++) {
    s = listener->s[i];
    RKTIO_SOCK_FD_SET(s, readfds);
    RKTIO_SOCK_FD_SET(s, exnfds);
    if (s > mx)
      mx = s;
  }
  
  do {
    sr = select(RKTIO_SOCKS(mx + 1), RKTIO_SOCK_FDS(readfds), NULL, RKTIO_SOCK_FDS(exnfds), &time);
  } while ((sr == -1) && NOT_WINSOCK(errno == EINTR));

  if (sr > 0) {
    if (report_which) {
      for (i = 0; i < listener->count; i++) {
        s = listener->s[i];
        if (RKTIO_SOCK_FD_ISSET(s, readfds)
            || RKTIO_SOCK_FD_ISSET(s, exnfds))
          return i + 1;
      }
    } else
      return RKTIO_POLL_READY;
  }

  if (sr == -1) {
    get_socket_error();
    return RKTIO_POLL_ERROR;
  } else
    return 0;
#endif
}

int rktio_poll_accept_ready(rktio_t *rktio, rktio_listener_t *listener)
{
  return do_poll_accept_ready(rktio, listener, 0);
}

void rktio_poll_add_accept(rktio_t *rktio, rktio_listener_t *listener, rktio_poll_set_t *fds)
{
  int i;
  rktio_socket_t s;
  rktio_poll_set_t *fds2;
  
  fds2 = RKTIO_GET_FDSET(fds, 2);
  
  for (i = 0; i < listener->count; i++) {
    s = listener->s[i];
    RKTIO_FD_SET((intptr_t)s, fds);
    RKTIO_FD_SET((intptr_t)s, fds2);
  }
}

rktio_fd_t *rktio_accept(rktio_t *rktio, rktio_listener_t *listener)
{
  int
    ready_pos;
  rktio_socket_t s, ls;
  rktio_sockopt_len_t l;
  char tcp_accept_addr[RKTIO_SOCK_NAME_MAX_LEN];

  ready_pos = do_poll_accept_ready(rktio, listener, 1);
  if (!ready_pos) {
    set_racket_error(RKTIO_ERROR_ACCEPT_NOT_READY);
    return NULL;
  }
  
  ls = listener->s[ready_pos-1];

  l = sizeof(tcp_accept_addr);

  do {
    s = accept(ls, (struct sockaddr *)tcp_accept_addr, &l);
  } while ((s == -1) && NOT_WINSOCK(errno == EINTR));

  if (s != INVALID_SOCKET) {    
# ifdef RKTIO_SYSTEM_UNIX
    RKTIO_WHEN_SET_SOCKBUF_SIZE(int size = TCP_SOCKSENDBUF_SIZE);
    RKTIO_WHEN_SET_SOCKBUF_SIZE(setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int)));
#  endif

    return rktio_system_fd(rktio, s, (RKTIO_OPEN_SOCKET | RKTIO_OPEN_INIT | RKTIO_OPEN_OWN
				      | RKTIO_OPEN_READ | RKTIO_OPEN_WRITE)); 
  } else {
    get_socket_error();
    return NULL;
  }
}

static char **get_numeric_strings(rktio_t *rktio, void *sa, unsigned int salen)
{
  char **r;
  int err;
#ifdef HAVE_GETADDRINFO
  char host[NI_MAXHOST], serv[NI_MAXSERV];
  
  err = getnameinfo(sa, salen, host, sizeof(host), serv, sizeof(serv),
                    NI_NUMERICHOST | NI_NUMERICSERV);
#else
  char host[128], serv[32];
  {
    unsigned char *b;
    b = (unsigned char *)&((struct sockaddr_in *)sa)->sin_addr;
    sprintf(host, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  }
  {
    int id;
    id = ntohs(((struct sockaddr_in *)sa)->sin_port);
    sprintf(serv, "%d", id);
  }
  err = 0;
#endif
  
  if (!err) {
    r = malloc(sizeof(char*) * 2);
    r[0] = MSC_IZE(strdup)(host);
    r[1] = MSC_IZE(strdup)(serv);
    return r;
  } else {
    set_gai_error(err);
    return NULL;
  }
}

char **rktio_socket_address(rktio_t *rktio, rktio_fd_t *rfd)
{
  char name[RKTIO_SOCK_NAME_MAX_LEN];
  rktio_sockopt_len_t name_len;
  
  name_len = sizeof(name);
  if (getsockname(rktio_fd_socket(rktio, rfd), (struct sockaddr *)name, &name_len)) {
    get_socket_error();
    return NULL;
  }

  return get_numeric_strings(rktio, name, name_len);
}

char **rktio_socket_peer_address(rktio_t *rktio, rktio_fd_t *rfd)
{
  char name[RKTIO_SOCK_NAME_MAX_LEN];
  rktio_sockopt_len_t name_len;
  
  name_len = sizeof(name);
  if (getpeername(rktio_fd_socket(rktio, rfd), (struct sockaddr *)name, &name_len)) {
    get_socket_error();
    return NULL;
  }

  return get_numeric_strings(rktio, name, name_len);
}

char **rktio_listener_address(rktio_t *rktio, rktio_listener_t *lnr)
{
  char name[RKTIO_SOCK_NAME_MAX_LEN];
  rktio_sockopt_len_t name_len;
  
  name_len = sizeof(name);
  if (getsockname(lnr->s[0], (struct sockaddr *)name, &name_len)) {
    get_socket_error();
    return NULL;
  }

  return get_numeric_strings(rktio, name, name_len);
}

/*========================================================================*/
/* UDP                                                                    */
/*========================================================================*/

rktio_fd_t *rktio_udp_open(rktio_t *rktio, rktio_addrinfo_t *addr, int family)
{
  rktio_socket_t s;
  
  if (addr)
    s = socket(RKTIO_AS_ADDRINFO(addr)->ai_family,
               RKTIO_AS_ADDRINFO(addr)->ai_socktype,
               RKTIO_AS_ADDRINFO(addr)->ai_protocol);
  else
    s = socket(family, SOCK_DGRAM, 0);

  if (s == INVALID_SOCKET) {
    get_socket_error();
    return NULL;
  }

  return rktio_system_fd(rktio, s, RKTIO_OPEN_SOCKET | RKTIO_OPEN_UDP | RKTIO_OPEN_INIT);
}

#ifdef UDP_DISCONNECT_EADRNOTAVAIL_OK
# define OK_DISCONNECT_ERROR(e) (((e) == RKTIO_AFNOSUPPORT) || ((e) == EADDRNOTAVAIL))
#else
# define OK_DISCONNECT_ERROR(e) ((e) == RKTIO_AFNOSUPPORT)
#endif

int rktio_udp_disconnect(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  int err;

#ifdef USE_NULL_TO_DISCONNECT_UDP
  err = connect(s, NULL, 0);
#else
  rktio_unspec_address ua;
  ua.sin_family = AF_UNSPEC;
  ua.sin_port = 0;
  memset(&(ua.sin_addr), 0, sizeof(ua.sin_addr));
  memset(&(ua.sin_zero), 0, sizeof(ua.sin_zero));
  err = connect(s, (struct sockaddr *)&ua, sizeof(ua));
#endif

  if (err) {
    err = SOCK_ERRNO();
    if (OK_DISCONNECT_ERROR(err))
      err = 0;
  }

  if (err) {
    get_socket_error();
    return 0;
  }

  return 1;
}

int rktio_udp_bind(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr, rktio_bool_t reuse)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  int err;

  if (reuse) {
    int one = 1;
    if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (void *) &one, sizeof(one))) {
      get_socket_error();
      return 0;
    }
  }

  /* bind using first address that works: */
  for (; addr; addr = (rktio_addrinfo_t *)RKTIO_AS_ADDRINFO(addr)->ai_next) {
    err = bind(s, RKTIO_AS_ADDRINFO(addr)->ai_addr, RKTIO_AS_ADDRINFO(addr)->ai_addrlen);
    if (!err) {
      return 1;
    }
  }

  get_socket_error();
  return 0;
}

int rktio_udp_connect(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  int err;

  /* connect using first address that works: */
  for (; addr; addr = (rktio_addrinfo_t *)RKTIO_AS_ADDRINFO(addr)->ai_next) {
    err = connect(s, RKTIO_AS_ADDRINFO(addr)->ai_addr, RKTIO_AS_ADDRINFO(addr)->ai_addrlen);
    if (!err) {
      return 1;
    }
  }

  get_socket_error();
  return 0;
}

intptr_t rktio_udp_sendto(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr, const char *buffer, intptr_t len)
{
  return do_socket_write(rktio, rfd, buffer, len, addr);
}

rktio_length_and_addrinfo_t *rktio_udp_recvfrom(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  rktio_length_and_addrinfo_t *r;
  int rn, errid;
  char src_addr[RKTIO_SOCK_NAME_MAX_LEN];
  rktio_sockopt_len_t asize = sizeof(src_addr);
  
  while (1) {
    if (!len) {
      /* recvfrom() doesn't necessarily wait if you pass a buffer size of 0;
         to be consistent with accepting a message but discarding bytes that
         don't fit, accept at least one byte and turn a `1` result into `0` */
      char buf[1];
      rn = recvfrom(s, buf, 1, 0, (struct sockaddr *)src_addr, &asize);
      if (rn == 1)
        rn = 0;
    } else
      rn = recvfrom(s, buffer, len, 0, (struct sockaddr *)src_addr, &asize);

    if (rn < 0) {
      errid = SOCK_ERRNO();
      if (WAS_ECONNREFUSED(errid)) {
        /* Delayed ICMP error. Client should ignore it and try again. */
        set_racket_error(RKTIO_ERROR_INFO_TRY_AGAIN);
        return NULL;
      } else if (WAS_WSAEMSGSIZE(errid)) {
        /* => data truncated on Windows, which counts as success on Unix */
        rn = len;
        break;
      } else if (NOT_WINSOCK(errid == EINTR)) {
        /* try again */
      } else if (WAS_EAGAIN(errid)) {
        /* no data available */
        set_racket_error(RKTIO_ERROR_TRY_AGAIN);
        return NULL;
      } else {
        get_socket_error();
        return NULL;
      }
    } else
      break;
  }

  r = malloc(sizeof(rktio_length_and_addrinfo_t));
  r->len = rn;
  r->address = get_numeric_strings(rktio, src_addr, asize);

  return r;
}

int rktio_udp_get_multicast_loopback(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  u_char loop;
  rktio_sockopt_len_t loop_len = sizeof(loop);
  int status;
  
  status = getsockopt(s, IPPROTO_IP, IP_MULTICAST_LOOP, (void *)&loop, &loop_len);
  
  if (status) {
    get_socket_error();
    return RKTIO_PROP_ERROR;
  } else
    return (loop ? 1 : 0);
}

int rktio_udp_set_multicast_loopback(rktio_t *rktio, rktio_fd_t *rfd, int on)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  u_char loop = (on ? 1 : 0);
  rktio_sockopt_len_t loop_len = sizeof(loop);
  int status;
  
  status = setsockopt(s, IPPROTO_IP, IP_MULTICAST_LOOP, (void *)&loop, loop_len);
  
  if (status) {
    get_socket_error();
    return 0;
  } else
    return 1;
}

int rktio_udp_get_multicast_ttl(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  u_char ttl;
  rktio_sockopt_len_t ttl_len = sizeof(ttl);
  int status;
  
  status = getsockopt(s, IPPROTO_IP, IP_MULTICAST_TTL, (void *)&ttl, &ttl_len);
  
  if (status) {
    get_socket_error();
    return RKTIO_PROP_ERROR;
  } else
    return ttl;
}

int rktio_udp_set_multicast_ttl(rktio_t *rktio, rktio_fd_t *rfd, int ttl_val)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  u_char ttl = ttl_val;
  rktio_sockopt_len_t ttl_len = sizeof(ttl);
  int status;
  
  status = setsockopt(s, IPPROTO_IP, IP_MULTICAST_TTL, (void *)&ttl, ttl_len);
  
  if (status) {
    get_socket_error();
    return 0;
  } else
    return 1;
}

char *rktio_udp_multicast_interface(rktio_t *rktio, rktio_fd_t *rfd)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd); 
  struct in_addr intf;
  rktio_sockopt_len_t intf_len = sizeof(intf);
  int status;
  
  status = getsockopt(s, IPPROTO_IP, IP_MULTICAST_IF, (void *)&intf, &intf_len);

  if (status) {
    get_socket_error();
    return NULL;
  } else {
    char host_buf[RKTIO_SOCK_HOST_NAME_MAX_LEN];
    unsigned char *b = (unsigned char *) &intf; /* yes, this is in network order */
    sprintf(host_buf, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
    return MSC_IZE(strdup)(host_buf);
  }
}

int rktio_udp_set_multicast_interface(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *intf_addr)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd); 
  struct in_addr intf;
  rktio_sockopt_len_t intf_len = sizeof(intf);
  int status;

  if (!intf_addr) {
    intf.s_addr = INADDR_ANY;
  } else {
    intf = ((struct sockaddr_in *)RKTIO_AS_ADDRINFO(intf_addr)->ai_addr)->sin_addr;
  }

  status = setsockopt(s, IPPROTO_IP, IP_MULTICAST_IF, (void *)&intf, intf_len);

  if (status) {
    get_socket_error();
    return 0;
  } else
    return 1;
}

int rktio_udp_change_multicast_group(rktio_t *rktio, rktio_fd_t *rfd,
                                     rktio_addrinfo_t *group_addr,
                                     rktio_addrinfo_t *intf_addr,
                                     int action)
{
  rktio_socket_t s = rktio_fd_socket(rktio, rfd);
  struct ip_mreq mreq;
  rktio_sockopt_len_t mreq_len = sizeof(mreq);
  int status;
  int optname;

  if (!intf_addr) {
    mreq.imr_interface.s_addr = INADDR_ANY;
  } else {
    mreq.imr_interface = ((struct sockaddr_in *)RKTIO_AS_ADDRINFO(intf_addr)->ai_addr)->sin_addr;
  }

  mreq.imr_multiaddr = ((struct sockaddr_in *)RKTIO_AS_ADDRINFO(group_addr)->ai_addr)->sin_addr;

  if (action == RKTIO_ADD_MEMBERSHIP)
    optname = IP_ADD_MEMBERSHIP;
  else
    optname = IP_DROP_MEMBERSHIP;

  status = setsockopt(s, IPPROTO_IP, optname, (void *) &mreq, mreq_len);
  
  if (status) {
    get_socket_error();
    return 0;
  } else
    return 1;
}
