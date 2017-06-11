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
# include <fcntl.h>
# include <errno.h>
# define TCP_SOCKSENDBUF_SIZE 32768
# define NOT_WINSOCK(x) x
# define SOCK_ERRNO() errno
# define WAS_EAGAIN(e) ((e == EWOULDBLOCK) || (e == EAGAIN) || (e == EINPROGRESS) || (e == EALREADY))
# define WAS_ECONNREFUSED(e) (e == ECONNREFUSED)
# define WAS_EBADADDRESS(e) (e == EINVAL)
# define WAS_WSAEMSGSIZE(e) 0
# define RKTIO_AFNOSUPPORT EAFNOSUPPORT

typedef intptr_t rktio_socket_t;
# define INVALID_SOCKET (-1)

static void reliably_close(intptr_t s) {
  int cr;
  do { 
    cr = close(s);
  } while ((cr == -1) && (errno == EINTR));
}

static void closesocket(rktio_socket_t s) {
  reliably_close(s);
}

typedef struct sockaddr_in rktio_unspec_address;
#define REGISTER_SOCKET(s) /**/
#define UNREGISTER_SOCKET(s) /**/

# if defined(__linux__) || defined(OS_X)
/* RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT uses IPV6_V6ONLY for IPv6
   listeners when the same listener has an IPv4 address, which means
   that the IpV6 listener accepts only IPv6 connections. This is used
   with Linux, for example, because a port cannot have both an IPv4
   and IPv6 listener if the IPv6 one doesn't use IPV6_V6ONLY. (The
   two listeners might be for different interfaces, in which case
   IPV6_V6ONLY is not necessary, but we must err on the side of being
   too restrictive. If IPV6_V6ONLY is not #defined or if setting the
   option doesn't work, then the IPv6 addresses are silently ignored
   when creating the listener (but only where there is at least once
   IPv4 address). */
#  define RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
# endif

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

typedef SOCKET rktio_socket_t;

typedef struct SOCKADDR_IN rktio_unspec_address;
# define REGISTER_SOCKET(s) winsock_remember(s)
# define UNREGISTER_SOCKET(s) winsock_forget(s)

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


#if RKTIO_SYSTEM_WINDOWS
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
/* Fallback using gethostbyname where getddrinfo isn't avaulable */

#ifdef HAVE_GETADDRINFO
# define rktio_AI_PASSIVE AI_PASSIVE 
# define do_getaddrinfo(n, s, h, res) getaddrinfo(n, s, RKTIO_AS_ADDRINFO(h), RKTIO_AS_ADDRINFO_PTR(res))
# define do_freeaddrinfo freeaddrinfo
# define do_gai_strerror gai_strerror
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
  ReleaseSemaphore(data->ghbn_lock, 1, NULL);
}

static void ghbn_wait(rktio_t *rktio)
{
  ghbn_unlock(rktio);
  WaitForSingleObject(rktio->ghbn_start, INFINITE);
  ghbn_lock(rktio);
}

static void ghbn_signal(rktio_t *rktio)
{
  ReleaseSemaphore(data->ghbn_ready, 1, NULL);
}

static void ghbn_wait_exit(rktio_t *rktio)
{
  WaitForSingleObject(rktio->th, INFINITE);
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
      ReleaseSemaphore(data->ready_sema, 1, NULL);  
# else
      {
        long v = 1;
        do {
          err = write(lookup->done_fd[1], &v, sizeof(v));
        } while ((err == -1) && (errno == EINTR));
        reliably_close(lookup->done_fd[1]);
      }
# endif

      if (lookup->mode == GHBN_ABANDONED) {
# ifdef RKTIO_SYSTEM_WINDOWS
        CloseHandle(data->ready_sema);
# else
        reliably_close(lookup->done_fd[0]);
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
  rktio->th = (HANDLE)_beginthreadex(NULL, 5000, 
                                     win_getaddrinfo_in_thread,
                                     rktio, 0, &id);
  if (rktio->th == INVALID_HANDLE) {
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
    HANDLE ready_sema;
    unsigned int id;
    intptr_t th;
      
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
      reliably_close(lookup->done_fd[0]);
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
  rktio_poll_set_add_handle(lookup->done_sema, fds, 1);
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
    reliably_close(lookup->done_fd[0]);
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
  int err;
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
  lookup->name = (hostname ? strdup(hostname) : NULL);
  lookup->svc = (service ? strdup(service) : NULL);
  lookup->hints = hints;
  init_lookup(lookup);
 
  return start_lookup(rktio, lookup);
}

void rktio_free_addrinfo(rktio_t *rktio, rktio_addrinfo_t *a)
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
static int wsr_size = 0;
static rktio_socket_t *wsr_array;

void rktio_winsock_init()
{
  if (!winsock_sema) {
    winsock_sema = CreateSemaphore(NULL, 1, 1, NULL);
  }
}

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

# ifdef RKTIO_USE_PLACES
  ReleaseSemaphore(winsock_sema, 1, NULL);
# endif
}

static void winsock_forget(rktio_socket_t s)
{
  int i;

# ifdef RKTIO_USE_PLACES
  WaitForSingleObject(winsock_sema, INFINITE);
# endif

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i] == s) {
      wsr_array[i] = (rktio_socket_t)NULL;
      break;
    }
  }

# ifdef RKTIO_USE_PLACES
  ReleaseSemaphore(winsock_sema, 1, NULL);
# endif
}

static int winsock_done(void)
{
  int i;

  /* only called in the original place */

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i]) {
      closesocket(wsr_array[i]);
      wsr_array[i] = (rktio_socket_t)NULL;
    }
  }

  return WSACleanup();
}

static void TCP_INIT(char *name)
{
  static int started = 0;
  
  WaitForSingleObject(winsock_sema, INFINITE);

  if (!started) {
    WSADATA data;
    if (!WSAStartup(MAKEWORD(1, 1), &data)) {
      started = 1;
#ifdef __BORLANDC__
      atexit((void(*)())winsock_done);
#else      
      _onexit(winsock_done);
#endif
    }
  }
  
  ReleaseSemaphore(winsock_sema, 1, NULL);
}
#else
/* Not Windows */
# define TCP_INIT(x) /* nothing */
#endif

/*========================================================================*/
/* TCP sockets                                                            */
/*========================================================================*/

int rktio_socket_close(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return rktio_close(rktio, rfd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  UNREGISTER_SOCKET(rfd->sock);
  closesocket(rfd->sock);

  return 1;
#endif
}

int rktio_socket_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd)
{
#ifdef RKTIO_SYSTEM_UNIX
  return rktio_poll_write_ready(rktio, rfd);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    rktio_socket_t s = rktio_fd_system_fd(rktio, rfd);
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
    
    sr = select(s + 1, NULL, writefds, exnfds, &time);

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
    rktio_socket_t s = rktio_fd_system_fd(rktio, rfd);
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
    
    sr = select(s + 1, readfdsfds, NULL, exnfds, &time);
    
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

static void init_socket(rktio_socket_t s)
{
#ifdef RKTIO_SYSTEM_UNIX
  fcntl(s, F_SETFL, RKTIO_NONBLOCKING);
#endif
#ifdef RKTIO_SYSTEM_WINDOWS
  {
    unsigned long ioarg = 1;
    ioctlsocket(s, FIONBIO, &ioarg);
  }
#endif
}

intptr_t rktio_socket_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
  rktio_socket_t s = rktio_fd_system_fd(rktio, rfd);
  int rn;
  
  do {
    rn = recv(s, buffer, len, 0);
  } while ((rn == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (rn > 0)
    return rn;
  else if (rn == 0)
    return RKTIO_READ_EOF;
  else if (WAS_EAGAIN(rn))
    return 0;
  else {
    get_socket_error();
    return RKTIO_READ_ERROR;
  }
}

intptr_t rktio_socket_write(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len)
{
  rktio_socket_t s = rktio_fd_system_fd(rktio, rfd);
  intptr_t sent;
  int errid;

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
    do {
      sent = send(s, buffer, len, 0);
    } while ((sent == -1) && (NOT_WINSOCK(errno) == EINTR));
    
    if (sent >= 0)
      return sent;
    
    errid = SOCK_ERRNO();
    
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
  int errid;

#ifdef USE_TCP
  TCP_INIT("tcp-connect");
#endif

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
      errno = status;
#endif

      REGISTER_SOCKET(s);
 
      conn->trying_fd = rktio_system_fd(rktio, s, RKTIO_OPEN_SOCKET | RKTIO_OPEN_READ | RKTIO_OPEN_WRITE);
      conn->inprogress = inprogress;

      return conn;
    }
  }
  
  get_socket_error();
  return NULL;
}

int rktio_poll_connect_ready(rktio_t *rktio, rktio_connect_t *conn)
{
  if (!conn->inprogress) {
    return rktio_socket_poll_write_ready(rktio, conn->trying_fd);
  } else
    return RKTIO_POLL_READY;
}

void rktio_poll_add_connect(rktio_t *rktio, rktio_connect_t *conn, rktio_poll_set_t *fds)
{
  rktio_poll_add(rktio, conn->trying_fd, fds, RKTIO_POLL_WRITE);
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
    unsigned int so_len = sizeof(errid);
    rktio_socket_t s = rktio_fd_system_fd(rktio, rfd);
    if (getsockopt(s, SOL_SOCKET, SO_ERROR, (void *)&errid, &so_len) != 0) {
      errid = SOCK_ERRNO();
    } else
      errid = 0;
#ifdef RKTIO_SYSTEM_WINDOWS
    if (!rktio->windows_nt_or_later && !errid) {
      /* getsockopt() seems not to work in Windows 95, so use the
         result from select(), which seems to reliably detect an error condition */
      if (rktio_poll_connect_ready() == RKTIO_POLL_ERROR) {
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

  init_socket(rktio_fd_system_fd(rktio, rfd));

  conn_free(conn);

  return rfd;
}

void rktio_connect_stop(rktio_t *rktio, rktio_connect_t *conn)
{
  rktio_close(rktio, conn->trying_fd);

  conn_free(conn);
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
#ifdef RKTIO_TCP_LISTEN_IPV6_ONLY_SOCKOPT
  int no_ipv6 = 0;
#endif
    
  {
    rktio_addrinfo_t *addr;
    int err, count = 0, pos = 0, i;
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

      l->count = pos;
      rktio_listen_stop(rktio, l);

      return NULL;
    }
  }
}

static int get_no_portno(rktio_t *rktio, rktio_socket_t socket)
{
  char here[RKTIO_SOCK_NAME_MAX_LEN];
  struct sockaddr_in *addr_in;
  unsigned int l = sizeof(here);
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
    sr = select(mx + 1, RKTIO_SOCK_FDS(readfds), NULL, RKTIO_SOCK_FDS(exnfds), &time);
  } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));

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

void rktio_poll_add_receive(rktio_t *rktio, rktio_listener_t *listener, rktio_poll_set_t *fds)
{
  int i;
  rktio_socket_t s;
  rktio_poll_set_t *fds2;
  
  fds2 = RKTIO_GET_FDSET(fds, 2);
  
  for (i = 0; i < listener->count; i++) {
    s = listener->s[i];
    RKTIO_FD_SET(s, fds);
    RKTIO_FD_SET(s, fds2);
  }
}

rktio_fd_t *rktio_accept(rktio_t *rktio, rktio_listener_t *listener)
{
  int was_closed = 0, errid, ready_pos;
  rktio_socket_t s, ls;
  unsigned int l;
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
  } while ((s == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (s != INVALID_SOCKET) {    
# ifdef RKTIO_SYSTEM_UNIX
    RKTIO_WHEN_SET_SOCKBUF_SIZE(int size = TCP_SOCKSENDBUF_SIZE);
    RKTIO_WHEN_SET_SOCKBUF_SIZE(setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int)));
#  endif

    REGISTER_SOCKET(s);
    
    return rktio_system_fd(rktio, s, RKTIO_OPEN_SOCKET | RKTIO_OPEN_READ | RKTIO_OPEN_WRITE); 
  } else {
    get_socket_error();
    return NULL;
  }
}

#if 0

static void mz_getnameinfo(void *sa, int salen, 
                           char *host, int hostlen,
                           char *serv, int servlen)
{
#ifdef HAVE_GETADDRINFO
  getnameinfo(sa, salen, host, hostlen, serv, servlen,
	      NI_NUMERICHOST | NI_NUMERICSERV);
#else
  if (host) {
    unsigned char *b;
    b = (unsigned char *)&((struct sockaddr_in *)sa)->sin_addr;
    sprintf(host, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  }
  if (serv) {
    int id;
    id = ntohs(((struct sockaddr_in *)sa)->sin_port);
    sprintf(serv, "%d", id);
  }
#endif
}

char **rktio_get_addresses()
{
}

static int extract_svc_value(char *svc_buf)
{
  int id = 0, j;
  for (j = 0; svc_buf[j]; j++) {
    id = (id * 10) + (svc_buf[j] - '0');
  }
  return id;
}

#define SCHEME_LISTEN_PORTP(p) SAME_TYPE(SCHEME_TYPE(p), scheme_listener_type)
#define SCHEME_UDP_PORTP(p) SAME_TYPE(SCHEME_TYPE(p), scheme_udp_type)

static Scheme_Object *tcp_addresses(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  rktio_socket_t socket = 0;
  Scheme_Tcp *tcp = NULL;
  int closed = 0;
  Scheme_Object *result[4];
  int with_ports = 0;
  int listener = 0;
  int udp = 0;

  if (SCHEME_OUTPUT_PORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(argv[0]);
    if (op->sub_type == scheme_tcp_output_port_type)
      tcp = op->port_data;
    closed = op->closed;
  } else if (SCHEME_INPUT_PORTP(argv[0])) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(argv[0]);
    if (ip->sub_type == scheme_tcp_input_port_type)
      tcp = ip->port_data;
    closed = ip->closed;
  }

  if (argc > 1)
    with_ports = SCHEME_TRUEP(argv[1]);

  if (tcp) {
    socket = tcp->tcp;
  }
  else {
    if (SCHEME_LISTEN_PORTP(argv[0])) {
      listener = 1;
      socket = ((listener_t *)argv[0])->s[0];
    } else if (SCHEME_UDP_PORTP(argv[0])) {
      udp = 1;
      socket = ((Scheme_UDP *)argv[0])->s;
    } else {
      scheme_wrong_contract("tcp-addresses", "tcp-port?", 0, argc, argv);
    }
  }

  if (closed)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-addresses: port is closed");

  {
    unsigned int l;
    char here[RKTIO_SOCK_NAME_MAX_LEN], there[RKTIO_SOCK_NAME_MAX_LEN];
    char host_buf[RKTIO_SOCK_HOST_NAME_MAX_LEN];
    char svc_buf[RKTIO_SOCK_SVC_NAME_MAX_LEN];
    unsigned int here_len;
    unsigned int there_len = 0;
    int peerrc = 0;

    l = sizeof(here);
    if (getsockname(socket, (struct sockaddr *)here, &l)) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "tcp-addresses: could not get local address\n"
                       "  system error: %E",
		       SOCK_ERRNO());
    }
    here_len = l;

    if (!listener) {
      l = sizeof(there);
      peerrc = getpeername(socket, (struct sockaddr *)there, &l);
      if (peerrc && !udp) {
        scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                         "tcp-addresses: could not get peer address\n"
                         "  system error: %E", 
                         SOCK_ERRNO());
      }
      there_len = l;
    }

    scheme_getnameinfo((struct sockaddr *)here, here_len, 
		       host_buf, sizeof(host_buf),
                       (with_ports ? svc_buf : NULL), 
                       (with_ports ? sizeof(svc_buf) : 0));
    result[0] = scheme_make_utf8_string(host_buf);
    if (with_ports) {
      l = extract_svc_value(svc_buf);
      result[1] = scheme_make_integer(l);
    }

    if (listener || (udp && peerrc)) {
      result[with_ports ? 2 : 1] = scheme_make_utf8_string("0.0.0.0");
      result[3] = scheme_make_integer(0);
    }
    else {
      scheme_getnameinfo((struct sockaddr *)there, there_len, 
          host_buf, sizeof(host_buf),
          (with_ports ? svc_buf : NULL), 
          (with_ports ? sizeof(svc_buf) : 0));
      result[with_ports ? 2 : 1] = scheme_make_utf8_string(host_buf);
      if (with_ports) {
        l = extract_svc_value(svc_buf);
        result[3] = scheme_make_integer(l);
      }
    }
  }

  return scheme_values(with_ports ? 4 : 2, result);
#else
  /* First arg can't possibly be right! */
  scheme_wrong_contract("tcp-addresses", "tcp-port?", 0, argc, argv);
#endif
}

intptr_t scheme_dup_socket(intptr_t fd) {
#ifdef USE_TCP
# ifdef RKTIO_SYSTEM_WINDOWS
  intptr_t nsocket;
  intptr_t rc;
  WSAPROTOCOL_INFO protocolInfo;
  rc = WSADuplicateSocket(fd, GetCurrentProcessId(), &protocolInfo);
  if (rc)
    return rc;
  nsocket = WSASocket(FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, &protocolInfo, 0, WSA_FLAG_OVERLAPPED);
  REGISTER_SOCKET(nsocket);
  return nsocket;
# else
  intptr_t nfd;
  do {
    nfd = dup(fd);
  } while (nfd == -1 && errno == EINTR);
  return nfd;
# endif
#else
  return -1;
#endif
}

/*========================================================================*/
/* UDP                                                                    */
/*========================================================================*/

/* Based on a design and implemenation by Eduardo Cavazos. */

#ifdef UDP_IS_SUPPORTED

typedef struct Scheme_UDP_Evt {
  Scheme_Object so; /* scheme_udp_evt_type */
  Scheme_UDP *udp;
  short for_read, with_addr;
  int offset, len;
  char *str;
  int dest_addr_count;
  char **dest_addrs;
  int *dest_addr_lens;
} Scheme_UDP_Evt;

static int udp_close_it(Scheme_Object *_udp)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (udp->s != INVALID_SOCKET) {
    closesocket(udp->s);
    (void)scheme_fd_to_semaphore(udp->s, MZFD_REMOVE, 1);
    udp->s = INVALID_SOCKET;

    scheme_remove_managed(udp->mref, (Scheme_Object *)udp);

    return 0;
  }

  return 1;
}

#else

typedef struct Scheme_UDP_Evt { } Scheme_UDP_Evt;
typedef Scheme_Object Scheme_UDP;

#endif

static Scheme_Object *make_udp(int argc, Scheme_Object *argv[])
{
#ifdef UDP_IS_SUPPORTED
  Scheme_UDP *udp;
  rktio_socket_t s;
  char *address = "";
  unsigned short origid, id;

  TCP_INIT("udp-open-socket");

  if ((argc > 0) && !SCHEME_FALSEP(argv[0]) && !SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("udp-open-socket", "(or/c string? #f)", 0, argc, argv);
  if ((argc > 1) && !SCHEME_FALSEP(argv[1]) && !CHECK_PORT_ID(argv[1]))
    scheme_wrong_contract("udp-open-socket", "(or/c " PORT_ID_TYPE " #f)", 1, argc, argv);

  if ((argc > 0) && SCHEME_TRUEP(argv[0])) {
    Scheme_Object *bs;
    bs = scheme_char_string_to_byte_string(argv[0]);
    address = SCHEME_BYTE_STR_VAL(bs);
  } else
    address = NULL;
  if ((argc > 1) && SCHEME_TRUEP(argv[1]))
    origid = (unsigned short)SCHEME_INT_VAL(argv[1]);
  else
    origid = 0;

  scheme_security_check_network("udp-open-socket", address, origid, 0);
  scheme_custodian_check_available(NULL, "udp-open-socket", "network");

  if (address || origid) {
    int err;
    GC_CAN_IGNORE struct rktio_addrinfo_t *udp_bind_addr = NULL;
    if (!origid)
      origid = 1025;
    id = origid;
    udp_bind_addr = scheme_get_host_address(address, id, &err, -1, 1, 0);
    if (!udp_bind_addr) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "udp-open-socket: can't resolve address\n"
                       "  address: %s\n"
                       "  system error: %N", 
		       address ? address : "<unspec>", 1, err);
      return NULL;
    }
    s = socket(udp_bind_addr->ai_family,
	       udp_bind_addr->ai_socktype,
	       udp_bind_addr->ai_protocol);
    mz_freeaddrinfo(udp_bind_addr);
  } else {
    s = socket(RKTIO_PF_INET, SOCK_DGRAM, 0);
  }

  if (s == INVALID_SOCKET) {
    int errid;
    errid = SOCK_ERRNO();
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "udp-open-socket: creation failed\n"
                     "  system error: %E", 
                     errid);
    return NULL;
  }

  udp = MALLOC_ONE_TAGGED(Scheme_UDP);
  udp->so.type = scheme_udp_type;
  udp->s = s;
  udp->bound = 0;
  udp->connected = 0;
  udp->previous_from_addr = NULL;

#ifdef RKTIO_SYSTEM_WINDOWS
  {
    unsigned long ioarg = 1;
    BOOL bc = 1;
    ioctlsocket(s, FIONBIO, &ioarg);
    setsockopt(s, SOL_SOCKET, SO_BROADCAST, (char *)(&bc), sizeof(BOOL));
  }
#else
  fcntl(s, F_SETFL, RKTIO_NONBLOCKING);
# ifdef SO_BROADCAST
  {
    int bc = 1;
    setsockopt(s, SOL_SOCKET, SO_BROADCAST, &bc, sizeof(bc));
  }
# endif
#endif

  {
    Scheme_Custodian_Reference *mref;
    mref = scheme_add_managed(NULL,
			      (Scheme_Object *)udp,
			      (Scheme_Close_Custodian_Client *)udp_close_it,
			      NULL,
			      1);
    udp->mref = mref;
  }

  return (Scheme_Object *)udp;
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "udp-open-socket: " NOT_SUPPORTED_STR);
  return NULL;
#endif
}

static Scheme_Object *
udp_close(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract("udp-close", "udp?", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  if (udp_close_it(argv[0])) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "udp-close: udp socket was already closed");
    return NULL;
  }
#endif

  return scheme_void;
}

static Scheme_Object *
udp_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_UDPP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
udp_bound_p(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract("udp-bound?", "udp?", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  return (((Scheme_UDP *)argv[0])->bound ? scheme_true : scheme_false);
#else
  return scheme_void;
#endif
}

static Scheme_Object *
udp_connected_p(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract("udp-connected?", "udp?", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  return (((Scheme_UDP *)argv[0])->connected ? scheme_true : scheme_false);
#else
  return scheme_void;
#endif
}

#ifdef UDP_DISCONNECT_EADRNOTAVAIL_OK
# define OK_DISCONNECT_ERROR(e) (((e) == RKTIO_AFNOSUPPORT) || ((e) == EADDRNOTAVAIL))
#else
# define OK_DISCONNECT_ERROR(e) ((e) == RKTIO_AFNOSUPPORT)
#endif

static Scheme_Object *udp_bind_or_connect(const char *name, int argc, Scheme_Object *argv[], int do_bind)
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp;
    char *address = NULL;
    unsigned short port = 0;
    GC_CAN_IGNORE struct rktio_addrinfo_t *udp_bind_addr = NULL, *addr;

    udp = (Scheme_UDP *)argv[0];

    if (!SCHEME_FALSEP(argv[1]) && !SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_contract(name, "(or/c string? #f)", 1, argc, argv);
    if (do_bind && !CHECK_LISTEN_PORT_ID(argv[2]))
      scheme_wrong_contract(name, LISTEN_PORT_ID_TYPE, 2, argc, argv);
    if (!do_bind && !SCHEME_FALSEP(argv[2]) && !CHECK_PORT_ID(argv[2]))
      scheme_wrong_contract(name, "(or/c " PORT_ID_TYPE " #f)", 2, argc, argv);

    if (SCHEME_TRUEP(argv[1])) {
      Scheme_Object *bs;
      bs = scheme_char_string_to_byte_string(argv[1]);
      address = SCHEME_BYTE_STR_VAL(bs);
    }
    if (SCHEME_TRUEP(argv[2]))
      port = (unsigned short)SCHEME_INT_VAL(argv[2]);
    
    if (!do_bind && (SCHEME_TRUEP(argv[1]) != SCHEME_TRUEP(argv[2]))) {
      scheme_contract_error(name,
                            "last second and third arguments must be both #f or both non-#f",
                            "second argument", 1, argv[1], 
                            "third argument", 1, argv[2],
                            NULL);
    }

    scheme_security_check_network(name, address, port, !do_bind);

    if (udp->s == INVALID_SOCKET) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                       "%s: udp socket was already closed\n"
                       "  socket: %V", 
                       name, udp); 
      return NULL;
    }
    if (do_bind && udp->bound) { 
      scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                       "%s: udp socket is already bound\n"
                       "  socket: %V", 
                       name, udp);
      return NULL;
    }

    if (SCHEME_FALSEP(argv[1]) && SCHEME_FALSEP(argv[2])) {
      /* DISCONNECT */
      int errid = 0;
      if (udp->connected) {
        int ok;
#ifdef USE_NULL_TO_DISCONNECT_UDP
        ok = !connect(udp->s, NULL, 0);
#else
        GC_CAN_IGNORE rktio_unspec_address ua;
        ua.sin_family = AF_UNSPEC;
        ua.sin_port = 0;
        memset(&(ua.sin_addr), 0, sizeof(ua.sin_addr));
        memset(&(ua.sin_zero), 0, sizeof(ua.sin_zero));
        ok = !connect(udp->s, (struct sockaddr *)&ua, sizeof(ua));
#endif
        if (!ok) errid = SOCK_ERRNO();
        if (ok || OK_DISCONNECT_ERROR(errid)) {
          udp->connected = 0;
          return scheme_void;
        }
        else {
          scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                           "%s: can't disconnect\n"
                           "  address: %s\n"
                           "  port number: %d\n"
                           "  system error: %E",
                           name, 
                           address ? address : "#f", port,
                           errid);
        }
      }
      return scheme_void;
    }

    {
      /* RESOLVE ADDRESS */
      int err;
      udp_bind_addr = scheme_get_host_address(address, port, &err, -1, do_bind, 0);
      if (!udp_bind_addr) {
        scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                         "%s: can't resolve address\n"
                         "  address: %s\n"
                         "  system error: %N", 
                         name, 
                         address, 
                         1, err);
        return NULL;
      }
    }

    if (!do_bind) {
      /* CONNECT CASE */
      int ok, errid = -1;

      /* connect using first address that works: */
      for (addr = udp_bind_addr; addr; addr = addr->ai_next) {
        ok = !connect(udp->s, addr->ai_addr, addr->ai_addrlen);
        if (ok) {
          udp->connected = 1;
          mz_freeaddrinfo(udp_bind_addr);
          return scheme_void;
        } else
          errid = SOCK_ERRNO();
      }

      mz_freeaddrinfo(udp_bind_addr);

      scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                       "%s: can't connect\n"
                       "  address: %s\n"
                       "  port number: %d\n"
                       "  system error: %E", 
                       name, 
                       address ? address : "#f", 
                       port, 
                       errid);

      return NULL;
    } else {
      /* BIND CASE */
      int ok, errid = -1;

      if ((argc > 3) && SCHEME_TRUEP(argv[3])) {
	int one = 1;
	if (setsockopt(udp->s, SOL_SOCKET, SO_REUSEADDR, (void *) &one, sizeof(one))) {
	  scheme_raise_exn(MZEXN_FAIL_NETWORK,
			   "%s: can't set SO_REUSEADDR\n"
			   "  system error: %E",
			   name,
			   SOCK_ERRNO());
	  return NULL;
	}
      }

      /* bind using first address that works: */
      for (addr = udp_bind_addr; addr; addr = addr->ai_next) {
        ok = !bind(udp->s, addr->ai_addr, addr->ai_addrlen);
        if (ok) {
          udp->bound = 1;
          mz_freeaddrinfo(udp_bind_addr);
          return scheme_void;
        } else
          errid = SOCK_ERRNO();
      }

      mz_freeaddrinfo(udp_bind_addr);

      scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                       "%s: can't bind\n"
                       "  address: %s\n"
                       "  port number: %d\n"
                       "  system error: %E",
                       name, 
                       address ? address : "#f", 
                       port, 
                       errid);
      return NULL;
    }
  }
#else
  return scheme_void;
#endif
}

static Scheme_Object *udp_bind(int argc, Scheme_Object *argv[])
{
  return udp_bind_or_connect("udp-bind!", argc, argv, 1);
}

static Scheme_Object *udp_connect(int argc, Scheme_Object *argv[])
{
  return udp_bind_or_connect("udp-connect!", argc, argv, 0);
}

#ifdef UDP_IS_SUPPORTED

static int udp_check_send(Scheme_Object *_udp, Scheme_Schedule_Info *sinfo)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (udp->s == INVALID_SOCKET)
    return 1;

  if (!sinfo || !sinfo->is_poll) {
    if (!check_fd_sema(udp->s, MZFD_CHECK_WRITE, sinfo, NULL))
      return 0;
  }

# ifdef HAVE_POLL_SYSCALL
  {
    GC_CAN_IGNORE struct pollfd pfd[1];
    int sr;

    pfd[0].fd = udp->s;
    pfd[0].events = POLLOUT;
    do {
      sr = poll(pfd, 1, 0);
    } while ((sr == -1) && (errno == EINTR));

    if (sr)
      return sr;
  }
# else
  {
    DECL_SOCK_FDSET(writefds);
    DECL_SOCK_FDSET(exnfds);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_SOCK_WR_FDSET(writefds);
    INIT_DECL_SOCK_ER_FDSET(exnfds);
    
    RKTIO_SOCK_FD_ZERO(writefds);
    RKTIO_SOCK_FD_SET(udp->s, writefds);
    RKTIO_SOCK_FD_ZERO(exnfds);
    RKTIO_SOCK_FD_SET(udp->s, exnfds);
    
    do {
      sr = select(udp->s + 1, NULL, writefds, exnfds, &time);
    } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));
   
    if (sr)
      return sr;
  }
#endif

  check_fd_sema(udp->s, MZFD_CREATE_WRITE, sinfo, NULL);
  
  return 0;
}

static void udp_send_needs_wakeup(Scheme_Object *_udp, void *fds)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;
  void *fds1, *fds2;
  rktio_socket_t s = udp->s;
  
  fds1 = RKTIO_GET_FDSET(fds, 1);
  fds2 = RKTIO_GET_FDSET(fds, 2);
  
  RKTIO_FD_SET(s, (fd_set *)fds1);
  RKTIO_FD_SET(s, (fd_set *)fds2);
}

#endif

#ifdef UDP_IS_SUPPORTED
static Scheme_Object *do_udp_send_it(const char *name, Scheme_UDP *udp,
				     char *bstr, intptr_t start, intptr_t end,
				     char *dest_addr, int dest_addr_len, int can_block,
                                     int ignore_addr_failure)
{
  intptr_t x;
  int errid = 0;

  while (1) {
    if (udp->s == INVALID_SOCKET) {
      /* socket was closed, maybe while we slept */
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: udp socket is closed\n"
                       "  socket: %V",
		       name, udp);
      return NULL;
    }
    if ((!dest_addr && !udp->connected)
	|| (dest_addr && udp->connected)) {
      /* socket is unconnected, maybe disconnected while we slept */
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: udp socket is%s connected\n"
                       "  socket: %V",
		       name, 
		       dest_addr ? "" : " not",
		       udp);
      return NULL;
    }

    udp->bound = 1; /* in case it's not bound already, send[to] binds it */

    if (dest_addr)
      x = sendto(udp->s, bstr XFORM_OK_PLUS start, end - start, 
		 0, (struct sockaddr *)dest_addr, dest_addr_len);
    else
      x = send(udp->s, bstr XFORM_OK_PLUS start, end - start, 0);

    if (x == -1) {
      errid = SOCK_ERRNO();
      if (ignore_addr_failure && WAS_EBADADDRESS(errid)) {
        return NULL;
      } else if (WAS_EAGAIN(errid)) {
	if (can_block) {
	  /* Block and eventually try again. */
          Scheme_Object *sema;
          sema = scheme_fd_to_semaphore(udp->s, MZFD_CREATE_WRITE, 1);
          if (sema)
            scheme_wait_sema(sema, 0);
          else
            scheme_block_until((Scheme_Ready_Fun)udp_check_send, 
                               udp_send_needs_wakeup, 
                               (Scheme_Object *)udp, 
                               0);
	} else
	  return scheme_false;
      } else if (NOT_WINSOCK(errid) != EINTR)
	break;
    } else if (x != (end - start)) {
      /* this isn't supposed to happen: */
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: didn't send enough (%d != %d)", 
		       name,
		       x, end - start);
      return NULL;
    } else
      break;
  }
    
  if (x > -1) {
    return (can_block ? scheme_void : scheme_true);
  } else {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: send failed\n"
                     "  system error: %E", 
		     name,
		     errid);
    return NULL;
  }
}
#endif

static Scheme_Object *udp_send_it(const char *name, int argc, Scheme_Object *argv[],
				  int with_addr, int can_block, Scheme_UDP_Evt *fill_evt)
{
#ifdef UDP_IS_SUPPORTED
  Scheme_UDP *udp;
  char *address = "";
  intptr_t start, end;
  int delta, err;
  unsigned short origid, id;
  GC_CAN_IGNORE struct rktio_addrinfo_t *udp_dest_addr;

  udp = (Scheme_UDP *)argv[0];
#endif

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  if (with_addr) {
    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_contract(name, "string?", 1, argc, argv);
    if (!CHECK_PORT_ID(argv[2]))
      scheme_wrong_contract(name, PORT_ID_TYPE, 2, argc, argv);
    delta = 0;
  } else
    delta = -2;

  if (!SCHEME_BYTE_STRINGP(argv[3 + delta]))
    scheme_wrong_contract(name, "bytes?", 3 + delta, argc, argv);
  
  scheme_get_substring_indices(name, argv[3 + delta], 
			       argc, argv,
			       4 + delta, 5 + delta, &start, &end);

  if (with_addr) {
    Scheme_Object *bs;
    bs = scheme_char_string_to_byte_string(argv[1]);
    address = SCHEME_BYTE_STR_VAL(bs);
    origid = (unsigned short)SCHEME_INT_VAL(argv[2]);

    scheme_security_check_network(name, address, origid, 1);

    id = origid;
  } else {
    address = NULL;
    id = origid = 0;
  }

  if (with_addr)
    udp_dest_addr = scheme_get_host_address(address, id, &err, -1, 0, 0);
  else
    udp_dest_addr = NULL;

  if (!with_addr || udp_dest_addr) {
    if (fill_evt) {
      char *s;
      fill_evt->str = SCHEME_BYTE_STR_VAL(argv[3+delta]);
      fill_evt->offset = start;
      fill_evt->len = end - start;
      if (udp_dest_addr) {
        GC_CAN_IGNORE struct rktio_addrinfo_t *addr;
        int j, *lens;
        char **addrs;
        for (j = 0, addr = udp_dest_addr; addr; addr = addr->ai_next) {
          j++;
        }
        fill_evt->dest_addr_count = j;
        addrs = MALLOC_N(char*, j);
        fill_evt->dest_addrs = addrs;
        lens = MALLOC_N_ATOMIC(int, j);
        fill_evt->dest_addr_lens = lens;
        for (j = 0, addr = udp_dest_addr; addr; addr = addr->ai_next, j++) {
          s = (char *)scheme_malloc_atomic(addr->ai_addrlen);
          memcpy(s, addr->ai_addr, addr->ai_addrlen);
          fill_evt->dest_addrs[j] = s;
          fill_evt->dest_addr_lens[j] = addr->ai_addrlen;
        }
        mz_freeaddrinfo(udp_dest_addr);
      }
      return scheme_void;
    } else {
      Scheme_Object *r;
      if (udp_dest_addr) {
        GC_CAN_IGNORE struct rktio_addrinfo_t *addr;
        r = NULL;
        for (addr = udp_dest_addr; !r && addr; addr = addr->ai_next) {
          r = do_udp_send_it(name, udp,
                             SCHEME_BYTE_STR_VAL(argv[3+delta]), start, end,
                             (char *)addr->ai_addr,
                             addr->ai_addrlen,
                             can_block,
                             !!addr->ai_next);
        }
	mz_freeaddrinfo(udp_dest_addr);
      } else {
        r = do_udp_send_it(name, udp,
                           SCHEME_BYTE_STR_VAL(argv[3+delta]), start, end,
                           NULL, 0, can_block, 1);
      }
      return r;
    }
  } else {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: can't resolve address\n"
                     "  address: %s\n"
                     "  system error: %N", 
		     name,
		     address, 1, err);
    return NULL;
  }
#else
  return scheme_void;
#endif
}

static Scheme_Object *udp_send_to(int argc, Scheme_Object *argv[])
{
  return udp_send_it("udp-send-to", argc, argv, 1, 1, NULL);
}

static Scheme_Object *udp_send(int argc, Scheme_Object *argv[])
{
  return udp_send_it("udp-send", argc, argv, 0, 1, NULL);
}

static Scheme_Object *udp_send_to_star(int argc, Scheme_Object *argv[])
{
  return udp_send_it("udp-send-to*", argc, argv, 1, 0, NULL);
}

static Scheme_Object *udp_send_star(int argc, Scheme_Object *argv[])
{
  return udp_send_it("udp-send*", argc, argv, 0, 0, NULL);
}

static Scheme_Object *udp_send_to_enable_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(udp_send_to, argc, argv);
}

static Scheme_Object *udp_send_enable_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(udp_send, argc, argv);
}

#ifdef UDP_IS_SUPPORTED

static int udp_check_recv(Scheme_Object *_udp, Scheme_Schedule_Info *sinfo)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (udp->s == INVALID_SOCKET)
    return 1;

  if (!sinfo || !sinfo->is_poll) {
    if (!check_fd_sema(udp->s, MZFD_CHECK_READ, sinfo, NULL))
      return 0;
  }

# ifdef HAVE_POLL_SYSCALL
  {
    GC_CAN_IGNORE struct pollfd pfd[1];
    int sr;

    pfd[0].fd = udp->s;
    pfd[0].events = POLLIN;
    do {
      sr = poll(pfd, 1, 0);
    } while ((sr == -1) && (errno == EINTR));

    if (sr)
      return sr;
  }
# else
  {
    DECL_SOCK_FDSET(readfds);
    DECL_SOCK_FDSET(exnfds);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_SOCK_RD_FDSET(readfds);
    INIT_DECL_SOCK_ER_FDSET(exnfds);
    
    RKTIO_SOCK_FD_ZERO(readfds);
    RKTIO_SOCK_FD_SET(udp->s, readfds);
    RKTIO_SOCK_FD_ZERO(exnfds);
    RKTIO_SOCK_FD_SET(udp->s, exnfds);
    
    do {
      sr = select(udp->s + 1, readfds, NULL, exnfds, &time);
    } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));
    
    if (sr)
      return sr;
  }
# endif

  check_fd_sema(udp->s, MZFD_CREATE_READ, sinfo, NULL);

  return 0;
}

static void udp_recv_needs_wakeup(Scheme_Object *_udp, void *fds)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;
  void *fds1, *fds2;

  rktio_socket_t s = udp->s;
  
  fds1 = RKTIO_GET_FDSET(fds, 0);
  fds2 = RKTIO_GET_FDSET(fds, 2);
  
  RKTIO_FD_SET(s, (fd_set *)fds1);
  RKTIO_FD_SET(s, (fd_set *)fds2);
}

#endif

static int do_udp_recv(const char *name, Scheme_UDP *udp, char *bstr, intptr_t start, intptr_t end, 
		       int can_block, Scheme_Object **v)
{
#ifdef UDP_IS_SUPPORTED
  intptr_t x;
  int errid = 0;
  char src_addr[RKTIO_SOCK_NAME_MAX_LEN];
  unsigned int asize = sizeof(src_addr);

  if (!udp->bound) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: udp socket is not bound\n"
                     "  socket: %V",
		     name,
		     udp);
    return 0;
  }

  while (1) {
    if (udp->s == INVALID_SOCKET) {
      /* socket was closed, maybe while we slept */
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: udp socket is closed\n"
                       "  socket: %V",
		       name, udp);
      return 0;
    }

    {
      if (end == start) {
        /* recvfrom() doesn't necessarily wait if you pass a buffer size of 0;
           to be consistent with accepting a message but discarding bytes that
           don't fit, accept at least one byte and turn a `1` result into `0` */
        char buf[1];
        x = recvfrom(udp->s, buf, 1, 0, (struct sockaddr *)src_addr, &asize);
        if (x == 1)
          x = 0;
      } else {
        x = recvfrom(udp->s, bstr XFORM_OK_PLUS start, end - start, 0,
                     (struct sockaddr *)src_addr, &asize);
      }
    }

    if (x == -1) {
      errid = SOCK_ERRNO();
      if (WAS_ECONNREFUSED(errid)) {
        /* Delayed ICMP error. Ignore it and try again. */
        errid = 0;
      } else if (WAS_WSAEMSGSIZE(errid)) {
	x = end - start;
	errid = 0;
	break;
      } else if (WAS_EAGAIN(errid)) {
	if (can_block) {
	  /* Block and eventually try again. */
          Scheme_Object *sema;
          sema = scheme_fd_to_semaphore(udp->s, MZFD_CREATE_READ, 1);
          if (sema)
            scheme_wait_sema(sema, 0);
          else
            scheme_block_until((Scheme_Ready_Fun)udp_check_recv, 
                               udp_recv_needs_wakeup, 
                               (Scheme_Object *)udp,
                               0);
	} else {
	  v[0] = scheme_false;
	  v[1] = scheme_false;
	  v[2] = scheme_false;
	  return 0;
	}
      } else if (NOT_WINSOCK(errid) != EINTR)
	break;
    } else
      break;
  }
  
  if (x > -1) {
    char host_buf[RKTIO_SOCK_HOST_NAME_MAX_LEN];
    char prev_buf[RKTIO_SOCK_HOST_NAME_MAX_LEN];
    char svc_buf[RKTIO_SOCK_SVC_NAME_MAX_LEN];
    int j, id;

    v[0] = scheme_make_integer(x);

    scheme_getnameinfo((struct sockaddr *)src_addr, asize,
		       host_buf, sizeof(host_buf),
		       svc_buf, sizeof(svc_buf));
    
    if (udp->previous_from_addr) {
      mzchar *s;
      s = SCHEME_CHAR_STR_VAL(udp->previous_from_addr);
      for (j = 0; s[j]; j++) {
	prev_buf[j] = (char)s[j];
      }
      prev_buf[j] = 0;
    }

    if (udp->previous_from_addr && !strcmp(prev_buf, host_buf)) {
      v[1] = udp->previous_from_addr;
    } else {
      Scheme_Object *vv;
      vv = scheme_make_immutable_sized_utf8_string(host_buf, -1);
      v[1] = vv;
      udp->previous_from_addr = v[1];
    }

    id = extract_svc_value(svc_buf);

    v[2] = scheme_make_integer(id);

    return 1;
  } else {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: receive failed\n"
                     "  system error: %E",
		     name,
		     errid);
    return 0;
  }
#else
  return 0;
#endif
}

static Scheme_Object *udp_recv(const char *name, int argc, Scheme_Object *argv[], 
			       int can_block, Scheme_UDP_Evt *fill_evt)
{
  Scheme_UDP *udp;
  intptr_t start, end;
  Scheme_Object *v[3];

  udp = (Scheme_UDP *)argv[0];

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);
  if (!SCHEME_BYTE_STRINGP(argv[1]) || !SCHEME_MUTABLEP(argv[1]))
    scheme_wrong_contract(name, "(or/c bytes? (not/c immutable?))", 1, argc, argv);
  
#ifdef UDP_IS_SUPPORTED
  scheme_get_substring_indices(name, argv[1], 
			       argc, argv,
			       2, 3, &start, &end);
  if (fill_evt) {
    fill_evt->str = SCHEME_BYTE_STR_VAL(argv[1]);
    fill_evt->offset = start;
    fill_evt->len = end - start;
    return scheme_void;
  } else {
    do_udp_recv(name, udp, SCHEME_BYTE_STR_VAL(argv[1]), start, end, can_block, v);
    
    return scheme_values(3,v);
  }
#else
  return NULL;
#endif
}

static Scheme_Object *udp_receive(int argc, Scheme_Object *argv[])
{
  return udp_recv("udp-receive!", argc, argv, 1, NULL);
}

static Scheme_Object *udp_receive_star(int argc, Scheme_Object *argv[])
{
  return udp_recv("udp-receive!*", argc, argv, 0, NULL);
}

static Scheme_Object *udp_receive_enable_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(udp_receive, argc, argv);
}

static Scheme_Object *make_udp_evt(const char *name, int argc, Scheme_Object **argv, int for_read)
{
#ifdef UDP_IS_SUPPORTED
  Scheme_UDP_Evt *uw;
#endif

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  uw = MALLOC_ONE_TAGGED(Scheme_UDP_Evt);
  uw->so.type = scheme_udp_evt_type;
  uw->udp = (Scheme_UDP *)argv[0];
  uw->for_read = for_read;

  return (Scheme_Object *)uw;
#else
  return scheme_void;
#endif
}

static Scheme_Object *udp_read_ready_evt(int argc, Scheme_Object *argv[])
{
  return make_udp_evt("udp-receive-ready-evt", argc, argv, 1);
}

static Scheme_Object *udp_write_ready_evt(int argc, Scheme_Object *argv[])
{
  return make_udp_evt("udp-send-ready-evt", argc, argv, 0);
}

static Scheme_Object *udp_read_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *evt;
  evt = make_udp_evt("udp-receive!-evt", argc, argv, 1);
  udp_recv("udp-receive!-evt", argc, argv, 0, (Scheme_UDP_Evt *)evt);
  return evt;
}

static Scheme_Object *udp_write_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *evt;
  evt = make_udp_evt("udp-send-evt", argc, argv, 0);
  udp_send_it("udp-send-evt", argc, argv, 0, 0, (Scheme_UDP_Evt *)evt);
  return evt;
}

static Scheme_Object *udp_write_to_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *evt;
  evt = make_udp_evt("udp-send-to-evt", argc, argv, 0);
  udp_send_it("udp-send-to-evt", argc, argv, 1, 0, (Scheme_UDP_Evt *)evt);
#ifdef UDP_IS_SUPPORTED
  ((Scheme_UDP_Evt *)evt)->with_addr = 1;
#endif
  return evt;
}

#ifdef UDP_IS_SUPPORTED
static int udp_evt_check_ready(Scheme_Object *_uw, Scheme_Schedule_Info *sinfo)
{
  Scheme_UDP_Evt *uw = (Scheme_UDP_Evt *)_uw;

  if (uw->for_read) {
    if (uw->str) {
      Scheme_Object *v[3];
      if (do_udp_recv("udp-receive!-evt", uw->udp, 
		      uw->str, uw->offset, uw->offset + uw->len, 
		      0, v)) {
	scheme_set_sync_target(sinfo, scheme_build_list(3, v), NULL, NULL, 0, 0, NULL);
	return 1;
      } else
	return 0;
    } else {
      return udp_check_recv((Scheme_Object *)uw->udp, NULL);
    }
  } else {
    if (uw->str) {
      Scheme_Object *r = NULL;
      int j;
      for (j = 0; !r && (j < (uw->dest_addrs ? uw->dest_addr_count : 1)); j++) {
        r = do_udp_send_it("udp-send-evt", uw->udp, 
                           uw->str, uw->offset, uw->offset + uw->len, 
                           uw->dest_addrs ? uw->dest_addrs[j] : NULL,
                           uw->dest_addrs ? uw->dest_addr_lens[j] : 0,
                           0,
                           j+1 < uw->dest_addr_count);
      }
      if (SCHEME_TRUEP(r)) {
	scheme_set_sync_target(sinfo, scheme_void, NULL, NULL, 0, 0, NULL);
	return 1;
      } else
	return 0;
    } else
      return udp_check_send((Scheme_Object *)uw->udp, NULL);
  }
}

static void udp_evt_needs_wakeup(Scheme_Object *_uw, void *fds)
{
  Scheme_UDP_Evt *uw = (Scheme_UDP_Evt *)_uw;

  if (uw->for_read)
    udp_recv_needs_wakeup((Scheme_Object *)uw->udp, fds);
  else
    udp_send_needs_wakeup((Scheme_Object *)uw->udp, fds);
}
#endif

static int udp_check_open(char const *name, int argc, Scheme_Object *argv[]) {
  if (!SCHEME_UDPP(argv[0])) {
    scheme_wrong_contract(name, "udp?", 0, argc, argv);
    return 0; /* Why does no-one else expect control back after scheme_wrong_contract? */
    /* Or, conversely, why does everyone expect control back after scheme_raise_exn? */
  }

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp = (Scheme_UDP *) argv[0];

    if (udp->s == INVALID_SOCKET) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "%s: udp socket was already closed\n"
                       "  socket: %V",
                       name, udp);
      return 0;
    }

    return 1;
  }
#else
  return 0;
#endif
}

static Scheme_Object *
udp_multicast_loopback_p(int argc, Scheme_Object *argv[])
{
  if (!udp_check_open("udp-multicast-loopback?", argc, argv))
    return NULL;

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp = (Scheme_UDP *) argv[0];
    u_char loop;
    unsigned int loop_len = sizeof(loop);
    int status;
    status = getsockopt(udp->s, IPPROTO_IP, IP_MULTICAST_LOOP, (void *) &loop, &loop_len);
    if (status)
      status = SOCK_ERRNO();
    if (status) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "udp-multicast-loopback?: getsockopt failed\n"
		       "  system error: %N",
		       0, status);
      return NULL;
    } else {
      return (loop ? scheme_true : scheme_false);
    }
  }
#else
  return scheme_void;
#endif
}

static Scheme_Object *
udp_multicast_set_loopback(int argc, Scheme_Object *argv[])
{
  if (!udp_check_open("udp-multicast-set-loopback!", argc, argv))
    return NULL;

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp = (Scheme_UDP *) argv[0];
    u_char loop = SCHEME_TRUEP(argv[1]) ? 1 : 0;
    unsigned int loop_len = sizeof(loop);
    int status;
    status = setsockopt(udp->s, IPPROTO_IP, IP_MULTICAST_LOOP, (void *) &loop, loop_len);
    if (status)
      status = SOCK_ERRNO();
    if (status) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "udp-multicast-set-loopback!: setsockopt failed\n"
		       "  system error: %N",
		       0, status);
      return NULL;
    } else {
      return scheme_void;
    }
  }
#else
  return scheme_void;
#endif
}

static Scheme_Object *
udp_multicast_ttl(int argc, Scheme_Object *argv[])
{
  if (!udp_check_open("udp-multicast-ttl", argc, argv))
    return NULL;

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp = (Scheme_UDP *) argv[0];
    u_char ttl;
    unsigned int ttl_len = sizeof(ttl);
    int status;
    status = getsockopt(udp->s, IPPROTO_IP, IP_MULTICAST_TTL, (void *) &ttl, &ttl_len);
    if (status)
      status = SOCK_ERRNO();
    if (status) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "udp-multicast-ttl: getsockopt failed\n"
		       "  system error: %N",
		       0, status);
      return NULL;
    } else {
      return scheme_make_integer(ttl);
    }
  }
#else
  return scheme_void;
#endif
}

static Scheme_Object *
udp_multicast_set_ttl(int argc, Scheme_Object *argv[])
{
  if (!udp_check_open("udp-multicast-set-ttl!", argc, argv))
    return NULL;
  if (!SCHEME_INTP(argv[1]) || (SCHEME_INT_VAL(argv[1]) < 0) || (SCHEME_INT_VAL(argv[1]) >= 256)) {
    scheme_wrong_contract("udp-multicast-set-ttl!", "byte?", 1, argc, argv);
    return NULL;
  }

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp = (Scheme_UDP *) argv[0];
    u_char ttl = (u_char) SCHEME_INT_VAL(argv[1]);
    unsigned int ttl_len = sizeof(ttl);
    int status;
    status = setsockopt(udp->s, IPPROTO_IP, IP_MULTICAST_TTL, (void *) &ttl, ttl_len);
    if (status)
      status = SOCK_ERRNO();
    if (status) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "udp-multicast-set-ttl!: setsockopt failed\n"
		       "  system error: %N",
		       0, status);
      return NULL;
    } else {
      return scheme_void;
    }
  }
#else
  return scheme_void;
#endif
}

static Scheme_Object *
udp_multicast_interface(int argc, Scheme_Object *argv[])
{
  if (!udp_check_open("udp-multicast-interface", argc, argv))
    return NULL;

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp = (Scheme_UDP *) argv[0];
    GC_CAN_IGNORE struct in_addr intf;
    unsigned int intf_len = sizeof(intf);
    int status;
    status = getsockopt(udp->s, IPPROTO_IP, IP_MULTICAST_IF, (void *) &intf, &intf_len);
    if (status)
      status = SOCK_ERRNO();
    if (status) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "udp-multicast-interface: getsockopt failed\n"
		       "  system error: %N",
		       0, status);
      return NULL;
    } else {
      char host_buf[RKTIO_SOCK_HOST_NAME_MAX_LEN];
      unsigned char *b = (unsigned char *) &intf; /* yes, this is in network order */
      sprintf(host_buf, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
      return scheme_make_utf8_string(host_buf);
    }
  }
#else
  return scheme_void;
#endif
}

static Scheme_Object *
udp_multicast_set_interface(int argc, Scheme_Object *argv[])
{
  if (!udp_check_open("udp-multicast-set-interface!", argc, argv))
    return NULL;
  if (!SCHEME_CHAR_STRINGP(argv[1]) && !SCHEME_FALSEP(argv[1])) {
    scheme_wrong_contract("udp-multicast-set-interface!", "(or/c string? #f)", 1, argc, argv);
    return NULL;
  }

#ifdef UDP_IS_SUPPORTED
  {
    Scheme_UDP *udp = (Scheme_UDP *) argv[0];
    GC_CAN_IGNORE struct in_addr intf;
    unsigned int intf_len = sizeof(intf);
    int status;

    if (SCHEME_FALSEP(argv[1])) {
      intf.s_addr = INADDR_ANY;
    } else {
      Scheme_Object *bs;
      char *address = "";
      GC_CAN_IGNORE struct rktio_addrinfo_t *if_addr = NULL;
      int err;
      bs = scheme_char_string_to_byte_string(argv[1]);
      address = SCHEME_BYTE_STR_VAL(bs);
      if_addr = scheme_get_host_address(address, -1, &err, RKTIO_PF_INET, 0, 0);
      if (!if_addr) {
	scheme_raise_exn(MZEXN_FAIL_NETWORK,
			 "udp-multicast-set-interface!: can't resolve interface address\n"
			 "  address: %s\n"
			 "  system error: %N",
			 address ? address : "<unspec>", 1, err);
	return NULL;
      }
      intf = ((struct sockaddr_in *)if_addr->ai_addr)->sin_addr;
      mz_freeaddrinfo(if_addr);
    }

    status = setsockopt(udp->s, IPPROTO_IP, IP_MULTICAST_IF, (void *) &intf, intf_len);
    if (status)
      status = SOCK_ERRNO();
    if (status) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "udp-multicast-set-interface!: setsockopt failed\n"
		       "  system error: %N",
		       0, status);
      return NULL;
    } else {
      return scheme_void;
    }
  }
#else
  return scheme_void;
#endif
}

#ifdef UDP_IS_SUPPORTED

static Scheme_Object *
do_udp_multicast_join_or_leave_group(char const *name, int optname, Scheme_UDP *udp, Scheme_Object *multiaddrname, Scheme_Object *ifaddrname)
{
  GC_CAN_IGNORE struct ip_mreq mreq;
  unsigned int mreq_len = sizeof(mreq);
  int status;

  if (SCHEME_FALSEP(ifaddrname)) {
    mreq.imr_interface.s_addr = INADDR_ANY;
  } else {
    Scheme_Object *bs;
    char *address = "";
    GC_CAN_IGNORE struct rktio_addrinfo_t *if_addr = NULL;
    int err;
    bs = scheme_char_string_to_byte_string(ifaddrname);
    address = SCHEME_BYTE_STR_VAL(bs);
    if_addr = scheme_get_host_address(address, -1, &err, RKTIO_PF_INET, 0, 0);
    if (!if_addr) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: can't resolve interface address\n"
		       "  address: %s\n"
		       "  system error: %N",
		       name, address ? address : "<unspec>", 1, err);
      return NULL;
    }
    mreq.imr_interface = ((struct sockaddr_in *)if_addr->ai_addr)->sin_addr;
    mz_freeaddrinfo(if_addr);
  }

  {
    Scheme_Object *bs;
    char *address = "";
    GC_CAN_IGNORE struct rktio_addrinfo_t *group_addr = NULL;
    int err;
    bs = scheme_char_string_to_byte_string(multiaddrname);
    address = SCHEME_BYTE_STR_VAL(bs);
    group_addr = scheme_get_host_address(address, -1, &err, RKTIO_PF_INET, 0, 0);
    if (!group_addr) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: can't resolve group address\n"
		       "  address: %s\n"
		       "  system error: %N",
		       name, address ? address : "<unspec>", 1, err);
      return NULL;
    }
    mreq.imr_multiaddr = ((struct sockaddr_in *)group_addr->ai_addr)->sin_addr;
    mz_freeaddrinfo(group_addr);
  }

  status = setsockopt(udp->s, IPPROTO_IP, optname, (void *) &mreq, mreq_len);
  if (status)
    status = SOCK_ERRNO();
  if (status) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: setsockopt failed\n"
		     "  system error: %N",
		     name, 0, status);
    return NULL;
  } else {
    return scheme_void;
  }
}

#endif

static Scheme_Object *
udp_multicast_join_or_leave_group(char const *name, int optname, int argc, Scheme_Object *argv[])
{
  if (!udp_check_open(name, argc, argv))
    return NULL;
  if (!SCHEME_CHAR_STRINGP(argv[1])) {
    scheme_wrong_contract(name, "string?", 1, argc, argv);
    return NULL;
  }
  if (!SCHEME_CHAR_STRINGP(argv[2]) && !SCHEME_FALSEP(argv[2])) {
    scheme_wrong_contract(name, "(or/c string? #f)", 2, argc, argv);
    return NULL;
  }

#ifdef UDP_IS_SUPPORTED
  return do_udp_multicast_join_or_leave_group(name, optname,
					      (Scheme_UDP *) argv[0], argv[1], argv[2]);
#else
  return scheme_void;
#endif
}

#ifdef UDP_IS_SUPPORTED
# define WHEN_UDP_IS_SUPPORTED(x) x
#else
# define WHEN_UDP_IS_SUPPORTED(x) 0
#endif

static Scheme_Object *
udp_multicast_join_group(int argc, Scheme_Object *argv[])
{
  return udp_multicast_join_or_leave_group("udp-multicast-join-group!",
					   WHEN_UDP_IS_SUPPORTED(IP_ADD_MEMBERSHIP),
					   argc,
					   argv);
}

static Scheme_Object *
udp_multicast_leave_group(int argc, Scheme_Object *argv[])
{
  return udp_multicast_join_or_leave_group("udp-multicast-leave-group!",
					   WHEN_UDP_IS_SUPPORTED(IP_DROP_MEMBERSHIP),
					   argc,
					   argv);
}

#endif
