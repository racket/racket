/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
  Copyright (c) 2000-2001 Matthew Flatt

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
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file implements the TCP and UDP interfaces. */

#include "schpriv.h"
#include <ctype.h>

#ifndef NO_TCP_SUPPORT
#ifdef USE_TCP

#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_ULIMIT
# include <ulimit.h>
#endif
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef BSTRING_INCLUDE
#  include <bstring.h>
# endif
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#ifdef INCLUDE_OSKIT_SOCKET
# include <oskit/net/socket.h>
#endif
#ifdef NO_ERRNO_GLOBAL
static int mzerrno = 0;
# define errno mzerrno
#else
# include <errno.h>
#endif
#include "schfd.h"

#ifdef USE_MAC_TCP
# define USESROUTINEDESCRIPTORS 1
# include <MacTCP.h>
# include "macdnr.inc"
#endif

#ifdef USE_UNIX_SOCKETS_TCP
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# include <fcntl.h>
# define TCP_SOCKSENDBUF_SIZE 32768
# define NOT_WINSOCK(x) x
# define SOCK_ERRNO() errno
# define WAS_EAGAIN(e) ((e == EWOULDBLOCK) || (e == EAGAIN) || (e == EINPROGRESS) || (e == EALREADY))
# define WAS_WSAEMSGSIZE(e) 0
# define mz_AFNOSUPPORT EAFNOSUPPORT
#endif

#ifdef USE_WINSOCK_TCP
# include <process.h>
# include <winsock.h>
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
# define mz_AFNOSUPPORT WSAEAFNOSUPPORT
extern int scheme_stupid_windows_machine;
#endif

#ifdef USE_MAC_TCP
# define TCP_BUFFER_SIZE 16384
#else
# define TCP_BUFFER_SIZE 4096
#endif

#ifdef USE_UNIX_SOCKETS_TCP
typedef long tcp_t;
# define INVALID_SOCKET (-1)
static void closesocket(long s) {
  int cr;
  do { 
    cr = close(s);
  } while ((cr == -1) && (errno == EINTR));
}
#endif

#ifdef USE_WINSOCK_TCP
typedef SOCKET tcp_t;
#endif

#ifdef USE_SOCKETS_TCP
typedef struct {
  Scheme_Object so;
  tcp_t s;
  Scheme_Custodian_Reference *mref;
} listener_t;
#endif

#ifdef USE_MAC_TCP
typedef struct tcp_t {
  void *create_pb;
  void *current_pb; /* prevents GC during async call */
  StreamPtr stream;
  int state;
  int async_errid;
  Scheme_Object *lock; /* read lock */
} tcp_t;

typedef struct {
  Scheme_Object so;
  int hostid;
  int portid;
  int count;
  struct Scheme_Tcp **datas;
  Scheme_Custodian_Reference *mref;
} listener_t;
# define htons(x) x
# define htonl(x) x
#endif

typedef struct Scheme_Tcp_Buf {
  MZTAG_IF_REQUIRED
  short refcount;
  char *buffer, *out_buffer;
  short bufpos, bufmax;
  short hiteof, bufmode;
  short out_bufpos, out_bufmax;
  short out_bufmode;
} Scheme_Tcp_Buf;

typedef struct Scheme_Tcp {
  Scheme_Tcp_Buf b;
  tcp_t tcp;
#ifdef USE_MAC_TCP
  struct TCPiopb *activeRcv;
#endif
  int flags;
} Scheme_Tcp;

# define MZ_TCP_ABANDON_OUTPUT 0x1
# define MZ_TCP_ABANDON_INPUT  0x2

#ifdef USE_MAC_TCP
static int num_tcp_send_buffers = 0;
static void **tcp_send_buffers;
#endif

#ifndef USE_MAC_TCP
/* UDP not supported in Mac OS Classic */
# define UDP_IS_SUPPORTED
#endif

#ifdef UDP_IS_SUPPORTED

typedef struct Scheme_UDP {
  Scheme_Object so; /* scheme_udp_type */
  MZ_HASH_KEY_EX
  tcp_t s; /* ok, tcp_t was a bad name */
  char bound, connected;
  Scheme_Object *previous_from_addr;
  Scheme_Custodian_Reference *mref;
} Scheme_UDP;

#endif /* UDP_IS_SUPPORTED */

#endif /* USE_TCP */

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_connect_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listen(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_stop(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept_ready(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_addresses(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_abandon_port(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_port_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_udp(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_close(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_bound_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_connected_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_bind(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_connect(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_send_to(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_send(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_send_to_star(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_send_star(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_send_to_enable_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_send_enable_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_receive(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_receive_star(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_receive_enable_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_read_ready_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_write_ready_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_read_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_write_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_write_to_evt(int argc, Scheme_Object *argv[]);

static int tcp_check_accept_evt(Scheme_Object *ae, Scheme_Schedule_Info *sinfo);
static void tcp_accept_evt_needs_wakeup(Scheme_Object *_ae, void *fds);
#ifdef UDP_IS_SUPPORTED
static int udp_evt_check_ready(Scheme_Object *uw, Scheme_Schedule_Info *sinfo);
static void udp_evt_needs_wakeup(Scheme_Object *_uw, void *fds);
#endif

static void register_tcp_listener_sync();

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_network(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

#ifdef USE_MAC_TCP
  REGISTER_SO(tcp_send_buffers);
#endif    

  scheme_add_global_constant("tcp-connect", 
			     scheme_make_prim_w_arity2(tcp_connect,
						       "tcp-connect", 
						       2, 4,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-connect/enable-break", 
			     scheme_make_prim_w_arity2(tcp_connect_break,
						       "tcp-connect/enable-break", 
						       2, 4,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listen", 
			     scheme_make_prim_w_arity(tcp_listen,
						      "tcp-listen", 
						      1, 4),
			     env);
  scheme_add_global_constant("tcp-close", 
			     scheme_make_prim_w_arity(tcp_stop,
						      "tcp-close", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept-ready?", 
			     scheme_make_prim_w_arity(tcp_accept_ready,
						      "tcp-accept-ready?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept", 
			     scheme_make_prim_w_arity2(tcp_accept,
						       "tcp-accept", 
						       1, 1,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-accept-evt", 
			     scheme_make_prim_w_arity(tcp_accept_evt,
						      "tcp-accept-evt", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept/enable-break", 
			     scheme_make_prim_w_arity2(tcp_accept_break,
						       "tcp-accept/enable-break", 
						       1, 1,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listener?", 
			     scheme_make_folding_prim(tcp_listener_p,
						      "tcp-listener?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("tcp-addresses", 
			     scheme_make_prim_w_arity2(tcp_addresses,
						       "tcp-addresses", 
						       1, 1,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-abandon-port", 
			     scheme_make_prim_w_arity(tcp_abandon_port,
						      "tcp-abandon-port", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-port?", 
			     scheme_make_folding_prim(tcp_port_p,
						      "tcp-port?", 
						      1, 1, 1), 
			     env);

  scheme_add_global_constant("udp-open-socket", 
			     scheme_make_prim_w_arity(make_udp,
						      "udp-open-socket", 
						      0, 0), 
			     env);
  scheme_add_global_constant("udp-close", 
			     scheme_make_prim_w_arity(udp_close,
						      "udp-close", 
						      1, 1), 
			     env);
  scheme_add_global_constant("udp?", 
			     scheme_make_folding_prim(udp_p,
						      "udp?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("udp-bound?", 
			     scheme_make_prim_w_arity(udp_bound_p,
						      "udp-bound?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("udp-connected?", 
			     scheme_make_prim_w_arity(udp_connected_p,
						      "udp-connected?", 
						      1, 1), 
			     env);

  scheme_add_global_constant("udp-bind!", 
			     scheme_make_prim_w_arity(udp_bind,
						      "udp-bind!", 
						      3, 3), 
			     env);
  scheme_add_global_constant("udp-connect!", 
			     scheme_make_prim_w_arity(udp_connect,
						      "udp-connect!", 
						      3, 3), 
			     env);

  scheme_add_global_constant("udp-send-to", 
			     scheme_make_prim_w_arity(udp_send_to,
						      "udp-send-to", 
						      4, 6), 
			     env);
  scheme_add_global_constant("udp-send", 
			     scheme_make_prim_w_arity(udp_send,
						      "udp-send", 
						      2, 4), 
			     env);
  scheme_add_global_constant("udp-send-to*", 
			     scheme_make_prim_w_arity(udp_send_to_star,
						      "udp-send-to*", 
						      4, 6), 
			     env);
  scheme_add_global_constant("udp-send*", 
			     scheme_make_prim_w_arity(udp_send_star,
						      "udp-send*", 
						      2, 4), 
			     env);
  scheme_add_global_constant("udp-send-to/enable-break", 
			     scheme_make_prim_w_arity(udp_send_to_enable_break,
						      "udp-send-to/enable-break", 
						      4, 6), 
			     env);
  scheme_add_global_constant("udp-send/enable-break", 
			     scheme_make_prim_w_arity(udp_send_enable_break,
						      "udp-send/enable-break", 
						      2, 4), 
			     env);

  scheme_add_global_constant("udp-receive!", 
			     scheme_make_prim_w_arity(udp_receive,
						      "udp-receive!", 
						      2, 4), 
			     env);
  scheme_add_global_constant("udp-receive!*", 
			     scheme_make_prim_w_arity(udp_receive_star,
						      "udp-receive!*", 
						      2, 4), 
			     env);
  scheme_add_global_constant("udp-receive!/enable-break", 
			     scheme_make_prim_w_arity(udp_receive_enable_break,
						      "udp-receive!/enable-break", 
						      2, 4), 
			     env);
  scheme_add_global_constant("udp-receive-ready-evt", 
			     scheme_make_prim_w_arity(udp_read_ready_evt,
						      "udp-receive-ready-evt", 
						      1, 1), 
			     env);
  scheme_add_global_constant("udp-send-ready-evt", 
			     scheme_make_prim_w_arity(udp_write_ready_evt,
						      "udp-send-ready-evt", 
						      1, 1), 
			     env);
  scheme_add_global_constant("udp-receive!-evt", 
			     scheme_make_prim_w_arity(udp_read_evt,
						      "udp-receive!-evt", 
						      2, 4), 
			     env);
  scheme_add_global_constant("udp-send-evt", 
			     scheme_make_prim_w_arity(udp_write_evt,
						      "udp-send-evt", 
						      2, 4), 
			     env);
  scheme_add_global_constant("udp-send-to-evt", 
			     scheme_make_prim_w_arity(udp_write_to_evt,
						      "udp-send-to-evt", 
						      4, 6), 
			     env);

  register_tcp_listener_sync();
}


/*========================================================================*/
/*                             TCP glue                                   */
/*========================================================================*/


/* These two need o be outside of USE_TCP */
#define PORT_ID_TYPE "exact integer in [1, 65535]"
#define CHECK_PORT_ID(obj) (SCHEME_INTP(obj) && (SCHEME_INT_VAL(obj) >= 1) && (SCHEME_INT_VAL(obj) <= 65535))

#ifdef USE_TCP

#ifdef USE_SOCKETS_TCP
#define MAKE_TCP_ARG tcp_t tcp, 
#else
#define MAKE_TCP_ARG
#endif

#define REGISTER_SOCKET(s) /**/
#define UNREGISTER_SOCKET(s) /**/

#ifdef USE_UNIX_SOCKETS_TCP
typedef struct sockaddr_in tcp_address;
#endif
#ifdef USE_WINSOCK_TCP
typedef struct SOCKADDR_IN tcp_address;
# undef REGISTER_SOCKET
# undef UNREGISTER_SOCKET
# define REGISTER_SOCKET(s) winsock_remember(s)
# define UNREGISTER_SOCKET(s) winsock_forget(s)
#endif

/******************************* hostnames ************************************/

static int parse_numerical(const char *address, unsigned long *addr)
{
  unsigned char *s = (unsigned char *)address, n[4];
  int p = 0, v = 0, vs[4];
  while (*s) {
    if (isdigit(*s)) {
      if (v < 256)
	v = (v * 10) + ((*s) - '0');
    } else if (*s == '.') {
      if (p < 4) {
	vs[p] = v;
	n[p] = (unsigned char)v;
	p = p XFORM_OK_PLUS 1;
      }
      v = 0;
    } else
      break;
    s = s XFORM_OK_PLUS 1;
  }
     
  if (p == 3) {
    vs[p] = v;
    n[p] = (unsigned char)v;
    p++;
  }
     
  if (!*s && (p == 4)
      && (vs[0] < 256) && (vs[1] < 256)
      && (vs[2] < 256) && (vs[3] < 256)) {
    /* Numerical address */
    *addr = *(unsigned long *)n;
    return 1;
  }

  return 0;
}

#ifdef OS_X
# define PTHREADS_OK_FOR_GHBN
#endif

#if defined(USE_WINSOCK_TCP) || defined(PTHREADS_OK_FOR_GHBN)

# ifdef USE_WINSOCK_TCP
#  ifdef __BORLANDC__
#   define MZ_LPTHREAD_START_ROUTINE unsigned int (__stdcall*)(void*)
#  else
#   define MZ_LPTHREAD_START_ROUTINE LPTHREAD_START_ROUTINE
#  endif
# else
#  include <pthread.h>
#   define MZ_LPTHREAD_START_ROUTINE void *(*)(void *)
# endif

static volatile int ghbn_lock;

typedef struct {
# ifdef USE_WINSOCK_TCP
  HANDLE th;
# else
  int pin;
# endif
  long result;
  int done;
} GHBN_Rec;

static char ghbn_hostname[256];
# ifdef USE_WINSOCK_TCP
HANDLE ready_sema;
# else
int ready_fd;
# endif

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

static long gethostbyname_in_thread(void *data)
{
  struct hostent *host;
  char hn_copy[256];
  long v;
# ifndef USE_WINSOCK_TCP
  int fd = ready_fd;
# endif
  
  memcpy(hn_copy, ghbn_hostname, 256);

# ifdef USE_WINSOCK_TCP
  ReleaseSemaphore(ready_sema, 1, NULL);  
# else
  write(fd, "?", 1);
# endif

  host = gethostbyname(hn_copy);

  if (host)
    v = *(long *) mzALIAS host->h_addr_list[0];
  else
    v = 0;

# ifndef USE_WINSOCK_TCP
  write(fd, &v, sizeof(v));
  close(fd);
# endif

  return v;
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

static void release_ghbn_lock(GHBN_Rec *rec)
{
  ghbn_lock = 0;
# ifdef USE_WINSOCK_TCP
  CloseHandle(rec->th);
# else
  close(rec->pin);
# endif
}

static int ghbn_lock_avail(Scheme_Object *_ignored)
{
  return !ghbn_lock;
}

static int ghbn_thread_done(Scheme_Object *_rec)
{
  GHBN_Rec *rec = (GHBN_Rec *)_rec;

  if (rec->done)
    return 1;

# ifdef USE_WINSOCK_TCP
  if (WaitForSingleObject(rec->th, 0) == WAIT_OBJECT_0) {
    DWORD code;

    GetExitCodeThread(rec->th, &code);
    rec->result = code;
    rec->done = 1;

    return 1;
  }
# else
  {
    long v;
    if (read(rec->pin, &v, sizeof(long)) > 0) {
      rec->result = v;
      rec->done = 1;
      return 1;
    }
  }
# endif

  return 0;
}

static void ghbn_thread_need_wakeup(Scheme_Object *_rec, void *fds)
{
  GHBN_Rec *rec = (GHBN_Rec *)_rec;

# ifdef USE_WINSOCK_TCP
  scheme_add_fd_handle((void *)rec->th, fds, 0);
# else
  {
    void *fds2;
    
    fds2 = MZ_GET_FDSET(fds, 2);
    
    MZ_FD_SET(rec->pin, (fd_set *)fds);
    MZ_FD_SET(rec->pin, (fd_set *)fds2);
  }
# endif
}

#define HOST_RESULT_IS_ADDR
static struct hostent *MZ_GETHOSTBYNAME(const char *name)
{
  GHBN_Rec *rec;
  int ok;

  if (strlen(name) > 255)
    return NULL;

  rec = MALLOC_ONE_ATOMIC(GHBN_Rec);
  rec->done = 0;

  scheme_block_until(ghbn_lock_avail, NULL, NULL, 0);

  ghbn_lock = 1;

  strcpy(ghbn_hostname, name);

# ifdef USE_WINSOCK_TCP
  {
    DWORD id;
    long th;
    
    ready_sema = CreateSemaphore(NULL, 0, 1, NULL);
    th = _beginthreadex(NULL, 5000, 
			(MZ_LPTHREAD_START_ROUTINE)gethostbyname_in_thread,
			NULL, 0, &id);
    WaitForSingleObject(ready_sema, INFINITE);
    CloseHandle(ready_sema);
    
    rec->th = (HANDLE)th;
    ok = 1;
  }
# else
  {
    int p[2];
    if (pipe(p)) {
      ok = 0;
    } else {
      pthread_t t;
      rec->pin = p[0];
      ready_fd = p[1];
      if (pthread_create(&t, NULL, 
			 (MZ_LPTHREAD_START_ROUTINE)gethostbyname_in_thread,
			 NULL)) {
	close(p[0]);
	close(p[1]);
	ok = 0;
      } else {
	char buf[1];
	pthread_detach(t);
	read(rec->pin, buf, 1);
	fcntl(rec->pin, F_SETFL, MZ_NONBLOCKING);
	ok = 1;
      }
    }

    if (!ok) {
      long r;
      r = gethostbyname_in_thread(rec);
      rec->result = r;
    }
  }
# endif

  if (ok) {
    BEGIN_ESCAPEABLE(release_ghbn_lock, rec);
    scheme_block_until(ghbn_thread_done, ghbn_thread_need_wakeup, (Scheme_Object *)rec, 0);
    END_ESCAPEABLE();

# ifdef USE_WINSOCK_TCP
    CloseHandle(rec->th);
# else
    close(rec->pin);
# endif
  }

  ghbn_lock = 0;

  return (struct hostent *)rec->result;
}
#else
# define MZ_GETHOSTBYNAME gethostbyname 
#endif

#ifdef USE_SOCKETS_TCP

static unsigned long by_number_id;
#ifndef HOST_RESULT_IS_ADDR
static unsigned long *by_number_array[2];
static struct hostent by_number_host;
#endif

int scheme_get_host_address(const char *address, int id, void *_result)
{
  tcp_address *result = (tcp_address *)_result;
  struct hostent *host;

  if (address) {
    if (parse_numerical(address, &by_number_id)) {
#ifdef HOST_RESULT_IS_ADDR
      host = (struct hostent *)by_number_id;
#else
      by_number_array[0] = &by_number_id;
      by_number_host.h_addr_list = (char **)by_number_array;
      by_number_host.h_length = sizeof(long);
      host = &by_number_host;
#endif
    } else {
      host = MZ_GETHOSTBYNAME(address);
    }
  } else
    host = NULL;

  if (!address || host) {
    result->sin_family = (id ? AF_INET : AF_UNSPEC);
    result->sin_port = id;
    memset(&(result->sin_addr), 0, sizeof(result->sin_addr));
    memset(&(result->sin_zero), 0, sizeof(result->sin_zero));
    if (host) {
#ifdef HOST_RESULT_IS_ADDR
      memcpy(&result->sin_addr, &host, sizeof(long)); 
#else
      memcpy(&result->sin_addr, host->h_addr_list[0], host->h_length); 
#endif
    }
    return 1;
  } else
    return 0;
}
#endif

/******************************* WinSock ***********************************/

#ifdef USE_WINSOCK_TCP

static int wsr_size = 0;
static tcp_t *wsr_array;

static void winsock_remember(tcp_t s)
{
  int i, new_size;
  tcp_t *naya;

  for (i = 0; i < wsr_size; i++) {
    if (!wsr_array[i]) {
      wsr_array[i] = s;
      return;
    }
  }

  if (!wsr_size) {
    REGISTER_SO(wsr_array);
    new_size = 32;
  } else
    new_size = 2 * wsr_size;

  naya = MALLOC_N_ATOMIC(tcp_t, new_size);
  for (i = 0; i < wsr_size; i++) {
    naya[i] = wsr_array[i];
  }

  naya[wsr_size] = s;

  wsr_array = naya;
  wsr_size = new_size;  
}

static void winsock_forget(tcp_t s)
{
  int i;

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i] == s) {
      wsr_array[i] = (tcp_t)NULL;
      return;
    }
  }
}

static int winsock_done(void)
{
  int i;

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i]) {
      closesocket(wsr_array[i]);
      wsr_array[i] = (tcp_t)NULL;
    }
  }

  return WSACleanup();
}

static void TCP_INIT(char *name)
{
  static int started = 0;
  
  if (!started) {
    WSADATA data;
    if (!WSAStartup(MAKEWORD(1, 1), &data)) {
      started = 1;
#ifdef __BORLANDC__
      atexit((void(*)())winsock_done);
#else      
      _onexit(winsock_done);
#endif
      return;
    }
  } else
    return;
  
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s: not supported on this machine"
		   " (no winsock driver)",
		   name);
}
#else

/********************************** Mac ***************************************/

#ifdef USE_MAC_TCP

/* Much of this is derived from (or at least influenced by) GUSI's TCP
   socket implementation, by Matthias Neeracher, which was derived from
   a library by Charlie Reiman Tom Milligan */

static short tcpDriverId;

static ProcessSerialNumber tcp_psn;

#define	SOCK_STATE_NO_STREAM 0 /* Socket doesn't have a MacTCP stream yet */
#define	SOCK_STATE_UNCONNECTED 1 /* Socket is unconnected. */
#define	SOCK_STATE_LISTENING 2 /* Socket is listening for connection. */
#define	SOCK_STATE_CONNECTING 4 /* Socket is initiating a connection. */
#define	SOCK_STATE_CONNECTED 5 /* Socket is connected. */
#define	SOCK_STATE_EOF_FROM_OTHER 6 /* Socket is half-closed */
#define	SOCK_STATE_CLOSED 8 /* Socket closed nicely */

typedef struct TCPiopbX {
  TCPiopb pb;
  Scheme_Tcp *data;
  struct TCPiopbX *next;
} TCPiopbX;

typedef struct {
  MZTAG_IF_REQUIRED
  wdsEntry e[2];
  TCPiopbX *xpb;
} WriteData;

static TCPiopbX *active_pbs;

static pascal void dnr_done(struct hostInfo *hi, int * done)
{
  *done = true;
  WakeUpProcess(&tcp_psn);
}

static ResultUPP u_dnr_done;

static pascal void tcp_notify(StreamPtr stream, unsigned short eventCode,
			      Ptr userDataPtr, unsigned short something,
			      struct ICMPReport *reportPtr)
{
  tcp_t *t = (tcp_t *)userDataPtr;

  switch (eventCode) {
  case TCPClosing:
    t->state = SOCK_STATE_EOF_FROM_OTHER;
    break;
    
  case TCPTerminate:
    if (t->state == SOCK_STATE_LISTENING)
      t->state = SOCK_STATE_CLOSED;
    else if (t->state == SOCK_STATE_EOF_FROM_OTHER)
      t->state = SOCK_STATE_CLOSED;
    else
      t->state = SOCK_STATE_UNCONNECTED;
    break;
  }

  WakeUpProcess(&tcp_psn);
}

static TCPNotifyUPP u_tcp_notify;

static void tcp_connect_done(TCPiopbX *pbx)
{
  if (!pbx->pb.ioResult)
    pbx->data->tcp.state = SOCK_STATE_CONNECTED;

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_connect_done;

static void tcp_listen_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  switch(pb->ioResult) {
  case noErr:
    data->tcp.state = SOCK_STATE_CONNECTED;
    break;
    
  case openFailed:
  case invalidStreamPtr:
  case connectionExists:
  case duplicateSocket:
  case commandTimeout:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_listen_done;

static void tcp_recv_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;
  
  if (!pb->ioResult)
    data->b.bufmax = pb->csParam.receive.rcvBuffLen;

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_recv_done;

static void tcp_send_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  /* mark it free: */
  ((WriteData *)(pb->csParam.send.wdsPtr))->xpb = NULL;

  switch (pb->ioResult) {
  case noErr:
    break;
  case ipNoFragMemErr:
  case connectionClosing:
  case connectionTerminated:
  case connectionDoesntExist:
  case ipDontFragErr:
  case invalidStreamPtr:
  case invalidLength:
  case invalidWDS:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_send_done;

/************** Mac Set-up *****************/

static void tcp_cleanup(void);

static pascal OSErr (*mzPBOpenSync)(ParmBlkPtr paramBlock);
static pascal OSErr (*mzPBControlSync)(ParmBlkPtr paramBlock);
static pascal OSErr (*mzPBControlAsync)(ParmBlkPtr paramBlock);

static void TCP_INIT(char *name)
{
  ParamBlockRec pb;
  short errNo;
  FSSpec spec;
  CFragConnectionID connID;
  OSErr err;
  void (*f)(...);
  char *netglue;

  GetCurrentProcess(&tcp_psn);

  netglue = scheme_get_exec_path();
  if (netglue) {
    char *file;
    int i;
    i = strlen(netglue);
    file = scheme_malloc_atomic(i + 20);
    memcpy(file, netglue, i);
    for (i--; (i >= 0) && (file[i] != ':'); i--) {
    }
    memcpy(file + i + 1, "netglue", 8);
    netglue = file;
  } else
    netglue = "netglue";

  if (!scheme_mac_path_to_spec(netglue, &spec))
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: TCP initialization error (can't find %q)",
		     name, netglue);

  err = GetDiskFragment(&spec, 0, 0, 0, kPrivateCFragCopy, &connID, 0, NULL);
  if (err != noErr)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: TCP initialization error (can't load %q: %e)",
		     name, netglue, err);
  err = FindSymbol(connID, "\pFillInNetPointers", (Ptr *)&f, 0);
  if (err != noErr)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: TCP initialization error (can't get netglue function: %e)",
		     name, err);

  f(&mzPBOpenSync, &mzPBControlSync, &mzPBControlAsync, 
    &mzSysEnvirons, &mzGetWDInfo, &mzNewRoutineDescriptor,
    &mzCallUniversalProc);

  pb.ioParam.ioCompletion = 0L; 
  pb.ioParam.ioNamePtr = (StringPtr) "\p.IPP"; 
  pb.ioParam.ioPermssn = fsCurPerm;
  
  if ((errNo = mzPBOpenSync(&pb))) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: TCP initialization error (at .IPP; %e)",
		     name, (int)errNo);
  }

  if ((errNo = OpenResolver(NULL))) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: TCP initialization error (at resolver; %e)",
		     name, (int)errNo);
  }
		
  tcpDriverId = pb.ioParam.ioRefNum; 
  
  u_dnr_done = NewResultProc(dnr_done);
  u_tcp_notify = NewTCPNotifyProc(tcp_notify);
  u_tcp_connect_done = NewTCPIOCompletionProc(tcp_connect_done);
  u_tcp_listen_done = NewTCPIOCompletionProc(tcp_listen_done);
  u_tcp_recv_done = NewTCPIOCompletionProc(tcp_recv_done);
  u_tcp_send_done = NewTCPIOCompletionProc(tcp_send_done);
  
  REGISTER_SO(active_pbs);
  
  atexit(tcp_cleanup);
}

long scheme_this_ip(void)
{
  struct GetAddrParamBlock ipBlock;
  OSErr      anErr;
 
  TCP_INIT("system-type");

  ipBlock.csCode = ipctlGetAddr;
  ipBlock.ioCRefNum = tcpDriverId;
 
  anErr = mzPBControlSync((ParmBlkPtr)&ipBlock);
  if (anErr == noErr)
    return ipBlock.ourAddress;
  else
    return 0;
}

static int tcp_addr(const char *address, struct hostInfo *info)
{
  int tries = 3;
  long *done = MALLOC_ONE_ATOMIC(long);
  
  /* Check for numerical address: */
  if (parse_numerical(address, &(info->addr[0])))
    return 0;

 try_again:
  *done = 0;
  info->rtnCode = 0;
  if (StrToAddr((char *)address, info, u_dnr_done, (char *)done) == cacheFault) {
    /* FIXME: If we get a break, it's possible that `info' and `done' will be
              GCed before the async call completes. */
    while (!*done) { scheme_thread_block(0.25); }
    scheme_current_thread->ran_some = 1;
  }
  if (info->rtnCode == cacheFault) {
    if (--tries)
      goto try_again;
  }
  if (info->rtnCode)
    return info->rtnCode;
  if (info->cname[0] == 0)
    return -42;
  
  return 0;
}

int scheme_get_host_address(const char *address, int id, void *_result)
{
  if (tcp_addr(address, (struct hostInfo *)_result)) {
    return 1;
  }
  return 0;
}

/* Forward prototype: */
static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount);

#define STREAM_BUFFER_SIZE 131072

static TCPiopbX *mac_make_xpb(Scheme_Tcp *data)
{
  TCPiopbX *xpb;

  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  
  memcpy(xpb, data->tcp.create_pb, sizeof(TCPiopb));

  xpb->data = data;
  
  data->tcp.current_pb = xpb;

  return xpb;
}

static int mac_tcp_make(TCPiopbX **_xpb, TCPiopb **_pb, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;

  data = make_tcp_port_data(2);
  
  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  xpb->next = active_pbs;
  active_pbs = xpb;
  
  pb = (TCPiopb *)xpb;

  pb->ioCRefNum = tcpDriverId;
  pb->csCode = TCPCreate;
  pb->csParam.create.rcvBuff = (char *)scheme_malloc_atomic(STREAM_BUFFER_SIZE);
  pb->csParam.create.rcvBuffLen = STREAM_BUFFER_SIZE;
  pb->csParam.create.notifyProc = u_tcp_notify;
  pb->csParam.create.userDataPtr = (char *)&data->tcp;
  
  xpb->data = data;
  
  if ((errid = mzPBControlSync((ParamBlockRec*)pb)))
    return errid;
	
  data->tcp.create_pb = (void *)pb;
  data->tcp.stream = pb->tcpStream;
  data->tcp.async_errid = -1;

  *_xpb = xpb;
  *_pb = pb;
  *_data = data;

  return 0;
}

static void mac_tcp_close(Scheme_Tcp *data, int cls, int rel)
{
  TCPiopb *pb;
  
  pb = (TCPiopb *)mac_make_xpb(data);
  
  if (cls) {
    pb->ioCompletion = NULL;
    pb->csCode = TCPClose;
    pb->csParam.close.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.close.ulpTimeoutValue = 60 /* seconds */;
    pb->csParam.close.ulpTimeoutAction = 1 /* 1:abort 0:report */;
    mzPBControlSync((ParamBlockRec*)pb);
  }

  if (rel) {
    pb->csCode = TCPRelease;
    mzPBControlSync((ParamBlockRec*)pb);

    {
      TCPiopbX *x, *prev = NULL;
      x = active_pbs;
      while (x) {
	if (x->data->tcp.stream == data->tcp.stream) {
	  if (!prev)
	    active_pbs = x->next;
	  else
	    prev->next = x->next;
	  break;
	} else {
	  prev = x;
	  x = x->next;
	}
      }
    }
  }
}

static void mac_tcp_close_all(Scheme_Tcp *data)
{
  mac_tcp_close(data, 1, 1);
}

static int mac_tcp_listen(int id, long host_id, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;
  
  if (!(errid = mac_tcp_make(&xpb, &pb, &data))) {
    data->tcp.state = SOCK_STATE_LISTENING;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;

    pb->ioCompletion = u_tcp_listen_done;
    pb->csCode = TCPPassiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.ulpTimeoutAction = 0 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.remoteHost = host_id;
    pb->csParam.open.remotePort = 0;
    pb->csParam.open.localHost = 0;
    pb->csParam.open.localPort = id;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if ((errid = mzPBControlAsync((ParmBlkPtr)pb))) {
      data->tcp.state = SOCK_STATE_UNCONNECTED;
      mac_tcp_close(data, 1, 1);
      return errid;
    } else {
      *_data = data;
      return 0;
    }
  } else
    return errid;
}

static void tcp_cleanup(void)
{
  while (active_pbs) {
    TCPiopbX *pb = active_pbs;
    active_pbs = active_pbs->next;
    mac_tcp_close(pb->data, 1, 1);
  }
}

#else
#define TCP_INIT(x) /* nothing */
#endif
#endif

/*========================================================================*/
/*                       TCP ports and listeners                          */
/*========================================================================*/

#ifdef USE_SOCKETS_TCP
#define LISTENER_WAS_CLOSED(x) (((listener_t *)(x))->s == INVALID_SOCKET)
#endif
#ifdef USE_MAC_TCP
#define LISTENER_WAS_CLOSED(x) !((listener_t *)(x))->datas
#endif
#ifndef LISTENER_WAS_CLOSED
#define LISTENER_WAS_CLOSED(x) 0
#endif

/* Forward declaration */
static int stop_listener(Scheme_Object *o);

static int tcp_check_accept(Scheme_Object *listener)
{
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};
  int sr;

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  s = ((listener_t *)listener)->s;

  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, readfds);
  MZ_FD_SET(s, exnfds);
  
  do {
    sr = select(s + 1, readfds, NULL, exnfds, &time);
  } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));

  return sr;
#endif
#ifdef USE_MAC_TCP
  int i, count;
  Scheme_Tcp **datas;

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  count = ((listener_t *)listener)->count;
  datas = ((listener_t *)listener)->datas;

  for (i = 0; i < count; i++)
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING))
      return 1;

  return 0;
#endif
}

static void tcp_accept_needs_wakeup(Scheme_Object *listener, void *fds)
{
#ifdef USE_SOCKETS_TCP
  if (!LISTENER_WAS_CLOSED(listener)) {
    tcp_t s = ((listener_t *)listener)->s;
    void *fds2;

    fds2 = MZ_GET_FDSET(fds, 2);
    
    MZ_FD_SET(s, (fd_set *)fds);
    MZ_FD_SET(s, (fd_set *)fds2);
  }
#endif
}

static int tcp_check_connect(Scheme_Object *connector_p)
{
#ifdef USE_MAC_TCP
  return ((TCPiopb *)connector_p)->ioResult != inProgress;
#else
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};
  int sr;

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  s = *(tcp_t *)connector_p;

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);
    
  do {
    sr = select(s + 1, NULL, writefds, exnfds, &time);
  } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (!sr)
    return 0;
  if (FD_ISSET(s, exnfds))
    return -1;
  else
    return 1;
#else
  return 0;
#endif
#endif
}

static void tcp_connect_needs_wakeup(Scheme_Object *connector_p, void *fds)
{
#ifdef USE_SOCKETS_TCP
  void *fds1, *fds2;
  tcp_t s = *(tcp_t *)connector_p;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}

#ifdef USE_MAC_TCP
static void tcp_read_needs_wakeup(Scheme_Object *connector, void *fds)
{
}

static int tcp_check_read(Scheme_Object *pb)
{
  return (((TCPiopb *)pb)->ioResult != inProgress);
}
#endif

static int tcp_check_write(Scheme_Object *port)
{
  Scheme_Tcp *data = (Scheme_Tcp *)((Scheme_Output_Port *)port)->port_data;

  if (((Scheme_Output_Port *)port)->closed)
    return 1;

#ifdef USE_SOCKETS_TCP
  {
    tcp_t s;
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_FDSET(writefds, 1);
    INIT_DECL_FDSET(exnfds, 1);
    
    s = data->tcp;
    
    MZ_FD_ZERO(writefds);
    MZ_FD_SET(s, writefds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(s, exnfds);
    
    do {
      sr = select(s + 1, NULL, writefds, exnfds, &time);
    } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));
    
    return sr;
  }
#else
  {
    TCPiopbX *xpb;
    TCPiopb *pb;
    int bytes;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    if (mzPBControlSync((ParamBlockRec*)pb))
      bytes = -1;
    else {
      bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
      if (bytes < 0)
	bytes = 0;
    }
    
    return !!bytes;
  }
#endif
}

static void tcp_write_needs_wakeup(Scheme_Object *port, void *fds)
{
#ifdef USE_SOCKETS_TCP
  Scheme_Object *conn = ((Scheme_Output_Port *)port)->port_data;
  void *fds1, *fds2;
  tcp_t s = ((Scheme_Tcp *)conn)->tcp;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);
  
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}


static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount)
{
  Scheme_Tcp *data;
  char *bfr;
  
  data = MALLOC_ONE_RT(Scheme_Tcp);
#ifdef MZTAG_REQUIRED
  data->b.type = scheme_rt_tcp;
#endif
#ifdef USE_SOCKETS_TCP
  data->tcp = tcp;
#endif

  bfr = (char *)scheme_malloc_atomic(TCP_BUFFER_SIZE);
  data->b.buffer = bfr;
  bfr = (char *)scheme_malloc_atomic(TCP_BUFFER_SIZE);
  data->b.out_buffer = bfr;

  data->b.bufpos = 0;
  data->b.bufmax = 0;
  data->b.hiteof = 0;
  data->b.refcount = refcount;

#ifndef USE_MAC_TCP
# ifdef USE_WINSOCK_TCP
  {
    unsigned long ioarg = 1;
    ioctlsocket(tcp, FIONBIO, &ioarg);
  }
# else
  fcntl(tcp, F_SETFL, MZ_NONBLOCKING);
# endif
#endif

  return data;
}

static int tcp_byte_ready (Scheme_Input_Port *port)
{
  Scheme_Tcp *data;
#ifdef USE_SOCKETS_TCP
  int sr;
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exfds, 1);
#endif

  if (port->closed)
    return 1;

  data = (Scheme_Tcp *)port->port_data;

  if (data->b.hiteof)
    return 1;
  if (data->b.bufpos < data->b.bufmax)
    return 1;

#ifdef USE_SOCKETS_TCP
  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exfds);
  MZ_FD_SET(data->tcp, readfds);
  MZ_FD_SET(data->tcp, exfds);
    
  do {
    sr = select(data->tcp + 1, readfds, NULL, exfds, &time);
  } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));

  return sr;
#endif

#ifdef USE_MAC_TCP
  if ((data->tcp.state == SOCK_STATE_CONNECTED)) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    pb->ioCompletion = NULL;

    if (mzPBControlSync((ParamBlockRec*)pb))
      return 1;
      
    if (pb->csParam.status.amtUnreadData)
      return 1;
 } else
   return 1;
#endif

  return 0;
}

static long tcp_get_string(Scheme_Input_Port *port, 
			   char *buffer, long offset, long size,
			   int nonblock,
			   Scheme_Object *unless)
{
  int errid;
  int read_amt;
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

 top:

  if (scheme_unless_ready(unless))
    return SCHEME_UNLESS_READY;

  if (data->b.hiteof)
    return EOF;

#ifdef USE_MAC_TCP
  if (!data->activeRcv)
#endif
    if (data->b.bufpos < data->b.bufmax) {
      int n;
      n = data->b.bufmax - data->b.bufpos;
      n = ((size <= n)
	   ? size
	   : n);
      
      memcpy(buffer + offset, data->b.buffer + data->b.bufpos, n);
      data->b.bufpos += n;

      return n;
    }

  while (!tcp_byte_ready(port)) {
    if (nonblock > 0)
      return 0;

#ifdef USE_SOCKETS_TCP
    scheme_block_until_unless((Scheme_Ready_Fun)tcp_byte_ready,
			      scheme_need_wakeup,
			      (Scheme_Object *)port,
			      0.0, unless,
			      nonblock);
#else
    do {
      scheme_thread_block_enable_break((float)0.0, nonblock);
      if (scheme_unless_ready(unless))
	break;
    } while (!tcp_byte_ready(port));
    scheme_current_thread->ran_some = 1;
#endif

    scheme_wait_input_allowed(port, nonblock);

    if (scheme_unless_ready(unless))
      return SCHEME_UNLESS_READY;
  }

  if (port->closed) {
    /* Another thread closed the input port while we were waiting. */
    /* Call scheme_get_byte to signal the error */
    scheme_get_byte((Scheme_Object *)port);
  }

  /* We assume that no other process has access to our sockets, so
     when we unblock, there's definitely something to read. */

  if (!data->b.bufmode || (size > TCP_BUFFER_SIZE))
    read_amt = TCP_BUFFER_SIZE;
  else
    read_amt = size;

#ifdef USE_SOCKETS_TCP
  {
    int rn;
    do {
      rn = recv(data->tcp, data->b.buffer, read_amt, 0);
    } while ((rn == -1) && (NOT_WINSOCK(errno) == EINTR));
    data->b.bufmax = rn;
  }
  errid = SOCK_ERRNO();

  /* Is it possible that an EAGAIN error occurs? That means that data
     isn't ready, even though select() says that data is ready. It
     seems to happen for at least one user, and there appears to be
     no harm in protecting against it. */
  if ((data->b.bufmax == -1) && WAS_EAGAIN(errid))
    goto top;

#endif
#ifdef USE_MAC_TCP
  /* Allow only one read at a time: */
  if (!data->tcp.lock)
    data->tcp.lock = scheme_make_sema(0);
  else {
    if (!scheme_wait_sema(data->tcp.lock, 1)) {
      /* Do it the hard way: */
      scheme_wait_sema(data->tcp.lock, 0);
      scheme_post_sema(data->tcp.lock);
      goto top;
    }
  }
  
  if (data->activeRcv 
      || (data->tcp.state == SOCK_STATE_CONNECTED)) {
    /* socket is connected or an old recv is unfinished */
    TCPiopb *pb;    

    if (data->activeRcv) {
      pb = data->activeRcv;
    } else {
      pb = (TCPiopb *)mac_make_xpb(data);
    
      pb->csCode = TCPRcv;
      pb->ioCompletion = u_tcp_recv_done;
      pb->csParam.receive.commandTimeoutValue = 0; /* seconds, 0 = blocking */
      pb->csParam.receive.rcvBuff = data->b.buffer;
      pb->csParam.receive.rcvBuffLen = read_amt;
    
      data->activeRcv = pb;

      mzPBControlAsync((ParamBlockRec*)pb);
    }

    BEGIN_ESCAPEABLE(scheme_post_sema, data->tcp.lock);
    /* No need for unless_evt, since we have a lock */
    scheme_block_until_enable_break(tcp_check_read, tcp_read_needs_wakeup, (Scheme_Object *)pb, 
				    0.0, nonblock);
    END_ESCAPEABLE();

    data->activeRcv = NULL;
    
    switch((errid = pb->ioResult)) {
    case noErr:
    case connectionClosing:
    case connectionTerminated:
      errid = 0;
      break;
    case commandTimeout:
    case connectionDoesntExist:
    case invalidStreamPtr:
    case invalidLength:
    case invalidBufPtr:
    default:
      break;
    }
  } else if (data->tcp.state == SOCK_STATE_EOF_FROM_OTHER 
             || data->tcp.state == SOCK_STATE_CLOSED) {
    data->b.bufmax = 0;
    errid = 0;
  } else
    errid = data->tcp.async_errid;
  
  if (errid)
    data->b.bufmax = -1;
    
  scheme_post_sema(data->tcp.lock);
#endif
  
  if (data->b.bufmax == -1) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-read: error reading (%e)",
		     errid);
    return 0;
  } else if (!data->b.bufmax) {
    data->b.hiteof = 1;
    return EOF;
  }

  {
    int n;
    n = data->b.bufmax;
    if (size < n)
      n = size;
    memcpy(buffer + offset, data->b.buffer, n);
    data->b.bufpos = n;

    return n;
  }
}

static void tcp_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_SOCKETS_TCP
  {
    void *fds2;
  
    fds2 = MZ_GET_FDSET(fds, 2);
    
    MZ_FD_SET(data->tcp, (fd_set *)fds);
    MZ_FD_SET(data->tcp, (fd_set *)fds2);
  }
#endif
}

static void tcp_close_input(Scheme_Input_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_SOCKETS_TCP
  if (!(data->flags & MZ_TCP_ABANDON_INPUT))
    shutdown(data->tcp, 0);
#endif

  if (--data->b.refcount)
    return;

#ifdef USE_SOCKETS_TCP
  UNREGISTER_SOCKET(data->tcp);
  closesocket(data->tcp);
#endif
#ifdef USE_MAC_TCP
  mac_tcp_close(data, 0, 1);
#endif

  --scheme_file_open_count;
}

static int
tcp_in_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)((Scheme_Input_Port *)p)->port_data;  
  if (mode < 0)
    return data->b.bufmode;
  else {
    data->b.bufmode = mode;
    return mode;
  }
}

static long tcp_do_write_string(Scheme_Output_Port *port, 
				const char *s, long offset, long len, 
				int rarely_block, int enable_break)
{
  /* We've already checked for buffering before we got here. */
  /* If rarely_block is 1, it means only write as much as
     can be flushed immediately, blocking only if nothing
     can be written. */
  /* If rarely_block is 2, it means only write as much as
     can be flushed immediately, never ever blocking. */

  Scheme_Tcp *data;
  int errid, would_block = 0;
  long sent;

  data = (Scheme_Tcp *)port->port_data;

 top:

#ifdef USE_SOCKETS_TCP
  do {
    sent = send(data->tcp, s XFORM_OK_PLUS offset, len, 0);
  } while ((sent == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (sent != len) {
#ifdef USE_WINSOCK_TCP
# define SEND_BAD_MSG_SIZE(e) (e == WSAEMSGSIZE)
#else    
# ifdef SEND_IS_NEVER_TOO_BIG
#  define SEND_BAD_MSG_SIZE(errid) 0
# else
#  define SEND_BAD_MSG_SIZE(errid) (errid == EMSGSIZE)
# endif
#endif
    errid = SOCK_ERRNO();
    if (sent > 0) {
      /* Some data was sent. Return, or recur to handle the rest. */
      if (rarely_block)
	return sent;
      else
	sent += tcp_do_write_string(port, s, offset + sent, len - sent, 0, enable_break);
      errid = 0;
    } else if ((len > 1) && SEND_BAD_MSG_SIZE(errid)) {
      /* split the message and try again: */
      int half = (len / 2);
      sent = tcp_do_write_string(port, s, offset, half, rarely_block, enable_break);
      if (rarely_block)
	return sent;
      sent += tcp_do_write_string(port, s, offset + half, len - half, 0, enable_break);
      errid = 0;
    } else if (WAS_EAGAIN(errid)) {
      errid = 0;
      would_block = 1;
    }
  } else
    errid = 0;
#endif
#ifdef USE_MAC_TCP
  errid = 0;
  if ((data->tcp.state == SOCK_STATE_CONNECTED) ||
      (data->tcp.state == SOCK_STATE_EOF_FROM_OTHER)) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    int bytes;

    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    if ((errid = mzPBControlSync((ParamBlockRec*)pb)))
      bytes = 0;
    else {
      bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
      if (bytes < 0)
	bytes = 0;
    }
    
    if (bytes >= len) {
      WriteData *wd;
      wdsEntry *e;
      int i;
      
      wd = NULL;
      for (i = 0; i < num_tcp_send_buffers; i++) {
	if (!((WriteData *)(tcp_send_buffers[i]))->xpb) {
	  wd = (WriteData *)(tcp_send_buffers[i]);
	  break;
	}
      }
      
      if (!wd) {
	void **naya;
	int nayac;
	
	nayac = (2 * num_tcp_send_buffers) + 1;
	naya = MALLOC_N(void *, nayac);
	memcpy(naya, tcp_send_buffers, sizeof(void *) * num_tcp_send_buffers);
	for (i = num_tcp_send_buffers; i < nayac; i++) {
	  wd = MALLOC_ONE_RT(WriteData);
#ifdef MZTAG_REQUIRED
	  wd->so.type = scheme_rt_write_data;
#endif
	  wd->xpb = NULL;
	  e = wd->e;
	  e[0].ptr = NULL;
	  e[1].ptr = NULL;
	  e[1].length = 0;
	  naya[i] = (void *)e;
	}

	wd = (WriteData *)naya[num_tcp_send_buffers];
	
	tcp_send_buffers = naya;
	num_tcp_send_buffers = nayac;
      }

      wd->xpb = xpb;
      e = wd->e;

      e[0].ptr = (Ptr)scheme_malloc_atomic(len);
      memcpy(e[0].ptr, s + offset, len);
      e[0].length = len;
      e[1].ptr = NULL;
      e[1].length = 0;

      pb->csCode = TCPSend;
      pb->ioCompletion = u_tcp_send_done;
      pb->csParam.send.validityFlags = timeoutValue | timeoutAction;
      pb->csParam.send.ulpTimeoutValue = 60 /* seconds */;
      pb->csParam.send.ulpTimeoutAction = 1 /* 0:abort 1:report */;
      pb->csParam.send.pushFlag = 1;
      pb->csParam.send.urgentFlag = 0;
      pb->csParam.send.wdsPtr = (Ptr)e;
      pb->csParam.send.sendFree = 0;
      pb->csParam.send.sendLength = 0;
      
      errid = mzPBControlAsync((ParamBlockRec*)pb);
    } else if (!errid) {
      if (bytes) {
      	/* Do partial write: */
        sent = tcp_do_write_string(port, s, offset, bytes, rarely_block, enable_break);
	if (rarely_block)
	  return sent;
        sent = tcp_do_write_string(port, s, offset + bytes, len - bytes, 0, enable_break);
	sent += bytes;
      } else
        would_block = 1;
    }
  } else
    errid = data->tcp.async_errid;
#endif

  if (would_block) {
    if (rarely_block == 2)
      return 0;

    /* Block for writing: */
    scheme_block_until_enable_break(tcp_check_write, tcp_write_needs_wakeup, (Scheme_Object *)port, 
				    (float)0.0, enable_break);

    /* Closed while blocking? */
    if (((Scheme_Output_Port *)port)->closed) {
      /* Call write again to signal the error: */
      scheme_put_byte_string("tcp-write-string", (Scheme_Object *)port, s, offset, len, 0);
      return sent + len; /* shouldn't get here */
    }

    /* Ok - try again! */
    would_block = 0;
    goto top;
  }

  if (errid)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-write: error writing (%e)",
		     errid);

  return sent;
}

static int tcp_flush(Scheme_Output_Port *port,
		     int rarely_block, int enable_break)
{
  Scheme_Tcp *data;
  int amt, flushed = 0;
  
  data = (Scheme_Tcp *)port->port_data;

  while (1) {
    if (data->b.out_bufpos == data->b.out_bufmax) {
      data->b.out_bufpos = 0;
      data->b.out_bufmax = 0;
      return flushed;
    }
    amt = tcp_do_write_string(port, data->b.out_buffer, data->b.out_bufpos, 
			      data->b.out_bufmax - data->b.out_bufpos,
			      rarely_block, enable_break);
    flushed += amt;
    data->b.out_bufpos += amt;
    if (rarely_block && (data->b.out_bufpos < data->b.out_bufmax))
      return flushed;
  }
}

static long tcp_write_string(Scheme_Output_Port *port, 
			     const char *s, long offset, long len, 
			     int rarely_block, int enable_break)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (!len) {
    /* Flush */
    return tcp_flush(port, rarely_block, enable_break);
  }

  if (rarely_block) {
    tcp_flush(port, rarely_block, enable_break);
    if (data->b.out_bufmax)
      return -1;
  } else {
    if (data->b.out_bufmode < 2) {
      if (data->b.out_bufmax + len < TCP_BUFFER_SIZE) {
	memcpy(data->b.out_buffer + data->b.out_bufmax, s + offset, len);
	data->b.out_bufmax += len;
	if (data->b.out_bufmode == 1) {
	  /* Check for newline */
	  int i;
	  for (i = 0; i < len; i++) {
	    if ((s[offset + i] == '\r')
		|| (s[offset + i] == '\n'))
	      break;
	  }
	  if (i < len)
	    tcp_flush(port, rarely_block, enable_break);
	}
	return len;
      }
    }
    tcp_flush(port, rarely_block, enable_break);
  }

  /* When we get here, the buffer is empty */
  return tcp_do_write_string(port, s, offset, len, rarely_block, enable_break);
}

static void tcp_close_output(Scheme_Output_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (data->b.out_bufmax && !scheme_force_port_closed)
    tcp_flush(port, 0, 0);

#ifdef USE_SOCKETS_TCP
  if (!(data->flags & MZ_TCP_ABANDON_OUTPUT))
    shutdown(data->tcp, 1);
#endif

#ifdef USE_MAC_TCP
  if (!(data->flags & MZ_TCP_ABANDON_OUTPUT)
      || (data->b.refcount == 1))
    mac_tcp_close(data, 1, data->b.refcount == 1);
#endif

  if (--data->b.refcount)
    return;

#ifdef USE_SOCKETS_TCP
  UNREGISTER_SOCKET(data->tcp);
  closesocket(data->tcp);
#endif

  --scheme_file_open_count;
}

static int
tcp_out_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)((Scheme_Output_Port *)p)->port_data;  
  if (mode < 0)
    return data->b.out_bufmode;
  else {
    int go;
    go = (mode > data->b.out_bufmode);
    data->b.out_bufmode = mode;
    if (go)
      tcp_flush((Scheme_Output_Port *)p, 0, 0);
    return mode;
  }
}

static Scheme_Object *
make_tcp_input_port(void *data, const char *name)
{
  Scheme_Input_Port *ip;
  
  ip = scheme_make_input_port(scheme_tcp_input_port_type,
			      data,
			      scheme_make_immutable_sized_utf8_string((char *)name, -1),
			      tcp_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_peeked_read_via_get,
			      tcp_byte_ready,
			      tcp_close_input,
			      tcp_need_wakeup,
			      1);

  ip->p.buffer_mode_fun = tcp_in_buffer_mode;

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_tcp_output_port(void *data, const char *name)
{
  Scheme_Output_Port *op;

  op = scheme_make_output_port(scheme_tcp_output_port_type,
						  data,
						  scheme_make_immutable_sized_utf8_string((char *)name, -1),
						  scheme_write_evt_via_write,
						  tcp_write_string,
						  (Scheme_Out_Ready_Fun)tcp_check_write,
						  tcp_close_output,
						  (Scheme_Need_Wakeup_Output_Fun)tcp_write_needs_wakeup,
						  NULL,
						  NULL,
						  1);

  op->p.buffer_mode_fun = tcp_out_buffer_mode;

  return (Scheme_Object *)op;
}

#endif /* USE_TCP */

/*========================================================================*/
/*                         TCP Scheme interface                           */
/*========================================================================*/

# ifdef PROTOENT_IS_INT
#  define PROTO_P_PROTO PROTOENT_IS_INT
# else
#  define PROTO_P_PROTO proto->p_proto
# endif

# ifndef MZ_PF_INET
#  define MZ_PF_INET PF_INET
# endif

#ifdef USE_SOCKETS_TCP
static void closesocket_w_decrement(tcp_t s)
{
  closesocket(s);
  --scheme_file_open_count;
}
#endif

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[])
{
  char * volatile address = "", * volatile src_address, * volatile errmsg = "";
  unsigned short origid, id, src_origid, src_id;
  int errpart = 0, errid = 0, no_local_spec;
  Scheme_Object *bs, *src_bs;
#ifdef USE_SOCKETS_TCP
  GC_CAN_IGNORE tcp_address tcp_connect_dest_addr, tcp_connect_src_addr;
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("tcp-connect", "string", 0, argc, argv);
  if (!CHECK_PORT_ID(argv[1]))
    scheme_wrong_type("tcp-connect", PORT_ID_TYPE, 1, argc, argv);
  if (argc > 2)
    if (!SCHEME_CHAR_STRINGP(argv[2]) && !SCHEME_FALSEP(argv[2]))
      scheme_wrong_type("tcp-connect", "string or #f", 2, argc, argv);
  if (argc > 3)
    if (SCHEME_TRUEP(argv[3]) && !CHECK_PORT_ID(argv[3]))
      scheme_wrong_type("tcp-connect", PORT_ID_TYPE " or #f", 3, argc, argv);

#ifdef USE_TCP
  TCP_INIT("tcp-connect");
#endif

  bs = argv[0];
  if (SCHEME_CHAR_STRINGP(bs))
    bs = scheme_char_string_to_byte_string(bs);

  address = SCHEME_BYTE_STR_VAL(bs);
  origid = (unsigned short)SCHEME_INT_VAL(argv[1]);

  if ((argc > 2) && SCHEME_TRUEP(argv[2])) {
    src_bs = scheme_char_string_to_byte_string(argv[2]);
    src_address = SCHEME_BYTE_STR_VAL(src_bs);
  } else
    src_address = NULL;
   
  if ((argc > 3) && SCHEME_TRUEP(argv[3])) {
    no_local_spec = 0;
    src_origid = (unsigned short)SCHEME_INT_VAL(argv[3]);
  } else {
    no_local_spec = 1;
    src_origid = 0;
    if (src_address) {
      scheme_arg_mismatch("tcp-connect",
			  "no local port number supplied when local hostname was supplied: ",
			  argv[2]);
    }
  }

  scheme_security_check_network("tcp-connect", address, origid, 1);
  scheme_custodian_check_available(NULL, "tcp-connect", "network");

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
  src_id = htons(src_origid);
#endif

#ifdef USE_MAC_TCP
  {
    TCPiopbX *xpb;
    TCPiopb *pb;
    Scheme_Tcp *data;
    int errNo, srchost;
    struct hostInfo *dest_host;
    struct hostInfo *src_host;
    Scheme_Object *v[2];
    
    dest_host = MALLOC_ONE_ATOMIC(struct hostInfo);
    if ((errNo = tcp_addr(address, dest_host))) {
      errpart = 1;
      errmsg = "; host not found";
      goto tcp_error;
    }
    if (src_address) {
      src_host = MALLOC_ONE_ATOMIC(struct hostInfo);
      if ((errNo = tcp_addr(src_address, src_host))) {
      errpart = 2;
      errmsg = "; local host not found";
      goto tcp_error;
      }
      srchost = src_host->addr[0];
    } else
      srchost = 0;
  

    if ((errNo = mac_tcp_make(&xpb, &pb, &data))) {
      errpart = 3;
      goto tcp_error;
    }

    data->tcp.state = SOCK_STATE_CONNECTING;
    
    pb->ioCompletion = u_tcp_connect_done;
    pb->csCode = TCPActiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 60 /* seconds */;
    pb->csParam.open.ulpTimeoutAction = 1 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0;
    pb->csParam.open.remoteHost = dest_host->addr[0];
    pb->csParam.open.remotePort = id;
    pb->csParam.open.localHost = srchost;
    pb->csParam.open.localPort = src_id;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if ((errNo = mzPBControlAsync((ParamBlockRec*)pb))) {
      errpart = 4;
      goto tcp_close_and_error;
    }
    
    BEGIN_ESCAPEABLE(mac_tcp_close_all, data);
    scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, (Scheme_Object *)pb, 0);
    END_ESCAPEABLE();
    
    if (data->tcp.state != SOCK_STATE_CONNECTED) {
      errpart = 5;
      errNo = pb->ioResult;
      goto tcp_close_and_error;
    }
    
    v[0] = make_tcp_input_port(data, address);
    v[1] = make_tcp_output_port(data, address);
    
    return scheme_values(2, v);
    
  tcp_close_and_error:
    
    mac_tcp_close(data, 1, 1);
    
  tcp_error:
    
    errid = errNo;
  }
#endif

#ifdef USE_SOCKETS_TCP
  if (scheme_get_host_address(address, id, &tcp_connect_dest_addr)) {
    if (scheme_get_host_address(src_address, src_id, &tcp_connect_src_addr)) {
#ifndef PROTOENT_IS_INT
      proto = getprotobyname("tcp");
      if (proto)
#endif
      {
        tcp_t s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
        if (s != INVALID_SOCKET) {
	  int status, inprogress;
	  if (no_local_spec
	      || !bind(s, (struct sockaddr *)&tcp_connect_src_addr, sizeof(tcp_connect_src_addr))) {
#ifdef USE_WINSOCK_TCP
	    unsigned long ioarg = 1;
	    ioctlsocket(s, FIONBIO, &ioarg);
#else
	    int size = TCP_SOCKSENDBUF_SIZE;
	    fcntl(s, F_SETFL, MZ_NONBLOCKING);
# ifndef CANT_SET_SOCKET_BUFSIZE
	    setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
# endif
#endif
	    status = connect(s, (struct sockaddr *)&tcp_connect_dest_addr, sizeof(tcp_connect_dest_addr));
#ifdef USE_UNIX_SOCKETS_TCP
	    if (status)
	      status = errno;
	    if (status == EINTR)
	      status = EINPROGRESS;
	
	    inprogress = (status == EINPROGRESS);
#endif
#ifdef USE_WINSOCK_TCP
	    if (status)
	      status = WSAGetLastError();

	    inprogress = (status == WSAEWOULDBLOCK);
	    errno = status;
#endif

	    scheme_file_open_count++;
	    
	    if (inprogress) {
	      tcp_t *sptr;

	      sptr = (tcp_t *)scheme_malloc_atomic(sizeof(tcp_t));
	      *sptr = s;

	      BEGIN_ESCAPEABLE(closesocket_w_decrement, s);
	      scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, (void *)sptr, (float)0.0);
	      END_ESCAPEABLE();

	      /* Check whether connect succeeded, or get error: */
	      {
	        int so_len = sizeof(status);
	        if (getsockopt(s, SOL_SOCKET, SO_ERROR, (void *)&status, &so_len) != 0) {
	          status = SOCK_ERRNO();
	        }
	        errno = status; /* for error reporting, below */
	      }

#ifdef USE_WINSOCK_TCP
	      if (scheme_stupid_windows_machine > 0) {
	        /* getsockopt() seems not to work in Windows 95, so use the
	           result from select(), which seems to reliably detect an error condition */
	        if (!status) {
	          if (tcp_check_connect((Scheme_Object *)sptr) == -1) {
		    status = 1;
		    errno = WSAECONNREFUSED; /* guess! */
	          }
	        }
	      }
#endif
	    }
	
	    if (!status) {
	      Scheme_Object *v[2];
	      Scheme_Tcp *tcp;

	      tcp = make_tcp_port_data(s, 2);
	      
	      v[0] = make_tcp_input_port(tcp, address);
	      v[1] = make_tcp_output_port(tcp, address);
	      
	      REGISTER_SOCKET(s);

	      return scheme_values(2, v);
	    } else {
	      errid = errno;
	      closesocket(s);
	      --scheme_file_open_count;
	      errpart = 6;
	    }
          } else {
	    errpart = 5;
	    errid = SOCK_ERRNO();
	  }
        } else {
	  errpart = 4;
	  errid = SOCK_ERRNO();
        }
      }
#ifndef PROTOENT_IS_INT
      else {
        errpart = 3;
        errid = SOCK_ERRNO();
      }
#endif
    } else {
      errpart = 2;
      errid = 0;
      errmsg = "; local host not found";
    } 
  } else {
    errpart = 1;
    errid = 0;
    errmsg = "; host not found";
  }
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_FAIL_NETWORK,
		   "tcp-connect: connection to %s, port %d failed%s (at step %d: %E)",
		   address, origid, errmsg, errpart, errid);
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "tcp-connect: not supported on this platform");
#endif

  return NULL;
}

static Scheme_Object *
tcp_connect_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(tcp_connect, argc, argv);
}

static Scheme_Object *
tcp_listen(int argc, Scheme_Object *argv[])
{
  unsigned short id, origid;
  int backlog, errid;
  int reuse = 0;
  const char *address;
#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!CHECK_PORT_ID(argv[0]))
    scheme_wrong_type("tcp-listen", PORT_ID_TYPE, 0, argc, argv);
  if (argc > 1) {
    if (!SCHEME_INTP(argv[1]) || (SCHEME_INT_VAL(argv[1]) < 1))
      scheme_wrong_type("tcp-listen", "small positive integer", 1, argc, argv);
  }
  if (argc > 2)
    reuse = SCHEME_TRUEP(argv[2]);
  if (argc > 3) {
    if (!SCHEME_CHAR_STRINGP(argv[3]) && !SCHEME_FALSEP(argv[3]))
      scheme_wrong_type("tcp-listen", "string or #f", 3, argc, argv);
  }
    
#ifdef USE_TCP
  TCP_INIT("tcp-listen");
#endif

  origid = (unsigned short)SCHEME_INT_VAL(argv[0]);
  if (argc > 1)
    backlog = SCHEME_INT_VAL(argv[1]);
  else
    backlog = 4;
  if ((argc > 3) && SCHEME_TRUEP(argv[3])) {
    Scheme_Object *bs;
    bs = scheme_char_string_to_byte_string(argv[3]);
    address = SCHEME_BYTE_STR_VAL(bs);
  } else
    address = NULL;

  scheme_security_check_network("tcp-listen", address, origid, 0);
  scheme_custodian_check_available(NULL, "tcp-listen", "network");

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
#endif

#ifdef USE_MAC_TCP
  {
    int i;
    long hostid;
    Scheme_Tcp **datas, *data;
    struct hostInfo *local_host;

    if (address) {
      local_host = MALLOC_ONE_ATOMIC(struct hostInfo);
      if ((errid = tcp_addr(address, local_host))) {
	scheme_raise_exn(MZEXN_FAIL_NETWORK,
			 "tcp-listen: host not found: %s (%E)",
			 address, errid);
	return NULL;
      }
      hostid = local_host->addr[0];
    } else
      hostid = 0;


    datas = MALLOC_N(Scheme_Tcp *, backlog);

    for (i = 0; i < backlog; i++) {
      if ((errid = mac_tcp_listen(id, hostid, &data))) {
        /* Close listeners that had succeeded: */
        int j;
        for (j = 0; j < i; j++)
          mac_tcp_close(datas[i], 1, 1);
	break;
      }
      datas[i] = data;
    }

    if (!errid) {
      listener_t *l = MALLOC_ONE_TAGGED(listener_t);

      l->so.type = scheme_listener_type;
      l->portid = id;
      l->hostid = hostid;
      l->count = backlog;
      l->datas = datas;
      l->mref = scheme_add_managed(NULL,
				   (Scheme_Object *)l,
				   (Scheme_Close_Custodian_Client *)stop_listener,
				   NULL,
				   1);
      
      return (Scheme_Object *)l;
    }
  }
#endif

#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  proto = getprotobyname("tcp");
  if (proto)
# endif
  {
    GC_CAN_IGNORE tcp_address tcp_listen_addr;

    if (scheme_get_host_address(address, id, &tcp_listen_addr)) {
      tcp_t s;

      s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
      if (s != INVALID_SOCKET) {
#ifdef USE_WINSOCK_TCP
	unsigned long ioarg = 1;
	ioctlsocket(s, FIONBIO, &ioarg);
#else
	fcntl(s, F_SETFL, MZ_NONBLOCKING);
#endif

	if (reuse) {
	  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)(&reuse), sizeof(int));
	}
      
	if (!bind(s, (struct sockaddr *)&tcp_listen_addr, sizeof(tcp_listen_addr))) {
	  if (!listen(s, backlog)) {
	    listener_t *l;

	    l = MALLOC_ONE_TAGGED(listener_t);
	    l->so.type = scheme_listener_type;
	    l->s = s;
	    {
	      Scheme_Custodian_Reference *mref;
	      mref = scheme_add_managed(NULL,
					(Scheme_Object *)l,
					(Scheme_Close_Custodian_Client *)stop_listener,
					NULL,
					1);
	      l->mref = mref;
	    }

	    scheme_file_open_count++;
	    REGISTER_SOCKET(s);

	    return (Scheme_Object *)l;
	  }
	}

	errid = SOCK_ERRNO();

	closesocket(s);
      } else
	errid = SOCK_ERRNO();
    } else {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "tcp-listen: host not found: %s",
		       address);
      return NULL;
    }
  }
# ifndef PROTOENT_IS_INT
  else {
    errid = SOCK_ERRNO();
  }
# endif
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_FAIL_NETWORK,
		   "tcp-listen: listen on %d failed (%E)",
		   origid, errid);
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "tcp-listen: not supported on this platform");
#endif

  return NULL;
}

#ifdef USE_TCP
static int stop_listener(Scheme_Object *o)
{
  int was_closed = 0;

#ifdef USE_MAC_TCP
  { 
    listener_t *l = (listener_t *)o;
    int i, count = l->count;
    Scheme_Tcp **datas = l->datas;
    if (!datas || !l->count)
      was_closed = 1;
    else {
      l->count = 0;
      for (i = 0; i < count; i++) {
	if (datas[i])
	  mac_tcp_close(datas[i], 1, 1);
      }
      scheme_remove_managed(l->mref, (Scheme_Object *)l);
    }
 }
#endif

#ifdef USE_SOCKETS_TCP
  {
    tcp_t s = ((listener_t *)o)->s;
    if (s == INVALID_SOCKET)
      was_closed = 1;
    else {
      UNREGISTER_SOCKET(s);
      closesocket(s);
      ((listener_t *)o)->s = INVALID_SOCKET;
      --scheme_file_open_count;
      scheme_remove_managed(((listener_t *)o)->mref, o);
    }
  }
#endif


  return was_closed;
}
#endif

static Scheme_Object *
tcp_stop(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-close");

  was_closed = stop_listener(argv[0]);

  if (was_closed) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-close: listener was already closed");
    return NULL;
  }

  return scheme_void;
#else
  scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept_ready(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int ready;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept-ready?", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept-ready?");

  if (LISTENER_WAS_CLOSED(argv[0])) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-accept-ready?: listener is closed");
    return NULL;
  }

  ready = tcp_check_accept(argv[0]);

  return (ready ? scheme_true : scheme_false);
#else
  scheme_wrong_type("tcp-accept-ready?", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed = 0, errid;
  Scheme_Object *listener;
# ifdef USE_SOCKETS_TCP
  tcp_t s;
  int l;
  tcp_address tcp_accept_addr; /* Use a long name for precise GC's xform.ss */
# endif
# ifdef USE_MAC_TCP
  listener_t *l;
  int i, count;
  Scheme_Tcp **datas;
# endif

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept");

  listener = argv[0];

  was_closed = LISTENER_WAS_CLOSED(listener);

  if (!was_closed) {
    if (!tcp_check_accept(listener)) {
      scheme_block_until(tcp_check_accept, tcp_accept_needs_wakeup, listener, 0.0);
    }
    was_closed = LISTENER_WAS_CLOSED(listener);
  }

  if (was_closed) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-accept: listener is closed");
    return NULL;
  }

  scheme_custodian_check_available(NULL, "tcp-accept", "network");
  
# ifdef USE_SOCKETS_TCP
  s = ((listener_t *)listener)->s;

  l = sizeof(tcp_accept_addr);

  do {
    s = accept(s, (struct sockaddr *)&tcp_accept_addr, &l);
  } while ((s == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (s != INVALID_SOCKET) {
    Scheme_Object *v[2];
    Scheme_Tcp *tcp;
    
#  ifdef USE_UNIX_SOCKETS_TCP
    int size = TCP_SOCKSENDBUF_SIZE;
#   ifndef CANT_SET_SOCKET_BUFSIZE
    setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
#   endif
#  endif

    tcp = make_tcp_port_data(s, 2);

    v[0] = make_tcp_input_port(tcp, "[accepted]");
    v[1] = make_tcp_output_port(tcp, "[accepted]");

    scheme_file_open_count++;
    REGISTER_SOCKET(s);
    
    return scheme_values(2, v);
  }
  errid = SOCK_ERRNO();
# endif

# ifdef USE_MAC_TCP
  l = (listener_t *)listener;
  count = l->count;
  datas = l->datas;

  errid = 0;
  for (i = 0; i < count; i++) {
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING)) {
      Scheme_Object *v[2];
      Scheme_Tcp *data;
      
      v[0] = make_tcp_input_port(datas[i], "[accepted]");
      v[1] = make_tcp_output_port(datas[i], "[accepted]");
      
      if (!(errid = mac_tcp_listen(l->portid, l->hostid, &data))) {
        /* new listener at the end of the queue: */
	memcpy(datas + i, datas + i + 1, sizeof(Scheme_Tcp *) * (count - i - 1));
	datas[count - 1] = data;

	scheme_file_open_count++;
      } else {
      	/* catastophic error; we permanently decrement the listener count */
        datas[i] = NULL;
      }
      return scheme_values(2, v);
    }
  }
# endif

  scheme_raise_exn(MZEXN_FAIL_NETWORK,
		   "tcp-accept: accept from listener failed (%E)", errid);
#else
  scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);
#endif

  return NULL;
}

static Scheme_Object *
tcp_accept_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(tcp_accept, argc, argv);
}

static void register_tcp_listener_sync()
{
#ifdef USE_TCP
  scheme_add_evt(scheme_listener_type, tcp_check_accept, tcp_accept_needs_wakeup, NULL, 0);
  scheme_add_evt(scheme_tcp_accept_evt_type, (Scheme_Ready_Fun)tcp_check_accept_evt, tcp_accept_evt_needs_wakeup, NULL, 0);
# ifdef UDP_IS_SUPPORTED
  scheme_add_evt(scheme_udp_evt_type, (Scheme_Ready_Fun)udp_evt_check_ready, udp_evt_needs_wakeup, NULL, 0);
# endif
#endif
}

static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[])
{
   return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type)
	   ? scheme_true
	   : scheme_false);
}

static Scheme_Object *tcp_addresses(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  Scheme_Tcp *tcp = NULL;
  int closed = 0;
  unsigned long here_a, there_a;
  unsigned char *b;
  Scheme_Object *result[2];
  char sa[20];

  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = (Scheme_Output_Port *)argv[0];
    if (op->sub_type == scheme_tcp_output_port_type)
      tcp = op->port_data;
    closed = op->closed;
  } else if (SCHEME_INPORTP(argv[0])) {
    Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)argv[0];
    if (ip->sub_type == scheme_tcp_input_port_type)
      tcp = ip->port_data;
    closed = ip->closed;
  }

  if (!tcp)
    scheme_wrong_type("tcp-addresses", "tcp-port", 0, argc, argv);

  if (closed)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-addresses: port is closed");

# ifdef USE_SOCKETS_TCP
  {
    /* Use a long name for precise GC's xform.ss: */
    tcp_address tcp_here_addr, tcp_there_addr;
    int l;
    
    l = sizeof(tcp_here_addr);
    if (getsockname(tcp->tcp, (struct sockaddr *)&tcp_here_addr, &l)) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "tcp-addresses: could not get local address (%e)",
		       SOCK_ERRNO());
    }
    l = sizeof(tcp_there_addr);
    if (getpeername(tcp->tcp, (struct sockaddr *)&tcp_there_addr, &l)) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "tcp-addresses: could not get peer address (%e)",
		       SOCK_ERRNO());
    }

    here_a = *(unsigned long *)&tcp_here_addr.sin_addr;
    there_a = *(unsigned long *)&tcp_there_addr.sin_addr;
  }
# endif
# ifdef USE_MAC_TCP
  {
    here_a = ((TCPOpenPB *)tcp->tcp.create_pb)->localHost;
    there_a = ((TCPOpenPB *)tcp->tcp.create_pb)->remoteHost;
  }
# endif

  b = (unsigned char *)&here_a;
  sprintf(sa, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  result[0] = scheme_make_utf8_string(sa);

  b = (unsigned char *)&there_a;
  sprintf(sa, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  result[1] = scheme_make_utf8_string(sa);

  return scheme_values(2, result);
#else
  /* First arg can't possible be right! */
  scheme_wrong_type("tcp-addresses", "tcp-port", 0, argc, argv);
#endif
}

static Scheme_Object *tcp_abandon_port(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = (Scheme_Output_Port *)argv[0];
    if (op->sub_type == scheme_tcp_output_port_type) {
      if (!op->closed) {
	((Scheme_Tcp *)op->port_data)->flags |= MZ_TCP_ABANDON_OUTPUT;
	scheme_close_output_port(argv[0]);
      }
      return scheme_void;
    }
  } else if (SCHEME_INPORTP(argv[0])) {
    /* Abandon is not really useful on input ports from the Schemer's
       perspective, but it's here for completeness. */
    Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)argv[0];
    if (ip->sub_type == scheme_tcp_input_port_type) {
      if (!ip->closed) {
	((Scheme_Tcp *)ip->port_data)->flags |= MZ_TCP_ABANDON_INPUT;
	scheme_close_input_port(argv[0]);
      }
      return scheme_void;
    }
  }
#endif

  scheme_wrong_type("tcp-abandon-port", "tcp-port", 0, argc, argv);

  return NULL;
}

static Scheme_Object *tcp_port_p(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  if (SCHEME_OUTPORTP(argv[0])) {
    if (((Scheme_Output_Port *)argv[0])->sub_type == scheme_tcp_output_port_type) {
      return scheme_true;
    }
  } else if (SCHEME_INPORTP(argv[0])) {
    if (((Scheme_Input_Port *)argv[0])->sub_type == scheme_tcp_input_port_type) {
      return scheme_true;
    }
  }
#endif

  return scheme_false;
}


static Scheme_Object *tcp_accept_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *r;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept-evt", "tcp-listener", 0, argc, argv);

  r = scheme_alloc_small_object();
  r->type = scheme_tcp_accept_evt_type;
  SCHEME_PTR_VAL(r) = argv[0];

  return r;
}

static int tcp_check_accept_evt(Scheme_Object *ae, Scheme_Schedule_Info *sinfo)
{
  if (tcp_check_accept(SCHEME_PTR_VAL(ae))) {
    Scheme_Object *a[2];
    a[0] = SCHEME_PTR_VAL(ae);
    tcp_accept(1, a);
    a[0] = scheme_current_thread->ku.multiple.array[0];
    a[1] = scheme_current_thread->ku.multiple.array[1];
    scheme_set_sync_target(sinfo, scheme_build_list(2, a), NULL, NULL, 0, 0);
    return 1;
  } else
    return 0;
}

static void tcp_accept_evt_needs_wakeup(Scheme_Object *ae, void *fds)
{
  tcp_accept_needs_wakeup(SCHEME_PTR_VAL(ae), fds);
}

/*========================================================================*/
/*                                 UDP                                    */
/*========================================================================*/

/* Based on a design and implemenation by Eduardo Cavazos. */

#ifdef UDP_IS_SUPPORTED

typedef struct Scheme_UDP_Evt {
  Scheme_Object so; /* scheme_udp_evt_type */
  Scheme_UDP *udp;
  short for_read, with_addr;
  int offset, len;
  char *str;
  tcp_address dest_addr;
} Scheme_UDP_Evt;

static int udp_close_it(Scheme_Object *_udp)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (udp->s != INVALID_SOCKET) {
    closesocket(udp->s);
    udp->s = INVALID_SOCKET;

    scheme_remove_managed(udp->mref, (Scheme_Object *)udp);

    return 0;
  }

  return 1;
}

#endif

static Scheme_Object *make_udp(int argc, Scheme_Object *argv[])
{
#ifdef UDP_IS_SUPPORTED
  Scheme_UDP *udp;
  tcp_t s;

  TCP_INIT("udp-open-socket");

  scheme_security_check_network("udp-open-socket", NULL, -1, 1);
  scheme_custodian_check_available(NULL, "udp-open-socket", "network");

  s = socket(PF_INET, SOCK_DGRAM, 0);

  if (s == INVALID_SOCKET) {
    int errid;
    errid = SOCK_ERRNO();
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "udp-open-socket: creation failed (%E)", errid);
    return NULL;
  }

  udp = MALLOC_ONE_TAGGED(Scheme_UDP);
  udp->so.type = scheme_udp_type;
  udp->s = s;
  udp->bound = 0;
  udp->connected = 0;
  udp->previous_from_addr = NULL;

#ifdef USE_WINSOCK_TCP
  {
    unsigned long ioarg = 1;
    BOOL bc = 1;
    ioctlsocket(s, FIONBIO, &ioarg);
    setsockopt(s, SOL_SOCKET, SO_BROADCAST, (char *)(&bc), sizeof(BOOL));
  }
#else
  fcntl(s, F_SETFL, MZ_NONBLOCKING);
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
		   "udp-open-socket: not supported on this platform");
  return NULL;
#endif
}

static Scheme_Object *
udp_close(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_type("udp-close", "udp socket", 0, argc, argv);

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
    scheme_wrong_type("udp-bound?", "udp socket", 0, argc, argv);

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
    scheme_wrong_type("udp-connected?", "udp socket", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  return (((Scheme_UDP *)argv[0])->connected ? scheme_true : scheme_false);
#else
  return scheme_void;
#endif
}

static Scheme_Object *udp_bind_or_connect(const char *name, int argc, Scheme_Object *argv[], int do_bind)
{
#ifdef UDP_IS_SUPPORTED
  Scheme_UDP *udp;
  char *address = "";
  unsigned short origid, id;
  GC_CAN_IGNORE tcp_address udp_bind_addr;
  int errid;

  udp = (Scheme_UDP *)argv[0];
#endif

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_type(name, "udp socket", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  if (!SCHEME_FALSEP(argv[1]) && !SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_type(name, (do_bind ? "string or #f" : "string"), 1, argc, argv);
  if ((do_bind || !SCHEME_FALSEP(argv[2])) && !CHECK_PORT_ID(argv[2]))
    scheme_wrong_type(name, (do_bind ? PORT_ID_TYPE : PORT_ID_TYPE " or #f"), 2, argc, argv);
		      
  if (SCHEME_TRUEP(argv[1])) {
    Scheme_Object *bs;
    bs = scheme_char_string_to_byte_string(argv[1]);
    address = SCHEME_BYTE_STR_VAL(bs);
  } else
    address = NULL;
  if (SCHEME_TRUEP(argv[2]))
    origid = (unsigned short)SCHEME_INT_VAL(argv[2]);
  else
    origid = 0;

  if (!do_bind && (SCHEME_TRUEP(argv[1]) != SCHEME_TRUEP(argv[2]))) {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: last two arguments must be both #f or both non-#f, given: %V %V",
		     name, argv[1], argv[2]);
  }

  scheme_security_check_network(name, address, origid, !do_bind);

  if (udp->s == INVALID_SOCKET) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: udp socket was already closed: %V",
		     name,
		     udp);
    return NULL;
  }


  if (do_bind && udp->bound) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: udp socket is already bound: %V",
		     name,
		     udp);
    return NULL;
  }

  /* Set id in network order: */
  id = htons(origid);

  if (scheme_get_host_address(address, id, &udp_bind_addr)) {
    if (do_bind) {
      if (!bind(udp->s, (struct sockaddr *)&udp_bind_addr, sizeof(udp_bind_addr))) {
	udp->bound = 1;
	return scheme_void;
      }
      errid = SOCK_ERRNO();
    } else {
      int ok;

#ifdef USE_NULL_TO_DISCONNECT_UDP
      if (!origid) {
	if (udp->connected)
	  ok = !connect(udp->s, NULL, 0);
	else
	  ok = 1;
      } else
#endif
	ok = !connect(udp->s, (struct sockaddr *)&udp_bind_addr, sizeof(udp_bind_addr));
      if (!ok)
	errid = SOCK_ERRNO();
      else
	errid = 0;

      if (!ok && (errid == mz_AFNOSUPPORT) && !origid) {
	/* It's ok. We were trying to disconnect */
	ok = 1;
      }

      if (ok) {
	if (origid)
	  udp->connected = 1;
	else
	  udp->connected = 0;
	return scheme_void;
      }
    }

    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: can't %s to port: %d on address: %s (%E)", 
		     name,
		     do_bind ? "bind" : "connect",
		     origid,
		     address ? address : "#f",
		     errid);
    return NULL;
  } else {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: can't resolve address: %s", 
		     name,
		     address);
    return NULL;
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

static int udp_check_send(Scheme_Object *_udp)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (udp->s == INVALID_SOCKET)
    return 1;

  {
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_FDSET(writefds, 1);
    INIT_DECL_FDSET(exnfds, 1);
    
    MZ_FD_ZERO(writefds);
    MZ_FD_SET(udp->s, writefds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(udp->s, exnfds);
    
    do {
      sr = select(udp->s + 1, NULL, writefds, exnfds, &time);
    } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));
    
    return sr;
  }
}

static void udp_send_needs_wakeup(Scheme_Object *_udp, void *fds)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;
  void *fds1, *fds2;
  tcp_t s = udp->s;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);
  
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
}

#endif

static Scheme_Object *do_udp_send_it(const char *name, Scheme_UDP *udp,
				     char *bstr, long start, long end,
				     tcp_address *dest_addr, int can_block)
{
  long x;
  int errid = 0;

  while (1) {
    if (udp->s == INVALID_SOCKET) {
      /* socket was closed, maybe while we slept */
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: udp socket is closed: %V",
		       name, udp);
      return NULL;
    }
    if ((!dest_addr && !udp->connected)
	|| (dest_addr && udp->connected)) {
      /* socket is unconnected, maybe disconnected while we slept */
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: udp socket is%s connected: %V",
		       name, 
		       dest_addr ? "" : " not",
		       udp);
      return NULL;
    }

    udp->bound = 1; /* in case it's not bound already, send[to] binds it */

    if (dest_addr)
      x = sendto(udp->s, bstr XFORM_OK_PLUS start, end - start, 
		 0, (struct sockaddr *)dest_addr, sizeof(tcp_address));
    else
      x = send(udp->s, bstr XFORM_OK_PLUS start, end - start, 0);

    if (x == -1) {
      errid = SOCK_ERRNO();
      if (WAS_EAGAIN(errid)) {
	if (can_block) {
	  /* Block and eventually try again. */
	  scheme_block_until(udp_check_send, udp_send_needs_wakeup, (Scheme_Object *)udp, 0);
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
		     "%s: send failed (%E)", 
		     name,
		     errid);
    return NULL;
  }
}

static Scheme_Object *udp_send_it(const char *name, int argc, Scheme_Object *argv[],
				  int with_addr, int can_block, Scheme_UDP_Evt *fill_evt)
{
#ifdef UDP_IS_SUPPORTED
  Scheme_UDP *udp;
  char *address = "";
  long start, end;
  int delta;
  unsigned short origid, id;
  GC_CAN_IGNORE tcp_address udp_dest_addr;

  udp = (Scheme_UDP *)argv[0];
#endif

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_type(name, "udp socket", 0, argc, argv);

#ifdef UDP_IS_SUPPORTED
  if (with_addr) {
    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_type(name, "string", 1, argc, argv);
    if (!CHECK_PORT_ID(argv[2]))
      scheme_wrong_type(name, PORT_ID_TYPE, 2, argc, argv);
    delta = 0;
  } else
    delta = -2;

  if (!SCHEME_BYTE_STRINGP(argv[3 + delta]))
    scheme_wrong_type(name, "byte string", 3 + delta, argc, argv);
  
  scheme_get_substring_indices(name, argv[3 + delta], 
			       argc, argv,
			       4 + delta, 5 + delta, &start, &end);

  if (with_addr) {
    Scheme_Object *bs;
    bs = scheme_char_string_to_byte_string(argv[1]);
    address = SCHEME_BYTE_STR_VAL(bs);
    origid = (unsigned short)SCHEME_INT_VAL(argv[2]);

    scheme_security_check_network(name, address, origid, 1);

    /* Set id in network order: */
    id = htons(origid);
  } else {
    address = NULL;
    id = origid = 0;
  }

  if (!with_addr || scheme_get_host_address(address, id, &udp_dest_addr)) {
    if (fill_evt) {
      fill_evt->str = SCHEME_BYTE_STR_VAL(argv[3+delta]);
      fill_evt->offset = start;
      fill_evt->len = end - start;
      memcpy(&fill_evt->dest_addr, &udp_dest_addr, sizeof(tcp_address));
      return scheme_void;
    } else {
      return do_udp_send_it(name, udp,
			    SCHEME_BYTE_STR_VAL(argv[3+delta]), start, end,
			    (with_addr ? &udp_dest_addr : NULL), can_block);
    }
  } else {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: can't resolve address: %s", 
		     name,
		     address);
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

static int udp_check_recv(Scheme_Object *_udp)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (udp->s == INVALID_SOCKET)
    return 1;

  {
    DECL_FDSET(readfds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_FDSET(readfds, 1);
    INIT_DECL_FDSET(exnfds, 1);
    
    MZ_FD_ZERO(readfds);
    MZ_FD_SET(udp->s, readfds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(udp->s, exnfds);
    
    do {
      sr = select(udp->s + 1, readfds, NULL, exnfds, &time);
    } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));
    
    return sr;
  }
}

static void udp_recv_needs_wakeup(Scheme_Object *_udp, void *fds)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;
  void *fds1, *fds2;

  tcp_t s = udp->s;
  
  fds1 = MZ_GET_FDSET(fds, 0);
  fds2 = MZ_GET_FDSET(fds, 2);
  
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
}

#endif

static int do_udp_recv(const char *name, Scheme_UDP *udp, char *bstr, long start, long end, 
		       int can_block, Scheme_Object **v)
{
#ifdef UDP_IS_SUPPORTED
  long x;
  int errid = 0;
  GC_CAN_IGNORE tcp_address udp_src_addr;

  if (!udp->bound) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: udp socket is not bound: %V",
		     name,
		     udp);
    return 0;
  }

  while (1) {
    if (udp->s == INVALID_SOCKET) {
      /* socket was closed, maybe while we slept */
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "%s: udp socket is closed: %V",
		       name, udp);
      return 0;
    }

    {
      int asize = sizeof(udp_src_addr);
      x = recvfrom(udp->s, bstr XFORM_OK_PLUS start, end - start, 0,
		   (struct sockaddr *)&udp_src_addr, &asize);
    }

    if (x == -1) {
      errid = SOCK_ERRNO();
      if (WAS_WSAEMSGSIZE(errid)) {
	x = end - start;
	errid = 0;
      } if (WAS_EAGAIN(errid)) {
	if (can_block) {
	  /* Block and eventually try again. */
	  scheme_block_until(udp_check_recv, udp_recv_needs_wakeup, (Scheme_Object *)udp, 0);
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
    int id;
    char buf[16];
    unsigned char *a;

    v[0] = scheme_make_integer(x);

    a = (unsigned char *)&udp_src_addr.sin_addr;
    sprintf(buf, "%d.%d.%d.%d", a[0], a[1], a[2], a[3]);
    if (udp->previous_from_addr && !strcmp(SCHEME_BYTE_STR_VAL(udp->previous_from_addr), buf)) {
      v[1] = udp->previous_from_addr;
    } else {
      Scheme_Object *vv;
      vv = scheme_make_immutable_sized_byte_string(buf, -1, 1);
      v[1] = vv;
      udp->previous_from_addr = v[1];
    }

    id = ntohs(udp_src_addr.sin_port);
    v[2] = scheme_make_integer(id);

    return 1;
  } else {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: receive failed (%E)", 
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
  long start, end;
  Scheme_Object *v[3];

  udp = (Scheme_UDP *)argv[0];

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_type(name, "udp socket", 0, argc, argv);
  if (!SCHEME_BYTE_STRINGP(argv[1]) || !SCHEME_MUTABLEP(argv[1]))
    scheme_wrong_type(name, "mutable byte string", 1, argc, argv);
  
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
    scheme_wrong_type(name, "udp socket", 0, argc, argv);

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
  ((Scheme_UDP_Evt *)evt)->with_addr = 1;
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
	scheme_set_sync_target(sinfo, scheme_build_list(3, v), NULL, NULL, 0, 0);
	return 1;
      } else
	return 0;
    } else {
      return udp_check_recv((Scheme_Object *)uw->udp);
    }
  } else {
    if (uw->str) {
      Scheme_Object *r;
      GC_CAN_IGNORE tcp_address dest_addr;
      if (uw->with_addr)
	memcpy(&dest_addr, &uw->dest_addr, sizeof(tcp_address));
      r = do_udp_send_it("udp-send-evt", uw->udp, 
			 uw->str, uw->offset, uw->offset + uw->len, 
			 (uw->with_addr ? &dest_addr : 0), 0);
      if (SCHEME_TRUEP(r)) {
	scheme_set_sync_target(sinfo, scheme_void, NULL, NULL, 0, 0);
	return 1;
      } else
	return 0;
    } else
      return udp_check_send((Scheme_Object *)uw->udp);
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



/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_NETWORK_C
#include "mzmark.c"

static void register_traversers(void)
{
#ifdef USE_TCP
  GC_REG_TRAV(scheme_rt_tcp, mark_tcp);
# ifdef USE_MAC_TCP
  GC_REG_TRAV(scheme_rt_write_data, mark_write_data);
# endif
# ifdef UDP_IS_SUPPORTED
  GC_REG_TRAV(scheme_udp_type, mark_udp);
  GC_REG_TRAV(scheme_udp_evt_type, mark_udp_evt);
# endif
#endif
  GC_REG_TRAV(scheme_listener_type, mark_listener);  
}

END_XFORM_SKIP;

#endif

#endif /* !NO_TCP_SUPPORT */

