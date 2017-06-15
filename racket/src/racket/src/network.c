/*
  Racket
  Copyright (c) 2004-2017 PLT Design Inc.
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
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file implements the TCP and UDP interfaces. */

#include "schpriv.h"
#include "schrktio.h"
#include <ctype.h>

#ifndef NO_TCP_SUPPORT
#ifdef USE_TCP

#define TCP_BUFFER_SIZE 4096

typedef struct {
  Scheme_Object so;
  Scheme_Custodian_Reference *mref;
  rktio_listener_t *lnr;
} listener_t;

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
  rktio_fd_t *tcp;
  int flags;
} Scheme_Tcp;

# define MZ_TCP_ABANDON_OUTPUT 0x1
# define MZ_TCP_ABANDON_INPUT  0x2

typedef struct Scheme_UDP {
  Scheme_Object so; /* scheme_udp_type */
  MZ_HASH_KEY_EX
  rktio_fd_t *s;
  char bound, connected;
  Scheme_Object *previous_from_addr;
  Scheme_Custodian_Reference *mref;
} Scheme_UDP;

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
static Scheme_Object *udp_multicast_loopback_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_multicast_set_loopback(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_multicast_ttl(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_multicast_set_ttl(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_multicast_interface(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_multicast_set_interface(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_multicast_join_group(int argc, Scheme_Object *argv[]);
static Scheme_Object *udp_multicast_leave_group(int argc, Scheme_Object *argv[]);

static int tcp_check_accept_evt(Scheme_Object *ae, Scheme_Schedule_Info *sinfo);
static void tcp_accept_evt_needs_wakeup(Scheme_Object *_ae, void *fds);
static int udp_evt_check_ready(Scheme_Object *uw, Scheme_Schedule_Info *sinfo);
static void udp_evt_needs_wakeup(Scheme_Object *_uw, void *fds);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_network(Scheme_Env *env)
{
  Scheme_Env *netenv;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  netenv = scheme_primitive_module(scheme_intern_symbol("#%network"), env);

  GLOBAL_PRIM_W_ARITY2 ( "tcp-connect"               , tcp_connect              , 2 , 4 , 2 , 2 , netenv ) ;
  GLOBAL_PRIM_W_ARITY2 ( "tcp-connect/enable-break"  , tcp_connect_break        , 2 , 4 , 2 , 2 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "tcp-listen"                , tcp_listen               , 1 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "tcp-close"                 , tcp_stop                 , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "tcp-accept-ready?"         , tcp_accept_ready         , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY2 ( "tcp-accept"                , tcp_accept               , 1 , 1 , 2 , 2 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "tcp-accept-evt"            , tcp_accept_evt           , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY2 ( "tcp-accept/enable-break"   , tcp_accept_break         , 1 , 1 , 2 , 2 , netenv ) ;
  GLOBAL_FOLDING_PRIM  ( "tcp-listener?"             , tcp_listener_p           , 1 , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY2 ( "tcp-addresses"             , tcp_addresses            , 1 , 2 , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "tcp-abandon-port"          , tcp_abandon_port         , 1 , 1 , netenv ) ;
  GLOBAL_FOLDING_PRIM  ( "tcp-port?"                 , tcp_port_p               , 1 , 1 , 1 , netenv ) ;

  GLOBAL_PRIM_W_ARITY  ( "udp-open-socket"           , make_udp                 , 0 , 2 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-close"                 , udp_close                , 1 , 1 , netenv ) ;
  GLOBAL_FOLDING_PRIM  ( "udp?"                      , udp_p                    , 1 , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-bound?"                , udp_bound_p              , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-connected?"            , udp_connected_p          , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-bind!"                 , udp_bind                 , 3 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-connect!"              , udp_connect              , 3 , 3 , netenv ) ;

  GLOBAL_PRIM_W_ARITY  ( "udp-send-to"               , udp_send_to              , 4 , 6 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send"                  , udp_send                 , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send-to*"              , udp_send_to_star         , 4 , 6 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send*"                 , udp_send_star            , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send-to/enable-break"  , udp_send_to_enable_break , 4 , 6 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send/enable-break"     , udp_send_enable_break    , 2 , 4 , netenv ) ;

  GLOBAL_PRIM_W_ARITY  ( "udp-receive!"              , udp_receive              , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-receive!*"             , udp_receive_star         , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-receive!/enable-break" , udp_receive_enable_break , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-receive-ready-evt"     , udp_read_ready_evt       , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send-ready-evt"        , udp_write_ready_evt      , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-receive!-evt"          , udp_read_evt             , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send-evt"              , udp_write_evt            , 2 , 4 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-send-to-evt"           , udp_write_to_evt         , 4 , 6 , netenv ) ;

  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-loopback?"   , udp_multicast_loopback_p , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-set-loopback!", udp_multicast_set_loopback,2, 2 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-ttl"         , udp_multicast_ttl        , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-set-ttl!"    , udp_multicast_set_ttl    , 2 , 2 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-interface"   , udp_multicast_interface  , 1 , 1 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-set-interface!", udp_multicast_set_interface,2,2, netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-join-group!" , udp_multicast_join_group , 3 , 3 , netenv ) ;
  GLOBAL_PRIM_W_ARITY  ( "udp-multicast-leave-group!", udp_multicast_leave_group, 3 , 3 , netenv ) ;

  scheme_finish_primitive_module(netenv);
}

static int check_fd_sema(rktio_fd_t s, int mode, Scheme_Schedule_Info *sinfo, Scheme_Object *orig)
{
  Scheme_Object *sema;

  sema = scheme_rktio_fd_to_semaphore(s, mode);
  
  if (sema) {
    if (!scheme_wait_sema(sema, 1)) {
      if (sinfo && !sinfo->no_redirect)
        scheme_set_sync_target(sinfo, sema, orig, NULL, 0, 0, NULL);
      return 0;
    }
  }

  return 1;
}

/*========================================================================*/
/*                             TCP glue                                   */
/*========================================================================*/


/* These two need o be outside of USE_TCP */
#define PORT_ID_TYPE "(integer-in 1 65535)"
#define CHECK_PORT_ID(obj) (SCHEME_INTP(obj) && (SCHEME_INT_VAL(obj) >= 1) && (SCHEME_INT_VAL(obj) <= 65535))
#define LISTEN_PORT_ID_TYPE "(integer-in 0 65535)"
#define CHECK_LISTEN_PORT_ID(obj) (SCHEME_INTP(obj) && (SCHEME_INT_VAL(obj) >= 0) && (SCHEME_INT_VAL(obj) <= 65535))

#ifdef USE_TCP

#define REGISTER_SOCKET(s) /**/
#define UNREGISTER_SOCKET(s) /**/

#ifdef USE_UNIX_SOCKETS_TCP
typedef struct sockaddr_in mz_unspec_address;
#endif
#ifdef USE_WINSOCK_TCP
typedef struct SOCKADDR_IN mz_unspec_address;
# undef REGISTER_SOCKET
# undef UNREGISTER_SOCKET
# define REGISTER_SOCKET(s) winsock_remember(s)
# define UNREGISTER_SOCKET(s) winsock_forget(s)
#endif

/******************************* hostnames ************************************/

#if defined(OS_X) || defined(USE_PTHREAD_THREAD_TIMER)
# define PTHREADS_OK_FOR_GHBN
#endif

# ifdef PROTOENT_IS_INT
#  define PROTO_P_PROTO PROTOENT_IS_INT
# else
SHARED_OK static struct protoent *proto;
#  define PROTO_P_PROTO (proto ? proto->p_proto : 0)
# endif

# ifndef MZ_PF_INET
#  define MZ_PF_INET PF_INET
# endif

/* For getting connection names: */
#define MZ_SOCK_NAME_MAX_LEN 256
#define MZ_SOCK_HOST_NAME_MAX_LEN 64
#define MZ_SOCK_SVC_NAME_MAX_LEN 32

/* mz_addrinfo is defined in scheme.h */

#if defined(__MINGW32__) && !defined(HAVE_GETADDRINFO)
/* Although `configure` didn't discover it, we do have getaddrinfo()
   from Winsock */
# define HAVE_GETADDRINFO
#endif

#ifdef HAVE_GETADDRINFO
# define mzAI_PASSIVE AI_PASSIVE 
# define mz_getaddrinfo getaddrinfo
# define mz_freeaddrinfo freeaddrinfo
# define mz_gai_strerror gai_strerror
#else
# define mzAI_PASSIVE 0
static int mz_getaddrinfo(const char *nodename, const char *servname,
			  const struct mz_addrinfo *hints, struct mz_addrinfo **res)
  XFORM_SKIP_PROC
{
  struct hostent *h;

  if (nodename)
    h = gethostbyname(nodename);
  else
    h = NULL;

  if (h || !nodename) {
    GC_CAN_IGNORE struct mz_addrinfo *ai;
    GC_CAN_IGNORE struct sockaddr_in *sa;
    int j, id = 0;

    ai = (struct mz_addrinfo *)malloc(sizeof(struct mz_addrinfo));
    sa = (struct sockaddr_in *)malloc(sizeof(struct sockaddr_in));
    ai->ai_addr = (struct sockaddr *)sa;

    ai->ai_addrlen = sizeof(struct sockaddr_in);
    if (servname) {
      for (j = 0; servname[j]; j++) {
	id = (id * 10) + (servname[j] - '0');
      }
    }

    ai->ai_family = MZ_PF_INET;
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
void mz_freeaddrinfo(struct mz_addrinfo *ai)
  XFORM_SKIP_PROC
{
  free(ai->ai_addr);
  free(ai);
}
const char *mz_gai_strerror(int ecode)
  XFORM_SKIP_PROC
{
  return hstrerror(ecode);
}
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

typedef struct {
# ifdef USE_WINSOCK_TCP
  HANDLE th;
# else
  int pin;
# endif
  struct mz_addrinfo *result;
  int err;
  int done;
} GHBN_Rec;

/* For in-thread DNS: */
#define MZ_MAX_HOSTNAME_LEN 128
#define MZ_MAX_SERVNAME_LEN 32

typedef struct GHBN_Thread_Data {
  int ghbn_lock;
  char ghbn_hostname[MZ_MAX_HOSTNAME_LEN];
  char ghbn_servname[MZ_MAX_SERVNAME_LEN];
  struct mz_addrinfo ghbn_hints;
# ifdef USE_WINSOCK_TCP
  HANDLE ready_sema;
# else
  int ready_fd;
# endif
  struct mz_addrinfo *ghbn_result;
  int ghbn_err;
} GHBN_Thread_Data;

THREAD_LOCAL_DECL(GHBN_Thread_Data *ghbn_thread_data);

static intptr_t getaddrinfo_in_thread(void *_data)
  XFORM_SKIP_PROC
{
  GHBN_Thread_Data *data = (GHBN_Thread_Data *)_data;
  int ok;
  struct mz_addrinfo *res, hints;
  char hn_copy[MZ_MAX_HOSTNAME_LEN], sn_copy[MZ_MAX_SERVNAME_LEN];
# ifndef USE_WINSOCK_TCP
  int fd = data->ready_fd;
  int cr;
# endif
  
  if (data->ghbn_result) {
    mz_freeaddrinfo(data->ghbn_result);
    data->ghbn_result = NULL;
  }

  strcpy(hn_copy, data->ghbn_hostname);
  strcpy(sn_copy, data->ghbn_servname);
  memcpy(&hints, &data->ghbn_hints, sizeof(hints));

# ifdef USE_WINSOCK_TCP
  ReleaseSemaphore(data->ready_sema, 1, NULL);  
# else
  do {
    cr = write(fd, "?", 1);
  } while ((cr == -1) && (errno == EINTR));
# endif

  res = NULL;

  ok = mz_getaddrinfo(hn_copy[0] ? hn_copy : NULL, 
		      sn_copy[0] ? sn_copy : NULL, 
		      &hints, &res);

  data->ghbn_result = res;
  data->ghbn_err = ok;

# ifndef USE_WINSOCK_TCP
  {
    long v = 1;
    do {
      cr = write(fd, &v, sizeof(v));
    } while ((cr == -1) && (errno == EINTR));
    do {
      cr = close(fd);
    } while ((cr == -1) && (errno == EINTR));
  }
# endif

  return 1;
}

#ifdef USE_WINSOCK_TCP
static unsigned int WINAPI win_getaddrinfo_in_thread(void *_data)
  XFORM_SKIP_PROC
{
  return (unsigned int)getaddrinfo_in_thread(_data);
}
#endif

static void release_ghbn_lock(GHBN_Rec *rec)
{
  ghbn_thread_data->ghbn_lock = 0;
# ifdef USE_WINSOCK_TCP
  CloseHandle(rec->th);
# else
  close(rec->pin);
# endif
}

static int ghbn_lock_avail(Scheme_Object *_ignored)
{
  return !ghbn_thread_data->ghbn_lock;
}

static int ghbn_thread_done(Scheme_Object *_rec)
{
  GHBN_Rec *rec = (GHBN_Rec *)_rec;

  if (rec->done)
    return 1;

# ifdef USE_WINSOCK_TCP
  if (WaitForSingleObject(rec->th, 0) == WAIT_OBJECT_0) {
    rec->result = ghbn_thread_data->ghbn_result;
    ghbn_thread_data->ghbn_result = NULL;
    rec->err = ghbn_thread_data->ghbn_err;
    rec->done = 1;
    return 1;
  }
# else
  {
    long v;
    int cr;
    do {
      cr = read(rec->pin, &v, sizeof(long));
    } while ((cr == -1) && (errno == EINTR));
    if (cr > 0) {
      rec->result = ghbn_thread_data->ghbn_result;
      ghbn_thread_data->ghbn_result = NULL;
      rec->err = ghbn_thread_data->ghbn_err;
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

static int MZ_GETADDRINFO(const char *name, const char *svc, struct mz_addrinfo *hints, struct mz_addrinfo **res)
{
  GHBN_Rec *rec;
  int ok;

  if ((name && ((strlen(name) >= MZ_MAX_HOSTNAME_LEN) || !name[0]))
      || (svc && ((strlen(svc) >= MZ_MAX_SERVNAME_LEN) || !svc[0]))) {
    /* Give up on a separate thread. */
    return mz_getaddrinfo(name, svc, hints, res);
  }

  if (!ghbn_thread_data) {
    ghbn_thread_data = (GHBN_Thread_Data *)malloc(sizeof(GHBN_Thread_Data));
    memset(ghbn_thread_data, 0, sizeof(GHBN_Thread_Data));
  }

  rec = MALLOC_ONE_ATOMIC(GHBN_Rec);
  rec->done = 0;

  scheme_block_until(ghbn_lock_avail, NULL, NULL, 0);

  ghbn_thread_data->ghbn_lock = 1;

  if (name)
    strcpy(ghbn_thread_data->ghbn_hostname, name);
  else
    ghbn_thread_data->ghbn_hostname[0] = 0;
  if (svc)
    strcpy(ghbn_thread_data->ghbn_servname, svc);
  else
    ghbn_thread_data->ghbn_servname[0] = 0;
  memcpy(&ghbn_thread_data->ghbn_hints, hints, sizeof(*hints));

# ifdef USE_WINSOCK_TCP
  {
    HANDLE ready_sema;
    unsigned int id;
    intptr_t th;
    
    ready_sema = CreateSemaphore(NULL, 0, 1, NULL);
    ghbn_thread_data->ready_sema = ready_sema;
    th = _beginthreadex(NULL, 5000, 
			win_getaddrinfo_in_thread,
			ghbn_thread_data, 0, &id);
    WaitForSingleObject(ghbn_thread_data->ready_sema, INFINITE);
    CloseHandle(ghbn_thread_data->ready_sema);
    
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
      ghbn_thread_data->ready_fd = p[1];
      if (pthread_create(&t, NULL, 
			 (MZ_LPTHREAD_START_ROUTINE)getaddrinfo_in_thread,
			 ghbn_thread_data)) {
	close(p[0]);
	close(p[1]);
	ok = 0;
      } else {
	char buf[1];
        int cr;
	pthread_detach(t);
        do {
          cr = read(rec->pin, buf, 1);
        } while ((cr == -1) && (errno == EINTR));
	fcntl(rec->pin, F_SETFL, MZ_NONBLOCKING);
	ok = 1;
      }
    }

    if (!ok) {
      getaddrinfo_in_thread(ghbn_thread_data);
      rec->result = ghbn_thread_data->ghbn_result;
      ghbn_thread_data->ghbn_result = NULL;
      rec->err = ghbn_thread_data->ghbn_err;
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

  ghbn_thread_data->ghbn_lock = 0;

  *res = rec->result;

  return rec->err;
}

void scheme_free_ghbn_data() {
  if (ghbn_thread_data) {
    free(ghbn_thread_data);
    ghbn_thread_data = NULL;
  }
}
#else
# define MZ_GETADDRINFO mz_getaddrinfo
void scheme_free_ghbn_data() { }
#endif

struct mz_addrinfo *scheme_get_host_address(const char *address, int id, int *err, 
					    int family, int passive, int tcp)
{
  char buf[32], *service;
  int ok;
  GC_CAN_IGNORE struct mz_addrinfo *r, hints;
  r = NULL;

  if (id >= 0) {
    service = buf;
    sprintf(buf, "%d", id);
  } else
    service = NULL;
  
  if (!address && !service) {
    *err = -1;
    return NULL;
  }

  memset(&hints, 0, sizeof(struct mz_addrinfo));
  hints.ai_family = ((family < 0) ? PF_UNSPEC : family);
  if (passive) {
    hints.ai_flags |= mzAI_PASSIVE;
  }
  if (tcp) {
    hints.ai_socktype = SOCK_STREAM;
# ifndef PROTOENT_IS_INT
    if (!proto) {
      proto = getprotobyname("tcp");
    }
# endif
    hints.ai_protocol= PROTO_P_PROTO;
  } else {
    hints.ai_socktype = SOCK_DGRAM;
  }

  ok = MZ_GETADDRINFO(address, service, &hints, &r);
  *err = ok;

  if (!ok)
    return r;
  else
    return NULL;
}

void scheme_free_host_address(struct mz_addrinfo *a)
{
  mz_freeaddrinfo(a);
}

const char *scheme_host_address_strerror(int errnum)
{
  return mz_gai_strerror(errnum);
}

/******************************* WinSock ***********************************/

#ifdef USE_WINSOCK_TCP

static int wsr_size = 0;
static tcp_t *wsr_array;

static void winsock_remember(tcp_t s)
{
  int i, new_size;
  tcp_t *naya;

# ifdef MZ_USE_PLACES
  WaitForSingleObject(winsock_sema, INFINITE);
# endif

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

    naya = malloc(sizeof(tcp_t) * new_size);
    for (i = 0; i < wsr_size; i++) {
      naya[i] = wsr_array[i];
    }

    naya[wsr_size] = s;

    if (wsr_array) free(wsr_array);

    wsr_array = naya;
    wsr_size = new_size;
  }  

# ifdef MZ_USE_PLACES
  ReleaseSemaphore(winsock_sema, 1, NULL);
# endif
}

static void winsock_forget(tcp_t s)
{
  int i;

# ifdef MZ_USE_PLACES
  WaitForSingleObject(winsock_sema, INFINITE);
# endif

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i] == s) {
      wsr_array[i] = (tcp_t)NULL;
      break;
    }
  }

# ifdef MZ_USE_PLACES
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
      wsr_array[i] = (tcp_t)NULL;
    }
  }

  return WSACleanup();
}

static void TCP_INIT(char *name)
{
  static int started = 0;
  
# ifdef MZ_USE_PLACES
  WaitForSingleObject(winsock_sema, INFINITE);
# endif

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

  if (!started)
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		     "%s: no winsock driver",
		     name);
  
# ifdef MZ_USE_PLACES
  ReleaseSemaphore(winsock_sema, 1, NULL);
# endif
}
#else
/* Not Winsock */
# define TCP_INIT(x) /* nothing */
#endif

/*========================================================================*/
/*                       TCP ports and listeners                          */
/*========================================================================*/

#define LISTENER_WAS_CLOSED(x) (!((listener_t *)(x))->lnr)

static int tcp_check_accept(Scheme_Object *_listener, Scheme_Schedule_Info *sinfo)
{
  listener_t *listener = (listener_t *)_listener;
  int sr, i;

  if (!sinfo || !sinfo->is_poll) {
    /* If listeners supported semaphores, we could have a check here
       along the lines of check_fd_sema(). See CREATE_LISTEN_SEMA in
       two places below. */
  }

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  if (rktio_poll_accept_ready(scheme_rktio, listener->lnr))
    return 1;

  if (sinfo && !sinfo->no_redirect) {
    Scheme_Object *evt;
    evt = listener_to_evt(listener);
    if (evt)
      scheme_set_sync_target(sinfo, evt, _listener, NULL, 0, 1, NULL);
  } else {
    /* CREATE_LISTEN_SEMA: This is where we'd create a semaphore for the listener, if that were possible */
  }

  return 0;
}

static int stop_listener(Scheme_Object *o)
{
  int was_closed = 0;
  listener_t *listener = (listener_t *)o;
  
  if (listener->lnr) {
    /* CREATE_LISTEN_SEMA: if we have listener semaphores, unregister
       a semaphore here */
    
    rktio_listen_stop(scheme_rktio, listener->lnr);
    listener->lnr = NULL;
    
    scheme_remove_managed(((listener_t *)o)->mref, o);
  } else {
    was_closed = 1;
  }

  return was_closed;
}

static void tcp_accept_needs_wakeup(Scheme_Object *_listener, void *fds)
{
  if (!LISTENER_WAS_CLOSED(_listener)) {
    listener_t *listener = (listener_t *)_listener;
    rktio_poll_add_accept(scheme_rktio, listener->lnr, fds);
  }
}

static int tcp_check_write(Scheme_Object *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Tcp *data = (Scheme_Tcp *)((Scheme_Output_Port *)port)->port_data;
    
  if (((Scheme_Output_Port *)port)->closed)
    return 1;

  if (!sinfo || !sinfo->is_poll) {
    if (!check_fd_sema(data->tcp, MZFD_CHECK_WRITE, sinfo, port))
      return 0;
  }

  if (rktio_poll_write_ready(scheme_rktio, data->tcp))
    return 1;

  check_fd_sema(data->tcp, MZFD_CREATE_WRITE, sinfo, port);

  return 0;
}

static void tcp_write_needs_wakeup(Scheme_Object *port, void *fds)
{
  rktio_fd_t s = ((Scheme_Tcp *)((Scheme_Output_Port *)port)->port_data)->tcp;
  
  rktio_poll_add(scheme_rktio, s, fds, RKTIO_POLL_WRITE);
}


static Scheme_Tcp *make_tcp_port_data(rktio_fd_t *tcp, int refcount)
{
  Scheme_Tcp *data;
  char *bfr;
  
  data = MALLOC_ONE_RT(Scheme_Tcp);
#ifdef MZTAG_REQUIRED
  data->b.type = scheme_rt_tcp;
#endif
  data->tcp = tcp;

  bfr = (char *)scheme_malloc_atomic(TCP_BUFFER_SIZE);
  data->b.buffer = bfr;
  bfr = (char *)scheme_malloc_atomic(TCP_BUFFER_SIZE);
  data->b.out_buffer = bfr;

  data->b.bufpos = 0;
  data->b.bufmax = 0;
  data->b.hiteof = 0;
  data->b.refcount = refcount;

  return data;
}

static int tcp_byte_ready (Scheme_Input_Port *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Tcp *data;

  if (port->closed)
    return 1;

  data = (Scheme_Tcp *)port->port_data;
  
  if (data->b.hiteof)
    return 1;
  if (data->b.bufpos < data->b.bufmax)
    return 1;

  if (!sinfo || !sinfo->is_poll) {
    if (!check_fd_sema(data->tcp, MZFD_CHECK_READ, sinfo, (Scheme_Object *)port))
      return 0;
  }

  if (rktio_poll_read_ready(scheme_rktio, data->tcp))
    return 1;

  check_fd_sema(data->tcp, MZFD_CREATE_READ, sinfo, (Scheme_Object *)port);

  return 0;
}

static intptr_t tcp_get_string(Scheme_Input_Port *port, 
			   char *buffer, intptr_t offset, intptr_t size,
			   int nonblock,
			   Scheme_Object *unless)
{
  int errid;
  int read_amt;
  Scheme_Tcp *data;
  Scheme_Object *sema;

  data = (Scheme_Tcp *)port->port_data;

  while (1) {
    if (scheme_unless_ready(unless))
      return SCHEME_UNLESS_READY;

    if (port->closed) {
      /* Another thread closed the input port while we were waiting. */
      /* Call scheme_get_byte to signal the error */
      scheme_get_byte((Scheme_Object *)port);
    }

    if (data->b.hiteof)
      return EOF;
    
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
    
    /* No data in buffer, so read from socket: */
    
    if (!data->b.bufmode || (size > TCP_BUFFER_SIZE))
      read_amt = TCP_BUFFER_SIZE;
    else
      read_amt = size;

    {
      int rn;
      rn = rktio_read(scheme_rktio, data->tcp, data->b.buffer, read_amt, 0);
      data->b.bufmax = rn; /* could be count, error, or EOF */
    }
    
    if (data->b.bufmax) {
      /* got data, error, or EOF */
      break;
    } else {
      /* no data/error is immediately avaulable */
      if (nonblock > 0)
        return 0;

      /* block until data is (probably) available */
      sema = scheme_rktio_fd_to_semaphore(data->tcp, MZFD_CREATE_READ, 1);
      if (sema)
        scheme_wait_sema(sema, nonblock ? -1 : 0);
      else
        scheme_block_until_unless((Scheme_Ready_Fun)tcp_byte_ready,
                                  scheme_need_wakeup,
                                  (Scheme_Object *)port,
                                  0.0, unless,
                                  nonblock);
      
      scheme_wait_input_allowed(port, nonblock);
    }
  }

  /* getting here means that data or error was ready */
  
  if (data->b.bufmax == RKTIO_READ_ERROR) {
    data->b.bufmax = 0;
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-read: error reading\n"
                     "  system error: %R",
		     errid);
    return 0;
  } else if (data->b.bufmax == RKTIO_READ_ERROR) {
    data->b.bufmax = 0;
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
  Scheme_Tcp *data = (Scheme_Tcp *)port->port_data;

  rktio_poll_add(scheme_rktio, data->tcp, fds, RKTIO_POLL_READ);
}

static void tcp_close_input(Scheme_Input_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (!(data->flags & MZ_TCP_ABANDON_INPUT))
    rktio_socket_shutdown(scheme_rktio, data->tcp, RKTIO_SHUTDOWN_READ);

  if (--data->b.refcount)
    return;

  (void)scheme_rktio_fd_to_semaphore(data->tcp, MZFD_REMOVE, 1);

  rktio_close(data->tcp);
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

static intptr_t tcp_do_write_string(Scheme_Output_Port *port, 
                                    const char *s, intptr_t offset, intptr_t len, 
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
  intptr_t sent;

  data = (Scheme_Tcp *)port->port_data;

 top:

  sent = rktio_write(scheme_rktio, data->tcp, s XFORM_OK_PLUS offset, len);

  if (sent != len) {
    if (sent > 0) {
      /* Some data was sent. Return, or recur to handle the rest. */
      if (rarely_block)
	return sent;
      else
	sent += tcp_do_write_string(port, s, offset + sent, len - sent, 0, enable_break);
    }
  }

  if (sent == 0) {
    if (rarely_block == 2)
      return 0;

    /* Block for writing: */
    {
      Scheme_Object *sema;
      sema = scheme_rktio_fd_to_semaphore(data->tcp, MZFD_CREATE_WRITE, 1);
      if (sema)
        scheme_wait_sema(sema, enable_break ? -1 : 0);
      else
        scheme_block_until_enable_break((Scheme_Ready_Fun)tcp_check_write, 
                                        tcp_write_needs_wakeup, 
                                        (Scheme_Object *)port, 
                                        (float)0.0, enable_break);
    }

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

  if (sent == RKTIO_WRITE_ERROR)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-write: error writing\n"
                     "  system error: %R");

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

static intptr_t tcp_write_string(Scheme_Output_Port *port, 
			     const char *s, intptr_t offset, intptr_t len, 
			     int rarely_block, int enable_break)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (!len) {
    /* Flush */
    tcp_flush(port, rarely_block, enable_break);
    if (data->b.out_bufpos != data->b.out_bufmax)
      return -1;
    return 0;
  }

  if (rarely_block) {
    tcp_flush(port, rarely_block, enable_break);
    if (data->b.out_bufmax)
      return -1;
  } else {
    if (data->b.out_bufmode < 2) {
      if (data->b.out_bufmax + len < TCP_BUFFER_SIZE) {
	memcpy(data->b.out_buffer + data->b.out_bufmax, s + offset, len);
	data->b.out_bufmax += (short)len;
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

  if (!(data->flags & MZ_TCP_ABANDON_OUTPUT)) {
    rktio_socket_shutdown(scheme_rktio, data->tcp, RKTIO_SHUTDOWN_WRITE);
  }

  if (--data->b.refcount)
    return;

  (void)scheme_rktio_fd_to_semaphore(data->tcp, MZFD_REMOVE, 1);

  rktio_close(data->tcp);
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
make_tcp_input_port_symbol_name(void *data, Scheme_Object *name, Scheme_Object *cust)
{
  Scheme_Input_Port *ip;

  if (cust)
    scheme_set_next_port_custodian((Scheme_Custodian *)cust);
  
  ip = scheme_make_input_port(scheme_tcp_input_port_type,
			      data,
                              name,
			      tcp_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_peeked_read_via_get,
			      (Scheme_In_Ready_Fun)tcp_byte_ready,
			      tcp_close_input,
			      tcp_need_wakeup,
			      1);

  ip->p.buffer_mode_fun = tcp_in_buffer_mode;

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_tcp_input_port(void *data, const char *name, Scheme_Object *cust)
{
  return make_tcp_input_port_symbol_name(data, scheme_intern_symbol(name), cust);
}

static Scheme_Object *
make_tcp_output_port_symbol_name(void *data, Scheme_Object *name, Scheme_Object *cust)
{
  Scheme_Output_Port *op;

  if (cust)
    scheme_set_next_port_custodian((Scheme_Custodian *)cust);

  op = scheme_make_output_port(scheme_tcp_output_port_type,
						  data,
						  name,
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

static Scheme_Object *
make_tcp_output_port(void *data, const char *name, Scheme_Object *cust)
{
  return make_tcp_output_port_symbol_name(data, scheme_intern_symbol(name), cust);
}

/*========================================================================*/
/*                        Hostname lookup helper                          */
/*========================================================================*/

/* Various things to free if we ever give up on a connect: */
typedef struct Connect_Progress_Data {
  rktio_lookup_t *lookup;
  rktio_connect_t *connect;
  rktio_addrinfo_t *dest_addr;
  rktio_addrinfo_t *src_addr;
  rktio_fd_t *s;
} Connect_Progress_Data;

static Connect_Progress_Data *make_conect_progress_data()
{
  Connect_Progress_Data *pd;
  
  MALLOC_ONE_ATOMIC(Connect_Progress_Data);
  pd->lookup = NULL;
  pd->dest_addr = NULL;
  pd->s = NULL;

  return pd;
}

static void connect_cleanup(Connect_Progress_Data *pd)
{
  if (pd->lookup) {
    rktio_addrinfo_lookup_stop(scheme_rktio, pd->lookup);
    pd->lookup = NULL;
  }
  if (pd->connect) {
    rktio_connect_stop(rktio, pd->connect);
    pd->connect = NULL;
  }
  if (pd->dest_addr) {
    rktio_addrinfo_free(scheme_rktio, pd->dest_addr);
    pd->dest_addr = NULL;
  }
  if (pd->src_addr) {
    rktio_addrinfo_free(scheme_rktio, pd->src_addr);
    pd->src_addr = NULL;
  }
  if (pd->s) {
    (void)scheme_rktio_fd_to_semaphore(s, MZFD_REMOVE, 1);
    rktio_close(s);
    pd->s = NULL;
  }
}

static int check_lookup(Connect_Progress_Data *pd, Scheme_Schedule_Info *sinfo)
{
  if (rktio_poll_addrinfo_lookup_ready(scheme_rktio, pd->lookup))
    return 1;
  
  return 0;
}

static void lookup_needs_wakeup(Connect_Progress_Data *pd, void *fds)
{
  rktio_poll_add_addrinfo_lookup(scheme_rktio, pd->lookup, fds);
}

static void wait_until_lookup(Connect_Progress_Data *pd)
{
  while (!rktio_poll_addrinfo_lookup_ready(scheme_rktio, lookup)) {
    BEGIN_ESCAPEABLE(connect_cleanup, pd);
    scheme_block_until((Scheme_Ready_Fun)check_lookup, 
                       lookup_needs_wakeup, 
                       (void *)pd, 
                       (float)0.0);
    END_ESCAPEABLE();  
  }
}

const char *scheme_hostname_error(int err)
{
  return rktio_get_error_string(scheme_rktio, RKTIO_ERROR_KIND_GAI, err);
}

/*========================================================================*/
/*                         TCP Racket interface                           */
/*========================================================================*/

static void connect_failed(Connect_Progress_Data *pd, const char *why, char *address, int id)
{
  if (pd) connect_cleanup(pd);

  scheme_raise_exn(MZEXN_FAIL_NETWORK,
                   "tcp-connect: connection failed%s%s"
                   "  address: %s\n"
                   "  port number: %d\n"
                   "  system error: %R",
                   why ? ";\n " : "",
                   why ? why : "",
                   address, id);
}

static int tcp_check_connect(Connect_Progress_Data *pd, Scheme_Schedule_Info *sinfo)
{
    
  rktio_fd_t *s = (rktio_fd_t *)connector_p;

  if (!sinfo || !sinfo->is_poll) {
    if (!check_fd_sema(s, MZFD_CHECK_WRITE, sinfo, NULL))
      return 0;
  }

  if (rktio_poll_connect_ready(scheme->rktio, s))
    return 1;

  check_fd_sema(s, MZFD_CREATE_WRITE, sinfo, NULL);

  return 0;
}

static void tcp_connect_needs_wakeup(Scheme_Object *connector_p, void *fds)
{
  rktio_fd_t *s = (rktio_fd_t *)connector_p;
  rktio_poll_add_connect(scheme_rktio, s, fds);
}

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[])
{
  Connect_Progress_Data *pd;
  char *address = "", *src_address, *errmsg = NULL;
  unsigned short id, src_id;
  int no_local_spec;
  Scheme_Object *bs, *src_bs;
  rktio_addrinfo_t *tcp_address_dest, *tcp_address_src;
  rktio_lookup_t *lookup;
  rktio_connect_t *connect;
  rktio_fd_t *s;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("tcp-connect", "string?", 0, argc, argv);
  if (!CHECK_PORT_ID(argv[1]))
    scheme_wrong_contract("tcp-connect", PORT_ID_TYPE, 1, argc, argv);
  if (argc > 2)
    if (!SCHEME_CHAR_STRINGP(argv[2]) && !SCHEME_FALSEP(argv[2]))
      scheme_wrong_contract("tcp-connect", "(or/c string? #f)", 2, argc, argv);
  if (argc > 3)
    if (SCHEME_TRUEP(argv[3]) && !CHECK_PORT_ID(argv[3]))
      scheme_wrong_contract("tcp-connect", "(or/c " PORT_ID_TYPE " #f)", 3, argc, argv);

  bs = argv[0];
  if (SCHEME_CHAR_STRINGP(bs))
    bs = scheme_char_string_to_byte_string(bs);

  address = SCHEME_BYTE_STR_VAL(bs);
  id = (unsigned short)SCHEME_INT_VAL(argv[1]);

  if ((argc > 2) && SCHEME_TRUEP(argv[2])) {
    src_bs = scheme_char_string_to_byte_string(argv[2]);
    src_address = SCHEME_BYTE_STR_VAL(src_bs);
  } else
    src_address = NULL;
   
  if ((argc > 3) && SCHEME_TRUEP(argv[3])) {
    no_local_spec = 0;
    src_id = (unsigned short)SCHEME_INT_VAL(argv[3]);
  } else {
    no_local_spec = 1;
    src_id = 0;
    if (src_address) {
      scheme_contract_error("tcp-connect",
                            "no local port number supplied when local hostname was supplied",
                            "hostname", 1, argv[2],
                            NULL);
    }
  }

  scheme_security_check_network("tcp-connect", address, id, 1);
  scheme_custodian_check_available(NULL, "tcp-connect", "network");

  pd = make_connect_progress_data();
  
  lookup = rktio_start_addrinfo_lookup(scheme_rktio, address, id, RKTIO_FAMILTY_ANY, 0, 1);
  if (!lookup)
    connect_failed(pd, "host not found", address, id);
  
  pd->lookup = lookup;
  wait_until_lookup(pd);
  pd->lookup = NULL;

  tcp_connect_dest = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
  if (!tcp_connect_dest)
    connect_failed(pd, "host not found", address, id);
  
  pd->dest_addr = tcp_connect_dest;
  
  if (!no_local_spec) {
    lookup = rktio_start_addrinfo_lookup(scheme_rktio, src_address, src_id, RKTIO_FAMILTY_ANY, 1, 1);
    if (!lookup)
      connect_failed(pd, "local host not found", src_address, src_id);

    pd->lookup = lookup;
    wait_until_lookup(pd);
    pd->lookup = NULL;
    
    tcp_connect_dest = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
    if (!tcp_connect_dest)
      connect_failed(pd, "local host not found", src_address, src_id);

    pd->lookup = lookup;
    wait_until_lookup(pd);
    pd->lookup = NULL;

    tcp_connect_src = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
    if (!tcp_connect_src)
      connect_failed(pd, "local host not found", src_address, src_id);
  }

  pd->src_addr = tcp_connect_src;

  connect = rktio_start_connect(racket_rktio, tcp_connect_dest, tcp_connect_src);
  if (!connect)
    connect_failed(pd, NULL, address, id);

  pd->connect = connect;

  while (1) {
    while (!rktio_poll_connect_ready(scheme_rktio, connect)) {
      BEGIN_ESCAPEABLE(connect_cleanup, pd);
      scheme_block_until((Scheme_Ready_Fun)tcp_check_connect, 
                         tcp_connect_needs_wakeup, 
                         (void *)pd, 
                         (float)0.0);
      END_ESCAPEABLE();  
    }

    s = rktio_connect_finish(rktio_rktio, connect);
    
    if (!s && scheme_last_error_is_racket(RKTIO_ERROR_CONNECT_TRYING_NEXT)) {
      /* try again */
    } else
      break;
  }

  pd->connect = NULL;

  if (!s)
    connect_failed(s, NULL, address, id);    

  connect_cleanup(pd);

  {
    Scheme_Object *v[2];
    Scheme_Tcp *tcp;
    
    if (tcp_connect_src)
      rktio_addrinfo_free(tcp_connect_src);
    
    tcp = make_tcp_port_data(s, 2);
    
    v[0] = make_tcp_input_port(tcp, address, NULL);
    v[1] = make_tcp_output_port(tcp, address, NULL);
    
    return scheme_values(2, v);
  }
}

static Scheme_Object *
tcp_connect_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(tcp_connect, argc, argv);
}

static void listen_failed(Connect_Progress_Data *pd, const char *why, char *address, int id)
{
  if (pd) connect_cleanup(pd);

  scheme_raise_exn(MZEXN_FAIL_NETWORK,
                   "tcp-listen: listen failed%s%s"
                   "%s%s%s"
                   "  port number: %d\n"
                   "  system error: %R",
                   why ? ";\n " : "",
                   why ? why : "",
                   address ? "  address: " : "",
                   address ? address : "",
                   address? "%s\n" : "",
                   address, id);
}

static Scheme_Object *
tcp_listen(int argc, Scheme_Object *argv[])
{
  unsigned short id;
  int backlog, errid;
  int reuse = 0, no_ipv6 = 0;
  const char *address;
  Connect_Progress_Data *pd;
  rktio_lookup_t *lookup;
  rktio_addrinfo_t *tcp_src;
  rktio_listener_t *lnr;
  listener_t *l;
  
  if (!CHECK_LISTEN_PORT_ID(argv[0]))
    scheme_wrong_contract("tcp-listen", LISTEN_PORT_ID_TYPE, 0, argc, argv);
  if (argc > 1) {
    if (!((SCHEME_INTP(argv[1]) && (SCHEME_INT_VAL(argv[1]) >= 1))
          || (SCHEME_BIGNUMP(argv[1]) && SCHEME_BIGPOS(argv[1]))))
      scheme_wrong_contract("tcp-listen", "exact-positive-integer?", 1, argc, argv);
  }
  if (argc > 2)
    reuse = SCHEME_TRUEP(argv[2]);
  if (argc > 3) {
    if (!SCHEME_CHAR_STRINGP(argv[3]) && !SCHEME_FALSEP(argv[3]))
      scheme_wrong_contract("tcp-listen", "(or/c string? #f)", 3, argc, argv);
  }
    
  id = (unsigned short)SCHEME_INT_VAL(argv[0]);
  if (argc > 1) {
    if (SCHEME_INTP(argv[1]))
      backlog = SCHEME_INT_VAL(argv[1]);
    else 
      backlog = 1024;
  } else
    backlog = 4;
  if ((argc > 3) && SCHEME_TRUEP(argv[3])) {
    Scheme_Object *bs;
    bs = scheme_char_string_to_byte_string(argv[3]);
    address = SCHEME_BYTE_STR_VAL(bs);
  } else
    address = NULL;

  scheme_security_check_network("tcp-listen", address, origid, 0);
  scheme_custodian_check_available(NULL, "tcp-listen", "network");

  pd = make_connect_progress_data();    

  while (1) {
    int family;

    if (no_ipv6)
      family = rktio_get_ipv4_family(scheme_rktio);
    else
      family = RKTIO_FAMILTY_ANY;
    
    lookup = rktio_start_addrinfo_lookup(scheme_rktio, address, id, family, 1, 1);
    if (!lookup)
      listen_failed(pd, "address-resolution error", address, id);

    pd->lookup = lookup;
    wait_until_lookup(pd);
    pd->lookup = NULL;

    tcp_src = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
    if (!tcp_src)
      connect_failed(pd, "address-resolution error", address, id);

    pd->src_addr = tcp_src;
    lnr = rktio_listen(rktio, tcp_src, backlog, reuse);

    pd->src_addr = NULL;
    rktio_addrinfo_free(scheme_rktio, tcp_src);
    
    if (!lnr) {
      if (scheme_last_error_is_racket(RKTIO_ERROR_TRY_AGAIN_WITH_IPV4)) {
        /* try again */
        no_ipv6 = 1;
      } else
        break;
    } else
      break;
  }

  if (!lnr)
    listen_failed(pd, NULL, address, id);

  l = MALLOC_ONE_TAGGED(listener_t);
  l->so.type = scheme_listener_type;
  l->lnr = lnr;
  {
    Scheme_Custodian_Reference *mref;
    mref = scheme_add_managed(NULL,
                              (Scheme_Object *)l,
                              (Scheme_Close_Custodian_Client *)stop_listener,
                              NULL,
                              1);
    l->mref = mref;
  }
  
  return (Scheme_Object *)l;
}

static Scheme_Object *
tcp_stop(int argc, Scheme_Object *argv[])
{
  int was_closed;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_contract("tcp-close", "tcp-listener?", 0, argc, argv);

  TCP_INIT("tcp-close");

  was_closed = stop_listener(argv[0]);

  if (was_closed) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-close: listener was already closed");
    return NULL;
  }

  return scheme_void;
}

static Scheme_Object *
tcp_accept_ready(int argc, Scheme_Object *argv[])
{
  int ready;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_contract("tcp-accept-ready?", "tcp-listener?", 0, argc, argv);

  TCP_INIT("tcp-accept-ready?");

  if (LISTENER_WAS_CLOSED(argv[0])) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "tcp-accept-ready?: listener is closed");
    return NULL;
  }

  ready = tcp_check_accept(argv[0], NULL);

  return (ready ? scheme_true : scheme_false);
}

static Scheme_Object *
do_tcp_accept(int argc, Scheme_Object *argv[], Scheme_Object *cust, char **_fail_reason)
/* If _fail_reason is not NULL, never raise an exception. */
{
#ifdef USE_TCP
  int was_closed = 0, errid, ready_pos;
  Scheme_Object *listener;
  rktio_fd_t *s;
  unsigned int l;
  GC_CAN_IGNORE char tcp_accept_addr[MZ_SOCK_NAME_MAX_LEN];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_contract("tcp-accept", "tcp-listener?", 0, argc, argv);

  listener = argv[0];

  was_closed = LISTENER_WAS_CLOSED(listener);

  if (!was_closed) {
    ready_pos = tcp_check_accept(listener, NULL);
    if (!ready_pos) {
      Scheme_Object *evt;
      evt = listener_to_evt((listener_t *)listener);
      if (evt)
        scheme_sync(1, &evt);
      else
        scheme_block_until((Scheme_Ready_Fun)tcp_check_accept, 
                           tcp_accept_needs_wakeup, 
                           listener, 
                           0.0);
      ready_pos = tcp_check_accept(listener, NULL);
    }
    was_closed = LISTENER_WAS_CLOSED(listener);
  } else
    ready_pos = 0;

  if (was_closed) {
    if (_fail_reason)
      *_fail_reason = "tcp-accept-evt: listener is closed";
    else
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "tcp-accept: listener is closed");
    return NULL;
  }

  if (!_fail_reason)
    scheme_custodian_check_available((Scheme_Custodian *)cust, "tcp-accept", "network");
  else {
    if (!scheme_custodian_is_available((Scheme_Custodian *)cust)) {
      *_fail_reason = "tcp-accept-evt: custodian is shutdown";
      return NULL;
    }
  }

  s = rktio_accept(scheme_rktio, ((listener_t *)listener)->lnr);

  if (s) {
    Scheme_Object *v[2];
    Scheme_Tcp *tcp;
    
    tcp = make_tcp_port_data(s, 2);

    v[0] = make_tcp_input_port(tcp, "tcp-accepted", cust);
    v[1] = make_tcp_output_port(tcp, "tcp-accepted", cust);

    return scheme_values(2, v);
  }
  
  if (_fail_reason)
    *_fail_reason = "tcp-accept-evt: accept from listener failed";
  else
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "tcp-accept: accept from listener failed\n"
                     "  system error: %R");

  return NULL;
}

static Scheme_Object *
tcp_accept(int argc, Scheme_Object *argv[])
{
  return do_tcp_accept(argc, argv, NULL, NULL);
}

static Scheme_Object *
tcp_accept_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(tcp_accept, argc, argv);
}

void scheme_register_network_evts()
{
#ifdef USE_TCP
  scheme_add_evt(scheme_listener_type, (Scheme_Ready_Fun)tcp_check_accept, tcp_accept_needs_wakeup, NULL, 0);
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
  rktio_fd_t *socket = NULL;
  rktio_listener_t *lnr = NULL;
  Scheme_Tcp *tcp = NULL;
  int closed = 0, require_peer = 0;
  Scheme_Object *result[4];
  int with_ports = 0;

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
    if (closed)
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "tcp-addresses: port is closed");
    require_peer = 1;
  } else {
    if (SCHEME_LISTEN_PORTP(argv[0])) {
      listener = 1;
      if (LISTENER_WAS_CLOSED(argv[0])) {
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "tcp-addresses: listener is closed");
      } else
        lnr = ((listener_t *)argv[0])->lnr;
    } else if (SCHEME_UDP_PORTP(argv[0])) {
      udp = 1;
      socket = ((Scheme_UDP *)argv[0])->s;
      if (!socket)
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "tcp-addresses: socket is closed");
    } else {
      scheme_wrong_contract("tcp-addresses", "(or/c tcp-port? listener? udp-socket?)", 0, argc, argv);
    }
  }

  if (socket)
    local_names = rktio_socket_address(socket);
  else
    local_names = rktio_listener_address(lnr);

  if (!local_names)
    scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                     "tcp-addresses: could not get address\n"
                     "  system error: %R");

  if (socket)
    peer_names = rktio_socket_address(socket);
  else
    peer_names = NULL;

  if (!peer_names && require_peer) {
    free(local_names[0]);
    free(local_names[1]);
    free(local_names);
    scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                     "tcp-addresses: could not get peer address\n"
                     "  system error: %R");
  }

  result[0] = scheme_make_utf8_string(local_names[0]);
  if (with_ports) {
    l = extract_svc_value(local_names[1]);
    result[1] = scheme_make_integer(l);
  }

  if (!peer_names) {
    result[with_ports ? 2 : 1] = scheme_make_utf8_string("0.0.0.0");
    result[3] = scheme_make_integer(0);
  } else {
    result[with_ports ? 2 : 1] = scheme_make_utf8_string(host_buf);
    if (with_ports) {
      l = extract_svc_value(svc_buf);
      result[3] = scheme_make_integer(l);
    }
  }

  free(local_names[0]);
  free(local_names[1]);
  free(local_names);
  if (peer_names) {
    free(peer_names[0]);
    free(peer_names[1]);
    free(perr_names);
  }
  
  return scheme_values(with_ports ? 4 : 2, result);
}

static Scheme_Object *tcp_abandon_port(int argc, Scheme_Object *argv[])
{
  if (SCHEME_OUTPUT_PORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(argv[0]);
    if (op->sub_type == scheme_tcp_output_port_type) {
      if (!op->closed) {
	((Scheme_Tcp *)op->port_data)->flags |= MZ_TCP_ABANDON_OUTPUT;
	scheme_close_output_port(argv[0]);
      }
      return scheme_void;
    }
  } else if (SCHEME_INPUT_PORTP(argv[0])) {
    /* Abandon is not really useful on input ports from the Racketeer's
       perspective, but it's here for completeness. */
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(argv[0]);
    if (ip->sub_type == scheme_tcp_input_port_type) {
      if (!ip->closed) {
	((Scheme_Tcp *)ip->port_data)->flags |= MZ_TCP_ABANDON_INPUT;
	scheme_close_input_port(argv[0]);
      }
      return scheme_void;
    }
  }

  scheme_wrong_contract("tcp-abandon-port", "tcp-port?", 0, argc, argv);

  return NULL;
}

void scheme_tcp_abandon_port(Scheme_Object *port) {
  tcp_abandon_port(1, &port);
}

static Scheme_Object *tcp_port_p(int argc, Scheme_Object *argv[])
{
  if (SCHEME_OUTPUT_PORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(argv[0]);
    if (op->sub_type == scheme_tcp_output_port_type) {
      return scheme_true;
    }
  } else if (SCHEME_INPUT_PORTP(argv[0])) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(argv[0]);
    if (ip->sub_type == scheme_tcp_input_port_type) {
      return scheme_true;
    }
  }

  return scheme_false;
}


static Scheme_Object *tcp_accept_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *r, *custodian;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_contract("tcp-accept-evt", "tcp-listener?", 0, argc, argv);

  custodian = scheme_get_param(scheme_current_config(), MZCONFIG_CUSTODIAN);

  scheme_custodian_check_available((Scheme_Custodian *)custodian, "tcp-accept", "network");
  
  r = scheme_alloc_object();
  r->type = scheme_tcp_accept_evt_type;
  SCHEME_PTR1_VAL(r) = argv[0];
  SCHEME_PTR2_VAL(r) = custodian;

  return r;
}

static Scheme_Object *accept_failed(void *_msg, int argc, Scheme_Object **argv)
{
  scheme_raise_exn(MZEXN_FAIL_NETWORK, (char *)_msg);
  return NULL;
} 

static int tcp_check_accept_evt(Scheme_Object *ae, Scheme_Schedule_Info *sinfo)
{
  if (tcp_check_accept(SCHEME_PTR1_VAL(ae), NULL)) {
    Scheme_Object *a[2];
    char *fail_reason = NULL;
    a[0] = SCHEME_PTR1_VAL(ae);
    if (do_tcp_accept(1, a, SCHEME_PTR2_VAL(ae), &fail_reason)) {
      a[0] = scheme_current_thread->ku.multiple.array[0];
      a[1] = scheme_current_thread->ku.multiple.array[1];
      scheme_set_sync_target(sinfo, scheme_build_list(2, a), NULL, NULL, 0, 0, NULL);
      return 1;
    } else {
      /* error on accept */
      scheme_set_sync_target(sinfo, scheme_always_ready_evt, 
                             scheme_make_closed_prim(accept_failed, fail_reason),
                             NULL, 0, 0, NULL);
      return 1;
    }
  } else
    return 0;
}

static void tcp_accept_evt_needs_wakeup(Scheme_Object *ae, void *fds)
{
  tcp_accept_needs_wakeup(SCHEME_PTR1_VAL(ae), fds);
}

int scheme_get_port_socket(Scheme_Object *p, intptr_t *_s)
{
  rktio_fd_t *s = NULL;
  int s_ok = 0;

  if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(p);
    if (op->sub_type == scheme_tcp_output_port_type) {
      if (!op->closed) {
	s = ((Scheme_Tcp *)op->port_data)->tcp;
	s_ok = 1;
      }
    }
  } else if (SCHEME_INPUT_PORTP(p)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(p);
    if (ip->sub_type == scheme_tcp_input_port_type) {
      if (!ip->closed) {
	s = ((Scheme_Tcp *)ip->port_data)->tcp;
	s_ok = 1;
      }
    }
  }

  if (s_ok) {
    intptr_t sv;
    sv = rktio_fd_system_fd(s);
    *_s = sv;
    return 1;
  } else
    return 0;
}

void scheme_socket_to_ports(intptr_t s, const char *name, int takeover,
                            Scheme_Object **_inp, Scheme_Object **_outp)
{
  Scheme_Tcp *tcp;
  Scheme_Object *v;
  rktio_fd_t *rfd;

  rfd = rktio_open(s, RKTIO_OPEN_READ | RKTIO_OPEN_WRITE | RKTIO_OPEN_SOCKET | RKTIO_OPEN_OWN);

  tcp = make_tcp_port_data(rfd, takeover ? 2 : 3);

  v = make_tcp_input_port(tcp, name, NULL);
  *_inp = v;
  v = make_tcp_output_port(tcp, name, NULL);
  *_outp = v;
}

void scheme_socket_to_input_port(intptr_t s, Scheme_Object *name, int takeover,
                                 Scheme_Object **_inp)
{
#ifdef USE_TCP
  Scheme_Tcp *tcp;
  Scheme_Object *v;

  tcp = make_tcp_port_data(s, takeover ? 1 : 2);

  v = make_tcp_input_port_symbol_name(tcp, name, NULL);
  *_inp = v;
  
  if (takeover) {
    REGISTER_SOCKET(s);
  }
#else
  *_inp = NULL;
#endif
}

void scheme_socket_to_output_port(intptr_t s, Scheme_Object *name, int takeover,
                                  Scheme_Object **_outp)
{
#ifdef USE_TCP
  Scheme_Tcp *tcp;
  Scheme_Object *v;

  tcp = make_tcp_port_data(s, takeover ? 1 : 2);

  v = make_tcp_output_port_symbol_name(tcp, name, NULL);
  *_outp = v;
  
  if (takeover) {
    REGISTER_SOCKET(s);
  }
#else
  *_outp = NULL;
#endif
}

intptr_t scheme_dup_socket(intptr_t fd) {
#ifdef USE_TCP
# ifdef USE_WINSOCK_TCP
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

void scheme_close_socket_fd(intptr_t fd) 
{
#ifdef USE_TCP
  UNREGISTER_SOCKET(fd);
  closesocket(fd);
  (void)scheme_fd_to_semaphore(fd, MZFD_REMOVE, 1);
#endif
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
  tcp_t s;
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
    GC_CAN_IGNORE struct mz_addrinfo *udp_bind_addr = NULL;
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
    s = socket(MZ_PF_INET, SOCK_DGRAM, 0);
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
# define OK_DISCONNECT_ERROR(e) (((e) == mz_AFNOSUPPORT) || ((e) == EADDRNOTAVAIL))
#else
# define OK_DISCONNECT_ERROR(e) ((e) == mz_AFNOSUPPORT)
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
    GC_CAN_IGNORE struct mz_addrinfo *udp_bind_addr = NULL, *addr;

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
        GC_CAN_IGNORE mz_unspec_address ua;
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
    DECL_OS_FDSET(writefds);
    DECL_OS_FDSET(exnfds);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_OS_WR_FDSET(writefds);
    INIT_DECL_OS_ER_FDSET(exnfds);
    
    MZ_OS_FD_ZERO(writefds);
    MZ_OS_FD_SET(udp->s, writefds);
    MZ_OS_FD_ZERO(exnfds);
    MZ_OS_FD_SET(udp->s, exnfds);
    
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
  tcp_t s = udp->s;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);
  
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
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
  GC_CAN_IGNORE struct mz_addrinfo *udp_dest_addr;

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
        GC_CAN_IGNORE struct mz_addrinfo *addr;
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
        GC_CAN_IGNORE struct mz_addrinfo *addr;
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
    DECL_OS_FDSET(readfds);
    DECL_OS_FDSET(exnfds);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_OS_RD_FDSET(readfds);
    INIT_DECL_OS_ER_FDSET(exnfds);
    
    MZ_OS_FD_ZERO(readfds);
    MZ_OS_FD_SET(udp->s, readfds);
    MZ_OS_FD_ZERO(exnfds);
    MZ_OS_FD_SET(udp->s, exnfds);
    
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

  tcp_t s = udp->s;
  
  fds1 = MZ_GET_FDSET(fds, 0);
  fds2 = MZ_GET_FDSET(fds, 2);
  
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
}

#endif

static int do_udp_recv(const char *name, Scheme_UDP *udp, char *bstr, intptr_t start, intptr_t end, 
		       int can_block, Scheme_Object **v)
{
#ifdef UDP_IS_SUPPORTED
  intptr_t x;
  int errid = 0;
  char src_addr[MZ_SOCK_NAME_MAX_LEN];
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
    char host_buf[MZ_SOCK_HOST_NAME_MAX_LEN];
    char prev_buf[MZ_SOCK_HOST_NAME_MAX_LEN];
    char svc_buf[MZ_SOCK_SVC_NAME_MAX_LEN];
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
      char host_buf[MZ_SOCK_HOST_NAME_MAX_LEN];
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
      GC_CAN_IGNORE struct mz_addrinfo *if_addr = NULL;
      int err;
      bs = scheme_char_string_to_byte_string(argv[1]);
      address = SCHEME_BYTE_STR_VAL(bs);
      if_addr = scheme_get_host_address(address, -1, &err, MZ_PF_INET, 0, 0);
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
    GC_CAN_IGNORE struct mz_addrinfo *if_addr = NULL;
    int err;
    bs = scheme_char_string_to_byte_string(ifaddrname);
    address = SCHEME_BYTE_STR_VAL(bs);
    if_addr = scheme_get_host_address(address, -1, &err, MZ_PF_INET, 0, 0);
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
    GC_CAN_IGNORE struct mz_addrinfo *group_addr = NULL;
    int err;
    bs = scheme_char_string_to_byte_string(multiaddrname);
    address = SCHEME_BYTE_STR_VAL(bs);
    group_addr = scheme_get_host_address(address, -1, &err, MZ_PF_INET, 0, 0);
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

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_network.inc"

static void register_traversers(void)
{
#ifdef USE_TCP
  GC_REG_TRAV(scheme_listener_type, mark_listener);
  GC_REG_TRAV(scheme_rt_tcp, mark_tcp);
# ifdef UDP_IS_SUPPORTED
  GC_REG_TRAV(scheme_udp_type, mark_udp);
  GC_REG_TRAV(scheme_udp_evt_type, mark_udp_evt);
# endif
#endif
}

END_XFORM_SKIP;

#endif

#endif /* !NO_TCP_SUPPORT */
