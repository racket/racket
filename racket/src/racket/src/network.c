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

#define TCP_BUFFER_SIZE 4096

typedef struct {
  Scheme_Object so;
  Scheme_Custodian_Reference *mref;
  rktio_listener_t *lnr;
} listener_t;

typedef struct Scheme_Tcp_Buf {
  MZTAG_IF_REQUIRED
  short refcount;
  short forget_on_close;
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

static int check_fd_sema(rktio_fd_t *s, int mode, Scheme_Schedule_Info *sinfo, Scheme_Object *orig)
/* Tries to convert a file descriptor to a semaphore, and redirects
   the sync to the semaphore if that works, which avoids a poll spin
   on the file descriptor. The result is 0 if the shortcut determines
   that the file descriptor is not ready. */
{
  Scheme_Object *sema;

  sema = scheme_rktio_fd_to_semaphore(s, mode);
  
  if (sema) {
    /* It would make sense to force a poll via
       scheme_check_fd_semaphores() here, although we'd only want to
       to that once per scheduler cycle. That would more reliably poll
       at the OS level, since we otherwise wait on the scheduler to
       check semaphores. It's not clear that the OS supports more
       precise reasoning about readiness, though, and Racket has
       traditonally not done that, so we're still skipping
       it. */
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

/*========================================================================*/
/*                       TCP ports and listeners                          */
/*========================================================================*/

#define LISTENER_WAS_CLOSED(x) (!((listener_t *)(x))->lnr)

static int tcp_check_accept(Scheme_Object *_listener, Scheme_Schedule_Info *sinfo)
{
  listener_t *listener = (listener_t *)_listener;

  if (!sinfo || !sinfo->is_poll) {
    /* If listeners supported semaphores, we could have a check here
       along the lines of check_fd_sema(). See CREATE_LISTEN_SEMA in
       two places below. */
  }

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  if (rktio_poll_accept_ready(scheme_rktio, listener->lnr))
    return 1;

  /* CREATE_LISTEN_SEMA: This is where we'd create a semaphore for the
     listener, if that were possible */

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
  rktio_fd_t *s = ((Scheme_Tcp *)((Scheme_Output_Port *)port)->port_data)->tcp;
  
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
      rn = rktio_read(scheme_rktio, data->tcp, data->b.buffer, read_amt);
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
      sema = scheme_rktio_fd_to_semaphore(data->tcp, MZFD_CREATE_READ);
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
                     "  system error: %R");
    return 0;
  } else if (data->b.bufmax == RKTIO_READ_EOF) {
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

  (void)scheme_rktio_fd_to_semaphore(data->tcp, MZFD_REMOVE);

  if (data->b.forget_on_close)
    rktio_forget(scheme_rktio, data->tcp);
  else
    rktio_close(scheme_rktio, data->tcp);
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
      sema = scheme_rktio_fd_to_semaphore(data->tcp, MZFD_CREATE_WRITE);
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

  (void)scheme_rktio_fd_to_semaphore(data->tcp, MZFD_REMOVE);

  if (data->b.forget_on_close)
    rktio_forget(scheme_rktio, data->tcp);
  else
    rktio_close(scheme_rktio, data->tcp);
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
  rktio_addrinfo_lookup_t *lookup;
  rktio_connect_t *connect;
  rktio_addrinfo_t *dest_addr;
  rktio_addrinfo_t *src_addr;
  rktio_fd_t *trying_s;
  rktio_fd_t *s;
} Connect_Progress_Data;

static Connect_Progress_Data *make_connect_progress_data()
{
  Connect_Progress_Data *pd;
  
  pd = MALLOC_ONE_ATOMIC(Connect_Progress_Data);
  pd->lookup = NULL;
  pd->connect = NULL;
  pd->dest_addr = NULL;
  pd->src_addr = NULL;
  pd->trying_s = NULL;
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
    rktio_connect_stop(scheme_rktio, pd->connect);
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
  if (pd->trying_s) {
    (void)scheme_rktio_fd_to_semaphore(pd->trying_s, MZFD_REMOVE);
    pd->trying_s = NULL;
  }
  if (pd->s) {
    (void)scheme_rktio_fd_to_semaphore(pd->s, MZFD_REMOVE);
    rktio_close(scheme_rktio, pd->s);
    pd->s = NULL;
  }
}

static int check_lookup(Connect_Progress_Data *pd, Scheme_Schedule_Info *sinfo)
{
  if (rktio_poll_addrinfo_lookup_ready(scheme_rktio, pd->lookup))
    return 1;
  
  return 0;
}

static void lookup_needs_wakeup(Scheme_Object *_pd, void *fds)
{
  Connect_Progress_Data *pd = (Connect_Progress_Data *)_pd;
  rktio_poll_add_addrinfo_lookup(scheme_rktio, pd->lookup, fds);
}

static void wait_until_lookup(Connect_Progress_Data *pd)
{
  while (!rktio_poll_addrinfo_lookup_ready(scheme_rktio, pd->lookup)) {
    BEGIN_ESCAPEABLE(connect_cleanup, pd);
    scheme_block_until((Scheme_Ready_Fun)check_lookup, 
                       lookup_needs_wakeup, 
                       (void *)pd, 
                       (float)0.0);
    END_ESCAPEABLE();  
  }
}

static rktio_addrinfo_t *do_resolve_address(const char *who, char *address, int id, int family, int passive, int show_id_on_error)
{
  Connect_Progress_Data *pd;
  rktio_addrinfo_lookup_t *lookup;
  rktio_addrinfo_t *addr;
    
  pd = make_connect_progress_data();    
    
  lookup = rktio_start_addrinfo_lookup(scheme_rktio, address, id, family, passive, 0);
  if (!lookup) {
    addr = NULL;
  } else {
    pd->lookup = lookup;
    wait_until_lookup(pd);
    pd->lookup = NULL;
      
    addr = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
  }

  if (!addr) {
    if (show_id_on_error) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "%s: can't resolve address\n"
                       "  address: %s\n"
                       "  port number: %d\n"
                       "  system error: %R",
                       who,
                       address ? address : "<unspec>",
                       id);
    } else {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "%s: can't resolve address\n"
                       "  address: %s\n"
                       "  system error: %R",
                       who,
                       address ? address : "<unspec>");
    }
    
    return NULL;
  }

  return addr;
}

const char *scheme_hostname_error(int err)
{
  return rktio_get_error_string(scheme_rktio, RKTIO_ERROR_KIND_GAI, err);
}


/*========================================================================*/
/*                         TCP Racket interface                           */
/*========================================================================*/

static void connect_failed(Connect_Progress_Data *pd, const char *why, const char *address, int id)
{
  if (pd) connect_cleanup(pd);

  scheme_raise_exn(MZEXN_FAIL_NETWORK,
                   "tcp-connect: connection failed%s%s\n"
                   "  address: %s\n"
                   "  port number: %d\n"
                   "  system error: %R",
                   why ? ";\n " : "",
                   why ? why : "",
                   address, id);
}

static int tcp_check_connect(Connect_Progress_Data *pd, Scheme_Schedule_Info *sinfo)
{
  if (!pd->trying_s) {
    rktio_fd_t *s;
    s = rktio_connect_trying(scheme_rktio, pd->connect);
    pd->trying_s = s;
  }
  
  if (!sinfo || !sinfo->is_poll) {
    if (pd->trying_s)
      if (!check_fd_sema(pd->trying_s, MZFD_CHECK_WRITE, sinfo, NULL))
        return 0;
  }

  if (rktio_poll_connect_ready(scheme_rktio, pd->connect))
    return 1;

  if (pd->trying_s)
    check_fd_sema(pd->trying_s, MZFD_CREATE_WRITE, sinfo, NULL);

  return 0;
}

static void tcp_connect_needs_wakeup(Scheme_Object *_pd, void *fds)
{
  Connect_Progress_Data *pd = (Connect_Progress_Data *)_pd;
  rktio_poll_add_connect(scheme_rktio, pd->connect, fds);
}

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[])
{
  Connect_Progress_Data *pd;
  char *address = "", *src_address;
  unsigned short id, src_id;
  int no_local_spec;
  Scheme_Object *bs, *src_bs;
  rktio_addrinfo_t *tcp_connect_dest, *tcp_connect_src;
  rktio_addrinfo_lookup_t *lookup;
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
  
  lookup = rktio_start_addrinfo_lookup(scheme_rktio, address, id, RKTIO_FAMILY_ANY, 0, 1);
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
    lookup = rktio_start_addrinfo_lookup(scheme_rktio, src_address, src_id, RKTIO_FAMILY_ANY, 1, 1);
    if (!lookup)
      connect_failed(pd, "local host not found", src_address, src_id);

    pd->lookup = lookup;
    wait_until_lookup(pd);
    pd->lookup = NULL;
    
    tcp_connect_src = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
    if (!tcp_connect_src)
      connect_failed(pd, "local host not found", src_address, src_id);
  } else
    tcp_connect_src = NULL;

  pd->src_addr = tcp_connect_src;

  connect = rktio_start_connect(scheme_rktio, tcp_connect_dest, tcp_connect_src);
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

    if (pd->trying_s)
      (void)scheme_rktio_fd_to_semaphore(pd->trying_s, MZFD_REMOVE);

    s = rktio_connect_finish(scheme_rktio, connect);
    
    if (!s && scheme_last_error_is_racket(RKTIO_ERROR_CONNECT_TRYING_NEXT)) {
      /* try again */
    } else
      break;
  }

  pd->connect = NULL;

  if (!s)
    connect_failed(pd, NULL, address, id);    

  connect_cleanup(pd);

  {
    Scheme_Object *v[2];
    Scheme_Tcp *tcp;
    
    if (tcp_connect_src)
      rktio_addrinfo_free(scheme_rktio, tcp_connect_src);
    
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

static void listen_failed(Connect_Progress_Data *pd, const char *why, const char *address, int id)
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
  int backlog;
  int reuse = 0, no_ipv6 = 0;
  const char *address;
  Connect_Progress_Data *pd;
  rktio_addrinfo_lookup_t *lookup;
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

  scheme_security_check_network("tcp-listen", address, id, 0);
  scheme_custodian_check_available(NULL, "tcp-listen", "network");

  pd = make_connect_progress_data();    

  while (1) {
    int family;

    if (no_ipv6)
      family = rktio_get_ipv4_family(scheme_rktio);
    else
      family = RKTIO_FAMILY_ANY;
    
    lookup = rktio_start_addrinfo_lookup(scheme_rktio, address, id, family, 1, 1);
    if (!lookup)
      listen_failed(pd, "address-resolution error", address, id);

    pd->lookup = lookup;
    wait_until_lookup(pd);
    pd->lookup = NULL;

    tcp_src = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
    if (!tcp_src)
      listen_failed(pd, "address-resolution error", address, id);

    pd->src_addr = tcp_src;
    lnr = rktio_listen(scheme_rktio, tcp_src, backlog, reuse);

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
  int was_closed = 0, ready_pos;
  Scheme_Object *listener;
  rktio_fd_t *s;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_contract("tcp-accept", "tcp-listener?", 0, argc, argv);

  listener = argv[0];

  was_closed = LISTENER_WAS_CLOSED(listener);

  if (!was_closed) {
    ready_pos = tcp_check_accept(listener, NULL);
    if (!ready_pos) {
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
  scheme_add_evt(scheme_listener_type, (Scheme_Ready_Fun)tcp_check_accept, tcp_accept_needs_wakeup, NULL, 0);
  scheme_add_evt(scheme_tcp_accept_evt_type, (Scheme_Ready_Fun)tcp_check_accept_evt, tcp_accept_evt_needs_wakeup, NULL, 0);
  scheme_add_evt(scheme_udp_evt_type, (Scheme_Ready_Fun)udp_evt_check_ready, udp_evt_needs_wakeup, NULL, 0);
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
  rktio_fd_t *socket = NULL;
  rktio_listener_t *lnr = NULL;
  Scheme_Tcp *tcp = NULL;
  int closed = 0, require_peer = 0;
  Scheme_Object *result[4];
  int with_ports = 0;
  intptr_t l;
  char **local_names, **peer_names;

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
      if (LISTENER_WAS_CLOSED(argv[0])) {
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "tcp-addresses: listener is closed");
      } else
        lnr = ((listener_t *)argv[0])->lnr;
    } else if (SCHEME_UDP_PORTP(argv[0])) {
      socket = ((Scheme_UDP *)argv[0])->s;
      if (!socket)
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "tcp-addresses: socket is closed");
    } else {
      scheme_wrong_contract("tcp-addresses", "(or/c tcp-port? listener? udp-socket?)", 0, argc, argv);
    }
  }

  if (socket)
    local_names = rktio_socket_address(scheme_rktio, socket);
  else
    local_names = rktio_listener_address(scheme_rktio, lnr);

  if (!local_names)
    scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                     "tcp-addresses: could not get address\n"
                     "  system error: %R");

  if (socket)
    peer_names = rktio_socket_address(scheme_rktio, socket);
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
    result[with_ports ? 2 : 1] = scheme_make_utf8_string(peer_names[0]);
    if (with_ports) {
      l = extract_svc_value(peer_names[1]);
      result[3] = scheme_make_integer(l);
    }
  }

  free(local_names[0]);
  free(local_names[1]);
  free(local_names);
  if (peer_names) {
    free(peer_names[0]);
    free(peer_names[1]);
    free(peer_names);
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
    sv = rktio_fd_system_fd(scheme_rktio, s);
    *_s = sv;
    return 1;
  } else
    return 0;
}

rktio_fd_t *scheme_get_port_rktio_socket(Scheme_Object *p)
{
  if (SCHEME_OUTPUT_PORTP(p)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(p);
    if (op->sub_type == scheme_tcp_output_port_type) {
      if (!op->closed) {
	return ((Scheme_Tcp *)op->port_data)->tcp;
      }
    }
  } else if (SCHEME_INPUT_PORTP(p)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(p);
    if (ip->sub_type == scheme_tcp_input_port_type) {
      if (!ip->closed) {
	return ((Scheme_Tcp *)ip->port_data)->tcp;
      }
    }
  }

  return NULL;
}

void scheme_socket_to_ports(intptr_t s, const char *name, int takeover,
                            Scheme_Object **_inp, Scheme_Object **_outp)
{
  Scheme_Tcp *tcp;
  Scheme_Object *v;
  rktio_fd_t *rfd;

  rfd = rktio_system_fd(scheme_rktio, s, RKTIO_OPEN_READ | RKTIO_OPEN_WRITE | RKTIO_OPEN_SOCKET | RKTIO_OPEN_OWN);

  tcp = make_tcp_port_data(rfd, 2);
  if (!takeover)
    tcp->b.forget_on_close = 1;

  v = make_tcp_input_port(tcp, name, NULL);
  *_inp = v;
  v = make_tcp_output_port(tcp, name, NULL);
  *_outp = v;
}

void scheme_socket_to_input_port(intptr_t s, Scheme_Object *name, int takeover,
                                 Scheme_Object **_inp)
{
  Scheme_Tcp *tcp;
  Scheme_Object *v;
  rktio_fd_t *fd;

  fd = rktio_system_fd(scheme_rktio, s, (RKTIO_OPEN_READ | RKTIO_OPEN_SOCKET
                                         | RKTIO_OPEN_INIT
                                         | (takeover ? RKTIO_OPEN_OWN : 0)));

  tcp = make_tcp_port_data(fd, takeover ? 1 : 2);

  v = make_tcp_input_port_symbol_name(tcp, name, NULL);
  *_inp = v;
}

void scheme_rktio_socket_to_input_port(rktio_fd_t *fd, Scheme_Object *name, int takeover,
                                       Scheme_Object **_inp)
{
  Scheme_Tcp *tcp;
  Scheme_Object *v;

  tcp = make_tcp_port_data(fd, takeover ? 1 : 2);

  v = make_tcp_input_port_symbol_name(tcp, name, NULL);
  *_inp = v;
}

void scheme_socket_to_output_port(intptr_t s, Scheme_Object *name, int takeover,
                                  Scheme_Object **_outp)
{
  Scheme_Tcp *tcp;
  Scheme_Object *v;
  rktio_fd_t *fd;
  
  fd = rktio_system_fd(scheme_rktio, s, (RKTIO_OPEN_WRITE | RKTIO_OPEN_SOCKET
                                         | RKTIO_OPEN_INIT
                                         | (takeover ? RKTIO_OPEN_OWN : 0)));
  
  tcp = make_tcp_port_data(fd, takeover ? 1 : 2);

  v = make_tcp_output_port_symbol_name(tcp, name, NULL);
  *_outp = v;
}

void scheme_rktio_socket_to_output_port(rktio_fd_t *fd, Scheme_Object *name, int takeover,
                                        Scheme_Object **_outp)
{
  Scheme_Tcp *tcp;
  Scheme_Object *v;

  tcp = make_tcp_port_data(fd, takeover ? 1 : 2);

  v = make_tcp_output_port_symbol_name(tcp, name, NULL);
  *_outp = v;
}

intptr_t scheme_dup_socket(intptr_t fd) {
  rktio_fd_t *rfd, *rfd2;
  intptr_t s;

  rfd = rktio_system_fd(scheme_rktio, fd, (RKTIO_OPEN_READ | RKTIO_OPEN_WRITE | RKTIO_OPEN_SOCKET));
  rfd2 = rktio_dup(scheme_rktio, rfd);

  s = rktio_fd_system_fd(scheme_rktio, rfd2);

  rktio_forget(scheme_rktio, rfd);
  rktio_forget(scheme_rktio, rfd2);

  return s;
}

void scheme_close_socket_fd(intptr_t fd) 
{
  rktio_fd_t *rfd;
  
  rfd = rktio_system_fd(scheme_rktio, fd, RKTIO_OPEN_SOCKET | RKTIO_OPEN_OWN);
  (void)scheme_rktio_fd_to_semaphore(rfd, MZFD_REMOVE);
  rktio_close(scheme_rktio, rfd);
}

/*========================================================================*/
/*                                 UDP                                    */
/*========================================================================*/

/* Based on a design and implemenation by Eduardo Cavazos. */

typedef struct Scheme_UDP_Evt {
  Scheme_Object so; /* scheme_udp_evt_type */
  Scheme_UDP *udp;
  short for_read, with_addr;
  int offset, len;
  char *str;
  rktio_addrinfo_t *dest_addr;
} Scheme_UDP_Evt;

static void free_dest_addr(void *udp_evt, void *ignored)
{
  Scheme_UDP_Evt *evt = (Scheme_UDP_Evt *)udp_evt;
  rktio_addrinfo_free(scheme_rktio, evt->dest_addr);
}

static int udp_default_family() {
  return rktio_get_ipv4_family(scheme_rktio);
}

static int udp_close_it(Scheme_Object *_udp)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (udp->s) {
    (void)scheme_rktio_fd_to_semaphore(udp->s, MZFD_REMOVE);
    rktio_close(scheme_rktio, udp->s);
    udp->s = NULL;

    scheme_remove_managed(udp->mref, (Scheme_Object *)udp);

    return 0;
  }

  return 1;
}

static Scheme_Object *make_udp(int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp;
  rktio_fd_t *s;
  char *address = "";
  unsigned short id;
  rktio_addrinfo_t *addr;

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
    id = (unsigned short)SCHEME_INT_VAL(argv[1]);
  else
    id = 0;

  scheme_security_check_network("udp-open-socket", address, id, 0);
  scheme_custodian_check_available(NULL, "udp-open-socket", "network");

  if (address || id) {
    int show_id_on_error = !!id;
    
    if (!id)
      id = 1025;

    addr = do_resolve_address("upd-open-socket", address, id, RKTIO_FAMILY_ANY, 0, show_id_on_error);
  } else
    addr = NULL;

  s = rktio_udp_open(scheme_rktio, addr, udp_default_family());

  if (addr)
    rktio_addrinfo_free(scheme_rktio, addr);
  
  if (!s) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "udp-open-socket: creation failed\n"
                     "  system error: %R");
    return NULL;
  }

  udp = MALLOC_ONE_TAGGED(Scheme_UDP);
  udp->so.type = scheme_udp_type;
  udp->s = s;
  udp->bound = 0;
  udp->connected = 0;
  udp->previous_from_addr = NULL;

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
}

static Scheme_Object *
udp_close(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract("udp-close", "udp?", 0, argc, argv);

  if (udp_close_it(argv[0])) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "udp-close: udp socket was already closed");
    return NULL;
  }

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

  return (((Scheme_UDP *)argv[0])->bound ? scheme_true : scheme_false);
}

static Scheme_Object *
udp_connected_p(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract("udp-connected?", "udp?", 0, argc, argv);

  return (((Scheme_UDP *)argv[0])->connected ? scheme_true : scheme_false);
}

static Scheme_Object *udp_bind_or_connect(const char *name, int argc, Scheme_Object *argv[], int do_bind)
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);

  {
    Scheme_UDP *udp;
    char *address = NULL;
    unsigned short port = 0;
    rktio_addrinfo_t *addr;

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

    if (!udp->s) {
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
      if (udp->connected) {
        if (!rktio_udp_disconnect(scheme_rktio, udp->s)) {
          scheme_raise_exn(MZEXN_FAIL_NETWORK,
                           "%s: can't disconnect\n"
                           "  system error: %R",
                           name);
        }
        udp->connected = 0;
      }
      return scheme_void;
    }

    addr = do_resolve_address(name, address, port, RKTIO_FAMILY_ANY, do_bind, 1);

    if (!do_bind) {
      /* CONNECT CASE */
      int ok;

      ok = rktio_udp_connect(scheme_rktio, udp->s, addr);
      rktio_addrinfo_free(scheme_rktio, addr);

      if (!ok) {
        scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                         "%s: can't connect\n"
                         "  address: %s\n"
                         "  port number: %d\n"
                         "  system error: %R", 
                         name, 
                         address ? address : "<unspec>", 
                         port);

        return NULL;
      }

      udp->connected = 1;
    } else {
      /* BIND CASE */
      int ok;
      int reuse = ((argc > 3) && SCHEME_TRUEP(argv[3]));

      ok = rktio_udp_bind(scheme_rktio, udp->s, addr, reuse);
      rktio_addrinfo_free(scheme_rktio, addr);

      if (!ok) {
        scheme_raise_exn(MZEXN_FAIL_NETWORK, 
                         "%s: can't bind%s\n"
                         "  address: %s\n"
                         "  port number: %d\n"
                         "  system error: %R",
                         name,
                         reuse ? " as reusable" : "",
                         address ? address : "<unspec>", 
                         port);
        return NULL;
      }

      udp->bound = 1;
    }
  }

  return scheme_void;
}

static Scheme_Object *udp_bind(int argc, Scheme_Object *argv[])
{
  return udp_bind_or_connect("udp-bind!", argc, argv, 1);
}

static Scheme_Object *udp_connect(int argc, Scheme_Object *argv[])
{
  return udp_bind_or_connect("udp-connect!", argc, argv, 0);
}

static int udp_check_send(Scheme_Object *_udp, Scheme_Schedule_Info *sinfo)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (!udp->s)
    return 1;

  if (!sinfo || !sinfo->is_poll) {
    if (!check_fd_sema(udp->s, MZFD_CHECK_WRITE, sinfo, NULL))
      return 0;
  }

  if (rktio_poll_write_ready(scheme_rktio, udp->s))
    return 1;

  check_fd_sema(udp->s, MZFD_CREATE_WRITE, sinfo, NULL);
  
  return 0;
}

static void udp_send_needs_wakeup(Scheme_Object *_udp, void *fds)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;
  rktio_poll_add(scheme_rktio, udp->s, fds, RKTIO_POLL_WRITE);
}

static Scheme_Object *do_udp_send_it(const char *name, Scheme_UDP *udp,
				     char *bstr, intptr_t start, intptr_t end,
                                     rktio_addrinfo_t *dest_addr, int free_addr,
                                     int can_block, int can_error)
{
  intptr_t x;

  while (1) {
    if (!udp->s) {
      /* socket was closed, maybe while we slept */
      if (free_addr) rktio_addrinfo_free(scheme_rktio, dest_addr);
      if (can_error)
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "%s: udp socket is closed\n"
                         "  socket: %V",
                         name, udp);
      return NULL;
    }
    if ((!dest_addr && !udp->connected)
	|| (dest_addr && udp->connected)) {
      /* socket is unconnected, maybe disconnected while we slept */
      if (free_addr) rktio_addrinfo_free(scheme_rktio, dest_addr);
      if (can_error)
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "%s: udp socket is%s connected\n"
                         "  socket: %V",
                         name, 
                         dest_addr ? "" : " not",
                         udp);
      return NULL;
    }

    udp->bound = 1; /* in case it's not bound already, send[to] binds it */

    x = rktio_udp_sendto(scheme_rktio, udp->s, dest_addr,
                         bstr XFORM_OK_PLUS start, end - start);
    
    if (x == RKTIO_WRITE_ERROR) {
      break;
    } else if (!x) {
      if (can_block) {
        /* Block and eventually try again. */
        Scheme_Object *sema;
        sema = scheme_rktio_fd_to_semaphore(udp->s, MZFD_CREATE_WRITE);
        if (sema)
          scheme_wait_sema(sema, 0);
        else
          scheme_block_until((Scheme_Ready_Fun)udp_check_send, 
                             udp_send_needs_wakeup, 
                             (Scheme_Object *)udp, 
                             0);
      } else {
        if (free_addr) rktio_addrinfo_free(scheme_rktio, dest_addr);
        return scheme_false;
      }
    } else if (x != (end - start)) {
      /* this isn't supposed to happen: */
      if (free_addr) rktio_addrinfo_free(scheme_rktio, dest_addr);
      if (can_error)
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "%s: didn't send enough (%d != %d)", 
                         name,
                         x, end - start);
      return NULL;
    } else
      break;
  }

  if (free_addr) rktio_addrinfo_free(scheme_rktio, dest_addr);

  if (x != RKTIO_WRITE_ERROR) {
    return (can_block ? scheme_void : scheme_true);
  } else {
    if (can_error)
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "%s: send failed\n"
                       "  system error: %R", 
                       name);
    return NULL;
  }
}

static Scheme_Object *udp_send_it(const char *name, int argc, Scheme_Object *argv[],
				  int with_addr, int can_block, Scheme_UDP_Evt *fill_evt)
{
  Scheme_UDP *udp;
  char *address = "";
  intptr_t start, end;
  int delta;
  unsigned short id;
  rktio_addrinfo_t *dest_addr;

  udp = (Scheme_UDP *)argv[0];

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);

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
    id = (unsigned short)SCHEME_INT_VAL(argv[2]);

    scheme_security_check_network(name, address, id, 1);

    dest_addr = do_resolve_address(name, address, id, RKTIO_FAMILY_ANY, 0, 1);
  } else {
    dest_addr = NULL;
  }
    
  
  if (fill_evt) {
    fill_evt->str = SCHEME_BYTE_STR_VAL(argv[3+delta]);
    fill_evt->offset = start;
    fill_evt->len = end - start;
    fill_evt->dest_addr = dest_addr;
    scheme_add_finalizer(fill_evt, free_dest_addr, NULL);
    return scheme_void;
  } else {
    return do_udp_send_it(name, udp,
                          SCHEME_BYTE_STR_VAL(argv[3+delta]), start, end,
                          dest_addr, 1, 
                          can_block, 1);
  }
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

static int udp_check_recv(Scheme_Object *_udp, Scheme_Schedule_Info *sinfo)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;

  if (!udp->s)
    return 1;

  if (!sinfo || !sinfo->is_poll) {
    if (!check_fd_sema(udp->s, MZFD_CHECK_READ, sinfo, NULL))
      return 0;
  }

  if (rktio_poll_read_ready(scheme_rktio, udp->s))
    return 1;

  check_fd_sema(udp->s, MZFD_CREATE_READ, sinfo, NULL);

  return 0;
}

static void udp_recv_needs_wakeup(Scheme_Object *_udp, void *fds)
{
  Scheme_UDP *udp = (Scheme_UDP *)_udp;
  rktio_poll_add(scheme_rktio, udp->s, fds, RKTIO_POLL_READ);
}

static int do_udp_recv(const char *name, Scheme_UDP *udp, char *bstr, intptr_t start, intptr_t end, 
		       int can_block, int can_error, Scheme_Object **v)
{
  rktio_length_and_addrinfo_t *result;

  if (!udp->bound) {
    if (can_error)
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "%s: udp socket is not bound\n"
                       "  socket: %V",
                       name,
                       udp);
    return -1;
  }

  while (1) {
    if (!udp->s) {
      /* socket was closed, maybe while we slept */
      if (can_error)
        scheme_raise_exn(MZEXN_FAIL_NETWORK,
                         "%s: udp socket is closed\n"
                         "  socket: %V",
                         name, udp);
      return -1;
    }

    result = rktio_udp_recvfrom(scheme_rktio, udp->s, bstr XFORM_OK_PLUS start, end - start);
    
    if (!result) {
      if (scheme_last_error_is_racket(RKTIO_ERROR_TRY_AGAIN)
          || scheme_last_error_is_racket(RKTIO_ERROR_INFO_TRY_AGAIN)) {
        if (!can_block) {
          v[0] = scheme_false;
	  v[1] = scheme_false;
	  v[2] = scheme_false;
	  return 0;
        }
        /* Block and eventually try again. */
        {
          Scheme_Object *sema;
          sema = scheme_rktio_fd_to_semaphore(udp->s, MZFD_CREATE_READ);
          if (sema)
            scheme_wait_sema(sema, 0);
          else
            scheme_block_until((Scheme_Ready_Fun)udp_check_recv, 
                               udp_recv_needs_wakeup, 
                               (Scheme_Object *)udp,
                               0);
        }
      } else {
        if (can_error)
          scheme_raise_exn(MZEXN_FAIL_NETWORK,
                           "%s: receive failed\n"
                           "  system error: %R",
                           name);
        return -1;
      }
    } else {
      /* Data received */
      char prev_buf[64];
      int id;

      v[0] = scheme_make_integer(result->len);
    
      if (udp->previous_from_addr) {
        /* See if we can use this cached string */
        mzchar *s;
        int j;
        s = SCHEME_CHAR_STR_VAL(udp->previous_from_addr);
        for (j = 0; s[j]; j++) {
          prev_buf[j] = (char)s[j];
        }
        prev_buf[j] = 0;
      }

      if (udp->previous_from_addr && !strcmp(prev_buf, result->address[0])) {
        v[1] = udp->previous_from_addr;
      } else {
        Scheme_Object *vv;
        vv = scheme_make_immutable_sized_utf8_string(result->address[0], -1);
        v[1] = vv;
        udp->previous_from_addr = v[1];
      }

      id = extract_svc_value(result->address[1]);
      
      v[2] = scheme_make_integer(id);

      free(result->address[0]);
      free(result->address[1]);
      free(result->address);
      free(result);

      return 1;
    }
  }
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
  
  scheme_get_substring_indices(name, argv[1], 
			       argc, argv,
			       2, 3, &start, &end);
  if (fill_evt) {
    fill_evt->str = SCHEME_BYTE_STR_VAL(argv[1]);
    fill_evt->offset = start;
    fill_evt->len = end - start;
    return scheme_void;
  } else {
    do_udp_recv(name, udp, SCHEME_BYTE_STR_VAL(argv[1]), start, end, can_block, 1, v);

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
  Scheme_UDP_Evt *uw;

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);

  uw = MALLOC_ONE_TAGGED(Scheme_UDP_Evt);
  uw->so.type = scheme_udp_evt_type;
  uw->udp = (Scheme_UDP *)argv[0];
  uw->for_read = for_read;

  return (Scheme_Object *)uw;
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

static int udp_evt_check_ready(Scheme_Object *_uw, Scheme_Schedule_Info *sinfo)
{
  Scheme_UDP_Evt *uw = (Scheme_UDP_Evt *)_uw;

  if (uw->for_read) {
    if (uw->str) {
      Scheme_Object *v[3];
      int r;
      
      r = do_udp_recv("udp-receive!-evt", uw->udp, 
		      uw->str, uw->offset, uw->offset + uw->len, 
		      0, !sinfo->false_positive_ok, v);
      if (r) {
        if (r != -1)
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
      r = do_udp_send_it("udp-send-evt", uw->udp, 
                         uw->str, uw->offset, uw->offset + uw->len, 
                         uw->dest_addr, 0,
                         0, !sinfo->false_positive_ok);
      if (!r || SCHEME_TRUEP(r)) {
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

static void udp_check_open(char const *name, int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp = (Scheme_UDP *)argv[0];
  
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);

  if (!udp->s) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "%s: udp socket was already closed\n"
                     "  socket: %V",
                     name, udp);
  }
}

static Scheme_Object *
udp_multicast_loopback_p(int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp = (Scheme_UDP *)argv[0];
  int r;

  udp_check_open("udp-multicast-loopback?", argc, argv);

  r = rktio_udp_get_multicast_loopback(scheme_rktio, udp->s);
  if (r == RKTIO_PROP_ERROR)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "udp-multicast-loopback?: getsockopt failed\n"
                     "  system error: %R");

  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *
udp_multicast_set_loopback(int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp = (Scheme_UDP *)argv[0];
  udp_check_open("udp-multicast-set-loopback!", argc, argv);

  if (!rktio_udp_set_multicast_loopback(scheme_rktio, udp->s, SCHEME_TRUEP(argv[1])))
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "udp-multicast-set-loopback!: setsockopt failed\n"
                     "  system error: %R");
    
  return scheme_void;
}

static Scheme_Object *
udp_multicast_ttl(int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp = (Scheme_UDP *) argv[0];
  int r;
  
  udp_check_open("udp-multicast-ttl", argc, argv);

  r = rktio_udp_get_multicast_ttl(scheme_rktio, udp->s);

  if (r == RKTIO_PROP_ERROR)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "udp-multicast-ttl: getsockopt failed\n"
                     "  system error: %R");

  return scheme_make_integer(r);
}

static Scheme_Object *
udp_multicast_set_ttl(int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp = (Scheme_UDP *)argv[0];

  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract("udp-multicast-set-ttl!", "udp?", 0, argc, argv);

  if (!SCHEME_INTP(argv[1]) || (SCHEME_INT_VAL(argv[1]) < 0) || (SCHEME_INT_VAL(argv[1]) >= 256)) {
    scheme_wrong_contract("udp-multicast-set-ttl!", "byte?", 1, argc, argv);
    return NULL;
  }
  
  udp_check_open("udp-multicast-set-ttl!", argc, argv);

  if (!rktio_udp_set_multicast_ttl(scheme_rktio, udp->s, SCHEME_INT_VAL(argv[1])))
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "udp-multicast-set-ttl!: setsockopt failed\n"
                     "  system error: %R");

  return scheme_void;
}

static Scheme_Object *
udp_multicast_interface(int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp = (Scheme_UDP *)argv[0];
  char *intf;
  Scheme_Object *res;
  
  udp_check_open("udp-multicast-interface", argc, argv);

  intf = rktio_udp_multicast_interface(scheme_rktio, udp->s);
  if (!intf)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "udp-multicast-interface: getsockopt failed\n"
                     "  system error: %R");

  res = scheme_make_utf8_string(intf);
  free(intf);

  return res;
}

static Scheme_Object *
udp_multicast_set_interface(int argc, Scheme_Object *argv[])
{
  Scheme_UDP *udp = (Scheme_UDP *)argv[0];
  rktio_addrinfo_t *addr;
  Scheme_Object *bs;
  int r;
  
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract("udp-multicast-set-interface!", "udp?", 0, argc, argv);
  
  if (!SCHEME_CHAR_STRINGP(argv[1]) && !SCHEME_FALSEP(argv[1])) {
    scheme_wrong_contract("udp-multicast-set-interface!", "(or/c string? #f)", 1, argc, argv);
    return NULL;
  }

  udp_check_open("udp-multicast-set-interface!", argc, argv);

  if (SCHEME_CHAR_STRINGP(argv[1])) {
    bs = scheme_char_string_to_byte_string(argv[1]);
    addr = do_resolve_address("udp-multicast-set-interface!", SCHEME_BYTE_STR_VAL(bs), -1, udp_default_family(), 0, 0);
  } else
    addr = NULL;

  r = rktio_udp_set_multicast_interface(scheme_rktio, udp->s, addr);

  if (addr) rktio_addrinfo_free(scheme_rktio, addr);

  if (!r)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "udp-multicast-set-interface!: setsockopt failed\n"
                     "  system error: %R");

  return scheme_void;
}

static Scheme_Object *
do_udp_multicast_join_or_leave_group(char const *name, int optname, Scheme_UDP *udp,
                                     Scheme_Object *multiaddrname, Scheme_Object *ifaddrname)
{
  Connect_Progress_Data *pd;
  Scheme_Object *bs;
  rktio_addrinfo_t *multi_addr, *intf_addr;
  char *address;
  rktio_addrinfo_lookup_t *lookup;
  int r;

  pd = make_connect_progress_data();

  bs = scheme_char_string_to_byte_string(multiaddrname);
  address = SCHEME_BYTE_STR_VAL(bs);

  lookup = rktio_start_addrinfo_lookup(scheme_rktio, address, -1, udp_default_family(), 0, 0);
  if (lookup) {
    pd->lookup = lookup;
    wait_until_lookup(pd);
    pd->lookup = NULL;

    multi_addr = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
  } else
    multi_addr = NULL;

  if (!multi_addr)
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
                     "%s: can't resolve group address\n"
                     "  address: %s\n"
                     "  system error: %R",
                     name,
                     address);

  pd->dest_addr = multi_addr;

  if (SCHEME_FALSEP(ifaddrname)) {
    intf_addr = NULL;
  } else {
    bs = scheme_char_string_to_byte_string(ifaddrname);
    address = SCHEME_BYTE_STR_VAL(bs);

    lookup = rktio_start_addrinfo_lookup(scheme_rktio, address, -1, udp_default_family(), 0, 0);
    if (lookup) {
      pd->lookup = lookup;
      wait_until_lookup(pd);
      pd->lookup = NULL;

      intf_addr = rktio_addrinfo_lookup_get(scheme_rktio, lookup);
    } else
      intf_addr = NULL;

    if (!intf_addr) {
      rktio_addrinfo_free(scheme_rktio, multi_addr);
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
                       "%s: can't resolve interface address\n"
                       "  address: %s\n"
                       "  system error: %R",
                       name,
                       address);
    }
  }

  r = rktio_udp_change_multicast_group(scheme_rktio, udp->s,
                                       multi_addr,
                                       intf_addr,
                                       optname);

  rktio_addrinfo_free(scheme_rktio, multi_addr);
  if (intf_addr) rktio_addrinfo_free(scheme_rktio, intf_addr);

  if (!r) 
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "%s: setsockopt failed\n"
		     "  system error: %R",
		     name);
  
  return scheme_void;
}

static Scheme_Object *
udp_multicast_join_or_leave_group(char const *name, int optname, int argc, Scheme_Object *argv[])
{
  if (!SCHEME_UDPP(argv[0]))
    scheme_wrong_contract(name, "udp?", 0, argc, argv);  
  
  if (!SCHEME_CHAR_STRINGP(argv[1])) {
    scheme_wrong_contract(name, "string?", 1, argc, argv);
    return NULL;
  }
  
  if (!SCHEME_CHAR_STRINGP(argv[2]) && !SCHEME_FALSEP(argv[2])) {
    scheme_wrong_contract(name, "(or/c string? #f)", 2, argc, argv);
    return NULL;
  }

  udp_check_open(name, argc, argv);
  
  return do_udp_multicast_join_or_leave_group(name, optname,
					      (Scheme_UDP *) argv[0], argv[1], argv[2]);
}

static Scheme_Object *
udp_multicast_join_group(int argc, Scheme_Object *argv[])
{
  return udp_multicast_join_or_leave_group("udp-multicast-join-group!",
					   RKTIO_ADD_MEMBERSHIP,
					   argc,
					   argv);
}

static Scheme_Object *
udp_multicast_leave_group(int argc, Scheme_Object *argv[])
{
  return udp_multicast_join_or_leave_group("udp-multicast-leave-group!",
					   RKTIO_DROP_MEMBERSHIP,
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
  GC_REG_TRAV(scheme_listener_type, mark_listener);
  GC_REG_TRAV(scheme_rt_tcp, mark_tcp);
  GC_REG_TRAV(scheme_udp_type, mark_udp);
  GC_REG_TRAV(scheme_udp_evt_type, mark_udp_evt);
}

END_XFORM_SKIP;

#endif
