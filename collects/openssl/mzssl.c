/* ssl.c: an extension to PLT MzScheme to allow SSL connections */

#define OPENSSL_NO_KRB5

#include <openssl/ssl.h>
#include <openssl/err.h>
#include "escheme.h"

#ifdef USE_UNIX_SOCKETS_TCP
# include <sys/types.h>
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>
# define SOCK_ERRNO() errno
# define NOT_WINSOCK(x) (x)
# define INVALID_SOCKET (-1)
# define WAS_EINPROGRESS(e) ((e == EINPROGRESS))
# define mz_h_errno() h_errno
# define mz_hstrerror(x) dup_errstr(hstrerror(x))
#endif

#ifdef USE_WINSOCK_TCP
# include <winsock2.h>
# include <ws2tcpip.h>
# define SOCK_ERRNO() WSAGetLastError()
# define NOT_WINSOCK(x) 0
# define WAS_EINPROGRESS(e) ((e == WSAEWOULDBLOCK))
# define mz_h_errno() WSAGetLastError()
# define mz_hstrerror(x) NULL
#endif

/* stolen from $(PLTHOME}/src/mzscheme/src/network.c */
/* For getting connection names: */
#define MZ_SOCK_NAME_MAX_LEN 256
#define MZ_SOCK_HOST_NAME_MAX_LEN 64
#define MZ_SOCK_SVC_NAME_MAX_LEN 32

/* stolen from $(PLTHOME}/src/mzscheme/src/schpriv.h */
#ifdef USE_FCNTL_O_NONBLOCK
# define MZ_NONBLOCKING O_NONBLOCK
#else
# define MZ_NONBLOCKING FNDELAY
#endif

/* stolen from $(PLTHOME)/src/mzscheme/src/schfd.h */
#ifdef USE_FAR_MZ_FDCALLS
# define DECL_FDSET(n, c) static fd_set *n
# define INIT_DECL_FDSET(n, c) (n = (n ? (fd_set *)scheme_init_fdset_array(n,c)\
                                       : (fd_set*)scheme_alloc_fdset_array(c,1)))
#else
# define DECL_FDSET(n, c) fd_set n[c]
# define INIT_DECL_FDSET(n, c) /* empty */
#endif
struct sslplt {
#ifdef MZ_PRECISE_GC
  Scheme_Type type;
#endif
  SSL *ssl;
  char *obuffer; /* Buffer for outgoing bytes, but this is not a "buffer"
		    at the Scheme level. A daemon thread flushes it.
		    This is necessary because there's no way to know
		    whether a write to SSL will succeed, and handing
		    WANT_READ and WANT_WRITE requires consistent
		    calls to SSL_write. */
  int ob_used;  /* Length of data in obuffer. */
  char ibuffer; /* One char is enough because SSL_read doesn't appear
		   to actually need consistency for WANT_READ and WANT_WRITE.
		   Or maybe I've just been [un]lucky. */
  char ib_used; /* 0 or 1, length of data in ibuffer */
  char close_in, close_out;
  char write_blocked_reason; /* 0 => might not be blocked, 1 => for read, 2 => for write */
  struct sslplt *next;
};

#define OBUFFER_SIZE 4096

typedef struct {
  Scheme_Object so;
  int s;
  Scheme_Custodian_Reference *mref;
  SSL_CTX *ctx;
} listener_t;

typedef struct {
  Scheme_Object so;
  SSL_CTX *ctx;
} mzssl_ctx_t;

static Scheme_Type ssl_listener_type;
static Scheme_Type ssl_ctx_type;
#ifdef MZ_PRECISE_GC
static Scheme_Type sslplt_type;
#endif

#define LISTENER_WAS_CLOSED(x) (((listener_t *)(x))->s == INVALID_SOCKET)

#ifndef MZ_PRECISE_GC
# define GC_CAN_IGNORE /* empty */
# define XFORM_OK_PLUS +
#endif

/* create_ register_sslplt: called when a new sslplt structure needs to be 
   created. */
struct sslplt *create_register_sslplt(SSL *ssl)
{
  struct sslplt *sslplt;
  char *obuffer;

#ifdef MZ_PRECISE_GC
  sslplt = (struct sslplt *)scheme_malloc_tagged(sizeof(struct sslplt));
  sslplt->type = sslplt_type;
#else
  sslplt = (struct sslplt *)scheme_malloc(sizeof(struct sslplt));
#endif

  obuffer = (char *)scheme_malloc_atomic(OBUFFER_SIZE);

  sslplt->ssl = ssl;
  sslplt->ib_used = 0; sslplt->ob_used = 0; 
  sslplt->close_in = 0; sslplt->close_out = 0;
  sslplt->write_blocked_reason = 0;
  sslplt->obuffer = obuffer;
  return sslplt;
}

/*****************************************************************************
 * GENERC SOCKET CHECKS: ready? and needs-wakeup.                            *
 *****************************************************************************/

int check_socket_ready(int s, int for_write)
{
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};
  int res;

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);

  do {
    res = select(s + 1, 
		 for_write ? NULL : writefds, 
		 for_write ? writefds : NULL, 
		 exnfds, &time);
  } while((res == -1) && NOT_WINSOCK(errno == EINTR));

  return res;
}

void socket_add_fds(int s, void *fds, int for_write)
{
  void *fds1, *fds2;
  
  fds1 = MZ_GET_FDSET(fds, (for_write ? 1 : 0));
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
}

/*****************************************************************************
 * TOP-LEVEL THREAD: This is the routine and data involved with running the  *
 * top level thread (the one that helps us fake a couple guarantees).        *
 * When output can't be written without blocking, we put one char in a       *
 * and promise to flush in a priviledged daemon thread. If this one-char     *
 * buffer is empty, we can promise a "non-blocking" write to the Scheme      *
 * level. We need the one-char buffer because the SSL layer offers no way to *
 * get a promise that at least one character can be written without          *
 * blocking.                                                                 *
 *****************************************************************************/
Scheme_Object *daemon_attn = NULL;
struct sslplt *ssls = NULL;

int daemon_ready(Scheme_Object *ignored)
{
  struct sslplt *cur;

  for (cur = ssls; cur; cur = cur->next) {
    if (!cur->write_blocked_reason) {
      /* Newly queued, or someone else operated on the same
	 SSL connection, so we should try again. */
      return 1;
    } else
      /* The SLL layer is waiting for either input or output
	 on the underlying socket: */
      if (check_socket_ready(BIO_get_fd(SSL_get_wbio(cur->ssl), NULL),
			     (cur->write_blocked_reason == 2)))
	return 1;
  }

  return 0;
}

void deamon_needs_wakeup(Scheme_Object *ignored, void *fds)
{
  struct sslplt *cur;

  for (cur = ssls; cur; cur = cur->next) {
    if (!cur->write_blocked_reason)
      scheme_cancel_sleep();
    else
      socket_add_fds(BIO_get_fd(SSL_get_wbio(cur->ssl), NULL),
		     fds,
		     (cur->write_blocked_reason == 2));
  }
}


/* write_close_thread: this is the thread that flushes out our buffers
   automatically and/or closes items which need closing */
Scheme_Object *write_close_thread(int argc, Scheme_Object *argv[])
{
  struct sslplt *cur, *prev;
  int empty;

  /* this thread should not terminate unless killed externally */
  while (1) {
    /* Wait until there's something to do: */
    scheme_wait_sema(daemon_attn, 0);

    while (1) {
      cur = ssls; prev = NULL;
      while (cur) {
	int status, drop = 1;

	if (cur->ob_used) {
	  cur->write_blocked_reason = 0;

	  /* Try to write: */
	  status = SSL_write(cur->ssl, cur->obuffer, cur->ob_used);

	  if (status >= 1) {
	    cur->ob_used -= status;
	    if (cur->ob_used) {
	      memmove(cur->obuffer, cur->obuffer + status, cur->ob_used);
	      drop = 0;
	    }
	  } else {
	    int err;
	    drop = 0;
	    err = SSL_get_error(cur->ssl, status);
	    if (err == SSL_ERROR_WANT_READ) {
	      cur->write_blocked_reason = 1;
	    } else if (err == SSL_ERROR_WANT_WRITE) {
	      cur->write_blocked_reason = 2;
	    } else {
	      /* Some error. We drop the char, and assume
		 that it's not a transient error, so the
		 next action will find the same error. */
	      drop = 1;
	      cur->ob_used = 0;
	    }
	  }
	} else if (cur->close_in && cur->close_out) {
	  /* Apparently a force close */
          SSL_free(cur->ssl);
        }
	/* there shouldn't be a 3rd possibility */
	
	if (!drop) {
	  prev = cur; 
	} else{
	  if (prev)
	    prev->next = cur->next;
	  else
	    ssls = cur->next;
	}
	cur = cur->next;
      }
      empty = !ssls;
   
      if (empty)
	break;

      /* wait until something becomes unblocked, or something new is queued */
      scheme_block_until(daemon_ready, deamon_needs_wakeup, NULL, (float)0.0);
    }
  }
}

/*****************************************************************************
 * ERROR FUNCTIONs: wrap the SSL error reporter to get a string and error    *
 * number that fits into MzScheme's %Z convention.                           *
 * Also, copy error strings provided libraries, in case of a thread swap     *
 * between the time the string is obtained and the string is put into an     *
 * error message (otherwise the string could get overwritten?).              *
 *****************************************************************************/

static const char *dup_errstr(const char *s) {
  char *t;
  long len;
  if (s){
    len = strlen(s);
    t = scheme_malloc_atomic(len+1);
    memcpy(t, s, len+1);
    return t;
  } else
    return s;
}

static int get_ssl_error_msg(int errid, const char **msg, int status, int has_status)
{
  if ((errid == SSL_ERROR_SYSCALL) && has_status) {
    if (status == 0) {
      *msg = "unexpected EOF";
    } else {
      *msg = NULL;
      errid = SOCK_ERRNO();
    }
  } else {
    const char *c;
    char buf[121];

    if (errid == SSL_ERROR_SSL) {
      errid = ERR_get_error();
    }

    if (errid && (ERR_GET_LIB(errid) == ERR_LIB_SYS)) {
      *msg = NULL;
      return ERR_GET_REASON(errid);
    } else {
      memset(buf, 0, 121);
      /* wants a buffer of size 120: */
      ERR_error_string(errid, buf);

      c = dup_errstr(buf);
      if (c)
	*msg = c;
      else
	*msg = "Unknown error";
    }
  }

  return errid;
}

/*****************************************************************************
 * INPORT PORT FUNCTIONS: This is the stuff that works on input ports. This  *
 * is a little complicated because we have to get char_ready to work on top  *
 * of a system that doesn't have such a function. So we buffer one character *
 * as necessary.                                                             *
 *****************************************************************************/

/* this is the new subtype we're creating */
static Scheme_Object *ssl_input_port_type = NULL; 

/* forward decls: */
static void sslin_need_wakeup(Scheme_Input_Port *port, void *fds);
static int sslin_char_ready(Scheme_Input_Port *port);

/* ssl_get_string: read a sequence of bytes into a buffer given to us. This is 
   made severely annoying by the nonblocking nature of the socket stream and 
   the possibly blocking nature that mzscheme might want from us. */
long ssl_do_get_string(Scheme_Input_Port *port, char *buffer, long offset,
		       long size, int nonblocking, 
		       int *stuck_why, int err_ok,
		       Scheme_Object *unless) 
{
  const char *errstr = "Unknown error";
  int err = 0;
  long status = 0;
  long bytes_read = 0;
  struct sslplt *ssl = (struct sslplt *)SCHEME_INPORT_VAL(port);

  while (!bytes_read) {
    /* check unless before anything else */
    if (scheme_unless_ready(unless))
      return SCHEME_UNLESS_READY;

    /* check the buffer */
    if(ssl->ib_used) {
      buffer[offset + bytes_read] = ssl->ibuffer;
      bytes_read++; 
      ssl->ib_used = 0;
    }

    /* make sure people aren't being sneaky */
    if(ssl->close_in) {
      errstr = "read from closed port!";
      goto read_error;
    }
    
    /* re-check writes,if any are blocked, since we're touching the
       ssl channel */
    ssl->write_blocked_reason = 0;
  
    if (ssl->ob_used) {
      /* A write needs to be re-tried. Can't read until then. */
      bytes_read = 0;
      *stuck_why = 3;
    } else {
      /* read the data. maybe. hopefully. please. */
      status = SSL_read(ssl->ssl, 
			buffer XFORM_OK_PLUS offset XFORM_OK_PLUS bytes_read, 
			size-bytes_read);
      
      if(status < 1) {
	/* see what kind of error this was */
	err = SSL_get_error(ssl->ssl, status);

	/* see if we've hit the end of file */
	if ((err == SSL_ERROR_ZERO_RETURN)
	    || ((err == SSL_ERROR_SYSCALL) && !status)) {
	  if(bytes_read == 0)
	    return EOF;
	  else
	    return bytes_read;
	} else if ((err != SSL_ERROR_WANT_READ) && (err != SSL_ERROR_WANT_WRITE)) {
	  /* critical error */
	  if (!err_ok) return 0;

	  err = get_ssl_error_msg(err, &errstr, status, 1);
	  goto read_error;
	}

	*stuck_why = ((err == SSL_ERROR_WANT_READ) ? 1 : 2);
      } else
	bytes_read += status;
    }

    if (nonblocking > 0)
      break;

    /* It might be tempting at this point to block on the fd
       for reading if SSL_ERROR_WANT_READ. That would be a bad
       idea, because another thread might be using the port,
       and might shift it into SSL_ERROR_WANT_WRITE mode.
       Use the general sll input blocking functions. */

    if (!bytes_read) {
      while (!sslin_char_ready(port)) {
	scheme_block_until_unless((Scheme_Ready_Fun)sslin_char_ready, 
				  (Scheme_Needs_Wakeup_Fun)sslin_need_wakeup,
				  (void *)port, (float)0.0,
				  unless,
				  nonblocking < 0);
	
	scheme_wait_input_allowed(port, nonblocking);
	
	if (scheme_unless_ready(unless))
	  return SCHEME_UNLESS_READY;
      }
    }
  }
  
  return bytes_read;

 read_error:
  scheme_raise_exn(MZEXN_FAIL_NETWORK, 
		   "ssl-read: error reading (%Z)",
		   err, errstr);
  return 0; /* needless, but it makes GCC happy */
}

long ssl_get_string(Scheme_Input_Port *port, char *buffer, long offset,
		    long size, int nonblocking,
		    Scheme_Object *unless) 
{
  int stuck_why;

  return ssl_do_get_string(port, buffer, offset, size, nonblocking, 
			   &stuck_why, 1,
			   unless);
}

/* sslin_char_ready: return 1 (true) iff a nonblocking call to get_string 
   can read at least one character (that is, it won't return 0). This 
   function is the cause of a bit of suffering, actually. */
static int sslin_do_char_ready(Scheme_Input_Port *port, int *stuck_why)
{
  struct sslplt *ssl = SCHEME_INPORT_VAL(port);
  char buf[1];
  int r;
  
  *stuck_why = 0;

  if (ssl->close_in) return 1;

  /* see if the buffer has something in it, and if so, return true */
  if(ssl->ib_used) return 1;

  /* otherwise, try to read a character in */
  r = ssl_do_get_string(port, buf, 0, 1, 1, stuck_why, 0, NULL);
  if (r) {
    if (r != EOF) {
      ssl->ib_used = 1;
      ssl->ibuffer = ((unsigned char *)buf)[0];
    }
    return 1;
  }

  if (!*stuck_why) {
    /* not-yet-reported error */
    return 1;
  }

  /* nothing buffered and we can't read, so the answer is no */
  return 0;
}

static int sslin_char_ready(Scheme_Input_Port *port)
{
  int stuck_why;

  return sslin_do_char_ready(port, &stuck_why);
}

/* sslin_close: close down a buffer, freeing the temporary structures we
   created. */
void sslin_close(Scheme_Input_Port *port)
{
  struct sslplt *ssl;
  ssl= (struct sslplt *)SCHEME_INPORT_VAL(port);

  ssl->close_in = 1;
  ssl->write_blocked_reason = 0;

  if (ssl->close_out)
    SSL_free(ssl->ssl);
}

/* sslin_need_wakeup: called when the input port is blocked to determine 
   what exactly it's blocked on. We have to try a read to find out
   why it's blocked: waiting for input or output on the low-level
   socket? */
static void sslin_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  struct sslplt *ssl = SCHEME_INPORT_VAL(port);
  long rfd;
  void *fds2;
  int stuck_why;

  rfd = BIO_get_fd(SSL_get_rbio(ssl->ssl), NULL);

  if (sslin_do_char_ready(port, &stuck_why)) {
    /* Need wakeup now! */
    scheme_cancel_sleep();
  } else {
    if (stuck_why != 3)
      socket_add_fds(rfd, fds, (stuck_why == 2));
    /* but stuck_why == 3 implies that a write is
       responsible for waking up */
  }
}

/* make_sslin_port: called to create a scheme input port to return to the
   caller, eventually */
Scheme_Input_Port *make_sslin_port(SSL *ssl, struct sslplt *wrapper, const char *name)
{
  return scheme_make_input_port(ssl_input_port_type, wrapper, 
				scheme_make_immutable_sized_utf8_string(name ? (char *)name : "ssl", -1),
				ssl_get_string, 
				NULL, 
				scheme_progress_evt_via_get,
				scheme_peeked_read_via_get,
				sslin_char_ready, sslin_close, 
				sslin_need_wakeup, 1);
}

/*****************************************************************************
 * OUTPUT PORT FUNCTIONS: This is the stuff that works on output ports. This *
 * is very complicated because we have to get char_ready to work on top of a *
 * system that doesn't have such a function. So we buffer one character on   *
 * output as necessary.                                                      *
 *****************************************************************************/

/* this is the new subtype we're creating */
static Scheme_Object *ssl_output_port_type = NULL;

/* forward decls: */
static int sslout_char_ready(Scheme_Output_Port *port);
static void sslout_need_wakeup(Scheme_Output_Port *port, void *fds);

/* write_string: write some bits of data out to the wire, if possible. This
   is made complicated by a host of problems. */
long write_string(Scheme_Output_Port *port, const char *buffer, long offset, 
		  long size, int rarely_block, int enable_break) 
{
  struct sslplt *ssl = (struct sslplt *)SCHEME_OUTPORT_VAL(port);
  const char *errstr = "Unknown error";
  int err = 0;
  int status = 0;
  long out_size;

  /* make sure people aren't trying to do something sneaky */
  if (ssl->close_out) {
    errstr = "write to closed port!"; 
    goto write_error;
  }

  if (ssl->ob_used) {
    if (rarely_block == 2)
      return size ? 0 : -1; /* return -1 if this was a flush request */
    /* Wait until it's writable */
    scheme_block_until_enable_break((Scheme_Ready_Fun)sslout_char_ready, 
				    (Scheme_Needs_Wakeup_Fun)sslout_need_wakeup,
				    (void *)port, (float)0.0,
				    enable_break);
  }

  /* We get here only when !ssl->ob_used. */

  if (!size) /* this was a flush request, and we've flushed */
    return 0;

  /* could have been closed by another thread */
  if (ssl->close_out) {
    errstr = "write to closed port!"; 
    goto write_error;
  }

  /* Try to write a decent sized chunk.  We have to copy it to
     obuffer, in case the write must be continued (in which case
     SSL_write insists on getting the same arguments that it received
     last time). */
  out_size = size;
  if (out_size > OBUFFER_SIZE)
    out_size = OBUFFER_SIZE;
  memcpy(ssl->obuffer, buffer + offset, out_size);
  status = SSL_write(ssl->ssl, ssl->obuffer, out_size);

  if (status > 0)
    return status; /* success */

  err = SSL_get_error(ssl->ssl, status);
  if((err != SSL_ERROR_WANT_READ) && (err != SSL_ERROR_WANT_WRITE)) {
    err = get_ssl_error_msg(err, &errstr, status, 1);
    goto write_error;
  }

  /* Can't write a decent-sized chunk. Put out_size chars in the outgoing
     buffer, and block as necessary until those chars are flushed */
  if (out_size) {
    int was_empty;

    ssl->ob_used = out_size;

    /* Put this SLL into the list of things that the deamon must
       process. */
    was_empty = !ssls;
    ssl->next = ssls;
    ssls = ssl;

    /* Wake up the daemon thread if the list used to be empty: */
    if (was_empty)
      scheme_post_sema(daemon_attn);

    /* We "wrote" out_size bytes. The daemon will ensure that the bytes
       actually go out. */

    return out_size;
  } else
    return 0;

 write_error:
   scheme_raise_exn(MZEXN_FAIL_NETWORK, 
		    "ssl-write: error writing (%Z)",
		   err, errstr);
  return 0; /* needless, but it makes GCC happy */
}

/* sslout_char_ready: return 1 (true) iff a nonblocking (version 1, not 
   version 2) call to write_string will write at least one character. */
static int sslout_char_ready(Scheme_Output_Port *port)
{
  struct sslplt *ssl = SCHEME_OUTPORT_VAL(port);

  return !ssl->ob_used;
}

static int shutdown_ready(Scheme_Object *_ssl)
{
  struct sslplt *ssl = (struct sslplt *)_ssl;

  if (!ssl->write_blocked_reason)
    return 1;
  else
    return check_socket_ready(BIO_get_fd(SSL_get_wbio(ssl->ssl), NULL),
			      (ssl->write_blocked_reason == 2));
}

static void shutdown_need_wakeup(Scheme_Object *_ssl, void *fds)
{
  struct sslplt *ssl = (struct sslplt *)_ssl;

  if (!ssl->write_blocked_reason)
    scheme_cancel_sleep();
  else
    socket_add_fds(BIO_get_fd(SSL_get_wbio(ssl->ssl), NULL),
		   fds,
		   (ssl->write_blocked_reason == 2));
}

/* sslout_close: close down a buffer */
void sslout_close(Scheme_Output_Port *port)
{
  struct sslplt *ssl = (struct sslplt *)SCHEME_OUTPORT_VAL(port);
  int forced = 0;

  if (ssl->ob_used && scheme_close_should_force_port_closed()) {
    /* Tell daemon to give up on this port, 
       and don't bother with a shutdown: */
    ssl->ob_used = 0;
    ssl->write_blocked_reason = 0;
    ssl->close_out = 1;
  } else {
    /* We want to shutdown. If there's still a write in
       progress, wait. */
    if (ssl->ob_used){
      scheme_block_until((Scheme_Ready_Fun)sslout_char_ready, 
			 (Scheme_Needs_Wakeup_Fun)sslout_need_wakeup,
			 (void *)port, (float)0.0);
    }
    /* assert: !ssl->ob_used */
    /* it's possible that we were shut down in another
       thread, though. */

    /* FIXME: what if a another thread writes at this point?
       In particular, the code below assumes that the deamon
       thread is not trying to output. */

    while (!ssl->close_out) {
      int status;
      int err, tries = 0;

      while (1) {
	status = SSL_shutdown(ssl->ssl);
	if (status < 1)
	  err = SSL_get_error(ssl->ssl, status);
	/* Note: SSL_ERROR_SYSCALL may be erroneous if status was 0.
	   Indeed 0 seems to be the result in many cases because the socket
	   is non-blocking, and then neither of the WANTs is returned.
	   We address this by simply trying 10 times and then giving
	   up. The two-step shutdown is optional, anyway. */

	if ((status < 0) && !scheme_close_should_force_port_closed()
	    /* if an eof occurs, let's agree that it's shut down */
	    && !(err == SSL_ERROR_SYSCALL)) {
	  if (err == SSL_ERROR_WANT_READ)
	    ssl->write_blocked_reason = 1;
	  else if (err == SSL_ERROR_WANT_WRITE)
	    ssl->write_blocked_reason = 2;
	  else {
	    const char *errstr;
	    err = get_ssl_error_msg(err, &errstr, status, 1);
	    scheme_raise_exn(MZEXN_FAIL_NETWORK, 
			     "ssl-close: error shutting down output (%Z)",
			     err, errstr);
	    return;
	  }

	  scheme_block_until(shutdown_ready, 
			     shutdown_need_wakeup,
			     (void *)ssl, (float)0.0);      
	} else if (status || (tries > 10)) {
	  ssl->close_out = 1;
	  if (ssl->close_in) {
	    SSL_free(ssl->ssl);
	  }
	  break;
	} else {
	  tries++;
	  scheme_thread_block(0.0);
	}
      }
    }
  }
}

/* sslout_need_wakeup: we don't do anything, because low-level
   blocking is handled by the daemon thread */
static void sslout_need_wakeup(Scheme_Output_Port *port, void *fds)
{
}

/* make_sslout_port: called to create a scheme output port to return to the
   caller, eventually. */
static Scheme_Output_Port *make_sslout_port(SSL *ssl, struct sslplt *data, const char *name)
{
  return scheme_make_output_port(ssl_output_port_type, data, 
				 scheme_make_immutable_sized_utf8_string(name ? (char *)name : "ssl-output", -1),
				 scheme_write_evt_via_write,
				 write_string, 
				 sslout_char_ready, sslout_close, 
				 sslout_need_wakeup, 
				 NULL, NULL, 1);
}

/*****************************************************************************
 * CLEANING AND NETWORK FUNCTIONS: These are the functions which convert the *
 * things we get in to things that are useful, plus the routines for doing   *
 * various network operations                                                *
 *****************************************************************************/

/* check_host_and_convert: Make absolutely sure the first argument was a 
   string, and then convert it into a character string we can actually use.
   Or scream bloody murder if it wasn't a string. */
char *check_host_and_convert(const char *name, int argc, Scheme_Object *argv[], int pos)
{
  if (SCHEME_CHAR_STRINGP(argv[pos]))
    return SCHEME_BYTE_STR_VAL(scheme_char_string_to_byte_string(argv[pos]));
  
  scheme_wrong_type(name, "string", pos, argc, argv);
  return NULL; /* unnecessary, but it makes GCC happy */
}

/* check_port_and_convert: Make absolutely sure the second argument
   was a potential port number, and if it is, convert it into a number
   we can actually use. Or scream if it wasn't kosher. */
unsigned short check_port_and_convert(const char *name, int argc, Scheme_Object *argv[], int pos)
{
  if(SCHEME_INTP(argv[pos]))
    if(SCHEME_INT_VAL(argv[pos]) >= 1)
      if(SCHEME_INT_VAL(argv[pos]) <= 65535)
	return SCHEME_INT_VAL(argv[pos]);
  scheme_wrong_type(name, "exact integer in [1, 65535]", pos, argc,argv);
  return 0; /* unnessary and wrong, but it makes GCC happy */
}

/* check_encrypt_and_convert: Check the third argument is a valid symbol here,
   and convert it to the SSL method function we'll be using if they gave us
   a good argument. Otherwise scream. The third argument tells us if we want
   client or server method functions. */
SSL_METHOD *check_encrypt_and_convert(const char *name, int argc, Scheme_Object *argv[], int pos, int c, int ctx_ok)
{
  Scheme_Object *v;

  if(argc <= pos)
    return (c ? SSLv23_client_method() : SSLv23_server_method());
    
  v = argv[pos];

  if (SAME_OBJ(v, scheme_intern_symbol("sslv2-or-v3"))) {
    return (c ? SSLv23_client_method() : SSLv23_server_method());
  } else if(SAME_OBJ(v, scheme_intern_symbol("sslv2"))) {
    return (c ? SSLv2_client_method() : SSLv2_server_method());
  } else if(SAME_OBJ(v, scheme_intern_symbol("sslv3"))) {
    return (c ? SSLv3_client_method() : SSLv3_server_method());
  } else if(SAME_OBJ(v, scheme_intern_symbol("tls"))) {
    return (c ? TLSv1_client_method() : TLSv1_server_method());
  } else {
#   define ALLOWED_SYMS "'sslv2-or-v3, 'sslv2, 'sslv3, or 'tls"
    scheme_wrong_type(name, 
		      ctx_ok ? "ssl-client-context, " ALLOWED_SYMS : ALLOWED_SYMS, 
		      pos, argc, argv);
    return NULL; /* unnecessary, but it makes GCC happy */
  }
}

/* ssl_check_sock: determine if a socket is ready for reading or
   writing; conector_p is an array of integers: socket and 
   0=>read/1=>write. */
int ssl_check_sock(Scheme_Object *connector_p)
{
  return check_socket_ready(((int *)connector_p)[0], ((int *)connector_p)[1]);
}

/* ssl_check_sock: block on socket for reading or
   writing; conector_p is an array of integers: socket and 
   0=>read/1=>write. */
void ssl_sock_needs_wakeup(Scheme_Object *connector_p, void *fds)
{
  socket_add_fds(((int *)connector_p)[0], fds, ((int *)connector_p)[1]);
}

#ifdef USE_UNIX_SOCKETS_TCP

/* closesocket: close a socket, and try real hard to do it. This is lifted 
   entirely from ${PLTHOME}/src/mzscheme/src/network.c */
void closesocket(long s)
{
  int cr;
  do { cr = close(s); } while((cr == -1) && NOT_WINSOCK(errno == EINTR));
}

#endif

/* close_socket_and_dec: called when we're broken out of our attempt to
   connect a socket */
void close_socket_and_dec(unsigned short sock)
{
  closesocket(sock);
}

/*****************************************************************************
 * SOCKET->SSL connection completion                                         *
 *****************************************************************************/

static Scheme_Object *finish_ssl(const char *name, int sock, SSL_METHOD *meth,
				 char *address, int port, int do_accept,
				 SSL_CTX *ctx_in)
{
  SSL_CTX *ctx = NULL;
  BIO *bio = NULL;
  SSL *ssl = NULL;
  struct sslplt *sslplt = NULL;
  const char *errstr = "Unknown error";
  Scheme_Object *retval[2]; 
  int status;
  int err = 0;
  int *sptr = NULL;

  /* set up the BIO pipe */
  bio = BIO_new_socket(sock, BIO_CLOSE);
  if(!bio) { errstr = "couldn't create BIO stream"; goto clean_up_and_die; }

  /* set up the SSL context object */
  if (!ctx_in) {
    ctx = SSL_CTX_new(meth);
    if(!ctx) { 
      err = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
      goto clean_up_and_die; 
    }
  }

  /* set up the full SSL object */
  ssl = SSL_new(ctx ? ctx : ctx_in);
  if(!ssl) {
    err = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
    goto clean_up_and_die; 
  }
  SSL_set_bio(ssl, bio, bio);

  if (ctx) {
    SSL_CTX_free(ctx); /* ssl has incremented ref count */
    ctx = NULL;
  }

  /* see if we can connect via SSL */
  if (do_accept)
    status = SSL_accept(ssl);
  else
    status = SSL_connect(ssl);
  while(status < 1) {
    err = SSL_get_error(ssl, status);
    if ((err == SSL_ERROR_WANT_READ) 
	|| (err == SSL_ERROR_WANT_WRITE)) {
      if (!sptr) {
	sptr = (int *)scheme_malloc_atomic(2 * sizeof(int));
	sptr[0] = sock;
      }
      sptr[1] = (err == SSL_ERROR_WANT_WRITE);

      BEGIN_ESCAPEABLE(close_socket_and_dec, sock);
      scheme_block_until(ssl_check_sock, ssl_sock_needs_wakeup, 
			 (void *)sptr, (float)0.0);
      END_ESCAPEABLE();
    } else {
      err = get_ssl_error_msg(err, &errstr, status, 1);
      goto clean_up_and_die;
    }
    if (do_accept)
      status = SSL_accept(ssl);
    else
      status = SSL_connect(ssl);
  }

  sslplt = create_register_sslplt(ssl);
  retval[0] = (Scheme_Object*)make_sslin_port(ssl, sslplt, address);
  retval[1] = (Scheme_Object*)make_sslout_port(ssl, sslplt, address);
  return scheme_values(2, retval);

 clean_up_and_die:
  if (ctx) 
    SSL_CTX_free(ctx);

  if (ssl) 
    SSL_free(ssl);
  else {
    if (bio)
      BIO_free(bio);
    else {
      if(sock) 
	closesocket(sock);
    }
  }

  if (do_accept)
    scheme_raise_exn(MZEXN_FAIL_NETWORK, 
		     "%s: accepted connection failed (%Z)",
		     name,
		     err, errstr);
  else
    scheme_raise_exn(MZEXN_FAIL_NETWORK, 
		     "%s: connection to %s, port %d failed (%Z)",
		     name,
		     address, port, err, errstr);
  
  /* not strictly necessary, but it makes our C compiler happy */
  return NULL;
}

#ifdef USE_WINSOCK_TCP
static int started;
static void TCP_INIT(char *name)
{
  static int started = 0;
  
  if (!started) {
    WSADATA data;
    if (!WSAStartup(MAKEWORD(1, 1), &data)) {
      started = 1;
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
# define TCP_INIT(n) /* empty */
#endif

/*****************************************************************************
 * SCHEME EXTERNAL FUNCTION IMPLEMENTATIONS: These are the implemenations of *
 * the functions which are actually going to be exported to MzScheme userland*
 *****************************************************************************/

static Scheme_Object *ssl_connect(int argc, Scheme_Object *argv[])
{
  char *address;
  unsigned short nport;
  int port;
  SSL_METHOD *meth;
  SSL_CTX *ctx;
  int status;
  const char *errstr = "Unknown error";
  int err = 0;
  GC_CAN_IGNORE struct addrinfo *addr;
  int sock;

  address = check_host_and_convert("ssl-connect", argc, argv, 0);
  nport = check_port_and_convert("ssl-connect", argc, argv, 1);
  port = SCHEME_INT_VAL(argv[1]);
  if ((argc > 2) && SAME_TYPE(SCHEME_TYPE(argv[2]), ssl_ctx_type)) {
    meth = NULL;
    ctx = ((mzssl_ctx_t *)(argv[2]))->ctx;
  } else {
    meth = check_encrypt_and_convert("ssl-connect", argc, argv, 2, 1, 1);
    ctx = NULL;
  }

  /* check we have the security clearance to actually do this */
  scheme_security_check_network("ssl-connect", address, port, 1);
  scheme_custodian_check_available(NULL, "ssl-connect", "network");

  TCP_INIT("ssl-connect");

  /* lookup hostname and get a reasonable structure */
  addr = scheme_get_host_address(address, nport, &err, -1, 0, 1);
  if (!addr) {
    sock = INVALID_SOCKET;
    errstr = gai_strerror(err);
    err = 0;
    goto clean_up_and_die;
  }

  /* try to create the socket */
  sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
  if (sock == INVALID_SOCKET)  { errstr = NULL; err = SOCK_ERRNO(); goto clean_up_and_die; }
#ifdef USE_WINSOCK_TCP
  {
    unsigned long ioarg = 1;
    ioctlsocket(sock, FIONBIO, &ioarg);
  }
#else
  fcntl(sock, F_SETFL, MZ_NONBLOCKING);
#endif
  
  status = connect(sock, (struct sockaddr *)addr->ai_addr, addr->ai_addrlen);
  freeaddrinfo(addr);

  /* here's the complicated bit */
  if (status) {
    int errid;
    errid = SOCK_ERRNO();
    if (!WAS_EINPROGRESS(errid)) { 
      errstr = NULL; err = errid; goto clean_up_and_die; 
    }

    {
      int *sptr;

      sptr = (int *)scheme_malloc_atomic(2 * sizeof(int));
      sptr[0] = sock;
      sptr[1] = 1;

      BEGIN_ESCAPEABLE(close_socket_and_dec, sock);
      scheme_block_until(ssl_check_sock, ssl_sock_needs_wakeup, 
			 (void *)sptr, (float)0.0);
      END_ESCAPEABLE();
    }

    /* see if the connection succeeded, or die if it didn't */
    {
      int so_len = sizeof(status);
      if(getsockopt(sock, SOL_SOCKET,SO_ERROR, (void*)&status, &so_len) != 0) {
	errstr = NULL;
	err = status; 
	goto clean_up_and_die;
      }
    }
  }

  return finish_ssl("ssl-connect", sock, meth, address, port, 0, ctx);

 clean_up_and_die:
  if (sock != INVALID_SOCKET) closesocket(sock);
  scheme_raise_exn(MZEXN_FAIL_NETWORK, 
		   "ssl-connect: connection to %T, port %d failed (%Z)",
		   argv[0], SCHEME_INT_VAL(argv[1]), 
		   err, errstr);
  
  /* not strictly necessary, but it makes our C compiler happy */
  return NULL;
}

static Scheme_Object *ssl_connect_break(int argc, Scheme_Object *argv[]) {
  return scheme_call_enable_break(ssl_connect, argc, argv);
}

/*****************************************************************************
 * SSL LISTENER: sadly, cut-and-paste from the MzScheme source.              *
 *****************************************************************************/

/* Forward declaration */
static int stop_listener(Scheme_Object *o);

static Scheme_Object *
ssl_listen(int argc, Scheme_Object *argv[])
{
  unsigned short id, origid;
  int backlog, errid;
  int reuse = 0;
  const char *address = NULL;
  SSL_METHOD *meth;
  SSL_CTX *ctx;

  id = check_port_and_convert("ssl-listen", argc, argv, 0);
  if (argc > 1) {
    if (!SCHEME_INTP(argv[1]) || (SCHEME_INT_VAL(argv[1]) < 1))
      scheme_wrong_type("ssl-listen", "small positive integer", 1, argc, argv);
  }
  if (argc > 2)
    reuse = SCHEME_TRUEP(argv[2]);
  if (argc > 3) {
    if (!SCHEME_FALSEP(argv[3]))
      address = check_host_and_convert("ssl-listen", argc, argv, 3);
  }
  
  if (0 && (argc > 4) && SAME_TYPE(SCHEME_TYPE(argv[4]), ssl_ctx_type)) {
    meth = NULL;
    ctx = ((mzssl_ctx_t *)(argv[4]))->ctx;
  } else {
    meth = check_encrypt_and_convert("ssl-connect", argc, argv, 4, 0, 0);
    ctx = NULL;
  }
    
  TCP_INIT("ssl-listen");

  origid = (unsigned short)SCHEME_INT_VAL(argv[0]);
  if (argc > 1)
    backlog = SCHEME_INT_VAL(argv[1]);
  else
    backlog = 4;

  scheme_security_check_network("ssl-listen", address, origid, 0);
  scheme_custodian_check_available(NULL, "ssl-listen", "network");

  if (!ctx) {
    ctx = SSL_CTX_new(meth);
    if(!ctx) { 
      const char *errstr;
      errid = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "sll-listen: context creation failed for listen on %d (%Z)",
		       origid, errid, errstr);
      return scheme_void;
    }
  }

  {
    GC_CAN_IGNORE struct addrinfo *tcp_listen_addr;
    int err;

    tcp_listen_addr = scheme_get_host_address(address, id, &err,
					      !address ? PF_INET : -1, 
					      1, 1);
    if (tcp_listen_addr) {
      int s;

      s = socket(tcp_listen_addr->ai_family,
		 tcp_listen_addr->ai_socktype,
		 tcp_listen_addr->ai_protocol);

      if (s != INVALID_SOCKET) {
#ifdef USE_WINSOCK_TCP
	unsigned long ioarg = 1;
	ioctlsocket(s, FIONBIO, &ioarg);
#else
	fcntl(s, F_SETFL, MZ_NONBLOCKING);
#endif

	if (reuse) {
	  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(int));
	}
      
	if (!bind(s, (struct sockaddr *)tcp_listen_addr->ai_addr, tcp_listen_addr->ai_addrlen)) {
	  if (!listen(s, backlog)) {
	    listener_t *l;

	    l = (listener_t *)scheme_malloc_tagged(sizeof(listener_t));
	    l->so.type = ssl_listener_type;
	    l->s = s;
	    l->ctx = ctx;
	    {
	      Scheme_Custodian_Reference *mref;
	      mref = scheme_add_managed(NULL,
					(Scheme_Object *)l,
					(Scheme_Close_Custodian_Client *)stop_listener,
					NULL,
					1);
	      l->mref = mref;
	    }

	    freeaddrinfo(tcp_listen_addr);
	    
	    return (Scheme_Object *)l;
	  }
	}

	errid = SOCK_ERRNO();

	closesocket(s);
	freeaddrinfo(tcp_listen_addr);
      } else {
	freeaddrinfo(tcp_listen_addr);
	errid = SOCK_ERRNO();
      }
    } else {
      if (ctx && meth)
	SSL_CTX_free(ctx);
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "ssl-listen: host not found: %s (%N)",
		       address, 1, err);
      return NULL;
    }
  }

  if (ctx && meth)
    SSL_CTX_free(ctx);
      
  scheme_raise_exn(MZEXN_FAIL_NETWORK,
		   "sll-listen: listen on %d failed (%E)",
		   origid, errid);

  return NULL;
}

static int stop_listener(Scheme_Object *o)
{
  int was_closed = 0;

  {
    int s = ((listener_t *)o)->s;
    if (s == INVALID_SOCKET)
      was_closed = 1;
    else {
      closesocket(s);
      ((listener_t *)o)->s = INVALID_SOCKET;
      scheme_remove_managed(((listener_t *)o)->mref, o);
      SSL_CTX_free(((listener_t *)o)->ctx);
    }
  }

  return was_closed;
}

static Scheme_Object *
ssl_close(int argc, Scheme_Object *argv[])
{
  int was_closed;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_listener_type))
    scheme_wrong_type("ssl-close", "ssl-listener", 0, argc, argv);

  was_closed = stop_listener(argv[0]);

  if (was_closed) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "ssl-close: listener was already closed");
    return NULL;
  }

  return scheme_void;
}

static Scheme_Object *
ssl_listener_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_listener_type))
	  ? scheme_true
	  : scheme_false);
}

enum {
  mzssl_CERT_CHAIN,
  mzssl_CERT_ROOT,
  mzssl_CERT_REQ,
  mzssl_RSA_KEY
};

static Scheme_Object *
ctx_load_file(const char *name, int mode, int client_ok, int argc, Scheme_Object *argv[])
{
  char *filename;
  const char *what;
  int result, use_rsa = 1, format = SSL_FILETYPE_PEM;
  SSL_CTX *ctx;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_listener_type)
      && (! client_ok || !SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_ctx_type)))
    scheme_wrong_type(name, 
		      (client_ok ? "ssl-listener or ssl-client-context" : "ssl-listener"),
		      0, argc, argv);

  if (!SCHEME_PATH_STRINGP(argv[1]))
    scheme_wrong_type(name, SCHEME_PATH_STRING_STR, 1, argc, argv);

  if (mode == mzssl_RSA_KEY) {
    if (argc > 2)
      use_rsa = SCHEME_TRUEP(argv[2]);
    if (argc > 3)
      if (SCHEME_TRUEP(argv[3]))
	format = SSL_FILETYPE_ASN1;
  }
  
  filename = scheme_expand_string_filename(argv[1],
					   name,
					   NULL,
					   SCHEME_GUARD_FILE_READ);

  if (SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_listener_type))
    ctx = ((listener_t *)(argv[0]))->ctx;
  else
    ctx = ((mzssl_ctx_t *)(argv[0]))->ctx;
  
  if (mode == mzssl_CERT_CHAIN) {
    result = SSL_CTX_use_certificate_chain_file(ctx, filename);
    what = "certificate chain";
  } else if (mode == mzssl_CERT_ROOT) {
    result = SSL_CTX_load_verify_locations(ctx, filename, NULL);
    what = "root certificates";
  } else if (mode == mzssl_CERT_REQ) {
    GC_CAN_IGNORE STACK_OF(X509_NAME) *stk;
    stk = SSL_load_client_CA_file(filename);
    if (stk) {
      result = 1;
      SSL_CTX_set_client_CA_list(ctx, stk);
    } else
      result = 0;
    what = "suggested certificate authorities";
  } else if (mode == mzssl_RSA_KEY) {
    if (use_rsa)
      result = SSL_CTX_use_RSAPrivateKey_file(ctx, filename, format);
    else
      result = SSL_CTX_use_PrivateKey_file(ctx, filename, format);
    what = "private key";
  }

  if (result != 1) {
    int errid;
    const char *errstr;
    errid = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "%s: %s load failed from: %s (%Z)",
		     name, what, filename, errid, errstr);
    return NULL;
  }

  return scheme_void;
}

static Scheme_Object *
ssl_load_cert_chain(int argc, Scheme_Object *argv[])
{
  return ctx_load_file("ssl-load-certificate-chain!", mzssl_CERT_CHAIN, 1, argc, argv);
}

static Scheme_Object *
ssl_load_cert_root(int argc, Scheme_Object *argv[])
{
  return ctx_load_file("ssl-load-root-verify-certificate!", mzssl_CERT_ROOT, 1, argc, argv);
}

static Scheme_Object *
ssl_load_accept_cert_auth(int argc, Scheme_Object *argv[])
{
  return ctx_load_file("ssl-load-suggested-certificate-authorities!", mzssl_CERT_REQ, 0, argc, argv);
}

static Scheme_Object *
ssl_load_priv_key(int argc, Scheme_Object *argv[])
{
  return ctx_load_file("ssl-load-prvate-key!", mzssl_RSA_KEY, 1, argc, argv);
}

static Scheme_Object *
ssl_set_verify(int argc, Scheme_Object *argv[])
{
  SSL_CTX *ctx;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_listener_type)
      && !SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_ctx_type))
    scheme_wrong_type("ssl-set-verify!", "ssl-listener or ssl-client-context", 0, argc, argv);

  if (SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_listener_type))
    ctx = ((listener_t *)(argv[0]))->ctx;
  else
    ctx = ((mzssl_ctx_t *)(argv[0]))->ctx;
  
  SSL_CTX_set_verify(ctx, 
		     (SCHEME_TRUEP(argv[1]) 
		      ? (SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT)
		      : SSL_VERIFY_NONE),
		     NULL);

  return scheme_void;
}

static void release_ctx(void *p, void *data)
{
  SSL_CTX_free(((mzssl_ctx_t *)p)->ctx);
}

static Scheme_Object *
ssl_mk_ctx(int argc, Scheme_Object *argv[])
{
  mzssl_ctx_t *c;
  SSL_METHOD *meth;
  SSL_CTX *ctx;

  meth = check_encrypt_and_convert("ssl-make-client-context", argc, argv, 0, 1, 0);

  c = (mzssl_ctx_t *)scheme_malloc_tagged(sizeof(mzssl_ctx_t));
  c->so.type = ssl_ctx_type;

  ctx = SSL_CTX_new(meth);
  if (!ctx) { 
    const char *errstr;
    int errid;
    errid = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "sll-make-context: context creation failed (%Z)",
		     errid, errstr);
    return scheme_void;
  }

  c->ctx = ctx;
  
  scheme_add_finalizer(c, release_ctx, NULL);

  return (Scheme_Object *)c;
}

static Scheme_Object *
ssl_ctx_p(int argc, Scheme_Object *argv[])
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_ctx_type))
	  ? scheme_true
	  : scheme_false);
}


static int tcp_check_accept(Scheme_Object *listener)
{
  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  return check_socket_ready(((listener_t *)listener)->s, 0);
}

static void tcp_accept_needs_wakeup(Scheme_Object *listener, void *fds)
{
  socket_add_fds(((listener_t *)listener)->s, fds, 0);
}

static Scheme_Object *
ssl_accept(int argc, Scheme_Object *argv[])
{
  int was_closed = 0, errid;
  Scheme_Object *listener;
  int s;
  int l;
  GC_CAN_IGNORE struct sockaddr_in tcp_accept_addr;
  
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), ssl_listener_type))
    scheme_wrong_type("ssl-accept", "ssl-listener", 0, argc, argv);

  scheme_custodian_check_available(NULL, "ssl-accept", "network");

  listener = argv[0];

  was_closed = LISTENER_WAS_CLOSED(listener);

  if (!was_closed) {
    scheme_block_until(tcp_check_accept, tcp_accept_needs_wakeup,
		       listener, (float)0.0);
    was_closed = LISTENER_WAS_CLOSED(listener);
  }

  if (was_closed) {
    scheme_raise_exn(MZEXN_FAIL_NETWORK,
		     "ssl-accept: listener is closed");
    return NULL;
  }

  scheme_custodian_check_available(NULL, "ssl-accept", "network");
  
  s = ((listener_t *)listener)->s;

  l = sizeof(tcp_accept_addr);

  do {
    s = accept(s, (struct sockaddr *)&tcp_accept_addr, &l);
  } while ((s == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (s != INVALID_SOCKET) {
# ifdef USE_WINSOCK_TCP
    {
      unsigned long ioarg = 1;
      ioctlsocket(s, FIONBIO, &ioarg);
    }
# else
    fcntl(s, F_SETFL, MZ_NONBLOCKING);
# endif
    
    return finish_ssl("ssl-accept", s, NULL, NULL, 0,
		      1, ((listener_t *)listener)->ctx);
  }


  errid = SOCK_ERRNO();
  scheme_raise_exn(MZEXN_FAIL_NETWORK,
		   "ssl-accept: accept from listener failed (%E)", errid);

  return NULL;
}

static Scheme_Object *ssl_accept_break(int argc, Scheme_Object *argv[]) {
  return scheme_call_enable_break(ssl_accept, argc, argv);
}

/*****************************************************************************
 * MISC                                                                      *
 *****************************************************************************/

static Scheme_Object *ssl_addresses(int argc, Scheme_Object *argv[])
{
  /* Again, sadly cut-and-paste from MzScheme's network.c */
  GC_CAN_IGNORE struct sockaddr_in tcp_here_addr, tcp_there_addr;
  int l, closed;
  struct sslplt *wrapper = NULL;
  Scheme_Object *result[2];
  int fd;

  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = (Scheme_Output_Port *)argv[0];
    if (op->sub_type == ssl_output_port_type) {
      wrapper = (struct sslplt *)op->port_data;
      fd = BIO_get_fd(SSL_get_wbio(wrapper->ssl), NULL);
    }
    closed = op->closed;
    
  } else if (SCHEME_INPORTP(argv[0])) {
    Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)argv[0];
    if (ip->sub_type == ssl_input_port_type) {
      wrapper = (struct sslplt *)ip->port_data;
      fd = BIO_get_fd(SSL_get_rbio(wrapper->ssl), NULL);
    }
    closed = ip->closed;
  }

  if (!wrapper)
    scheme_wrong_type("ssl-addresses", "ssl-port", 0, argc, argv);

  if (closed)
    scheme_raise_exn(MZEXN_FAIL,
		     "ssl-addresses: port is closed");

  {
    int l;
    char here[MZ_SOCK_NAME_MAX_LEN], there[MZ_SOCK_NAME_MAX_LEN];
    char host_buf[MZ_SOCK_HOST_NAME_MAX_LEN];
    int here_len, there_len;

    l = sizeof(here);
    if (getsockname(fd, (struct sockaddr *)here, &l)) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "tcp-addresses: could not get local address (%e)",
		       SOCK_ERRNO());
    }
    here_len = l;

    l = sizeof(there);
    if (getpeername(fd, (struct sockaddr *)there, &l)) {
      scheme_raise_exn(MZEXN_FAIL_NETWORK,
		       "tcp-addresses: could not get peer address (%e)",
		       SOCK_ERRNO());
    }
    there_len = l;

    getnameinfo((struct sockaddr *)here, here_len, 
		host_buf, sizeof(host_buf),
		NULL, 0,
		NI_NUMERICHOST | NI_NUMERICSERV);
    result[0] = scheme_make_utf8_string(host_buf);

    getnameinfo((struct sockaddr *)there, there_len, 
		host_buf, sizeof(host_buf),
		NULL, 0,
		NI_NUMERICHOST | NI_NUMERICSERV);
    result[1] = scheme_make_utf8_string(host_buf);
  }


  return scheme_values(2, result);
}

/*****************************************************************************
 * PRECISE GC                                                                *
 *****************************************************************************/

#ifdef MZ_PRECISE_GC

int sslplt_SIZE(void *p) {
  return
  gcBYTES_TO_WORDS(sizeof(struct sslplt));
}

int sslplt_MARK(void *p) {
  struct sslplt *ssl = (struct sslplt *)p;

  gcMARK(ssl->next);
  gcMARK(ssl->obuffer);

  return
  gcBYTES_TO_WORDS(sizeof(struct sslplt));
}

int sslplt_FIXUP(void *p) {
  struct sslplt *ssl = (struct sslplt *)p;

  gcFIXUP(ssl->next);
  gcFIXUP(ssl->obuffer);

  return
  gcBYTES_TO_WORDS(sizeof(struct sslplt));
}

int listener_SIZE(void *p) {
  return
  gcBYTES_TO_WORDS(sizeof(listener_t));
}

int listener_MARK(void *p) {
  listener_t *l = (listener_t *)p;

  gcMARK(l->mref);

  return
  gcBYTES_TO_WORDS(sizeof(listener_t));
}

int listener_FIXUP(void *p) {
  listener_t *l = (listener_t *)p;

  gcFIXUP(l->mref);

  return
  gcBYTES_TO_WORDS(sizeof(listener_t));
}

int mzssl_ctx_SIZE(void *p) {
  return
  gcBYTES_TO_WORDS(sizeof(mzssl_ctx_t));
}

int mzssl_ctx_MARK(void *p) {
  mzssl_ctx_t *l = (mzssl_ctx_t *)p;

  return
  gcBYTES_TO_WORDS(sizeof(mzssl_ctx_t));
}

int mzssl_ctx_FIXUP(void *p) {
  mzssl_ctx_t *l = (mzssl_ctx_t *)p;

  return
  gcBYTES_TO_WORDS(sizeof(mzssl_ctx_t));
}

#endif

/*****************************************************************************
 * REGISTRATION FUNCTIONS: The functions that register the above externals so*
 * everybody else can use them.                                              *
 *****************************************************************************/

/* scheme_initialize: called when the extension is first loaded */
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  Scheme_Object *thread;
  Scheme_Custodian *newcust;
  Scheme_Config *cfg;

  thread = scheme_make_prim_w_arity(write_close_thread,
				    "SSL Flushing Thread",
				    0, 0);
  newcust = scheme_make_custodian(NULL);

  scheme_register_extension_global(&daemon_attn, sizeof(daemon_attn));
  scheme_register_extension_global(&ssls, sizeof(ssls));
  scheme_register_extension_global(&ssl_input_port_type, sizeof(ssl_input_port_type));
  scheme_register_extension_global(&ssl_output_port_type, sizeof(ssl_output_port_type));
  
  SSL_library_init();
  daemon_attn = scheme_make_sema(0);
  ssl_listener_type = scheme_make_type("<ssl-listener>");
  ssl_ctx_type = scheme_make_type("<ssl-client-context>");
#ifdef MZTAG_REQUIRED
  sslplt_type = scheme_make_type("<ssl-plt-internal>");
#endif
  ssl_input_port_type = scheme_make_port_type("<ssl-input-port>");
  ssl_output_port_type = scheme_make_port_type("<ssl-output-port>");
  
#ifdef MZ_PRECISE_GC
  GC_register_traversers(ssl_listener_type, listener_SIZE,
			 listener_MARK, listener_FIXUP,
			 1, 0);
  GC_register_traversers(ssl_ctx_type, mzssl_ctx_SIZE,
			 mzssl_ctx_MARK, mzssl_ctx_FIXUP,
			 1, 1);
  GC_register_traversers(sslplt_type, sslplt_SIZE,
			 sslplt_MARK, sslplt_FIXUP,
			 1, 0);
#endif

  SSL_load_error_strings();


  scheme_add_evt(ssl_listener_type,
		 tcp_check_accept, tcp_accept_needs_wakeup,
		 NULL, 0);

  scheme_thread_w_details(thread, NULL, NULL, NULL, newcust, 0);
  return scheme_reload(env);
}

/* scheme_reload: called when an extension is loaded a second+ time */
Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object *v;

  v = scheme_intern_symbol("mzssl");
  env = scheme_primitive_module(v, env);
  
  /* add ssl-connect */
  v = scheme_make_prim_w_arity(ssl_connect, "ssl-connect", 2, 3);
  scheme_add_global("ssl-connect", v, env);

  v = scheme_make_prim_w_arity(ssl_connect_break,"ssl-connect/enable-break",2,3);
  scheme_add_global("ssl-connect/enable-break", v, env);

  v = scheme_make_prim_w_arity(ssl_listen,"ssl-listen",1,5);
  scheme_add_global("ssl-listen", v, env);

  v = scheme_make_prim_w_arity(ssl_close,"ssl-close",1,1);
  scheme_add_global("ssl-close", v, env);

  v = scheme_make_prim_w_arity(ssl_listener_p,"ssl-listener?",1,1);
  scheme_add_global("ssl-listener?", v, env);

  v = scheme_make_prim_w_arity(ssl_load_cert_chain,"ssl-load-certificate-chain!",2,2);
  scheme_add_global("ssl-load-certificate-chain!", v, env);

  v = scheme_make_prim_w_arity(ssl_load_cert_root,"ssl-load-verify-root-certificates!",2,2);
  scheme_add_global("ssl-load-verify-root-certificates!", v, env);

  v = scheme_make_prim_w_arity(ssl_load_cert_chain,"ssl-load-suggested-certificate-authorities!",2,2);
  scheme_add_global("ssl-load-suggested-certificate-authorities!", v, env);

  v = scheme_make_prim_w_arity(ssl_load_priv_key,"ssl-load-private-key!",2,4);
  scheme_add_global("ssl-load-private-key!", v, env);

  v = scheme_make_prim_w_arity(ssl_set_verify,"ssl-set-verify!",2,2);
  scheme_add_global("ssl-set-verify!", v, env);

  v = scheme_make_prim_w_arity(ssl_mk_ctx,"ssl-make-client-context",0,1);
  scheme_add_global("ssl-make-client-context", v, env);

  v = scheme_make_prim_w_arity(ssl_ctx_p,"ssl-client-context?",1,1);
  scheme_add_global("ssl-client-context?", v, env);

  v = scheme_make_prim_w_arity(ssl_accept,"ssl-accept",1,1);
  scheme_add_global("ssl-accept", v, env);

  v = scheme_make_prim_w_arity(ssl_accept_break,"ssl-accept/enable-break",1,1);
  scheme_add_global("ssl-accept/enable-break", v, env);

  v = scheme_make_prim_w_everything(ssl_addresses, 0, "ssl-addresses", 1, 1, 0, 2, 2);
  scheme_add_global("ssl-addresses", v, env);

  scheme_add_global("ssl-available?", scheme_true, env);
  scheme_finish_primitive_module(env);

  return scheme_void;
}

/* scheme_module_name: called to get the name of this module */
Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("mzssl");
}
