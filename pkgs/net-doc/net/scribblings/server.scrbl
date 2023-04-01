#lang scribble/doc

@(require "common.rkt"
          racket/runtime-path
          scribble/example
          (for-label net/server
                     openssl
                     racket/contract
                     racket/tcp
                     racket/list))

@title[#:tag "server"]{General-purpose Server}

@defmodule[net/server]{The @racketmodname[net/server] library provides
support for running general-purpose networked servers.}

@defproc[(start-server [host (or/c #f string?)]
                       [port listen-port-number?]
                       [handle (-> input-port? output-port? any)]
                       [#:reuse? reuse? any/c #t]
                       [#:max-allow-wait max-allow-wait exact-nonnegative-integer? 511]
                       [#:max-concurrent max-concurrent
                                         (or/c +inf.0 exact-positive-integer?)
                                         +inf.0]
                       [#:listen-proc listen
                                      (-> listen-port-number?
                                          exact-nonnegative-integer?
                                          any/c
                                          (or/c #f string?)
                                          (and/c listener? evt?))
                                      tcp-listen]
                       [#:accept-proc accept
                                      (-> listener? (values input-port? output-port?))
                                      tcp-accept]
                       [#:close-proc close
                                     (-> listener? void?)
                                     tcp-close]
                       [#:timeout-evt-proc make-timeout-evt
                                           (-> thread? input-port? output-port? boolean? evt?)
                                           (λ (thd in out break-sent?) never-evt)]) (-> void?)]{

  Creates a listening server on @racket[port], bound to the interface
  associated with @racket[host].  After the listener is ready, it
  spawns a background thread to accept new connections.  For every new
  connection, @racket[handle] is called with two arguments: an input
  port to read from the client, and an output port to write to the
  client.  The returned procedure stops the accepting thread and
  closes the listener but does not terminate active connections.

  Each client connection is managed by a new custodian, and each call
  to @racket[handle] occurs in a new thread (also managed by the
  connection's custodian).  Each handling thread has an associated
  supervising thread that shuts down the connection's custodian when
  the handling thread terminates or when the result of
  @racket[make-timeout-evt] is ready for synchronization.  Breaks are
  enabled in handling threads if breaks are enabled when
  @racket[start-server] is called.  Handling threads need not close
  the provided input and output ports.

  The @racket[make-timeout-evt] procedure is called with the
  connection-handling thread, the input port and the output port of
  every new connection, and a boolean to signal if the handling thread
  has already been sent a break.  When the event it returns is ready
  for synchronization and if the handling thread is still running, the
  handling thread is sent a break and @racket[make-timeout-evt] is
  called again to produce an event that, when ready for
  synchronization, will cause the connection's custodian to be shut
  down and, consequently, the handling thread to be killed if it is
  still running by that time.

  The server keeps track of the number of active connections and
  pauses accepting new connections once that number reaches
  @racket[max-concurrent], resuming once the number goes down again.

  The @racket[reuse?] and @racket[max-allow-wait] arguments have the
  same meaning as in @racket[tcp-listen].  The @racket[listen],
  @racket[accept] and @racket[close] arguments may be used to run the
  server on an alternate protocol.  The @racket[listener?] part of the
  contract indicates that the procedures must all work on the same
  kind of listener value.  The values returned by @racket[listen] must
  be synchronizable events that are ready for synchronization when
  @racket[accept] would not block; the synchronization result of the
  listener must be a value that can be passed to @racket[accept].

  @history[#:added "1.1"]
}

@defproc[(run-server [host (or/c #f string?)]
                     [port listen-port-number?]
                     [handle (-> input-port? output-port? any)]
                     [#:reuse? reuse? any/c #t]
                     [#:max-allow-wait max-allow-wait exact-nonnegative-integer? 511]
                     [#:max-concurrent max-concurrent
                                       (or/c +inf.0 exact-positive-integer?)
                                       +inf.0]
                     [#:listen-proc listen
                                    (-> listen-port-number?
                                        exact-nonnegative-integer?
                                        any/c
                                        (or/c #f string?)
                                        (and/c listener? evt?))
                                    tcp-listen]
                     [#:accept-proc accept
                                    (-> listener? (values input-port? output-port?))
                                    tcp-accept]
                     [#:close-proc close
                                   (-> listener? void?)
                                   tcp-close]
                     [#:timeout-evt-proc make-timeout-evt
                                         (-> thread? input-port? output-port? boolean? evt?)
                                         (λ (thd in out break-sent?) never-evt)]) (-> void?)]{

  Spawns a server using @racket[start-server] and blocks the current
  thread until a break is received.  Before returning, it stops the
  spawned server.  The server is run with breaks disabled.

  @history[#:added "1.1"]
}

@section{Examples}

@; Run `raco setup` with the environment variable RECORD_NET_SERVER_EVAL
@; set to any value to update the recorded example output. Note that one
@; of the examples expects unix-socket-lib to be installed.
@(define-runtime-path server-log.rktd "server-log.rktd")
@(define ev (make-log-based-eval server-log.rktd (if (getenv "RECORD_NET_SERVER_EVAL") 'record 'replay)))
@(ev '(require net/server racket/tcp))

@subsection{TCP Echo Server}

Here is an implementation of a TCP echo server using
@racket[start-server]:

@examples[
  #:label #f
  #:eval ev
  (define (echo in out)
    (define buf (make-bytes 4096))
    (let loop ()
      (define n-read (read-bytes-avail! buf in))
      (unless (eof-object? n-read)
        (write-bytes buf out 0 n-read)
        (flush-output out)
        (loop))))
  (code:line)

  (define stop
    (start-server "127.0.0.1" 9000 echo))

  (code:line)
  (define-values (in out)
    (tcp-connect "127.0.0.1" 9000))
  (displayln "hello" out)
  (flush-output out)
  (read-line in)
  (close-output-port out)
  (close-input-port in)
  (stop)
]

@subsection{TCP Echo Server with TLS Support}

Here is how you might wrap the previous echo server implementation to
add TLS support:

@margin-note{
  For brevity, we use an insecure client context here.  See
  @other-doc['(lib "openssl/openssl.scrbl")] for details.
}

@examples[
  #:label #f
  #:eval ev
  (require openssl)
  (code:line)

  (define ((make-tls-echo ctx) in out)
    (define-values (ssl-in ssl-out)
      (ports->ssl-ports
       #:context ctx
       #:mode 'accept
       in out))
    (echo ssl-in ssl-out))
  (code:line)

  (define server-ctx
    (ssl-make-server-context))
  (ssl-load-certificate-chain! server-ctx (collection-file-path "test.pem" "openssl"))
  (ssl-load-private-key! server-ctx (collection-file-path "test.pem" "openssl"))
  (ssl-seal-context! server-ctx)
  (code:line)

  (define stop
    (start-server "127.0.0.1" 9000 (make-tls-echo server-ctx)))

  (code:line)
  (define-values (in out)
    (ssl-connect "127.0.0.1" 9000))
  (displayln "hello" out)
  (flush-output out)
  (read-line in)
  (close-output-port out)
  (close-input-port in)
  (stop)
]

@subsection{Echo Server over Unix Domain Sockets}

This example builds upon the previous one to run an echo server with
TLS over Unix domain sockets.  The Unix socket listener is wrapped in
a custom struct to keep track of the socket path so it can be deleted
on server shutdown.

@margin-note{See the @other-doc['(lib
"scribblings/socket/unix-socket.scrbl") #:indirect "Unix Domain
Sockets"] for details on the procedures used here.}

@examples[
  #:label #f
  #:eval ev
  (require racket/unix-socket)
  (code:line)

  (struct listener (path the-wrapped-listener)
    #:property prop:evt (struct-field-index the-wrapped-listener))
  (code:line)

  (define path "/tmp/server.sock")
  (define stop
    (start-server
     #:listen-proc (λ (port backlog reuse? host)
                     (unless (unix-socket-path? host)
                       (error 'start-server "invalid socket path: ~e" host))
                     (listener host (unix-socket-listen host backlog)))
     #:accept-proc unix-socket-accept
     #:close-proc (λ (l) (delete-file (listener-path l)))
     path 0 (make-tls-echo server-ctx)))
  (code:line)

  (define-values (in out)
    (let-values ([(in out) (unix-socket-connect path)])
      (ports->ssl-ports in out)))
  (displayln "hello" out)
  (flush-output out)
  (read-line in)
  (close-output-port out)
  (close-input-port in)
  (stop)
]

@subsection{Echo Server over Ports}

Finally, here is an echo server that operates entirely within a Racket
process and does not rely on any networking machinery:

@examples[
  #:label #f
  #:eval ev
  (define ch (make-channel))
  (define stop
    (start-server
     #:listen-proc (λ (port backlog reuse? host) ch)
     #:accept-proc (λ (ports) (apply values ports))
     #:close-proc void
     "127.0.0.1" 0 echo))
  (code:line)

  (define-values (in out)
    (make-pipe))
  (channel-put ch (list in out))
  (displayln "hello" out)
  (read-line in)
  (stop)
]
