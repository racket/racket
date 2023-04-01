#lang scribble/doc

@(require "common.rkt"
          scribble/example
          (for-label net/server
                     racket/contract
                     racket/tcp
                     racket/list))

@title[#:tag "server"]{General-purpose Server}

@defmodule[net/server]{The @racketmodname[net/server] library provides
support for running general-purpose networked servers.}

@(define ev (make-base-eval))

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
  server on an alternate protocol (such as SSL).  The
  @racket[listener?] part of the contract indicates that the
  procedures must all work on the same kind of listener value.  The
  values returned by @racket[listen] must be synchronizable events
  that are ready for synchronization when @racket[accept] would not
  block; the synchronization result of the listener must be a value
  that can be passed to @racket[accept].

  To listen on and retrieve an ephemeral port, set the @racket[port]
  argument to @racket[0] and pass a custom @racket[listen] procedure
  that retrieves the port using @racket[tcp-addresses] in the case of
  a TCP server.  For example:

  @examples[
    #:label #f
    #:eval ev
    (require net/server
             racket/tcp)
    (define ((make-listen-proc port-ch) port backlog reuse? host)
      (define listener
        (tcp-listen port backlog reuse? host))
      (define-values (local-host local-port remote-host remote-port)
        (tcp-addresses listener #t))
      (thread (lambda () (channel-put port-ch local-port)))
      listener)
    (define port-ch
      (make-channel))
    (define stop
      (start-server
        #:listen-proc (make-listen-proc port-ch)
        "127.0.0.1" 0 void))
    (channel-get port-ch)
    (stop)
  ]

  Here is a simple line-oriented echo server that listens on an
  ephemeral port:

  @examples[
    #:label #f
    #:eval ev
    (define (echo in out)
      (let loop ()
        (define line (read-line in))
        (unless (eof-object? line)
          (displayln line out)
          (flush-output out)
          (loop))))
    (define port-ch
      (make-channel))
    (define stop
      (start-server
        #:listen-proc (make-listen-proc port-ch)
        "127.0.0.1" 0 echo))
    (define port (channel-get port-ch))
    (define-values (in out)
      (tcp-connect "127.0.0.1" port))
    (displayln "hello" out)
    (flush-output out)
    (read-line in)
    (close-output-port out)
    (close-input-port in)
    (stop)
  ]

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
