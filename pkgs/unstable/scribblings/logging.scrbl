#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/logging))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/logging))

@title{Logging}
@unstable[@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]]

@defmodule[unstable/logging]

This module provides tools for logging.

@defproc[(with-logging-to-port
           [port output-port?] [proc (-> any)]
           [log-spec (or/c 'fatal 'error 'warning 'info 'debug symbol? #f)] ...)
         any]{

Runs @racket[proc], outputting any logging that would be received by
@racket[(make-log-receiver (current-logger) log-spec ...)] to @racket[port].
Returns whatever @racket[proc] returns.

@defexamples[
#:eval the-eval
(let ([my-log (open-output-string)])
  (with-logging-to-port my-log
    (lambda ()
      (log-warning "Warning World!")
      (+ 2 2))
    'warning)
  (get-output-string my-log))]}


@defproc[(with-intercepted-logging
           [interceptor (-> (vector/c
                              (or/c 'fatal 'error 'warning 'info 'debug)
                              string?
                              any/c
                              (or/c symbol? #f))
                             any)]
           [proc (-> any)]
           [log-spec (or/c 'fatal 'error 'warning 'info 'debug symbol? #f)] ...)
         any]{

Runs @racket[proc], calling @racket[interceptor] on any log message that would
be received by @racket[(make-log-receiver (current-logger) log-spec ...)].
@racket[interceptor] receives the entire log vectors
(see @secref["receiving-logged-events" #:doc '(lib "scribblings/reference/reference.scrbl")])
as arguments. Returns whatever @racket[proc] returns.

@defexamples[
#:eval the-eval
(let ([warning-counter 0])
  (with-intercepted-logging
    (lambda (l)
      (when (eq? (vector-ref l 0) ; actual level
                 'warning)
        (set! warning-counter (add1 warning-counter))))
    (lambda ()
      (log-warning "Warning!")
      (log-warning "Warning again!")
      (+ 2 2))
    'warning)
  warning-counter)]}


A lower-level interface to logging is also available.

@deftogether[[
  @defproc[(start-recording
            [log-spec (or/c 'fatal 'error 'warning 'info 'debug symbol? #f)] ...)
           listener?]
  @defproc[(stop-recording [listener listener?])
           (listof (vector/c (or/c 'fatal 'error 'warning 'info 'debug)
                             string?
                             any/c
                             (or/c symbol? #f)))]]]{

@racket[start-recording] starts recording log messages matching the given
@racket[log-spec] (see @racket[make-log-receiver] for how @racket[log-spec] is
interpreted). Messages will be recorded until stopped by passing the returned
listener object to @racket[stop-recording]. @racket[stop-recording] will then
return a list of the log messages that have been reported.

@defexamples[
#:eval the-eval
(define l (start-recording 'warning))
(log-warning "1")
(log-warning "2")
(stop-recording l)
]}


@close-eval[the-eval]
