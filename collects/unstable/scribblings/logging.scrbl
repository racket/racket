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
           [#:level level (or/c 'fatal 'error 'warning 'info 'debug) 'debug])
         any]{

Runs @racket[proc], outputting any logging of level @racket[level] or higher to
@racket[port]. Returns whatever @racket[proc] returns.

@defexamples[
#:eval the-eval
(let ([my-log (open-output-string)])
  (with-logging-to-port my-log
    (lambda ()
      (log-warning "Warning World!")
      (+ 2 2))
    #:level 'warning)
  (get-output-string my-log))]}


@defproc[(with-intercepted-logging
           [interceptor (-> (vector/c
	                      (or/c 'fatal 'error 'warning 'info 'debug)
	                      string?
			      any/c)
			     any)]
           [proc (-> any)]
           [#:level level (or/c 'fatal 'error 'warning 'info 'debug) 'debug])
         any]{

Runs @racket[proc], calling @racket[interceptor] on any log message of level
@racket[level] or higher. @racket[interceptor] receives the entire log vectors
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
    #:level 'warning)
  warning-counter)]}


A lower-level interface to logging is also available.

@deftogether[[
  @defproc[(start-recording
            [#:level level (or/c 'fatal 'error 'warning 'info 'debug) 'debug])
           listener?]
  @defproc[(stop-recording [listener listener?])
           (listof (vector/c (or/c 'fatal 'error 'warning 'info 'debug)
                             string?
                             any/c))]]]{

@racket[start-recording] starts recording log messages of the desired level or
higher. Messages will be recorded until stopped by passing the returned
listener object to @racket[stop-recording]. @racket[stop-recording] will then
return a list of the log messages that have been reported.

@defexamples[
#:eval the-eval
(define l (start-recording #:level 'warning))
(log-warning "1")
(log-warning "2")
(stop-recording l)
]}
