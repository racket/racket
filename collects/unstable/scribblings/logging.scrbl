#lang scribble/manual

@(require scribble/eval "utils.rkt" (for-label racket unstable/logging))

@title{Logging}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/logging))

@defmodule[unstable/logging]

This module provides tools for logging.

@unstable[@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]]

@defproc[(with-logging-to-port
           [port output-port?] [proc (-> any)]
           [#:level level (or/c 'fatal 'error 'warning 'info 'debug) 'info])
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
           [#:level level (or/c 'fatal 'error 'warning 'info 'debug) 'info])
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
