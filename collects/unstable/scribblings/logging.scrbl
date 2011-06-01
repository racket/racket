#lang scribble/manual

@(require scribble/eval "utils.rkt" (for-label racket unstable/logging))

@title{Logging}

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
#:eval (eval/require 'unstable/logging)
(let ([my-log (open-output-string)])
  (with-logging-to-port my-log
    (lambda ()
      (log-warning "Warning World!")
      (+ 2 2))
    #:level 'warning)
  (display (get-output-string my-log)))
]

}
