#lang racket/base
(require "check.rkt"
         (submod "thread.rkt" scheduling)
         (prefix-in host: "host.rkt"))

(provide current-process-milliseconds
         set-get-subprocesses-time!)

(define/who (current-process-milliseconds [scope #f])
  (cond
    [(not scope) (host:current-process-milliseconds)]
    [(thread? scope) (thread-cpu-time scope)]
    [(eq? scope 'subprocesses) (get-subprocesses-time)]
    [else
     (raise-argument-error who "(or/c #f thread? 'subprocesses)" scope)]))

(define get-subprocesses-time (lambda () 0))

(define (set-get-subprocesses-time! f)
  (set! get-subprocesses-time f))
