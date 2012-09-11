#lang racket/base

(require "mzc.rkt" unstable/logging)

(provide with-intercepted-opt-logging)

;; Intercepts both TR optimizer logging and mzc optimizer logging.
;; Interceptor accepts log-entry structs.
(define (with-intercepted-opt-logging interceptor thunk)
  (with-intercepted-logging
      (lambda (l)
        ;; From mzc, create a log-entry from the info.
        (interceptor (mzc-opt-log-message->log-entry (vector-ref l 1))))
    (lambda ()
      (with-intercepted-logging
          (lambda (l)
            ;; From TR, use the log-entry struct provided.
            (interceptor (vector-ref l 2)))
        thunk
        'debug 'TR-optimizer))
    'debug 'optimizer))
