#lang racket/base

(require "mzc.rkt"
         typed-racket/optimizer/logging
         unstable/logging)

(provide with-intercepted-opt-logging)

;; Intercepts both TR optimizer logging and mzc optimizer logging.
;; Interceptor accepts log-entry structs.
(define (with-intercepted-opt-logging interceptor thunk)
  (with-intercepted-logging
      (lambda (l)
        (cond [(log-message-from-tr-opt? l)
               ;; From TR, use the log-entry struct provided.
               (interceptor (cdr (vector-ref l 2)))]
              ;; We look at the message to tell if it's from mzc.
              [(log-message-from-mzc-opt? (vector-ref l 1))
               ;; From mzc, create a log-entry from the info.
               (interceptor (mzc-opt-log-message->log-entry (vector-ref l 1)))]))
    thunk
    'debug 'optimizer 'debug 'TR-optimizer))
