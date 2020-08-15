#lang racket/base
(require ffi/unsafe/custodian)

;; This module is run by "custodian-finalize.rkt"

(define (log-info x)
  (log-message (current-logger) 'info 'finalize x))

(define (finalize x)
  (log-info "finalizing"))

(define the-box
  (let ([v (string-copy "a value")])
    (register-finalizer-and-custodian-shutdown v finalize #:at-exit? #t)
    (log-info "created")
    (box v)))

(module* check-gc #f
  (set-box! the-box #f)
  (collect-garbage 'major)
  (sync (system-idle-evt))
  (log-info "exiting"))

(module* check-exit #f
  (log-info "exiting"))
