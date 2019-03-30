#lang racket/base
(require "../path/path.rkt"
         "../path/complete.rkt"
         "../path/parameter.rkt"
         "../path/cleanse.rkt"
         "../host/rktio.rkt"
         "../security/main.rkt")

(provide ->host
         ->host/as-is
         host->)

;; Note: `(host-> (->host x who flags))` is not the same as `x`, since
;; it normalizes `x`. That's why `(host-> (->host x))` is generally
;; used in error reporting.

(define (->host p who guards)
  (let ([p (->path p)])
    (when who
      (security-guard-check-file who p guards))
    (path-bytes (cleanse-path (path->complete-path p current-directory #:wrt-given? #f)))))

(define (->host/as-is p who src)
  (let ([p (->path p)])
    (when who
      (if src
          (security-guard-check-file-link who src p)
          (security-guard-check-file who p '(exists))))
    (path-bytes p)))

(define (host-> s)
  (path (bytes->immutable-bytes s)
        (system-path-convention-type)))
