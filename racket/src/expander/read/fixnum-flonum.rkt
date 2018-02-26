#lang racket/base
(require "config.rkt"
         "error.rkt"
         "whitespace.rkt"
         "location.rkt"
         "special.rkt"
         "symbol-or-number.rkt")

(provide read-fixnum
         read-flonum)

(define (read-fixnum read-one init-c in config)
  (define c (read-char/skip-whitespace-and-comments init-c read-one in config))
  (define-values (line col pos) (port-next-location* in c))
  (define v (read-number-literal c in config "#e"))
  (cond
   [(fixnum? v) v]
   [(eof-object? v) v]
   [else
    (reader-error in (reading-at config line col pos)
                  "expected a fixnum, found ~a"
                  v)]))

(define (read-flonum read-one init-c in config)
  (define c (read-char/skip-whitespace-and-comments init-c read-one in config))
  (define-values (line col pos) (port-next-location* in c))
  (define v (read-number-literal c in config "#i"))
  (cond
   [(flonum? v) v]
   [(eof-object? v) v]
   [else
    (reader-error in (reading-at config line col pos)
                  "expected a flonum, found ~a"
                  v)]))

;; ----------------------------------------

(define (read-number-literal c in config mode)
  (cond
   [(not (char? c)) c]
   [else
    (read-symbol-or-number c in config #:mode mode)]))
