#lang mzscheme

(require mzlib/unit)
(require "sig.rkt")

(provide compiler:option@)

(define-unit compiler:option@ (import) (export compiler:option^)

  (define somewhat-verbose (make-parameter #f))
  (define verbose (make-parameter #f))
  (define 3m (make-parameter (eq? '3m (system-type 'gc))))

  (define setup-prefix (make-parameter ""))

  (define compile-subcollections (make-parameter #t)))
