#lang racket/base

;; Generative structure definitions:
(define-struct dt (pred-stx variants) #:mutable)
(define-struct vt (name-stx predicate-stx accessor-stx field-count) #:mutable)

;; Helper function:
(define (variant-assq name-stx variants)
  (let loop ([l variants])
    (if (free-identifier=? name-stx (vt-name-stx (car l)))
        (car l)
        (loop (cdr l)))))

(provide (struct-out dt)
         (struct-out vt)
         variant-assq)
