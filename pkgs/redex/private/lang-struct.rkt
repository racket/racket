#lang racket/base

(provide (struct-out nt)
         (struct-out rhs)
         the-not-hole
         the-hole
         hole?)

;; lang = (listof nt)
;; nt = (make-nt sym (listof rhs))
;; rhs = (make-rhs single-pattern)
;; single-pattern = sexp
(define-struct nt (name rhs) #:transparent)
(define-struct rhs (pattern) #:transparent)
(define-values (the-hole the-not-hole hole?)
  (let ()
    (define-struct hole (id)
      #:property prop:equal+hash (list (λ (x y recur) #t) (λ (v recur) 255) (λ (v recur) 65535))
      #:inspector #f)
    (define the-hole (make-hole 'the-hole))
    (define the-not-hole (make-hole 'the-not-hole))
    (values the-hole the-not-hole hole?)))
