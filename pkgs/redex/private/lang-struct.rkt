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
    (define-struct hole ()
      #:property prop:equal+hash (list (λ (x y recur) #t) (λ (v recur) 255) (λ (v recur) 65535))
      #:inspector #f)
    (define-struct not-hole ()
      #:property prop:equal+hash (list (λ (x y recur) #t) (λ (v recur) 254) (λ (v recur) 65534))
      #:inspector #f)
    (define the-hole (make-hole))
    (define the-not-hole (make-not-hole))
    (define -hole?
      (let ([hole?
             (λ (x) (or (hole? x) (not-hole? x)))])
        hole?))
    (values the-hole the-not-hole -hole?)))
