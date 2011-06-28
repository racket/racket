#lang racket

(require "common.rkt" redex)
(provide (all-defined-out))

(define (num n)
  (if (zero? n)
      `("0")
      `("S" ,(num (sub1 n)))))

(define (lst l)
  (if (empty? l)
      `("nil")
      `("cons" ,(first l) ,(lst (rest l)))))

(define (:let name named-expr body)
  `((λ (,name) ,body)
    ,named-expr))

(define =-impl
  `(λ (x y)
     (match x
       [("0") 
        (match y
          [("0") ("#t")]
          [("S" yn) ("#f")])]
       [("S" xn)
        (match y
          [("0") ("#f")]
          [("S" yn) ((ref =) xn yn)])])))

(define +-impl
  `(λ (x y)
     (match x
       [("0") y]
       [("S" xn)
        ,(:let 'in '((ref +) xn y)
               '("S" in))])))

(define --impl
  `(λ (n m)
     (match n
       [("0") n]
       [("S" k) 
        (match m
          [("0") n]
          [("S" l) ((ref -) k l)])])))

(define *-impl
  `(λ (n m)
     (match n
       [("0") ("0")]
       [("S" p) ,(:let 'tmp '((ref *) p m)
                       '((ref +) m tmp))])))

(define if-impl
  `(λ (cond true false)
     (match cond
       [("#t") (true)]
       [("#f") (false)])))

(define (with-arith e)
  `(letrec
       ([(ref =) ,=-impl]
        [(ref +) ,+-impl]
        [(ref -) ,--impl]
        [(ref *) ,*-impl]
        [(ref if) ,if-impl])
     ,e))

(define arith-store
  (term
   (make-store [= ,=-impl] 
               [+ ,+-impl]
               [- ,--impl]
               [* ,*-impl]
               [if ,if-impl])))

(define (:if cond true false)
  (:let 'cond-val cond
        `((ref if)
          cond-val
          (λ () ,true)
          (λ () ,false))))
