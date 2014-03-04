#lang typed/racket

;; Test forward references to definitions that occur later in
;; the file (with an annotation) but are referred to from a
;; definition earlier in the file

(define (f x) (g x))

(: g (-> Any String))
(define (g x) "hello world")

;;;

(define c%
  (class object%
    (super-new)
    (define/public (m x)
      (g1 x))))

(: g1 (-> Any String))
(define (g1 x) "hello world")

;;;

(define (f2 x) (h2 "foo"))
(define (g2 x) (h2 "bar"))

(: h2 (-> String String))
(define (h2 x) (string-append x "baz"))

;;; PR 11544

(define (some/function x) constant)
(: constant Any)
(define constant 1)
