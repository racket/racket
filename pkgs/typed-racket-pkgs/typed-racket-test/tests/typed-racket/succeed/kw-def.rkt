#lang typed/racket

(: f (case-> 
      (Integer [#:k Integer] -> Integer)
      (Integer String [#:k Integer] -> Integer)))
(define f
  (lambda (x [z 2] #:k [y 1]) (+ x y)))

(: f2 (case-> 
       (Integer [#:k Integer] -> Integer)
       (Integer String [#:k Integer] -> Integer)))
(define (f2 x [z 2] #:k [y 1]) (+ x y))

(f 0)
(f 0 "s")
(f 0 #:k 1)
(f 0 "s" #:k 1)
(f 0 #:k 1 "s")

(f2 0)
(f2 0 "s")
(f2 0 #:k 1)
(f2 0 "s" #:k 1)
(f2 0 #:k 1 "s")

(: g (Integer #:k Integer -> Integer))
(define g
  (lambda (x #:k y) (+ x y)))

(: g2 (Integer #:k Integer -> Integer))
(define (g2 x #:k y) (+ x y))

(g 0 #:k 1)
(g2 0 #:k 1)

;; Additional keyword function tests
;; FIXME: These really belong in the unit tests, but for some reason
;;        the unit tests don't work well with keywords.
(: f0:a (#:a String -> (List String)))
(define (f0:a #:a a) (list a))

(: f1:a (Symbol #:a String -> (List Symbol String)))
(define (f1:a x #:a a) (list x a))

(: f1:a? (Symbol [#:a String] -> (List Symbol String)))
(define (f1:a? x #:a [a "a"]) (list x a))

(: f1+:a (String #:a String String * -> (Listof String)))
(define (f1+:a x #:a a . args) (cons x (cons a args)))

(: f1+:a? (String [#:a String] String * -> (Listof String)))
(define (f1+:a? x #:a [a "a"] . args) (cons x (cons a args)))

(: f0:a:b (#:a String #:b Symbol -> (List String Symbol)))
(define (f0:a:b #:a a #:b b) (list a b))

(: f0:a?:b ([#:a String] #:b Symbol -> (List String Symbol)))
(define (f0:a?:b #:a [a "a"] #:b b) (list a b))

(: f1:a:b (String #:a String #:b Symbol -> (List String String Symbol)))
(define (f1:a:b x #:a a #:b b) (list x a b))

(: f1:a?:b (String [#:a String] #:b Symbol -> (List String String Symbol)))
(define (f1:a?:b x #:a [a "a"] #:b b) (list x a b))

(: f1+:a:b (String #:a String #:b String String * -> (Listof String)))
(define (f1+:a:b x #:a a #:b b . args) (cons x (cons a (cons b args))))

(: f0:a:b? (#:a String [#:b Symbol] -> (List String Symbol)))
(define (f0:a:b? #:a a #:b [b 'b]) (list a b))

(: f0:a?:b? ([#:a String] [#:b Symbol] -> (List String Symbol)))
(define (f0:a?:b? #:a [a "a"] #:b [b 'b]) (list a b))

(: f1:a:b? (String #:a String [#:b Symbol] -> (List String String Symbol)))
(define (f1:a:b? x #:a a #:b [b 'b]) (list x a b))

(: f1:a?:b? (String [#:a String] [#:b Symbol] -> (List String String Symbol)))
(define (f1:a?:b? x #:a [a "a"] #:b [b 'b]) (list x a b))

(: f1+:a:b? (String #:a String [#:b String] String * -> (Listof String)))
(define (f1+:a:b? x #:a a #:b [b "b"] . args)
  (cons x (cons a (cons b args))))

(: f1+:a?:b? (String [#:a String] [#:b String] String * -> (Listof String)))
(define (f1+:a?:b? x #:a [a "a"] #:b [b "b"] . args)
  (cons x (cons a (cons b args))))
