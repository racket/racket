#lang typed/racket

(: f (All (A B ...) (A ... B -> (List (Boxof A) ... B))))
(define (f . args)
    (map (inst box A) args))

(: h (-> Nothing))
(define (h) (h))

(: g (All (A B ...) (A ... B -> (Values (Boxof A) ... B))))
(define (g . args)
   (h))


(ann ((inst f String Symbol Symbol) "c" "d") (List (Boxof String) (Boxof String)))
(lambda (x)
  (ann ((inst g String Symbol Symbol) "c" "d") (values (Boxof String) (Boxof String))))
