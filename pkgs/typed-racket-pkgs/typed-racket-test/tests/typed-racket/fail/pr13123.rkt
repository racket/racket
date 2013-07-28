#lang typed/racket

(: unsound-cast (All (a b) (a b -> a)))
(define (unsound-cast w r)
  (let: ([x : a w])
    (: set-x (All (a)
                  (a -> Void)))
    (define (set-x y)
      (set! x y))
    (set-x r)
    x))

(: wrong Integer)
(define wrong (unsound-cast 42 'oops))
