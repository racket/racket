#lang scheme

(provide intersperse)

(define (intersperse seperator xs)
    (cond
      [(null? xs) '()]
      [(null? (cdr xs)) xs]
      [else (cons (car xs)
                  (cons seperator
                        (intersperse seperator (cdr xs))))]))