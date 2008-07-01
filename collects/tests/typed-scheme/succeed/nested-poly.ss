#lang typed-scheme

(: f (All (A ...) (All (B ...) (A ... A -> Int))))

(define (f . xs) 5)