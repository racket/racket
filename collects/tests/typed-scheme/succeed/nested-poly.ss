#lang typed-scheme

(: f (All (A ...) (All (B ...) (A ... A -> Integer))))

(define (f . xs) 5)