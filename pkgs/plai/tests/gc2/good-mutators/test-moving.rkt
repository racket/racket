#lang plai/gc2/mutator
(allocator-setup "../good-collectors/trivial-moving-collector.rkt" 100)

;; just some random allocation here
;; this is really about testing 
;; things moving around in a moving collector

;; the 'let' is important as it means that the
;; closure is the only thing holding onto the '2'

(define f (cons 1 (let ([y 2]) (λ (x) (+ y x)))))
(define a ((rest f) 11))
(define b ((rest f) 22))

(define c
  ((let ([x (cons 1 2)])
     (λ (y)
       ((first y) (first x))))
   (cons (λ (z) z)
         5)))
