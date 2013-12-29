#lang typed/racket

(: g : Number -> Number)
(define (g x) (add1 x))

(provide g)

(module* main #f
  (g (assert (string->number 
	      (vector-ref (current-command-line-arguments) 0)))))

;; Test mode:
(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "1")])
    (dynamic-require (quote-module-path "..") #f)))
