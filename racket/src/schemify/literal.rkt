#lang racket/base
(require "wrap.rkt")

(provide literal?
         unwrap-literal)

(define (literal? v)
  (define u (unwrap v))
  (or (number? u)
      (boolean? u)
      (and (pair? u)
           (eq? (unwrap (car u)) 'quote)
           (let ([u (unwrap (wrap-car (cdr u)))])
             (or (symbol? u)
                 (null? u))))))

;; Unwrap a literal so that it can be serialized
(define (unwrap-literal v)
  (define u (unwrap v))
  (if (pair? u)
      `',(unwrap (wrap-car (cdr u)))
      u))
