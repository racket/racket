#lang racket/base
(require "wrap.rkt")

(provide literal?
         unwrap-literal
         wrap-literal)

(define (literal? v)
  (define u (unwrap v))
  (or (number? u)
      (boolean? u)
      (eq? u 'eof)
      (and (pair? u)
           (let ([a (unwrap (car u))])
             (cond
               [(eq? a 'quote)
                (let ([u (unwrap (cadr u))])
                  (or (symbol? u)
                      (null? u)
                      (char? u)
                      (void? u)))]
               [(and (eq? a 'void)
                     (null? (cdr u)))
                #t]
               [else #f])))))

;; Unwrap a literal so that it can be serialized
;; or constant-folded
(define (unwrap-literal v)
  (define u (unwrap v))
  (cond
    [(pair? u)
     (let ([a (unwrap (car u))])
       (cond
         [(eq? a 'quote) (unwrap (cadr u))]
         [(eq? a 'void) (void)]))]
    [(eq? u 'eof) eof]
    [else u]))

(define (wrap-literal x)
  (cond
    [(or (string? x) (bytes? x) (boolean? x) (number? x))
     x]
    [(void? x) '(void)]
    [(eof-object? x) 'eof]
    [else
     `(quote ,x)]))
