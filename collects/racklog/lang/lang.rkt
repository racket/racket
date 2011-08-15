#lang racket/base
(require racket/dict
         racket/match
         racket/function
         datalog/ast)

(define (racklog-answers->literals t as)
  (for/list ([a (in-list as)]
             #:when a)
    (literal-subst a t)))

(define (literal-subst a t)
  (struct-copy literal t
               [terms (map (curry term-subst a)
                           (literal-terms t))]))

(define (term-subst a t)
  (match t
    [(? constant?) t]
    [(variable srcloc sym)
     (constant srcloc (dict-ref a sym))]))

(provide #%module-begin
         #%top
         #%top-interaction
         racklog-answers->literals)
