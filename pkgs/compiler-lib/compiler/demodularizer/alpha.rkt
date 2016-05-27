#lang racket/base

(require racket/match racket/contract compiler/zo-parse)

(define (alpha-vary-ctop top)
  (match top
    [(struct compilation-top (max-let-depth binding-namess prefix form))
     (make-compilation-top max-let-depth binding-namess (alpha-vary-prefix prefix) form)]))
(define (alpha-vary-prefix p)
  (struct-copy prefix p
               [toplevels
                (map (match-lambda
                       [(and sym (? symbol?))
                        (gensym sym)]
                       [other
                        other])
                     (prefix-toplevels p))]))

(provide/contract
 [alpha-vary-ctop (compilation-top? . -> . compilation-top?)])
