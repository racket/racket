#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "import.rkt"
         "known.rkt"
         "find-known.rkt"
         "mutated-state.rkt"
         "literal.rkt")

(provide optimize)

;; Perform shallow optimizations. The `schemify` pass calls `optimize`
;; on each schemified form, which means that subexpressions of the
;; immediate expression have already been optimized.

(define (optimize v prim-knowns knowns imports mutated)
  (match v
    [`(if ,t ,e1 ,e2)
     (if (literal? t)
         (if (unwrap t) e1 e2)
         v)]
    [`(procedure? ,e)
     (define u (unwrap e))
     (cond
       [(symbol? u)
        (define k (find-known u prim-knowns knowns imports mutated))
        (if (known-procedure? k)
            '#t
            v)]
       [else v])]
    [`(procedure-arity-includes? ,e ,n)
     (define u (unwrap e))
     (define u-n (unwrap n))
     (cond
       [(and (symbol? u)
             (exact-integer? n))
        (define k (find-known u prim-knowns knowns imports mutated))
        (if (and (known-procedure? k)
                 (bitwise-bit-set? (known-procedure-arity-mask k) u-n))
            '#t
            v)]
       [else v])]
    [`,_
     (define u (unwrap v))
     (cond
       [(symbol? u)
        (define k (hash-ref-either knowns imports u))
        (cond
          [(and (known-literal? k)
                (simple-mutated-state? (hash-ref mutated u #f)))
           (known-literal-expr k)]
          ;; Note: we can't do `known-copy?` here, because a copy of
          ;; an imported or exported name will need to be schemified
          ;; to a different name
          [else v])]
       [else v])]))
