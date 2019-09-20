#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "known.rkt"
         "import.rkt"
         "mutated-state.rkt")

(provide ensure-single-valued)

(define (ensure-single-valued v knowns prim-knowns imports mutated)
  (match v
    [`(quote ,_) v]
    [`(lambda . ,_) v]
    [`(case-lambda . ,_) v]
    [`(,proc-or-form . ,_)
     (define u (unwrap proc-or-form))
     (cond
       [(and (symbol? u)
             (simple-mutated-state? (hash-ref mutated u #f))
             (let ([k (or (hash-ref prim-knowns u #f)
                          (hash-ref-either knowns imports u))])
               (known-procedure/pure? k)))
        v]
       [else `($value ,v)])]
    [`,_ v]))
