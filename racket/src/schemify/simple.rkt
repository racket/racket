#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "mutated-state.rkt")

(provide simple?
         simple/can-copy?)

;; Check whether an expression is simple in the sense that its order
;; of evaluation isn't detectable. This function receives both
;; schemified and non-schemified expressions.
(define (simple? e prim-knowns knowns imports mutated
                 #:pure? [pure? #t])
  (let simple? ([e e])
    (match e
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`(quote . ,_) #t]
      [`(#%variable-reference . ,_) #t]
      [`(let-values ([,_ ,rhss] ...) ,body)
       (and (for/and ([rhs (in-list rhss)])
              (simple? rhs))
            (simple? body))]
      [`(let ([,_ ,rhss] ...) ,body)
       (and (for/and ([rhs (in-list rhss)])
              (simple? rhs))
            (simple? body))]
      [`(letrec-values ([(,idss ...) ,rhss] ...) ,body)
       (and (for/and ([rhs (in-list rhss)])
              (simple? rhs))
            (simple? body))]
      [`(letrec* ([,ids ,rhss] ...) ,body)
       (and (for/and ([rhs (in-list rhss)])
              (simple? rhs))
            (simple? body))]
      [`(begin ,es ...)
       #:guard (not pure?)
       (for/and ([e (in-list es)])
         (simple? e))]
      [`(,proc . ,args)
       (let ([proc (unwrap proc)])
         (and (symbol? proc)
              (let ([v (or (hash-ref-either knowns imports proc)
                           (hash-ref prim-knowns proc #f))])
                (and (if pure?
                         (known-procedure/pure? v)
                         (known-procedure/succeeds? v))
                     (bitwise-bit-set? (known-procedure-arity-mask v) (length args))))
              (simple-mutated-state? (hash-ref mutated proc #f))
              (for/and ([arg (in-list args)])
                (simple? arg))))]
      [`,_
       (let ([e (unwrap e)])
         (or (and (symbol? e)
                  (simple-mutated-state? (hash-ref mutated e #f)))
             (integer? e)
             (boolean? e)
             (string? e)
             (bytes? e)
             (regexp? e)))])))

(define (simple/can-copy? e prim-knowns knowns imports mutated)
  (match e
    [`(quote ,v) (can-copy-literal? v)]
    [`(,_ . ,_) #f]
    [`,_
     (let ([e (unwrap e)])
       (or (and (symbol? e)
                (simple-mutated-state? (hash-ref mutated e #f)))
           (can-copy-literal? e)))]))

(define (can-copy-literal? e)
  (or (integer? e)
      (boolean? e)
      (symbol? e)))
