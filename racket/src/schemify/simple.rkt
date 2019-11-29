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
(define (simple? e prim-knowns knowns imports mutated simples
                 #:pure? [pure? #t]
                 #:result-arity [result-arity 1])
  (let simple? ([e e] [result-arity result-arity])
    (define-syntax-rule (cached expr)
      (let* ([c (hash-ref simples e #(unknown unknown 1))]
             [r (vector-ref c (if pure? 0 1))]
             [arity-match? (eqv? result-arity (vector-ref c 2))])
        (if (or (eq? 'unknown r)
                (not arity-match?))
            (let ([r expr])
              (hash-set! simples e (if pure?
                                       (vector r
                                               (if arity-match? (vector-ref c 1) 'unknown)
                                               result-arity)
                                       (vector (if arity-match? (vector-ref c 0) 'unknown)
                                               r
                                               result-arity)))
              r)
            r)))
    (define (returns n)
      (or (not result-arity)
          (eqv? n result-arity)))
    (match e
      [`(lambda . ,_) (returns 1)]
      [`(case-lambda . ,_) (returns 1)]
      [`(quote . ,_) (returns 1)]
      [`(#%variable-reference . ,_) (returns 1)]
      [`(let-values ([,idss ,rhss] ...) ,body)
       (cached
        (and (for/and ([ids (in-list idss)]
                       [rhs (in-list rhss)])
               (simple? rhs (length ids)))
             (simple? body result-arity)))]
      [`(let ([,_ ,rhss] ...) ,body)
       (cached
        (and (for/and ([rhs (in-list rhss)])
               (simple? rhs 1))
             (simple? body result-arity)))]
      [`(letrec-values ([(,idss ...) ,rhss] ...) ,body)
       (cached
        (and (for/and ([ids (in-list idss)]
                       [rhs (in-list rhss)])
               (simple? rhs (length ids)))
             (simple? body result-arity)))]
      [`(letrec* ([,ids ,rhss] ...) ,body)
       (cached
        (and (for/and ([rhs (in-list rhss)])
               (simple? rhs 1))
             (simple? body result-arity)))]
      [`(begin ,es ...)
       #:guard (not pure?)
       (cached
        (let loop ([es es])
          (cond
            [(null? (cdr es))
             (simple? (car es) result-arity)]
            [else
             (and (simple? (car es) #f)
                  (loop (cdr es)))])))]
      [`(begin0 ,e0 ,es ...)
       (cached
        (and (simple? e0 result-arity)
             (for/and ([e (in-list es)])
               (simple? e #f))))]
      [`(set! ,_ ,e)
       #:guard (not pure?)
       (simple? e 1)
       (returns 1)]
      [`(values ,es ...)
       (cached
        (and (returns (length es))
             (for/and ([e (in-list es)])
               (simple? e 1))))]
      [`(,proc . ,args)
       (cached
        (let ([proc (unwrap proc)])
          (and (symbol? proc)
               (let ([v (or (hash-ref-either knowns imports proc)
                            (hash-ref prim-knowns proc #f))])
                 (and (if pure?
                          (and (known-procedure/pure? v)
                               (returns 1))
                          (and (known-procedure/no-prompt? v)
                               (eqv? result-arity #f)))
                      (bitwise-bit-set? (known-procedure-arity-mask v) (length args))))
               (simple-mutated-state? (hash-ref mutated proc #f))
               (for/and ([arg (in-list args)])
                 (simple? arg 1)))))]
      [`,_
       (let ([e (unwrap e)])
         (and (returns 1)
              (or (and (symbol? e)
                       (simple-mutated-state? (hash-ref mutated e #f)))
                  (integer? e)
                  (boolean? e)
                  (string? e)
                  (bytes? e)
                  (regexp? e))))])))

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
