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
                 #:pure? [pure? #t])
  (let simple? ([e e])
    (define-syntax-rule (cached expr)
      (let* ([c (hash-ref simples e '(unknown . unknown))]
             [r (if pure? (car c) (cdr c))])
        (if (eq? 'unknown r)
            (let ([r expr])
              (hash-set! simples e (if pure? (cons r (cdr c)) (cons (car c) r)))
              r)
            r)))
    (match e
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`(quote . ,_) #t]
      [`(#%variable-reference . ,_) #t]
      [`(let-values ([,_ ,rhss] ...) ,body)
       (cached
        (and (for/and ([rhs (in-list rhss)])
               (simple? rhs))
             (simple? body)))]
      [`(let ([,_ ,rhss] ...) ,body)
       (cached
        (and (for/and ([rhs (in-list rhss)])
               (simple? rhs))
             (simple? body)))]
      [`(letrec-values ([(,idss ...) ,rhss] ...) ,body)
       (cached
        (and (for/and ([rhs (in-list rhss)])
               (simple? rhs))
             (simple? body)))]
      [`(letrec* ([,ids ,rhss] ...) ,body)
       (cached
        (and (for/and ([rhs (in-list rhss)])
               (simple? rhs))
             (simple? body)))]
      [`(begin ,es ...)
       #:guard (not pure?)
       (cached
        (for/and ([e (in-list es)])
          (simple? e)))]
      [`(set! ,_ ,e)
       #:guard (not pure?)
       (simple? e)]
      [`(values ,es ...)
       #:guard (not pure?)
       (cached
        (for/and ([e (in-list es)])
          (simple? e)))]
      [`(,proc . ,args)
       (cached
        (let ([proc (unwrap proc)])
          (and (symbol? proc)
               (let ([v (or (hash-ref-either knowns imports proc)
                            (hash-ref prim-knowns proc #f))])
                 (and (if pure?
                          (known-procedure/pure? v)
                          (known-procedure/no-prompt? v))
                      (bitwise-bit-set? (known-procedure-arity-mask v) (length args))))
               (simple-mutated-state? (hash-ref mutated proc #f))
               (for/and ([arg (in-list args)])
                 (simple? arg)))))]
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
