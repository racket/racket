#lang racket/base
(require racket/fixnum
         "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "mutated-state.rkt")

(provide simple?
         simple/can-copy?)

;; Check whether an expression is simple in the sense that its order
;; of evaluation isn't detectable (`pure?` = #t) or at least it won't
;; try to capture a continuation (`pure?` = #f). In `pure?` mode, if
;; `no-alloc?` is true, then allocation counts as detectable (for
;; ordering with respect to functions that might capture a continuation).
;; If `ordered?` is true with `pure?` as true, then things that always
;; succeed with the same value are allowed, even if they may depend
;; on an earlier action not raising an exception.
;; If `succeeds?` is true with `pure?` and `ordered?` as true, then
;; things that always succeed are allowed, even if they aren't pure
;; (i.e., a later call might produce a different result).
;; This function receives both schemified and non-schemified expressions.
(define (simple? e prim-knowns knowns imports mutated simples unsafe-mode?
                 #:pure? [pure? #t]
                 #:no-alloc? [no-alloc? #f]
                 #:ordered? [ordered? #f] ; weakens `pure?` to allow some reordering
                 #:succeeds? [succeeds? #f] ; weakens `ordered?` to allow more reordering
                 #:result-arity [result-arity 1])
  (let simple? ([e e] [result-arity result-arity])
    (define-syntax-rule (cached expr)
      (let* ([c (hash-ref simples e #(0 0 1))]
             [bit (let ([AT (lambda (x) (fxlshift 1 x))])
                    (if pure?
                        (if no-alloc?
                            (if ordered? (if succeeds? (AT 0) (AT 1)) (AT 2))
                            (if ordered? (if succeeds? (AT 3) (AT 4)) (AT 5)))
                        (AT 6)))]
             [r (cond
                  [(fx= bit (fxand (vector-ref c 0) bit)) #t]
                  [(fx= bit (fxand (vector-ref c 1) bit)) #f]
                  [else 'unknown])]
             [arity-match? (eqv? result-arity (vector-ref c 2))])
        (if (or (eq? 'unknown r)
                (not arity-match?))
            (let ([r expr])
              (hash-set! simples e (vector (if r
                                               (fxior (vector-ref c 0) bit)
                                               (vector-ref c 0))
                                           (if r
                                               (vector-ref c 1)
                                               (fxior (vector-ref c 1) bit))
                                           (vector-ref c 2)))
              r)
            r)))
    (define (returns n)
      (or (not result-arity)
          (eqv? n result-arity)))
    (define (simple-begin? es)
      (cached
       (let loop ([es es])
         (cond
           [(null? (cdr es))
            (simple? (car es) result-arity)]
           [else
            (and (simple? (car es) #f)
                 (loop (cdr es)))]))))
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
       (simple-begin? es)]
      [`(begin-unsafe ,es ...)
       (simple-begin? es)]
      [`(begin0 ,e0 ,es ...)
       (cached
        (and (simple? e0 result-arity)
             (for/and ([e (in-list es)])
               (simple? e #f))))]
      [`(set! ,_ ,e)
       #:guard (not pure?)
       (simple? e 1)
       (returns 1)]
      [`(if ,tst ,thn ,els)
       (and (simple? tst 1)
            (simple? thn result-arity)
            (simple? els result-arity))]
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
                          (and (or (if no-alloc?
                                       (known-procedure/pure? v)
                                       (known-procedure/allocates? v))
                                   (and ordered?
                                        (or (known-procedure/then-pure? v)
                                            (and succeeds?
                                                 (null? args)
                                                 (known-procedure/parameter? v))
                                            ;; in unsafe mode, we can assume no contract error:
                                            (and unsafe-mode?
                                                 (known-field-accessor? v)
                                                 (known-field-accessor-authentic? v)
                                                 (known-field-accessor-known-immutable? v)))))
                               (returns 1))
                          (or (and (known-procedure/no-prompt? v)
                                   (returns 1))
                              (and succeeds?
                                   (null? args)
                                   (known-procedure/parameter? v)
                                   (returns 1))
                              (and (known-procedure/no-prompt/multi? v)
                                   (eqv? result-arity #f))
                              (and (known-field-accessor? v)
                                   (known-field-accessor-authentic? v)
                                   (returns 1))
                              (and (known-field-mutator? v)
                                   (known-field-mutator-authentic? v)
                                   (returns 1))))
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
