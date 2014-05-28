#lang racket/base

(require racket/bool
         racket/match
         redex/reduction-semantics
         redex/benchmark
         redex/examples/racket-machine/grammar
         redex/examples/racket-machine/verification
         redex/examples/racket-machine/randomized-tests
         (only-in redex/private/generate-term pick-an-index))

(provide (all-defined-out))

(module+ adhoc-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'grammar)
  (define (generate)
    (fix (generate-term bytecode e 5))))

(module+ enum-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'enum)
  (define (generate [p-value 0.03])
    (fix (generate-term bytecode e #:i-th (pick-an-index p-value)))))

(module+ ordered-mod
  (provide generate get-generator type)
  (define (get-generator)
    (let ([index 0])
      (λ () (begin0
              (generate index)
              (set! index (add1 index))))))
  (define type 'ordered)
  (define (generate [index 0])
    (fix (generate-term bytecode e #:i-th index))))

(module+ check-mod
  (provide check)
  (define (check e)
    (let/ec vf-not-total
      (or (not e)
          (implies (with-handlers ([exn:fail? (λ (exc)
                                                (maybe-log-exn exc e)
                                                (vf-not-total #f))])
                     (bytecode-ok? e))
                   (match 
                       (with-handlers
                           ([exn:fail? (λ (exc)
                                         (maybe-log-exn exc e)
                                         #f)])
                         (run e '() 100))
                     [(cutoff) #t]
                     [(answer _) #t]
                     [_ #f])))))
  
  (define (maybe-log-exn exc e)
    (if (regexp-match? #rx"counterexample|domain|clauses" (exn-message exc))
        (begin (bmark-log 'rvm-expected-exception
                          `(#:exn-message ,(exn-message exc) #:expression ,e))
               #f)
        (begin (bmark-log 'rvm-unexpected-exception
                          `(#:exn-message ,(exn-message exc) #:expression ,e))
               (raise exc)))))

