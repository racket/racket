#lang racket/base

(require "guts.ss")

(provide hash/c)

(define (hash/c dom rng #:immutable [immutable 'dont-care])
  (unless (memq immutable '(#t #f dont-care))
    (error 'hash/c "expected #:immutable argument to be either #t, #f, or 'dont-care, got ~s" immutable))
  (cond
    [(eq? immutable #t) 
     (make-immutable-hash/c (coerce-contract 'hash/c dom) 
                            (coerce-contract 'hash/c rng))]
    [else
     (make-hash/c (coerce-flat-contract 'hash/c dom) 
                  (coerce-flat-contract 'hash/c rng)
                  immutable)]))

;; hash-test : hash/c -> any -> bool
(define (hash-test ctc)
  (let ([dom-proc (flat-contract-predicate (hash/c-dom ctc))]
        [rng-proc (flat-contract-predicate (hash/c-rng ctc))]
        [immutable (hash/c-immutable ctc)])
    (λ (val)
      (and (hash? val)
           (case immutable
             [(#t) (immutable? val)]
             [(#f) (not (immutable? val))]
             [(dont-care) #t])
           (let/ec k
             (hash-for-each
              val
              (λ (dom rng)
                (unless (dom-proc dom) (k #f))
                (unless (rng-proc rng) (k #f))))
             #t)))))

(define-struct hash/c (dom rng immutable)
  #:omit-define-syntaxes

  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order hash-test
   #:projection
   (λ (ctc)
      (let ([dom-proc (contract-projection (hash/c-dom ctc))]
            [rng-proc (contract-projection (hash/c-rng ctc))]
            [immutable (hash/c-immutable ctc)])
        (λ (blame)
           (let ([partial-dom-contract (dom-proc blame)]
                 [partial-rng-contract (rng-proc blame)])
             (λ (val)
                (unless (hash? val)
                  (raise-blame-error blame val "expected a hash, got ~a" val))
                (case immutable
                  [(#t) (unless (immutable? val)
                          (raise-blame-error blame val
                                             "expected an immutable hash, got ~a" val))]
                  [(#f) (when (immutable? val)
                          (raise-blame-error blame val
                                             "expected a mutable hash, got ~a" val))]
                  [(dont-care) (void)])
                
                (hash-for-each
                 val
                 (λ (key val)
                    (partial-dom-contract key)
                    (partial-rng-contract val)))
                
                val)))))

   #:name
   (λ (ctc) (apply 
             build-compound-type-name
             'hash/c (hash/c-dom ctc) (hash/c-rng ctc)
             (if (eq? 'dont-care (hash/c-immutable ctc))
               '()
               (list '#:immutable (hash/c-immutable ctc)))))))

(define-struct immutable-hash/c (dom rng)
  #:omit-define-syntaxes

  #:property prop:contract
  (build-contract-property
   #:first-order (λ (ctc) (λ (val) (and (hash? val) (immutable? val))))
   #:projection
   (λ (ctc)
      (let ([dom-proc (contract-projection (immutable-hash/c-dom ctc))]
            [rng-proc (contract-projection (immutable-hash/c-rng ctc))])
        (λ (blame)
           (let ([partial-dom-contract (dom-proc blame)]
                 [partial-rng-contract (rng-proc blame)])
             (λ (val)
                (unless (and (hash? val)
                             (immutable? val))
                  (raise-blame-error blame val
                                     "expected an immutable hash"))
                (make-immutable-hash
                 (hash-map
                  val
                  (λ (k v)
                     (cons (partial-dom-contract k)
                           (partial-rng-contract v))))))))))

   #:name
   (λ (ctc) (build-compound-type-name
             'hash/c (immutable-hash/c-dom ctc) (immutable-hash/c-rng ctc)
             '#:immutable #t))))