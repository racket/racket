#lang racket/base

(require "guts.ss")

(provide (rename-out [build-hash/c hash/c]))

(define (build-hash/c dom rng #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (unless (memq immutable '(#t #f dont-care))
    (error 'hash/c "expected #:immutable argument to be either #t, #f, or 'dont-care, got ~s" immutable))
  (let ([dom-ctc (if flat?
                     (coerce-flat-contract 'hash/c dom)
                     (coerce-contract 'hash/c dom))]
        [rng-ctc (if flat?
                     (coerce-flat-contract 'hash/c rng)
                     (coerce-contract 'hash/c rng))])
    (if (or flat?
            (and (eq? immutable #t)
                 (flat-contract? dom-ctc)
                 (flat-contract? rng-ctc)))
        (make-flat-hash/c dom-ctc rng-ctc immutable)
        (make-ho-hash/c dom-ctc rng-ctc immutable))))

(define (hash/c-first-order ctc) 
  (let ([dom-ctc (hash/c-dom ctc)]
        [rng-ctc (hash/c-rng ctc)]
        [immutable (hash/c-immutable ctc)]
        [flat? (flat-hash/c? ctc)])
    (λ (val #:blame [blame #f])
      (let/ec return
        (define (fail . args)
          (if blame
              (apply raise-blame-error blame val args)
              (return #f)))
        (unless (hash? val)
          (fail "expected a hash, got ~a" val))
        (case immutable
          [(#t) 
           (unless (immutable? val) 
             (fail "expected an immutable hash, got ~a" val))]
          [(#f)
           (when (immutable? val)
             (fail "expected an mutable hash, got ~a" val))]
          [(dont-care) (void)])
        (when (or flat? (immutable? val))
          (for ([(k v) (in-hash val)])
            (if blame
                (begin (((contract-projection dom-ctc) blame) k)
                       (((contract-projection rng-ctc) blame) v)
                       (void))
                (unless (and (contract-first-order-passes? dom-ctc k)
                             (contract-first-order-passes? rng-ctc v))
                  (fail)))))
        #t))))

(define (hash/c-name ctc)
  (apply 
   build-compound-type-name
   'hash/c (hash/c-dom ctc) (hash/c-rng ctc)
   (append
    (if (and (flat-hash/c? ctc)
             (not (eq? (hash/c-immutable ctc) #t)))
        (list '#:flat? #t)
        null)
    (case (hash/c-immutable ctc)
      [(dont-care) null]
      [(#t)
       (list '#:immutable #t)]
      [(#f)
       (list '#:immutable #f)]))))

(define-struct hash/c (dom rng immutable))

(define-struct (flat-hash/c hash/c) ()
  #:omit-define-syntaxes

  #:property prop:flat-contract
  (build-flat-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         ((hash/c-first-order ctc) val #:blame blame)
         val)))))

(define-struct (ho-hash/c hash/c) ()
  #:omit-define-syntaxes

  #:property prop:contract
  (build-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   
   #:projection
   (λ (ctc)
      (let ([dom-proc (contract-projection (hash/c-dom ctc))]
            [rng-proc (contract-projection (hash/c-rng ctc))]
            [immutable (hash/c-immutable ctc)])
        (λ (blame)
           (let ([pos-dom-proj (dom-proc blame)]
                 [neg-dom-proj (dom-proc (blame-swap blame))]
                 [pos-rng-proj (rng-proc blame)]
                 [neg-rng-proj (rng-proc (blame-swap blame))])
             (λ (val)
               ((hash/c-first-order ctc) val #:blame blame)
               
               (if (immutable? val)
                   (let ([hash-maker
                          (cond
                            [(hash-equal? val) make-immutable-hash]
                            [(hash-eqv? val) make-immutable-hasheqv]
                            [(hash-eq? val) make-immutable-hasheq])])
                     (hash-maker
                      (for/list ([(k v) (in-hash val)])
                        (cons (pos-dom-proj k)
                              (pos-rng-proj v)))))
                   (proxy-hash
                    val
                    (λ (h k)
                      (values (neg-dom-proj k)
                              (λ (h k v)
                                (pos-rng-proj v))))
                    (λ (h k v)
                      (values (neg-dom-proj k)
                              (neg-rng-proj v)))
                    (λ (h k)
                      (neg-dom-proj k))
                    (λ (h k)
                      (pos-dom-proj k)))))))))))
