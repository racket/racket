#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info)
         racket/list
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt")

(provide struct/c)

(define-syntax (struct/c stx)
  (syntax-case stx ()
    [(_ . args) 
     (with-syntax ([x (syntax/loc stx (do-struct/c . args))])
       (syntax/loc stx (#%expression x)))]))

;; name is symbol
;; predicate is (-> any bool)
;; immutables is (listof (list natural contract selector-proc))
;; mutables is (listof (list natural contract selector-proc mutator-proc))
(define-struct base-struct/c (name predicate immutables mutables))

(define (struct/c-name ctc)
  (let ([ctcs (map second
                   (sort (append (base-struct/c-immutables ctc) (base-struct/c-mutables ctc))
                         < #:key first))])
    (apply build-compound-type-name 'struct/c (base-struct/c-name ctc) ctcs)))

(define (check-struct/c ctc)
  (let ([name (base-struct/c-name ctc)]
        [pred? (base-struct/c-predicate ctc)]
        [ctc/ref-pairs (map (λ (l) (cons (second l) (third l)))
                            (append (base-struct/c-immutables ctc) (base-struct/c-mutables ctc)))])
    (λ (val fail [first-order? #f])
      (unless (pred? val)
        (fail "expected: ~s, got ~e" name val))
      (when first-order?
        (for ([p (in-list ctc/ref-pairs)])
          (let ([c (car p)] [v ((cdr p) val)])
            (unless (contract-first-order-passes? c v)
              (fail "expected: ~s, got ~e" (contract-name c) v)))))
      #t)))

(define (struct/c-first-order ctc)
  (let ([f (check-struct/c ctc)])
    (λ (val)
      (let/ec fail
        (f val (λ args (fail #f)) #t)))))

(define (flat-struct/c-proj ctc)
  (let ([checker (check-struct/c ctc)]
        [name (base-struct/c-name ctc)]
        [pred (base-struct/c-predicate ctc)]
        [projs (map contract-projection (map second (base-struct/c-immutables ctc)))]
        [refs (map third (base-struct/c-immutables ctc))])
    (λ (blame)
      (let ([pos-projs (map (λ (f) (f blame)) projs)])
        (λ (val)
          (checker val (λ args (apply raise-blame-error blame val args)))
          (for ([p (in-list pos-projs)] [ref (in-list refs)])
            (p (ref val)))
          val)))))

(define-struct (flat-struct/c base-struct/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name struct/c-name
   #:first-order struct/c-first-order
   #:projection flat-struct/c-proj))

(define (chaperone-struct/c-proj ctc)
  (let-values ([(flat-imms chap-imms)
                (partition (λ (l) (flat-contract? (second l))) (base-struct/c-immutables ctc))])
    (let ([checker (check-struct/c ctc)]
          [name (base-struct/c-name ctc)]
          [pred (base-struct/c-predicate ctc)]
          [flat-imm-projs (map (compose contract-projection second) flat-imms)]
          [flat-imm-refs (map third flat-imms)]
          [chap-imm-projs (map (compose contract-projection second) chap-imms)]
          [chap-imm-refs (map third chap-imms)]
          [mut-projs (map (compose contract-projection second) (base-struct/c-mutables ctc))]
          [mut-refs (map third (base-struct/c-mutables ctc))]
          [mut-sets (map fourth (base-struct/c-mutables ctc))])
      (λ (blame)
        (let* ([swapped-blame (blame-swap blame)]
               [flat-imm-pos-projs (map (λ (f) (f blame)) flat-imm-projs)]
               [chap-imm-pos-projs (map (λ (f) (f blame)) chap-imm-projs)]
               [mut-pos-projs (map (λ (f) (f blame)) mut-projs)]
               [mut-neg-projs (map (λ (f) (f swapped-blame)) mut-projs)])
          (λ (val)
            (checker val (λ args (apply raise-blame-error blame val args)))
            (for ([p (in-list flat-imm-pos-projs)]
                  [ref (in-list flat-imm-refs)])
              (p (ref val)))
            ;; While gathering up the selectors and the appropriate projections,
            ;; we go ahead and apply the projection to check the first order properties.
            (let ([combined-imm-refs 
                   (for/list ([p (in-list chap-imm-pos-projs)]
                              [ref (in-list chap-imm-refs)])
                     (p (ref val))
                     (list ref (λ (s v) (p v))))]
                  [combined-mut-refs
                   (for/list ([p (in-list mut-pos-projs)]
                              [ref (in-list mut-refs)])
                     (p (ref val))
                     (list ref (λ (s v) (p v))))]
                  [combined-mut-sets
                   (for/list ([p (in-list mut-neg-projs)]
                              [set (in-list mut-sets)])
                     (list set (λ (s v) (p v))))])
              (apply chaperone-struct val
                     (flatten (list combined-imm-refs combined-mut-refs combined-mut-sets
                                    impersonator-prop:contracted ctc))))))))))

(define-struct (chaperone-struct/c base-struct/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name struct/c-name
   #:first-order struct/c-first-order
   #:projection chaperone-struct/c-proj))

(define (impersonator-struct/c-proj ctc)
  (let-values ([(flat-imms chap-imms)
                (partition (λ (l) (flat-contract? (second l))) (base-struct/c-immutables ctc))])
    (let ([checker (check-struct/c ctc)]
          [name (base-struct/c-name ctc)]
          [pred (base-struct/c-predicate ctc)]
          [flat-imm-projs (map (compose contract-projection second) flat-imms)]
          [flat-imm-refs (map third flat-imms)]
          [chap-imm-projs (map (compose contract-projection second) chap-imms)]
          [chap-imm-refs (map third chap-imms)]
          [mut-projs (map (compose contract-projection second) (base-struct/c-mutables ctc))]
          [mut-refs (map third (base-struct/c-mutables ctc))]
          [mut-sets (map fourth (base-struct/c-mutables ctc))])
      (λ (blame)
        (let* ([swapped-blame (blame-swap blame)]
               [flat-imm-pos-projs (map (λ (f) (f blame)) flat-imm-projs)]
               [chap-imm-pos-projs (map (λ (f) (f blame)) chap-imm-projs)]
               [mut-pos-projs (map (λ (f) (f blame)) mut-projs)]
               [mut-neg-projs (map (λ (f) (f swapped-blame)) mut-projs)])
          (λ (val)
            (checker val (λ args (apply raise-blame-error blame val args)))
            (for ([p (in-list flat-imm-pos-projs)]
                  [ref (in-list flat-imm-refs)])
              (p (ref val)))
            ;; While gathering up the selectors and the appropriate projections,
            ;; we go ahead and apply the projection to check the first order properties.
            (let ([combined-imm-refs 
                   (for/list ([p (in-list chap-imm-pos-projs)]
                              [ref (in-list chap-imm-refs)])
                     (p (ref val))
                     (list ref (λ (s v) (p v))))]
                  [combined-mut-refs
                   (for/list ([p (in-list mut-pos-projs)]
                              [ref (in-list mut-refs)])
                     (p (ref val))
                     (list ref (λ (s v) (p v))))]
                  [combined-mut-sets
                   (for/list ([p (in-list mut-neg-projs)]
                              [set (in-list mut-sets)])
                     (list set (λ (s v) (p v))))])
              (apply impersonate-struct
                     (apply chaperone-struct val
                            combined-imm-refs)
                     (flatten (list combined-mut-refs combined-mut-sets
                                    impersonator-prop:contracted ctc))))))))))

(define-struct (impersonator-struct/c base-struct/c) ()
  #:property prop:contract
  (build-contract-property
   #:name struct/c-name
   #:first-order struct/c-first-order
   #:projection impersonator-struct/c-proj))

(define-syntax (do-struct/c stx)
  (syntax-case stx ()
    [(_ struct-name args ...)
     (and (identifier? (syntax struct-name))
          (struct-info? (syntax-local-value (syntax struct-name) (λ () #f))))
     (let* ([si (extract-struct-info (syntax-local-value (syntax struct-name)))]
            [predicate-id (third si)]
            [selector-ids (reverse (fourth si))]
            [mutator-ids (reverse (fifth si))]
            [ctcs (syntax->list #'(args ...))]
            [ctc-names (generate-temporaries #'(args ...))])
       (unless (= (length selector-ids) (length ctcs))
         (raise-syntax-error 'struct/c 
                             (format "expected ~a contracts because struct ~a has ~a fields"
                                     (length selector-ids)
                                     (syntax-e #'struct-name)
                                     (length selector-ids))
                             stx))
       (unless predicate-id
         (raise-syntax-error 'struct/c 
                             (format "could not determine predicate for ~s" (syntax-e #'struct-name))
                             stx))
       (unless (andmap values selector-ids)
         (raise-syntax-error 'struct/c
                             (format "could not determine selectors for ~s" (syntax-e #'struct-name))
                             stx))
       
       (let ([combined-ids (for/list ([n (in-naturals)]
                                      [ctc-name (in-list ctc-names)]
                                      [ref-name (in-list selector-ids)]
                                      [mut-name (in-list mutator-ids)])
                             (list n ctc-name ref-name mut-name))])
         (let-values ([(mutables immutables) (partition (λ (l) (fourth l)) combined-ids)])
           (with-syntax ([(ctc-x ...) ctc-names]
                         [predicate-id predicate-id]
                         [((imm-count imm-ctc-x imm-ref _) ...) immutables]
                         [((mut-count mut-ctc-x mut-ref mut-set) ...) mutables])
             (syntax
              (let ([ctc-x (coerce-contract 'struct/c args)] ...)
                (let ([immutables (list (list imm-count imm-ctc-x imm-ref) ...)]
                      [mutables (list (list mut-count mut-ctc-x mut-ref mut-set) ...)])
                  (struct/c/proc 'struct-name predicate-id immutables mutables))))))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))

(define (struct/c/proc struct-name predicate immutables mutables)
  (for ([lst (in-list immutables)])
    (define imm-count (list-ref lst 0))
    (define imm-ctc (list-ref lst 1))
    (unless (chaperone-contract? imm-ctc)
      (error 'struct/c "expected a chaperone contract for immutable field ~v (counting from 0), got ~e"
             imm-count imm-ctc)))
  (cond
    [(and (null? mutables) (andmap (λ (l) (flat-contract? (second l))) immutables))
     (make-flat-struct/c struct-name predicate immutables mutables)]
    [(andmap (λ (l) (chaperone-contract? (second l))) mutables)
     (make-chaperone-struct/c struct-name predicate immutables mutables)]
    [else
     (make-impersonator-struct/c struct-name predicate immutables mutables)]))
