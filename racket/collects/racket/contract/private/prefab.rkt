#lang racket/base

;; This module implements contracts for prefab structure types

(provide prefab/c)

(require racket/list
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt")

(define (prefab/c key . *field-ctcs)
  (define field-ctcs (coerce-contracts 'prefab/c *field-ctcs))
  (unless (prefab-key? key)
    (raise-argument-error 'prefab/c "prefab-key?" key))
  (define (handle-key-inconsistency exn)
    (raise-arguments-error 'prefab/c
                           "mismatch between prefab key and field count"
                           "prefab key" key
                           "field count" (length *field-ctcs)))
  (define struct-type
    (with-handlers ([exn:fail:contract? handle-key-inconsistency])
      (prefab-key->struct-type key (length *field-ctcs))))
  (define mutable?
    (and (pair? key) (ormap vector? key)))
  (check-mutable-impersonator-consistency struct-type field-ctcs)
  (cond [(and (andmap flat-contract? field-ctcs)
              (not mutable?))
         (flat-prefab/c key struct-type field-ctcs)]
        [(andmap chaperone-contract? field-ctcs)
         (chaperone-prefab/c key struct-type field-ctcs)]
        [else
         (impersonator-prefab/c key struct-type field-ctcs)]))

(define (prefab/c-name ctc)
  (apply build-compound-type-name
         'prefab/c
         (base-prefab/c-key ctc)
         (base-prefab/c-ctcs ctc)))

(define ((prefab/c-proj proxy) ctc)
  (define instance?
    (struct-type-make-predicate (base-prefab/c-struct-type ctc)))
  (define ho-projs
    (for/list ([ctc (in-list (base-prefab/c-ctcs ctc))])
      (contract-projection ctc)))
  (define-values (accessors mutators)
    (compute-accessors/mutators (base-prefab/c-struct-type ctc)))
  (λ (blame)
    (define-values (pos-projs neg-projs)
      (for/lists (_1 _2) ([proj (in-list ho-projs)])
        (values (λ (this v) ((proj blame) v))
                (λ (this v) ((proj (blame-swap blame)) v)))))
    (define accessor/proj
      (for/fold ([pairs null])
                ([accessor (in-list accessors)]
                 [proj (in-list pos-projs)])
        (cons accessor (cons proj pairs))))
    (define mutator/proj
      (for/fold ([pairs null])
                ([mutator (in-list mutators)]
                 [proj (in-list neg-projs)]
                 #:when mutator)
        (cons mutator (cons proj pairs))))
    (λ (val)
      (unless (contract-first-order-passes? ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s" given: "~e")
         (contract-name ctc)
         val))
      (apply proxy val
             (append accessor/proj
                     mutator/proj
                     (list impersonator-prop:contracted ctc
                           impersonator-prop:blame blame))))))

(define ((prefab/c-val-first-proj proxy) ctc)
  (define instance?
    (struct-type-make-predicate (base-prefab/c-struct-type ctc)))
  (define ho-projs
    (map get/build-val-first-projection (base-prefab/c-ctcs ctc)))
  (define-values (accessors mutators)
    (compute-accessors/mutators (base-prefab/c-struct-type ctc)))
  (λ (blame)
    (define-values (pos-projs neg-projs)
      (for/lists (_1 _2) ([proj (in-list ho-projs)])
        (values
         (λ (neg-party)
           (λ (pr v) (((proj blame) v) neg-party)))
         (λ (neg-party)
           (λ (pr v) (((proj (blame-swap blame)) v) neg-party))))))
    (λ (val)
      (cond
        [(contract-first-order-passes? ctc val)
         (λ (neg-party)
           (define accessor/proj
             (for/fold ([pairs null])
                       ([accessor (in-list accessors)]
                        [proj (in-list pos-projs)])
               (cons accessor (cons (proj neg-party) pairs))))
           (define mutator/proj
             (for/fold ([pairs null])
                       ([mutator (in-list mutators)]
                        [proj (in-list neg-projs)]
                        #:when mutator)
               (cons mutator (cons (proj neg-party) pairs))))
           (apply proxy val
                  (append accessor/proj
                          mutator/proj
                          (list impersonator-prop:contracted ctc
                                impersonator-prop:blame blame))))]
        [else
         (λ (neg-party)
           (raise-blame-error
            blame #:missing-party neg-party
            val '(expected: "~s" given: "~e")
            (contract-name ctc)
            val))]))))

;; Struct-Type -> (Listof Procedure) (Listof (U #f Procedure))
;; Given a struct type for a prefab, get all of the accessor
;; and mutator functions in the struct hierarchy in order
;; (parent first). Fields that are not mutable have #f instead of
;; a mutator.
(define (compute-accessors/mutators struct-type)
  (define-values (_1 init auto ref set immutable parent _6)
    (struct-type-info struct-type))
  (define field-count (+ init auto))
  (define-values (parent-accessors parent-mutators)
    (if parent
        (compute-accessors/mutators parent)
        (values null null)))
  (define-values (base-accessors base-mutators)
    (for/lists (_1 _2) ([idx (in-range init)])
      (values (make-struct-field-accessor ref idx)
              (and (not (member idx immutable =))
                   (make-struct-field-mutator set idx)))))
  (define-values (auto-accessors auto-mutators)
    (for/lists (_1 _2) ([idx (in-range auto)])
      (values (make-struct-field-accessor ref idx)
              #f)))
  (values (append parent-accessors base-accessors auto-accessors)
          (append parent-mutators base-mutators auto-mutators)))

;; Struct-Type (Listof Contract) -> Void
;; Check that impersonator contracts are only specified on mutable fields
;; Precondition: the ctcs length is consistent with the struct-type
(define (check-mutable-impersonator-consistency struct-type ctcs)
  (define-values (_1 init auto _2 _3 immutable parent _4)
    (struct-type-info struct-type))
  (define-values (for-parents for-this)
    (split-at-right ctcs (+ init auto)))
  (when parent
    (check-mutable-impersonator-consistency parent for-parents))
  (for ([(ctc idx) (in-indexed (in-list ctcs))])
    (when (and (or (member idx immutable =)
                   ;; is an auto-field index, which are always immutable
                   (> idx (sub1 init)))
               (impersonator-contract? ctc))
      (raise-argument-error 'prefab/c
                            "a chaperone-contract? for immutable field"
                            ctc))))

(define (prefab/c-first-order ctc)
  (define instance?
    (struct-type-make-predicate (base-prefab/c-struct-type ctc)))
  (λ (val)
    (and (instance? val)
         (for/and (;; skip first elem (the struct name)
                   [elem (in-vector (struct->vector val) 1)]
                   [ctc (in-list (base-prefab/c-ctcs ctc))])
           (contract-first-order-passes? ctc elem)))))

(define (prefab/c-stronger? this that)
  (and (base-prefab/c? that)
       (equal? (base-prefab/c-key this)
               (base-prefab/c-key that))
       (for/and ([this-ctc (in-list (base-prefab/c-ctcs this))]
                 [that-ctc (in-list (base-prefab/c-ctcs that))])
         (contract-stronger? this-ctc that-ctc))))

(struct base-prefab/c (key struct-type ctcs))

(struct flat-prefab/c base-prefab/c ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name prefab/c-name
   #:first-order prefab/c-first-order
   #:stronger prefab/c-stronger?))

(struct chaperone-prefab/c base-prefab/c ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection (prefab/c-proj chaperone-struct)
   #:val-first-projection (prefab/c-val-first-proj chaperone-struct)
   #:name prefab/c-name
   #:first-order prefab/c-first-order
   #:stronger prefab/c-stronger?))

(struct impersonator-prefab/c base-prefab/c ()
  #:property prop:contract
  (build-contract-property
   #:projection (prefab/c-proj impersonate-struct)
   #:val-first-projection (prefab/c-val-first-proj impersonate-struct)
   #:name prefab/c-name
   #:first-order prefab/c-first-order
   #:stronger prefab/c-stronger?))
