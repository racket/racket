#lang racket/base

(require "prop.rkt" "guts.rkt" "blame.rkt" "vector-common.rkt"
         "collapsible-common.rkt" "merge-cache.rkt"
         (submod "collapsible-common.rkt" properties)
         (only-in racket/unsafe/ops unsafe-chaperone-vector unsafe-impersonate-vector)
         (for-syntax racket/base))

(provide build-collapsible-vector
         build-doubling-collapsible-vector
         vector-collapsible-guard
         vector-enter-collapsible-mode/continue
         vector-enter-collapsible-mode/collapse)

(module+ for-testing
  (provide  collapsible-vector? collapsible-vector-ref-ctcs collapsible-vector-set-ctcs))

(struct vector-first-order-check (immutable length blame missing-party))

;; mutable field are only to support impersonation in `build-doubling-c-c-vector`
(struct collapsible-vector collapsible-ho/c (first-order [ref-ctcs #:mutable] [set-ctcs #:mutable]) #:transparent)

(define (do-vector-first-order-checks m/c val neg-party)
  (define checks (collapsible-vector-first-order m/c))
  (for ([c (in-list checks)])
    (define immutable (vector-first-order-check-immutable c))
    (define length (vector-first-order-check-length c))
    (define blame (vector-first-order-check-blame c))
    (define neg (or (vector-first-order-check-missing-party c) neg-party))
    (check-vector/c val blame immutable length neg)))

(define (vector-first-order-check-stronger? f1 f2)
  (define f1-immutable (vector-first-order-check-immutable f1))
  (define f1-length (vector-first-order-check-length f1))
  (define f2-immutable (vector-first-order-check-immutable f2))
  (define f2-length (vector-first-order-check-length f2))
  (and (or (eq? f2-immutable 'dont-care)
           (eq? f1-immutable f2-immutable))
       (or (not f2-length)
           (and f1-length (= f1-length f2-length)))))

(define (build-collapsible-vector c-c-pos c-c-neg ctc blame chap-not-imp?)
  (define focs (list (build-vector-first-order-checks ctc blame)))
  (if chap-not-imp?
      (chaperone-collapsible-vector blame #f ctc focs c-c-pos c-c-neg)
      (impersonator-collapsible-vector blame #f ctc focs c-c-pos c-c-neg)))

(define (build-doubling-collapsible-vector fetch-c-c-pos fetch-c-c-neg ctc blame chap-not-imp?)
  (define focs (list (build-vector-first-order-checks ctc blame)))
  (define dummy
    (if chap-not-imp?
        (chaperone-collapsible-vector blame #f ctc focs 'dummy-c-c-pos 'dummy-c-c-neg)
        (impersonator-collapsible-vector blame #f ctc focs 'dummy-c-c-pos 'dummy-c-c-neg)))
  (impersonate-struct dummy
                      collapsible-vector-ref-ctcs
                      (λ (self field-v) (fetch-c-c-pos))
                      collapsible-vector-set-ctcs
                      (λ (self field-v) (fetch-c-c-neg))))

(define (build-vector-first-order-checks ctc blame)
  (cond
    [(base-vectorof? ctc)
     (vector-first-order-check
      (base-vectorof-immutable ctc)
      #f
      blame
      #f)]
    [(base-vector/c? ctc)
     (vector-first-order-check
      (base-vector/c-immutable ctc)
      (length (base-vector/c-elems ctc))
      blame
      #f)]))

(define (add-f-o-neg-party focs neg-party)
  (for/list ([foc (in-list focs)])
    (define missing-party (vector-first-order-check-missing-party foc))
    (struct-copy
     vector-first-order-check
     foc
     [missing-party (or missing-party neg-party)])))

(define (vector-first-order-merge new new-neg old old-neg)
  (first-order-check-join
   (add-f-o-neg-party new new-neg)
   (add-f-o-neg-party old old-neg)
   vector-first-order-check-stronger?))

(define/merge-cache (vector-try-merge new-collapsible new-neg old-collapsible old-neg)
  (define constructor (get-constructor new-collapsible old-collapsible))
  (and constructor
       (constructor
        (collapsible-ho/c-latest-blame new-collapsible)
        (or (collapsible-ho/c-missing-party new-collapsible) new-neg)
        (collapsible-ho/c-latest-ctc new-collapsible)
        (vector-first-order-merge
         (collapsible-vector-first-order new-collapsible) new-neg
         (collapsible-vector-first-order old-collapsible) old-neg)
        (merge* (collapsible-vector-ref-ctcs new-collapsible)
                new-neg
                (collapsible-vector-ref-ctcs old-collapsible)
                old-neg)
        (merge* (collapsible-vector-set-ctcs old-collapsible)
                old-neg
                (collapsible-vector-set-ctcs new-collapsible)
                new-neg))))

(define (merge* new new-neg old old-neg)
  (cond
    [(and (vector? new) (vector? old))
     (for/vector ([nc (in-vector new)]
                  [oc (in-vector old)])
       (merge nc new-neg oc old-neg))]
    [(vector? new)
     (for/vector ([nc (in-vector new)])
       (merge nc new-neg old old-neg))]
    [(vector? old)
     (for/vector ([oc (in-vector old)])
       (merge new new-neg oc old-neg))]
    [else
     (merge new new-neg old old-neg)]))

(define (get-constructor new old)
  (or (and (chaperone-collapsible-vector? new)
           (chaperone-collapsible-vector? old)
           chaperone-collapsible-vector)
      (and (impersonator-collapsible-vector? new)
           (impersonator-collapsible-vector? old)
           impersonator-collapsible-vector)))

(define (vector-collapsible-guard c-c val neg-party)
  (do-vector-first-order-checks c-c val neg-party)
  (define chap-not-imp? (chaperone-collapsible-vector? c-c))
  (define prop (get-impersonator-prop:collapsible val #f))
  (define safe-for-c-c?
    (if prop
        (and (collapsible-property? prop)
             (eq? (collapsible-property-ref prop) val))
        (not (impersonator? val))))
  (cond
    ;; not safe, bail out
    [(not safe-for-c-c?)
     (bail-to-regular-wrapper c-c val neg-party)]
    ;; already in c-c mode, so stay in
    [(collapsible-wrapper-property? prop)
     (vector-enter-collapsible-mode/continue
      c-c
      val
      neg-party
      (collapsible-property-c-c prop)
      (collapsible-property-neg-party prop)
      (collapsible-wrapper-property-checking-wrapper prop)
      chap-not-imp?)]
    ;; need to collapse contracts ...
    [(collapsible-count-property? prop)
     (vector-enter-collapsible-mode/collapse
      c-c
      val
      neg-party
      prop
      chap-not-imp?)]
    ;; else enter directly
    [else
     (vector-enter-collapsible-mode/direct c-c val neg-party chap-not-imp?)]))

(define (add-collapsible-vector-chaperone merged c-c neg-party checking-wrapper chap-not-imp?)
  (define chap/imp (if chap-not-imp? chaperone-vector impersonate-vector))
  (define c-c-prop
    (collapsible-wrapper-property merged neg-party #f checking-wrapper))
  (define wrapped
    (chap/imp
     checking-wrapper
     #f
     #f
     impersonator-prop:collapsible c-c-prop))
  (set-collapsible-property-ref! c-c-prop wrapped)
  wrapped)

(define (make-checking-wrapper unwrapped chap-not-imp?)
  (if chap-not-imp?
      (chaperone-vector* unwrapped ref-wrapper set-wrapper)
      (impersonate-vector* unwrapped ref-wrapper set-wrapper)))

(define (make-unsafe-checking-wrapper val unwrapped chap-not-imp?)
  (if chap-not-imp?
      (chaperone-vector*
       (unsafe-chaperone-vector val unwrapped)
       ref-wrapper
       set-wrapper)
      (impersonate-vector*
       (unsafe-impersonate-vector val unwrapped)
       ref-wrapper
       set-wrapper)))

(define-syntax (make-vector-checking-wrapper stx)
  (syntax-case stx ()
    [(_ set? maybe-closed-over-m/c maybe-closed-over-neg)
     #`(λ (outermost v i elt)
         (define-values (m/c neg-party)
           #,(if (syntax-e #'maybe-closed-over-m/c)
                 #'(values maybe-closed-over-m/c maybe-closed-over-neg)
                 #'(let ()
                     (define prop (get-impersonator-prop:collapsible outermost))
                     (values (collapsible-property-c-c prop)
                             (collapsible-property-neg-party prop)))))
         (define neg (or (collapsible-ho/c-missing-party m/c) neg-party))
         (define field
           #,(if (syntax-e #'set?)
                 #'(collapsible-vector-set-ctcs m/c)
                 #'(collapsible-vector-ref-ctcs m/c)))
         (define c-c
           (if (vector? field) (vector-ref field i) field))
         (define blame (cons (collapsible-ho/c-latest-blame m/c) neg))
         (with-collapsible-contract-continuation-mark
             (with-contract-continuation-mark
                 blame
               (collapsible-guard c-c elt neg))))]))

(define ref-wrapper (make-vector-checking-wrapper #f #f #f))
(define set-wrapper (make-vector-checking-wrapper #t #f #f))

(define (bail-to-regular-wrapper m/c val neg-party)
  (define chap-not-imp? (chaperone-collapsible-vector? m/c))
  (define neg (or (collapsible-ho/c-missing-party m/c) neg-party))
  (define blame (cons (collapsible-ho/c-latest-blame m/c) neg))
  (define ctc (collapsible-ho/c-latest-ctc m/c))
  (define merged+neg-party (cons m/c neg))
  ((if chap-not-imp? chaperone-vector* impersonate-vector*)
   val
   (make-vector-checking-wrapper #f m/c neg)
   (make-vector-checking-wrapper #t m/c neg)
   impersonator-prop:contracted ctc
   impersonator-prop:blame blame))

(define vector-enter-collapsible-mode/continue
  (make-enter-collapsible-mode/continue
   vector-try-merge
   add-collapsible-vector-chaperone
   bail-to-regular-wrapper))

(define vector-enter-collapsible-mode/collapse
  (make-enter-collapsible-mode/collapse
   make-unsafe-checking-wrapper
   add-collapsible-vector-chaperone
   vector-try-merge
   bail-to-regular-wrapper))

(define vector-enter-collapsible-mode/direct
  (make-enter-collapsible-mode/direct
   make-checking-wrapper
   add-collapsible-vector-chaperone))

(define (vector-collapsible-contract-property chap-not-imp?)
  (build-collapsible-contract-property
   #:try-merge vector-try-merge
   #:collapsible-guard vector-collapsible-guard))

(struct chaperone-collapsible-vector collapsible-vector ()
  #:property prop:collapsible-contract (vector-collapsible-contract-property #t))
(struct impersonator-collapsible-vector collapsible-vector ()
  #:property prop:collapsible-contract (vector-collapsible-contract-property #f))
