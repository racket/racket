#lang racket/base

(require "prop.rkt" "guts.rkt" "blame.rkt" "vector-common.rkt"
         "space-efficient-common.rkt" "merge-cache.rkt"
         (submod "space-efficient-common.rkt" properties)
         (only-in racket/unsafe/ops unsafe-chaperone-vector unsafe-impersonate-vector)
         (for-syntax racket/base))

(provide build-s-e-vector
         build-doubling-s-e-vector
         vector-space-efficient-guard
         vector-enter-space-efficient-mode/continue
         vector-enter-space-efficient-mode/collapse)

(module+ for-testing
  (provide  multi-vector? multi-vector-ref-ctcs multi-vector-set-ctcs))

(struct vector-first-order-check (immutable length blame missing-party))

;; mutable field are only to support impersonation in `build-doubling-s-e-vector`
(struct multi-vector multi-ho/c (first-order [ref-ctcs #:mutable] [set-ctcs #:mutable]) #:transparent)

(define (do-vector-first-order-checks m/c val neg-party)
  (define checks (multi-vector-first-order m/c))
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

(define (build-s-e-vector s-e-pos s-e-neg ctc blame chap-not-imp?)
  (define focs (list (build-vector-first-order-checks ctc blame)))
  (if chap-not-imp?
      (chaperone-multi-vector blame #f ctc focs s-e-pos s-e-neg)
      (impersonator-multi-vector blame #f ctc focs s-e-pos s-e-neg)))

(define (build-doubling-s-e-vector fetch-s-e-pos fetch-s-e-neg ctc blame chap-not-imp?)
  (define focs (list (build-vector-first-order-checks ctc blame)))
  (define dummy
    (if chap-not-imp?
        (chaperone-multi-vector blame #f ctc focs 'dummy-s-e-pos 'dummy-s-e-neg)
        (impersonator-multi-vector blame #f ctc focs 'dummy-s-e-pos 'dummy-s-e-neg)))
  (impersonate-struct dummy
                      multi-vector-ref-ctcs
                      (λ (self field-v) (fetch-s-e-pos))
                      multi-vector-set-ctcs
                      (λ (self field-v) (fetch-s-e-neg))))

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

(define/merge-cache (vector-try-merge new-multi new-neg old-multi old-neg)
  (define constructor (get-constructor new-multi old-multi))
  (and constructor
       (constructor
        (multi-ho/c-latest-blame new-multi)
        (or (multi-ho/c-missing-party new-multi) new-neg)
        (multi-ho/c-latest-ctc new-multi)
        (vector-first-order-merge
         (multi-vector-first-order new-multi) new-neg
         (multi-vector-first-order old-multi) old-neg)
        (merge* (multi-vector-ref-ctcs new-multi)
                new-neg
                (multi-vector-ref-ctcs old-multi)
                old-neg)
        (merge* (multi-vector-set-ctcs old-multi)
                old-neg
                (multi-vector-set-ctcs new-multi)
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
  (or (and (chaperone-multi-vector? new)
           (chaperone-multi-vector? old)
           chaperone-multi-vector)
      (and (impersonator-multi-vector? new)
           (impersonator-multi-vector? old)
           impersonator-multi-vector)))

(define (vector-space-efficient-guard s-e val neg-party)
  (do-vector-first-order-checks s-e val neg-party)
  (define chap-not-imp? (chaperone-multi-vector? s-e))
  (define prop (get-impersonator-prop:space-efficient val #f))
  (define safe-for-s-e?
    (if prop
        (and (space-efficient-property? prop)
             (eq? (space-efficient-property-ref prop) val))
        (not (impersonator? val))))
  (cond
    ;; not safe, bail out
    [(not safe-for-s-e?)
     (bail-to-regular-wrapper s-e val neg-party)]
    ;; already in s-e mode, so stay in
    [(space-efficient-wrapper-property? prop)
     (vector-enter-space-efficient-mode/continue
      s-e
      val
      neg-party
      (space-efficient-property-s-e prop)
      (space-efficient-property-neg-party prop)
      (space-efficient-wrapper-property-checking-wrapper prop)
      chap-not-imp?)]
    ;; need to collapse contracts ...
    [(space-efficient-count-property? prop)
     (vector-enter-space-efficient-mode/collapse
      s-e
      val
      neg-party
      prop
      chap-not-imp?)]
    ;; else enter directly
    [else
     (vector-enter-space-efficient-mode/direct s-e val neg-party chap-not-imp?)]))

(define (add-space-efficient-vector-chaperone merged s-e neg-party checking-wrapper chap-not-imp?)
  (define chap/imp (if chap-not-imp? chaperone-vector impersonate-vector))
  (define s-e-prop
    (space-efficient-wrapper-property merged neg-party #f checking-wrapper))
  (define wrapped
    (chap/imp
     checking-wrapper
     #f
     #f
     impersonator-prop:space-efficient s-e-prop))
  (set-space-efficient-property-ref! s-e-prop wrapped)
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
                     (define prop (get-impersonator-prop:space-efficient outermost))
                     (values (space-efficient-property-s-e prop)
                             (space-efficient-property-neg-party prop)))))
         (define neg (or (multi-ho/c-missing-party m/c) neg-party))
         (define field
           #,(if (syntax-e #'set?)
                 #'(multi-vector-set-ctcs m/c)
                 #'(multi-vector-ref-ctcs m/c)))
         (define s-e
           (if (vector? field) (vector-ref field i) field))
         (define blame (cons (multi-ho/c-latest-blame m/c) neg))
         (with-space-efficient-contract-continuation-mark
             (with-contract-continuation-mark
                 blame
               (space-efficient-guard s-e elt neg))))]))

(define ref-wrapper (make-vector-checking-wrapper #f #f #f))
(define set-wrapper (make-vector-checking-wrapper #t #f #f))

(define (bail-to-regular-wrapper m/c val neg-party)
  (define chap-not-imp? (chaperone-multi-vector? m/c))
  (define neg (or (multi-ho/c-missing-party m/c) neg-party))
  (define blame (cons (multi-ho/c-latest-blame m/c) neg))
  (define ctc (multi-ho/c-latest-ctc m/c))
  (define merged+neg-party (cons m/c neg))
  ((if chap-not-imp? chaperone-vector* impersonate-vector*)
   val
   (make-vector-checking-wrapper #f m/c neg)
   (make-vector-checking-wrapper #t m/c neg)
   impersonator-prop:contracted ctc
   impersonator-prop:blame blame))

(define vector-enter-space-efficient-mode/continue
  (make-enter-space-efficient-mode/continue
   vector-try-merge
   add-space-efficient-vector-chaperone
   bail-to-regular-wrapper))

(define vector-enter-space-efficient-mode/collapse
  (make-enter-space-efficient-mode/collapse
   make-unsafe-checking-wrapper
   add-space-efficient-vector-chaperone
   vector-try-merge
   bail-to-regular-wrapper))

(define vector-enter-space-efficient-mode/direct
  (make-enter-space-efficient-mode/direct
   make-checking-wrapper
   add-space-efficient-vector-chaperone))

(define (vector-space-efficient-contract-property chap-not-imp?)
  (build-space-efficient-contract-property
   #:try-merge vector-try-merge
   #:space-efficient-guard vector-space-efficient-guard))

(struct chaperone-multi-vector multi-vector ()
  #:property prop:space-efficient-contract (vector-space-efficient-contract-property #t))
(struct impersonator-multi-vector multi-vector ()
  #:property prop:space-efficient-contract (vector-space-efficient-contract-property #f))
