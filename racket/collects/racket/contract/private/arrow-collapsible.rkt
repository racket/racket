#lang racket/base

;; collapsible arrow contracts
;; supports a subset of full arrow contracts
;; based on a prototype by Christophe Scholliers

(require racket/unsafe/ops
         "collapsible-common.rkt" "merge-cache.rkt"
         (submod "collapsible-common.rkt" properties)
         "prop.rkt" "guts.rkt" "misc.rkt" "blame.rkt" "arrow-common.rkt"
         "arity-checking.rkt"
         (for-syntax racket/base))

(provide arrow-enter-collapsible-mode/continue
         arrow-enter-collapsible-mode/collapse
         val-has-arrow-collapsible-support?
         ->-contract-has-collapsible-support?
         build-collapsible-arrow)
(module+ for-testing
  (provide collapsible->? collapsible->-doms collapsible->-rng))

;; General Strategy

;; Each function contracted with a collapsible contract has two or three
;; chaperone wrappers.
;; - Functions that are wrapped in a "top-level" arrow contract (i.e., not a
;;   subcontract of an arrow contract) are first contracted using a regular
;;   function contract wrapper (before reaching this code). Upon being
;;   contracted a second time, they reach this code, and get three chaperone
;;   wrappers:
;;   - first, an unsafe-chaperone wrapper, which chaperones the current
;;     contracted value (to pretend it's it), but actually just calls the
;;     original, uncontracted function (i.e. skips the original contract)
;;   - second, a chaperone* wrapper, which gets passed the outermost wrapper,
;;     and looks at a property on it to figure out what to check, then does
;;     the actual contract checking
;;   - third, a property-only chaperone wrapper, which has a collapsible contract
;;     on a property, to keep track of which contracts to check.
;;   When additional contracts are applied, this third chaperone is swapped out
;;   for a new one, which keeps track of the new, merged contract to check.
;;   Because it's a property-only chaperone, replacing it with a new one doesn't
;;   affect chaperone-of-ness.
;; - Functions that are wrapped in an "internal node" arrow contract (i.e.,
;;   their arrow contract is a subcontract of another arrow contract) may be
;;   wrapped with collapsible wrappers from the start (i.e., before getting
;;   any other contract).
;;   Note: This could be changed. Just avoid recursively converting contracts in
;;     `ho/c->collapsible->`, and instead have doms and rngs be `ho-leaf/c` always.
;;   Because of this, they don't need the first, unsafe chaperone wrapper above.
;;   They only have the last two wrappers, otherwise the above strategy applies.

;; Alternatively, we may try to attach an (internal node) collapsible
;; contract to a value that doesn't support collapsible contracts (e.g.,
;; a function that takes keyword arguments). In this case, we must fall back to
;; regular contract wrapping, and convert the collapsible contract to a
;; regular checking wrapper, as used elsewhere in the contract system (c.f.
;; `bail-to-regular-wrapper`).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structures

;; we store the most recent blame only. when contracts fail, they assign
;; blame based on closed-over blame info, so `latest-blame` is only used
;; for things like prop:blame, contract profiling, and tail marks, in which
;; case we lose information, but it's ok to be conservative in these places
;; (and this behavior is consistent with what would happen in the absence
;; of collapsible contracts anyway)
;; ditto for `latest-ctc` and prop:contracted
(struct collapsible-> collapsible-ho/c (doms rng first-order-checks))

;; contains all the information necessary to both (1) perform first order checks
;; for an arrow contract, and (2) determine which such checks are redundant and
;; can be eliminated
(struct arrow-first-order-check (n-doms blame missing-party method?))
;; stronger really means "the same" here
(define (arrow-first-order-check-stronger? x y)
  (= (arrow-first-order-check-n-doms x) (arrow-first-order-check-n-doms y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applicability checks

(define (->-contract-has-collapsible-support? ctc)
  (define-syntax-rule (bail reason)
    (begin
      (log-collapsible-contract-bailout-info (format "arrow: ~a" reason))
      #f))
  (cond [(collapsible->? ctc) ; already one
         #t]
        [(base->? ctc) ; only applies to regular arrow contracts (for now)
         (define doms (base->-doms ctc))
         (define rngs (base->-rngs ctc))
         (and
          ;; TODO: we can probably handle more of these cases for an -> contract
          (or doms
              (bail "no doms"))
          (or (= (length doms) (base->-min-arity ctc)) ; no optional args
              (bail "has optional args"))
          (or (null? (base->-kwd-infos ctc)) ; no keyword args
              (bail "has keyword args"))
          (or (not (base->-rest ctc)) ; no rest arg
              (bail "has rest arg"))
          (or (not (base->-pre? ctc)) ; no pre-condition
              (bail "has pre-condition"))
          (or (not (base->-post? ctc)) ; no post-condition
              (bail "has post-condition"))
          (or rngs
              (bail "no rngs"))
          (or (= (length rngs) 1)
              (bail "multiple return values")))]
        [else
         (bail "not base arrow")
         #f]))

(define (val-has-arrow-collapsible-support? val)
  (define-syntax-rule (bail reason)
    (begin
      (log-collapsible-value-bailout-info (format "arrow: ~a" reason))
      #f))
  (and
   (or (not (procedure-impersonator*? val))
       (bail "procedure-impersonator*?"))
   ;; the interposition wrapper has to support a superset of the arity
   ;; of the function it's wrapping, and ours can't support optional
   ;; args, keywords, etc. so just bail out in these cases

   ;; TODO: I think we can actually support optional arguments without any additional work
   ;; here ... so maybe this check can be removed
   (or (integer? (procedure-arity val))
       (bail "has optional args"))
   (or (let-values ([(man opt) (procedure-keywords val)]) ; no keyword arguments
         (and (null? man) (null? opt)))
       (bail "has keyword args"))
   ;; TODO: we can maybe support non single return value functions
   (or (equal? (procedure-result-arity val) 1)
       (bail "can't prove single-return-value"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper management and contract checking

(define (arrow-collapsible-guard c-c val neg-party)
  (do-arrow-first-order-checks c-c val neg-party)
  (define chap-not-imp? (chaperone-collapsible->? c-c))
  (define prop (get-impersonator-prop:collapsible val #f))
  (define safe-for-c-c?
    (if prop
        (and (collapsible-property? prop)
             (eq? (collapsible-property-ref prop) val))
        (val-has-arrow-collapsible-support? val)))
  (cond
    [(not safe-for-c-c?) (bail-to-regular-wrapper c-c val neg-party)]
    [(collapsible-wrapper-property? prop)
     (arrow-enter-collapsible-mode/continue
      c-c
      val
      neg-party
      (collapsible-property-c-c prop)
      (collapsible-property-neg-party prop)
      (collapsible-wrapper-property-checking-wrapper prop)
      chap-not-imp?)]
    [(collapsible-count-property? prop)
     (arrow-enter-collapsible-mode/collapse
      c-c
      val
      neg-party
      prop
      chap-not-imp?)]
    ;; else enter directly
    [else
     (arrow-enter-collapsible-mode/direct c-c val neg-party chap-not-imp?)]))

(define (add-collapsible-arrow-chaperone merged c-c neg-party checking-wrapper chap-not-imp?)
  (define chap/imp (if chap-not-imp? chaperone-procedure impersonate-procedure))
  (define c-c-prop
    (collapsible-wrapper-property merged neg-party #f checking-wrapper))
  (define wrapped
    (chap/imp
     checking-wrapper
     #f
     impersonator-prop:collapsible c-c-prop))
  (set-collapsible-property-ref! c-c-prop wrapped)
  wrapped)


(define (make-checking-wrapper unwrapped chap-not-imp?)
  (if chap-not-imp?
      (chaperone-procedure* unwrapped arrow-wrapper)
      (impersonate-procedure* unwrapped arrow-wrapper)))

(define (make-unsafe-checking-wrapper val unwrapped chap-not-imp?)
  (if chap-not-imp?
      (chaperone-procedure*
       (unsafe-chaperone-procedure val unwrapped)
       arrow-wrapper)
      (impersonate-procedure*
       (unsafe-impersonate-procedure val unwrapped)
       arrow-wrapper)))

;; If requested, we can log the arities of the contracts that end up being
;; collapsible. That can inform whether we should have arity-specific
;; wrappers, and if so, for which arities.
(define-logger collapsible-contract-arrow-wrapper-arity)

;; Create the 2nd chaperone wrapper procedure (see comment at the top),
;; as well as "deoptimization" wrappers (see below).
;; Checking wrappers come in different varieties, along two axes:
;; - chaperone vs impersonator (to know how to wrap for subcontracts)
;; - where to find the checks (on an impersonator property, for actual
;;   collapsible contracts, vs closed over, for cases where we need
;;   a regular contract wrapper (i.e., a subcontract has to "bail out,
;;   and can't use the collapsible machinery (but since subcontracts
;;   always start-out as collapsible, they can't bail out via the
;;   checks in arrow-higher-order, so we need to handle them here)))
(define-syntax (make-interposition-procedure stx)
  (syntax-case stx ()
    [(_ maybe-closed-over-m/c maybe-closed-over-neg)
     ;; Note: it would be more efficient to have arity-specific wrappers here,
     ;;   as opposed to using a rest arg.
     #`(λ (outermost-chaperone . args)
         (define-values (m/c neg-party)
           #,(if (syntax-e #'maybe-closed-over-m/c)
                 #'(values maybe-closed-over-m/c maybe-closed-over-neg)
                 #'(let ()
                     (define prop (get-impersonator-prop:collapsible outermost-chaperone))
                     (values (collapsible-property-c-c prop)
                             (collapsible-property-neg-party prop)))))
         (define neg (or (collapsible-ho/c-missing-party m/c) neg-party))
         (define doms   (collapsible->-doms         m/c))
         (define rng    (collapsible->-rng          m/c))
         (define blame  (collapsible-ho/c-latest-blame m/c))
         (define blame+neg-party  (cons blame neg))
         (define n-args (length args))
         (define n-doms (length doms))
         (log-collapsible-contract-arrow-wrapper-arity-info
          (number->string n-doms))
         (unless (= n-args n-doms)
           (raise-wrong-number-of-args-error blame #:missing-party neg outermost-chaperone
                                             n-args n-doms n-doms #f))
         ;; Note: to support (i.e., not bail on) functions that can't be proven
         ;;   to return a single value, have a `case-lambda` wrapper here. (With
         ;;   the possibility of using return-arity-specific wrappers if return
         ;;   arity happens to be known.)
         ;; Note: should add tail-marks-match support here.
         (define rng-checker
           (lambda (result)
             (with-collapsible-contract-continuation-mark
               (with-contract-continuation-mark
                 blame+neg-party
                 (collapsible-guard rng result neg)))))
         (apply values
                rng-checker
                (for/list ([dom (in-list doms)]
                           [arg (in-list args)])
                  (with-collapsible-contract-continuation-mark
                    (with-contract-continuation-mark
                      blame+neg-party
                      (collapsible-guard dom arg neg))))))]))

(define arrow-wrapper (make-interposition-procedure #f #f))

;; create a regular checking wrapper from a collapsible wrapper for a value
;; that can't use collapsible wrapping
(define (bail-to-regular-wrapper m/c val neg-party)
  (define chap-not-imp? (chaperone-collapsible->? m/c))
  (define neg (or (collapsible-ho/c-missing-party m/c) neg-party))
  ((if chap-not-imp? chaperone-procedure* impersonate-procedure*)
   val
   (make-interposition-procedure m/c neg)
   impersonator-prop:contracted (collapsible-ho/c-latest-ctc   m/c)
   impersonator-prop:blame (cons
                            (collapsible-ho/c-latest-blame m/c)
                            neg)))

(define (do-arrow-first-order-checks m/c val neg-party)
  (define checks (collapsible->-first-order-checks m/c))
  (for ([c (in-list checks)])
    (define n-doms (arrow-first-order-check-n-doms c))
    (define partial-blame (arrow-first-order-check-blame c))
    (define neg (arrow-first-order-check-missing-party c))
    (cond [(do-arity-checking
            partial-blame
            val
            (for/list ([i (in-range n-doms)]) #f) ; has to have the right length
            #f ; no rest arg
            n-doms ; min-arity = max-arity
            '() ; no keywords
            (arrow-first-order-check-method? c))
           => (lambda (fail) (fail (or neg neg-party)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collapsible contract data structure management

(define (build-collapsible-arrow rng doms ctc blame chap? [maybe-focs #f] [maybe-neg-blame #f])
  (define focs
    (or maybe-focs (list (arrow-first-order-check (length doms) blame maybe-neg-blame (base->-method? ctc)))))
  (if chap?
      (chaperone-collapsible-> blame maybe-neg-blame ctc doms rng focs)
      (impersonator-collapsible-> blame maybe-neg-blame ctc doms rng focs)))

;; merge two collapsible->
(define/merge-cache (arrow-try-merge new-collapsible new-neg old-collapsible old-neg)
  (define constructor (get-constructor new-collapsible old-collapsible))
  (and constructor
       (constructor
        (collapsible-ho/c-latest-blame new-collapsible)
        (or (collapsible-ho/c-missing-party new-collapsible) new-neg)
        (collapsible-ho/c-latest-ctc   new-collapsible)
        ;; if old and new don't have the same arity, then one of them will *have*
        ;; to fail its first order checks, so we're fine.
        ;; (we don't support optional arguments)
        (merge-list (collapsible->-doms old-collapsible) old-neg (collapsible->-doms new-collapsible) new-neg)
        (merge (collapsible->-rng new-collapsible) new-neg (collapsible->-rng old-collapsible) old-neg)
        (arrow-first-order-merge
         (collapsible->-first-order-checks new-collapsible) new-neg
         (collapsible->-first-order-checks old-collapsible) old-neg))))

(define (merge-list news new-neg olds old-neg)
  (for/list ([new (in-list news)]
             [old (in-list olds)])
    (merge new new-neg old old-neg)))

(define (arrow-first-order-merge new new-neg old old-neg)
  (first-order-check-join
   (add-f-o-neg-party new new-neg)
   (add-f-o-neg-party old old-neg)
   arrow-first-order-check-stronger?))

(define arrow-enter-collapsible-mode/continue
  (make-enter-collapsible-mode/continue
   arrow-try-merge
   add-collapsible-arrow-chaperone
   bail-to-regular-wrapper))

(define arrow-enter-collapsible-mode/collapse
    (make-enter-collapsible-mode/collapse
     make-unsafe-checking-wrapper
     add-collapsible-arrow-chaperone
     arrow-try-merge
     bail-to-regular-wrapper))

(define arrow-enter-collapsible-mode/direct
  (make-enter-collapsible-mode/direct
   make-checking-wrapper
   add-collapsible-arrow-chaperone))

(define (add-f-o-neg-party focs neg-party)
  (for/list ([foc (in-list focs)])
    (define missing-party (arrow-first-order-check-missing-party foc))
    (struct-copy
     arrow-first-order-check
     foc
     [missing-party (or missing-party neg-party)])))

(define (get-constructor new old)
  (or (and (chaperone-collapsible->? new)
           (chaperone-collapsible->? old)
           chaperone-collapsible->)
      (and (impersonator-collapsible->? new)
           (impersonator-collapsible->? old)
           impersonator-collapsible->)))

(define (->-collapsible-contract-property chap?)
  (build-collapsible-contract-property
   #:try-merge arrow-try-merge
   #:collapsible-guard arrow-collapsible-guard))

(struct chaperone-collapsible-> collapsible-> ()
  #:property prop:collapsible-contract (->-collapsible-contract-property #t))
(struct impersonator-collapsible-> collapsible-> ()
  #:property prop:collapsible-contract (->-collapsible-contract-property #f))
