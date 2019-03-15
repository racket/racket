#lang racket/base

;; Common functionality used by all collapsible contracts

(require "prop.rkt" "merge-cache.rkt")

(provide (struct-out collapsible-ho/c)
         (struct-out collapsible-leaf/c)
         (struct-out collapsible-property)
         (struct-out collapsible-count-property)
         (struct-out collapsible-wrapper-property)
         build-collapsible-leaf
         prop:collapsible-contract
         collapsible-contract-property?
         build-collapsible-contract-property
         collapsible-contract-property?
         collapsible-contract?
         merge
         collapsible-guard
         first-order-check-join
         log-collapsible-value-bailout-info
         log-collapsible-contract-bailout-info
         log-collapsible-cache-fail-info
         make-enter-collapsible-mode/continue
         make-enter-collapsible-mode/collapse
         make-enter-collapsible-mode/direct
         COLLAPSIBLE-LIMIT)

(module+ for-testing
  (provide collapsible-leaf/c?
           collapsible-leaf/c-contract-list
           collapsible-leaf/c-proj-list
           collapsible-property-c-c
           collapsible-property-ref
           has-impersonator-prop:collapsible?
           get-impersonator-prop:collapsible
           collapsible-wrapper-property?
           collapsible-wrapper-property-checking-wrapper
           calculate-drops))

;; object contracts need to propagate properties across procedure->method
(module+ properties
  (provide impersonator-prop:collapsible
           has-impersonator-prop:collapsible?
           get-impersonator-prop:collapsible))

(define-logger collapsible-value-bailout)
(define-logger collapsible-contract-bailout)
(define-logger collapsible-merging)
(define-logger collapsible-cache-fail)

(define COLLAPSIBLE-LIMIT 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties
(define-values (impersonator-prop:collapsible
                has-impersonator-prop:collapsible?
                get-impersonator-prop:collapsible)
  (make-impersonator-property 'impersonator-prop:collapsible))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An interface for collapsible contract conversion and merging
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct collapsible-contract-property
  (try-merge
   collapsible-guard)
  #:omit-define-syntaxes)

(define (collapsible-contract-property-guard prop info)
  (unless (collapsible-contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a collapsible contract property; got: ~e"
              prop)
      (current-continuation-marks))))
  prop)

(define-values (prop:collapsible-contract collapsible-contract? get-collapsible-contract-property)
  (make-struct-type-property 'collapsible-contract collapsible-contract-property-guard))

(define (build-collapsible-contract-property
         #:try-merge [try-merge #f]
         #:collapsible-guard
         [collapsible-guard
          (lambda (ctc val neg)
            (error "internal error: contract does not support `collapsible-guard`" ctc))])
  (collapsible-contract-property
   (or try-merge (lambda (_1 _2 _3 _4) #f))
   collapsible-guard))

;; Parent structure for higher order collapsible contracts
;; which must keep track of the latest blame and missing party
;; and latest contract applied
(struct collapsible-ho/c (latest-blame missing-party latest-ctc))

(struct collapsible-leaf/c (proj-list contract-list blame-list missing-party-list)
  #:property prop:collapsible-contract
  (build-collapsible-contract-property
   #:try-merge (lambda (new new-neg old old-neg)
                 (and (collapsible-leaf/c? old)
                      (collapsible-leaf/c? new)
                      (join-collapsible-leaf/c new new-neg old old-neg)))
   #:collapsible-guard
   (lambda (c-c val neg-party)
     (apply-proj-list (collapsible-leaf/c-proj-list c-c)
                      (collapsible-leaf/c-missing-party-list c-c)
                      val
                      neg-party))))

(define (build-collapsible-leaf proj ctc blame)
  (collapsible-leaf/c (list proj) (list ctc) (list blame) (list #f)))

;; Allow the bailout to be passed as an optional to avoid
;; an extra indirection through the property when possible
(define (collapsible->leaf c neg-party [bail #f])
  (cond
    [(collapsible-leaf/c? c) c]
    [else
     (define bailout (or bail (get-bail c)))
     (collapsible-leaf/c
      (list bailout)
      (list #f) ;; Bail out of ctc comparison when we see #f
      (list (collapsible-ho/c-latest-blame c))
      (list neg-party))]))

;; Apply a list of projections over a value
(define (apply-proj-list proj-list missing-parties val neg-party)
  (for/fold ([val* val])
            ([proj (in-list proj-list)]
             [missing-party (in-list missing-parties)])
    (proj val* (or missing-party neg-party))))

;; checks whether the contract c is already implied by one of the
;; contracts in contract-list
(define (implied-by-one? contract-list c #:implies implies)
  (for/or ([e (in-list contract-list)])
    (implies e c)))

(define (leaf-implied-by-one? contract-list new-ctc)
  (and new-ctc
       (for/or ([old-ctc (in-list contract-list)])
         (and old-ctc
              (flat-contract-struct? new-ctc)
              (trusted-contract-struct-stronger? old-ctc new-ctc)))))

;; join two collapsible-leaf contracts
(define (join-collapsible-leaf/c new-collapsible new-neg old-collapsible old-neg)
  (define new-proj-list (collapsible-leaf/c-proj-list new-collapsible))
  (define new-flat-list (collapsible-leaf/c-contract-list new-collapsible))
  (define new-blame-list (collapsible-leaf/c-blame-list new-collapsible))
  (define new-missing-party-list (collapsible-leaf/c-missing-party-list new-collapsible))
  (define old-proj-list (collapsible-leaf/c-proj-list old-collapsible))
  (define old-flat-list (collapsible-leaf/c-contract-list old-collapsible))
  (define old-blame-list (collapsible-leaf/c-blame-list old-collapsible))
  ;; We have to traverse the list to add the new neg party where it is missing
  (define old-missing-party-list (add-missing-parties (collapsible-leaf/c-missing-party-list old-collapsible) old-neg))
  (define-values (not-implied-projs not-implied-flats not-implied-blames not-implied-missing-parties)
    (for/lists (_1 _2 _3 _4) ([new-proj (in-list new-proj-list)]
                              [new-flat (in-list new-flat-list)]
                              [new-blame (in-list new-blame-list)]
                              [new-missing-party (in-list new-missing-party-list)]
                              #:when (not (leaf-implied-by-one? old-flat-list new-flat)))
      (values new-proj new-flat new-blame (or new-missing-party new-neg))))
  (define res-flats  (fast-append old-flat-list not-implied-flats))
  (define res-blames (fast-append old-blame-list not-implied-blames))
  (define res-missings (fast-append old-missing-party-list not-implied-missing-parties))
  (define res-projs (fast-append old-proj-list not-implied-projs))
  (define-values (pruned-projs pruned-flats pruned-blames pruned-missings)
    (prune res-projs res-flats res-blames res-missings))
  (collapsible-leaf/c pruned-projs pruned-flats pruned-blames pruned-missings))

(define (add-missing-parties missing-parties new-neg-party)
  (for/list ([neg-party (in-list missing-parties)])
    (or neg-party new-neg-party)))

(define (calculate-drops flats)
  (define-values (to-drop _1 _2)
       (for/fold ([indices '()]
                  [seen (hasheq)]
                  [maybe-drop (hasheq)])
                 ([flat (in-list flats)]
                  [i (in-naturals)])
         (cond
           [(or (flat-contract-struct? flat) (chaperone-contract-struct? flat))
            (cond
              [(hash-ref seen flat #f)
               (define maybe-index (hash-ref maybe-drop flat #f))
               (cond
                 [maybe-index
                  (define new-maybe-drop (hash-set maybe-drop flat i))
                  (values (cons maybe-index indices) seen new-maybe-drop)]
                 [else
                  (define new-maybe-drop (hash-set maybe-drop flat i))
                  (values indices seen new-maybe-drop)])]
              [else
               (define new-seen (hash-set seen flat #t))
               (values indices new-seen maybe-drop)])]
           [else
            (values indices seen maybe-drop)])))
  to-drop)

(define (prune projs flats blames missings)
  (cond
    [((length flats) . <= . 10)
     (define to-drop (calculate-drops flats))
     (for/lists (_1 _2 _3 _4) ([proj (in-list projs)]
                               [flat (in-list flats)]
                               [blame (in-list blames)]
                               [missing (in-list missings)]
                               [i (in-naturals)]
                               #:when (not (memv i to-drop)))
       (values proj flat blame missing))]
    [else (values projs flats blames missings)]))

;; A specialized version of append that will immediately return if either
;; argument is empty
(define (fast-append l1 l2)
  (cond
    [(null? l2) l1]
    [(null? l1) l2]
    [else
     (cons (car l1) (fast-append (cdr l1) l2))]))

;; Assuming that merging is symmetric, ie old-can-merge? iff new-can-merge?
;; This is true of the current c-c implementation, but if it ever changes
;; this function will neef to check both directions for merging
(define/merge-cache (merge new-c-c new-neg old-c-c old-neg)
  (define-values (new-try-merge new-proj) (get-merge-components new-c-c))
  (define-values (_ old-proj) (get-merge-components old-c-c))
  (or (new-try-merge new-c-c new-neg old-c-c old-neg)
      (join-collapsible-leaf/c (collapsible->leaf new-c-c new-neg new-proj)
                         new-neg
                         (collapsible->leaf old-c-c old-neg old-proj)
                         old-neg)))

(define (get-merge-components collapsible)
  (define prop (get-collapsible-contract-property collapsible))
  (define guard (collapsible-contract-property-collapsible-guard prop))
  (values
   (collapsible-contract-property-try-merge prop)
   ;; FIXME: don't really want to build a lambda here ...
   (λ (val neg) (guard collapsible val neg))))

(define (collapsible-guard collapsible val neg-party)
  (define prop (get-collapsible-contract-property collapsible))
  (define guard (collapsible-contract-property-collapsible-guard prop))
  (guard collapsible val neg-party))

(define (get-bail collapsible)
  (define prop (collapsible-contract-property collapsible))
  (define guard (collapsible-contract-property-collapsible-guard prop))
  ;; FIXME: don't really want to build this lambda ...
  (λ (val neg) (guard collapsible val neg)))

(define (first-order-check-join new-checks old-checks stronger?)
  (fast-append old-checks

               (for/list ([new (in-list new-checks)]
                          #:when (not (implied-by-one?
                                       old-checks new
                                       #:implies stronger?)))
                 new)))

(struct collapsible-property (c-c neg-party [ref #:mutable]))
(struct collapsible-count-property collapsible-property (count prev))
(struct collapsible-wrapper-property collapsible-property (checking-wrapper))

;; A Collapsible-Property is one of
;; - (collapsible-count-property collapsible?
;;                               neg-party?
;;                               impersonator?
;;                               natural-number/c
;;                               (or/c collapsible-count-property?
;;                                     (not/c collapsible-count-property?)))
;;     a count of the contracts currently attached to the value along with other
;;     necessary collapsible information
;; - (collapsible-wrapper-property collapsible? neg-party? impersonator? impersonator?)
;;     indicates this value is in collapsible mode, holds the attached collapsible contract,
;;     the most recent neg-party, a pointer to the
;;     last known collapsible wrapper, and the checking wrapper that has
;;     the collapsible interposition functions

(define (make-enter-collapsible-mode/direct
         make-checking-wrapper
         add-c-c-chaperone)
  (λ (c-c val neg-party chap-not-imp?)
    (define checking-wrapper (make-checking-wrapper val chap-not-imp?))
    (add-c-c-chaperone c-c c-c neg-party checking-wrapper chap-not-imp?)))

(define (make-enter-collapsible-mode/continue
         try-merge
         add-c-c-chaperone
         bail)
  (λ (new-c-c val new-neg-party c-c neg-party checking-wrapper chap-not-imp?)
    (define merged-c-c (try-merge new-c-c new-neg-party c-c neg-party))
    (cond
      [merged-c-c
       ;; Passing #f as the new-neg seems ugly, need to do more to fix this plumbing
       (add-c-c-chaperone merged-c-c new-c-c #f checking-wrapper chap-not-imp?)]
      [else (bail new-c-c val new-neg-party)])))

(define (make-enter-collapsible-mode/collapse
         make-unsafe-checking-wrapper
         add-c-c-chaperone
         try-merge
         bail)
  (λ (c-c val neg-party c-c-prop chap-not-imp?)
    (define-values (merged-c-c checking-wrapper)
      (let loop ([left c-c]
                 [left-neg neg-party]
                 [prop c-c-prop])
        (cond
          [left
           (define right (collapsible-property-c-c prop))
           (define right-neg (collapsible-property-neg-party prop))
           (define prev (collapsible-count-property-prev prop))
           (define merged (try-merge left left-neg right right-neg))
           (cond
             ;; there is another contract underneath this one
             [(collapsible-count-property? prev)
              (loop merged #f prev)]
             ;; we've reached the bottom of the contract stack
             [else
              (define checking-wrapper
                (make-unsafe-checking-wrapper val prev chap-not-imp?))
              (values merged checking-wrapper)])]
          ;; a merge failed, so we should return immediately
          ;; indicating the failure
          [else (values #f #f)])))
    (cond
      [merged-c-c
       (add-c-c-chaperone merged-c-c c-c neg-party checking-wrapper chap-not-imp?)]
      [else (bail c-c val neg-party)])))
