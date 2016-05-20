#lang racket/base

;; Common functionality used by all space-efficient contracts

(require "prop.rkt" "blame.rkt" "merge-cache.rkt")

(provide (struct-out multi-ho/c)
         (struct-out multi-leaf/c)
         (struct-out space-efficient-property)
         (struct-out space-efficient-count-property)
         (struct-out space-efficient-wrapper-property)
         build-space-efficient-leaf
         prop:space-efficient-contract
         build-space-efficient-contract-property
         space-efficient-contract?
         merge
         space-efficient-guard
         first-order-check-join
         log-space-efficient-value-bailout-info
         log-space-efficient-contract-bailout-info
         log-space-efficient-cache-fail-info
         make-enter-space-efficient-mode/continue
         make-enter-space-efficient-mode/collapse
         make-enter-space-efficient-mode/direct
         SPACE-EFFICIENT-LIMIT)

(module+ for-testing
  (provide multi-leaf/c?
           multi-leaf/c-contract-list
           multi-leaf/c-proj-list
           space-efficient-property-s-e
           space-efficient-property-ref
           has-impersonator-prop:space-efficient?
           get-impersonator-prop:space-efficient
           space-efficient-wrapper-property?
           space-efficient-wrapper-property-checking-wrapper
           calculate-drops))

;; object contracts need to propagate properties across procedure->method
(module+ properties
  (provide impersonator-prop:space-efficient
           has-impersonator-prop:space-efficient?
           get-impersonator-prop:space-efficient))

(define-logger space-efficient-value-bailout)
(define-logger space-efficient-contract-bailout)
(define-logger space-efficient-merging)
(define-logger space-efficient-cache-fail)

(define SPACE-EFFICIENT-LIMIT 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties
(define-values (impersonator-prop:space-efficient
                has-impersonator-prop:space-efficient?
                get-impersonator-prop:space-efficient)
  (make-impersonator-property 'impersonator-prop:space-efficient))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An interface for space-efficient contract conversion and merging
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct space-efficient-contract-property
  (try-merge
   space-efficient-guard)
  #:omit-define-syntaxes)

(define (space-efficient-contract-property-guard prop info)
  (unless (space-efficient-contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a space-efficient contract property; got: ~e"
              prop)
      (current-continuation-marks))))
  prop)

(define-values (prop:space-efficient-contract space-efficient-contract? get-space-efficient-contract-property)
  (make-struct-type-property 'space-efficient-contract space-efficient-contract-property-guard))

(define (build-space-efficient-contract-property
         #:try-merge [try-merge #f]
         #:space-efficient-guard
         [space-efficient-guard
          (lambda (ctc val)
            (error "internal error: contract does not support `space-efficient-guard`" ctc))])
  (space-efficient-contract-property
   (or try-merge (lambda (_1 _2 _3 _4) #f))
   space-efficient-guard))

;; Parent structure for higher order space-efficient contracts
;; which must keep track of the latest blame and missing party
;; and latest contract applied
(struct multi-ho/c (latest-blame missing-party latest-ctc))

(struct multi-leaf/c (proj-list contract-list blame-list missing-party-list)
  #:property prop:space-efficient-contract
  (build-space-efficient-contract-property
   #:try-merge (lambda (new new-neg old old-neg)
                 (and (multi-leaf/c? old)
                      (multi-leaf/c? new)
                      (join-multi-leaf/c new new-neg old old-neg)))
   #:space-efficient-guard
   (lambda (s-e val neg-party)
     (apply-proj-list (multi-leaf/c-proj-list s-e)
                      (multi-leaf/c-missing-party-list s-e)
                      val
                      neg-party))))

(define (build-space-efficient-leaf proj ctc blame)
  (multi-leaf/c (list proj) (list ctc) (list blame) (list #f)))

;; Allow the bailout to be passed as an optional to avoid
;; an extra indirection through the property when possible
(define (multi->leaf c neg-party [bail #f])
  (cond
    [(multi-leaf/c? c) c]
    [else
     (define bailout (or bail (get-bail c)))
     (multi-leaf/c
      (list bailout)
      (list #f) ;; Bail out of ctc comparison when we see #f
      (list (multi-ho/c-latest-blame c))
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
              (contract-struct-stronger? old-ctc new-ctc)))))

;; join two multi-leaf contracts
(define (join-multi-leaf/c new-multi new-neg old-multi old-neg)
  (define new-proj-list (multi-leaf/c-proj-list new-multi))
  (define new-flat-list (multi-leaf/c-contract-list new-multi))
  (define new-blame-list (multi-leaf/c-blame-list new-multi))
  (define new-missing-party-list (multi-leaf/c-missing-party-list new-multi))
  (define old-proj-list (multi-leaf/c-proj-list old-multi))
  (define old-flat-list (multi-leaf/c-contract-list old-multi))
  (define old-blame-list (multi-leaf/c-blame-list old-multi))
  ;; We have to traverse the list to add the new neg party where it is missing
  (define old-missing-party-list (add-missing-parties (multi-leaf/c-missing-party-list old-multi) old-neg))
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
  (multi-leaf/c pruned-projs pruned-flats pruned-blames pruned-missings))

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
;; This is true of the current s-e implementation, but if it ever changes
;; this function will neef to check both directions for merging
(define/merge-cache (merge new-s-e new-neg old-s-e old-neg)
  (define-values (new-try-merge new-proj) (get-merge-components new-s-e))
  (define-values (_ old-proj) (get-merge-components old-s-e))
  (or (new-try-merge new-s-e new-neg old-s-e old-neg)
      (join-multi-leaf/c (multi->leaf new-s-e new-neg new-proj)
                         new-neg
                         (multi->leaf old-s-e old-neg old-proj)
                         old-neg)))

(define (get-merge-components multi)
  (define prop (get-space-efficient-contract-property multi))
  (define guard (space-efficient-contract-property-space-efficient-guard prop))
  (values
   (space-efficient-contract-property-try-merge prop)
   ;; FIXME: don't really want to build a lambda here ...
   (λ (val neg) (guard multi val neg))))

(define (space-efficient-guard multi val neg-party)
  (define prop (get-space-efficient-contract-property multi))
  (define guard (space-efficient-contract-property-space-efficient-guard prop))
  (guard multi val neg-party))

(define (get-bail multi)
  (define prop (space-efficient-contract-property multi))
  (define guard (space-efficient-contract-property-space-efficient-guard prop))
  ;; FIXME: don't really want to build this lambda ...
  (λ (val neg) (guard multi val neg)))

(define (first-order-check-join new-checks old-checks stronger?)
  (fast-append old-checks

               (for/list ([new (in-list new-checks)]
                          #:when (not (implied-by-one?
                                       old-checks new
                                       #:implies stronger?)))
                 new)))

(struct space-efficient-property (s-e neg-party [ref #:mutable]))
(struct space-efficient-count-property space-efficient-property (count prev))
(struct space-efficient-wrapper-property space-efficient-property (checking-wrapper))

;; A Space-Efficient-Property is one of
;; - no-s-e-support -- indicicating that the value with this property does not support s-e mode
;; - (space-efficient-count-property impersonator?
;;                                   space-efficient?
;;                                   neg-party?
;;                                   natural-number/c
;;                                   (or/c space-efficient-count-property?
;;                                         (not/c space-efficient-count-property?)))
;;     a count of the contracts currently attached to the value along with other
;;     necessary space-efficient information
;; - (space-efficient-property impersonator? impersonator?)
;;     indicates this value is in space-efficient mode, holds a pointer to the
;;     last known space-efficient wrapper, and the checking wrapper that has
;;     the space-efficient interposition functions
;;    when this property is attached to a value there is also a
;;    impersonator-prop:merged property holding a (cons/c space-efficient? neg-party)

(define (make-enter-space-efficient-mode/direct
         make-checking-wrapper
         add-s-e-chaperone)
  (λ (s-e val neg-party chap-not-imp?)
    (define checking-wrapper (make-checking-wrapper val chap-not-imp?))
    (add-s-e-chaperone s-e s-e neg-party checking-wrapper chap-not-imp?)))

(define (make-enter-space-efficient-mode/continue
         try-merge
         add-s-e-chaperone
         bail)
  (λ (new-s-e val new-neg-party s-e neg-party checking-wrapper chap-not-imp?)
    (define merged-s-e (try-merge new-s-e new-neg-party s-e neg-party))
    (cond
      [merged-s-e
       ;; Passing #f as the new-neg seems ugly, need to do more to fix this plumbing
       (add-s-e-chaperone merged-s-e new-s-e #f checking-wrapper chap-not-imp?)]
      [else (bail new-s-e val new-neg-party)])))

(define (make-enter-space-efficient-mode/collapse
         make-unsafe-checking-wrapper
         add-s-e-chaperone
         try-merge
         bail)
  (λ (s-e val neg-party s-e-prop chap-not-imp?)
    (define-values (merged-s-e checking-wrapper)
      (let loop ([left s-e]
                 [left-neg neg-party]
                 [prop s-e-prop])
        (cond
          [left
           (define right (space-efficient-property-s-e prop))
           (define right-neg (space-efficient-property-neg-party prop))
           (define prev (space-efficient-count-property-prev prop))
           (define merged (try-merge left left-neg right right-neg))
           (cond
             ;; there is another contract underneath this one
             [(space-efficient-count-property? prev)
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
      [merged-s-e
       (add-s-e-chaperone merged-s-e s-e neg-party checking-wrapper chap-not-imp?)]
      [else (bail s-e val neg-party)])))
