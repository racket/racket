#lang racket/base
(require "minimatch.rkt"
         "runtime-progress.rkt")
(provide (struct-out failure)

         expect?
         (struct-out expect:thing)
         (struct-out expect:atom)
         (struct-out expect:literal)
         (struct-out expect:message)
         (struct-out expect:disj)

         normalize-expectstack
         simplify-common-expectstacks
         maximal-failures
         partition/equal?)

;; A Failure is (make-failure PS ExpectStack)
;; A FailureSet is one of
;;   - Failure
;;   - (cons FailureSet FailureSet)

;; FailFunction = (FailureSet -> Answer)

(define-struct failure (progress expectstack) #:prefab)

;; == Expectations

;; FIXME: add phase to expect:literal

#|
An ExpectStack is (listof Expect)

An Expect is one of
  - (make-expect:thing string boolean)
  * (make-expect:message string)
  * (make-expect:atom atom)
  * (make-expect:literal identifier)
  * (make-expect:disj (non-empty-listof Expect))

The *-marked variants can only occur at the top of the stack.
|#
(define-struct expect:thing (description transparent?) #:prefab)
(define-struct expect:message (message) #:prefab)
(define-struct expect:atom (atom) #:prefab)
(define-struct expect:literal (literal) #:prefab)
(define-struct expect:disj (expects) #:prefab)

(define (expect? x)
  (or (expect:thing? x)
      (expect:message? x)
      (expect:atom? x)
      (expect:literal? x)
      (expect:disj? x)))


;; == Failure simplification ==

;; maximal-failures : FailureSet -> (listof (listof Failure))
(define (maximal-failures fs)
  (define ann-failures
    (for/list ([f (in-list (flatten fs null))])
      (cons f (invert-ps (failure-progress f)))))
  (maximal/progress ann-failures))

(define (flatten fs onto)
  (cond [(pair? fs)
         (flatten (car fs) (flatten (cdr fs) onto))]
        [else
         (cons fs onto)]))

;; == Expectation simplification ==

;; normalize-expectstack : ExpectStack -> ExpectStack
(define (normalize-expectstack es)
  (filter-expectstack (truncate-opaque-expectstack es)))

;; truncate-opaque-expectstack : ExpectStack -> ExpectStack
;; Eliminates expectations on top of opaque (ie, transparent=#f) frames.
(define (truncate-opaque-expectstack es)
  (let/ec return
    (let loop ([es es])
      (match es
        ['() '()]
        [(cons (expect:thing description '#f) rest-es)
         ;; Tricky! If multiple opaque frames, multiple "returns",
         ;; but innermost one called first, so jumps past the rest.
         (return (cons (car es) (loop rest-es)))]
        [(cons expect rest-es)
         (cons expect (loop rest-es))]))))

;; filter-expectstack : ExpectStack -> ExpectStack
;; Eliminates missing (ie, #f) messages and descriptions
(define (filter-expectstack es)
  (filter (lambda (expect)
            (match expect
              [(expect:thing '#f _)
               #f]
              [(expect:message '#f)
               #f]
              [_ #t]))
          es))

#|
Simplification dilemma

What if we have (e1 e2) and (e2)? How do we report that?
Options:
  1) consider them separate
  2) simplify to (e2), drop e1

Big problem with Option 1:
  eg (x:id ...) matching #'1 yields
  (union (failure #:progress () #:expectstack ())
         (failure #:progress () #:expectstack (#s(expect:atom ()))))
but we don't want to see "expected ()"

So we go with option 2.
|#

;; simplify-common-expectstacks : (listof ExpectStack) -> (listof ExpectStack)
;; Should call remove-duplicates first.
(define (simplify-common-expectstacks ess)
  ;; simplify : (listof ReversedExpectStack) -> (listof ReversedExpectStack)
  (define (simplify ress)
    (let ([ress-partitions (partition/car ress)])
      (if ress-partitions
          (apply append
                 (for/list ([ress-partition (in-list ress-partitions)])
                   (let ([proto-frame (car (car ress-partition))]
                         [cdr-ress (map cdr ress-partition)])
                     (map (lambda (res) (cons proto-frame res))
                          (simplify/check-leafs cdr-ress)))))
          (list null))))
  ;; simplify/check-leafs : (listof ReversedExpectStack) -> (listof ReversedExpectStack)
  (define (simplify/check-leafs ress)
    (let ([ress (simplify ress)])
      (cond [(andmap singleton? ress)
             (let* ([frames (map car ress)])
               (list (list (if (singleton? frames)
                               (car frames)
                               (expect:disj frames)))))]
            [else ress])))
  ;; singleton? : list -> boolean
  (define (singleton? res)
    (and (pair? res) (null? (cdr res))))
  (map reverse (simplify/check-leafs (map reverse ess))))

;; partition/car : (listof list) -> (listof (listof list))/#f
;; Returns #f if any of lists is empty.
(define (partition/car lists)
  (and (andmap pair? lists)
       (partition/equal? lists car)))

(define (partition/equal? items key)
  (let ([r-keys null] ;; mutated
        [key-t (make-hash)])
    (for ([item (in-list items)])
      (let ([k (key item)])
        (let ([entry (hash-ref key-t k null)])
          (when (null? entry)
            (set! r-keys (cons k r-keys)))
          (hash-set! key-t k (cons item entry)))))
    (let loop ([r-keys r-keys] [acc null])
      (cond [(null? r-keys) acc]
            [else
             (loop (cdr r-keys)
                   (cons (reverse (hash-ref key-t (car r-keys)))
                         acc))]))))

;; ==== Debugging

(provide failureset->sexpr
         failure->sexpr
         expectstack->sexpr
         expect->sexpr)

(define (failureset->sexpr fs)
  (let ([fs (flatten fs null)])
    (case (length fs)
      ((1) (failure->sexpr (car fs)))
      (else `(union ,@(map failure->sexpr fs))))))

(define (failure->sexpr f)
  (match f
    [(failure progress expectstack)
     `(failure ,(progress->sexpr progress)
               #:expected ,(expectstack->sexpr expectstack))]))

(define (expectstack->sexpr es)
  (map expect->sexpr es))

(define (expect->sexpr e)
  e)
