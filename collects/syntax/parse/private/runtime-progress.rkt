#lang racket/base
(require racket/list
         "minimatch.rkt")
(provide ps-empty
         ps-add-car
         ps-add-cdr
         ps-add-post
         ps-add-stx
         ps-add-unbox
         ps-add-unvector
         ps-add-unpstruct
         ps-add-opaque

         ps-pop-opaque
         ps-context-syntax
         ps-difference

         (struct-out failure)
         expect?
         (struct-out expect:thing)
         (struct-out expect:atom)
         (struct-out expect:literal)
         (struct-out expect:message)
         (struct-out expect:disj)

         es-add-thing
         es-add-message
         es-add-atom
         es-add-literal)

;; FIXME: add phase to expect:literal

;; == Failure ==

#|
A Failure is (failure PS ExpectStack)

A FailureSet is one of
  - Failure
  - (cons FailureSet FailureSet)

A FailFunction = (FailureSet -> Answer)
|#
(define-struct failure (progress expectstack) #:prefab)


;; == Progress ==

#|
Progress (PS) is a non-empty list of Progress Frames (PF).

A Progress Frame (PF) is one of
  - stx     ;; "Base" frame
  - 'car    ;; car of pair; also vector->list, unbox, struct->list, etc
  - nat     ;; Represents that many repeated cdrs
  - 'post
  - 'opaque

The error-reporting context (ie, syntax-parse #:context arg) is always
the final frame.

All non-stx frames (eg car, cdr) interpreted as applying to nearest following
stx frame.

A stx frame is introduced
  - always at base (that is, by syntax-parse)
    - if syntax-parse has #:context arg, then two stx frames at bottom:
      (list to-match-stx context-stx)
  - by #:with/~parse
  - by #:fail-*/#:when/~fail & stx

Interpretation: later frames are applied first.
 eg, (list 'car 1 stx)
      means ( car of ( cdr once of stx ) )
      NOT apply car, then apply cdr once, then stop
|#

(define (ps-empty stx ctx)
  (if (eq? stx ctx)
      (list stx)
      (list stx ctx)))
(define (ps-add-car parent)
  (cons 'car parent))
(define (ps-add-cdr parent [times 1])
  (if (zero? times)
      parent
      (match (car parent)
        [(? exact-positive-integer? n)
         (cons (+ times n) (cdr parent))]
        [_
         (cons times parent)])))
(define (ps-add-post parent)
  (cons 'post parent))
(define (ps-add-stx parent stx)
  (cons stx parent))
(define (ps-add-unbox parent)
  (ps-add-car parent))
(define (ps-add-unvector parent)
  (ps-add-car parent))
(define (ps-add-unpstruct parent)
  (ps-add-car parent))
(define (ps-add-opaque parent)
  (cons 'opaque parent))

;; ps-context-syntax : Progress -> syntax
(define (ps-context-syntax ps)
  ;; Bottom frame is always syntax
  (last ps))

;; ps-difference : PS PS -> nat
;; Returns N s.t. B = (ps-add-cdr^N A)
(define (ps-difference a b)
  (define (whoops)
    (error 'ps-difference "~e is not an extension of ~e" a b))
  (match (list a b)
    [(list (cons (? exact-positive-integer? na) pa)
           (cons (? exact-positive-integer? nb) pb))
     (unless (equal? pa pb) (whoops))
     (- nb na)]
    [(list pa (cons (? exact-positive-integer? nb) pb))
     (unless (equal? pa pb) (whoops))
     nb]
    [_
     (unless (equal? a b) (whoops))
     0]))

;; ps-pop-opaque : PS -> PS
;; Used to continue with progress from opaque head pattern.
(define (ps-pop-opaque ps)
  (match ps
    [(cons (? exact-positive-integer? n) (cons 'opaque ps*))
     (cons n ps*)]
    [(cons 'opaque ps*)
     ps*]
    [_ (error 'ps-pop-opaque "opaque marker not found: ~e" ps)]))


;; == Expectations ==

#|
An ExpectStack (during parsing) is one of
  - (make-expect:thing Progress string boolean string/#f ExpectStack)
  * (make-expect:message string ExpectStack)
  * (make-expect:atom atom ExpectStack)
  * (make-expect:literal identifier ExpectStack)

The *-marked variants can only occur at the top of the stack.

Goal during parsing is to minimize/consolidate allocations.

During reporting, the representation changes somewhat:

An ExpectStack (during reporting) is (listof Expect)
An Expect is one of
  - (expect:thing (cons syntax nat) string #t string/#f _)
  * (expect:message string _)
  * (expect:atom atom _)
  * (expect:literal identifier _)
  - (expect:disj (non-empty-listof Expect) _)

That is, next link always ignored (replace with #f for sake of equal? cmp)
and expect:thing term represented as syntax with index.

Goal during reporting is ease of manipulation.
|#
(struct expect:thing (term description transparent? role next) #:prefab)
(struct expect:message (message next) #:prefab)
(struct expect:atom (atom next) #:prefab)
(struct expect:literal (literal next) #:prefab)
(struct expect:disj (expects next) #:prefab)

(define (expect? x)
  (or (expect:thing? x)
      (expect:message? x)
      (expect:atom? x)
      (expect:literal? x)
      (expect:disj? x)))

(define (es-add-thing ps description transparent? role next)
  (if description
      (expect:thing ps description transparent? role next)
      next))

(define (es-add-message message next)
  (if message
      (expect:message message next)
      next))

(define (es-add-atom atom next)
  (expect:atom atom next))

(define (es-add-literal literal next)
  (expect:literal literal next))
