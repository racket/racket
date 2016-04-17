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
         (struct-out post)

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
         (struct-out expect:proper-pair)

         es-add-thing
         es-add-message
         es-add-atom
         es-add-literal
         es-add-proper-pair)

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
  - stx     ;; "Base" frame, or ~parse/#:with term
  - 'car    ;; car of pair; also vector->list, unbox, struct->list, etc
  - nat     ;; Represents that many repeated cdrs
  - #s(post group index) ;; late/post-traversal check, only comparable w/in group
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
(define-struct post (group index) #:prefab)

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
(define (ps-add-post parent [group #f] [index 0])
  (cons (post group index) parent))
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
There are multiple types that use the same structures, optimized for
different purposes.

-- During parsing, the goal is to minimize/consolidate allocations.

An ExpectStack (during parsing) is one of
  - (expect:thing Progress String Boolean String/#f ExpectStack)
  * (expect:message String ExpectStack)
  * (expect:atom Datum ExpectStack)
  * (expect:literal Identifier ExpectStack)
  * (expect:proper-pair FirstDesc ExpectStack)

The *-marked variants can only occur at the top of the stack (ie, not
in the next field of another Expect). The top of the stack contains
the most specific information.

-- During reporting, the goal is ease of manipulation.

An ExpectList (during reporting) is (listof Expect).

An Expect is one of
  - (expect:thing #f String #t String/#f StxIdx)
  * (expect:message String StxIdx)
  * (expect:atom Datum StxIdx)
  * (expect:literal Identifier StxIdx)
  * (expect:proper-pair FirstDesc StxIdx)
  * (expect:disj (NEListof Expect) StxIdx)
  - '...

A StxIdx is (cons Syntax Nat)

That is, the next link is replaced with the syntax+index of the term
being complained about. An expect:thing's progress is replaced with #f.

An expect:disj never contains a '... or another expect:disj.

We write ExpectList when the most specific information comes first and
RExpectList when the most specific information comes last.
|#
(struct expect:thing (term description transparent? role next) #:prefab)
(struct expect:message (message next) #:prefab)
(struct expect:atom (atom next) #:prefab)
(struct expect:literal (literal next) #:prefab)
(struct expect:disj (expects next) #:prefab)
(struct expect:proper-pair (first-desc next) #:prefab)

(define (expect? x)
  (or (expect:thing? x)
      (expect:message? x)
      (expect:atom? x)
      (expect:literal? x)
      (expect:disj? x)
      (expect:proper-pair? x)))

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

(define (es-add-proper-pair first-desc next)
  (expect:proper-pair first-desc next))

#|
A FirstDesc is one of
 - #f                   -- unknown, multiple possible, etc
 - string               -- description
 - (list 'any)
 - (list 'literal symbol)
 - (list 'datum datum)
|#
