#lang racket/base
(require racket/list
         unstable/struct
         syntax/stx
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

         invert-ps
         ps->stx+index
         ps-context-syntax
         ps-difference

         (struct-out failure)
         expect?
         (struct-out expect:thing)
         (struct-out expect:atom)
         (struct-out expect:literal)
         (struct-out expect:message)
         (struct-out expect:disj))

#|
Progress (PS) is a non-empty list of Progress Frames (PF).

A PF is one of
  - stx     ;; "Base" frame
  - 'car
  - nat     ;; Represents that many repeated cdrs
  - 'post
  - 'opaque

stx frame introduced
  - always at base (that is, by syntax-parse)
    - if syntax-parse has #:context arg, then two stx frames at bottom:
      (list to-match-stx context-stx)
  - by #:with/~parse
  - by #:fail-*/#:when/~fail & stx

Interpretation: Inner PS structures are applied first.
 eg, (list 'car 1 #'___)
      means ( car of ( cdr once of the term ) )
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
  (car (reverse ps)))

;; ps->stx+index : Progress -> (values stx nat)
;; Gets the innermost stx that should have a real srcloc, and the offset
;; (number of cdrs) within that where the progress ends.
(define (ps->stx+index ps)
  (define (interp ps)
    (match ps
      [(cons (? syntax? stx) _) stx]
      [(cons 'car parent)
       (let* ([d (interp parent)]
              [d (if (syntax? d) (syntax-e d) d)])
         (cond [(pair? d) (car d)]
               [(vector? d) (vector->list d)]
               [(box? d) (unbox d)]
               [(prefab-struct-key d) (struct->list d)]
               [else (error 'ps->stx+index "INTERNAL ERROR: unexpected: ~e" d)]))]
      [(cons (? exact-positive-integer? n) parent)
       (for/fold ([stx (interp parent)]) ([i (in-range n)])
         (stx-cdr stx))]
      [(cons 'post parent)
       (interp parent)]))
  (let ([ps (ps-truncate-opaque ps)])
    (match ps
      [(cons (? syntax? stx) _)
       (values stx 0)]
      [(cons 'car parent)
       (values (interp ps) 0)]
      [(cons (? exact-positive-integer? n) parent)
       (values (interp parent) n)]
      [(cons 'post parent)
       (ps->stx+index parent)])))

;; ps-difference : PS PS -> nat
;; Returns N s.t. B = (ps-add-cdr^N A)
(define (ps-difference a b)
  (define (whoops)
    (error 'ps-difference "~e is not an extension of ~e"
           (progress->sexpr b) (progress->sexpr a)))
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

;; ps-truncate-opaque : PS -> PS
(define (ps-truncate-opaque ps)
  (let/ec return
    (let loop ([ps ps])
      (cond [(null? ps)
             null]
            [(eq? (car ps) 'opaque)
             ;; Tricky! We only jump after loop returns,
             ;; so jump closest to end wins.
             (return (loop (cdr ps)))]
            [else
             ;; Either (loop _) jumps, or it is identity
             (loop (cdr ps))
             ps]))))

;; An Inverted PS (IPS) is a PS inverted for easy comparison.
;; An IPS may not contain any 'opaque frames.

;; invert-ps : PS -> IPS
(define (invert-ps ps)
  (reverse (ps-truncate-opaque ps)))


;; ==== Failure ====

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

(define (progress->sexpr ps)
  (for/list ([pf (in-list (invert-ps ps))])
    (match pf
      [(? syntax? stx) 'stx]
      ['car 'car]
      ['post 'post]
      [(? exact-positive-integer? n) n]
      ['opaque 'opaque])))
