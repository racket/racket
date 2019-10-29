#lang racket/base

;; During the `mutated-in-body` pass, an identifier is mapped to one
;; of the following:
;;
;;  * 'set!ed - the identifier is `set!`ed
;;
;;  * 'set!ed-too-early - the identifier is `set!`ed potentially
;;                        before it is initialized
;;
;;  * 'implicitly-set!ed - the `letrec`-bound identifier maybe be
;;                         implicitly `set!`ed via `call/cc`
;;
;;  * 'too-early - the identifier may be referenced before it is
;;                 defined
;;
;;  * 'too-early/ready - a variant of 'too-early where the variable
;;                 is now definitely ready, used only for top levels
;;
;;  * 'not-ready - the identifier's value is not yet ready, so a
;;                 reference transitions to 'too-early
;;
;;  * 'undefined - the identifier is "exported" from the linklet, but
;;                 not defined
;;
;;  * a thunk - the identifier is defined, where evaluation of the
;;              definition is side-efect-free; force the thunk on a
;;              first use, since anything referenced by the thunk
;;              might be first used at that point
;;
;;  * #f (not mapped) - defined and never `set!`ed
;;
;; By the end of the `mutated-in-body` pass, only 'set!ed,
;; 'set!ed-too-early, 'implicitly-set!ed, 'too-early,
;; 'too-early/ready, 'not-ready (for exported but not defined) and #f
;; are possible for identifiers that are reachable by evaluation.

(provide too-early
         delayed-mutated-state?
         simple-mutated-state?
         not-ready-mutated-state?
         too-early-mutated-state?
         too-early-mutated-state-name
         needs-letrec-convert-mutated-state?
         via-variable-mutated-state?
         set!ed-mutated-state?
         state->set!ed-state)

;; Used for `letrec` bindings to record a name:
(struct too-early (name set!ed?))

(define (delayed-mutated-state? v) (procedure? v))

(define (simple-mutated-state? v)
  (or (not v)
      (delayed-mutated-state? v)
      (eq? v 'too-early/ready)))

(define (not-ready-mutated-state? v)
  (eq? v 'not-ready))

(define (too-early-mutated-state? v)
  (or (eq? v 'too-early)
      (eq? v 'set!ed-too-early)
      (eq? v 'undefined)
      (too-early? v)))

(define (too-early-mutated-state-name v default-sym)
  (if (too-early? v)
      (too-early-name v)
      default-sym))

(define (needs-letrec-convert-mutated-state? v)
  (or (too-early? v)
      (eq? v 'too-early)
      (eq? v 'too-early/ready)
      (eq? v 'implicitly-set!ed)))

;; When referencing an exported identifier, we need to consistently go
;; through a `variable` record when it can be `set!`ed or is not yet
;; ready (as indicated by 'too-early, which is changed to 'too-eary/ready
;; as the variable becomes ready)
(define (via-variable-mutated-state? v)
  (or (eq? v 'set!ed)
      (eq? v 'undefined)
      (eq? v 'too-early)
      (eq? v 'set!ed-too-early)))

;; At the end of a linklet, known-value information is reliable unless
;; the identifier is explicitly mutated
(define (set!ed-mutated-state? v)
  (or (eq? v 'set!ed)
      (eq? v 'set!ed-too-early)
      (and (too-early? v)
           (too-early-set!ed? v))))

(define (state->set!ed-state v)
  (cond
    [(too-early? v)
     (struct-copy too-early v [set!ed? #t])]
    [(eq? v 'not-ready) 'set!ed-too-early]
    [(too-early-mutated-state? v) 'set!ed-too-early]
    [(eq? v 'implicitly-set!ed) v]
    [else 'set!ed]))
