#lang racket/base

;; During the `mutated-in-body` pass, an identifier is mapped to one
;; of the following:
;;
;;  * 'set!ed - the identifier is `set!`ed
;;
;;  * 'too-early - the identifier may be referenced before it is
;;                 defined
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
;; By the end of the `mutated-in-body` pass, only 'set!ed, 'too-early,
;; 'not-ready (for exported but not defined) and #f are possible for
;; identifiers that are reachable by evaluation.

(provide delayed-mutated-state?
         simple-mutated-state?
         not-ready-mutated-state?
         via-variable-mutated-state?
         set!ed-mutated-state?)

(define (delayed-mutated-state? v) (procedure? v))

(define (simple-mutated-state? v)
  (or (not v)
      (delayed-mutated-state? v)))

(define (not-ready-mutated-state? v)
  (eq? v 'not-ready))

;; When referecing an exported identifier, we need to consistently go
;; through a `variable` record when it can be `set!`ed. We don't need
;; to go through a `variable` record if the identifier might simply be
;; used too early, because the host Scheme takes care of that issue.
(define (via-variable-mutated-state? v)
  (or (eq? v 'set!ed)
      (eq? v 'undefined)))

;; At the end of a linklet, known-value information is reliable unless
;; the identifier is mutated
(define (set!ed-mutated-state? v)
  (eq? v 'set!ed))
