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
;;  * (eventually-set!ed v) - the identifier is eventually `set!`ed,
;;                            but the pass for determing mutation state
;;                            hasn't reached the `set!`, yet; this
;;                            distinction is useful for determining whether
;;                            a variable might be mutated too early
;;
;;  * #f (not mapped) - defined and never `set!`ed
;;
;; By the end of the `mutated-in-body` pass, only 'set!ed,
;; 'set!ed-too-early, 'implicitly-set!ed, 'too-early,
;; 'too-early/ready, 'not-ready (for exported but not defined) and #f
;; are possible for identifiers that are reachable by evaluation.

(provide too-early
         hash-ref-mutated
         hash-set-mutated!
         eventually-set!ed?
         eventually-set!ed-inner
         eventually-set!ed-init
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
(struct too-early (name set!ed?) #:authentic)

;; Should appear only during the "mutated.rkt" pass, communicates
;; "`set!`ed" to most analyses but does not interfere with the
;; mutation-detection pass itself
(struct eventually-set!ed (inner) #:authentic #:transparent)

;; For use in  "mutated.rkt" to build mutated table
(define (hash-ref-mutated mutated key default)
  (define v (hash-ref mutated key default))
  (if (eventually-set!ed? v)
      (eventually-set!ed-inner v)
      v))

;; For use in "mutated.rkt" when building mutated table
(define (hash-set-mutated! mutated key val)
  (define v (hash-ref mutated key #f))
  (cond
    [(eventually-set!ed? v)
     (hash-set! mutated key (eventually-set!ed val))]
    [(not val)
     (hash-remove! mutated key)]
    [else
     (hash-set! mutated key val)]))

(define eventually-set!ed-init (eventually-set!ed #f))

(define (delayed-mutated-state? v)
  (procedure? v))

(define (simple-mutated-state? v)
  (or (not v)
      (delayed-mutated-state? v)
      (eq? v 'too-early/ready)))

(define (not-ready-mutated-state? v)
  (eq? v 'not-ready))

(define (too-early-mutated-state? v)
  (or (eq? v 'not-ready)
      (eq? v 'too-early)
      (eq? v 'set!ed-too-early)
      (eq? v 'undefined)
      (too-early? v)
      (and (eventually-set!ed? v)
           (too-early-mutated-state? (eventually-set!ed-inner v)))))

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
           (too-early-set!ed? v))
      (eventually-set!ed? v)))

(define (state->set!ed-state v)
  (cond
    [(too-early? v)
     (struct-copy too-early v [set!ed? #t])]
    [(eq? v 'not-ready) 'set!ed-too-early]
    [(too-early-mutated-state? v) 'set!ed-too-early]
    [(eq? v 'implicitly-set!ed) v]
    [else 'set!ed]))
