#lang racket/base
(require ffi/unsafe/atomic
         racket/private/place-local
         (rename-in racket/phase+space
                    [phase+space+ raw:phase+space+])
         "phase.rkt")

;; Terminology:
;;
;; See "phase.rkt" for an explanation of "<x>" vs. "<x> shift".
;;
;; In the case of spaces, the representations of "space" and
;; "space shift" are different:
;;
;;  * A standalone space is either #f or a symbol
;;
;;  * A standalone space shift is either #f, a symbol, or '#:none; a
;;    space shift of #f shifts other spaces to the default space,
;;    while a space shift of '#:none means that the space is left
;;    alone
;;
;;  Standalone space shifts are only ever represented internally. They
;;  are not part of the public API. The public API always combines a
;;  phase and space or a phase shift and a space shift:
;;
;;  * A phase and space is a pair, except that just the phase part is
;;    use if the space would be #f
;;
;;  * A phase and space is a pair, except that just the phase part is
;;    use if the space would be '#:none

(provide space?
         space+
         
         phase+space?
         intern-phase+space
         phase+space-place-init!

         phase+space-phase
         phase+space-space

         phase+space-shift?
         intern-phase+space-shift

         phase+space-shift-phase-level
         phase+space-shift-space-level ; '#:none => no delta
         phase+space+

         phase+space?-string
         phase+space-shift?-string

         phase+space<?

         has-default-space?)

(define-place-local interned (make-weak-hash))

(define (phase+space-place-init!)
  (set! interned (make-weak-hash)))

;; used for both phase+space and phase+space-shift
(define (intern new-key)
  (start-atomic)
  (define old-key (hash-ref-key interned new-key #f))
  (unless old-key
    (hash-set! interned new-key #t))
  (end-atomic)
  (or old-key new-key))

(define (space+ s s-level)
  (if (eq? s-level '#:none)
      s
      s-level))

(define intern-phase+space
  (case-lambda
    [(phase space)
     (cond
       [(not space) phase]
       [else (intern (cons phase space))])]
    [(phase+space)
     (intern phase+space)]))

(define intern-phase+space-shift
  (case-lambda
    [(phase-level space-level)
     (cond
       [(eq? space-level '#:none) phase-level]
       [else (intern (cons phase-level space-level))])]
    [(phase+space-shift)
     (intern phase+space-shift)]))

(define (phase+space-shift-phase-level level) (if (pair? level) (car level) level))
(define (phase+space-shift-space-level level) (if (pair? level) (car level) '#:none))

(define (phase+space+ p+s level)
  (intern-phase+space (raw:phase+space+ p+s level)))

;; For contract errors:
(define phase+space?-string "phase+space?")
(define phase+space-shift?-string "phase+space-shift?")

(define (phase+space<? a b)
  (define phase-a (phase+space-phase a))
  (define phase-b (phase+space-phase b))
  (or (phase<? phase-a phase-b)
      (cond
        [(eqv? phase-a phase-b)
         (define space-a (phase+space-space a))
         (define space-b (phase+space-space b))
         (or (not space-a)
             (and space-b
                  (symbol<? space-a space-b)))]
        [else #f])))

(define (has-default-space? p+s)
  (not (pair? p+s)))
