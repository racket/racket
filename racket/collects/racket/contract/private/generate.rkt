#lang racket/base

(require "rand.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt"
         racket/list)

(provide generate-env
         env-stash
         contract-random-generate
         contract-exercise
         generate/direct
         generate/choose
         make-generate-ctc-fail
         generate-ctc-fail?
         with-definitely-available-contracts
         can-generate/env?
         try/env)

(define (contract-exercise v [fuel 10])
  (define ctc (value-contract v))
  (when ctc
    (define-values (go ctcs) 
      (parameterize ([generate-env (make-hash)])
        ((contract-struct-exercise ctc) fuel)))
    (for ([x (in-range fuel)])
      (go v))))

;; a stash of values and the contracts that they correspond to
;; that generation has produced earlier in the process 
(define generate-env (make-parameter 'generate-env-not-currently-set))

;; (parameter/c (listof contract?))
;; contracts in this will definitely have values available
;; by the time generation happens; those values will be 
;; in the env-stash.
(define definitely-available-contracts (make-parameter '()))

; Adds a new contract and value to the environment if
; they don't already exist
(define (env-stash env ctc val)
  (define curvals (hash-ref env ctc '()))
  (hash-set! env ctc (cons val curvals)))

(define (with-definitely-available-contracts ctcs thunk)
  (parameterize ([definitely-available-contracts 
                   (append ctcs (definitely-available-contracts))])
    (thunk)))

; generate : contract int -> ctc value or error
(define (contract-random-generate ctc fuel [_fail #f])
  (define def-ctc (coerce-contract 'contract-random-generate ctc))
  (define proc
    (parameterize ([generate-env (make-hash)]) 
      (generate/choose def-ctc fuel)))
  (cond
    [proc (proc)]
    [_fail (_fail)]
    [else
     (error 'contract-random-generate
            "unable to construct any generator for contract: ~e"
            def-ctc)]))

;; generate/choose : contract? nonnegative-int -> (or/c #f (-> any/c))
; Iterates through generation methods until failure. Returns
; #f if no value could be generated
(define (generate/choose ctc fuel)
  (define direct (generate/direct ctc fuel))
  (define env-can? (can-generate/env? ctc))
  (define env (generate-env))
  (cond
    [direct
     (λ ()
       (define use-direct? (zero? (rand 2)))
       (if use-direct?
           (direct)
           (try/env ctc env direct)))]
    [env-can?
     (λ ()
       (try/env 
        ctc env
        (λ () (error 'generate/choose "internal generation failure"))))]
    [else #f]))

; generate/direct :: contract nonnegative-int -> (or/c #f (-> val))
;; generate directly via the contract's built-in generator, if possible
(define (generate/direct ctc fuel) ((contract-struct-generate ctc) fuel))
  
(define (try/env ctc env fail)
  (define available 
    (for/list ([(avail-ctc vs) (in-hash env)]
               #:when (contract-stronger? avail-ctc ctc)
               [v (in-list vs)])
      v))
  (cond
    [(null? available) (fail)]
    [else (oneof available)]))

(define (can-generate/env? ctc)
  (for/or ([avail-ctc (in-list (definitely-available-contracts))])
    (contract-stronger? avail-ctc ctc)))
