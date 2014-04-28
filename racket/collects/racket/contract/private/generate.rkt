#lang racket/base

(require "rand.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt"
         racket/list)

(provide generate-env
         env-stash
         contract-random-generate
         generate/direct
         generate/choose
         make-generate-ctc-fail
         generate-ctc-fail?
         with-definitely-available-contracts)

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
(define (contract-random-generate ctc fuel
                                  [fail (λ () 
                                          (error 'contract-random-generate
                                                 "unable to construct any generator for contract: ~e"
                                                 (coerce-contract 'contract-random-generate ctc)))])
  (define def-ctc (coerce-contract 'contract-random-generate ctc))
  (parameterize ([generate-env (make-hash)])
    (let ([proc (generate/choose def-ctc fuel)])
      (if proc
          (proc)
          (fail)))))

;; generate/choose : contract? nonnegative-int -> (or/c #f (-> any/c))
; Iterates through generation methods until failure. Returns
; #f if no value could be generated
(define (generate/choose ctc fuel)
  (let loop ([options (permute (list generate/direct generate/env))])
    (cond
      [(empty? options)
       #f]
      [else
       (define option (car options))
       (define gen (option ctc fuel))
       (or gen (loop (cdr options)))])))

; generate/direct :: contract nonnegative-int -> (or/c #f (-> val))
;; generate directly via the contract's built-in generator, if possible
(define (generate/direct ctc fuel) ((contract-struct-generate ctc) fuel))
  
; generate/direct-env :: contract nonnegative-int -> value
; Attemps to find a value with the given contract in the environment.
;; NB: this doesn't yet try to call things in the environment to generate
(define (generate/env ctc fuel)
  (define env (generate-env))
  (for/or ([avail-ctc (in-list (definitely-available-contracts))])
    (and (contract-stronger? avail-ctc ctc)
         (λ ()
           (define available 
             (for/list ([(avail-ctc vs) (in-hash env)]
                        #:when (contract-stronger? avail-ctc ctc)
                        [v (in-list vs)])
               v))
           (when (null? available)
             (error 'generate.rkt "internal error: no values satisfying ~s available" ctc))
           (oneof available)))))
