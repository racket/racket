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
         generate-ctc-fail?)

; env parameter
(define generate-env (make-parameter #f))

; Adds a new contract and value to the environment if
; they don't already exist
(define (env-stash env ctc val)
  (let* ([curvals (hash-ref env ctc (list))])
    (hash-set! env ctc (cons val curvals))))

(define (gen-pred/direct pred fuel)
  (let ([ctc (coerce-contract 'contract-direct-gen pred)])
    (generate/direct ctc fuel)))

; generate : contract int -> ctc value or error
(define (contract-random-generate ctc fuel
                                  [fail (λ () 
                                          (error 'contract-random-generate
                                                 "Unable to construct any generator for contract: ~s"
                                                 (contract-struct-name 
                                                  (coerce-contract 'contract-random-generate ctc))))])
  (let ([def-ctc (coerce-contract 'contract-random-generate ctc)])
    (parameterize ([generate-env (make-hash)])
      ; choose randomly
      (let ([val (generate/choose def-ctc fuel)])
        (if (generate-ctc-fail? val)
            (fail)
            val)))))

; Iterates through generation methods until failure. Returns
; generate-ctc-fail if no value could be generated
(define (generate/choose ctc fuel)
  (let ([options (permute (list generate/direct
                                generate/direct-env
                                ))])
    ; choose randomly
    (let trygen ([options options])
      (if (empty? options)
          (make-generate-ctc-fail)
          (let* ([option (car options)]
                 [val (option ctc fuel)])
            (if (generate-ctc-fail? val)
                (trygen (cdr options))
                val))))))

; generate/direct :: contract int -> value for contract
; Attempts to make a generator that generates values for this contract
; directly. Returns generate-ctc-fail if making a generator fails.
(define (generate/direct ctc fuel)
  (let ([g (contract-struct-generate ctc)])
    ; Check if the contract has a direct generate attached
    (if (generate-ctc-fail? g)
        ; Everything failed -- we can't directly generate this ctc
        g 
        (g fuel))))

; generate/direct-env :: contract int -> value
; Attemps to find a value with the given contract in the environment.
; Returns it if found and generate-ctc-fail otherwise.
(define (generate/direct-env ctc fuel)
  ; TODO: find out how to make negative test cases
  (let* ([keys (hash-keys (generate-env))]
         [valid-ctcs (filter (λ (c)
                               (contract-stronger? c ctc))
                             keys)])
    (if (> (length valid-ctcs) 0)
        (oneof (oneof (map (λ (key)
                             (hash-ref (generate-env) key))
                           valid-ctcs)))
        (make-generate-ctc-fail))))

; generate/indirect-env :: contract int -> (int -> value for contract)
; Attempts to make a generator that generates values for this contract
; by calling functions in the environment
(define (generate/indirect-env ctc fuel)
  (if (> fuel 0)
      (make-generate-ctc-fail)
      (make-generate-ctc-fail)))

