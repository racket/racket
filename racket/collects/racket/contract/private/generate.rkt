#lang racket/base

(require "rand.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt"
         racket/list)

(provide contract-random-generate
         contract-random-generate-stash
         contract-random-generate-get-current-environment
         contract-random-generate/choose
         contract-random-generate-env-hash
         contract-random-generate-env?
         contract-exercise
         generate/direct
         contract-random-generate-fail
         contract-random-generate-fail?
         with-definitely-available-contracts
         can-generate/env?
         try/env
         multi-exercise
         fail-escape)

(define (contract-exercise #:fuel [fuel 10] v1 . vs)
  (define vals 
    (for/list ([val (in-list (cons v1 vs))]
               #:when (value-contract val))
      val))
  (define ctcs (map value-contract vals))
  (define-values (go _) 
    (parameterize ([generate-env (contract-random-generate-env (make-hash))])
      ((multi-exercise ctcs) fuel)))
  (for ([x (in-range fuel)])
    (go vals)))

(define (contract-random-generate-get-current-environment)
  (define env (generate-env))
  (unless (contract-random-generate-env? env)
    (error 'get-current-contract-generation-environment
           "expected to be called only during generation"))
  env)

;; multi-exercise : (listof contract?) -> fuel -> (values (listof ctc) (-> (listof val[ctcs]) void)
(define (multi-exercise orig-ctcs)
  (λ (fuel)
    (let loop ([ctcs orig-ctcs]
               [exers '()]
               [previously-available-ctcs '()]
               [available-ctcs '()]
               [max-iterations 4])
      (cond
        [(null? ctcs)
         (cond
           [(or (zero? max-iterations)
                (equal? previously-available-ctcs available-ctcs))
            (define rev-exers (reverse exers))
            (values (λ (orig-vals)
                      (let loop ([exers rev-exers]
                                 [vals orig-vals])
                        (cond
                          [(null? exers) (void)]
                          [(null? vals) (loop exers orig-vals)]
                          [else
                           ((car exers) (car vals))
                           (loop (cdr exers) (cdr vals))])))
                    available-ctcs)]
           [else
            (loop orig-ctcs
                  exers
                  available-ctcs
                  available-ctcs
                  (- max-iterations 1))])]
        [else
         (define-values (exer newly-available-ctcs) 
           (with-definitely-available-contracts
            available-ctcs
            (λ ()
              ((contract-struct-exercise (car ctcs)) fuel))))
         (loop (cdr ctcs)
               (cons exer exers)
               previously-available-ctcs
               (add-new-contracts newly-available-ctcs available-ctcs)
               max-iterations)]))))

(define (add-new-contracts newly-available-ctcs available-ctcs)
  (let loop ([available-ctcs available-ctcs]
             [newly-available-ctcs newly-available-ctcs])
    (cond
      [(null? newly-available-ctcs) available-ctcs]
      [else
       (if (member (car newly-available-ctcs) available-ctcs)
           (loop available-ctcs
                 (cdr newly-available-ctcs))
           (loop (cons (car newly-available-ctcs) available-ctcs)
                 (cdr newly-available-ctcs)))])))
           

;; a stash of values and the contracts that they correspond to
;; that generation has produced earlier in the process 
(define generate-env (make-parameter #f))
(struct contract-random-generate-env (hash))
  
;; (parameter/c (listof contract?))
;; contracts in this will definitely have values available
;; by the time generation happens; those values will be 
;; in the env-stash.
(define definitely-available-contracts (make-parameter '()))

; Adds a new contract and value to the environment if
; they don't already exist
(define (contract-random-generate-stash env ctc val)
  (unless (contract-random-generate-env? env)
    (raise-argument-error 'contract-random-generate-stash
                          "contract-random-generate-env?"
                          0
                          env ctc val))
  (unless (contract-struct? ctc)
    (raise-argument-error 'contract-random-generate-stash
                          "contract?"
                          1
                          env ctc val))
  (define env-hash (contract-random-generate-env-hash env))
  (define curvals (hash-ref env-hash ctc '()))
  (hash-set! env-hash ctc (cons val curvals)))

(define (with-definitely-available-contracts ctcs thunk)
  (parameterize ([definitely-available-contracts 
                   (append ctcs (definitely-available-contracts))])
    (thunk)))

(define (contract-random-generate ctc [fuel 5] [_fail #f])
  (define def-ctc (coerce-contract 'contract-random-generate ctc))
  (unless (exact-nonnegative-integer? fuel)
    (raise-argument-error 'contract-random-generate
                          "exact-nonnegative-integer?"
                          fuel))
  (unless (or (not _fail) 
              (and (procedure? _fail) 
                   (or (procedure-arity-includes? _fail 0)
                       (procedure-arity-includes? _fail 1))))
    (raise-argument-error 'contract-random-generate
                          (format "~s" '(or/c #f (-> any) (-> boolean? any)))
                          3
                          ctc fuel _fail))
  (define fail
    (cond
      [(not _fail) #f]
      [(procedure-arity-includes? _fail 1) _fail]
      [else (λ (x) (_fail))]))
        
  (define proc
    (parameterize ([generate-env (contract-random-generate-env (make-hash))])
      (contract-random-generate/choose def-ctc fuel)))
  (define-values (success? value)
    (cond
      [proc 
       (let/ec k
         (parameterize ([fail-escape (λ () (k #f #f))])
           (values #t (proc))))]
      [else (values #f #f)]))
  (cond
    [(and success?
          (not (contract-random-generate-fail? value)))
     value]
    [fail (fail (not success?))]
    [else
     (if success?
         (error 'contract-random-generate
                "unable generate a value satisfying: ~e"
                def-ctc)
         (error 'contract-random-generate
                "unable to construct any generator for contract: ~e"
                def-ctc))]))

;; generate/choose : contract? nonnegative-int -> (or/c #f (-> any/c))
; Iterates through generation methods until failure. Returns
; #f if no value could be generated
;; if it returns a thunk, the thunk will not return contract-random-generate-fail?
(define (contract-random-generate/choose ctc fuel)
  (define direct (generate/direct ctc fuel))
  (define env-can? (can-generate/env? ctc))
  (define env (generate-env))
  (unless (contract-random-generate-env? env)
    (error 'contract-random-generate/choose 
           "expected to be called only during generation"))
  (cond
    [direct
     (λ ()
       (define use-direct? (zero? (rand 2)))
       (cond
         [use-direct?
          (define candidate (direct))
          (if (contract-random-generate-fail? candidate)
              (try/env ctc env direct)
              candidate)]
         [else (try/env ctc env direct)]))]
    [env-can?
     (λ ()
       (try/env 
        ctc env
        (λ () (error 'generate/choose "internal generation failure"))))]
    [else #f]))

;; generate/direct :: contract nonnegative-int -> (or/c #f (-> val))
;; generate directly via the contract's built-in generator, if possible
;; if it returns a thunk, the thunk will not return contract-random-generate-fail?
(define (generate/direct ctc fuel) 
  (define candidate ((contract-struct-generate ctc) fuel))
  (cond
    [(contract-random-generate-fail? candidate) ((fail-escape))]
    [else candidate]))
  
(define (try/env ctc env fail)
  (define env-hash (contract-random-generate-env-hash env))
  (define available 
    (for/list ([(avail-ctc vs) (in-hash env-hash)]
               #:when (contract-stronger? avail-ctc ctc)
               [v (in-list vs)])
      v))
  (cond
    [(null? available) (fail)]
    [else
     (oneof available)]))
       

(define (can-generate/env? ctc)
  (for/or ([avail-ctc (in-list (definitely-available-contracts))])
    (contract-stronger? avail-ctc ctc)))
