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
  (cond
    [proc
     (define value
       (let/ec k
         (parameterize ([fail-escape (λ () (k contract-random-generate-fail))])
           (proc))))
     (cond
       [(contract-random-generate-fail? value)
        (if fail
            (fail #f)
            (error 'contract-random-generate
                   "unable generate a value satisfying: ~e"
                   def-ctc))]
       [else value])]
    [else 
     (if fail
         (fail #t)
         (error 'contract-random-generate
                "unable to construct any generator for contract: ~e"
                def-ctc))]))

;; generate/choose : contract? nonnegative-int -> (or/c #f (-> any/c))
; Iterates through generation methods until failure. Returns
; #f if no value could be generated
;; if it returns a thunk, the thunk will not return contract-random-generate-fail?
(define (contract-random-generate/choose ctc fuel)
  (define direct ((contract-struct-generate ctc) fuel))
  (define env-can? (can-generate/env? ctc))
  (define env (generate-env))
  (unless (contract-random-generate-env? env)
    (error 'contract-random-generate/choose 
           "expected to be called only during generation"))
  (cond
    [direct
     (λ ()
       (define to-try (list direct (λ () (try/env ctc env))))
       (let loop ([to-try (if (zero? (rand 2))
                              (reverse to-try)
                              to-try)])
         (cond
           [(null? to-try) ((fail-escape))]
           [else
            (define this-try ((car to-try)))
            (cond
              [(contract-random-generate-fail? this-try)
               (loop (cdr to-try))]
              [else
               this-try])])))]
    [env-can?
     (λ ()
       (define candidate (try/env ctc env))
       (when (contract-random-generate-fail? candidate)
         (error 'contract-random-generate/choose
                "internal generation failure; env should have had ~s but didn't"
                ctc))
       candidate)]
    [else #f]))
  
(define (try/env ctc env)
  (define env-hash (contract-random-generate-env-hash env))
  (define available 
    (for/list ([(avail-ctc vs) (in-hash env-hash)]
               #:when (contract-stronger? avail-ctc ctc)
               [v (in-list vs)])
      v))
  (cond
    [(null? available)
     contract-random-generate-fail]
    [else
     (oneof available)]))
       

(define (can-generate/env? ctc)
  (for/or ([avail-ctc (in-list (definitely-available-contracts))])
    (contract-stronger? avail-ctc ctc)))
