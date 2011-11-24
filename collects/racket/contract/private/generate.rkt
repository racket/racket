#lang racket/base

(require "rand.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt"
         racket/list)

(provide generate-env
         env-stash

         contract-generate

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

;; hash tables
;(define freq-hash (make-hash))
;(define gen-hash (make-hash))

;; thread-cell
;(define arg-names-count (make-thread-cell 0))

;; generate integer? 
(add-generate integer?
           (λ (fuel)
             (rand-choice
              [1/10 0]
              [1/10 1]
              [1/10 -1]
              [1/10 2147483647]
              [1/10 -2147483648]
              [3/10 (rand-range -100 200)]
              [else (rand-range -1000000000 2000000000)])))

(add-generate exact-nonnegative-integer?
               (λ (fuel)
                 (abs ((find-generate integer?) fuel))))


(add-generate positive?
           (λ (fuel)
             (rand-choice
              [1/10 1]
              [1/10 1/3]
              [1/10 0.12]
              [1/10 2147483647]
              [else 4])))

(add-generate boolean?
    (λ (fuel)
       (rand-choice
         [1/2 #t]
         [else #f])))

(add-generate char?
  (λ (fuel)
     (let* ([gen (oneof (list (rand-range 0 55295)
                              (rand-range 57344 1114111)))])
       (integer->char gen))))

(add-generate string?
  (λ (fuel)
     (let* ([len (rand-choice [1/10 0]
                              [1/10 1]
                              [else (rand-range 2 260)])]
            [strl (build-list len
                              (λ (x)
                                 (gen-pred/direct char? fuel)))])
       (apply string strl))))

(add-generate byte?
  (λ (fuel)
     (rand 256)))

(add-generate bytes?
  (λ (fuel)
     (let* ([len (rand-choice
                   [1/10 0]
                   [1/10 1]
                   [else (+ 2 (rand 260))])]
            [bstr (build-list len
                              (λ (x)
                                 (rand 256)))])
       (apply bytes bstr))))

(define (gen-pred/direct pred fuel)
  (let ([ctc (coerce-contract 'contract-direct-gen pred)])
    (generate/direct ctc fuel)))

; generate : contract int -> ctc value or error
(define (contract-generate ctc fuel [fail (λ () 
                                            (error 'contract-generate
                                                   "Unable to construct any generator for contract: ~s"
                                                   (contract-struct-name (coerce-contract 'contract-generate ctc))))])
  (let ([def-ctc (coerce-contract 'contract-generate ctc)])
    (printf "def-ctc ~s\n" def-ctc)
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

