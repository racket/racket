#lang racket/base
(require "rand.rkt")

(provide
  (rename-out [sngleton-maker make-generate-ctc-fail])
  generate-ctc-fail?
  find-generate
  
  get-arg-names-space
  gen-arg-names
  env-item
  env-item-name
  env-item-ctc
  
  predicate-generator-table
  exact-nonnegative-integer-gen)

 
;; generate 
(define-struct env-item (ctc name))

;; generate failure type
(define-struct generate-ctc-fail ())
(define a-generate-ctc-fail (make-generate-ctc-fail))
(define sngleton-maker
  (let ([make-generate-contract-fail
         (λ () a-generate-ctc-fail)])
    make-generate-contract-fail))

(define (gen-char fuel)
  (let* ([gen (oneof (list (rand-range 0 55295)
                           (rand-range 57344 1114111)))])
    (integer->char gen)))

(define (integer-gen fuel)
  (* (rand-choice [1/2 1.0] [else 1])
     (rand-choice [1/2 -1]  [else 1])
     (exact-nonnegative-integer-gen fuel)))

(define (exact-nonnegative-integer-gen fuel)
  (rand-choice
   [1/10 0]
   [1/10 1]
   [1/10 2147483647]
   [3/10 (rand-range 0 200)]
   [else (rand-range 0 2000000000)]))
  
(define (exact-positive-integer-gen fuel)
  (rand-choice
   [1/10 1]
   [1/10 2]
   [1/10 (oneof (list (expt 2 32) (expt 2 64) 
                      (- (expt 2 31)) (- (expt 2 63))
                      (- (expt 2 31) 1) (- (expt 2 63) 1)))]
   [3/10 (rand-range 1 200)]
   [else (rand-range 1 2000000000)]))
  
(define (rational-gen fuel)
  (/ (integer-gen fuel)
     (exact-positive-integer-gen fuel)))

(define predicate-generator-table 
  (hash
   ;; generate integer? 
   integer?
   integer-gen
   
   exact-nonnegative-integer?
   exact-nonnegative-integer-gen
   
   positive?
   (λ (fuel)
     (rand-choice
      [1/10 1]
      [1/10 1/3]
      [1/10 0.12]
      [1/10 2147483647]
      [else 4]))

   rational?
   rational-gen
   
   number?
   (λ (fuel)
     (rand-choice
      [1/10 (integer-gen fuel)]
      [1/10 (exact-nonnegative-integer-gen fuel)]
      [1/10 (+ (integer-gen fuel)
               (* 0+1i (integer-gen fuel)))]
      [else (rational-gen fuel)]))
   
   real?
   (λ (fuel)
     (rand-choice
      [1/10 (integer-gen fuel)]
      [1/10 (exact-nonnegative-integer-gen fuel)]
      [1/20 (oneof '(+inf.0 -inf.0 +nan.0 0 0.0))]
      [else (rational-gen fuel)]))
   
   
   boolean?
   (λ (fuel)
     (rand-choice
      [1/2 #t]
      [else #f]))
   
   char?
   gen-char
   
   string?
   (λ (fuel)
     (let* ([len (rand-choice [1/10 0]
                              [1/10 1]
                              [else (rand-range 2 260)])]
            [strl (build-list len (λ (x) (gen-char fuel)))])
       (apply string strl)))
   
   
   byte?
   (λ (fuel)
     (rand 256))
  
   bytes?
   (λ (fuel)
     (let* ([len (rand-choice
                  [1/10 0]
                  [1/10 1]
                  [else (+ 2 (rand 260))])]
            [bstr (build-list len (λ (x) (rand 256)))])
       (apply bytes bstr)))))


;; thread-cell
(define arg-names-count (make-thread-cell 0))

;; given a predicate returns a generate for this predicate or generate-ctc-fail
(define (find-generate func [name "internal"])
  (hash-ref predicate-generator-table func #f))

(define (get-arg-names-space space-needed)
  (let ([rv (thread-cell-ref arg-names-count)])
    (thread-cell-set! arg-names-count (+ rv space-needed))
    rv))

(define (gen-arg-names st-num size)
  (cond
    [(<= size 0) (list)]
    [else (cons (string->symbol (string-append "x-" (number->string st-num)))
                (gen-arg-names (+ st-num 1) (- size 1)))]))


