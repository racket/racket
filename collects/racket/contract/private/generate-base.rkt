#lang racket/base
(require "rand.rkt")

(provide
  make-generate-ctc-fail
  generate-ctc-fail?
  find-generate
  
  get-arg-names-space
  gen-arg-names
  env-item
  env-item-name
  env-item-ctc)

 
;; generate 
(define-struct env-item (ctc name))

;; generate failure type
(define-struct generate-ctc-fail ())

(define (gen-char fuel)
  (let* ([gen (oneof (list (rand-range 0 55295)
                           (rand-range 57344 1114111)))])
    (integer->char gen)))
(define gen-hash 
  (hash
   ;; generate integer? 
   integer?
   (λ (fuel)
     (rand-choice
      [1/10 0]
      [1/10 1]
      [1/10 -1]
      [1/10 2147483647]
      [1/10 -2147483648]
      [3/10 (rand-range -100 200)]
      [else (rand-range -1000000000 2000000000)]))
   
   exact-nonnegative-integer?
   (λ (fuel)
     (abs ((find-generate integer?) fuel)))
   
   positive?
   (λ (fuel)
     (rand-choice
      [1/10 1]
      [1/10 1/3]
      [1/10 0.12]
      [1/10 2147483647]
      [else 4]))
   
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
           [bstr (build-list len
                             (λ (x)
                               (rand 256)))])
      (apply bytes bstr)))))


;; thread-cell
(define arg-names-count (make-thread-cell 0))

;; given a predicate returns a generate for this predicate or generate-ctc-fail
(define (find-generate func [name "internal"])
  (hash-ref gen-hash func make-generate-ctc-fail))

(define (get-arg-names-space space-needed)
  (let ([rv (thread-cell-ref arg-names-count)])
    (thread-cell-set! arg-names-count (+ rv space-needed))
    rv))

(define (gen-arg-names st-num size)
  (cond
    [(<= size 0) (list)]
    [else (cons (string->symbol (string-append "x-" (number->string st-num)))
                (gen-arg-names (+ st-num 1) (- size 1)))]))


