#lang racket/base

(require "private/unix-rand.rkt" "private/windows-rand.rkt"
         racket/contract/base racket/sequence racket/set)
(provide (contract-out [crypto-random-bytes (-> exact-nonnegative-integer? bytes?)]
                       [random-ref (->* (sequence?) (pseudo-random-generator?) any/c)]
                       [random-sample (->* (sequence? exact-nonnegative-integer?)
                                           (pseudo-random-generator?
                                            #:replacement? any/c)
                                           (listof any/c))]))

; (: crypto-random-bytes (-> Positive-Integer Bytes))
; returns n random bytes from the os.
(define (crypto-random-bytes n)
  (case (system-type 'os)
    [(unix macosx) (crypto-random-unix-bytes n)]
    [(windows) (crypto-random-windows-bytes n)]
    [else (raise (make-exn:fail:unsupported
                  "not supported on the current platform"
                  (current-continuation-marks)))]))

(define (random-ref seq [prng (current-pseudo-random-generator)])
  (define samples
    (random-sample/out-replacement seq 1 prng))
  (unless samples
    (raise-argument-error 'random-ref "non-empty sequence" seq))
  (vector-ref samples 0))

(define not-there (gensym))

(define (random-sample seq n [prng (current-pseudo-random-generator)]
                       #:replacement? [replacement? #t])
  ;; doing reservoir sampling, to do a single pass over the sequence
  ;; (some sequences may not like multiple passes, e.g., ports)
  (cond
   [(zero? n) '()]
   [(not replacement?)
    (define samples
      (random-sample/replacement seq n prng))
    ;; did we get enough?
    (unless (for/and ([s (in-vector samples)])
              (not (eq? s not-there)))
      (raise-argument-error 'random-sample
                            "integer less than or equal to sequence length"
                            1 seq n prng))
    (vector->list samples)]
   [else
    (define samples
      (random-sample/out-replacement seq n prng))
    (unless samples
      (raise-argument-error 'random-sample
                            "non-empty sequence for n>0"
                            0 seq n prng))
    (vector->list samples)]))

(define (random-sample/replacement seq n prng)
  ;; Based on: http://rosettacode.org/wiki/Knuth's_algorithm_S#Racket
  (define samples (make-vector n not-there))
  (for ([elt seq]
        [i   (in-naturals)])
    (cond [(< i n) ; we're not full, sample for sure
           (vector-set! samples i elt)]
          [(< (random (add1 i) prng) n) ; we've already seen n items; replace one?
           (vector-set! samples (random n prng) elt)]))
  samples)

(define (random-sample/out-replacement seq n prng)
  ;; similar to above, except each sample is independent
  (define samples #f)
  (for ([elt seq]
        [i   (in-naturals)])
    (cond [(= i 0) ; initialize samples
           (set! samples (make-vector n elt))]
          [else ; independently, maybe replace
           (for ([j (in-range n)])
             (when (zero? (random (add1 i) prng))
               (vector-set! samples j elt)))]))
  samples)
