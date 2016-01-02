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
  (sequence-ref seq (random (sequence-length seq))))

(define (random-sample seq n [prng (current-pseudo-random-generator)]
                       #:replacement? [replacement? #t])
  (cond [replacement?
         (for/list ([i (in-range n)])
           (random-ref seq prng))]
        [else
         (unless (>= (sequence-length seq) n)
           (raise-argument-error 'random-sample
                                 "integer less than sequence length"
                                 n))
         (define l (sequence-length seq))
         ;; sequences don't necessarily support removal, so instead sample
         ;; indices without replacement, then index into the sequence
         (let loop ([res-idx (set)])
           (cond [(= (set-count res-idx) n) ; we have all we need, we're done
                  (for/list ([i (in-set res-idx)]) (sequence-ref seq i))]
                 [else
                  (loop (set-add res-idx (random l)))]))]))
