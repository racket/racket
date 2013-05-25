;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;
;; Adapted from CMUCL code by Dima Dorfman; bit-vector stuff by Alex Shinn;
;; cobbled together by felix, converted to MzScheme by Brent Fulgham

#lang racket/base
(require racket/cmdline)

(define (make-bit-vector size)
  (let* ((len (quotient (+ size 7) 8))
         (res (make-bytes len #b11111111)))
    (let ((off (remainder size 8)))
      (unless (zero? off)
        (bytes-set! res (- len 1) (- (arithmetic-shift 1 off) 1))))
    res))

(define (bit-vector-ref vec i)
  (let ((byte (arithmetic-shift i -3))
        (off (bitwise-and i #x7)))
    (and (< byte (bytes-length vec))
         (not (zero? (bitwise-and (bytes-ref vec byte)
                                  (arithmetic-shift 1 off)))))))

(define (bit-vector-set! vec i x)
  (let ((byte (arithmetic-shift i -3))
        (off (bitwise-and i #x7)))
    (let ((val (bytes-ref vec byte))
          (mask (arithmetic-shift 1 off)))
      (bytes-set! vec
                  byte
                  (if x
                      (bitwise-ior val mask)
                      (bitwise-and val (bitwise-not mask)))))))

(define (nsievebits m)
  (let ((a (make-bit-vector m)))
    (define (clear i)
      (do ([j (+ i i) (+ j i)])
          ((>= j m))
        (bit-vector-set! a j #f)))
    (let ([c 0])
      (do ([i 2 (add1 i)])
          ((>= i m) c)
        (when (bit-vector-ref a i)
          (clear i)
          (set! c (add1 c)))))))

(define (string-pad s len)
  (string-append (make-string (- len (string-length s)) #\space)
                 s))

(define (test n)
  (let* ((m (* (expt 2 n) 10000))
         (count (nsievebits m)))
    (printf "Primes up to ~a ~a\n"
            (string-pad (number->string m) 8)
            (string-pad (number->string count) 8))))

(define (main n)
  (when (>= n 0) (test n))
  (when (>= n 1) (test (- n 1)))
  (when (>= n 2) (test (- n 2))))

(command-line #:args (n) (main (string->number n)))
