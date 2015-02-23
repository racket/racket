#lang racket/base

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/

(require (for-syntax racket/base))
(require racket/cmdline)

(define O (current-output-port))

(define LIMIT-SQR 4.0)
(define ITERATIONS 50)
(define N (command-line #:args (n) (string->number n)))
(define N.0 (exact->inexact N))
(define 2/N (/ 2.0 N.0))
(define Crs
  (let ([v (make-vector N)])
    (for ([x (in-range N)])
      (vector-set! v x (- (/ (* 2 x) N.0) 1.5)))
    v))

(define-syntax (let-n stx)
  (syntax-case stx ()
    [(_ N bindings E)
     (let loop ([N (syntax-e #'N)] [E #'E])
       (if (zero? N) E (loop (sub1 N) #`(let bindings #,E))))]))

(define-syntax-rule (mandelbrot Cr Ci)
  (let loop ([i 0] [Zr 0.0] [Zi 0.0])
    (cond [(> (+ (* Zr Zr) (* Zi Zi)) LIMIT-SQR) 0]
          [(= i ITERATIONS) 1]
          [else (let-n 5 ([Zr (+ (- (* Zr Zr) (* Zi Zi)) Cr)]
                          [Zi (+ (* 2.0 (* Zr Zi)) Ci)])
                  (loop (+ i 5) Zr Zi))])))

(fprintf O "P4\n~a ~a\n" N N)
(let loop-y ([y N])
  (let ([Ci (- (* 2/N y) 1.0)])
    (let loop-x ([x 0] [bitnum 0] [byteacc 0])
      (if (< x N)
        (let* ([Cr (vector-ref Crs x)]
               [bitnum (+ bitnum 1)]
               [byteacc (+ (arithmetic-shift byteacc 1) (mandelbrot Cr Ci))])
          (cond [(= bitnum 8)
                 (write-byte byteacc O)
                 (loop-x (+ x 1) 0 0)]
                [else (loop-x (+ x 1) bitnum byteacc)]))
        (begin (when (> bitnum 0)
                 (write-byte (arithmetic-shift byteacc (- 8 (bitwise-and N #x7))) O))
               (when (> y 1) (loop-y (- y 1))))))))
