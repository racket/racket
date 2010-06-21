;; $Id: nsieve-mzscheme.code,v 1.6 2006/06/10 23:38:29 bfulgham Exp $
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; nsieve benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Converted to MzScheme by Brent Fulgham

#lang racket/base
(require racket/cmdline)

(define (nsieve m)
  (let ((a (make-vector m #t)))
    (let loop ((i 2) (n 0))
      (if (< i m)
          (if (vector-ref a i)
              (begin
                (let clear ((j (+ i i)))
                  (when (< j m)
                    (vector-set! a j #f)
                    (clear (+ j i))))
                (loop (+ 1 i) (+ 1 n)))
              (loop (+ 1 i) n))
          n))))

(define (string-pad s len)
  (string-append (make-string (- len (string-length s)) #\space)
                 s))

(define (test n)
  (let* ((m (* (expt 2 n) 10000))
         (count (nsieve m)))
    (printf "Primes up to ~a ~a\n"
            (string-pad (number->string m) 8)
            (string-pad (number->string count) 8))))

(define (main n)
  (when (>= n 0) (test n))
  (when (>= n 1) (test (- n 1)))
  (when (>= n 2) (test (- n 2))))

(command-line #:args (n) (main (string->number n)))
