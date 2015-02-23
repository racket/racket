;; ---------------------------------------------------------------------
;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Based on D language implementation by Dave Fladebo [imperative version]
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla
;; ---------------------------------------------------------------------

#lang racket/base
(require racket/cmdline)

(let ((n (exact->inexact
          (string->number
           (command-line #:args (n) n)))))
  
  (let loop ([d 0.0]
             (alt 1) (d2 0) (d3 0) (ds 0) (dc 0)
             (s0 0) (s1 0) (s2 0) (s3 0) (s4 0) (s5 0) (s6 0) (s7 0) (s8 0))
    (if (= d n)
        (let ([format-result
               (lambda (str n)
                 (printf str (real->decimal-string n 9)))])
          
          (format-result "~a\t(2/3)^k\n" s0)
          (format-result "~a\tk^-0.5\n" s1)
          (format-result "~a\t1/k(k+1)\n" s2)
          (format-result "~a\tFlint Hills\n" s3)
          (format-result "~a\tCookson Hills\n" s4)
          (format-result "~a\tHarmonic\n" s5)
          (format-result "~a\tRiemann Zeta\n" s6)
          (format-result "~a\tAlternating Harmonic\n" s7)
          (format-result "~a\tGregory\n" s8))
        
        (let* ((d (+ d 1))
               (d2 (* d d))
               (d3 (* d2 d))
               (ds (sin d))
               (dc (cos d))

               (s0 (+ s0 (expt (/ 2.0 3) (- d 1))))
               (s1 (+ s1 (/ 1 (sqrt d))))
               (s2 (+ s2 (/ 1 (* d (+ d 1)))))
               (s3 (+ s3 (/ 1 (* d3 (* ds ds)))))
               (s4 (+ s4 (/ 1 (* d3 (* dc dc)))))
               (s5 (+ s5 (/ 1 d)))
               (s6 (+ s6 (/ 1 d2)))
               (s7 (+ s7 (/ alt d)))
               (s8 (+ s8 (/ alt (- (* 2 d) 1))))
               (alt (- alt)))
          
	  (loop d
		alt d2 d3 ds dc
		s0 s1 s2 s3 s4 s5 s6 s7 s8)))))
