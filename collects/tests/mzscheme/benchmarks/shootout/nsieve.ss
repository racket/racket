#!/usr/bin/mzscheme -qu
;; $Id: nsieve-mzscheme.code,v 1.6 2006/06/10 23:38:29 bfulgham Exp $
;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; nsieve benchmark for The Computer Language Shootout
;; Written by Dima Dorfman, 2004
;; Converted to MzScheme by Brent Fulgham

(module nseive mzscheme
  (require (only (lib "13.ss" "srfi") string-index string-pad))

  (define (nsieve m)
    (let ((a (make-vector m #t)))
      (let loop ((i 2) (n 0))
        (if (< i m)
            (begin
	      (if (vector-ref a i)
	          (begin
		    (let clear ((j (+ i i)))
		      (if (< j m)
		          (begin
			    (vector-set! a j #f)
			    (clear (+ j i)))))
		      (loop (+ 1 i) (+ 1 n)))
		  (loop (+ 1 i) n)))
	    n))))

  (define (test n)
    (let* ((m (* (expt 2 n) 10000))
           (count (nsieve m)))
      (printf "Primes up to ~a ~a~%"
              (string-pad (number->string m) 8)
              (string-pad (number->string count) 8))))

  (define (main args)
    (if (< (vector-length args) 1)
        (begin
          (display "An argument is required") (newline) 2)
        (let ((n (string->number (vector-ref args 0))))
	  (if (not n)
	      (begin
                (display "An integer is required") (newline) 2)
	      (begin
	        (if (>= n 0) (test n))
	        (if (>= n 1) (test (- n 1)))
	        (if (>= n 2) (test (- n 2)))
	         0)))))

  (main (current-command-line-arguments)))
