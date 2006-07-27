;;; http://shootout.alioth.debian.org/
;;; Random implementation, by Jens Axel Sogaard
;;;
;;; Modified for proper string output by Brent Fulgham

(module random mzscheme
  (provide main)

  (define IM 139968)
  (define IA   3877)
  (define IC  29573)

  (define gen_random
    (let ((LAST 42))
      (lambda (max)
	(set! LAST (modulo (+ (* LAST IA) IC) IM))
	(/ (* max LAST) IM))))

  (define (roundto digits num)
    (let* ([e (expt 10 digits)]
	   [num (round (* e (inexact->exact num)))])
      (format "~a.~a"
	      (quotient num e)
	      (substring (string-append (number->string (remainder num e))
					(make-string digits #\0))
			 0 digits))))

  (define (main args)
    (let ((n (if (= (vector-length args) 0)
		 1
		 (string->number (vector-ref args 0)))))
      (let loop ((iter n))
	(if (> iter 1)
	    (begin
	      (gen_random 100.0)
	      (loop (- iter 1)))))
      (printf "~a~%"
	      (roundto 9 (gen_random 100.0)))))

  (main (current-command-line-arguments)))
