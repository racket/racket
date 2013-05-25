#lang scheme/base

(provide print-with-values)

; This is like the printf procedures---it uses `print-string' to print
; the string portions, and print-formatted to print the values
; referenced via ~F. ~<w> is not supported.

(define (print-with-values fstring print-string print-formatted
			   . vals)
  (let ((size (string-length fstring)))
    (let loop ((start 0)
	       (i 0)
	       (vals vals)
	       (seen-vals '())) ; reversed
      (cond
       ((>= i size)
	(print-string (apply format (substring fstring start i) (reverse seen-vals))))
       ((char=? (string-ref fstring i) #\~)
	(case (string-ref fstring (+ 1 i))
	  ((#\n #\~) (loop start (+ 1 i) vals seen-vals))
	  ((#\F #\f)
	   (print-string (apply format (substring fstring start i) (reverse seen-vals)))
	   (print-formatted (car vals))
	   (loop (+ 2 i) (+ 2 i) (cdr vals) '()))
	  (else
	   (loop start (+ 2 i) (cdr vals) (cons (car vals) seen-vals)))))
       (else
	(loop start (+ 1 i) vals seen-vals))))))
	  

