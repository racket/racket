;;;
;;; <mime-util.ss> ---- Extra utilities
;;; Time-stamp: <01/05/07 17:41:12 solsona>
;;;
;;; Copyright (C) 2001 by Francisco Solsona. 
;;;
;;; This file is part of mime-plt.

;;; mime-plt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; mime-plt is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with mime-plt; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

(module mime-util mzscheme
  (require (lib "etc.ss"))

  (provide string-tokenizer
           trim-all-spaces
           trim-spaces
           trim-comments
           lowercase
           warning
           cat)

  ;; string-index returns the leftmost index in string s
  ;; that has character c
  (define string-index
    (lambda (s c)
      (let ((n (string-length s)))
	(let loop ((i 0))
	  (cond ((>= i n) #f)
		((char=? (string-ref s i) c) i)
		(else (loop (+ i 1))))))))

  ;; string-tokenizer breaks string s into substrings separated by character c
  (define string-tokenizer
    (lambda (c s)
      (let loop ((s s))
	(if (string=? s "") '()
	    (let ((i (string-index s c)))
	      (if i (cons (substring s 0 i)
			  (loop (substring s (+ i 1)
					   (string-length s))))
		  (list s)))))))

    
  ;; Trim all spaces, except those in quoted strings.
  (define trim-all-spaces
    (lambda (str)
      (letrec ((eat-quoted-string (lambda (str)
				    (let* ((r (regexp "(\".*\")(.*)"))
					   (ans (regexp-match r str)))
				      (when (not ans)
					    ;; Uh oh, unterminated quoted string
					    ;; I will try to recover, but...
					    (values (substring str 0 1)
						    (substring str 1 (string-length str))))
				      (values (regexp-replace r str "\\1")
					      (regexp-replace r str "\\2"))))))					
	(let loop ((out "") (str str))
	  (cond ((string=? str "") out)
		(else
		 (let ((c (string-ref str 0)))
		   (cond ((or (char=? c #\space)
			      (char=? c #\tab))
			  (loop out (substring str 1 (string-length str))))
			 ((char=? c #\");; Begin of quoted string
			  (let-values ([(quoted rest)
					(eat-quoted-string str)])
				      (loop (string-append out quoted) rest)))
			 (else 
			  (loop (string-append out (string c))
				(substring str 1 (string-length str))))))))))))

  ;; Only trims left and right spaces:
  (define trim-spaces
    (lambda (str)
      (trim-right (trim-left str))))

  (define trim-left
    (lambda (str)
      (let* ((r (regexp "^[ 	]*(.*)"))
	     (ans (regexp-match r str)))
	(and ans (regexp-replace r str "\\1")))))

  (define trim-right
    (lambda (str)
      (let* ((r (regexp "[ 	]*$"))
	     (pos (regexp-match-positions r str)))
	(substring str 0 (caar pos)))))

  (define trim-comments
    (lambda (str)
      (let* ((reg (regexp "\\(.*\\)"))
	     (positions (regexp-match-positions reg str)))
	(if (regexp-match reg str)
	    (string-append (substring str 0 (caar positions))
			   (substring str (cdar positions) (string-length str)))
	    str))))

  (define lowercase
    (lambda (str)
      (let loop ((out "") (rest str) (size (string-length str)))
	(cond ((zero? size) out)
	      (else
	       (loop (string-append out (string
					 (char-downcase
					  (string-ref rest 0))))
		     (substring rest 1 size)
		     (sub1 size)))))))

  (define warning
    (lambda (msg . args)
      (fprintf (current-error-port)
	       (apply format (cons msg args)))
      (newline (current-error-port))))

  ;; Copies its input `in' to its ouput port if given, it uses
  ;; current-output-port if out is not provided.
  (define cat
    (opt-lambda (in (out (current-output-port)))
		(let loop ((ln (read-line in)))
		  (unless (eof-object? ln)
			  (fprintf out "~a~n" ln)
			  (loop (read-line in))))))

  )
;;; mime-util.ss ends here
