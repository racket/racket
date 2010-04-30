;;;
;;; <string-test.ss> ---- SRFI-13 (string) tests
;;; Time-stamp: <06/06/09 16:05:08 nhw>
;;;
;;; Copyright (C) 2002 by Francisco Solsona. 
;;;
;;; This file is part of PLT SRFI.

;;; PLT SRFI is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; PLT SRFI is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with PLT SRFI; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

(module string-test mzscheme
  ;; Noel's Test Framework: (get your copy @ schematics.sourceforge.net)
  (require rktunit)
  (require srfi/13/string
           srfi/14/char-set
	   )
  (provide string-tests)

  (define string-tests
    (let ((abc null)
	  (cba null)
	  (test-string "This is a simple test string to generate a very simple char set!")
	  )
	    
      (test-suite
       "String tests"
       (test-case "string? test 1"
		       (check-true (string? test-string)))

       (test-case "string? test 2"
		       (check-true (not (string? 'hello))))
	     
       (test-case "string-null? test 1"
		       (check-true (string-null? "")))
	     
       (test-case "string-null? test 2"
		       (check-true (not (string-null? "not empty"))))

       (test-case "string-every test 1 (all #\a)"
		       (check-true (string-every  #\a "aaaaaaaa")))

       (test-case "string-every test 2 (charset a b c)"
		       (check-true (string-every
				     (char-set #\a #\b #\c)
				     "baacaaaabbaa")))

       (test-case "string-every test 3 (pred vowel?)"
		       (check-true (string-every vowel? "aeiou")))

	     
       ;; string-every char/char-set/pred s [start end] -> value
       ;; string-any char/char-set/pred s [start end] -> value
	     
	     
	     
       )))

  (define vowel?
    (lambda (v)
      (and (char? v)
	   (or (char=? v #\a) (char=? v #\e) (char=? v #\i) (char=? v #\o) (char=? v #\u)))))

  )

;;; string-test.ss ends here
