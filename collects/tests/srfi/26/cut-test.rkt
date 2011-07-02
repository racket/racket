;;;
;;; <cut-test.rkt> ---- SRFI 26 tests
;;; Time-stamp: <06/06/09 16:04:14 nhw>
;;;
;;; Usually, I would add a copyright notice, and the announce that
;;; this code is under the LGPL licence.  However, I only did the
;;; port to PLT Scheme, the original comment follows:

; CONFIDENCE TEST FOR IMPLEMENTATION OF SRFI-26
; =============================================
;
; Sebastian.Egner@philips.com, 3-Jun-2002.
;
; This file checks a few checks about the implementation.
; If you run it and no error message is issued, the implementation
; is correct on the cases that have been tested.
;
; compliance:
;   Scheme R5RS with
;     SRFI-23: error
;

; $Id: cut-test.rkt,v 1.1 2002/06/20 15:40:52 noel Exp $

(module cut-test mzscheme
  (require rackunit)
  (require srfi/26/cut)

  (provide cut-tests)

  (define cut-tests
    (test-suite
     "Cut (SRFI 26) Tests"
     (test-case
      "Cut test"
      (begin
        (check-equal? ((cut list)) '())
        (check-equal? ((cut list <...>)) '())
        (check-equal? ((cut list 1)) '(1))
        (check-equal? ((cut list <>) 1) '(1))
        (check-equal? ((cut list <...>) 1) '(1))
        (check-equal? ((cut list 1 2)) '(1 2))
        (check-equal? ((cut list 1 <>) 2) '(1 2))
        (check-equal? ((cut list 1 <...>) 2) '(1 2))
        (check-equal? ((cut list 1 <...>) 2 3 4) '(1 2 3 4))
        (check-equal? ((cut list 1 <> 3 <>) 2 4) '(1 2 3 4))
        (check-equal? ((cut list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
        (check-equal? 
         (let* ((x 'wrong) (y (cut list x)))
           (set! x 'ok) (y))
         '(ok))
        (check-equal? 
         (let ((a 0))
           (map (cut + (begin (set! a (+ a 1)) a) <>)
                '(1 2))
           a)
         2)))
     
     (test-case
      "Cute test"
      (begin
        (check-equal? ((cute list)) '())
        (check-equal? ((cute list <...>)) '())
        (check-equal? ((cute list 1)) '(1))
        (check-equal? ((cute list <>) 1) '(1))
        (check-equal? ((cute list <...>) 1) '(1))
        (check-equal? ((cute list 1 2)) '(1 2))
        (check-equal? ((cute list 1 <>) 2) '(1 2))
        (check-equal? ((cute list 1 <...>) 2) '(1 2))
        (check-equal? ((cute list 1 <...>) 2 3 4) '(1 2 3 4))
        (check-equal? ((cute list 1 <> 3 <>) 2 4) '(1 2 3 4))
        (check-equal? ((cute list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
        (check-equal? 
         (let ((a 0))
           (map (cute + (begin (set! a (+ a 1)) a) <>)
                '(1 2))
           a)
         1)))
     ))
  )

;;; cut-test.scm ends here
