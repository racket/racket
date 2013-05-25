;;;
;;; <char-set-test.rkt> ---- Test driver for the SRFI-14 port: char-set
;;; Time-stamp: <06/06/09 16:38:25 nhw>
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

(module char-set-test mzscheme

  (require rackunit)
  (require srfi/14/char-set)
  (provide char-set-tests)

  (define char-set-tests
    (test-suite
     "Char-Set tests"

     (test-case
      "char-set? correct"
      (check-false (char-set? null))
      (check-true (char-set? (char-set #\a #\b #\c))))

     (test-case
      "char-set= equality"
      (let ((abc (char-set #\a #\b #\c)))
        (check-true (char-set= abc (char-set #\a #\b #\c)))))

     (test-case
      "char-set<= test 1 (strictly less than)"
      (check-true (char-set<= (char-set #\a #\b #\c)
                              (char-set #\a #\b #\c #\d))))

     (test-case
      "char-set<= test 2 (equal)"
      (check-true (char-set<= (char-set #\a #\b #\c)
                              (char-set #\a #\b #\c))))

     (test-case
      "Hash invariant (default bound)"
      (check-eqv? (char-set-hash char-set:ascii)
                  (char-set-hash char-set:ascii)))

     (test-case
      "Char set cursor test"
      (let ((abc (char-set #\a #\b #\c)))
        (check-equal?
         (let lp ((cur (char-set-cursor abc)) (ans '()))
           (if (end-of-char-set? cur) ans
               (lp (char-set-cursor-next abc cur)
                   (cons (char-set-ref abc cur) ans))))
         '(#\c #\b #\a))))

     (test-case
      "char-set-fold test 1 (cs members)"
      (check-equal? (char-set-fold cons '() (char-set #\a #\b #\c))
                    '(#\c #\b #\a)))

     (test-case
      "char-set-fold test 2 (cs size)"
      (check-eqv? (char-set-fold (lambda (c i) (+ i 1)) 0 (char-set #\a #\b #\c))
                  3))

     (test-case
      "char-set-fold test 3 (how many vowels in cs)"
      (check-eqv? (char-set-fold (lambda (c i) (if (vowel? c) (+ i 1) i))
                                 0 (char-set #\a #\b #\c))
                  1))

     (test-case
      "char-set-unfold test 1 (string-port->char-set)"
      (check-equal?
       ;; FIXME: We haven't see if char-set->string works, and we are using it.
       (char-set->string
        ((lambda (sp)
           (char-set-unfold eof-object? values
                            (lambda (x) (read-char sp))
                            (read-char sp)))
         (open-input-string "This is a simple test string to generate a very simple char set!")))
       "yvtsrponmlihgecaT! "))

     (test-case
      "char-set-for-each (dummy, really)"
      (begin
        (char-set-for-each (lambda (c)
                             (char? c)) (char-set #\a #\b #\c))
        (check-true #t)))

     (test-case
      "char-set-map test 1 (downcase)"
      (check-true
       (char-set=
        (char-set-map char-downcase (char-set #\A #\b #\C))
        (char-set #\a #\b #\c))))

     (test-case
      "char-set-copy (copy constructor)"
      (check-true
       (let* ((orig (char-set #\a #\b #\c))
              (copy (char-set-copy orig)))
         (and (char-set= orig copy)
              (not (char-set=
                    (char-set-difference orig (char-set #\a))
                    copy))))))

     (test-case
      "list->char-set test 1 (using empty char-set)"
      (check-true
       (char-set= (list->char-set '(#\a #\b #\c #\a #\b #\a))
                  (char-set #\a #\b #\c))))

     (test-case
      "list->char-set test 2 (adding to an existent char-set)"
      (check-true
       (let ((c (char-set #\a #\b)))
         (and
          (char-set= (list->char-set '(#\a #\b #\c #\a #\b #\a) c)
                     (char-set #\a #\b #\c))
          ;; It has to be non-destructive:
          (char-set= c (char-set #\a #\b))))))

     (test-case
      "list->char-set! (destructive addition)"
      (let ((c (char-set #\a #\b)))
         (check
          char-set=
          (list->char-set! '(#\a #\b #\c #\a #\b #\a) c)
          (char-set #\a #\b #\c))))

     (test-case
      "string->char-set test 1 (using empty char-set)"
      (check-true
       (char-set= (string->char-set "aaaabbaaccc")
                  (char-set #\a #\b #\c))))

     (test-case
      "string->char-set test 2 (using a non empty char-set)"
      (check-true
       (let ((c (char-set #\a #\b)))
         (and
          (char-set= (string->char-set "aaabbbaaaccc" c)
                     (char-set #\a #\b #\c))
          ;; It has to be non-destructive:
          (char-set= c (char-set #\a #\b))))))

     (test-case
      "string->char-set! (destructive addition)"
      (let ((c (char-set #\a #\b)))
         (check
          char-set=
          (string->char-set! "aaabbbaaaccc" c)
          (char-set #\a #\b #\c))))

     (test-case
      "char-set-filter test 1 (using an empty set)"
      (check-true
       (char-set= (char-set-filter vowel? (char-set #\a #\b #\a #\e #\i #\c #\c #\d))
                  (char-set #\a #\e #\i))))

     (test-case
      "char-set-filter test 2 (using a non empty char-set)"
      (check-true
       (let ((c (char-set #\a #\e)))
         (and
          (char-set= (char-set-filter vowel? (char-set #\a #\b #\a #\e #\i #\c #\c #\d) c)
                     (char-set #\a #\e #\i))
          ;; It has to be non destructive
          (char-set= c (char-set #\a #\e))))))

     (test-case
      "char-set-filter! test (using a non empty char-set)"
      (let ((c (char-set #\a #\e)))
         (check
          char-set=
          (char-set-filter! vowel? (char-set #\a #\b #\a #\e #\i #\c #\c #\d) c)
          (char-set #\a #\e #\i))))

     ;; MISSIGN:
     ;; ucs-range->char-set, ucs-range->char-set!
     ;; ->char-set

     (test-case
      "char-set-size test"
      (check-true (= (char-set-size (char-set #\a #\b #\c)) 3)))

     (test-case
      "char-set-count test"
      (check-true (= (char-set-count vowel? (char-set #\a #\b #\a #\e #\i #\z)) 3)))

     (test-case
      "char-set->list test"
      (check-true
       (let ((l (char-set->list (char-set #\a #\b #\c))))
         (and (pair? l) (= (length l) 3)))))
     (test-case
      "char-set->string test"
      (check-true
       (string=? (char-set->string (char-set #\a #\b #\c)) "cba")))

     (test-case
      "char-set-contains? test (if there)"
      (check-true (char-set-contains? (char-set #\a #\b #\c) #\b)))

     (test-case
      "char-set-contains? test (if not there)"
      (check-true (not (char-set-contains? (char-set #\a #\b #\c) #\z))))

     ))

  (define vowel?
    (lambda (v)
      (and (char? v)
           (or (char=? v #\a) (char=? v #\e) (char=? v #\i) (char=? v #\o) (char=? v #\u)))))

  )

;;; char-set-test.rkt ends here
