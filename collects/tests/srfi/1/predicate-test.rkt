;;;
;;; <predicate-test.rkt> ---- List predicate tests
;;; Time-stamp: <05/12/16 21:16:27 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; SRFI-1 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI-1 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI-1; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

;; Originally created by:

;; John David Stone
;; Department of Mathematics and Computer Science
;; Grinnell College
;; stone@math.grin.edu

(module predicate-test
  mzscheme
  (require rackunit)
  (require srfi/1/predicate
   srfi/1/cons)

  (provide predicate-tests)

  (define predicate-tests
    (test-suite
     "List predicate tests"

     ;; PROPER-LIST?

     (test-case
      "proper-list?:list"
      (check-true (proper-list? (list 1 2 3 4 5))))

     (test-case
      "proper-list?:dotted-list"
      (check-true (not (proper-list? (cons 1 (cons 2 (cons 3 4)))))))

     (test-case
      "proper-list?:zero-length"
      (check-true (proper-list? (list))))

     (test-case
      "proper-list?:circular-list"
      (check-true (not (proper-list? (circular-list 'a 'b 'c 'd)))))

     (test-case
      "proper-list?:simple-value"
      (check-true (not (proper-list? 1))))

     ;; DOTTED-LIST?

     (test-case
      "dotted-list?:dotted-list"
      (check-true (dotted-list? '(1 2 3 . 4))))

     (test-case
      "dotted-list?:proper-list"
      (check-true (not (dotted-list? (list 'a 'b 'c 'd)))))

     (test-case
      "dotted-list?:empty-list"
      (check-true (not (dotted-list? (list)))))

     (test-case
      "dotted-list?:simple-value"
      (check-true (dotted-list? "hello")))

     ;; CIRCULAR-LIST

     (test-case
      "circular-list?:proper-list"
      (check-true (not (circular-list? (list 1 2 3 4)))))

     (test-case
      "circular-list?:dotted-list"
      (check-true (not (circular-list? '(a b c . d)))))

     (test-case
      "circular-list?:simple-value"
      (check-true (not (circular-list? 1))))

     (test-case
      "circular-list?:circular-list"
      (check-true (circular-list? (circular-list 1 2 3 4))))

     ;; NOT-PAIR

     (test-case
      "not-pair?:list"
      (check-true (not (not-pair? (list 1 2 3 4)))))

     (test-case
      "not-pair?:number"
      (check-true (not-pair? 1)))

     (test-case
      "not-pair?:symbol"
      (check-true (not-pair? 'symbol)))

     (test-case
      "not-pair?:string"
      (check-true (not-pair? "string")))

     ;; NULL-LIST?

     (test-case
      "null-list?:null-list"
      (check-true (null-list? (list))))

     (test-case
      "null-list?:list"
      (check-true (not (null-list? (list 'a 'b 'c)))))

     (test-case
      "null-list?:pair"
      (check-true (not (null-list? (cons 1 2)))))

     ;; LIST=

     (test-case
      "list=:number-list"
      (check-true (list= = (list 1.0 2.0 3.0) (list 1 2 3))))

     (test-case
      "list=:symbol-vs-string-list"
      (check-true (list= (lambda (x y)
                            (string=? (symbol->string x) y))
                          (list 'a 'b 'c)
                          (list "a" "b" "c"))))

     (test-case
      "list=:unequal-lists"
      (check-true (not (list= eq? (list 1 2 3) (list 'a 'b 'c) (list 1 2 3)))))

     (test-case
      "list=:unequal-lengths"
      (check-true (not (list= eq? (list 1 2 3) (list 1 2 3 4)))))

     (test-case
      "list=:empty-lists"
      (check-true (list= eq? (list) (list) (list))))

     (test-case
      "list=:no-list"
      (check-true (list= eq?)))

     ))
  )
;;; predicate-test.rkt ends here
