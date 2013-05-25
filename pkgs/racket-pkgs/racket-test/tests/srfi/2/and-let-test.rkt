;;;
;;; <and-let-test.rkt> ---- and-let* macro tests
;;; Time-stamp: <06/06/09 15:58:59 nhw>
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

(module and-let-test mzscheme
  (require rackunit)
  (require srfi/2/and-let)
  (provide and-let*-tests)

  (define and-let*-tests
    (test-suite
     "and-let* tests"
     (test-case "empty body 1"
                ;; check-true, check-eqv?, etc.
                (check-eqv? (and-let* () ) #t))

     (test-case "empty claws 1"
                (check-eqv? (and-let* () 1) 1))

     (test-case "empty claws 2"
                (check-eqv? (and-let* () 1 2) 2))

     (test-case "singleton claw 1"
                (check-eqv? (let ((x #f))
                              (and-let* (x)))
                            #f))

     (test-case "singleton claw 2"
                (check-eqv? (let ((x 1))
                              (and-let* (x)))
                            1))

     (test-case "let-like assignment 1"
                (check-eqv? (and-let* ((x #f))) #f))

     (test-case "let-like assignment 2"
                (check-eqv? (and-let* ((x 1))) 1))

     ;;(test-case "gotta break 1"
     ;;           (check-true (and-let* (#f (x 1)))))

     (test-case "mixed claws 1"
                (check-eqv? (and-let* ((#f) (x 1))) #f))

     ;; (test-case "gotta break 2"
     ;;            (check-true (and-let* (2 (x 1)))))

     (test-case "mixed claws 2"
                (check-eqv? (and-let* ((2) (x 1))) 1))

     (test-case "mixed claws 3"
                (check-eqv? (and-let* ((x 1) (2))) 2))

     (test-case "simple claw 1"
                (check-eqv?
                 (let ((x #f))
                   (and-let* (x) x))
                 #f))

     (test-case "simple claw 2"
                (check-equal?
                 (let ((x ""))
                   (and-let* (x) x))
                 ""))
     
     (test-case "simple claw 3"
                (check-equal?
                 (let ((x ""))
                   (and-let* (x)))
                 ""))

     (test-case "simple claw 4"
                (check-eqv?
                 (let ((x 1))
                   (and-let* (x) (+ x 1)))
                 2))

     (test-case "simple claw 5"
                (check-eqv?
                 (let ((x #f))
                   (and-let* (x) (+ x 1)))
                 #f))

     (test-case "simple claw 6"
                (check-eqv?
                 (let ((x 1))
                   (and-let* (((positive? x))) (+ x 1)))
                 2))

     (test-case "simple claw 7"
                (check-eqv?
                 (let ((x 1))
                   (and-let* (((positive? x)))))
                 #t))

     (test-case "simple claw 8"
                (check-eqv?
                 (let ((x 0))
                   (and-let* (((positive? x))) (+ x 1)))
                 #f))

     (test-case "simple claw 9"
                (check-eqv?
                 (let ((x 1))
                   (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))
                 3))

     ;; (test-case "gotta break 3"
     ;;            (check-true (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1))))
     
     (test-case "complex claw 1"
                (check-eqv?
                 (let ((x 1))
                   (and-let* (x ((positive? x))) (+ x 1)))
                 2))

     (test-case "complex claw 2"
                (check-eqv?
                 (let ((x 1))
                   (and-let* (((begin x)) ((positive? x))) (+ x 1)))
                 2))

     (test-case "complex claw 3"
                (check-eqv?
                 (let ((x 0))
                   (and-let* (x ((positive? x))) (+ x 1)))
                 #f))

     (test-case "complex claw 4"
                (check-eqv?
                 (let ((x #f))
                   (and-let* (x ((positive? x))) (+ x 1)))
                 #f))

     (test-case "complex claw 5"
                (check-eqv?
                 (let ((x #f))
                   (and-let* (((begin x)) ((positive? x))) (+ x 1)))
                 #f))

     (test-case "funky claw 1"
                (check-eqv?
                 (let ((x 1))
                   (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
                 #f))

     (test-case "funky claw 2"
                (check-eqv?
                 (let ((x 0))
                   (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
                 #f))

     (test-case "funky claw 3"
                (check-eqv?
                 (let ((x #f))
                   (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
                 #f))

     (test-case "funky claw 4"
                (check-eqv?
                 (let ((x 3))
                   (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
                 3/2))
     ))

  )

;;; and-let-test.rkt ends here
