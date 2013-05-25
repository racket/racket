;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         div.sch
; Description:  DIV benchmarks
; Author:       Richard Gabriel
; Created:      8-Apr-85
; Modified:     19-Jul-85 18:28:01 (Bob Shaw)
;               23-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; DIV2 -- Benchmark which divides by 2 using lists of n ()'s.
;;; This file contains a recursive as well as an iterative test.

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define *ll* (create-n 200))
 
(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))
 
(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))
 
(define (test-1 l)
  (do ((i 3000 (- i 1)))
      ((= i 0))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))
 
(define (test-2 l)
  (do ((i 3000 (- i 1)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))
 
;;; for the iterative test call: (test-1 *ll*)
;;; for the recursive test call: (test-2 *ll*)
 
(let ((input (with-input-from-file "input.txt" read)))
  (time (let loop ((n 200) (v 0))
          (if (zero? n)
              v
              (loop (- n 1)
                    (cons
                     (test-1 (if input *ll* '()))
                     (test-2 (if input *ll* '()))))))))
