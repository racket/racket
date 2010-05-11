;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         ctak.sch
; Description:  The ctak benchmark
; Author:       Richard Gabriel
; Created:      5-Apr-85
; Modified:     10-Apr-85 14:53:02 (Bob Shaw)
;               24-Jul-87 (Will Clinger)
;               3-May-10 (Vincent St-Amour)
; Language:     Typed Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The original version of this benchmark used a continuation mechanism that
; is less powerful than call-with-current-continuation and also relied on
; dynamic binding, which is not provided in standard Scheme.  Since the
; intent of the benchmark seemed to be to test non-local exits, the dynamic
; binding has been replaced here by lexical binding.

; For Scheme the comment that follows should read:
;;; CTAK -- A version of the TAK procedure that uses continuations.

;;; CTAK -- A version of the TAK function that uses the CATCH/THROW facility.

#lang typed/scheme/base

(: ctak (Integer Integer Integer -> Integer))
(define (ctak x y z)
  ((inst call-with-current-continuation Integer Integer)
   (lambda (k)
     (ctak-aux k x y z))))

(: ctak-aux ((Integer -> Integer) Integer Integer Integer -> Integer))
(define (ctak-aux k x y z)
  (cond ((not (< y x))  ;xy
         (k z))
        (else ((inst call-with-current-continuation Integer Integer)
               (lambda (dummy)
                 (ctak-aux
                  k
                  ((inst call-with-current-continuation Integer Integer)
                   (lambda (k)
                     (ctak-aux k
                               (- x 1)
                               y
                               z)))
                  ((inst call-with-current-continuation Integer Integer)
                   (lambda (k)
                     (ctak-aux k
                               (- y 1)
                               z
                               x)))
                  ((inst call-with-current-continuation Integer Integer)
                   (lambda (k)
                     (ctak-aux k
                               (- z 1)
                               x
                               y)))))))))

;;; call: (ctak 18 12 6)

(let ((input (with-input-from-file "input.txt" read)))
  (time (let: loop : Integer
              ((n : Integer 8) (v : Integer 0))
              (if (zero? n)
                  v
                  (loop (- n 1)
                        (ctak 18 12 (if input 6 0)))))))
