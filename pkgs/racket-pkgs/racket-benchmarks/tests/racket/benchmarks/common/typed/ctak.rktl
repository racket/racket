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

(: ctak (Integer Integer Integer -> Integer))
(define (ctak x y z)
  (call-with-current-continuation
   (lambda: ((k : (Integer -> Nothing)))
     (ctak-aux k x y z))))

(: ctak-aux ((Integer -> Integer) Integer Integer Integer -> Integer))
(define (ctak-aux k x y z)
  (cond ((not (< y x))  ;xy
         (k z))
        (else (call-with-current-continuation
               (let ([v (ctak-aux
                         k
                         (call-with-current-continuation
                          (lambda: ((k : (Integer -> Nothing)))
                            (ctak-aux k
                                      (- x 1)
                                      y
                                      z)))
                         (call-with-current-continuation
                          (lambda: ((k : (Integer -> Nothing)))
                            (ctak-aux k
                                      (- y 1)
                                      z
                                      x)))
                         (call-with-current-continuation
                          (lambda: ((k : (Integer -> Nothing)))
                            (ctak-aux k
                                      (- z 1)
                                      x
                                      y))))])
                 (lambda (dummy) v))))))

;;; call: (ctak 18 12 6)

(let ((input (with-input-from-file "input.txt" read)))
  (time (let: loop : Integer
              ((n : Integer 25) (v : Integer 0))
              (if (zero? n)
                  v
                  (loop (- n 1)
                        (ctak 18 12 (if input 6 0)))))))
