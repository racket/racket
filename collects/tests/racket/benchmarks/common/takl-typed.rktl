;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         takl.sch
; Description:  TAKL benchmark from the Gabriel tests
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 10:07:00 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
;               10-May-10 (Vincent St-Amour)
; Language:     Typed Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; TAKL -- The TAKeuchi function using lists as counters.

(: listn (Integer -> (Listof Integer)))
(define (listn n)
  (if (not (= 0 n))
      (cons n (listn (- n 1)))
      '()))

(define l18l (listn 18))
(define l12l (listn 12))
(define  l6l (listn 2))

(: mas (All (X) ((Listof X) (Listof X) (Listof X) -> (Listof X))))
(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x)
                 y z)
            (mas (cdr y)
                 z x)
            (mas (cdr z)
                 x y))))

(: shorterp (All (X) ((Listof X) (Listof X) -> Boolean)))
(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))
 
;;; call: (mas 18l 12l 6l)


(let ((x (if (with-input-from-file "input.txt" read) l6l '())))
  (time (let: loop : (Listof Integer)
              ((n : Integer 20) (v : (Listof Integer) '()))
          (if (zero? n)
              v
              (loop (- n 1)
                    (mas l18l l12l x))))))
