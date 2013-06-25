#lang typed-scheme

(define: (evenp [n : Number]) : Boolean
  (if (zero? n) #t (oddp (- n 1))))

(define: (oddp [n : Number]) : Boolean
  (if (zero? n) #f (evenp (- n 1))))


