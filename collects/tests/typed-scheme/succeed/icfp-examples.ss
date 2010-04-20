#lang typed/scheme

;; Example 1
(define: x : Any 7)
(if (number? x) (add1 x) 0)

;; Example 2
(define: (f [x : (U String Number)]) : Number
  (if (number? x) (add1 x) (string-length x)))

;; Example 3
(define: v : Number 12)
(define: l : (Listof Number) (list 1 2 3 12))
(let ([x (member v l)])
   (if x
       (add1 (car x))
       (error 'fail)))

;; Example 4
(if (or (number? x) (string? x)) (f x) 0)

;; Example 5
(define: y : Any "foo")
(if (and (number? x) (string? y))
    (+ x (string-length y))
    0) 

;; Example 6 has an intentional error
(define: z : (U Number String) 7)
(if (and (number? z) (string? y))
    (+ z (string-length y))
    (string-length z))

;; Example 7
(if (if (number? x) (string? y) #f)
    (+ x (string-length y))
    0)


