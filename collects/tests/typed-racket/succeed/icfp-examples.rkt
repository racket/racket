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
#;(if (and (number? z) (string? y))
    (+ z (string-length y))
    (string-length z))

;; Example 7
(if (if (number? x) (string? y) #f)
    (+ x (string-length y))
    0)

;; Example 8
(: strnum? (Any -> Boolean : (U String Number)))
(define (strnum? x)
  (or (string? x) (number? x)))

;; Example 9
(if (let ([tmp (number? x)]) (if tmp tmp (string? x))) (f x) 0)

;; Example 10
(define: p : (Pair Any Any) (cons 1 2))
(if (number? (car p)) (add1 (car p)) 7)

;; Example 11
(define: (g [arg : (Pair Number Number)]) : Number 0)
(lambda: ([p : (Pair Any Any)])
  (if (and (number? (car p)) (number? (cdr p)))
      (g p)
      'no))

;; Example 12
(: carnum? : (Pair Any Any) -> Boolean : Number @ car)
(define (carnum? x)
  (number? (car x)))

;; Example 13
(cond [(and (number? x) (string? z)) (add1 x)]
      [(number? x) (add1 z)]
      [else 0])

;; Example 14
(lambda: ([input : (U Number String)]
          [extra : (Pair Any Any)])
  (cond
    [(and (number? input) (number? (car extra)))
     (+ input (car extra))]
    [(number? (car extra))
     (+ (string-length input) (car extra))]
    [else 0]))

