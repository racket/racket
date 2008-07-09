;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graphing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/graphing)

(define (fun1 x) (+ (* x x) 1))
(check-expect (graph-fun fun1 'red) true)

(define (fun2 x) (+ (* -1 x x) 1))
(check-expect (graph-fun fun2 'blue) true)

(define (line1 x) (+ (* +1 x) 10))
(check-expect (graph-line line1 'black) true)

(define (line2 x) (+ (* -1 x) 10))
(check-expect (graph-line line2 'green) true)
