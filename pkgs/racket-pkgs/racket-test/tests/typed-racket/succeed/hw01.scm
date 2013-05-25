#lang typed-scheme

(: sqr (Real -> Real))
(define (sqr x) (* x x))

(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)

;(require "support.ss")

;; ---------------------------------------------------------------------
;; 2a

;; triangle: number number -> number
;; Calculates the area of a triange
(define: (triangle [b : number] [h : number]) : number
  (* b (/ h 2)))

;; tests:
(= 4 (triangle 2 4))
(= 20 (triangle 5 8))

;; ---------------------------------------------------------------------
;; 2b

;; total-profit: integer -> number
;; Calculates profit made by the theater
(define: (total-profit [people : number]) : number
  (- (* people 5)
     (+ (* people .5) 20)))

;; tests:
(= -20 (total-profit 0))
(= 25 (total-profit 10))

;; ---------------------------------------------------------------------
;; 3a

;; interest: number ->number
;; Calculates interest for a given sum
(define: (interest [sum : Real]) : number
  (* sum
     (cond [(<= sum 1000) .04]
	   [(<= sum 5000) .045]
	   [else .05])))

;; tests:
(=   0   (interest 0))
(=  20   (interest 500))
(=  40   (interest 1000))
(= 112.5 (interest 2500))
(= 500   (interest 10000))

;; ---------------------------------------------------------------------
;; 3b

;; how-many: int int int -> int
;; Returns the number of roots in the equation
(define: (how-many [a : Integer] [b : Integer] [c : Integer]) : Integer
  (cond [(> (sqr b) (* 4 a c)) 2]
	[(< (sqr b) (* 4 a c)) 0]
	[else 1]))

;; tests:
(= 1 (how-many 1 2 1))
(= 2 (how-many 1 3 1))
(= 0 (how-many 1 1 1))

;; ---------------------------------------------------------------------
;; 4

;; what-kind: int int int -> symbol
;; Determines the type of the eqation
(define: (what-kind [a : Integer] [b : Integer] [c : Integer]) : symbol
  (cond [(= a 0) 'degenerate]
	[(> (sqr b) (* 4 a c)) 'two]
	[(< (sqr b) (* 4 a c)) 'none]
	[else 'one]))

;; tests:
(eq? 'one (what-kind 1 2 1))
(eq? 'two (what-kind 1 3 1))
(eq? 'none (what-kind 1 1 1))
(eq? 'degenerate (what-kind 0 1 1))

;; ---------------------------------------------------------------------
;; 5

;; list-length: (list-of any) -> integer
;; Computes the length of a list
(define: (list-length [loa : (Listof top)]) : number
  (if (null? loa)
      0
      (+ 1 (list-length (cdr loa)))))

#| tail recursive version:
(define (list-length-helper loa acc)
  (if (null? loa)
      acc
      (list-length-helper (cdr loa) (+ acc 1))))
(define (list-length loa)
  (list-length-helper loa 0))
|#

  ;; tests:
  (= 0 (list-length '()))
  (= 2 (list-length '(1 2)))
  (= 3 (list-length '(1 2 (1 2 3 4))))
