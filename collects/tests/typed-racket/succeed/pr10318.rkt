#lang typed-scheme

(define-struct: rect ((nw : Symbol) (width : Number) (height : Number)))
(define-struct: circ ((cntr : Symbol)  (radius : Number)))
(define-struct: over ((top : Shape) (bot : Shape)))

(define-type-alias Shape (Rec Shape (U Plain over [Listof Plain])))
(define-type-alias Plain (U rect circ))
;; (define-type-alias Rect (U (make-rect Posn Number Number)))
;; Circ  = (make-circ Posn Number)

(: area (Shape -> Number))
;; the area of all rectangles in this s
(define (area s)
  (cond
    [(plain? s) (plain-area s)]
    [(over? s) (+ (area (over-top s)) (area (over-bot s)))]
    [else (apply + (map rect-area (filter rect? s)))]))

(: plain? (Any -> Boolean : Plain))
;; is this p a plain shape?
(define (plain? p)
  (or (rect? p) (circ? p)))

(: plain-area (Plain -> Number))
;; the area of this plain shape s
(define (plain-area s)
  (cond
    [(rect? s) (rect-area s)]
    [(circ? s) 0]))

(: rect-area (rect -> Number))
;; the area of this rectangle r
(define (rect-area s)
  (* (rect-width s) (rect-height s)))
