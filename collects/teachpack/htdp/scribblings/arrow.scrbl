#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme 
                     teachpack/htdp/arrow))

@teachpack["arrow"]{Managing Control Arrows}

@declare-exporting[teachpack/htdp/arrow]

The teachpack implements a controller for moving shapes across a canvass. A
shape is a class of data for which @scheme[move] and @scheme[draw]
operations can be drawn. 

@defproc[(control-left-right
	   [shape Shape]
	   [n number?]
	   [move (-> number? Shape Shape)]
	   [draw (-> Shape true)]) true]{Moves shape @scheme[n] pixels left
(negative) or right (positive).} 

@defproc[(control-up-down
	   [shape Shape]
	   [n number?]
	   [move (-> number? Shape Shape)]
	   [draw (-> Shape true)]) true]{Moves shape @scheme[n] pixels up
(negative) or down (positive).} 

@defproc[(control
	   [shape Shape]
	   [n number?]
	   [move-lr (-> number? Shape Shape)]
	   [move-ud (-> number? Shape Shape)]
	   [draw (-> Shape true)]) true]{
Moves shape @scheme[N] pixels left or right and up or down, respectively.} 

Example:
@(begin
#reader scribble/comment-reader
(schemeblock 
;; A shape is a structure:
;;   (make-posn num num)

;; RAD : the radius of the simple disk moving across a canvas
(define RAD 10)

;; move : number shape -> shape or false
;; to move a shape by delta according to translate
;; effect: to redraw it
(define (move delta sh)
  (cond
    [(and (clear-solid-disk sh RAD)
          (draw-solid-disk (translate sh delta) RAD))
     (translate sh delta)]
    [else false]))

;; translate : shape number -> shape
;; to translate a shape by delta in the x direction
(define (translate sh delta)
  (make-posn (+ (posn-x sh) delta) (posn-y sh)))

;; draw-it : shape -> true
;; to draw a shape on the canvas: a disk with radius
(define (draw-it sh)
  (draw-solid-disk sh RAD))

;; RUN: 

;; this creates the canvas
(start 100 50)

;; this creates the controller GUI
(control-left-right (make-posn 10 20) 10 move draw-it)
))
