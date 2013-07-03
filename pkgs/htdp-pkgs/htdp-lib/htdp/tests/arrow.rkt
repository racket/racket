;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/arrow)
(require htdp/draw)

;; ---------------------------------------------------------------------
;; 

;; A shape is a structure: 
;;   (make-posn num num)

;; RAD : the radius of the simple disk moving across a canvas
(define RAD 10)

;; move : number shape   -> shape or false 
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

;; TESTS: 

;; this creates the canvas
(check-expect (start 100 50) true)
(check-expect (draw-solid-string (make-posn 5 10) "click on arrow keys") true)

;; this creates the controller GUI
(check-expect (control-left-right (make-posn 10 20) 10 move draw-it) true)

; (load "tester.rkt")

; (test-error (control-left-right 'aa 10 move))

;; cannot raise an exception:
;(control-left-right 'aa 10 move draw-it)
;; shape is polymorphic!!

; (test-error (control-left-right 'aa 'bb move draw-it))
; (control-left-right 'aa 'bb move draw-it)
; (test-error (control-left-right 'aa 10 'cc draw-it))
; (control-left-right 'aa 10 'cc draw-it)
; (test-error (control-left-right 'aa 10 move 'cc))
; (control-left-right 'aa 10 move 'cc)
