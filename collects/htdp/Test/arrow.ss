; (load "tester.ss")

;; TeachPack : arrow.ss, draw.ss
;; Language: Beginner

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
(start 100 50)

;; this creates the controller GUI
(control-left-right (make-posn 10 20) 10 move draw-it)

; (load "tester.ss")

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
