;; code below relies on the felixs-world.ss teachpack.
(define-struct info (count x y s))

;; A World is a (make-info Num Num Num MouseEventType)

;; A MouseEventType is one of:
;; - 'button-down
;; - 'button-up
;; - 'drag
;; - 'move
;; - 'enter
;; - 'leave

;; handle-mouse-event : World Num Num MouseEventType -> World
(define (handle-mouse-event w x y s)
  (make-info (add1 (info-count w)) x y s))

;; draw-world : World -> Image
(define (draw-world w)
  (overlay
   (move-pinhole (text (string-append "count: " (number->string (info-count w))) 20 'blue)   0 -20)
   (move-pinhole (text (string-append "x: "     (number->string (info-x w)))     20 'red)    0 -40)
   (move-pinhole (text (string-append "y: "     (number->string (info-y w)))     20 'yellow) 0 -60)
   (move-pinhole (text (string-append "type: "  (symbol->string (info-s w)))     20 'green)  0 -80)
   ))

(big-bang 500 500 0.1 (make-info 0 0 0 'none))
(on-redraw draw-world)
(on-mouse-event handle-mouse-event)