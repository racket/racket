;; TeachPack : arrow-gui.ss, gui.ss

(define msg (make-message (make-string 22 #\space)))
(create-window (list (list msg)))

#| Language: Intermediate with Lambda 
;; make-model : sym -> (button% event% -> void)
(define (make-model2 dir)
  (lambda (b e)
    (local ([define _ (view dir)])
      (draw-message msg (format "~a ~n" (control))))))

(connect 
 (make-model "left")
 (make-model "right")
 (make-model "up")
 (make-model "down"))
|#

#| Language: Beginner |#

(define (left b e) (draw-message msg "left"))
(define (right b e) (draw-message msg "right"))
(define (up b e) (draw-message msg "up"))
(define (down b e) (draw-message msg "down"))

(connect left right up down)
