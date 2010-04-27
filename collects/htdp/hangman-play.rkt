#lang scheme

(require htdp/hangman
         htdp/big-draw
         lang/prim
         lang/posn)

(provide go)

(define-primitive go go/proc)

#| ------------------------------------------------------------------------
  draw-next-part :
  { 'noose 'head 'right-arm 'left-arm 'body 'right-leg 'left-leg } -> #t
  result: #t if things went okay
  effect: to draw the specified body part in a canvas of size W x H
  credit: John Clements 
  |#
(define (draw-next-part body-part)
  (cond ((eq? body-part 'body)
         (draw-solid-line (make-posn 100 60) (make-posn 100 130) 'black))
        ((eq? body-part 'right-leg)
         (draw-solid-line (make-posn 100 130) (make-posn 30 170) 'black))
        ((eq? body-part 'left-leg)
         (draw-solid-line (make-posn 100 130) (make-posn 170 170) 'black))
        ((eq? body-part 'right-arm)
         (draw-solid-line (make-posn 100 75) (make-posn 40 65) 'black))
        ((eq? body-part 'left-arm)
         (draw-solid-line (make-posn 100 75) (make-posn 160 65) 'black))
        ((eq? body-part 'head)
         (draw-circle (make-posn 100 50) 10 'black))
        ((eq? body-part 'noose)
         (and
          (draw-solid-line (make-posn 100 30) (make-posn 100 10) 'black)
          (draw-solid-line (make-posn 100 10) (make-posn 0 10) 'black)
          (draw-solid-line (make-posn 115 35) (make-posn 123 43) 'black)
          (draw-solid-line (make-posn 123 35) (make-posn 115 43) 'black)
          (draw-solid-line (make-posn 131 40) (make-posn 139 48) 'black)
          (draw-solid-line (make-posn 139 40) (make-posn 131 48) 'black)
          (draw-circle (make-posn 120 50) 30 'red)))))

;; reveal-list : list-of-letters list-of-letters letter -> list-of-letters
(define (reveal-list l1 l2 gu)
  (map (lambda (x1 x2)
         (cond
           [(eq? x1 gu) gu]
           [else x2]))
       l1 l2))

(define (go/proc x)
  (start 200 400)
  (hangman-list reveal-list draw-next-part))    

