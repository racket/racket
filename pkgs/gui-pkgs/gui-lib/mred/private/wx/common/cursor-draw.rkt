#lang racket/base
(require racket/class
         racket/draw)

(provide make-cursor-image
         draw-watch
         draw-nw/se
         draw-ne/sw
         draw-bullseye)

(define (make-cursor-image draw-proc [smoothing 'aligned])
  (let* ([bm (make-object bitmap% 16 16 #f #t)]
         [dc (make-object bitmap-dc% bm)])
    (send dc set-smoothing smoothing)
    (draw-proc dc 16 16)
    (send dc set-bitmap #f)
    bm))

(define (draw-watch dc w h)
  (send dc set-brush "black" 'solid)
  (send dc draw-rectangle 5 0 6 4)
  (send dc draw-rectangle 5 12 6 4)
  (send dc set-brush "white" 'solid)
  (send dc draw-ellipse 3 3 10 10)
  (send dc draw-line 7 5 7 8)
  (send dc draw-line 7 8 9 8))

(define (draw-nw/se dc w h)
  (bolden
   dc
   (lambda ()
     (send dc set-smoothing 'unsmoothed)
     (send dc draw-line 0 16 16 0)
     (send dc draw-line 0 0 16 16)
     (send dc draw-line 1 4 1 1)
     (send dc draw-line 1 1 4 1)
     (send dc draw-line 12 15 15 15)
     (send dc draw-line 15 15 15 12))))
   
(define (draw-ne/sw dc w h)
  (bolden
   dc
   (lambda ()
     (send dc set-smoothing 'unsmoothed)
     (send dc draw-line 0 16 16 0)
     (send dc draw-line 0 0 16 16)
     (send dc draw-line 12 1 15 1)
     (send dc draw-line 15 1 15 4)
     (send dc draw-line 1 12 1 15)
     (send dc draw-line 1 15 4 15))))

(define (draw-bullseye dc w h)
  (send dc draw-ellipse 1 1 (- w 2) (- h 2))
  (send dc draw-ellipse 4 4 (- w 8) (- h 8))
  (send dc draw-ellipse 7 7 2 2))

(define (bolden dc draw)
  (send dc set-pen "white" 4 'solid)
  (draw)
  (send dc set-pen "black" 2 'solid)
  (draw))
