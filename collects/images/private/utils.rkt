#lang racket/base

(require racket/draw racket/class racket/match racket/list)

(provide (all-defined-out))

(define (draw-ellipse/smoothed dc x y w h)
  (define pen (send dc get-pen))
  (define brush (send dc get-brush))
  (send dc set-pen "black" 1 'transparent)
  (send dc draw-ellipse x y (- w 1) (- h 1))
  (send dc set-pen pen)
  (send dc set-brush "black" 'transparent)
  (send dc draw-ellipse x y w h)
  (send dc set-brush brush))

(define (apply-path-commands p cmds)
  (let loop ([x 0] [y 0] [cmds cmds])
    (cond
      [(empty? cmds)  (values x y)]
      [else
       (define cmd (first cmds))
       (match cmd
         ;; absolute commands
         [`(M)  (loop x y (rest cmds))]
         [`(L)  (loop x y (rest cmds))]
         [`(C)  (loop x y (rest cmds))]
         [`(M (,ax . ,ay) ,as ...)  (send p move-to ax ay)
                                    (loop ax ay (cons `(M ,@as) (rest cmds)))]
         [`(L (,ax . ,ay) ,as ...)  (send p line-to ax ay)
                                    (loop ax ay (cons `(L ,@as) (rest cmds)))]
         [`(C (,ax1 . ,ay1) (,ax2 . ,ay2) (,ax . ,ay) ,as ...)
          (send p curve-to ax1 ay1 ax2 ay2 ax ay)
          (loop ax ay (cons `(C ,@as) (rest cmds)))]
         ;; relative commands
         [`(m)  (loop x y (rest cmds))]
         [`(l)  (loop x y (rest cmds))]
         [`(c)  (loop x y (rest cmds))]
         [`(m (,dx . ,dy) ,ds ...)  (send p move-to (+ x dx) (+ y dy))
                                    (loop (+ x dx) (+ y dy) (cons `(m ,@ds) (rest cmds)))]
         [`(l (,dx . ,dy) ,ds ...)  (send p line-to (+ x dx) (+ y dy))
                                    (loop (+ x dx) (+ y dy) (cons `(l ,@ds) (rest cmds)))]
         [`(c (,dx1 . ,dy1) (,dx2 . ,dy2) (,dx . ,dy) ,ds ...)
          (send p curve-to (+ dx1 x) (+ dy1 y) (+ dx2 x) (+ dy2 y) (+ dx x) (+ dy y))
          (loop (+ x dx) (+ y dy) (cons `(c ,@ds) (rest cmds)))]
         [_  (error 'apply-path-commands "unknown path command ~e" cmd)])]))
  (void))

(define (draw-path-commands dc x y cmds)
  (define p (new dc-path%))
  (apply-path-commands p (cons `(M (,x . ,y)) cmds))
  (send dc draw-path p))

(define (get-text-size str font)
  (define bm (make-bitmap 1 1))
  (define dc (make-object bitmap-dc% bm))
  (define-values (w h _1 _2) (send dc get-text-extent str font #t))
  (values (inexact->exact (ceiling w))
          (inexact->exact (ceiling h))))
