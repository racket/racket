#lang racket/gui

(require racket/math
         images/gui
         images/flomap
         images/logos)

(module test racket/base)

(define frame-delay 1/30)

(define size 256)
(define blur 8)
(define frame-num 10)
(define end-frame-quality 90)
(define mid-frame-quality 35)

(define background-fm (make-flomap* size size #(1 1 1 1)))

(define (flomap-shadowed fm σ color)
  (flomap-cc-superimpose (flomap-shadow fm σ color) fm))

(define plt-fm
  (flomap-shadowed (flomap-inset (plt-flomap #:height (- size (* 4 blur))) (* 2 blur))
                   blur #(1/2 0 0 1/8)))

(define racket-fm
  (flomap-shadowed (flomap-inset (racket-flomap #:height (- size (* 4 blur))) (* 2 blur))
                   blur #(1/2 1/8 0 0)))

(define logo-flomap* (flomap-whirl-morph plt-fm racket-fm))

(define (logo-flomap t)
  (flomap-cc-superimpose background-fm (logo-flomap* t)))

(define logo-frames
  (time
   (append (list (time (flomap->bitmap (logo-flomap 0))))
           (time
            (for/list ([t  (in-range 1 frame-num)])
              (flomap->bitmap (logo-flomap (/ t frame-num)))))
           (list (time (flomap->bitmap (logo-flomap 1)))))))

(define frame (new frame% [label "Whirl Morph Logo"] [width 256] [height 256]))
(define canvas (make-object bitmap-canvas% frame (first logo-frames)))
(send frame show #t)

(for ([_  (in-range 5)])
  (for ([frame  (in-list logo-frames)])
    (send canvas set-bitmap frame)
    (send canvas refresh)
    (sleep/yield frame-delay))
  (sleep 1)
  (for ([frame  (in-list (reverse logo-frames))])
    (send canvas set-bitmap frame)
    (send canvas refresh)
    (sleep/yield frame-delay))
  (sleep 1))

(send frame show #f)
