#lang racket/gui

(require images/gui
         images/compile-time
         (for-syntax racket/math
                     images/private/flomap
                     images/logos)
         images/private/flomap
         images/logos)

(define frame-delay 1/30)

(begin-for-syntax
  (define size 256)
  (define blur 8)
  (define frame-num 10)
  (define end-frame-quality 90)
  (define mid-frame-quality 35)
  
  (define background-fm (make-flomap* size size (flvector 1.0 1.0 1.0 1.0)))
  
  (define plt-fm
    (flomap-shadowed (flomap-inset (plt-flomap (- size (* 4 blur))) (* 2 blur))
                     blur (flvector 0.0 0.0 0.1)))
  
  (define racket-fm
    (flomap-shadowed (flomap-inset (racket-flomap (- size (* 4 blur))) (* 2 blur))
                     blur (flvector 0.1 0.0 0.0)))
  
  (define logo-flomap* (flomap-whirl-morph plt-fm racket-fm))
  
  (define (logo-flomap t)
    (flomap-cc-superimpose background-fm (logo-flomap* t))))

(define logo-frames
  (time
   (append (list (compiled-bitmap (time (flomap->bitmap (logo-flomap 0)))
                                  end-frame-quality))
           (compiled-bitmap-list
            (time
             (for/list ([t  (in-range 1 frame-num)])
               (flomap->bitmap (logo-flomap (/ t frame-num)))))
            mid-frame-quality)
           (list (compiled-bitmap (time (flomap->bitmap (logo-flomap 1)))
                                  end-frame-quality)))))

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
