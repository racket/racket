#lang scheme/gui

(define width 16)
(define height 16)

(define (draw1 dc color?) (draw dc "pdf" 0 7 color?))
(define (draw2 dc color?) (draw dc "htm" -1 5 color?))

(define (draw dc str dx dy color?)
  (send dc clear)
  (send dc set-font (send the-font-list find-or-create-font 15 " Futura" 'swiss 'normal 'bold))
  (let-values ([(tw th _1 _2) (send dc get-text-extent "@")])
    (when color? (send dc set-text-foreground (send the-color-database find-color "gray")))
    (send dc draw-text "@" 
          (- (/ width 2) (/ tw 2))
          (- (/ height 2) (/ th 2)))
    (send dc set-font (send the-font-list find-or-create-font 6 " Gill Sans" 'swiss 'normal 'bold))
    (when color? (send dc set-text-foreground (send the-color-database find-color "purple")))
    (send dc draw-text str (+ 0 dx) (- height dy) #f 0 (* pi 1/4))))

(define f (new frame% [label ""] [width 100] [height 100] [alignment '(center center)]))
(define c (new canvas% 
               [parent f]
               [stretchable-width #f]
               [stretchable-height #f]
               [paint-callback (λ (c dc) (draw-bm dc pdf-bitmap))]
               [min-width 16]
               [min-height 16]))
(define c2 (new canvas% 
               [parent f]
               [stretchable-width #f]
               [stretchable-height #f]
               [paint-callback (λ (c dc) (draw-bm dc html-bitmap))]
               [min-width 16]
               [min-height 16]))


(define (mk-bitmap draw)
  (define mask-bm (make-object bitmap% width height))
  (define bm (make-object bitmap% width height))
  (define bdc (make-object bitmap-dc%))
  (send bm set-loaded-mask mask-bm)
  (send bdc set-bitmap mask-bm)
  (draw bdc #f)
  (send bdc set-bitmap bm)
  (draw bdc #t)
  (send bdc set-bitmap #f)
  bm)

(define (draw-bm dc bm)
  #;(send dc draw-bitmap (send bm get-loaded-mask) 0 0)
  
  (send dc set-pen "lightgray" 1 'transparent)
  (send dc set-brush "lightgray" 'solid)
  (send dc draw-rectangle 0 0 width height)
  (send dc draw-bitmap bm 0 0 
        'solid
        (send the-color-database find-color "black")
        (send bm get-loaded-mask)))


(define pdf-bitmap (mk-bitmap draw1))
(define html-bitmap (mk-bitmap draw2))

(module+ main
  (send pdf-bitmap save-file "../pdf.png" 'png)
  (send html-bitmap save-file "../html.png" 'png)
  (send f show #t))
