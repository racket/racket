#lang scheme/gui

(require 2htdp/image
         "constants.ss")

;; make dot.png
(let* ([bm (make-object bitmap% 1 1)]
       [mask (make-object bitmap% 1 1)]
       [bdc (make-object bitmap-dc% bm)])
  (send bm set-loaded-mask mask)
  (send bdc set-brush (make-object color% 50 100 20) 'solid)
  (send bdc set-pen "black" 1 'transparent)
  (send bdc draw-rectangle 0 0 1 1)
  (send bdc set-bitmap mask)
  (send bdc set-brush (make-object color% 100 100 100) 'solid)
  (send bdc draw-rectangle 0 0 1 1)
  (send bdc set-bitmap #f)
  (send bm save-file "dot.png" 'png)
  (void))

(define (save-bitmap mask-image color filename)
  (let* ([w (image-width mask-image)]
         [h (image-height mask-image)]
         [bm (make-object bitmap% w h)]
         [mask (make-object bitmap% w h)]
         [bdc (make-object bitmap-dc% bm)])

    (unless (= w before-and-after-image-width)
      (error 'mk-img.ss "expected ~a image's width to be ~a, got ~a" 
             filename
             before-and-after-image-width
             w))
    
    (send bm set-loaded-mask mask)
    (send bdc set-brush color 'solid)
    (send bdc set-pen "black" 1 'transparent)
    (send bdc draw-rectangle 0 0 w h)
    (send bdc set-bitmap mask)
    (send bdc clear)
    (send mask-image draw bdc 0 0 0 0 w h 0 0 'show-caret)
    (send bdc set-bitmap #f)
    (send bm save-file filename 'png)
    (void)))
  

(define (space-out img x-place)
  (overlay/align
   x-place
   'center
   img
   (rectangle (+ (image-width img) 4)
              graph-height
              'solid
              'white)))

(save-bitmap (space-out (rotate 90 (triangle 16 'solid 'black)) 'left)
             "forestgreen"
             "before.png")

(save-bitmap (space-out (rotate -90 (triangle 16 'solid 'black)) 'right)
             "forestgreen"
             "after.png")
