#lang racket/gui
(require mrlib/private/image-core-bitmap)
(define img (read-bitmap (build-path (collection-path "icons") "plt-logo-red-shiny.png")))

(define amount .5)
(define remove-margin 50)

(define-values (bmbytes bm-w bm-h) (bitmap->bytes img))
(define-values (left-bytes left-w left-h) (linear-transform bmbytes bm-w bm-h 1 0 (- amount) 1))
(define-values (right-bytes right-w right-h) (linear-transform bmbytes bm-w bm-h 1 0 amount 1))
(define left-bm (bytes->bitmap left-bytes left-w left-h))
(define right-bm (bytes->bitmap right-bytes right-w right-h))
(define f (new frame% [label ""]))
(define hp (new horizontal-panel% [parent f] [alignment '(center center)]))

(define heart-w left-w)
(define heart-h (- left-h remove-margin remove-margin))

(define (draw-heart dc)
  (send dc draw-bitmap-section 
        left-bm 
        0 (- remove-margin) 
        0 0
        (floor (/ heart-w 2)) left-h)
  (send dc draw-bitmap-section 
        right-bm
        (floor (/ heart-w 2)) (- remove-margin) 
        (floor (/ heart-w 2)) 0
        (floor (/ heart-w 2)) right-h))

(define corig (new canvas%
               [parent hp]
               [min-width bm-w]
               [min-height bm-h]
               [stretchable-height #f]
               [paint-callback
                (λ (c dc)
                  (send dc draw-bitmap img 0 0))]))
(define c (new canvas%
               [parent hp]
               [min-width heart-w]
               [min-height heart-h]
               [paint-callback
                (λ (c dc)
                  (draw-heart dc))]))
(void (new grow-box-spacer-pane% [parent f]))
(send f show #t)

(define heart-bm (make-bitmap heart-w heart-h))
(define heart-bdc (make-object bitmap-dc% heart-bm))
(draw-heart heart-bdc)
(send heart-bdc set-bitmap #f)
;; uncomment the next line to actually save the icon in the collects dir
;;(send heart-bm save-file (build-path (collection-path "icons") "heart.png") 'png)
