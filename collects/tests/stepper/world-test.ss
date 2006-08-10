(define (next t)
  (+ t 1))

(define (image t)
  (place-image (circle 3 'solid 'red) 20 t (empty-scene 50 50)))

;; --- run program run
(big-bang 50 50 .1 0)
(on-redraw image)
(on-tick-event next)
