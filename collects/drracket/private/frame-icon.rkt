#lang racket/base
(require racket/class racket/draw)
(provide todays-icon)

(define todays-icon
  (and (eq? (system-type) 'unix)
       (let ()
         ;; avoid building the mask unless we use it
         (define todays-icon
           (make-object bitmap% 
             (collection-file-path 
              (case (date-week-day (seconds->date (current-seconds)))
                [(6 0) "plt-logo-red-shiny.png"]
                [else "plt-logo-red-diffuse.png"])
              "icons")
             'png/mask))
         
         (define todays-icon-bw-mask 
           (and (send todays-icon ok?)
                (send todays-icon get-loaded-mask)
                (let* ([w (send todays-icon get-width)]
                       [h (send todays-icon get-height)]
                       [bm (make-object bitmap% w h #t)]
                       [color-mask (send todays-icon get-loaded-mask)]
                       [src-bytes (make-bytes (* w h 4) 0)]
                       [dest-bits (make-bytes (* w h 4) 255)]
                       [bdc (make-object bitmap-dc% bm)]
                       [black (send the-color-database find-color "black")]
                       [white (send the-color-database find-color "white")])
                  (send color-mask get-argb-pixels 0 0 w h src-bytes #t)
                  (for ([i (in-range 0 w)])
                    (for ([j (in-range 0 h)])
                      (let ([b (= (bytes-ref src-bytes (* 4 (+ i (* j h)))) 0)])
                        (send bdc set-pixel i j (if b white black)))))
                  (send bdc set-bitmap #f)
                  bm)))
         
         (when todays-icon-bw-mask
           (send todays-icon set-loaded-mask todays-icon-bw-mask))
         todays-icon)))
