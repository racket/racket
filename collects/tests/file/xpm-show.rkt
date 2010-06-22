#lang racket/gui
(require file/xpm)

(define the-bitmap
  (command-line #:program "xpm-show"
                #:args (file)
                (xpm->bitmap% (with-input-from-file file xpm-read))))

(define frame (new frame% [label "XPM"]))

(define canvas
  (new canvas%
       [parent frame]
       [paint-callback
        (λ (c dc)
          (send dc draw-bitmap the-bitmap 0 0))]))

(send frame show #t)