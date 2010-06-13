#lang racket/base
(require racket/class
         racket/draw)

(provide get-window-text-extent*)

(define get-window-text-extent*
  (let ([bm #f][dc #f])
    (case-lambda
     [(string font) (get-window-text-extent* string font #f)]
     [(string font combine?)
      (unless bm
        (set! bm (make-object bitmap% 2 2))
        (set! dc (make-object bitmap-dc%))
        (send dc set-bitmap bm))
      (unless (send bm ok?)
        (error 'get-window-text-extent "couldn't allocate sizing bitmap"))
      (let-values ([(w h d a) (send dc get-text-extent string font combine?)])
        (values w h d a))])))
