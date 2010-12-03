#lang racket/gui
(require racket/math)

;; This test creates a background that draws a circle in changing
;; colors. It draws in a background thread --- on in response to
;; `on-paint', and with no flushing controls --- but it should nevertheless
;; refresh onscreen frequently through an automatic flush.

(define f (new frame%
               [label "Snake"]
               [width 400]
               [height 400]))

(define c (new canvas% [parent f]))

(send f show #t)

(define prev-count 0)
(define next-time (+ (current-inexact-milliseconds) 1000))

(define (go)
  (let loop ([n 0])
    (when ((current-inexact-milliseconds) . > . next-time)
      (printf "~s\n" (- n prev-count))
      (set! prev-count n)
      (set! next-time (+ (current-inexact-milliseconds) 1000)))
    (let ([p (make-polar 175 (* pi (/ n 100)))]
          [dc (send c get-dc)])
      (send dc set-brush 
            (make-object color% 
                         (remainder n 256)
                         (remainder (* 2 n) 256)
                         (remainder (* 3 n) 256))
            'solid)
      (send dc draw-rectangle
            (+ 180 (real-part p))
            (+ 180 (imag-part p))
            20
            20)
      (loop (add1 n)))))

(thread go)

