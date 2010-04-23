#lang racket/base
(require racket/class racket/gui/base)
(provide draw-palaka palaka-pattern-size)

(define scale 1)
(define palaka-color (send the-color-database find-color "lightsteelblue"))

(define stripe-width (* scale 6))
(define stripe-gap (* scale 2))
(define blank-space-between-stripe-sets (* stripe-width 5))
(define ε 0)
(define vert-stripe-percent (- 1/2 ε))
(define horiz-stripe-percent (+ 1/4 ε))
(define quadrant-size (+ (* stripe-width 4)
                         (* stripe-gap 3)
                         blank-space-between-stripe-sets))
(define-syntax-rule 
  (four-times i e1 e ...)
  (let loop ([i 0])
    (when (< i 4)
      e1 e ...
      (loop (+ i 1)))))

(define (draw-palaka dc w h)
  (let ([alpha (send dc get-alpha)])
    (send dc set-pen palaka-color 1 'transparent)
    (let loop ([dx (- (/ quadrant-size 2))])
      (when (< dx w)
        (let loop ([dy (- (/ quadrant-size 2))])
          (when (< dy h)
            (send dc set-alpha 1)
            (send dc set-brush palaka-color 'solid)
            (send dc draw-rectangle dx dy quadrant-size quadrant-size)
            (send dc set-brush "white" 'solid)
            (draw-one-palaka dc dx dy)
            (loop (+ dy quadrant-size))))
        (loop (+ dx quadrant-size))))
    (send dc set-alpha alpha)))

(define (draw-one-palaka dc dx dy)
  (four-times 
   i
   (send dc set-alpha vert-stripe-percent)
   (send dc draw-rectangle 
         (+ dx (* i (+ stripe-width stripe-gap)))
         dy
         stripe-width
         quadrant-size)
   (send dc set-alpha horiz-stripe-percent)
   (send dc draw-rectangle
         dx
         (+ dy (* i (+ stripe-width stripe-gap)))
         quadrant-size
         stripe-width)))

(define (palaka-pattern-size i) (+ (* quadrant-size i) blank-space-between-stripe-sets))

#;
(begin
  (define f (new frame% [label "Palaka"]))
  (define c (new canvas%
                 [parent f]
                 [min-width (palaka-pattern-size 4)]
                 [min-height (palaka-pattern-size 4)]
                 [paint-callback
                  (λ (c dc) 
                    (let-values ([(cw ch) (send c get-client-size)])
                      (send dc set-smoothing 'aligned)
                      (draw-palaka dc cw ch)))]))
  (send f show #t))
