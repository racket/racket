#lang racket/base
(require scribble/eval)
(provide make-img-eval)

(define (make-img-eval)
  (define img-eval (make-base-eval))
  (interaction-eval #:eval img-eval (require 2htdp/image))
  (interaction-eval #:eval img-eval (require lang/posn))
  
  (img-eval '(define extra-margin (make-parameter 0)))
  
  (img-eval
   `(let ([ce (current-eval)])
      (define (adjust-image exp i)
        (if (image? i)
            (let ([em (extra-margin)])
              (overlay/xy i 
                          (- em) (- em)
                          (rectangle 
                           (+ (image-width i) 1 em em)
                           (+ (image-height i) 1 em em)
                           'solid
                           (color 255 0 0 0))))
            i))
      (current-eval
       (λ (exp)
         (adjust-image exp (ce exp))))))
  
  img-eval)