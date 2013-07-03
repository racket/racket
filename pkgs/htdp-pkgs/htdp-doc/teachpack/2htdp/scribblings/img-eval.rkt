#lang racket/base
(require scribble/eval)
(provide make-img-eval
         set-eval-handler-to-tweak-images)

#|

make-img-eval : -> (any/c . -> . any)
  calls @racket[make-base-eval] to create
  an evaluator, and then initializes it with
  @racket[set-eval-handler-to-tweak-images].


set-eval-handler-to-tweak-images : (any/c . -> . any) -> void?

  loads @racketmodname[2htdp/image] and @racketmodname[lang/posn]
  into the given evaluator and then sets its eval-handler to
  increase the bounding box for 2htdp/image images by 1.
  
  It does this only for images that the result of evaluation
  directly (not for example, when lists that contain images
  are the result of evaluation).

|#

(define (make-img-eval)
  (define img-eval (make-base-eval))
  (set-eval-handler-to-tweak-images img-eval)
  img-eval)

(define (set-eval-handler-to-tweak-images img-eval)
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
         (adjust-image exp (ce exp)))))))
