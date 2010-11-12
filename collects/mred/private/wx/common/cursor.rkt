#lang racket/base
(require racket/class
         racket/draw
         "local.rkt"
         (only-in "../platform.rkt" cursor-driver%)
          "../../syntax.rkt")

(provide cursor%)

(define standards (make-hash))

(define (is-16x16? image)
  (and (not (send image is-color?))
       (= 16 (send image get-width))
       (= 16 (send image get-height))))

(defclass cursor% object%

  (init-rest args)
  (define driver
    (case-args 
     args
     [([(symbol-in arrow bullseye cross hand ibeam watch blank
                   size-n/s size-e/w size-ne/sw size-nw/se
                   arrow+watch) 
        sym])
      (or (hash-ref standards sym #f)
          (let ([c (new cursor-driver%)])
            (send c set-standard sym)
            (hash-set! standards sym c)
            c))]
     [([bitmap% image]
       [bitmap% mask]
       [(integer-in 0 15) [hot-spot-x 0]]
       [(integer-in 0 15) [hot-spot-y 0]])
      (unless (is-16x16? image)
        (raise-type-error (init-name 'cursor%) '|bitmap (16x16 monochrome)| image))
      (unless (is-16x16? mask)
        (raise-type-error (init-name 'cursor%) '|bitmap (16x16 monochrome)| mask))
      (let ([c (new cursor-driver%)])
        (send c set-image image mask hot-spot-x hot-spot-y)
        c)]
     (init-name 'cursor%)))

  (define/public (get-driver) driver)

  (def/public (ok?) (send driver ok?))
  (super-new))
