#lang racket/base
(require racket/class
         racket/draw
         (only-in "../platform.rkt" cursor-driver%)
          "../../syntax.rkt")

(provide cursor%)

(define standards (make-hash))

(defclass cursor% object%

  (init-rest args)
  (define driver
    (case-args 
     args
     [([(symbol-in arrow bullseye cross hand ibeam watch blank
                   size-n/s size-e/w size-ne/sw size-nw/se) 
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
      (let ([c (new cursor-driver%)])
        (send c set-image image mask hot-spot-x hot-spot-y)
        c)]
     (init-name 'cursor%)))

  (def/public (ok?) (send driver ok?))
  (super-new))
