#lang scheme/base
(require scheme/class
          "syntax.rkt")

(provide gl-config%)

(defclass gl-config% object%
  (super-new)

  (define double-buffered? #t)
  (define/public (get-double-buffered) double-buffered?)
  (define/public (set-double-buffered v) (set! double-buffered? (and v #t)))

  (define stereo? #f)
  (define/public (get-stereo) stereo?)
  (define/public (set-stereo v) (set! stereo? (and v #t)))

  (define stencil-size 0)
  (define/public (get-stencil-size) stencil-size)
  (def/public (set-stencil-size [(integer-in 0 256) s])
    (set! stencil-size s))
    
  (define accum-size 0)
  (define/public (get-accum-size) accum-size)
  (def/public (set-accum-size [(integer-in 0 256) s])
    (set! accum-size s))

  (define depth-size 1)
  (define/public (get-depth-size) depth-size)
  (def/public (set-depth-size [(integer-in 0 256) s])
    (set! depth-size s))

  (define multisample-size 0)
  (define/public (get-multisample-size) multisample-size)
  (def/public (set-multisample-size [(integer-in 0 256) s])
    (set! multisample-size s)))
