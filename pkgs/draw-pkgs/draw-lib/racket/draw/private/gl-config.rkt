#lang racket/base
(require racket/class
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
  (define/public (set-stencil-size s)
    (set! stencil-size s))
    
  (define accum-size 0)
  (define/public (get-accum-size) accum-size)
  (define/public (set-accum-size s)
    (set! accum-size s))

  (define depth-size 1)
  (define/public (get-depth-size) depth-size)
  (define/public (set-depth-size s)
    (set! depth-size s))

  (define multisample-size 0)
  (define/public (get-multisample-size) multisample-size)
  (define/public (set-multisample-size s)
    (set! multisample-size s))

  (define share-context #f)
  (define/public (get-share-context) share-context)
  (define/public (set-share-context s)
    (set! share-context s)))
