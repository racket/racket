#lang racket/gui
(require sgl)

(define c%
   (class canvas%
     (inherit with-gl-context swap-gl-buffers)
     (define/override (on-paint)
       (with-gl-context
        (lambda ()
          (gl-clear-color (random) (random) (random) 1)
          (gl-clear 'color-buffer-bit)
          (swap-gl-buffers)
          (gl-flush))))
     (super-new (style '(gl no-autoclear)))))

(define f (new frame% [label ""] [width 100] [height 300]))
(define c (new c% [parent f]))
(new message% [parent f] [label "Canvas changes color on refresh,"])
(new message% [parent f] [label "so check that it's not too often."])
(new button% [parent f] [label "Refresh"] [callback (lambda (b e) (send c refresh))])
(send f show #t)
