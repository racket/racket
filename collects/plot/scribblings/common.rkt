#lang racket/base

(require scribble/eval
         (for-label racket
                    racket/gui/base
                    slideshow/pict
                    plot
                    plot/utils)
         plot
         plot/utils
         plot/common/contract-doc)

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket
                                  racket/gui/base
                                  slideshow/pict
                                  plot
                                  plot/utils))
         (all-from-out plot)
         (all-from-out plot/utils)
         doc-apply)

(define (plot-name) "PLoT")

(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval #'(require racket/math racket/match racket/list
                     (rename-in (except-in plot plot plot3d)
                                [plot-bitmap  plot]
                                [plot3d-bitmap  plot3d])
                     plot/utils))
    eval))
