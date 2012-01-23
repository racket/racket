#lang racket/base

(require scribble/eval
         (for-label racket
                    racket/gui/base
                    slideshow/pict
                    db
                    plot
                    plot/utils)
         plot
         plot/utils
         plot/doc
         unstable/latent-contract/defthing)

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket
                                  racket/gui/base
                                  slideshow/pict
                                  db
                                  plot
                                  plot/utils))
         (all-from-out plot)
         (all-from-out plot/doc)
         (all-from-out plot/utils)
         doc-apply)

(define (plot-name) "PLoT")

(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require racket/math racket/match racket/list racket/draw racket/class
                      (rename-in (except-in plot plot plot3d)
                                 [plot-bitmap  plot]
                                 [plot3d-bitmap  plot3d])
                      plot/utils)))
    eval))
