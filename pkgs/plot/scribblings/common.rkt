#lang racket/base

(require scribble/eval
         (for-label racket
                    racket/gui/base
                    pict
                    db
                    plot
                    plot/utils
                    unstable/contract)
         plot
         plot/utils
         plot/private/doc
         unstable/latent-contract/defthing)

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket
                                  racket/gui/base
                                  pict
                                  db
                                  plot
                                  plot/utils
                                  unstable/contract))
         (all-from-out plot)
         (all-from-out plot/private/doc)
         (all-from-out plot/utils)
         doc-apply)

(define (plot-name) "Plot")

(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require racket/math racket/match racket/list racket/draw racket/class
                      plot/pict
                      plot/utils)))
    eval))

(define (close-plot-eval)
  (close-eval plot-eval))
