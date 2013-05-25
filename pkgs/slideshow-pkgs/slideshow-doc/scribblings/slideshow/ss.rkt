#lang racket/base

(require scribble/manual)
(provide (all-from-out scribble/manual))

(require (for-label (except-in racket only drop)
                    slideshow/base
                    pict))
(provide (for-label (all-from-out racket
                                  slideshow/base
                                  pict)))
