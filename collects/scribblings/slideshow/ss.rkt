#lang racket/base

(require scribble/manual)
(provide (all-from-out scribble/manual))

(require (for-label (except-in racket only drop)
                    slideshow/base
                    slideshow/pict))
(provide (for-label (all-from-out racket
                                  slideshow/base
                                  slideshow/pict)))
