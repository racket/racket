#lang scheme/base

(require scribble/manual)
(provide (all-from-out scribble/manual))

(require (for-label (except-in scheme only drop)
                    slideshow/base
                    slideshow/pict))
(provide (for-label (all-from-out scheme
                                  slideshow/base
                                  slideshow/pict)))
