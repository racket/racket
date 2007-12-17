#lang scheme/base

(require scribble/manual)
(provide (all-from-out scribble/manual))

(require (for-label (except-in scheme only)
                    slideshow/base
                    slideshow/pict))
(provide (for-label (all-from-out scheme
                                  slideshow/base
                                  slideshow/pict)))
