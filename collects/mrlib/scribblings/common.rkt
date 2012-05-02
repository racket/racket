#lang scheme/base

(require scribble/manual
         (for-label scheme/base
                    scheme/contract
                    scheme/class
                    (except-in scheme/gui/base make-color make-pen)))

(provide (all-from-out scribble/manual)
         (for-label (all-from-out scheme/base
                                  scheme/contract
                                  scheme/class
                                  scheme/gui/base)))
