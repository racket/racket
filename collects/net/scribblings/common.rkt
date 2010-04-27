#lang scheme/base

(require scribble/manual
         (for-label scheme/base
                    scheme/contract))

(provide (all-from-out scribble/manual)
         (for-label (all-from-out scheme/base
                                  scheme/contract)))
