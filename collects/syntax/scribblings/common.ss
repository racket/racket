#lang scheme/base

(require scribble/manual
         (for-label scheme/base
                    scheme/contract))

(provide (all-from-out scribble/manual)
         (for-label (except-out (all-from-out scheme/base
                                              scheme/contract)
                                #%module-begin))
         refman)

(define refman '(lib "scribblings/reference/reference.scrbl"))
