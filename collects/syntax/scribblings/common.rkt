#lang racket/base

(require scribble/manual
         (for-label racket/base
                    racket/contract))

(provide (all-from-out scribble/manual)
         (for-label (except-out (all-from-out racket/base
                                              racket/contract)
                                #%module-begin))
         refman)

(define refman '(lib "scribblings/reference/reference.scrbl"))
