#lang racket/base

(require (for-syntax racket/base)
         "private/flonum/flonum-bits.rkt"
         "private/flonum/flonum-constants.rkt"
         "private/flonum/flonum-functions.rkt"
         "private/flonum/flonum-search.rkt"
         "private/flonum/flonum-exp.rkt"
         "private/flonum/flonum-log.rkt"
         "private/flonum/flonum-more-functions.rkt"
         "private/flonum/flonum-factorial.rkt"
         "private/flonum/flonum-log1pmx.rkt"
         "private/flonum/flonum-polyfun.rkt"
         "private/flonum/flonum-error.rkt"
         "private/flonum/expansion/expansion-base.rkt"
         "private/flonum/expansion/expansion-exp.rkt"
         "private/flonum/expansion/expansion-log.rkt"
         "private/flonum/flvector.rkt")

(provide (all-from-out
          "private/flonum/flonum-bits.rkt"
          "private/flonum/flonum-constants.rkt"
          "private/flonum/flonum-functions.rkt"
          "private/flonum/flonum-search.rkt"
          "private/flonum/flonum-exp.rkt"
          "private/flonum/flonum-log.rkt"
          "private/flonum/flonum-more-functions.rkt"
          "private/flonum/flonum-factorial.rkt"
          "private/flonum/flonum-log1pmx.rkt"
          "private/flonum/flonum-polyfun.rkt"
          "private/flonum/flonum-error.rkt"
          "private/flonum/expansion/expansion-base.rkt"
          "private/flonum/expansion/expansion-exp.rkt"
          "private/flonum/expansion/expansion-log.rkt"
          "private/flonum/flvector.rkt")
         lg* lg/ lgprod)

(define-syntax lg* (make-rename-transformer #'fl+))
(define-syntax lg/ (make-rename-transformer #'fl-))
(define-syntax lgprod (make-rename-transformer #'flsum))
