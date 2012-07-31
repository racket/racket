#lang racket/base

(require (except-in racket/unit struct/ctc)
         (submod racket/unit compat))
(provide (all-from-out racket/unit)
         (rename-out [struct~s struct]
                     [struct~s/ctc struct/ctc]))
