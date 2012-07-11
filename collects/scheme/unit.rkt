#lang racket/base

(require racket/unit
         (submod racket/unit compat))
(provide (except-out (all-from-out racket/unit) struct/ctc)
         (rename-out [struct~s struct]
                     [struct~s/ctc struct/ctc]))
