#lang racket/base

;; deprecated library, see `racket/unit`

(require racket/unit
         (submod racket/unit compat))
(provide (except-out (all-from-out racket/unit) struct/ctc)
         (all-from-out (submod racket/unit compat)))
