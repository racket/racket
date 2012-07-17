#lang racket/base

;; deprecated library, see `racket/unit`

(require (except-in racket/unit struct/ctc)
         (submod racket/unit compat))
(provide (all-from-out racket/unit)
         (all-from-out (submod racket/unit compat)))
