#lang scheme/base

(require scribble/manual
         scribble/struct
         scribble/decode
         (only-in "../inside/utils.ss" cpp)
         (for-syntax scheme/base)
         (for-label scheme/base
                    scheme/contract
                    (except-in "unsafe-foreign.ss" ->)))

(provide cpp
         InsideMzScheme
         (all-from-out scribble/manual)
         (for-label (all-from-out scheme/base
                                  scheme/contract
                                  "unsafe-foreign.ss")))


(define InsideMzScheme
  (other-manual '(lib "scribblings/inside/inside.scrbl")))
