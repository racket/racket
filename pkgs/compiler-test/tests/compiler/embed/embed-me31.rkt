#lang racket/base

;; check that `declare-preserve-for-embedding` works

(module keep-me racket/base
  (module declare-preserve-for-embedding racket/base)
  (define kept "This is 31.\n")
  (provide kept))

(display
 (dynamic-require (module-path-index-join
                   '(submod "." keep-me)
                   (variable-reference->module-path-index
                    (#%variable-reference)))
                  'kept))

