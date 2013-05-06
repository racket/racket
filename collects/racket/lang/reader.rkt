;; This module is preserved only for backward compatiblity
(module reader '#%kernel
  (#%require (submod racket reader))
  (#%provide (all-from (submod racket reader))))
