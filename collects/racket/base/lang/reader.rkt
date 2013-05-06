;; This module is preserved only for backward compatiblity
(module reader '#%kernel
  (#%require (submod racket/base reader))
  (#%provide (all-from (submod racket/base reader))))
