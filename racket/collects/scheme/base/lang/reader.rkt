;; This module is preserved only for backward compatiblity
(module reader '#%kernel
  (#%require (submod scheme/base reader))
  (#%provide (all-from (submod scheme/base reader))))
