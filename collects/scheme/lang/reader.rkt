;; This module is preserved only for backward compatiblity
(module reader '#%kernel
  (#%require (submod scheme reader))
  (#%provide (all-from (submod scheme reader))))
