;; This module is preserved only for backward compatiblity
(module reader '#%kernel
  (#%require (submod slideshow reader))
  (#%provide (all-from (submod slideshow reader))))
