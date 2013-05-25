;; A tiny language to build our promises with no built-in interference
(module mz-without-promises mzscheme
  (provide (all-from-except mzscheme delay force promise?)))
