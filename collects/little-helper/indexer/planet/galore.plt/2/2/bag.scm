;;; bag.scm  --  Jens Axel Soegaard  --  19th dec 2005

; The default bag implementation is based on red-black tree sets.

(module bag mzscheme
  (require "red-black-tree-bag.scm")
  (provide (all-from "red-black-tree-bag.scm")))
  
