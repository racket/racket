;;; set.scm  --  Jens Axel Soegaard  --  19th dec 2005

; The default set implementation is based on red-black trees.

(module set mzscheme
  (require "red-black-tree-set.scm")
  (provide (all-from "red-black-tree-set.scm")))
  
