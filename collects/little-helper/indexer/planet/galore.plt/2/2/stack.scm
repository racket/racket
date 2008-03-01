;;; stack.scm  --  Jens Axel Soegaard  --  6th nov 2005

; The default stack implementation is based on lists.

(module stack mzscheme
  (require "list-stack.scm")
  (provide (all-from "list-stack.scm")))
  
