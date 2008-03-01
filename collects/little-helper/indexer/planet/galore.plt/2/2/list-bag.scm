;;; list-bag.scm  --  Jens Axel Søgaard

; Bags implemented on top of sets represented as sorted lists

(module list-bag mzscheme
  (require
   (prefix set: "list-set.scm")
   (lib "include.ss"))
  
  (include "private/generic-bag.scm"))

