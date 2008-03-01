;;; red-black-tree-bag.scm  --  Jens Axel Søgaard

(module red-black-tree-bag mzscheme
  (require
   (prefix set: "red-black-tree-set.scm")
   (lib "include.ss"))
  
  (include "private/generic-bag.scm"))

