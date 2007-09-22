(module main mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  (provide (all-from mzscheme)
           (all-from (lib "class.ss"))
           (all-from (lib "mred.ss" "mred"))))
