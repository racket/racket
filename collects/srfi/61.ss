;; module loader for SRFI-61
(module |61| mzscheme
  (require (lib "cond.ss" "srfi" "61"))
  (provide (rename srfi:cond cond)))