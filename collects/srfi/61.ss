;; module loader for SRFI-61
(module |61| mzscheme
  (require srfi/61/cond)
  (provide (rename srfi:cond cond)))