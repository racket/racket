;; module loader for SRFI-9
(module |9| mzscheme
  (require (lib "record.ss" "srfi" "9"))
  (provide (all-from (lib "record.ss" "srfi" "9"))))
