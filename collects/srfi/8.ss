;; module loader for SRFI-8
(module |8| mzscheme
  (require (lib "receive.ss" "srfi" "8"))
  (provide (all-from (lib "receive.ss" "srfi" "8"))))
