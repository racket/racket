;; module loader for SRFI-40
(module |40| mzscheme
  (require (lib "stream.ss" "srfi" "40"))
  (provide (all-from (lib "stream.ss" "srfi" "40"))))
