;; module loader for SRFI-48
(module |48| mzscheme
  (require (lib "format.ss" "srfi" "48"))
  (provide (rename s:format format)))
