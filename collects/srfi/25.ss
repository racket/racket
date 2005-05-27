;; module loader for SRFI-25
(module |25| mzscheme
  (require (lib "array.ss" "srfi" "25"))
  (provide (all-from (lib "array.ss" "srfi" "25"))))
