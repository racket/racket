;; module loader for SRFI-42
(module |42| mzscheme
  (require (lib "comprehensions.ss" "srfi" "42"))
  (provide (all-from (lib "comprehensions.ss" "srfi" "42"))))
