;; module loader for SRFI-64
(module |64| mzscheme
  (require (lib "testing.ss" "srfi" "64"))
  (provide (all-from (lib "testing.ss" "srfi" "64"))))
