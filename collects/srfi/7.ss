;; module loader for SRFI-7
(module |7| mzscheme
  (require (lib "program.ss" "srfi" "7"))
  (provide (all-from (lib "program.ss" "srfi" "7"))))
