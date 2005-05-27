;; module loader for SRFI-5
(module |5| mzscheme
  (require (rename (lib "let.ss" "srfi" "5") my-let let))
  (provide (rename my-let let)))
