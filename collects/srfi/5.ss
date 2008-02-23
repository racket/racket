;; module loader for SRFI-5
(module |5| mzscheme
  (require (rename srfi/5/let my-let let))
  (provide (rename my-let let)))
