;; module loader for SRFI-42
(module |42| mzscheme
  (require srfi/42/comprehensions)
  (provide (all-from srfi/42/comprehensions)))
