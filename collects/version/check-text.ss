(module check-text mzscheme
  (require "private/text-defs.ss" "private/go-check.ss")
  (provide check-version)
  (define (check-version) (go-check #f #f text-defs@)))
