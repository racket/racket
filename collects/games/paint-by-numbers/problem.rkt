(module problem mzscheme
  (provide (struct problem (name rows cols solution)))
  (define-struct problem (name rows cols solution)))
