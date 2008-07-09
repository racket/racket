#lang scheme/load

;; provide another source to import srcloc? to avoid
;; collisions with the version from mzscheme.
(module srcloc scheme/base

  (provide (rename-out [srcloc? s:srcloc?])))

(module foo typed-scheme

  (require/opaque-type Srcloc s:srcloc? 'srcloc))
