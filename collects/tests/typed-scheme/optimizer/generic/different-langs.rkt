;; to see if the harness supports having the 2 versions of a test being
;; written in different languages
(module different-langs typed/scheme #:optimize
  (+ 1 2))
