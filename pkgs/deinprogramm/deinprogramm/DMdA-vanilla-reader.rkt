(module DMdA-vanilla-reader mzscheme
  (require "DMdA-reader.rkt")
  (provide (rename -read-syntax read-syntax)
           (rename -read read))
  (define -read-syntax (make-read-syntax '(lib "DMdA-vanilla.ss" "deinprogramm")))
  (define -read (make-read '(lib "DMdA-vanilla.ss" "deinprogramm"))))
