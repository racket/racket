(module htdp-beginner-reader mzscheme
  (require "htdp-reader.rkt")
  (provide (rename -read-syntax read-syntax)
           (rename -read read))
  (define -read-syntax (make-read-syntax '(lib "htdp-beginner.ss" "lang")))
  (define -read (make-read '(lib "htdp-beginner.ss" "lang"))))
