(module htdp-intermediate-reader mzscheme
  (require "htdp-reader.ss")
  (provide (rename -read-syntax read-syntax)
           (rename -read read))
  (define -read-syntax (make-read-syntax '(lib "htdp-intermediate.ss" "lang")))
  (define -read (make-read '(lib "htdp-intermediate.ss" "lang"))))
