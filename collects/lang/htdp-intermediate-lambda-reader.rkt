(module htdp-intermediate-lambda-reader mzscheme
  (require "htdp-reader.rkt")
  (provide (rename -read-syntax read-syntax)
           (rename -read read))
  (define -read-syntax (make-read-syntax '(lib "htdp-intermediate-lambda.ss" "lang")))
  (define -read (make-read '(lib "htdp-intermediate-lambda.ss" "lang"))))
