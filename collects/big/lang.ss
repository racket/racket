(module lang mzscheme
  (require "define-struct.ss"
           "new-lambda.ss")

  (provide (all-from-except mzscheme #%datum lambda define #%app define-struct)
           (rename new-datum #%datum)
           (rename new-lambda lambda)
           (rename new-define define)
           (rename new-app #%app)
           make-keyword-procedure
           keyword-apply
           procedure-keywords
           (rename define-struct* define-struct)
           struct-field-index))


