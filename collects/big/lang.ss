(module lang mzscheme
  (require "define-struct.ss"
           "new-lambda.ss"
           (lib "contract.ss")
           (lib "for.ss"))

  (provide (all-from (lib "contract.ss"))
           (all-from (lib "for.ss"))
           (all-from-except mzscheme #%datum lambda define #%app define-struct)
           (rename new-datum #%datum)
           (rename new-lambda lambda)
           (rename new-define define)
           (rename new-app #%app)
           (rename #%app #%plain-app)
           (rename lambda #%plain-lambda)
           make-keyword-procedure
           keyword-apply
           procedure-keywords
           (rename define-struct* define-struct)
           struct-field-index))
