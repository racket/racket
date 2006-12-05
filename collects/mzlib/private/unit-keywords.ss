(module unit-keywords mzscheme
  (provide (all-defined-except define-syntax-for-error))

  (define-syntax define-syntax-for-error
    (syntax-rules ()
      ((_ name message)
       (begin
         (define-syntax name
           (make-set!-transformer
            (lambda (stx)
              (raise-syntax-error 
               #f
               message
               stx))))))))
  
  (define-syntax-for-error only
    "misuse of unit import keyword")
  (define-syntax-for-error except
    "misuse of unit import keyword")
  (define-syntax-for-error prefix
    "misuse of unit import and export keyword")
  (define-syntax-for-error rename
    "misuse of unit import and export keyword")
  (define-syntax-for-error tag
    "misuse of unit import and export keyword")
  (define-syntax-for-error import
    "misuse of unit keyword")
  (define-syntax-for-error export
    "misuse of unit keyword")
  (define-syntax-for-error init-depend
    "misuse of unit keyword")
  (define-syntax-for-error link
    "misuse of compound-unit keyword")
  (define-syntax-for-error extends
    "misuse of define-signature keyword"))
  
