(module a-signature mzscheme
  (require-for-syntax racket/private/unit-compiletime
                      racket/private/unit-syntax)
  (require "unit.rkt")
  
  (provide (rename module-begin #%module-begin)
           (all-from-except mzscheme #%module-begin)
           (all-from "unit.rkt"))

  (define-for-syntax (make-name s)
    (string->symbol
     (string-append (regexp-replace "-sig$" (symbol->string s) "")
                    "^")))

  (define-syntax (module-begin stx)
    (parameterize ((error-syntax stx))
      (with-syntax ((name (make-name (syntax-property stx 'enclosing-module-name))))
        (syntax-case stx ()
          ((_ . x)
           (with-syntax ((((reqs ...) . (body ...))
                          (split-requires (checked-syntax->list #'x))))
             (datum->syntax-object
              stx
              (syntax-e #'(#%module-begin
                           reqs ...
                           (provide name)
                           (define-signature name (body ...))))
              stx))))))))
  
