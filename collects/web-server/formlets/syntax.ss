#lang scheme
(require (for-syntax scheme)
         "lib.ss"
         (for-syntax "lib.ss"))

(define-for-syntax (cross-of stx)
  (syntax-case stx (unquote unquote-splicing => #%#)
    [s (string? (syntax->datum #'s))
       (syntax/loc stx empty)]
    [,(formlet . => . name) (syntax/loc stx name)]
    [,e (syntax/loc stx empty)]
    [,@e (syntax/loc stx empty)]
    [(#%# n ...)
     (quasisyntax/loc stx (list #,@(map cross-of (syntax->list #'(n ...)))))]
    [(t ([k v] ...) n ...)
     (quasisyntax/loc stx (list #,@(map cross-of (syntax->list #'(n ...)))))]
    [(t n ...)
     (quasisyntax/loc stx (list #,@(map cross-of (syntax->list #'(n ...)))))]))

(define-for-syntax (circ-of stx)
  (syntax-case stx (unquote unquote-splicing => #%#)
    [s (string? (syntax->datum #'s))
       (syntax/loc stx (text s))]
    [,(formlet . => . name) (syntax/loc stx formlet)]
    [,e (syntax/loc stx (xml e))]
    [,@e (syntax/loc stx (xml-forest e))]
    [(#%# n ...)
     (let ([n-cross (map cross-of (syntax->list #'(n ...)))])
       (quasisyntax/loc stx
         (cross*
          (pure (match-lambda*
                  [(list #,@n-cross)
                   (list #,@n-cross)]))
          #,@(map circ-of (syntax->list #'(n ...))))))]
    [(t ([k v] ...) n ...)
     (quasisyntax/loc stx
       (tag-xexpr `t `([k v] ...)
                 #,(circ-of (syntax/loc stx (#%# n ...)))))]
    [(t n ...)
     (quasisyntax/loc stx
       (tag-xexpr `t empty
                 #,(circ-of (syntax/loc stx (#%# n ...)))))]))

(define-syntax (formlet stx)
  (syntax-case stx ()
    [(_ q e)
     (quasisyntax/loc stx
       (cross (pure (match-lambda [#,(cross-of #'q) e]))
              #,(circ-of #'q)))]))

(provide formlet)
