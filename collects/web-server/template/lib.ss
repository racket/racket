#lang scheme

(provide for)
(define-syntax for
  (syntax-rules ()
    [(_ (for-clause ...) body ...)
     (apply
      string-append
      (for/list (for-clause ...)
        (#%string-append body ...)))]))

(provide (rename-out [#%string-append t]))

(provide #%string-append)
(define-syntax (#%string-append stx)
  (syntax-case stx ()
    [(_) 
     (syntax/loc stx "")]
    [(_ e)
     (syntax/loc stx e)]
    [(_ e1 e2 e ...)
     (let ([e1* (syntax->datum #'e1)]
           [e2* (syntax->datum #'e2)])
       (cond
         [(and (string? e1*) (string? e2*))
          (quasisyntax/loc stx
            (#%string-append #,(datum->syntax stx (string-append e1* e2*) stx)
                             e ...))]
         [else
          (syntax/loc stx
            (string-append e1 (#%string-append e2 e ...)))]))]))
