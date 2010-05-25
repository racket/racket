#lang scheme

(provide (all-defined-out))

#;
(define-syntax (honu-syntax stx)
  (syntax-case stx ()
    [(_ expr)
     #'(honu-unparsed-expr expr)
     #;
     (begin
       (printf "honu syntax ~a\n" stx)
       (raise-syntax-error 'honu-syntax "dont call this")
       #'(make-honu-transformer (lambda (stx ctx)
                                  (printf "honu syntax ~a\n" stx)
                                  #'(expr ...))))]))

#;
(define-syntax honu-unparsed-expr
  (lambda (stx) (raise-syntax-error 'honu-unparsed-expr "dont use this")))

(define honu-scheme-syntax 'honu-scheme-syntax)

#;
(define honu-scheme-syntax (gensym))

(define-syntax-rule (scheme-syntax stx)
                    (syntax-property (syntax stx) honu-scheme-syntax #t))

(define (raw-scheme? stx)
  (syntax-property stx honu-scheme-syntax))

(define (apply-scheme-syntax stx)
  (syntax-property stx honu-scheme-syntax #t))

#;
(define-syntax (scheme-syntax stx)
  (syntax-case stx ()
    [(_ x ...)
     (lambda () '(syntax-property #'(x ...) honu-scheme-syntax #t))]))
