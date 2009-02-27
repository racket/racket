#lang scheme/base

(require scheme/include (for-syntax scheme/base)
          (only-in scribble/private/lp chunk)
          scribble/manual)

(provide lp-include)

(define-syntax (module stx)
  (syntax-case stx ()
    [(module name base body ...)
     (begin
       #'(begin body ...))]))

(define-syntax (lp-include stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([there (datum->syntax stx 'there)])
       #'(include-at/relative-to here there name))]))

