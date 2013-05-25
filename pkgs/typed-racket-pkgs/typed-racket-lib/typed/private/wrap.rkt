#lang scheme

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ id)
     #'(#%plain-module-begin
	(require id)
	(provide (all-from-out id)))]))

(provide (rename-out [mb #%module-begin]))
