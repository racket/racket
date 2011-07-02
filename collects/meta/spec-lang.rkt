#lang scheme/base
(require "specs.rkt")

(provide (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin . rest)
  (#%module-begin
   (provide register-specs!)
   (define (register-specs! [param *specs*])
     (process-specs 'rest param))))
