#lang racket/base
(provide (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin mod)
  (#%plain-module-begin
   (require mod)
   (provide (all-from-out mod))))
