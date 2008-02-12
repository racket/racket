#lang scheme/base

(provide (rename-out [module-begin #%module-begin])
         quote)

(define-syntax-rule (module-begin . stuff)
  (#%module-begin 
   "The R6RS language just supplies a reader, so far."
   'stuff))
