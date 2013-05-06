#lang racket/base

(require "private/namespace.rkt")

(define-syntax-rule (module-begin . forms)
  (#%printing-module-begin 
   (module configure-runtime '#%kernel
     (#%require scheme/runtime-config)
     (configure #f))
   . forms))

(provide (except-out (all-from-out racket/base) 
                     struct
                     hash hasheq hasheqv
                     in-directory
                     local-require
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         make-base-empty-namespace
         make-base-namespace)

(module reader syntax/module-reader
  scheme/base)

(module configure-runtime '#%kernel
  (#%require scheme/runtime-config)
  (configure #f))
