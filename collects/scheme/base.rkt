#lang scheme/private
(require "private/namespace.rkt")

(provide (except-out (all-from-out racket/base) 
                     struct
                     hash hasheq hasheqv
                     in-directory
                     local-require)
         make-base-empty-namespace
         make-base-namespace)
