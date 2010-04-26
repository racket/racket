#lang scheme/private
(require "private/namespace.ss")

(provide (except-out (all-from-out racket/base) 
                     struct
                     hash hasheq hasheqv)
         make-base-empty-namespace
         make-base-namespace)
