#lang racket/base
(require "lang.rkt")

(provide (rename-out [module-begin #%module-begin]
                     ; XXX Because the REPL doesn't use the correct reader, I can't really test this
                     #;[top-interaction #%top-interaction])
         (except-out (all-from-out racket/base) 
                     #%top-interaction
                     #%module-begin))