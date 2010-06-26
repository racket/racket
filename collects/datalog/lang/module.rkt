#lang racket/base
(require "lang.rkt")

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         (except-out (all-from-out racket/base) 
                     #%top-interaction
                     #%module-begin))