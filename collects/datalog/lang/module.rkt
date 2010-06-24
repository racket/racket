#lang racket/base
(require "lang.rkt")

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket/base) 
                     #%top-interaction
                     #%module-begin))