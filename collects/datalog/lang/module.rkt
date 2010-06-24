#lang scheme/base
(require "lang.ss")

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base) 
                     #%top-interaction
                     #%module-begin))