#lang racket/base
(require "modbeg.rkt"
         ;; `for-syntax` import intended to push "modbeg.rkt"
         ;; into it's own submodule pane
         (only-in (for-syntax "modbeg.rkt")))

(provide (rename-out [module-begin #%module-begin]))
