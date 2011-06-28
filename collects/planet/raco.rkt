#lang racket/base
(require "private/cmdline-tool.rkt")

(with-handlers ([exn:fail? 
                 (lambda (e) 
                   ((error-display-handler) (exn-message e) e)
                   (exit 1))])
  (start #t))
