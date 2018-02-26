#lang racket/base
(require racket/pretty
         "chezify.rkt")

(pretty-print
 (chezify-linklet '(linklet 
                    (import (a b c))
                    (export f g x)
                    (define-values (f) (lambda () (g)))
                    (define-values (g) (lambda () (a (f))))
                    (define-values (x) 5))
                  #hasheq()))

