#lang racket/base
(require syntax/parse
         (for-template racket/base))

(define-syntax-class sre 
  #:attributes (machine)
  
  (pattern pat:expr
           #:attr machine
           #'1))

(provide sre)
