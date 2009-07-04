#lang scheme

(define-syntax-rule (provide/contract* [id ctrct] ...)
  #;(provide/contract [id ctrct] ...)
  (provide id ...))

(provide 
 provide/contract*)
