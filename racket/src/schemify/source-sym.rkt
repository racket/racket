#lang racket/base
(require "match.rkt"
         "wrap.rkt")

(provide get-definition-source-syms)

(define (get-definition-source-syms bodys)
  (for/fold ([src-syms #hasheq()]) ([body (in-list bodys)])
    (match body
      [`(define-values ,ids ,rhs)
       (for/fold ([src-syms #hasheq()]) ([id (in-list ids)])
         (define u-id (unwrap id))
         (define sym (or (wrap-property id 'source-name) u-id))
         (cond
           [(eq? sym u-id) src-syms]
           [else (hash-set src-syms u-id sym)]))]
      [`,_ src-syms])))


