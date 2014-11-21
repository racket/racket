#lang racket/base
(require syntax/parse racket/syntax
         (for-template racket/unsafe/ops racket/base (prefix-in k- '#%kernel))
         "../utils/utils.rkt"
         (optimizer utils logging))

(provide apply-opt-expr)

(define-literal-syntax-class +)
(define-literal-syntax-class *)
(define-literal-syntax-class k-apply)
(define-literal-syntax-class map)
(define-literal-syntax-class app^ (#%plain-app))


(define-syntax-class apply-op
  #:commit
  (pattern :+^ #:with identity #'0)
  (pattern :*^ #:with identity #'1))

(define-syntax-class apply-opt-expr
  #:commit
  (pattern (kw:app^ appl:k-apply^ op:apply-op (kw2:app^ m:map^ f:opt-expr l:opt-expr))
    #:do [(log-opt "apply-map" "apply-map deforestation.")]
    #:with opt (with-syntax ([(f* lp v lst) (map generate-temporary '(f* loop v lst))])
                 (syntax/loc/origin
                  this-syntax #'kw
                  (let ([f* f.opt])
                    (let loop ([v op.identity] [lst l.opt])
                      (if (null? lst)
                          v
                          (loop (op v (f* (unsafe-car lst)))
                                (unsafe-cdr lst)))))))))
