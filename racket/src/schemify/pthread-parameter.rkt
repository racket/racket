#lang racket/base
(require "wrap.rkt"
         "known.rkt")

(provide pthread-parameter?)

(define (pthread-parameter? v prim-knowns knowns mutated)
  (and (wrap-pair? v)
       (wrap-pair? (wrap-cdr v))
       (wrap-null? (wrap-cdr (wrap-cdr v)))
       (let ([u-rator (unwrap (wrap-car v))])
         (or (eq? u-rator 'make-pthread-parameter)
             (and (symbol? u-rator)
                  (let ([k (hash-ref knowns u-rator #f)])
                    (and (known-copy? k)
                         (eq? 'make-pthread-parameter (known-copy-id k)))))))))
