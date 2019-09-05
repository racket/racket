#lang racket/base
(require "wrap.rkt"
         "known.rkt")

(provide parameter-result?)

(define (parameter-result? v prim-knowns knowns mutated)
  (and (wrap-pair? v)
       (let ([u-rator (unwrap (wrap-car v))])
         (or (eq? u-rator 'make-parameter)
             (eq? u-rator 'derived-parameter)
             (eq? u-rator 'make-pthread-parameter)
             (and (symbol? u-rator)
                  (let ([k (hash-ref knowns u-rator #f)])
                    (and (known-copy? k)
                         (let ([id (known-copy-id k)])
                           (or (eq? 'make-parameter id)
                               (eq? 'make-derived-parameter id)
                               (eq? 'make-pthread-parameter id))))))))))
