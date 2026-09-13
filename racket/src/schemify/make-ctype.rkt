#lang racket/base
(require "wrap.rkt"
         "known.rkt"
         "import.rkt")

(provide make-ctype?/rep)

(define (make-ctype?/rep v prim-knowns knowns imports mutated)
  (and (wrap-pair? v)
       (let ([u-rator (unwrap (wrap-car v))])
         (and (or (eq? u-rator 'make-ctype)
                  (eq? u-rator 'assert-ctype))
              (wrap-pair? (wrap-cdr v))
              (let loop ([u-arg (unwrap (wrap-car (wrap-cdr v)))])
                (and (symbol? u-arg)
                     (let ([k (or (hash-ref prim-knowns u-arg #f)
                                  (hash-ref-either knowns imports u-arg))])
                       (cond
                         [(known-ctype? k) (known-ctype-rep k)]
                         [(known-copy? k) (loop (unwrap (known-copy-id k)))]
                         [else #f]))))))))
