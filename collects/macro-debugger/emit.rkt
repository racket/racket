#lang racket/base
(require racket/contract/base)

(provide/contract
 [emit-remark
  (->* () (#:unmark? any/c) #:rest (listof (or/c string? syntax?))
       any)])

(define current-expand-observe
  (dynamic-require ''#%expobs 'current-expand-observe))

(define (emit-remark #:unmark? [unmark? #t] . args)
  (let ([observe (current-expand-observe)])
    (when observe
      (let ([args
             (if unmark?
                 (for/list ([arg (in-list args)])
                   (if (syntax? arg)
                       (syntax-local-introduce arg)
                       arg))
                 args)])
        (observe 'local-remark args)))))
