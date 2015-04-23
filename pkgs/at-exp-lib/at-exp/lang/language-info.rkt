#lang racket/base

(provide get-language-info)

(require racket/match)

(define (get-language-info data)
  (define other-get-info
    (match data
      [(vector mod sym data2)
       ((dynamic-require mod sym) data2)]
      [_ (lambda (key default) default)]))
  (lambda (key default)
    (case key
      [(configure-runtime)
       (define config-vec '#[at-exp/lang/runtime-config configure #f])
       (define other-config (other-get-info key default))
       (cond [(list? other-config) (cons config-vec other-config)]
             [else (list config-vec)])]
      [else (other-get-info key default)])))
