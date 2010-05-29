#lang scheme

(require scheme/sandbox unstable/syntax unstable/sandbox)

(define (evaluator . require-specs)
  (let* ([ev (make-scribble-evaluator 'scheme)])
    (ev `(require ,@require-specs))
    ev))

(provide evaluator)
