#lang scheme

(require scheme/sandbox unstable/syntax "../sandbox.ss")

(define (evaluator . require-specs)
  (let* ([ev (make-scribble-evaluator 'scheme)])
    (ev `(require ,@require-specs))
    ev))

(provide evaluator)
