#lang racket/base
(require scribble/manual
         scribble/eval
         (for-label racket/base
                    racket/contract))
(provide (all-defined-out)
         (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)))

(define (tech/reference . pre-flows)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") pre-flows))

;; ----

(define the-eval (make-base-eval))
(void
 (interaction-eval #:eval the-eval
                   (require racket/class
                            db/base
                            db/util/datetime))
 (interaction-eval #:eval the-eval
                   (define connection% (class object% (super-new))))
 (interaction-eval #:eval the-eval
                   (define connection-pool% (class object% (super-new)))))

(define-syntax-rule (examples/results [example result] ...)
  (examples #:eval the-eval (eval:alts example result) ...))
(define-syntax-rule (my-interaction [example result] ...)
  (interaction #:eval the-eval (eval:alts example result) ...))
