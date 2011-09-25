#lang racket/base
(require racket/class
         racket/contract/base
         unstable/class-iop
         "model/trace.rkt"
         "view/interfaces.rkt"
         "view/view.rkt")

(define (create-stepper deriv)
  (define director (new macro-stepper-director%))
  (define stepper (send/i director director<%> new-stepper))
  (send/i director director<%> add-deriv deriv)
  (void))

(define (expand/step stx)
  (create-stepper (trace stx)))

(define (expand-module/step module-path)
  (create-stepper (trace-module module-path)))

(provide/contract
 [expand/step
  (-> syntax? void?)]
 [expand-module/step
  (-> module-path? void?)])
