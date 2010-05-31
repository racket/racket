#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
          "../common/queue.rkt"
         "frame.rkt")

(provide dialog%)

(defclass dialog% frame%
  (super-new [is-dialog? #t])

  (define close-sema #f)

  (define/override (direct-show on?)
    (unless on?
      (when close-sema
        (semaphore-post close-sema)
        (set! close-sema #f)))
    (super direct-show on?))

  (define/override (show on?)
    (if on?
        (unless close-sema
          (let ([s (make-semaphore)])
            (set! close-sema s)
            (super show on?)
            (yield s)))
        (super show on?))))
