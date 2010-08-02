#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
          "../common/queue.rkt"
          "../../lock.rkt"
         "frame.rkt")

(provide dialog%)

(defclass dialog% frame%
  (super-new [is-dialog? #t])

  (define close-sema #f)

  (define/override (direct-show on?)
    (unless on?
      (as-entry
       (lambda ()
         (when close-sema
           (semaphore-post close-sema)
           (set! close-sema #f)))))
    (super direct-show on?))

  ;; #t result avoids children sheets
  (define/override (get-sheet) #t)

  (define/override (show on?)
    (if on?
        (let ([s (as-entry
                  (lambda ()
                    (let ([s (or close-sema (make-semaphore))])
                      (unless close-sema (set! close-sema s))
                      s)))])
          (super show on?)
          (yield s)
          (void))
        (super show on?))))
