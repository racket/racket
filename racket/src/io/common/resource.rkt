#lang racket/base
(require "../host/thread.rkt"
         "../host/rktio.rkt")

(provide call-with-resource)

;; In uninterruptable mode
;;
;; The `destroy` function is called in uninterruptible mode only if `handle`
;; hasn't returned by the time of an escape or thread kill, and only if
;; the resource `r` is not a rktio error or a boxed rktio error. In the case
;; of a thread kill, `destroy` is called in atomic mode. So,
;; at the point where `r` is destoyed by `handle`, `handle` must
;; return still in uninterruptable mode to ensure that `destroy` is not
;; triggered.
;;
(define (call-with-resource r destroy handle)
  (cond
    [(or (rktio-error? r)
         (and (box? r)
              (rktio-error? (unbox r))))
     (handle r)]
    [else
     (define completed? #f)
     (define (do-destroy)
       (unless completed?
         (destroy r)))
     (thread-push-kill-callback! do-destroy)
     (dynamic-wind
      void
      (lambda ()
        (begin0
          (handle r)
          (set! completed? #t)))
      (lambda ()
        (thread-pop-kill-callback!)
        ;; In case of an escape out of the body, we
        ;; may not be in atomic mode:
        (start-uninterruptible)
        (do-destroy)
        (end-uninterruptible)))]))
