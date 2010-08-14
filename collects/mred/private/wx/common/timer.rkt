#lang scheme/base
(require scheme/class
         "../../syntax.rkt"
         "../../lock.rkt"
         "queue.rkt")

(provide timer%)

;; FIXME: need checks
(defclass timer% object%
  (init [notify-callback void]
        [(ival interval) #f]
        [just-once? #f])
  (define notify-cb notify-callback)
  (define current-interval ival)
  (define current-once? (and just-once? #t))
  (define cb #f)
  (def/public (interval) current-interval)
  (define/private (do-start msec once?)
    (as-entry
     (lambda ()
       (do-stop)
       (set! current-interval msec)
       (set! current-once? (and once? #t))
       (letrec ([new-cb
                 (make-timer-callback (+ msec (current-inexact-milliseconds))
                                      (lambda ()
                                        (when (eq? cb new-cb)
                                          (notify)
                                          (as-entry
                                           (lambda ()
                                             (unless once?
                                               (when (eq? cb new-cb)
                                                 (do-start msec #f))))))))])
         (set! cb new-cb)
         (add-timer-callback new-cb)))))
  (def/public (start [(integer-in 0 1000000000) msec] [any? [once? #f]])
    (do-start msec once?))
  (define/private (do-stop)
    (as-entry
     (lambda ()
       (when cb
         (remove-timer-callback cb)
         (set! cb #f)))))
  (def/public (stop) (do-stop))
  (def/public (notify) (notify-cb) (void))
  (super-new)
  (when ival
    (start ival just-once?)))
