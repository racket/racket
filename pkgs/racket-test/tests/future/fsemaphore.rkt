#lang racket

;; Check `touch` on a future blocked on an fsemaphore

(define mutex (make-fsemaphore 1))

(define futures
  (for/list ([i 10])
    (future
     (lambda ()
       (fsemaphore-wait mutex)
       (println (add1 i))
       (fsemaphore-post mutex)))))

(void
 (map sync
      (for/list ([f (in-list futures)])
        (thread (lambda () (touch f))))))
