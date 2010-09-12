#lang racket/base

(provide wait-for-lock)

;; wait-for-lock : string -> void
;;  Gets a lock on `user' for the calling thread; the lock lasts until the
;;  calling thread terminates.  If the lock was actually acquired, then on
;;  release the cleanup-thunk will be executed (unless it is #f), even if it
;;  was released when the acquiring thread crashed.
;; *** Warning: It's vital that a clean-up thunk doesn't raise an exception,
;;  since this will kill the lock thread which will lock down everything
(define (wait-for-lock user . cleanup-thunk)
  (let ([s (make-semaphore)])
    (channel-put req-ch
                 (make-req (thread-dead-evt (current-thread)) user s
                           (and (pair? cleanup-thunk) (car cleanup-thunk))))
    (semaphore-wait s)))

(define req-ch (make-channel))

(define-struct req (thread-dead-evt user sema cleanup-thunk))

(define (lock-loop)
  (let loop ([locks null]
             [reqs null])
    (let-values ([(locks reqs)
                  ;; Try to satisfy lock requests:
                  (let loop ([reqs (reverse reqs)]
                             [locks locks]
                             [new-reqs null])
                    (if (null? reqs)
                      (values locks new-reqs)
                      (let ([req (car reqs)]
                            [rest (cdr reqs)])
                        (if (assoc (req-user req) locks)
                          ;; Lock not available:
                          (loop rest locks (cons req new-reqs))
                          ;; Lock is available, so take it:
                          (begin (semaphore-post (req-sema req))
                                 (loop (cdr reqs)
                                       (cons (cons (req-user req) req) locks)
                                       new-reqs))))))])
      (sync
       (handle-evt req-ch (lambda (req) (loop locks (cons req reqs))))
       ;; Release a lock whose thread is gone:
       (apply choice-evt
              (map (lambda (name+req)
                     (handle-evt
                      (req-thread-dead-evt (cdr name+req))
                      (lambda (v)
                        ;; releasing a lock => run cleanup
                        (cond [(req-cleanup-thunk (cdr name+req))
                               => (lambda (t) (t))])
                        (loop (remq name+req locks) reqs))))
                   locks))
       ;; Throw away a request whose thread is gone:
       (apply choice-evt
              (map (lambda (req)
                     (handle-evt
                      (req-thread-dead-evt req)
                      (lambda (v) (loop locks (remq req reqs)))))
                   reqs))))))

(define lock-thread (thread lock-loop))
