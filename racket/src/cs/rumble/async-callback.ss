
(define-record async-callback-queue (lock condition in wakeup))

(define (current-async-callback-queue)
  (place-async-callback-queue))

(define (async-callback-place-init!)
  (place-async-callback-queue (make-async-callback-queue (make-mutex)
                                                         (make-condition)
                                                         '()
                                                         (make-async-callback-poll-wakeup))))

(define (call-as-asynchronous-callback thunk)
  (with-interrupts-disabled*
   (async-callback-queue-call (current-async-callback-queue) thunk #t)))

(define (async-callback-queue-call async-callback-queue thunk need-interrupts?)
  (let* ([result-done? (box #f)]
         [result #f]
         [q async-callback-queue]
         [m (async-callback-queue-lock q)])
    (mutex-acquire m)
    (set-async-callback-queue-in! q (cons (lambda ()
                                            (set! result (thunk))
                                            (mutex-acquire m)
                                            (set-box! result-done? #t)
                                            (condition-broadcast (async-callback-queue-condition q))
                                            (mutex-release m))
                                          (async-callback-queue-in q)))
    ((async-callback-queue-wakeup q))
    (let loop ()
      (unless (unbox result-done?)
        (when need-interrupts?
          ;; Enable interrupts so that the thread is deactivated
          ;; when we wait on the condition
          (enable-interrupts))
        (condition-wait (async-callback-queue-condition q) m)
        (when need-interrupts? (disable-interrupts))
        (loop)))
    (mutex-release m)
    result))

(define make-async-callback-poll-wakeup (lambda () void))
(define (set-make-async-callback-poll-wakeup! make-wakeup)
  (set! make-async-callback-poll-wakeup make-wakeup))

;; Returns callbacks to run in atomic mode
(define (poll-async-callbacks)
  (let ([q (current-async-callback-queue)])
    (mutex-acquire (async-callback-queue-lock q))
    (let ([in (async-callback-queue-in q)])
      (cond
       [(null? in)
        (mutex-release (async-callback-queue-lock q))
        '()]
       [else
        (set-async-callback-queue-in! q '())
        (mutex-release (async-callback-queue-lock q))
        (reverse in)]))))
