#lang racket/base

;; Like "thread-wait-gc.rkt", but for `thread/suspend-to-kill`
;; and and `thread-suspend-evt`.

(define (go #:adjust [adjust void]
            #:finalize [finalize void])
  (define c (make-custodian))
  (define done (make-semaphore))
  (let ([t (parameterize ([current-custodian c])
             (thread/suspend-to-kill
              (lambda ()
                (sync (make-semaphore)))))])
    (adjust t)
    (thread (lambda ()
              ;; make sure this completes even if the thread is
              ;; GCed before its custodian is shut down
              (let ([t0 t])
                (set! t #f) ; extra help for BC, which probably retains the initial closure
                (sync (thread-suspend-evt t0))
                (semaphore-post done)))))
  (sync (system-idle-evt))
  (collect-garbage)
  (collect-garbage)
  (custodian-shutdown-all c)
  (finalize)
  (semaphore-wait done))

(go)

(for ([rev (in-list (list values reverse))])
  (let* ([c1 (make-custodian)]
         [c2 (make-custodian c1)]
         [c3 (make-custodian)]
         [cs (rev (list c1 c2 c3))])
    (go #:adjust (lambda (t)
                   (for ([c (in-list cs)])
                     (thread-resume t c)))
        #:finalize (lambda ()
                     (for ([c (in-list cs)])
                       (custodian-shutdown-all c))))))
