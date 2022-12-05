#lang racket/base

;; Check whether `thread-dead-evt` works right when a thread is
;; blocked and GCed before it is terminated via a custodian

(define (go thread-wait*
            #:adjust [adjust void]
            #:finalize [finalize void])
  (define c (make-custodian))
  (define done (make-semaphore))
  (let ([t (parameterize ([current-custodian c])
             (thread (lambda ()
                       (sync (make-semaphore)))))])
    (adjust t)
    (thread (lambda ()
              ;; make sure this completes even if the thread is
              ;; GCed before its custodian is shut down
              (let ([t0 t])
                (set! t #f) ; extra help for BC, which probably retains the initial closure
                (thread-wait* t0)
                (semaphore-post done)))))
  (sync (system-idle-evt))
  (collect-garbage)
  (collect-garbage)
  (custodian-shutdown-all c)
  (finalize)
  (semaphore-wait done))

(go thread-wait)
(go (lambda (t) (sync (thread-dead-evt t))))

(for ([rev (in-list (list values reverse))])
  (let* ([c1 (make-custodian)]
         [c2 (make-custodian c1)]
         [c3 (make-custodian)]
         [cs (rev (list c1 c2 c3))])
    (go thread-wait
        #:adjust (lambda (t)
                   (for ([c (in-list cs)])
                     (thread-resume t c)))
        #:finalize (lambda ()
                     (for ([c (in-list cs)])
                       (custodian-shutdown-all c))))))
