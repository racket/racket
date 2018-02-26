#lang racket/base
(require "../host/thread.rkt")

;; A commit manager orchestrates attempts to commit peeked
;; bytes in potentially many threads

(provide make-commit-manager
         commit-manager-pause         
         commit-manager-wait)

(struct commit-manager (pause-channel commit-channel thread))

(struct commit-request (ext-evt progress-evt abandon-evt finish result-ch))
(struct commit-response (abandon-evt result-put-evt))

(define (make-commit-manager)
  (define pause-ch (make-channel))
  (define commit-ch (make-channel))
  (commit-manager
   pause-ch
   commit-ch
   (thread
    (lambda ()
      (let loop ([reqs '()] [resps '()])
        ;; Poll progress and abandon evts:
        (define-values (live-reqs new-resps)
          (poll-commit-liveness reqs resps))
        ;; Drop abandoned responses, too:
        (define live-resps
          (drop-abandoned new-resps))
        (apply
         sync
         (handle-evt pause-ch
                     (lambda (evt)
                       ;; The port's state can change in other
                       ;; threads only while the manager thread is
                       ;; right here, before the `sync` completes:
                       (sync evt)
                       (loop live-reqs live-resps)))
         (handle-evt commit-ch
                     (lambda (req)
                       (loop (cons req live-reqs) live-resps)))
         (append
          (for/list ([req (in-list live-reqs)])
            (handle-evt (commit-request-ext-evt req)
                        (lambda (v)
                          ;; commit request succeeds
                          (atomically
                           ((commit-request-finish req)))
                          (loop (remq req live-reqs)
                                (cons (commit-response
                                       (commit-request-abandon-evt req)
                                       (channel-put-evt
                                        (commit-request-result-ch req)
                                        #t))
                                      live-resps)))))
          (for/list ([resp (in-list live-resps)])
            (handle-evt (commit-response-result-put-evt resp)
                        (lambda (ignored)
                          ;; response delivered
                          (loop live-reqs
                                (remq resp live-resps))))))))))))

(define (poll-commit-liveness reqs resps)
  (let loop ([reqs reqs] [live-reqs '()] [resps resps])
    (cond
      [(null? reqs) (values live-reqs resps)]
      [(sync/timeout 0 (commit-request-progress-evt (car reqs)))
       ;; commit fails
       (loop (cdr reqs)
             live-reqs
             (cons (commit-response
                    (commit-request-abandon-evt (car reqs))
                    (channel-put-evt
                     (commit-request-result-ch (car reqs))
                     #f))
                   resps))]
      [(sync/timeout 0 (commit-request-abandon-evt (car reqs)))
       ;; request abandoned
       (loop (cdr reqs) live-reqs resps)]
      [else
       (loop (cdr reqs) (cons (car reqs) live-reqs) resps)])))

(define (drop-abandoned resps)
  (for/list ([resp (in-list resps)]
             #:unless (sync/timeout 0 (commit-response-abandon-evt resp)))
    resp))

;; in atomic mode; can leave it and return
;;  After this function returns, the committing thread
;;  is definitely not trying to sync to complete
;;  a commit, but it can resume as soon as we go back
;;  out of atomic mode
(define (commit-manager-pause mgr)
  (define lock (make-semaphore))
  (define suspend-evt (thread-suspend-evt (current-thread)))
  (dynamic-wind
   void
   (lambda ()
     (non-atomically
      ;; the manager thread, just in case:
      (thread-resume (commit-manager-thread mgr) (current-thread))
      ;; ask the manager to pause; syncing on the channel means that
      ;; it has stopped trying a commit sync; we let the manager
      ;; thread resume by posting to th elock --- but beware that
      ;; *this* thread might get suspended or killed
      (sync
       (channel-put-evt (commit-manager-pause-channel mgr)
                        (choice-evt (list lock
                                          suspend-evt
                                          (thread-dead-evt (current-thread))))))))
   (lambda ()
     ;; Either back in atomic mode or escaping, so it's ok for the
     ;; waiting thread to try again when it eventually gets to run
     (semaphore-post lock)))
  ;; If this thread was suspended during `pause-waiting-commit`, we
  ;; may have let the committing thread go, so try again
  (when (sync/timeout 0 suspend-evt)
    (commit-manager-pause mgr)))

;; in atmomic mode; can leave it and return
(define (commit-manager-wait mgr progress-evt ext-evt finish)
  (define result-ch (make-channel))
  (define abandon-evt (make-semaphore))
  (dynamic-wind
   void
   (lambda ()
     (non-atomically
      (sync
       (channel-put-evt (commit-manager-commit-channel mgr)
                        (commit-request ext-evt
                                        progress-evt
                                        (choice-evt (list abandon-evt
                                                          (thread-dead-evt (current-thread))))
                                        finish
                                        result-ch)))
      (sync result-ch)))
   (lambda ()
     (semaphore-post abandon-evt))))
