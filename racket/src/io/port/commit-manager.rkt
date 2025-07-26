#lang racket/base
(require "../host/thread.rkt")

;; A commit manager orchestrates attempts to commit peeked
;; bytes in potentially many threads

(provide make-commit-manager
         commit-manager-pause         
         commit-manager-wait)

(struct commit-manager (pause-channel commit-channel thread)
  #:authentic)

(struct commit-request (ext-evt progress-evt abandon-evt finish result-ch thread)
  #:authentic)
(struct commit-response (abandon-evt result-put-evt)
  #:authentic)

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
        (define-values (req-peek-evts remain-reqs all-resps)
          (for/fold ([req-peek-evts null] [reqs null] [resps live-resps]) ([req (in-list live-reqs)])
            ;; need to atomically succeed synchronizing and calling finish,
            ;; or gether a peek-style event that will tell us when it's worth
            ;; trying again
            (atomically
             (cond
               [(sync/timeout 0 (commit-request-progress-evt req))
                ;; commit fails due to progress
                (values req-peek-evts
                        reqs
                        (cons (commit-response
                               (commit-request-abandon-evt req)
                               (channel-put-evt
                                (commit-request-result-ch req)
                                #f))
                              resps))]
               [(not (thread-running? (commit-request-thread req)))
                ;; a suspended thread should not complete a commit;
                ;; wake up and try again when the thread resumes
                (values (cons (thread-resume-evt (commit-request-thread req))
                              req-peek-evts)
                        (cons req reqs)
                        resps)]
               [else
                (define evt (commit-request-ext-evt req))
                (define results-or-peeking-evt
                  (cond
                    [(semaphore? evt)
                     (if (sync/timeout 0 evt)
                         null
                         (semaphore-peek-evt evt))]
                    [(channel? evt)
                     (channel-get-poll-or-semaphore evt)]
                    [(channel-put-evt? evt)
                     (channel-put-poll-or-semaphore evt)]
                    [else
                     ;; anything else can be polled or `sync`ed multiple times
                     (if (sync/timeout 0 evt)
                         null
                         evt)]))
                (cond
                  [(or (pair? results-or-peeking-evt)
                       (null? results-or-peeking-evt))
                   ;; success, so call finishing function while
                   ;; still in atomic mode; note that this may
                   ;; cause some other requests to see progress
                   ((commit-request-finish req))
                   (values req-peek-evts
                           reqs
                           (cons (commit-response
                                  (commit-request-abandon-evt req)
                                  (channel-put-evt
                                   (commit-request-result-ch req)
                                   #t))
                                 resps))]
                  [else
                   ;; wake up and try again when the peeking evt is ready
                   (values (cons results-or-peeking-evt req-peek-evts)
                           (cons req reqs)
                           resps)])]))))
        (apply
         sync
         (handle-evt pause-ch
                     (lambda (evt)
                       ;; The port's state can change in other
                       ;; threads only while the manager thread is
                       ;; right here, before the `sync` completes:
                       (sync evt)
                       (loop remain-reqs all-resps)))
         (handle-evt commit-ch
                     (lambda (req)
                       (loop (cons req remain-reqs) all-resps)))
         (append
          (for/list ([req-peek-evt (in-list req-peek-evts)])
            (handle-evt req-peek-evt
                        (lambda (v)
                          ;; loop to poll requests, since the corresponding
                          ;; request may now succeed
                          (loop remain-reqs all-resps))))
          (for/list ([resp (in-list all-resps)])
            (handle-evt (commit-response-result-put-evt resp)
                        (lambda (ignored)
                          ;; response delivered
                          (loop remain-reqs
                                (remq resp all-resps))))))))))))

(define (poll-commit-liveness reqs resps)
  (let loop ([reqs reqs] [live-reqs '()] [resps resps])
    (cond
      [(null? reqs) (values live-reqs resps)]
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
      ;; resume the manager thread, just in case:
      (thread-resume (commit-manager-thread mgr) (current-thread))
      ;; ask the manager to pause; syncing on the channel means that
      ;; it has stopped trying a commit sync; we let the manager
      ;; thread resume by posting to the lock --- but beware that
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

;; in atomic mode; can leave it and return
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
                                        result-ch
                                         (current-thread))))
      (sync result-ch)))
   (lambda ()
     (semaphore-post abandon-evt))))
