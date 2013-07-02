#lang typed/racket

;; Integration test for synchronizable events
;;
;; example from unstable/logging

(define-type Log-Receiver-Sync-Result
  (Vector Symbol String Any (Option Symbol)))

(: receiver-thread
   (Log-Receiver (Channelof 'stop)
    (Log-Receiver-Sync-Result -> Void)
    -> Thread))
(define (receiver-thread receiver stop-chan intercept)
  (thread
   (lambda ()
     (: clear-events (-> Void))
     (define (clear-events)
       (let: ([l : (Option Log-Receiver-Sync-Result)
               (sync/timeout 0 receiver)])
         (when l ; still something to read
           (intercept l) ; interceptor gets the whole vector
           (clear-events))))
     (let loop ()
       (let: ([l : (U Log-Receiver-Sync-Result 'stop)
               (sync receiver stop-chan)])
         (cond [(eq? l 'stop)
                ;; we received all the events we were supposed
                ;; to get, read them all (w/o waiting), then
                ;; stop
                (clear-events)]
               [else ; keep going
                (intercept l)
                (loop)]))))))

(struct: listener ([stop-chan : (Channelof 'stop)]
                   ;; ugly, but the thread and the listener need to know each
                   ;; other
                   [thread : (Option Thread)]
                   [rev-messages : (Listof Log-Receiver-Sync-Result)]
                   [done? : Any])
          #:mutable)

(: start-recording (Log-Level -> listener))
(define (start-recording log-level)
  (let* ([receiver     (make-log-receiver (current-logger) log-level)]
         [stop-chan    ((inst make-channel 'stop))]
         [cur-listener (listener stop-chan #f '() #f)]
         [t (receiver-thread
             receiver stop-chan
             (lambda: ([l : Log-Receiver-Sync-Result])
               (set-listener-rev-messages!
                cur-listener
                (cons l (listener-rev-messages cur-listener)))))])
    (set-listener-thread! cur-listener t)
    cur-listener))

(: stop-recording (listener -> (Listof Log-Receiver-Sync-Result)))
(define (stop-recording cur-listener)
  (define the-thread (listener-thread cur-listener))
  (unless (or (not the-thread)
              (listener-done? cur-listener))
    (channel-put (listener-stop-chan cur-listener)
                 'stop) ; stop the receiver thread
    (thread-wait the-thread)
    (set-listener-done?! cur-listener #t))
  (reverse (listener-rev-messages cur-listener)))

(: with-intercepted-logging
   (((Vector Symbol String Any (Option Symbol)) -> Void)
    (-> Void)
    Log-Level
    -> Void))
(define (with-intercepted-logging interceptor proc log-level)
  (let* ([orig-logger (current-logger)]
         ;; We use a local logger to avoid getting messages that didn't
         ;; originate from proc. Since it's a child of the original logger,
         ;; the rest of the program still sees the log entries.
         [logger      (make-logger #f orig-logger)]
         [receiver    (make-log-receiver logger log-level)]
         [stop-chan   ((inst make-channel 'stop))]
         [t           (receiver-thread receiver stop-chan interceptor)])
    (begin0
        (parameterize ([current-logger logger])
          (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))

(require typed/rackunit)

;; extracted from unstable/logging tests
(let ([l (start-recording 'warning)])
  (log-warning "1")
  (log-warning "2")
  (log-warning "3")
  (log-info "4")
  (stop-recording l) ; stopping should be idempotent
  (let ([out (stop-recording l)])
    (check-equal? (map (lambda: ([l : Log-Receiver-Sync-Result])
                         (vector-ref l 1)) out)
                  '("1" "2" "3"))
    (check-true (andmap (lambda: ([l : Log-Receiver-Sync-Result])
                          (eq? (vector-ref l 0) 'warning))
                        out))))

