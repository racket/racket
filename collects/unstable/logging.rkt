#lang racket/base

(require racket/contract/base)

(define level/c (or/c 'fatal 'error 'warning 'info 'debug))
(define log-message/c (vector/c level/c string? any/c))

;; helper used below
(define (receiver-thread receiver stop-chan intercept)
  (thread
   (lambda ()
     (define (clear-events)
       (let ([l (sync/timeout 0 receiver)])
         (when l ; still something to read
           (intercept l) ; interceptor gets the whole vector
           (clear-events))))
     (let loop ()
       (let ([l (sync receiver stop-chan)])
         (cond [(eq? l 'stop)
                ;; we received all the events we were supposed
                ;; to get, read them all (w/o waiting), then
                ;; stop
                (clear-events)]
               [else ; keep going
                (intercept l)
                (loop)]))))))

(struct listener (stop-chan
                  ;; ugly, but the thread and the listener need to know each
                  ;; other
                  [thread #:mutable]
                  [rev-messages #:mutable]
                  [done? #:mutable]))

;; [level] -> listener
(define (start-recording #:level [level 'debug])
  (let* ([receiver     (make-log-receiver (current-logger) level)]
         [stop-chan    (make-channel)]
         [cur-listener (listener stop-chan #f '() #f)]
         [t (receiver-thread
             receiver stop-chan
             (lambda (l)
               (set-listener-rev-messages!
                cur-listener
                (cons l (listener-rev-messages cur-listener)))))])
    (set-listener-thread! cur-listener t)
    cur-listener))

;; listener -> listof messages
(define (stop-recording cur-listener)
  (unless (listener-done? cur-listener)
    (channel-put (listener-stop-chan cur-listener)
                 'stop) ; stop the receiver thread
    (thread-wait (listener-thread cur-listener))
    (set-listener-done?! cur-listener #t))
  (reverse (listener-rev-messages cur-listener)))

(provide/contract
 [start-recording (->* () (#:level level/c) listener?)]
 [stop-recording  (-> listener? (listof log-message/c))])


(define (with-intercepted-logging interceptor proc #:level [level 'debug])
  (let* ([orig-logger (current-logger)]
         ;; the new logger is unrelated to the original, to avoid getting
         ;; messages sent to it that didn't originate from proc
         [logger      (make-logger)]
         [receiver    (make-log-receiver logger level)]
         [stop-chan   (make-channel)]
         [t (receiver-thread
             receiver stop-chan
             (lambda (l)
               ;; we want to send l to the original logger, so that
               ;; the rest of the system can see it too.
               (log-message orig-logger
                            (vector-ref l 0)  ; level
                            (vector-ref l 1)  ; message
                            (vector-ref l 2)) ; data
               (interceptor l)))])
    (begin0
        (parameterize ([current-logger logger])
          (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))

(define (with-logging-to-port port proc #:level [level 'debug])
  (with-intercepted-logging
   (lambda (l) (displayln (vector-ref l 1) ; actual message
                          port))
   proc #:level level))

(provide/contract [with-intercepted-logging
                   (->* ((-> log-message/c any)
                         (-> any))
                        (#:level level/c)
                        any)]
                  [with-logging-to-port
                   (->* (output-port? (-> any))
                        (#:level level/c)
                        any)])
