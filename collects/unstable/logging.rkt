#lang racket/base

(require racket/contract/base)

(define level/c (or/c 'fatal 'error 'warning 'info 'debug))
(define log-spec/c (listof (or/c symbol? #f)))
(define log-message/c (vector/c level/c string? any/c (or/c symbol? #f)))

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
(define (start-recording . log-spec)
  (let* ([receiver     (apply make-log-receiver (current-logger) log-spec)]
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
 [start-recording (->* () #:rest log-spec/c listener?)]
 [stop-recording  (-> listener? (listof log-message/c))])


(define (with-intercepted-logging interceptor proc . log-spec)
  (let* ([orig-logger (current-logger)]
         ;; We use a local logger to avoid getting messages that didn't
         ;; originate from proc. Since it's a child of the original logger,
         ;; the rest of the program still sees the log entries.
         [logger      (make-logger #f orig-logger)]
         [receiver    (apply make-log-receiver logger log-spec)]
         [stop-chan   (make-channel)]
         [t           (receiver-thread receiver stop-chan interceptor)])
    (begin0
        (parameterize ([current-logger logger])
          (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))

(define (with-logging-to-port port proc . log-spec)
  (apply with-intercepted-logging
         (lambda (l) (displayln (vector-ref l 1) ; actual message
                                port))
         proc
         log-spec))

(provide/contract [with-intercepted-logging
                   (->* ((-> log-message/c any)
                         (-> any))
                        #:rest log-spec/c
                        any)]
                  [with-logging-to-port
                   (->* (output-port? (-> any))
                        #:rest log-spec/c
                        any)])
