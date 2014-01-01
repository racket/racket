#lang racket/base
(require tests/stress
         net/websocket
         net/url
         racket/async-channel)

(fit "websocket echo server"
     500
     (λ (n)
       (define confirm (make-async-channel))
       (define shutdown!
         (ws-serve #:port 0
                   #:confirmation-channel confirm
                   (λ (wsc _)
                     (let loop ()
                       (define m (ws-recv wsc))
                       (unless (eof-object? m)
                         (ws-send! wsc m)
                         (loop))))))
       (define port (async-channel-get confirm))
       
       (define THREADS 10)
       (define REQS n)
       
       (for-each thread-wait
                 (for/list ([t (in-range THREADS)])
                   (thread
                    (λ ()
                      (define conn (ws-connect (string->url (format "ws://localhost:~a" port))))
                      (for ([r (in-range REQS)])
                        (ws-send! conn "ping")
                        (ws-recv conn))))))
       
       (shutdown!)))

(module+ test
  (module config info
    (define random? #t)))
