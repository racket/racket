#lang racket/base
(require net/url
         (prefix-in srfi-date: srfi/19)
         racket/date
         racket/async-channel
         racket/match
         racket/contract)
(require web-server/dispatchers/dispatch
         web-server/http)  
(define format-req/c (request? . -> . string?))
(define log-format/c (symbols 'parenthesized-default 'extended 'apache-default))

(provide/contract
 [format-req/c contract?]
 [log-format/c contract?]
 [log-format->format (log-format/c . -> . format-req/c)]
 [paren-format format-req/c]
 [extended-format format-req/c]
 [apache-default-format format-req/c]
 [interface-version dispatcher-interface-version/c]
 [make (->* ()
            (#:format format-req/c
             #:log-path path-string?)
            dispatcher/c)])

(define interface-version 'v1)
(define (make #:format [format paren-format]
              #:log-path [log-path "log"])
  (define log-message (make-log-message log-path format))
  (lambda (conn req)
    (log-message req)
    (next-dispatcher)))

(define (log-format->format log-format)
  (case log-format
    [(parenthesized-default)
     paren-format]
    [(extended)
     extended-format]
    [(apache-default)
     apache-default-format]))

(define (request-line-raw req)
  (format "~a ~a HTTP/1.1"
          (string-upcase (bytes->string/utf-8 (request-method req)))
          (url->string (request-uri req))))
(define (apache-default-format req)
  (define request-time (srfi-date:current-date))
  (format "~a - - [~a] \"~a\" ~a ~a\n"
          (request-client-ip req)
          (srfi-date:date->string request-time "~d/~b/~Y:~T ~z")
          (request-line-raw req)
          "-"
          "-"))

(define (paren-format req)
  (format "~s\n"
          (list 'from (request-client-ip req)
                'to (request-host-ip req)
                'for (url->string (request-uri req)) 'at
                (date->string (seconds->date (current-seconds)) #t))))

(define (extended-format req)
  (format "~s\n"
          `((client-ip ,(request-client-ip req))
            (host-ip ,(request-host-ip req))
            (referer ,(let ([R (headers-assq* #"Referer" (request-headers/raw req))])
                        (if R
                            (header-value R)
                            #f)))                                              
            (uri ,(url->string (request-uri req)))
            (time ,(current-seconds)))))

(define (make-log-message log-path format-req)
  (define log-ch (make-async-channel))
  (define log-thread
    (thread/suspend-to-kill
     (lambda ()
       (let loop ([log-p #f])
         (sync
          (handle-evt 
           log-ch
           (match-lambda
             [(list req)
              (loop
               (with-handlers ([exn:fail? (lambda (e)
                                            ((error-display-handler) "dispatch-log.rkt Error writing log entry" e)
                                            (with-handlers ([exn:fail? (lambda (e) #f)])
                                              (close-output-port log-p))
                                            #f)])
                 (define the-log-p
                   (if (not (and log-p (file-exists? log-path)))
                       (begin
                         (unless (eq? log-p #f)
                           (close-output-port log-p))
                         (let ([new-log-p (open-output-file log-path #:exists 'append)])
                           (file-stream-buffer-mode new-log-p 'line)
                           new-log-p))
                       log-p))
                 (display (format-req req) the-log-p)
                 the-log-p))])))))))
  (lambda args
    (thread-resume log-thread (current-custodian))
    (async-channel-put log-ch args)
    (void)))
