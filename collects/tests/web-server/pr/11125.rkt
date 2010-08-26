#lang racket
(require net/url
         web-server/http
         web-server/http/request
         web-server/servlet-env
         tests/eli-tester)

(define PORT 8999)

(define (do-the-test PORT #:connection-close? connection-close?)
  (define tc (make-thread-cell 0))
  
  (define (start req)
    (thread-cell-set! tc (add1 (thread-cell-ref tc)))
    (number->string (thread-cell-ref tc)))
  
  (define-values (pipe-read-p pipe-write-p)
    (make-pipe))
  
  (define server-t
    (thread
     (λ ()
       (parameterize ([current-output-port pipe-write-p])
         (serve/servlet start
                        #:launch-browser? #f
                        #:connection-close? connection-close?
                        #:quit? #f
                        #:listen-ip #f
                        #:port PORT
                        #:servlet-path "/")))))
  
  ; Wait for server to start
  (void (read-line pipe-read-p)
        (read-line pipe-read-p))
  
  (define-values (http-read-p http-write-p)
    (tcp-connect "localhost" PORT))
  
  (define (get-tc/err http-read-p http-write-p)
    (with-handlers
        ([exn?
          (λ (x)
            (define-values (new-http-read-p new-http-write-p)
              (tcp-connect "localhost" PORT))
            (set! http-read-p new-http-read-p)
            (set! http-write-p new-http-write-p)
            (get-tc http-read-p http-write-p))])
      (get-tc http-read-p http-write-p)))
  
  (define (get-tc http-read-p http-write-p)
    (fprintf http-write-p "GET / HTTP/1.1\r\n\r\n")
    (flush-output http-write-p)
    
    (read-line http-read-p)
    (read-headers http-read-p)
    (string->number (string (read-char http-read-p))))
  
  (begin0
    (list (get-tc/err http-read-p http-write-p)
          (get-tc/err http-read-p http-write-p))
    
    (kill-thread server-t)))

(test
 (do-the-test PORT #:connection-close? #f) => (list 1 2)
 (do-the-test (add1 PORT) #:connection-close? #t) => (list 1 1))
