#lang racket
(require net/websocket
         web-server/http
         racket/runtime-path
         web-server/templates
         web-server/servlet-env)

(framing-mode 'old)

(define stop-ws!
  (ws-serve (λ (wsc _)
              (let loop ()
                (define m (ws-recv wsc))
                (printf "~a\n" m)
                (unless (eof-object? m)
                  (ws-send! wsc m)
                  (loop))))
            #:conn-headers 
            (λ (_ hs)
              (define origin (header-value (headers-assq* #"Origin" hs)))
              (values (list (make-header #"Sec-WebSocket-Origin" origin)
                            (make-header #"Sec-WebSocket-Location" #"ws://localhost:8080/"))
                      #f))
            #:port 8080))

(define-runtime-path example-pth ".")

(serve/servlet (λ (req)
                 (response/full
                  200 #"Okay"
                  (current-seconds) TEXT/HTML-MIME-TYPE
                  empty
                  (list (string->bytes/utf-8 (include-template "index.html")))))
               #:servlet-path "/"
               #:port 8081
               #:extra-files-paths (list example-pth))
