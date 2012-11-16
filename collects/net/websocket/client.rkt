#lang racket/base

(require racket/list
         racket/port
         racket/contract
         racket/tcp
         net/url
         web-server/http/response
         web-server/http/request
         web-server/http/request-structs
         net/websocket/conn
         net/websocket/handshake
         openssl)
(provide (except-out (all-from-out net/websocket/conn) ws-conn))

(define (wss-url? u)
  (and (url? u)
       (equal? (url-scheme u) "wss")))

(define (ws-url? u)
  (and (url? u)
       (or (equal? (url-scheme u) "ws")
           (wss-url? u))))

(provide/contract
 [ws-url? (-> any/c boolean?)]
 [wss-url? (-> any/c boolean?)]
 [ws-connect (->* (ws-url?)
                  (#:headers (listof header?))
                  open-ws-conn?)])

(define (ws-connect url
                    #:headers [headers empty])
  (define host (or (url-host url) "localhost"))
  (define port (or (url-port url) 80))
  (define upath (url-path url))
  (define the-path
    (if (empty? upath)
        "/"
        (let ([pre-path
               (add-between
                (map (λ (pp)
                       (define p (path/param-path pp))
                       (case p
                         [(up) ".."]
                         [(same) "."]
                         [else p]))
                     upath)
                "/")])
          (apply string-append
                 (if (url-path-absolute? url)
                     (list* "/"
                            pre-path)
                     pre-path)))))
  ; Connect
  (define connect (if (wss-url? url) ssl-connect tcp-connect))
  (define-values (ip op) (connect host port))
  ; Handshake (client)
  (define-values (key1 key2 key3 client-ans) (generate-key))
  (write-bytes (call-with-output-bytes
                (λ (op)
                  (fprintf op "GET ~a HTTP/1.1\r\n" the-path)
                  (print-headers
                   op
                   (list* (make-header #"Host" (string->bytes/utf-8 host))
                          (make-header #"Connection" #"Upgrade")
                          (make-header #"Upgrade" #"WebSocket")
                          (make-header #"Sec-WebSocket-Key1" (string->bytes/utf-8 key1))
                          (make-header #"Sec-WebSocket-Key2" (string->bytes/utf-8 key2))
                          headers))
                  (write-bytes key3 op)))
               op)
  (flush-output op)
  ; Handshake (server)
  (define sresponse (read-bytes-line ip 'any))
  (define rheaders (read-headers ip))
  (define server-ans (read-bytes 16 ip))
  (unless (bytes=? client-ans server-ans)
    (error 'ws-connect "Invalid server handshake response. Expected ~e, got ~e" client-ans server-ans))
  
  (ws-conn #f sresponse rheaders ip op))

(define (freadf ip s)
  (define i (read-line ip 'any))
  (unless (string=? s i)
    (error 'ws-connect "Invalid server response. Expected ~e, got ~e" s i)))
