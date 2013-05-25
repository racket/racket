#lang racket/base

(require racket/list
         racket/unit
         racket/contract
         unstable/contract
         web-server/private/dispatch-server-unit
         web-server/private/dispatch-server-sig
         web-server/private/connection-manager
         web-server/http/response
         web-server/http/request
         web-server/http/request-structs
         racket/async-channel
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         net/websocket/conn
         net/websocket/handshake)
(provide (except-out (all-from-out net/websocket/conn) ws-conn))

(provide/contract
 [ws-serve
  (->* ((open-ws-conn? any/c . -> . void))
       (#:conn-headers 
        (bytes? (listof header?) . -> . (values (listof header?) any/c))
        #:tcp@
        (unit/c (import) (export tcp^))
        #:port
        tcp-listen-port?
        #:listen-ip
        (or/c string? false/c)
        #:max-waiting
        integer?
        #:timeout
        integer?
        #:confirmation-channel
        (or/c false/c async-channel?))
       (-> void))])

(define (ws-serve conn-dispatch
                  #:conn-headers [pre-conn-dispatch (λ (cline hs) (values empty (void)))]
                  #:tcp@ [tcp@ raw:tcp@]
                  #:port [port 80]
                  #:listen-ip [listen-ip #f]
                  #:max-waiting [max-waiting 4]
                  #:timeout [initial-connection-timeout (* 60 60)]
                  #:confirmation-channel [confirm-ch #f])
  (define (read-request c p port-addresses)
    (values #f #t))
  (define (dispatch c _)
    (define ip (connection-i-port c))
    (define op (connection-o-port c))
    (define cline (read-bytes-line ip 'any))
    (define headers (read-headers ip))
    (define key1h (headers-assq* #"Sec-WebSocket-Key1" headers))
    (unless key1h (error 'ws-serve "Invalid WebSocket request, no Key1"))
    (define key1 (header-value key1h))
    (define key2h (headers-assq* #"Sec-WebSocket-Key2" headers))
    (unless key2h (error 'ws-serve "Invalid WebSocket request, no Key2"))
    (define key2 (header-value key2h))
    (define key3 (read-bytes 8 ip))
    
    (define-values (conn-headers state) (pre-conn-dispatch cline headers))
    
    (fprintf op "HTTP/1.1 101 WebSocket Protocol Handshake\r\n")
    (print-headers 
     op
     (list* (make-header #"Upgrade" #"WebSocket")
            (make-header #"Connection" #"Upgrade")
            conn-headers))
    
    (write-bytes
     (handshake-solution (bytes->string/utf-8 key1)
                         (bytes->string/utf-8 key2)
                         key3)
     op)
    (flush-output op)
    
    (define conn
      (ws-conn #f cline conn-headers ip op))
    
    (conn-dispatch conn state))
  
  (define-unit-binding a-tcp@
    tcp@ (import) (export tcp^))
  (define-compound-unit/infer dispatch-server@/tcp@
    (import dispatch-server-config^)
    (link a-tcp@ dispatch-server@)
    (export dispatch-server^))
  (define-values/invoke-unit
    dispatch-server@/tcp@
    (import dispatch-server-config^)
    (export dispatch-server^))
  (serve #:confirmation-channel confirm-ch))
