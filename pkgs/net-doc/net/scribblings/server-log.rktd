;; This file was created by make-log-based-eval
((require net/server racket/tcp)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define (echo in out)
   (define buf (make-bytes 4096))
   (let loop ()
     (define n-read (read-bytes-avail! buf in))
     (unless (eof-object? n-read)
       (write-bytes buf out 0 n-read)
       (flush-output out)
       (loop))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define stop (start-server "127.0.0.1" 9000 echo))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-values (in out) (tcp-connect "127.0.0.1" 9000))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((displayln "hello" out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((flush-output out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((read-line in) ((3) 0 () 0 () () (c values c (u . "hello"))) #"" #"")
((close-output-port out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((close-input-port in) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((stop) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((require openssl) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define ((make-tls-echo ctx) in out)
   (define-values
    (ssl-in ssl-out)
    (ports->ssl-ports #:context ctx #:mode 'accept in out))
   (echo ssl-in ssl-out))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define server-ctx (ssl-make-server-context))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((ssl-load-certificate-chain!
  server-ctx
  (collection-file-path "test.pem" "openssl"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((ssl-load-private-key! server-ctx (collection-file-path "test.pem" "openssl"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((ssl-seal-context! server-ctx) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define stop (start-server "127.0.0.1" 9000 (make-tls-echo server-ctx)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-values (in out) (ssl-connect "127.0.0.1" 9000))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((displayln "hello" out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((flush-output out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((read-line in) ((3) 0 () 0 () () (c values c (u . "hello"))) #"" #"")
((close-output-port out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((close-input-port in) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((stop) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((require racket/unix-socket) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((struct
  listener
  (path the-wrapped-listener)
  #:property
  prop:evt
  (struct-field-index the-wrapped-listener))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define path "/tmp/server.sock")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define stop
   (start-server
    #:listen-proc
    (λ (port backlog reuse? host)
      (unless (unix-socket-path? host)
        (error 'start-server "invalid socket path: ~e" host))
      (listener host (unix-socket-listen host backlog)))
    #:accept-proc
    unix-socket-accept
    #:close-proc
    (λ (l) (delete-file (listener-path l)))
    path
    0
    (make-tls-echo server-ctx)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-values
  (in out)
  (let-values (((in out) (unix-socket-connect path)))
    (ports->ssl-ports in out)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((displayln "hello" out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((flush-output out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((read-line in) ((3) 0 () 0 () () (c values c (u . "hello"))) #"" #"")
((close-output-port out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((close-input-port in) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((stop) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define ch (make-channel)) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define stop
   (start-server
    #:listen-proc
    (λ (port backlog reuse? host) ch)
    #:accept-proc
    (λ (ports) (apply values ports))
    #:close-proc
    void
    "127.0.0.1"
    0
    echo))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-values (in out) (make-pipe))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((channel-put ch (list in out)) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((displayln "hello" out) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((read-line in) ((3) 0 () 0 () () (c values c (u . "hello"))) #"" #"")
((stop) ((3) 0 () 0 () () (c values c (void))) #"" #"")
