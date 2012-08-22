#lang racket/base
(require racket/class
         racket/tcp
         openssl
         "../generic/interfaces.rkt"
         "../generic/common.rkt"
         "../generic/socket.rkt"
         "connection.rkt")
(provide postgresql-connect
         postgresql-guess-socket-path
         postgresql-password-hash)

(define (postgresql-connect #:user user
                            #:database database
                            #:password [password #f]
                            #:server [server #f]
                            #:port [port #f]
                            #:socket [socket #f]
                            #:allow-cleartext-password? [allow-cleartext-password? #f]
                            #:ssl [ssl 'no]
                            #:ssl-context [ssl-context
                                           (case ssl
                                             ((no) #f)
                                             (else (ssl-make-client-context 'sslv3)))]
                            #:notice-handler [notice-handler void]
                            #:notification-handler [notification-handler void]
                            #:debug? [debug? #f])
  (let ([connection-options
         (+ (if (or server port) 1 0)
            (if socket 1 0))]
        [notice-handler (make-handler notice-handler "notice")]
        [notification-handler
         (if (procedure? notification-handler)
             notification-handler
             (make-print-notification notification-handler))]
        [socket
         (if (eq? socket 'guess)
             (postgresql-guess-socket-path)
             socket)])
    (when (> connection-options 1)
      (error 'postgresql-connect "cannot give both server/port and socket arguments"))
    (let ([c (new connection%
                  (notice-handler notice-handler)
                  (notification-handler notification-handler)
                  (allow-cleartext-password? allow-cleartext-password?))])
      (when debug? (send c debug #t))
      (let-values ([(in out)
                    (cond [socket (unix-socket-connect socket)]
                          [else (let ([server (or server "localhost")]
                                      [port (or port 5432)])
                                  (tcp-connect server port))])])
        (send c attach-to-ports in out ssl ssl-context)
        (send c start-connection-protocol database user password)
        c))))

(define socket-paths
  (case (system-type)
    ((unix) '("/var/run/postgresql/.s.PGSQL.5432"))
    (else '())))

(define (postgresql-guess-socket-path)
  (guess-socket-path/paths 'postgresql-guess-socket-path socket-paths))

;; make-print-notification : output-port -> string -> void
(define ((make-print-notification out) condition)
  (fprintf (case out
             ((output) (current-output-port))
             ((error) (current-error-port))
             (else out))
           "notification: ~a\n" condition))

(define (postgresql-password-hash user password)
  (bytes->string/latin-1 (password-hash user password)))
