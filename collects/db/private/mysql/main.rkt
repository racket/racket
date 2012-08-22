#lang racket/base
(require racket/class
         racket/tcp
         openssl
         "../generic/interfaces.rkt"
         "../generic/common.rkt"
         "../generic/socket.rkt"
         "connection.rkt")
(provide mysql-connect
         mysql-guess-socket-path
         mysql-password-hash)

(define (mysql-connect #:user user
                       #:database [database #f]
                       #:password [password #f]
                       #:server [server #f]
                       #:port [port #f]
                       #:socket [socket #f]
                       #:ssl [ssl 'no]
                       #:ssl-context [ssl-context
                                      (case ssl
                                        ((no) #f)
                                        (else (ssl-make-client-context 'tls)))]
                       #:notice-handler [notice-handler void]
                       #:debug? [debug? #f])
  (let ([connection-options
         (+ (if (or server port) 1 0)
            (if socket 1 0))])
    (when (> connection-options 1)
      (error 'mysql-connect "cannot give both server/port and socket arguments")))
  (let* ([notice-handler
          (cond [(procedure? notice-handler) notice-handler]
                [else (make-print-notice notice-handler)])]
         [c (new connection% (notice-handler notice-handler))])
    (when debug? (send c debug #t))
    (cond [socket
           (let ([socket (if (eq? socket 'guess)
                             (mysql-guess-socket-path)
                             socket)])
             (let-values ([(in out) (unix-socket-connect socket)])
               (send c attach-to-ports in out)))]
          [else
           (let ([server (or server "localhost")]
                 [port (or port 3306)])
             (let-values ([(in out) (tcp-connect server port)])
               (send c attach-to-ports in out)))])
    (send c start-connection-protocol database user password ssl ssl-context)
    c))

;; make-print-notification : output-port -> number string -> void
(define ((make-print-notice out) code condition)
  (fprintf (case out
             ((output) (current-output-port))
             ((error) (current-error-port))
             (else out))
           "notice: ~a (MySQL code ~a)\n" condition code))

(define socket-paths
  (case (system-type)
    ((unix) '("/var/run/mysqld/mysqld.sock"))
    (else '())))

(define (mysql-guess-socket-path)
  (guess-socket-path/paths 'mysql-guess-socket-path socket-paths))
