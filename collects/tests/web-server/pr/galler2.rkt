#lang racket
(require net/url
         web-server/http
         web-server/http/request
         web-server/servlet-env
         rackunit)

(define (start req)
  (error "Bad"))

(define-values (pipe-read-p pipe-write-p)
  (make-pipe))

(define server-t
  (thread
   (λ ()
     (parameterize ([current-output-port pipe-write-p])
       (serve/servlet start
                      #:launch-browser? #f
                      #:quit? #f
                      #:listen-ip #f
                      #:port 0
                      #:servlet-responder
                      (λ (url exn)
                        (response/xexpr
                         "Good!"))
                      #:servlet-path "/")))))

;; Wait for server to start
(define port-embedded-line (read-line pipe-read-p))
(match-define (regexp #rx"Your Web application is running at http://localhost:([0-9]+)\\."
                      (list _ port-string))
              port-embedded-line)
(define port (string->number port-string))
(void (read-line pipe-read-p))

(check-equal? "Good!"
              (port->string (get-pure-port (string->url (format "http://localhost:~a/" port)))))
