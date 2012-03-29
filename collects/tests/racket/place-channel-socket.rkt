#lang racket/base
(require racket/match
         racket/place
         racket/tcp
         rackunit)

(module+ test
  (main))

(define (main)
  (test-case
    "places use of scheme_socket_to_ports abandons ports correctly"
    (define port-ch (make-channel))

    (define p 
      (place ch
        (match (place-channel-get ch)
          [(list in out)
            (printf "IN PLACE\n")
            (with-handlers ([exn? (lambda (e) (printf "Place Write Exception Caught ~e\n" e) (raise e))])
              (write "From Place" out)
              (flush-output out))
              ;(sleep 12)
            (tcp-abandon-port in)
            (tcp-abandon-port out)
            ])))

    (define t 
      (thread
        (lambda ()

          (define s (tcp-listen 0))
          (define-values (h1 p1 h2 p2) (tcp-addresses s #t))
          (printf "~a ~a ~a ~a\n" h1 p1 h2 p2)
          (channel-put port-ch p1)
          (define-values (in out) (tcp-accept s))
          (place-channel-put p (list in out))
          (place-wait p)
          ;(close-input-port in)
          ;(close-output-port out)
          (with-handlers ([exn? (lambda (e) (printf "Server Write Exception Caught ~e\n" e) (raise e))])
            (write "From Server" out)
            (flush-output out))
          (with-handlers ([exn? (lambda (e) (printf "Server Read Exception Caught2 ~e\n" e) (raise e))])
            (define x (read in))
            (printf "SERVER IN ~a\n" x))
          )))

    (define-values (in out) (tcp-connect "localhost" (channel-get port-ch)))
    (printf "Connected\n")
    (sleep 3)
    (with-handlers ([exn? (lambda (e) (printf "Client Write Exception Caught ~e\n" (raise e)))])
      (write "From Client" out)
      (flush-output out))
    (printf "CLIENT IN1 ~a\n" (read in))
    (printf "CLIENT IN2 ~a\n" (read in))
    (thread-wait t)
    (place-wait p)))

