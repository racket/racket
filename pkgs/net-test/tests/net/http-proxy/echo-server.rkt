#lang racket/base
(provide server current-conn-timeout)

(require racket/port "generic-server.rkt")

(define (server)
  ;; Although this is ≡ (serve copy-port), I’m explicit about i and o
  ;; to illustrate the calling convention for serve
  (serve (lambda (i o) (copy-port i o))))

(module+
  main
  (define-values (the-port server-thread shutdown-server) (server))
  (dynamic-wind
   void
   (λ () (thread-wait server-thread))
   shutdown-server))

(module+
    test
  (require rackunit racket/tcp)
  (define-values (the-port server-thread shutdown-server) (server))  
  (dynamic-wind
   void
   (λ ()
     (define-values (cl:from cl:to)
       (tcp-connect "localhost" the-port))
     (file-stream-buffer-mode cl:to 'none)
     (file-stream-buffer-mode cl:from 'none)
     (fprintf cl:to "Monkeys!")
     (flush-output cl:to)
     (close-output-port cl:to)
     (check-equal? (read-string 1024 cl:from) "Monkeys!")
     (tcp-abandon-port cl:from))
   shutdown-server))
