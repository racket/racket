#lang racket/base
; An echo server -- ripped off the racket homepage
(provide server current-listen-port)

(require racket/port "generic-server.rkt")

(define (server)
  (serve (lambda (i o) (copy-port i o))))

(module+
  main
  (define-values (server-thread shutdown-server) (server))
  (thread-wait server-thread))

(module+
  test
 (require rackunit racket/tcp)
 (define-values (server-thread shutdown-server) (server))

 (define-values (cl:from cl:to)
   (tcp-connect "localhost" (current-listen-port)))
 (file-stream-buffer-mode cl:to 'none)
 (file-stream-buffer-mode cl:from 'none)
 (fprintf cl:to "Monkeys!")
 (flush-output cl:to)
 (close-output-port cl:to)
 (check-equal? (read-string 1024 cl:from) "Monkeys!")
 (tcp-abandon-port cl:from)
 (sleep 5)
 (shutdown-server))
