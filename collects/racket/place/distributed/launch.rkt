#lang racket/base
(require racket/match
         racket/tcp
         racket/place/distributed
         racket/place/private/coercion)

(provide main)

(define (main . args)
  (match args
    [(list "spawn" node-port)
       (define listener (tcp-listen (->number node-port) 4 #t))
       (printf/f "~a\n" (list (->number node-port)))
       (start-spawned-node-router listener)]

    ;; Used to launch Design Pattern 1, MPI style distributed system.
    [(list "launch" mod-path conf-name i)
       (startup-config (dynamic-require (->path mod-path) (string->symbol conf-name)) (->number i))]))

