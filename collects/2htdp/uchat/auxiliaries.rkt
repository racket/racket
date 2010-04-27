#lang scheme

(provide spawn)

(define (spawn f name)
  (thread (lambda () (f name))))
