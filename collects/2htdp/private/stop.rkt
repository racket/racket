#lang scheme

(provide (struct-out stop-the-world))

(define-struct stop-the-world (world) #:transparent)
