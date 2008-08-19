#lang scheme

(define-struct client (fname surname email))
(provide (struct-out client))