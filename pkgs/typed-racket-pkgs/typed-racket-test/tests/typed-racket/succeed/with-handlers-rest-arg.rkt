#lang typed/racket/base

(define (run-ground-vm)
  (with-handlers ([exn:break? void])
    (void)))
