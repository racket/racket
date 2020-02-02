#lang racket/base
(require racket/path
         setup/collects)

(provide (all-defined-out))

(define (rkt->ss p)
  (if (path-has-extension? p #".rkt")
      (path-replace-extension p #".ss")
      p))

(define (path*->collects-relative p)
  (if (bytes? p)
      (let ([q (path->collects-relative (bytes->path p))])
        (if (path? q)
            (path->bytes q)
            q))
      (path->collects-relative p)))

(define (collects-relative*->path p cache)
  (if (bytes? p)
      (bytes->path p)
      (hash-ref! cache p (lambda () (collects-relative->path p)))))
