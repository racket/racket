#lang racket/base

(define (check f)
  (with-handlers ([exn:fail? (λ (x) x)])
    (with-input-from-file f
      (λ ()
        (for ([e (in-port)])
          (void))))
    #f))

(for ([f (in-directory)]
      #:when (file-exists? f)
      #:when (regexp-match #rx"timing$" f))
  (define c (check f))
  (printf "~a\n" f)
  (when c
    (printf "\t~a\n" c)
    (exit 1)))
