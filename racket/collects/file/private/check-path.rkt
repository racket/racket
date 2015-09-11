#lang racket/base

(provide check-unpack-path)

(define (check-unpack-path who filename
                           #:allow-up? [allow-up? #f])
  (when (absolute-path? filename)
    (error who "won't extract a file with an absolute path\n  path: ~e" filename))
  (unless allow-up?
    (for ([e (in-list (explode-path filename))])
      (when (eq? e 'up)
        (error who "won't extract a file with an up-directory element\n  path: ~e" filename)))))
