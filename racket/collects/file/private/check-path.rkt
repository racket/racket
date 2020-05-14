#lang racket/base

(provide check-unpack-path)

(define (check-unpack-path who filename
                           #:allow-up? [allow-up? #f]
                           #:kind [kind "file"])
  (when (absolute-path? filename)
    (error who "won't extract a ~a with an absolute path\n  path: ~e" kind filename))
  (unless allow-up?
    (for ([e (in-list (explode-path filename))])
      (when (eq? e 'up)
        (error who "won't extract a ~a with an up-directory element\n  path: ~e" kind filename)))))
