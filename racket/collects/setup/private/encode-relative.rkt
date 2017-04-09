#lang racket/base

;; Similar to "../path-relativize.rkt", but works on already-releative
;; paths and allows ".." in the path.

(provide encode-relative-path
         decode-relative-path)

(define (encode-relative-path p)
  (cons 'rel
        (for/list ([e (in-list (explode-path p))])
          (if (path? e)
              (path-element->bytes e)
              e))))

(define (decode-relative-path l)
  (apply build-path
         (for/list ([e (in-list (cdr l))])
           (if (bytes? e)
               (bytes->path-element e)
               e))))
