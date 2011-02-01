#lang racket/base

(provide bytes-append* bytes-join)

(define bytes-append*
  (case-lambda [(strs) (apply bytes-append strs)] ; optimize common case
               [(str . strss) (apply bytes-append (apply list* str strss))]))

(require (only-in scheme/list add-between))

(define (bytes-join strs sep)
  (cond [(not (and (list? strs) (andmap bytes? strs)))
         (raise-type-error 'bytes-join "list-of-byte-strings" strs)]
        [(not (bytes? sep))
         (raise-type-error 'bytes-join "bytes" sep)]
        [(null? strs) #""]
        [(null? (cdr strs)) (car strs)]
        [else (apply bytes-append (add-between strs sep))]))
