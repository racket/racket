#lang racket/base

(provide bytes-append* bytes-join bytes->number)

(define bytes-append*
  (case-lambda [(strs) (apply bytes-append strs)] ; optimize common case
               [(str . strss) (apply bytes-append (apply list* str strss))]))

(require (only-in racket/list add-between)
         (only-in file/sha1 bytes->hex-string))

(define (bytes-join strs sep)
  (cond [(not (and (list? strs) (andmap bytes? strs)))
         (raise-argument-error 'bytes-join "(listof bytes?)" strs)]
        [(not (bytes? sep))
         (raise-argument-error 'bytes-join "bytes?" sep)]
        [(null? strs) #""]
        [(null? (cdr strs)) (car strs)]
        [else (apply bytes-append (add-between strs sep))]))

(define bytes->number (compose (lambda (s) (string->number s 16)) bytes->hex-string))