#lang racket/base

(provide bytes-append* bytes-join)

(define (check-bytes-list l name)
  (or (and (list? l) (andmap bytes? l) l)
      (raise-argument-error name "(listof bytes?)" l)))

(define bytes-append*
  (case-lambda [(strs) (apply bytes-append (check-bytes-list strs 'bytes-append*))] ; optimize common case
               [(str . strss) (apply bytes-append (check-bytes-list (apply list* str strss) 'bytes-append*))]))

(require (only-in racket/list add-between))

(define (bytes-join strs sep)
  (cond [(not (and (list? strs) (andmap bytes? strs)))
         (raise-argument-error 'bytes-join "(listof bytes?)" strs)]
        [(not (bytes? sep))
         (raise-argument-error 'bytes-join "bytes?" sep)]
        [(null? strs) #""]
        [(null? (cdr strs)) (bytes-copy (car strs))]
        [else (apply bytes-append (add-between strs sep))]))
