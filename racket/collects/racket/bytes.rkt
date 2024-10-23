#lang racket/base

(provide bytes-append* bytes-join)

(define (check-bytes-list* l name)
  (let loop ((l l) (p null))
    (cond ((null? (cdr l))
           (define tail (car l))
           (or (and (list? tail) (andmap bytes? tail))
               (raise-argument-error name "(listof bytes?)" tail))
           (foldl cons tail p))
          (else
           (define e (car l))
           (or (bytes? e)
               (raise-argument-error name "bytes?" e))
           (loop (cdr l) (cons e p))))))

(define bytes-append*
  (case-lambda [(strs) (apply bytes-append (check-bytes-list* (list strs) 'bytes-append*))] ; optimize common case
               [(str . strss) (apply bytes-append (check-bytes-list* (cons str strss) 'bytes-append*))]))

(require (only-in racket/list add-between))

(define (bytes-join strs sep)
  (cond [(not (and (list? strs) (andmap bytes? strs)))
         (raise-argument-error 'bytes-join "(listof bytes?)" strs)]
        [(not (bytes? sep))
         (raise-argument-error 'bytes-join "bytes?" sep)]
        [(null? strs) (bytes)]
        [(null? (cdr strs)) (bytes-copy (car strs))]
        [else (apply bytes-append (add-between strs sep))]))
