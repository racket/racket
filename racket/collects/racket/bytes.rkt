#lang racket/base

(provide bytes-append* bytes-join
         sequence->bytes)

(define bytes-append*
  (case-lambda [(strs) (apply bytes-append strs)] ; optimize common case
               [(str . strss) (apply bytes-append (apply list* str strss))]))

(require (only-in racket/list add-between))

(define (bytes-join strs sep)
  (cond [(not (and (list? strs) (andmap bytes? strs)))
         (raise-argument-error 'bytes-join "(listof bytes?)" strs)]
        [(not (bytes? sep))
         (raise-argument-error 'bytes-join "bytes?" sep)]
        [(null? strs) #""]
        [(null? (cdr strs)) (car strs)]
        [else (apply bytes-append (add-between strs sep))]))

(define (sequence->bytes sequence)
  (unless (sequence? sequence)
    (raise-argument-error 'sequence->bytes "(sequence/c byte?)" sequence))
  ;; This is a common enough case that we should give a better error message for it.
  (when (string? sequence)
    (raise-arguments-error
     'sequence->bytes
     "contract violation;
 strings are sequences of characters, not bytes, use string->bytes/utf-8 or a similar function instead"
     "expected" (unquoted-printing-string "(sequence/c byte?)")
     "given" sequence))
  ;; TODO(jackfirth): Is there a way to reduce the number of copies we create here?
  (cond
    [(bytes? sequence) (bytes->immutable-bytes sequence)]
    [(list? sequence) (bytes->immutable-bytes (list->bytes sequence))]
    [else (bytes->immutable-bytes (list->bytes (for/list ([element sequence]) element)))]))
