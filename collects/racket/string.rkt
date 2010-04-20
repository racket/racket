#lang scheme/base

(provide string-append* string-join)

(define string-append*
  (case-lambda [(strs) (apply string-append strs)] ; optimize common case
               [(str . strss) (apply string-append (apply list* str strss))]))

(require (only-in scheme/list add-between))

(define (string-join strs sep)
  (cond [(not (and (list? strs) (andmap string? strs)))
         (raise-type-error 'string-join "list-of-strings" strs)]
        [(not (string? sep))
         (raise-type-error 'string-join "string" sep)]
        [(null? strs) ""]
        [(null? (cdr strs)) (car strs)]
        [else (apply string-append (add-between strs sep))]))
