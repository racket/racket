#lang scheme/base

(provide string-append* string-join)

(define string-append*
  (case-lambda [(strs) (apply string-append strs)] ; optimize common case
               [(s1 strs) (apply string-append s1 strs)]
               [(s1 s2 strs) (apply string-append s1 s2 strs)]
               [(s1 s2 s3 strs) (apply string-append s1 s2 s3 strs)]
               [(s1 s2 s3 s4 strs) (apply string-append s1 s2 s3 s4 strs)]
               [(str . strss) (apply apply string-append str strss)]))

(require (only-in scheme/list add-between))

(define (string-join strs sep)
  (cond [(not (and (list? strs) (andmap string? strs)))
         (raise-type-error 'string-join "list-of-strings" strs)]
        [(not (string? sep))
         (raise-type-error 'string-join "string" sep)]
        [(null? strs) ""]
        [(null? (cdr strs)) (car strs)]
        [else (apply string-append (add-between strs sep))]))
