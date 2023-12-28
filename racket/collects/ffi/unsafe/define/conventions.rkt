#lang racket/base

(provide convention:hyphen->underscore
         convention:hyphen->camelCase
         convention:hyphen->PascalCase
         ;; deprecated
         convention:hyphen->camelcase)
(require (for-syntax racket/base
                     racket/symbol))

(define-for-syntax (id->string id)
  (symbol->immutable-string (syntax-e id)))

(define-for-syntax (string->id orig-id str)
  (datum->syntax orig-id (string->symbol str) orig-id))

(define-syntax (convention:hyphen->underscore id)
  (define str (id->string id))
  (string->id id (regexp-replace* #rx"-" str "_")))

(define-for-syntax (id->strings/hyphen id)
  (regexp-split #rx"-" (id->string id)))

(define-syntax (convention:hyphen->camelCase id)
  (define strs (id->strings/hyphen id))
  (string->id id (apply string-append
                        (string-downcase (car strs))
                        (map string-titlecase (cdr strs)))))

(define-syntaxes (convention:hyphen->PascalCase
                  convention:hyphen->camelcase)
  (let ()
    (define (t id)
      (define strs (id->strings/hyphen id))
      (string->id id (apply string-append
                            (map string-titlecase strs))))
    (values t t)))
