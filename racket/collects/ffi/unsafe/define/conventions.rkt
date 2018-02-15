#lang racket/base

(provide convention:hyphen->underscore
         convention:hyphen->camelcase)
(require (for-syntax racket/base
                     racket/syntax
                     racket/string))

(define-syntax (convention:hyphen->underscore id)
  (format-id id (string-replace (symbol->string (syntax-e id)) "-" "_")))

(define-syntax (convention:hyphen->camelcase id)
  (define str (symbol->string (syntax-e id)))
  (format-id id
             (apply string-append
                    (map string-titlecase (string-split str "-")))))
