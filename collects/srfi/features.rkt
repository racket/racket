#lang scheme/base

(provide feature-present? feature->require-clause)

(define *feature-alist*
  '())

(define (srfi-id? id)
  (regexp-match? #rx"^srfi-[0-9]+$" (symbol->string id)))

(define (srfi-id->filename srfi-id)
  (regexp-replace #rx"^srfi-([0-9]+)$" (symbol->string srfi-id) "\\1/\\1.rkt"))

(define (srfi-id-present? srfi-id)
  (file-exists? (build-path (collection-path "srfi")
                            (srfi-id->filename srfi-id))))

(define (feature-present? id)
  (or (and (srfi-id? id) (srfi-id-present? id))
      (and (assq id *feature-alist*) #t)))

(define (feature->require-clause id)
  (cond [(and (srfi-id? id) (srfi-id-present? id))
         (string->symbol (regexp-replace #rx"^srfi-([0-9]+)$"
                                         (symbol->string id)
                                         "srfi/\\1/\\1"))]
        [(assq id *feature-alist*) => cdr]
        [else (error 'feature->require-clause "unknown feature: ~e" id)]))
