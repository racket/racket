#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/error.rkt")

(provide make-check-no-duplicate-table
         check-no-duplicate-ids)

(define (make-check-no-duplicate-table) #hasheq())

;; Check for duplicates, returning a table on success that can be
;; used for further checking.
;; The `ids` argument can be a single identifier, a list, a list of
;; lists, etc.
(define (check-no-duplicate-ids ids phase s [ht (make-check-no-duplicate-table)]
                                #:what [what "binding name"])
  (let loop ([v ids] [ht ht])
    (cond
     [(identifier? v)
      (define l (hash-ref ht (syntax-e v) null))
      (for ([id (in-list l)])
        (when (bound-identifier=? id v phase)
          (raise-syntax-error #f (string-append "duplicate " what) s v)))
      (hash-set ht (syntax-e v) (cons v l))]
     [(pair? v)
      (loop (cdr v) (loop (car v) ht))]
     [else
      ht])))
