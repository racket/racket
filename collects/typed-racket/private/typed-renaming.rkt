#lang racket/base

(require (for-syntax racket/base))

(provide make-typed-renaming get-alternate)

;; target : identifier
;; alternate : identifier
(define-struct typed-renaming (target alternate)
  #:property prop:rename-transformer 0)

;; identifier -> identifier
;; get the alternate field of the renaming, if it exists
(define (get-alternate id)
  (define-values (v new-id) (syntax-local-value/immediate id (lambda _ (values #f #f))))
  (cond [(typed-renaming? v)
         (typed-renaming-alternate v)]
        [(rename-transformer? v)
         (get-alternate (rename-transformer-target v))]
        [else id]))
