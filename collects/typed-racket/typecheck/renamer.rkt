#lang racket/base

(provide renamer get-alternate un-rename)

;; target : identifier
;; alternate : identifier
(define-struct typed-renaming (target alternate)
  #:property prop:rename-transformer 0)

;; identifier -> identifier
;; get the alternate field of the renaming, if it exists
(define (get-alternate id)
  (define-values (v new-id) (syntax-local-value/immediate id (λ _ (values #f #f))))
  (cond [(typed-renaming? v)
         (typed-renaming-alternate v)]
        [(rename-transformer? v)
         (get-alternate (rename-transformer-target v))]
        [else id]))

(define (renamer id [alt #f])
  (if alt
      (make-typed-renaming (syntax-property id 'not-free-identifier=? #t) alt)
      (make-rename-transformer (syntax-property id 'not-free-identifier=? #t))))

;; Undo renaming for type lookup.
;; Used because of macros that mark the identifier used as the binding such as
;; kw-application or struct constructors
;;
;; The syntax-transforming check is for unit tests
(define (un-rename id)
  (if (syntax-transforming?)
      (let-values (((binding new-id) (syntax-local-value/immediate id (lambda () (values #f #f)))))
        (if (typed-renaming? binding)
            new-id
            id))
      id))
