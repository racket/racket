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
         (get-alternate new-id)]
        [else id]))

;; Undo renaming for type lookup. Returns the original id.
;; Used because some macros use the binding of the identifier in macro position,
;; such as struct constructors, and we need to track this to the original binding.
;;
;; The syntax-transforming check is for unit tests.
(define (un-rename id)
  (if (syntax-transforming?)
      (let-values (((binding new-id) (syntax-local-value/immediate id (lambda () (values #f #f)))))
        (cond [(typed-renaming? binding)
               new-id]
              [(rename-transformer? binding)
               (un-rename new-id)]
              [else id]))
      id))


(define (renamer id [alt #f])
  (if alt
      (make-typed-renaming (syntax-property id 'not-free-identifier=? #t) alt)
      (make-rename-transformer (syntax-property id 'not-free-identifier=? #t))))
