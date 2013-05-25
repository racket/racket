#lang scheme/base
(require (for-syntax scheme/base
                     syntax/boundmap))

(provide (for-syntax add-no-set!-identifiers)
         r6rs:set!)

;; Provided identifier cannot be `set!'ed. The list
;; is relevant only within the module being compiled.
(define-for-syntax no-set!-identifiers (make-free-identifier-mapping))

(define-for-syntax (add-no-set!-identifiers ids)
  (for ([id (in-list ids)])
    (free-identifier-mapping-put! no-set!-identifiers id #t)))

(define-for-syntax (no-set!-identifier? id)
  (free-identifier-mapping-get no-set!-identifiers id (lambda () #f)))

;; ----------------------------------------

(define-syntax (r6rs:set! stx)
  (syntax-case stx ()
    [(_ id rhs)
     (identifier? #'id)
     (if (no-set!-identifier? #'id)
         (raise-syntax-error
          #f
          "cannot mutate exported identifier"
          stx
          #'id)
         (syntax/loc stx (set! id rhs)))]
    [(_ . rest)
     (syntax/loc stx (set! . rest))]))
