#lang scheme/base

(require scheme/contract)

(provide require/doc
         provide/doc)

(define-syntax-rule (require/doc spec ...)
  (void (quote-syntax (require/doc spec ...))))

(define-syntax-rule (provide/doc [id contract desc] ...)
  (begin
    (void (quote-syntax (provide/doc [id contract desc] ...)))
    (provide/contracted [id (strip-names contract)]) ...))

(define-syntax provide/contracted
  (syntax-rules (->)
    [(_ [(rename orig-id new-id) contract])
     (provide/contract (rename orig-id new-id contract))]
    [(_ [id contract])
     (provide/contract [id contract])]))

(define-syntax strip-names
  (syntax-rules (->)
    [(_ (-> [id contract] ... result))
     (-> contract ... result)]
    [(_ other) other]))
