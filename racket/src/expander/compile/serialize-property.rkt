#lang racket/base

;; Structures that support serialization (e.g., syntax objects)
;; implement the `prop:serialize` property, and so on.

(provide prop:serialize
         serialize?
         serialize-ref

         prop:serialize-fill!
         serialize-fill!?
         serialize-fill!-ref

         prop:reach-scopes
         reach-scopes?
         reach-scopes-ref

         prop:scope-with-bindings
         scope-with-bindings?
         scope-with-bindings-ref

         prop:binding-reach-scopes
         binding-reach-scopes?
         binding-reach-scopes-ref)

(define-values (prop:serialize serialize? serialize-ref)
  (make-struct-type-property 'serialize))

;; For values with mutable fields, so that cycles can be reconstructed
(define-values (prop:serialize-fill! serialize-fill!? serialize-fill!-ref)
  (make-struct-type-property 'serialize-fill!))

;; A property for a value that contains references to scopes, so that
;; all reachable scopes can be found
(define-values (prop:reach-scopes reach-scopes? reach-scopes-ref)
  (make-struct-type-property 'reach-scopes))

;; A property for scopes, used when detecting reachable scopes;
;; a scope has bindings that conditionally reach additional scopes
(define-values (prop:scope-with-bindings scope-with-bindings? scope-with-bindings-ref)
  (make-struct-type-property 'scope-with-bindings))

;; Like `prop:reach-scopes`, but return a single value; used for bindings:
(define-values (prop:binding-reach-scopes binding-reach-scopes? binding-reach-scopes-ref)
  (make-struct-type-property 'binding-reach-scopes))
