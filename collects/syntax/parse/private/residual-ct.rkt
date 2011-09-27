#lang racket/base
(provide (struct-out attr)
         (struct-out stxclass)
         (struct-out options)
         (struct-out integrate)
         (struct-out conventions)
         (struct-out literalset)
         (struct-out eh-alternative-set)
         (struct-out eh-alternative)
         (struct-out den:delayed))

;; == from rep-attr.rkt
(define-struct attr (name depth syntax?) #:prefab)

;; == from rep-data.rkt

#|
A stxclass is
  #s(stxclass symbol (listof symbol) (list-of SAttr) identifier bool Options Integrate/#f)
where Options = #s(options boolean boolean)
      Integrate = #s(integrate id string)
Arity is defined in kws.rkt
|#
(define-struct stxclass (name arity attrs parser splicing? options integrate)
  #:prefab)

(define-struct options (commit? delimit-cut?)
  #:prefab)
(define-struct integrate (predicate description)
  #:prefab)

#|
A Conventions is
  (make-conventions id (-> (listof ConventionRule)))
A ConventionRule is (list regexp DeclEntry)
|#
(define-struct conventions (get-procedures get-rules) #:transparent)

#|
A LiteralSet is
 (make-literalset (listof (list symbol id phase-var-id)))
|#
(define-struct literalset (literals) #:transparent)

#|
An EH-alternative-set is
  (eh-alternative-set (listof EH-alternative))
An EH-alternative is
  (eh-alternative RepetitionConstraint (listof SAttr) id)
|#
(define-struct eh-alternative-set (alts))
(define-struct eh-alternative (repc attrs parser))

(define-struct den:delayed (parser class))
