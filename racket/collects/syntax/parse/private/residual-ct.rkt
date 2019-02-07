#lang racket/base
(provide (struct-out attr)
         (struct-out stxclass)
         (struct-out scopts)
         (struct-out conventions)
         (struct-out literalset)
         (struct-out lse:lit)
         (struct-out lse:datum-lit)
         (struct-out eh-alternative-set)
         (struct-out eh-alternative)
         (struct-out den:lit)
         (struct-out den:datum-lit)
         (struct-out den:delayed)
         prop:syntax-class
         has-stxclass-prop?
         stxclass-prop-ref
         alt-stxclass-mapping
         log-syntax-parse-error
         log-syntax-parse-warning
         log-syntax-parse-info
         log-syntax-parse-debug
         prop:pattern-expander
         pattern-expander?
         pattern-expander-proc
         syntax-local-syntax-parse-pattern-introduce)

(define-logger syntax-parse)

;; == from rep-attr.rkt
(define-struct attr (name depth syntax?) #:prefab)

;; == from rep-data.rkt

;; A stxclass is #s(stxclass Symbol Arity SAttrs Id Bool scopts Id/#f)
(define-struct stxclass
  (name         ;; Symbol
   arity        ;; Arity (defined in kws.rkt)
   attrs        ;; (Listof SAttr)
   parser       ;; Id, reference to parser (see parse.rkt for parser signature)
   splicing?    ;; Bool
   opts         ;; scopts
   inline       ;; Id/#f, reference to a predicate
   ) #:prefab)

(define-values [prop:syntax-class has-stxclass-prop? stxclass-prop-ref]
  (make-struct-type-property 'syntax-class))

;; alt-stxclass-mapping : (boxof (listof (pair Identifier Stxclass)))
;; Maps existing bindings (can't use syntax-local-value mechanism) to stxclasses.
;; Uses alist to avoid residual dependence on syntax/id-table.
(define alt-stxclass-mapping (box null))

;; A scopts is #s(scopts Nat Bool Bool String/#f)
;; These are passed on to var patterns.
(define-struct scopts
  (attr-count   ;; Nat
   commit?      ;; Bool
   delimit-cut? ;; Bool
   desc         ;; String/#f, String = known constant description
   ) #:prefab)

#|
A Conventions is
  (make-conventions id (-> (listof ConventionRule)))
A ConventionRule is (list regexp DeclEntry)
|#
(define-struct conventions (get-procedures get-rules) #:transparent)

#|
A LiteralSet is
 (make-literalset (listof LiteralSetEntry))
An LiteralSetEntry is one of
 - (make-lse:lit Symbol Id Stx)
 - (make-lse:datum-lit Symbol Symbol)
|#
(define-struct literalset (literals) #:transparent)
(define-struct lse:lit (internal external phase) #:transparent)
(define-struct lse:datum-lit (internal external) #:transparent)

#|
An EH-alternative-set is
  (eh-alternative-set (listof EH-alternative))
An EH-alternative is
  (eh-alternative RepetitionConstraint (listof SAttr) id)
|#
(define-struct eh-alternative-set (alts))
(define-struct eh-alternative (repc attrs parser))

(define-struct den:lit (internal external input-phase lit-phase) #:transparent)
(define-struct den:datum-lit (internal external) #:transparent)
(define-struct den:delayed (parser class))

;; == Pattern expanders

(define-values (prop:pattern-expander pattern-expander? get-proc-getter)
  (make-struct-type-property 'pattern-expander))

(define (pattern-expander-proc pat-expander)
  (define get-proc (get-proc-getter pat-expander))
  (get-proc pat-expander))

(define (syntax-local-syntax-parse-pattern-introduce stx)
  (syntax-local-introduce stx))
