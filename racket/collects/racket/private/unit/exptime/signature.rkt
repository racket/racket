#lang racket/base

;; This module defines the structure types used to represent signatures
;; at compile time, plus some functions on those types.

(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/parse/private/pattern-expander)
         racket/base
         racket/syntax
         syntax/parse/pre
         "util.rkt"
         (for-template racket/base
                       "../keywords.rkt"))

(provide (rename-out [build-siginfo make-siginfo])
         siginfo-names
         siginfo-ctime-ids
         siginfo-rtime-ids
         ~bind-siginfo
         siginfo-subtype

         siginfo->key-expr
         siginfo->key-exprs
         ~bind-keys

         (struct-out signature)
         make-signature-member-introducer
         lookup-signature
         ~bind-signature
         signature-id
         tagged-signature-id
         opt-init-depends)

;; -----------------------------------------------------------------------------
;; siginfo

;; A siginfo contains information about the identity of a signature
;; and its super-signatures. Each of the three list fields are always
;; non-empty and the same length; the first element of each list
;; corresponds to the child signature, and each subsequent element
;; corresponds to the next super-signature in the inheritance chain.
(define-struct siginfo
  (names         ; (listof identifier?) - the identifiers bound by `define-signature`
   ctime-ids     ; (listof symbol?) - gensyms that uniquely identify the signature
                 ;   in the transformer environment
   rtime-ids     ; (listof identifier?) - identifiers bound to a gensym that
                 ;   uniquely identifies the signature at runtime; see
                 ;   Note [Signature runtime representation] in "../runtime.rkt"
   super-table)) ; (hash/c symbol? #t) - a hash that maps the elements of ctime-ids,
                 ;   to #t, used for efficient subtyping checks

;; build-siginfo : (listof symbol) (listof symbol) (listof identifier) -> siginfo
(define (build-siginfo names rtime-ids)
  (define ctime-ids 
    (cons (gensym)
          (if (null? (cdr names))
              null
              (siginfo-ctime-ids 
               (signature-siginfo
                (lookup-signature (cadr names)))))))
  (make-siginfo names
                ctime-ids
                rtime-ids 
                (make-immutable-hasheq (map (λ (x) `(,x . #t)) ctime-ids))))

;; Helper for bulk-binding attributes for siginfo values.
(define-syntax ~bind-siginfo
  (pattern-expander
   (syntax-parser
     [(_ x:attr-decl e:expr)
      #'{~bind/nested
         [x e] tmp
         ([{id 1} (siginfo-names tmp)]
          [self-id (car (siginfo-names tmp))]
          [{super-id 1} (cdr (siginfo-names tmp))]
          [{ctime-id 1} (siginfo-ctime-ids tmp)]
          [{rtime-id 1} (siginfo-rtime-ids tmp)])}])))

;; siginfo-subtype : siginfo siginfo -> bool
(define (siginfo-subtype s1 s2)
  (hash-ref (siginfo-super-table s1)
            (car (siginfo-ctime-ids s2))
            (λ () #f)))

(define (build-key-expr tag rtime-id)
  (if tag
      #`(cons '#,tag #,rtime-id)
      rtime-id))

;; siginfo->key-expr : siginfo? (or/c symbol? #f) -> syntax?
;; Builds an expression that evaluates to this signature’s runtime key;
;; see Note [Signature runtime representation] in "../runtime.rkt".
(define (siginfo->key-expr info tag)
  (build-key-expr tag (car (siginfo-rtime-ids info))))

;; siginfo->key-exprs : siginfo? (or/c symbol? #f) -> (listof syntax?)
;; Builds a list of expressions that evaluate to runtime keys for this
;; signature and each of its super-signatures; see Note [Signature
;; runtime representation] in "../runtime.rkt".
(define (siginfo->key-exprs info tag)
  (map (λ (id) (build-key-expr tag id)) (siginfo-rtime-ids info)))

;; Helper for bulk-binding attributes for signature key expressions.
(define-syntax ~bind-keys
  (pattern-expander
   (syntax-parser
     [(_ x:attr-decl info-e:expr tag-e:expr)
      #'{~bind/nested
         #:only-nested
         [x (attributes-map siginfo->key-exprs 'x.depth info-e tag-e)]
         keys
         ([{key 1} keys]
          [self-key (car keys)]
          [{super-key 1} (cdr keys)])}])))

;; -----------------------------------------------------------------------------
;; signature

;; The compile-time value of a signature binding.
;; Note that a slightly modified variant of this structure is
;; sometimes used when processing imports and exports in a unit body, see
;; Note [Parsed signature imports and exports] in "import-export.rkt" for details.
(define-struct signature
  (siginfo       ; siginfo?
   vars          ; (listof identifier?)
   val-defs      ; (listof (cons/c (listof identifier?) syntax?))
   stx-defs      ; (listof (cons/c (listof identifier?) syntax?))
   post-val-defs ; (listof (cons/c (listof identifier?) syntax?))
   ctcs)         ; (listof (or/c syntax? #f))
  #:property prop:procedure
  (lambda (_ stx)
    (parameterize ((current-syntax-context stx))
      (raise-stx-err "illegal use of signature name"))))

(define (make-signature-member-introducer sig ref-stx)
  (make-relative-introducer ref-stx (car (siginfo-names (signature-siginfo sig)))))

#| Note [Generated export definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `define-values-for-export` form is used to generate a definition
in any unit that exports the signature. While the idea is conceptually
a natural dual to `define-values` in a signature (which synthesizes
definitions in units that *import* the signature), there are several
ways in which they work somewhat differently.

For one, identifiers bound by export definitions do not conflict with
other members of the signature. For example, this is entirely legal:

  (define-signature sig^
    [some-value
     (define-values-for-export [some-value] 'something)])

There is no conflict because, of course, an `export` declaration
introduces *uses* of the signature members, not bindings for them. The
generated export supplies the value that is exported by the unit.

It is not currently possible for a user to suppress the generation of
these definitions when a signature is exported (though we sometimes do
it internally; see Note [Suppress generated definitions on reexport]).
It is also not possible for a user to rename them directly, but we
*do* implicitly rename them if a user renames an export with the same
name. For example, given the above definition of `sig^`, the unit

  (unit (import)
        (export (rename sig^ [another-value some-value]))
        another-value)

is accepted without error. Essentially, we treat these “exported member +
generated definition pairs” as permanently linked. |#

;; lookup-signature : syntax-object -> signature
(define (lookup-signature id)
  (let ((s (lookup id "unknown signature")))
    (unless (signature? s)
      (raise-stx-err "not a signature" id))
    s))

;; Helpers for bulk-binding attributes for signature values.
(define-syntax ~bind-signature
  (pattern-expander
   (syntax-parser
     [(_ x:attr-decl e:expr)
      #`{~and
         {~do (define tmp e)}
         {~bind/nested
          [x tmp] tmp
          ([{var-id 1} (signature-vars tmp)]
           [{val-def.id 2} (map car (signature-val-defs tmp))]
           [{val-def.rhs 1} (map cdr (signature-val-defs tmp))]
           [{stx-def.id 2} (map car (signature-stx-defs tmp))]
           [{stx-def.rhs 1} (map cdr (signature-stx-defs tmp))]
           [{post-def.id 2} (map car (signature-post-val-defs tmp))]
           [{post-def.rhs 1} (map cdr (signature-post-val-defs tmp))]
           [{ctc 1} (signature-ctcs tmp)])}
         {~bind-siginfo {#,(dotted-id #'x.id #'info) x.depth}
                        (attribute-map signature-siginfo 'x.depth tmp)}}])))

(define-syntax-class signature-id
  #:description #f
  #:attributes [value info
                {info.id 1} info.self-id {info.super-id 1}
                {info.ctime-id 1}
                {info.rtime-id 1}
                {var-id 1}
                {val-def.id 2} {val-def.rhs 1}
                {stx-def.id 2} {stx-def.rhs 1}
                {post-def.id 2} {post-def.rhs 1}
                {ctc 1}]
  #:commit
  (pattern {~var || (static/extract signature? "identifier bound to a signature")}
    #:and {~bind-signature || (attribute value)}))

(define-syntax-class tagged-signature-id
  #:description "tagged signature identifier"
  #:attributes [tag-id tag-sym sig-id value info
                {info.id 1} info.self-id {info.super-id 1}
                {info.ctime-id 1}
                {info.rtime-id 1}
                {var-id 1}
                {val-def.id 2} {val-def.rhs 1}
                {stx-def.id 2} {stx-def.rhs 1}
                {post-def.id 2} {post-def.rhs 1}
                {ctc 1}
                {key 1} self-key {super-key 1}]
  #:commit
  #:literals [tag]
  (pattern (tag ~! tag-id:id {~and sig-id :signature-id})
    #:attr tag-sym (syntax-e #'tag-id)
    #:and {~bind-keys || (attribute info) (attribute tag-sym)})
  (pattern {~and sig-id :signature-id}
    #:attr tag-id #f
    #:attr tag-sym #f
    #:and {~bind-keys || (attribute info) (attribute tag-sym)}))

(define-splicing-syntax-class opt-init-depends
  #:description #f
  #:attributes [{dep 1} {tag-id 1} {tag-sym 1} {sig-id 1} {value 1} {info 1}
                {info.id 2} {info.self-id 1} {info.super-id 2}
                {info.ctime-id 2}
                {info.rtime-id 2}
                {var-id 2}
                {val-def.id 3} {val-def.rhs 2}
                {stx-def.id 3} {stx-def.rhs 2}
                {post-def.id 3} {post-def.rhs 2}
                {ctc 2}
                {key 2} {self-key 1} {super-key 2}]
  #:commit
  #:literals [init-depend]
  (pattern (init-depend ~! {~and dep :tagged-signature-id} ...))
  ;; Handling the optionality this way rather than wrapping the
  ;; pattern with ~optional avoids having to deal with #f attributes.
  (pattern {~seq} #:with [{~and dep :tagged-signature-id} ...] #'[]))
