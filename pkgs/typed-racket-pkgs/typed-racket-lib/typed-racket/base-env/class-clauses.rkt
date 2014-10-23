#lang racket/base

;; This module provides helper syntax classes and macros that are used
;; to implement the typed class macro. It's separated in order to allow
;; other parts of TR to use the bindings of init, public, etc. without
;; requiring prims.rkt

(require (prefix-in untyped: racket/class)
         "colon.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "../private/syntax-properties.rkt"))

(provide (for-syntax class-clause
                     clause
                     clause?
                     clause-stx
                     clause-kind
                     clause-ids
                     init-clause
                     init-clause?
                     init-clause-optional?)
         init
         init-field
         field
         inherit-field
         init-rest
         public
         pubment
         override
         augment
         private
         inherit)

;; for tests
(module+ internal (provide (for-syntax init-decl)))

(begin-for-syntax
 ;; A Clause is a (clause Syntax Id Listof<Syntax> Listof<Option<Type>>)
 ;;
 ;; interp. a class clause such as init or field.
 ;;   stx   - the syntax of the entire clause with types erased
 ;;   kind  - the kind of clause (e.g., init, field)
 ;;   ids   - list of the ids defined in this clause
 ;;   types - types for each id, #f if a type is not provided
 (struct clause (stx kind ids types) #:transparent)

 ;; An InitClause is a (init-clause Syntax Id Listof<Syntax> Boolean)
 ;;
 ;; interp. an init class clause
 (struct init-clause clause (optional?) #:transparent)

 (define-literal-set class-literals
   (:))

 ;; interp:
 ;;   optional? - optional init arg or not (has default value or not)
 ;;   ids       - internal and external id for this argument
 ;;   type      - type annotation, if any
 ;;   form      - type erased form
 (define-syntax-class init-decl
   #:attributes (optional? ids type form)
   #:literal-sets (class-literals)
   (pattern id:id
            #:attr optional? #f
            #:with ids #'(id id)
            #:attr type #f
            #:with form this-syntax)
   (pattern (id:id : type:expr)
            #:attr optional? #f
            #:with ids #'(id id)
            #:with form #'id)
   (pattern (ren:renamed (~optional (~seq : type:expr)))
            #:attr optional? #f
            #:with ids #'ren.ids
            #:with form #'(ren))
   (pattern (mren:maybe-renamed
             (~optional (~seq : type:expr))
             default-value:expr)
            #:attr optional? #t
            #:with ids #'mren.ids
            #:with form #'(mren default-value)))

 (define-syntax-class field-decl
   #:attributes (ids type form)
   #:literal-sets (class-literals)
   (pattern (mren:maybe-renamed
             (~optional (~seq : type:expr))
             default-value:expr)
            #:with ids #'mren.ids
            #:with form #'(mren default-value)))

 (define-syntax-class method-decl
   #:attributes (ids type form)
   #:literal-sets (class-literals)
   (pattern mren:maybe-renamed
            #:with ids #'mren.ids
            #:attr type #f
            #:with form this-syntax)
   (pattern (mren:maybe-renamed : type:expr)
            #:with ids #'mren.ids
            #:with form #'mren))

 (define-syntax-class private-decl
   #:attributes (ids type form)
   #:literal-sets (class-literals)
   (pattern id:id
            #:attr ids #'id
            #:attr type #f
            #:with form this-syntax)
   (pattern (id:id : type:expr)
            #:attr ids #'id
            #:with form #'id))

 (define-syntax-class renamed
   #:attributes (ids)
   (pattern (internal-id:id external-id:id)
            #:with ids #'(internal-id external-id)))

 (define-syntax-class maybe-renamed
   #:attributes (ids)
   (pattern id:id
            #:with ids #'(id id))
   (pattern ren:renamed
            #:with ids #'ren.ids))

 (define-syntax-class init-like-clause-names
   (pattern (~or (~literal untyped:init)
                 (~literal untyped:init-field))))

 ;; matches ids with clauses shaped like method clauses,
 ;; not necessarily clauses that declare methods
 (define-syntax-class method-like-clause-names
   (pattern (~or (~literal untyped:inherit-field)
                 (~literal untyped:public)
                 (~literal untyped:pubment)
                 (~literal untyped:public-final)
                 (~literal untyped:override)
                 (~literal untyped:overment)
                 (~literal untyped:override-final)
                 (~literal untyped:augment)
                 (~literal untyped:augride)
                 (~literal untyped:augment-final)
                 (~literal untyped:inherit)
                 (~literal untyped:inherit/super)
                 (~literal untyped:inherit/inner)
                 (~literal untyped:rename-super))))

 (define-syntax-class private-like-clause-names
   (pattern (~or (~literal untyped:private)
                 (~literal untyped:abstract))))

 (define-syntax-class class-clause
   (pattern (clause-name:init-like-clause-names names:init-decl ...)
            #:attr data
            (init-clause #'(clause-name names.form ...)
                         #'clause-name
                         (stx->list #'(names.ids ...))
                         (attribute names.type)
                         (attribute names.optional?)))
   (pattern ((~literal untyped:init-rest) name:private-decl)
            #:attr data (clause #'(untyped:init-rest name.form)
                                #'untyped:init-rest
                                (stx->list #'(name.ids))
                                (list (attribute name.type))))
   (pattern ((~literal untyped:field) names:field-decl ...)
            #:attr data (clause #'(untyped:field names.form ...)
                                #'untyped:field
                                (stx->list #'(names.ids ...))
                                (attribute names.type)))
   (pattern (clause-name:method-like-clause-names names:method-decl ...)
            #:attr data
            (clause #'(clause-name names.form ...)
                    #'clause-name
                    (stx->list #'(names.ids ...))
                    (attribute names.type)))
   (pattern (clause-name:private-like-clause-names names:private-decl ...)
            #:attr data
            (clause #'(clause-name names.form ...)
                    #'clause-name
                    (stx->list #'(names.ids ...))
                    (attribute names.type)))))

;; overriden declaration forms
(define-syntax (define-decl-forms stx)
  (syntax-parse stx
    [(_ ((?clause-name:id ?orig-name:id ?decl-class:id) ...))
     #'(begin (define-syntax (?clause-name stx)
                (syntax-parse stx
                  [(_ (~var ??decl ?decl-class) (... ...))
                   #`(begin #,@(for/list ([id (in-list (attribute ??decl.ids))]
                                          [type (in-list (attribute ??decl.type))]
                                          #:when type)
                                 (tr:class:top-level-property
                                  (tr:class:type-annotation-property
                                   #`(: #,(if (stx-pair? id) (stx-car id) id) #,type)
                                   #t)
                                  #t))
                            ;; set a property here to avoid taint-related issues because
                            ;; we can't transplant the identifiers in the expansion (into the
                            ;; class local table) in certain cases
                            (?orig-name #,@(attribute ??decl.form)))]))
              ...)]))

(define-decl-forms ([init          untyped:init          init-decl]
                    [init-field    untyped:init-field    init-decl]
                    [field         untyped:field         field-decl]
                    [inherit-field untyped:inherit-field method-decl]
                    [init-rest     untyped:init-rest     private-decl]
                    [public        untyped:public        method-decl]
                    [pubment       untyped:pubment       method-decl]
                    [override      untyped:override      method-decl]
                    [augment       untyped:augment       method-decl]
                    [private       untyped:private       private-decl]
                    [inherit       untyped:inherit       method-decl]))
