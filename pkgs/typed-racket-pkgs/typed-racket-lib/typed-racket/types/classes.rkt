#lang racket/base

;; This module provides helper syntax classes and functions for
;; working with class/object types and rows

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types resolve)
         (except-in racket/class private)
         racket/dict
         racket/list
         racket/match
         syntax/parse
         syntax/stx
         (only-in unstable/list check-duplicate)
         (only-in unstable/sequence in-syntax)
         (for-template racket/class))

(provide Class:
         row-constraints
         row-clauses
         infer-row-constraints
         infer-row
         check-row-constraints
         object-type-clauses
         class-type-clauses)

;; Data definitions
;;
;; A RowConstraint is a
;;   List(List<Sym>, List<Sym>, List<Sym>)

;; Syntax -> Syntax
;; Turn into datums and then flatten
(define (flatten/datum stx)
  (flatten (syntax->datum stx)))

;; Syntax classes for rows
(define-splicing-syntax-class row-constraints
  #:literals (init init-field field)
  (pattern (~seq (~or (init iname:id ...)
                      (init-field ifname:id ...)
                      (field fname:id ...)
                      mname:id)
                 ...)
            #:attr init-names (flatten/datum #'((iname ...) ...))
            #:attr init-field-names (flatten/datum #'((ifname ...) ...))
            #:attr field-names (flatten/datum #'((fname ...) ...))
            #:attr method-names (syntax->datum #'(mname ...))
            #:attr all-field-names (append (attribute init-field-names)
                                           (attribute field-names))
            #:attr all-init-names (append (attribute init-field-names)
                                          (attribute init-names))
            #:fail-when
            (check-duplicate (attribute all-init-names))
            "duplicate init or init-field clause"
            #:fail-when
            (check-duplicate (attribute all-field-names))
            "duplicate field or init-field clause"
            #:attr constraints
            (list (attribute all-init-names)
                  (attribute all-field-names)
                  (attribute method-names))))

;; Row RowConstraints (Symbol -> Void) -> Void
;; Check if the given row satisfies the absence constraints
;; on the row variable or not. Call the fail thunk if it
;; doesn't.
(define (check-row-constraints row constraints fail)
  (match-define (list init-absents field-absents method-absents)
                constraints)
  (match-define (Row: inits fields methods) row)
  ;; check a given clause type (e.g., init, field)
  (define (check-clauses row-dict absence-set)
    (for ([(name _) (in-dict row-dict)])
      (when (member name absence-set)
        (fail name))))
  (check-clauses inits init-absents)
  (check-clauses fields field-absents)
  (check-clauses methods method-absents))

;; Row types are similar to class types
(define-splicing-syntax-class (row-clauses parse-type)
  #:description "Row type clause"
  #:attributes (row)
  (pattern (~seq (~var clause (type-clause parse-type)) ...)
           #:attr inits (apply append (attribute clause.init-entries))
           #:attr fields (apply append (attribute clause.field-entries))
           #:attr methods (apply append (attribute clause.method-entries))
           #:attr row (make-Row (attribute inits)
                                (attribute fields)
                                (attribute methods))
           #:fail-when
           (check-duplicate (map first (attribute inits)))
           "duplicate init or init-field clause"
           #:fail-when
           (check-duplicate (map first (attribute fields)))
           "duplicate field or init-field clause"
           #:fail-when
           (check-duplicate (map first (attribute methods)))
           "duplicate method clause"))

;; Type -> RowConstraint
;; Infer constraints on a row for a row polymorphic function
(define (infer-row-constraints type)
  (define constraints (list null null null))
  ;; Crawl the type tree and mutate constraints when a
  ;; class type with row variable is found.
  (define (inf type)
    (type-case
     (#:Type inf #:Filter (sub-f inf) #:Object (sub-o inf))
     type
     [#:Class row inits fields methods
      (cond
       [(and row (F? row))
        (match-define (list init-cs field-cs method-cs) constraints)
        (set! constraints
              (list (append (dict-keys inits) init-cs)
                    (append (dict-keys fields) field-cs)
                    (append (dict-keys methods) method-cs)))
        (make-Class row inits fields methods)]
       [else
        (match-define (list (list init-names init-tys init-reqds) ...) inits)
        (match-define (list (list field-names field-tys) ...) fields)
        (match-define (list (list method-names method-tys) ...) methods)
        (make-Class
         (and row (inf row))
         (map list init-names (map inf init-tys) init-reqds)
         (map list field-names (map inf field-tys))
         (map list method-names (map inf method-tys)))])]))
  (inf type)
  (map remove-duplicates constraints))

;; infer-row : RowConstraints Type -> Row
;; Infer a row based on a class type and row constraints
(define (infer-row constraints class-type)
  (match-define (list init-cs field-cs method-cs) constraints)
  (match-define (Class: _ inits fields methods)
                (resolve class-type))
  (define (dict-remove* dict keys)
    (for/fold ([dict dict])
              ([key keys])
      (dict-remove dict key)))
  (make-Row (dict-remove* inits init-cs)
            (dict-remove* fields field-cs)
            (dict-remove* methods method-cs)))

;; Syntax -> Syntax
;; removes two levels of nesting
(define (flatten-class-clause stx)
  (flatten (map stx->list (stx->list stx))))

;; Syntax class for object type parsing
(define-splicing-syntax-class object-type-clauses
  #:description "Object type clause"
  #:attributes (field-names field-types method-names method-types)
  #:literals (field)
  (pattern (~seq (~or (field field-clause:field-or-method-type ...)
                      method-clause:field-or-method-type)
                 ...)
           #:with field-names (flatten-class-clause #'((field-clause.label ...) ...))
           #:with field-types (flatten-class-clause #'((field-clause.type ...) ...))
           #:with method-names #'(method-clause.label ...)
           #:with method-types #'(method-clause.type ...)
           #:fail-when
           (check-duplicate-identifier (syntax->list #'field-names))
           "duplicate field or init-field clause"
           #:fail-when
           (check-duplicate-identifier (syntax->list #'method-names))
           "duplicate method clause"))

;; Syntax class for class type parsing
;;
;; The `parse-type` argument is provided so that parsing can
;; happen in the syntax class without causing circular module
;; dependencies
(define-splicing-syntax-class (class-type-clauses parse-type)
  #:description "Class type clause"
  #:attributes (row-var extends-types
                inits fields methods)
  (pattern (~seq (~or (~optional (~seq #:row-var row-var:id))
                      (~seq #:implements extends-type:expr)
                      (~var clause (type-clause parse-type)))
                 ...)
           #:attr inits (apply append (attribute clause.init-entries))
           #:attr fields (apply append (attribute clause.field-entries))
           #:attr methods (apply append (attribute clause.method-entries))
           #:with extends-types #'(extends-type ...)
           #:fail-when
           (check-duplicate (map first (attribute inits)))
           "duplicate init or init-field clause"
           #:fail-when
           (check-duplicate (map first (attribute fields)))
           "duplicate field or init-field clause"
           #:fail-when
           (check-duplicate (map first (attribute methods)))
           "duplicate method clause"))

;; Stx Stx Listof<Boolean> (Stx -> Type) -> Listof<(List Symbol Type Boolean)>
;; Construct init entries for a dictionary for the class type
(define (make-init-entries labels types optionals parse-type)
  (for/list ([label (in-syntax labels)]
             [type (in-syntax types)]
             [optional? optionals])
    (list (syntax-e label)
          (parse-type type)
          optional?)))

;; Stx Stx (Stx -> Type) -> Listof<(List Symbol Type)>
;; Construct field entries for a class type dictionary
(define (make-field-entries labels types parse-type)
  (for/list ([label (in-syntax labels)]
             [type (in-syntax types)])
    (list (syntax-e label) (parse-type type))))

(define-syntax-class (type-clause parse-type)
  #:attributes (init-entries field-entries method-entries)
  #:literals (init init-field field)
  (pattern (~or (init init-clause:init-type ...)
                (init-field init-field-clause:init-type ...)
                (field field-clause:field-or-method-type ...)
                method-clause:field-or-method-type)
           #:attr init-entries
           (append (if (attribute init-clause)
                       (make-init-entries
                        #'(init-clause.label ...)
                        #'(init-clause.type ...)
                        (attribute init-clause.optional?)
                        parse-type)
                       null)
                   (if (attribute init-field-clause)
                       (make-init-entries
                        #'(init-field-clause.label ...)
                        #'(init-field-clause.type ...)
                        (attribute init-field-clause.optional?)
                        parse-type)
                       null))
           #:attr field-entries
           (append (if (attribute field-clause)
                       (make-field-entries
                        #'(field-clause.label ...)
                        #'(field-clause.type ...)
                        parse-type)
                       null)
                   (if (attribute init-field-clause)
                       (make-field-entries
                        #'(init-field-clause.label ...)
                        #'(init-field-clause.type ...)
                        parse-type)
                       null))
           #:attr method-entries
           (if (attribute method-clause)
               (list (list (syntax-e #'method-clause.label)
                           (parse-type #'method-clause.type)))
               null)))

(define-syntax-class init-type
  #:description "Initialization argument label and type"
  #:attributes (label type optional?)
  (pattern
   (label:id type:expr
    (~optional (~and #:optional (~bind [optional? #t]))))))

(define-syntax-class field-or-method-type
  #:description "Pair of field or method label and type"
  #:attributes (label type)
  (pattern (label:id type:expr)))

