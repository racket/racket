#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))

@title{Typed Classes}

@bold{Warning}: the features described in this section are experimental
and may not work correctly. Some of the features will change by
the next release. In particular, typed-untyped interaction for classes
will not be backwards compatible so do not rely on the current semantics.

Typed Racket provides support for object-oriented programming with
the classes and objects provided by the @racketmodname[racket/class]
library.

@section{Special forms}

@defmodule[typed/racket/class]

The special forms below are provided by the @racketmodname[typed/racket/class]
and @racketmodname[typed/racket] modules but not by
@racketmodname[typed/racket/base]. The @racketmodname[typed/racket/class] module
additional provides all other bindings from @racketmodname[racket/class].

@;; This uses a trick to link to racket/class's class identifier
@;; in certain cases rather than the class defined here
@(module id-holder racket/base
   (require scribble/manual (for-label racket/class))
   (provide class-element
            d/p-element d/o-element
            d/pm-element d/a-element
            d/pr-element)
   (define class-element (racket class))
   (define d/p-element (racket define/public))
   (define d/o-element (racket define/override))
   (define d/pm-element (racket define/pubment))
   (define d/a-element (racket define/augment))
   (define d/pr-element (racket define/private)))
@(require 'id-holder)

@defform[#:literals (inspect init init-field init-rest field inherit-field
                     public pubment override augment private inherit
                     begin)
         (class superclass-expr
           maybe-type-parameters
           class-clause ...)
         #:grammar ([class-clause (inspect inspector-expr)
                                  (init init-decl ...)
                                  (init-field init-decl ...)
                                  (init-rest id/type)
                                  (field field-decl ...)
                                  (inherit-field field-decl ...)
                                  (public maybe-renamed/type ...)
                                  (pubment maybe-renamed/type ...)
                                  (override maybe-renamed/type ...)
                                  (augment maybe-renamed/type ...)
                                  (private id/type ...)
                                  (inherit id ...)
                                  method-definition
                                  definition
                                  expr
                                  (begin class-clause ...)]
                    [maybe-type-parameters (code:line)
                                           (code:line #:forall (type-variable ...))
                                           (code:line #:∀ (type-variable ...))]
                    [init-decl id/type
                               [renamed]
                               [renamed : type-expr]
                               [maybe-renamed default-value-expr]
                               [maybe-renamed : type-expr default-value-expr]]
                    [field-decl (maybe-renamed default-value-expr)
                                (maybe-renamed : type-expr default-value-expr)]
                    [id/type id
                             [id : type-expr]]
                    [maybe-renamed/type maybe-renamed
                                        [maybe-renamed : type-expr]]
                    [maybe-renamed id
                                   renamed]
                    [renamed (internal-id external-id)])]{
  Produces a class with type annotations that allows Typed Racket to type-check
  the methods, fields, and other clauses in the class.

  The meaning of the class clauses are the same as in the @class-element
  form from the @racketmodname[racket/class] library with the exception of
  the additional optional type annotations. Additional class clause
  forms from @class-element that are not listed in the grammar above are
  not currently supported in Typed Racket.

  @ex[
    (define fish%
      (class object%
        (init [size : Real])

        (: current-size Real)
        (define current-size size)

        (super-new)

        (: get-size (-> Real))
        (define/public (get-size)
          current-size)

        (: grow (Real -> Void))
        (define/public (grow amt)
          (set! current-size (+ amt current-size)))

        (: eat ((Object [get-size (-> Real)]) -> Void))
        (define/public (eat other-fish)
          (grow (send other-fish get-size)))))

    (define dory (new fish% [size 5.5]))
  ]

  Within a typed class form, one of the class clauses must be a call
  to @racket[super-new]. Failure to call @racket[super-new] will result in
  a type error. In addition, dynamic uses of @racket[super-new] (e.g.,
  calling it in a separate function within the dynamic extent of the
  class form's clauses) are restricted.

  @ex[
    (class object%
      (code:comment "Note the missing `super-new`")
      (init-field [x : Real 0] [y : Real 0]))
  ]

  If any identifier with an optional type annotation is left without an
  annotation, the type-checker will assume the type @racket[Any]
  (or @racket[Procedure] for methods) for that identifier.

  @ex[
    (define point%
      (class object%
        (super-new)
        (init-field x y)))
    point%
  ]

  When @racket[type-variable] is provided, the class is parameterized
  over the given type variables. These type variables are in scope inside
  the body of the class. The resulting class can be instantiated at
  particular types using @racket[inst].

  @ex[
    (define cons%
      (class object%
        #:forall (X Y)
        (super-new)
        (init-field [car : X] [cdr : Y])))
    cons%
    (new (inst cons% Integer String) [car 5] [cdr "foo"])
  ]

  Initialization arguments may be provided by-name using the @racket[new]
  form, by-position using the @racket[make-object] form, or both using
  the @racket[instantiate] form.

  As in ordinary Racket classes, the order in which initialization arguments
  are declared determines the order of initialization types in the class type.

  Furthermore, a class may also have a typed @racket[init-rest] clause, in
  which case the class constructor takes an unbounded number of arguments
  by-position. The type of the @racket[init-rest] clause must be either a
  @racket[List] type, @racket[Listof] type, or any other list type.

  @ex[
    (define point-copy%
      (code:comment "a point% with a copy constructor")
      (class object%
        (super-new)
        (init-rest [rst : (U (List Integer Integer)
                             (List (Object (field [x Integer]
                                                  [y Integer]))))])
        (field [x : Integer 0] [y : Integer 0])
        (match rst
          [(list (? integer? *x) *y)
           (set! x *x) (set! y *y)]
          [(list (? (negate integer?) obj))
           (set! x (get-field x obj))
           (set! y (get-field y obj))])))
    (define p1 (make-object point-copy% 1 2))
    (make-object point-copy% p1)
  ]
}

@(define (define/foo-content define/foo)
   @elem{
   Like @define/foo from @racketmodname[racket/class], but uses the binding of
   @racket[define] from Typed Racket.

   The @racket[formals] may specify type annotations as in @racket[define].
})

@defform*[((define/public id expr)
           (define/public (id . formals) body ...+))]{
  @define/foo-content[d/p-element]
}

@defform*[((define/override id expr)
           (define/override (id . formals) body ...+))]{
  @define/foo-content[d/o-element]
}

@defform*[((define/pubment id expr)
           (define/pubment (id . formals) body ...+))]{
  @define/foo-content[d/pm-element]
}

@defform*[((define/augment id expr)
           (define/augment (id . formals) body ...+))]{
  @define/foo-content[d/a-element]
}

@defform*[((define/private id expr)
           (define/private (id . formals) body ...+))]{
  @define/foo-content[d/pr-element]
}

@section{Types}

@defform[#:literals (init init-field init-rest field augment)
         (Class class-type-clause ...)
         #:grammar ([class-type-clause name+type
                                       (init init-type ...)
                                       (init-field init-type ...)
                                       (init-rest name+type)
                                       (field name+type ...)
                                       (augment name+type ...)
                                       (code:line #:implements type-alias-id)
                                       (code:line #:row-var row-var-id)]
                    [init-type name+type
                               [id type #:optional]]
                    [name+type [id type]])]{
  The type of a class with the given initialization argument, method, and
  field types.

  @ex[
    (: food% (Class (init [liquid? Boolean])
                    (field [nutrition Integer])
                    [get-nutrition (-> Integer)]))
  ]

  The types of methods are provided either without a keyword, in which case
  they correspond to public methods, or with the @racketidfont{augment}
  keyword, in which case they correspond to a method that can be augmented.

  An initialization argument type specifies a name and type and optionally
  a @racket[#:optional] keyword. An initialization argument type with
  @racket[#:optional] corresponds to an argument that does not need to
  be provided at object instantiation.

  @ex[
    (: drink% (Class (init [color String]
                           [carbonated? Boolean]
                           [viscosity Positive-Real #:optional])))
  ]

  The order of initialization arguments in the type is significant, because
  it determines the types of by-position arguments for use with
  @racket[make-object] and @racket[instantiate].

  @ex[
    (define drink%
      (class object%
        (super-new)
        (code:comment "The order of `color' and `carbonated?' cannot be swapped")
        (init color carbonated? [viscosity 1.002])))
    (code:comment "The order of initialization matches the order in the type")
    (make-object drink% "purple" #t)
  ]

  When @racket[type-alias-id] is provided, the resulting class type
  includes all of the initialization argument, method, and field types
  from the specified type alias (which must be an alias for a class type).
  Multiple @racket[#:implements] clauses may be provided for a single class
  type.

  @ex[
    (define-type Point<%> (Class (field [x Real] [y Real])))
    (: colored-point% (Class #:implements Point<%>
                             (field [color String])))
  ]

  When @racket[row-var-id] is provided, the class type is an abstract type
  that is row polymorphic. A row polymorphic class type can be instantiated
  at a specific row using @racket[inst]. Only a single @racket[#:row-var]
  clause may appear in a class type.
}

@defidform[ClassTop]{
  The supertype of all class types. A value of this type
  cannot be used for subclassing, object creation, or most
  other class functions. Its primary use is for reflective
  operations such as @racket[is-a?].
}

@defform[#:literals (field)
         (Object object-type-clause ...)
         #:grammar ([object-type-clause name+type
                                        (field name+type ...)])]{
  The type of an object with the given field and method types.

  @ex[
    (new object%)
    (new (class object% (super-new) (field [x : Real 0])))
  ]
}

@defform[(Instance class-type-expr)]{
  The type of an object that corresponds to @racket[class-type-expr].

  This is the same as an @racket[Object] type that has all of the
  method and field types from @racket[class-type-expr]. The types for
  the @racketidfont{augment} and @racketidfont{init} clauses in the
  class type are ignored.

  @ex[
    (define-type Point% (Class (init-field [x Integer] [y Integer])))
    (: a-point (Instance Point%))
    (define a-point
      (new (class object%
             (super-new)
             (init-field [x : Integer 0] [y : Integer 0]))))
  ]
}

