#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require[scribble/bnf]
@require["guide-utils.ss"]

@(define posn-eval (make-base-eval))

@title[#:tag "define-struct"]{Programmer-Defined Datatypes}

@refalso["structures"]{structure types}

New datatypes are normally created with the @scheme[define-struct]
form, which is the topic of this chapter. The class-based object
system, which we defer to @secref["classes"], offers an alternate
mechanism for creating new datatypes, but even classes and objects are
implemented in terms of structure types.

@; ------------------------------------------------------------
@section{Simple Structure Types: @scheme[define-struct]}

@refalso["define-struct"]{@scheme[define-struct]}

To a first approximation, the syntax of @scheme[define-struct] is

@specform[
(define-struct struct-id (field-id ...))
]{}

A @scheme[define-struct] declaration binds @scheme[_struct-id], but
only to static information about the structure type that cannot be
used directly:

@def+int[
#:eval posn-eval
(define-struct posn (x y))
posn
]

We explain one use of the @scheme[_struct-id] binding in the next
section, @secref["struct-subtypes"].

Meanwhile, in addition to defining @scheme[_struct-id],
@scheme[define-struct] also defines a number of identifiers that are
built from @scheme[_struct-id] and the @scheme[_field-id]s:

@itemize{

 @item{@schemeidfont{make-}@scheme[_struct-id] : a
       @deftech{constructor} function that takes as many arguments as
       the number of @scheme[_field-id]s, and returns an instance of
       the structure type.

       @examples[#:eval posn-eval (make-posn 1 2)]}

 @item{@scheme[_struct-id]@schemeidfont{?} : a @deftech{predicate}
       function that takes a single argument and returns @scheme[#t]
       if it is an instance of the structure type, @scheme[#f]
       otherwise.

       @examples[#:eval posn-eval (posn? 3) (posn? (make-posn 1 2))]}

 @item{@scheme[_struct-id]@schemeidfont{-}@scheme[_field-id] : for
       each @scheme[_field-id], an @deftech{accessor} that extracts
       the value of the corresponding field from an instance of the
       structure type.

       @examples[#:eval posn-eval 
                 (posn-x (make-posn 1 2)) (posn-y (make-posn 1 2))]}

 @item{@schemeidfont{struct:}@scheme[_struct-id] : a
       @deftech{structure type descriptor}, which is a value that
       represents the structure type as a first-class value (with
       @scheme[#:super], as discussed later in
       @secref["struct-options"]).}

}

A @scheme[define-struct] form places no constraints on the kinds of
values that can appear for fields in an instance of the structure
type. For example, @scheme[(make-posn "apple" #f)] produces an
instance of @scheme[posn], even though @scheme["apple"] and
@scheme[#f] are not valid coordinates for the obvious uses of
@scheme[posn] instances. Enforcing constraints on field values, such
as requiring them to be numbers, is normally the job of a contract, as
discussed later in @secref["contracts"].

@; ------------------------------------------------------------
@section[#:tag "struct-subtypes"]{Structure Subtypes}

An extended form of @scheme[define-struct] can be used to define a
@defterm{structure subtype}, which is a structure type that extends an
existing structure type:

@specform[
(define-struct (struct-id super-id) (field-id ...))
]

The @scheme[_super-id] must be a structure type name bound by
@scheme[define-struct] (i.e., the name that cannot be used directly as
an expression).

@as-examples[@schemeblock+eval[
#:eval posn-eval 
(define-struct posn (x y))
(define-struct (3d-posn posn) (z))
]]

A structure subtype inherits the fields of its supertype, and the
subtype constructor accepts the values for the subtype fields after
values for the supertype fields. An instance of a structure subtype
can be used with the predicate and accessors of the
supertype.

@examples[
#:eval posn-eval 
(define p (make-3d-posn 1 2 3))
p
(posn? p)
(posn-x p)
(3d-posn-z p)
]

@; ------------------------------------------------------------
@section[#:tag "trans-struct"]{Opaque versus Transparent Stucture Types}

With a structure type definition like

@schemeblock[
(define-struct posn (x y))
]

an instance of the structure type prints in a way that does not show
any information about the fields values. That is, structure types by
default are @defterm{opaque}. If the accessors and mutators of a
structure type are kept private to a module, then no other module can
rely on the representation of the type's instances.

To make a structure type @defterm{transparent}, use the
@scheme[#:transparent] keyword after the field-name sequence:

@def+int[
#:eval posn-eval
(define-struct posn (x y)
               #:transparent)
(make-posn 1 2)
]

An instance of a transparent structure type prints like a vector, and
it shows the content of the structure's fields. A transparent
structure type also allows reflective operations, such as
@scheme[struct?] and @scheme[struct-info], to be used on its instances
(see @secref["reflection"]).

Structure types are opaque by default, because opaque structure
instances provide more encapsulation guarantees. That is, a library
can use an opaque structure to encapsulate data, and clients of the
library cannot manipulate the data in the structure except as allowed
by the library.

@; ------------------------------------------------------------
@section[#:tag "struct-options"]{More Structure Type Options}

The full syntax of @scheme[define-struct] supports many options, both
at the structure-type level and at the level of individual fields:

@specform/subs[(define-struct id-maybe-super (field ...)
                              struct-option ...)
               ([id-maybe-super struct-id
                                (struct-id super-id)]
                [field field-id
                       [field-id field-option ...]])]

A @scheme[_struct-option] always starts with a keyword:

 @specspecsubform[#:mutable]{

    Causes all fields of the structure to be mutable, and introduces
    for each @scheme[_field-id] a @deftech{mutator}
     @schemeidfont{set-}@scheme[_struct-id]@schemeidfont{-}@scheme[_field-id]@schemeidfont{!}
    that sets the value of the corresponding field in an instance of
    the structure type.

     @defexamples[(define-struct dot (x y) #:mutable)]
                  (define d (make-dot 1 2))
                  (dot-x d)
                  (set-dot-x! d 10)
                  (dot-x d)]

   The @scheme[#:mutable] option can also be used as a
   @scheme[_field-option], in which case it makes an individual field
   mutable.
       
   @defexamples[
   (define-struct person (name [age #:mutable]))
   (define friend (make-person "Barney" 5))
   (set-person-age! friend 6)
   (set-person-name! friend "Mary")]}

 @specspecsubform[(code:line #:transparent)]{
  Controls reflective access to structure instances, as discussed
  in the previous section (@secref["trans-struct"]).}

 @specspecsubform[(code:line #:inspector inspector-expr)]{
  Generalizes @scheme[#:transparent] to support more controlled access
  to reflective operations.}

 @specspecsubform[(code:line #:auto-value auto-expr)]{

  Specifies a value to be used for all automatic fields in the
  structure type, where an automatic field is indicated by the
  @scheme[#:auto] field option. The constructor procedure does not
  accept arguments for automatic fields.

  @defexamples[
    (define-struct posn (x y [z #:auto])
                   #:transparent
                   #:auto-value 0)
    (make-posn 1 2)
  ]}

@;-- FIXME:
@;-- Explain when to use guards instead of contracts, and vice-versa

 @specspecsubform[(code:line #:guard guard-expr)]{
  Specifies a guard procedure to be called whenever an instance of
  the structure type is created. The guard takes as many arguments
  as non-automatic fields in the structure type, and it should return
  the same number of values. The guard can raise an exception if one
  of the given arguments is unacceptable, or it can convert an
  argument.

 @defexamples[
   #:eval posn-eval
   (define-struct thing (name)
                  #:transparent
                  #:guard (lambda (name type-name)
                            (cond
                              [(string? name) name]
                              [(number? name)
                               (number->string name)]
                              [else (error "bad name" name)])))
   (make-thing "apple")
   (make-thing 1/2)
   (make-thing #f)]

  The guard is called even when subtype instances are created. In that
  case, only the fields accepted by the constructor are provided to
  the guard (but the subtype's guard gets both the original fields and
  fields added by the subtype).

 @defexamples[
  #:eval posn-eval
  (define-struct (person thing) (age)
                 #:transparent
                 #:guard (lambda (name age type-name)
                           (if (negative? age)
                               (error "bad age" age)
                               (values name age))))
  (make-person "John" 10)
  (make-person "Mary" -1)
  (make-person #f 10)]}

 @specspecsubform[(code:line #:property prop-expr val-expr)]{
   Associates a property and value with the structure type.  For
   example, the @scheme[prop:procedure] property allows a structure
   instance to be used as a function; the property value determines
   how a call is implemented when using the structure as a function.

 @defexamples[
   (define-struct greeter (name)
                  #:property prop:procedure
                             (lambda (self other)
                               (string-append
                                "Hi " other
                                ", I'm " (greeter-name self))))
   (define joe-greet (make-greeter "Joe"))
   (greeter-name joe-greet)
   (joe-greet "Mary")
   (joe-greet "John")]}

 @specspecsubform[(code:line #:super super-expr)]{

  An alternative to supplying a @scheme[super-id] next to
  @scheme[struct-id]. Instead of the name of a structure type (which is
  not an expression), @scheme[super-expr] should produce a
  @tech{structure type descriptor} value. An advantage of
  @scheme[#:super] is that structure type descriptors are values, so
  they can be passed to procedures.

  @defexamples[
    #:eval posn-eval
    (define (make-raven-constructor super-type)
      (define-struct raven ()
                     #:super super-type
                     #:transparent
                     #:property prop:procedure (lambda (self)
                                                 'nevermore))
      make-raven)
    (let ([r ((make-raven-constructor struct:posn) 1 2)])
      (list r (r)))
    (let ([r ((make-raven-constructor struct:thing) "apple")])
      (list r (r)))]}


@; ------------------------------------------------------------
@section{Structure Type Generativity}

Each time that a @scheme[define-struct] form is evaluated, it
generates a structure type that is distinct from all existing
structure types, even if some other structure type has the same name
and fields.

This generativity is useful for enforcing abstractions and
implementing programs such as interpreters, but beware of placing a
@scheme[define-struct] form in positions that are evaluated multiple
times.

@defexamples[
(define (add-bigger-fish lst)
  (define-struct fish (size) #:transparent) (code:comment #,(t "new every time"))
  (cond
   [(null? lst) (list (make-fish 1))]
   [else (cons (make-fish (* 2 (fish-size (car lst))))
               lst)]))

(add-bigger-fish null)
(add-bigger-fish (add-bigger-fish null))
]
@defs+int[
[(define-struct fish (size) #:transparent)
 (define (add-bigger-fish lst)
   (cond
    [(null? lst) (list (make-fish 1))]
    [else (cons (make-fish (* 2 (fish-size (car lst))))
                lst)]))]
(add-bigger-fish (add-bigger-fish null))
]

@refdetails["structures"]{structure types}
