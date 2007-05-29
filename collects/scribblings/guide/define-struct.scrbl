#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require[(lib "bnf.ss" "scribble")]
@require["guide-utils.ss"]

@title{Programmer-Defined Datatypes}

This section introduces the @scheme[define-struct] form for creating
your own datatypes. The class-based object system offers an alternate
mechanism for creating new datatypes; the resulting objects are
nevertheless implemented as structures, and we defer discussion of
objects to @secref["classes"].

@; ------------------------------------------------------------
@section{Simple Structure Types}

To a first approximation, the syntax of @scheme[define-struct] is

@schemeblock[
(define-struct _struct-id (_field-id ...))
]

Such a definition binds @scheme[_struct-id], but only to static
information about the structure type that cannot be used directly:

@def+int[
(define-struct posn (x y))
posn
]

We explain one use of the @scheme[_struct-id] binding in the next
section.

In addition to defining @scheme[_struct-id], however,
@scheme[define-struct] also defines a number of procedures whose names
are built from @scheme[_struct-id] and the @scheme[_field-id]s:

@itemize{

 @item{@schemeidfont{make-}@scheme[_struct-id] : a
       @defterm{constructor} procedure that takes as many arguments as
       the number of @scheme[_field-id]s, and returns an instance of
       the structure type.

       @examples[(make-posn 1 2)]}

 @item{@scheme[_struct-id]@schemeidfont{?} : a @defterm{predicate}
       procedure that takes a single argument and returns @scheme[#t]
       if it is an instance of the structure type, @scheme[#f]
       otherwise.

       @examples[(posn? 3) (posn? (make-posn 1 2))]}

 @item{@scheme[_struct-id]@schemeidfont{-}@scheme[_field-id] : for
       each @scheme[_field-id], an @defterm{accessor} that extracts
       the value of the corresponding field from an instance of the
       structure type.

       @examples[(posn-x (make-posn 1 2)) (posn-y (make-posn 1 2))]}

 @item{@schemeidfont{set-}@scheme[_struct-id]@schemeidfont{-}@scheme[_field-id]@schemeidfont{!} : for
       each @scheme[_field-id], a @defterm{mutator} that sets
       the value of the corresponding field in an instance of the
       structure type.

       @examples[(define p (make-posn 1 2))
                 (posn-x p)
                 (set-posn-x! p 10)
                 (posn-x p)]}
}

A @scheme[define-struct] form places no constraints on the kinds of
values that can appears for fields in an instance of the structure
type. For example, @scheme[(make-posn "apple" #f)] produces an
instance of @scheme[posn], even though @scheme["apple"] and
@scheme[#f] are not valid co-ordinates for the obvious uses of
@scheme[posn] instances. Enforcing constraints on field values, such
as requiring them to be numbers, is the job of a contract, as
discussed later in @secref["contracts"].

@; ------------------------------------------------------------
@section{Structure Subtypes}

An extended form of @scheme[defin-struct] can be used to define a
@defterm{structure subtype}, which is a structure type that extends an
existing structure type:

@schemeblock[
(define-struct (_struct-id _super-id) (_field-id ...))
]

The @scheme[_super-id] must be a structure type name bound by
@scheme[define-struct] (i.e., the name bound by @scheme[define-struct]
that cannot be used directly as an expression).

@as-examples[@schemeblock+eval[
(define-struct posn (x y))
(define-struct (3d-posn posn) (z))
]]

A structure subtype inherits the fields of its supertype, and the
subtype constructor accepts the values for the subtype fields after
values for the supertype fields. An instance of a structure subtype
can be used with the predicate, accessor, and mutator fields of the
supertype.

@examples[
(define p (make-3d-posn 1 2 3))
p
(posn? p)
(posn-x p)
(3d-posn-z p)
]

@; ------------------------------------------------------------
@section{Opaque versus Transparent Stucture Types}

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
@scheme[#:inspector] keyword with the value @scheme[#f] after the
field-name sequence:

@def+int[
(define-struct posn (x y) 
               #f)
(make-posn 1 2)
]

An instance of a transparent structure type prints like a vector, and
it shows the content of the structure's fields. A transparent
structure type allows allows reflective operations like
@scheme[struct?] and @scheme[struct-info] to be used on its
instances (see @secref["reflection"]).
