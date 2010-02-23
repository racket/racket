#lang scribble/manual

@begin[(require "utils.ss"
		scribble/core scribble/eval
		(for-label (only-meta-in 0 typed/scheme) mzlib/etc))]

@(define the-eval (make-base-eval))
@(the-eval '(require typed/scheme))

@title[#:tag "types"]{Types in Typed Scheme}

Typed Scheme provides a rich variety of types to describe data. This
section introduces them.  

@section{Basic Types}

The most basic types in Typed Scheme are those for primitive data,
such as @scheme[True] and @scheme[False] for booleans, @scheme[String]
for strings, and @scheme[Char] for characters.

@interaction[#:eval the-eval
'"hello, world"
#\f
#t
#f]

Each symbol is given a unique type containing only that symbol.  The
@scheme[Symbol] type includes all symbols.

@interaction[#:eval the-eval
'foo
'bar]

Typed Scheme also provides a rich hierarchy for describing particular
kinds of numbers.

@interaction[#:eval the-eval
0
-7
14
3.2
7+2.8i]

Finally, any value is itself a type:

@interaction[#:eval the-eval
(ann 23 : 23)]

@section{Function Types}

We have already seen some examples of function types.  Function types
are constructed using @scheme[->], with the argument types before the
arrow and the result type after.  Here are some example function
types:

@schemeblock[
(Number -> Number)
(String String -> Boolean)
(Char -> (values String Natural))
]

The first type requires a @scheme[Number] as input, and produces a
@scheme[Number].  The second requires two arguments.  The third takes
one argument, and produces  @rtech{multiple values}, of types
@scheme[String] and @scheme[Natural].  Here are example functions for
each of these types.

@interaction[#:eval the-eval
(lambda: ([x : Number]) x)
(lambda: ([a : String] [b : String]) (equal? a b))
(lambda: ([c : Char]) (values (string c) (char->integer c)))]


@section{Union Types}

Sometimes a value can be one of several types.  To specify this, we
can use a union type, written with the type constructor @scheme[U].  

@interaction[#:eval the-eval
(let ([a-number 37])
  (if (even? a-number)
      'yes
      'no))]

Any number of types can be combined together in a union, and nested
unions are flattened.  

@schemeblock[(U Number String Boolean Char)]

@section{Recursive Types}

@deftech{Recursive types} can refer to themselves.  This allows a type
to describe an infinite family of data.  For example, this is the type
of binary trees of numbers.  

@schemeblock[
(Rec BT (U Number (Pair BT BT)))]

The @scheme[Rec] type constructor specifies that the type @scheme[BT]
refers to the whole binary tree type within the body of the
@scheme[Rec] form.

@section{Structure Types}

Using @scheme[define-struct:] introduces new types, distinct from any
previous type.    

@schemeblock[(define-struct: point ([x : Real] [y : Real]))]

Instances of this structure, such as @scheme[(make-point 7 12)], have type @scheme[point].

@section{Subtyping}

In Typed Scheme, all types are placed in a hierarchy, based on what
values are included in the type.  When an element of a larger type is
expected, an element of a smaller type may be provided.  The smaller
type is called a @deftech{subtype} of the larger type.  The larger
type is called a @deftech{supertype}. For example,
@scheme[Integer] is a subtype of @scheme[Real], since every integer is
a real number.  Therefore, the following code is acceptable to the
type checker:

@schemeblock[
(: f (Real -> Real))
(define (f x) (* x 0.75))

(: x Integer)
(define x -125)

(f x)
]

All types are subtypes of the @scheme[Any] type.

The elements of a union type are individually subtypes of the whole
union, so @scheme[String] is a subtype of @scheme[(U String Number)].
One function type is a subtype of another if they have the same number
of arguments, the subtype's arguments are more permissive (is a supertype), and the
subtype's result type is less permissive (is a subtype).
For example, @scheme[(Any -> String)] is a subtype of @scheme[(Number
-> (U String #f))].

@;@section{Occurrence Typing}

@section{Polymorphism}

Typed Scheme offers abstraction over types as well as values.

@subsection{Polymorphic Data Structures}

Virtually every Scheme program uses lists and sexpressions.  Fortunately, Typed
Scheme can handle these as well.  A simple list processing program can be
written like this:

@schememod[
typed/scheme
(: sum-list ((Listof Number) -> Number))
(define (sum-list l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))]))
]

This looks similar to our earlier programs --- except for the type
of @scheme[l], which looks like a function application.  In fact, it's
a use of the @italic{type constructor} @scheme[Listof], which takes
another type as its input, here @scheme[Number].  We can use
@scheme[Listof] to construct the type of any kind of list we might
want.  

We can define our own type constructors as well.  For example, here is
an analog of the @tt{Maybe} type constructor from Haskell:

@schememod[
typed/scheme
(define-struct: None ())
(define-struct: (a) Some ([v : a]))

(define-type-alias (Opt a) (U None (Some a)))

(: find (Number (Listof Number) -> (Opt Number)))
(define (find v l)
  (cond [(null? l) (make-None)]
        [(= v (car l)) (make-Some v)]
        [else (find v (cdr l))]))
]

The first @scheme[define-struct:] defines @scheme[None] to be
a structure with no contents.  

The second definition

@schemeblock[
(define-struct: (a) Some ([v : a]))
]

creates a parameterized type, @scheme[Just], which is a structure with
one element, whose type is that of the type argument to
@scheme[Just].  Here the type parameters (only one, @scheme[a], in
this case) are written before the type name, and can be referred to in
the types of the fields.

The type alias definiton
@schemeblock[
  (define-type-alias (Opt a) (U None (Some a)))
]
creates a parameterized alias --- @scheme[Opt] is a potential
container for whatever type is supplied.

The @scheme[find] function takes a number @scheme[v] and list, and
produces @scheme[(make-Some v)] when the number is found in the list,
and @scheme[(make-None)] otherwise.  Therefore, it produces a
@scheme[(Opt Number)], just as the annotation specified.  

@subsection{Polymorphic Functions}

Sometimes functions over polymorphic data structures only concern
themselves with the form of the structure.  For example, one might
write a function that takes the length of a list of numbers:

@schememod[
typed/scheme
(: list-number-length ((Listof Number) -> Integer))
(define (list-number-length l)
  (if (null? l)
      0
      (add1 (list-number-length (cdr l)))))]

and also a function that takes the length of a list of strings:

@schememod[
typed/scheme
(: list-string-length ((Listof String) -> Integer))
(define (list-string-length l)
  (if (null? l)
      0
      (add1 (list-string-length (cdr l)))))]

Notice that both of these functions have almost exactly the same
definition; the only difference is the name of the function.  This
is because neither function uses the type of the elements in the
definition.

We can abstract over the type of the element as follows:

@schememod[
typed/scheme
(: list-length (All (A) ((Listof A) -> Integer)))
(define (list-length l)
  (if (null? l)
      0
      (add1 (list-length (cdr l)))))]

The new type constructor @scheme[All] takes a list of type
variables and a body type.  The type variables are allowed to
appear free in the body of the @scheme[All] form.


@include-section["varargs.scrbl"]

@;@section{Refinement Types}
