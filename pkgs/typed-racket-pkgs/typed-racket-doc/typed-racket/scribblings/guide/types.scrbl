#lang scribble/manual

@begin[(require "../utils.rkt"
		scribble/core scribble/eval
		(for-label (only-meta-in 0 typed/racket)))]

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))

@title[#:tag "types"]{Types in Typed Racket}

Typed Racket provides a rich variety of types to describe data. This
section introduces them.

@section{Basic Types}

The most basic types in Typed Racket are those for primitive data,
such as @racket[True] and @racket[False] for booleans, @racket[String]
for strings, and @racket[Char] for characters.

@interaction[#:eval the-eval
'"hello, world"
#\f
#t
#f]

Each symbol is given a unique type containing only that symbol.  The
@racket[Symbol] type includes all symbols.

@interaction[#:eval the-eval
'foo
'bar]

Typed Racket also provides a rich hierarchy for describing particular
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
are constructed using @racket[->], with the argument types before the
arrow and the result type after.  Here are some example function
types:

@racketblock[
(Number -> Number)
(String String -> Boolean)
(Char -> (values String Natural))
]

The first type requires a @racket[Number] as input, and produces a
@racket[Number].  The second requires two arguments.  The third takes
one argument, and produces  @rtech{multiple values}, of types
@racket[String] and @racket[Natural].  Here are example functions for
each of these types.

@interaction[#:eval the-eval
(lambda: ([x : Number]) x)
(lambda: ([a : String] [b : String]) (equal? a b))
(lambda: ([c : Char]) (values (string c) (char->integer c)))]


@section{Union Types}

Sometimes a value can be one of several types.  To specify this, we
can use a union type, written with the type constructor @racket[U].

@interaction[#:eval the-eval
(let ([a-number 37])
  (if (even? a-number)
      'yes
      'no))]

Any number of types can be combined together in a union, and nested
unions are flattened.

@racketblock[(U Number String Boolean Char)]

@section{Recursive Types}

@deftech{Recursive types} can refer to themselves.  This allows a type
to describe an infinite family of data.  For example, this is the type
of binary trees of numbers.

@racketblock[
(define-type BinaryTree (Rec BT (U Number (Pair BT BT))))]

The @racket[Rec] type constructor specifies that the type @racket[BT]
refers to the whole binary tree type within the body of the
@racket[Rec] form.

@section{Structure Types}

Using @racket[struct:] introduces new types, distinct from any
previous type.

@racketblock[(struct: point ([x : Real] [y : Real]))]

Instances of this structure, such as @racket[(point 7 12)], have type @racket[point].

@section{Subtyping}

In Typed Racket, all types are placed in a hierarchy, based on what
values are included in the type.  When an element of a larger type is
expected, an element of a smaller type may be provided.  The smaller
type is called a @deftech{subtype} of the larger type.  The larger
type is called a @deftech{supertype}. For example,
@racket[Integer] is a subtype of @racket[Real], since every integer is
a real number.  Therefore, the following code is acceptable to the
type checker:

@racketblock[
(: f (Real -> Real))
(define (f x) (* x 0.75))

(: x Integer)
(define x -125)

(f x)
]

All types are subtypes of the @racket[Any] type.

The elements of a union type are individually subtypes of the whole
union, so @racket[String] is a subtype of @racket[(U String Number)].
One function type is a subtype of another if they have the same number
of arguments, the subtype's arguments are more permissive (is a supertype), and the
subtype's result type is less permissive (is a subtype).
For example, @racket[(Any -> String)] is a subtype of @racket[(Number
-> (U String #f))].

@;@section{Occurrence Typing}

@section{Polymorphism}

Typed Racket offers abstraction over types as well as values. This allows
the definition of functions that use @deftech{parametric polymorphism}.

@subsection{Polymorphic Data Structures}

Virtually every Racket program uses lists and other collections.  Fortunately, Typed
Racket can handle these as well.  A simple list processing program can be
written like this:

@racketmod[
typed/racket
(: sum-list ((Listof Number) -> Number))
(define (sum-list l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))]))
]

This looks similar to our earlier programs --- except for the type
of @racket[l], which looks like a function application.  In fact, it's
a use of the @italic{type constructor} @racket[Listof], which takes
another type as its input, here @racket[Number].  We can use
@racket[Listof] to construct the type of any kind of list we might
want.

We can define our own type constructors as well.  For example, here is
an analog of the @tt{Maybe} type constructor from Haskell:

@racketmod[
typed/racket
(struct: None ())
(struct: (a) Some ([v : a]))

(define-type (Opt a) (U None (Some a)))

(: find (Number (Listof Number) -> (Opt Number)))
(define (find v l)
  (cond [(null? l) (None)]
        [(= v (car l)) (Some v)]
        [else (find v (cdr l))]))
]

The first @racket[struct:] defines @racket[None] to be
a structure with no contents.

The second definition

@racketblock[
(struct: (a) Some ([v : a]))
]

creates a parameterized type, @racket[Some], which is a structure with
one element, whose type is that of the type argument to
@racket[Some].  Here the type parameters (only one, @racket[a], in
this case) are written before the type name, and can be referred to in
the types of the fields.

The type definiton
@racketblock[
  (define-type (Opt a) (U None (Some a)))
]
creates a parameterized type --- @racket[Opt] is a potential
container for whatever type is supplied.

The @racket[find] function takes a number @racket[v] and list, and
produces @racket[(Some v)] when the number is found in the list,
and @racket[(None)] otherwise.  Therefore, it produces a
@racket[(Opt Number)], just as the annotation specified.

@subsection{Polymorphic Functions}

Sometimes functions over polymorphic data structures only concern
themselves with the form of the structure.  For example, one might
write a function that takes the length of a list of numbers:

@racketmod[
typed/racket
(: list-number-length ((Listof Number) -> Integer))
(define (list-number-length l)
  (if (null? l)
      0
      (add1 (list-number-length (cdr l)))))]

and also a function that takes the length of a list of strings:

@racketmod[
typed/racket
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

@racketmod[
typed/racket
(: list-length (All (A) ((Listof A) -> Integer)))
(define (list-length l)
  (if (null? l)
      0
      (add1 (list-length (cdr l)))))]

The new type constructor @racket[All] takes a list of type
variables and a body type.  The type variables are allowed to
appear free in the body of the @racket[All] form.

@subsection{Lexically Scoped Type Variables}

When the @racket[:] type annotation form includes type variables
for @tech{parametric polymorphism},
the type variables are @emph{lexically scoped}.
In other words, the type variables are bound in the body of the
definition that you annotate.

For example, the following definition of @racket[_my-id] uses
the type variable @racket[_a] to annotate the argument
@racket[_x]:

@racketblock[
(: my-id (All (a) (a -> a)))
(define my-id (lambda: ([x : a]) x))
]

Lexical scope also implies that type variables can be shadowed,
such as in the following example:

@racketblock[
(: my-id (All (a) (a -> a)))
(define my-id
  (lambda: ([x : a])
    (: helper (All (a) (a -> a)))
    (define helper
      (lambda: ([y : a]) y))
    (helper x)))
]

The reference to @racket[_a] inside the inner @racket[lambda:]
refers to the type variable in @racket[_helper]'s annotation.
That @racket[_a] is @emph{not} the same as the @racket[_a]
in the annotation of the outer @racket[lambda:] expression.


@(close-eval the-eval)

@include-section["varargs.scrbl"]

@;@section{Refinement Types}
