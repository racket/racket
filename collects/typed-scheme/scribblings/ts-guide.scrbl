#lang scribble/manual

@begin[(require "utils.ss" (for-label (only-meta-in 0 typed/scheme)))]

@title[#:tag "top"]{@bold{Typed Scheme}: Scheme with Static Types}

@author["Sam Tobin-Hochstadt"]

@section-index["typechecking" "typechecker" "typecheck"]

Typed Scheme is a family of languages, each of which enforce
that programs written in the language obey a type system that ensures
the absence of many common errors.  This guide is intended for programmers familiar 
with PLT Scheme.  For an introduction to PLT Scheme, see the @(other-manual '(lib "scribblings/guide/guide.scrbl")).

@local-table-of-contents[]

@include-section["quick.scrbl"]
@include-section["begin.scrbl"]
@include-section["more.scrbl"]
@include-section["types.scrbl"]

@;@section{How the Type System Works}

@;@section{Integrating with Untyped Code}

@;{
@section{Starting with Typed Scheme}

If you already know PLT Scheme, or even some other Scheme, it should be
easy to start using Typed Scheme.

@subsection{A First Function}

The following program defines the Fibonacci function in PLT Scheme:

@schememod[
scheme
(define (fib n)
  (cond [(= 0 n) 1]
	[(= 1 n) 1]
	[else (+ (fib (- n 1)) (fib (- n 2)))]))
]

This program defines the same program using Typed Scheme.
 
@schememod[
typed-scheme
(: fib (Number -> Number))
(define (fib n)
  (cond [(= 0 n) 1]
        [(= 1 n) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))
]

There are two differences between these programs:

@itemize[
  @item*[@elem{The Language}]{@schememodname[scheme] has been replaced by @schememodname[typed-scheme].}

  @item*[@elem{The Type Annotation}]{We have added a type annotation
for the @scheme[fib] function, using the @scheme[:] form.}  ]

In general, these are most of the changes that have to be made to a
PLT Scheme program to transform it into a Typed Scheme program.
@margin-note{Changes to uses of @scheme[require] may also be necessary
- these are described later.}

@subsection[#:tag "complex"]{Adding more complexity}

Other typed binding forms are also available.  For example, we could have
rewritten our fibonacci program as follows:

@schememod[
typed-scheme
(: fib (Number -> Number))
(define (fib n)
  (let ([base? (or (= 0 n) (= 1 n))])
    (if base? 
	1
        (+ (fib (- n 1)) (fib (- n 2))))))
]

This program uses the @scheme[let] binding form, but no new type
annotations are required.  Typed Scheme infers the type of
@scheme[base?].  

We can also define mutually-recursive functions:

@schememod[
typed-scheme
(: my-odd? (Number -> Boolean))
(define (my-odd? n)
  (if (= 0 n) #f
      (my-even? (- n 1))))

(: my-even? (Number -> Boolean))
(define (my-even? n)
  (if (= 0 n) #t
      (my-odd? (- n 1))))

(my-even? 12)
]

As expected, this program prints @schemeresult[#t].


@subsection{Defining New Datatypes}

If our program requires anything more than atomic data, we must define
new datatypes.  In Typed Scheme, structures can be defined, similarly
to PLT Scheme structures.  The following program defines a date
structure and a function that formats a date as a string, using PLT
Scheme's built-in @scheme[format] function.

@schememod[
typed-scheme
(define-struct: Date ([day : Number] [month : String] [year : Number]))

(: format-date (Date -> String))
(define (format-date d)
  (format "Today is day ~a of ~a in the year ~a" 
          (Date-day d) (Date-month d) (Date-year d)))

(format-date (make-Date 28 "November" 2006))
]

Here we see the built-in type @scheme[String] as well as a definition
of the new user-defined type @scheme[Date].  To define
@scheme[Date], we provide all the information usually found in a
@scheme[define-struct], but added type annotations to the fields using
the @scheme[define-struct:] form.
Then we can use the functions that this declaration creates, just as
we would have with @scheme[define-struct].  


@subsection{Recursive Datatypes and Unions}

Many data structures involve multiple variants.  In Typed Scheme, we
represent these using @italic{union types}, written @scheme[(U t1 t2 ...)].

@schememod[
typed-scheme
(define-type-alias Tree (U leaf node))
(define-struct: leaf ([val : Number]))
(define-struct: node ([left : Tree] [right : Tree]))

(: tree-height (Tree -> Number))
(define (tree-height t)
  (cond [(leaf? t) 1]
        [else (max (+ 1 (tree-height (node-left t)))
                   (+ 1 (tree-height (node-right t))))]))

(: tree-sum (Tree -> Number))
(define (tree-sum t)
  (cond [(leaf? t) (leaf-val t)]
        [else (+ (tree-sum (node-left t))
                 (tree-sum (node-right t)))]))
]

In this module, we have defined two new datatypes: @scheme[leaf] and
@scheme[node].  We've also defined the type alias @scheme[Tree] to be
@scheme[(U node leaf)], which represents a binary tree of numbers.  In
essence, we are saying that the @scheme[tree-height] function accepts
a @scheme[Tree], which is either a @scheme[node] or a @scheme[leaf],
and produces a number.

In order to calculate interesting facts about trees, we have to take
them apart and get at their contents.  But since accessors such as
@scheme[node-left] require a @scheme[node] as input, not a
@scheme[Tree], we have to determine which kind of input we
were passed.  

For this purpose, we use the predicates that come with each defined
structure.  For example, the @scheme[leaf?] predicate distinguishes
@scheme[leaf]s from all other Typed Scheme values.  Therefore, in the
first branch of the @scheme[cond] clause in @scheme[tree-sum], we know
that @scheme[t] is a @scheme[leaf], and therefore we can get its value
with the @scheme[leaf-val] function.

In the else clauses of both functions, we know that @scheme[t] is not
a @scheme[leaf], and since the type of @scheme[t] was @scheme[Tree] by
process of elimination we can determine that @scheme[t] must be a
@scheme[node].  Therefore, we can use accessors such as
@scheme[node-left] and @scheme[node-right] with @scheme[t] as input.

@section[#:tag "poly"]{Polymorphism}

Typed Scheme offers abstraction over types as well as values.

@subsection{Polymorphic Data Structures}

Virtually every Scheme program uses lists and sexpressions.  Fortunately, Typed
Scheme can handle these as well.  A simple list processing program can be
written like this:

@schememod[
typed-scheme
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
typed-scheme
(define-struct: Nothing ())
(define-struct: (a) Just ([v : a]))

(define-type-alias (Maybe a) (U Nothing (Just a)))

(: find (Number (Listof Number) -> (Maybe Number)))
(define (find v l)
  (cond [(null? l) (make-Nothing)]
        [(= v (car l)) (make-Just v)]
        [else (find v (cdr l))]))
]

The first @scheme[define-struct:] defines @scheme[Nothing] to be
a structure with no contents.  

The second definition

@schemeblock[
(define-struct: (a) Just ([v : a]))
]

creates a parameterized type, @scheme[Just], which is a structure with
one element, whose type is that of the type argument to
@scheme[Just].  Here the type parameters (only one, @scheme[a], in
this case) are written before the type name, and can be referred to in
the types of the fields.

The type alias definiton
@schemeblock[
  (define-type-alias (Maybe a) (U Nothing (Just a)))
]
creates a parameterized alias --- @scheme[Maybe] is a potential
container for whatever type is supplied.

The @scheme[find] function takes a number @scheme[v] and list, and
produces @scheme[(make-Just v)] when the number is found in the list,
and @scheme[(make-Nothing)] otherwise.  Therefore, it produces a
@scheme[(Maybe Number)], just as the annotation specified.  

@subsection{Polymorphic Functions}

Sometimes functions over polymorphic data structures only concern
themselves with the form of the structure.  For example, one might
write a function that takes the length of a list of numbers:

@schememod[
typed-scheme
(: list-number-length ((Listof Number) -> Integer))
(define (list-number-length l)
  (if (null? l)
      0
      (add1 (list-number-length (cdr l)))))]

and also a function that takes the length of a list of strings:

@schememod[
typed-scheme
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
typed-scheme
(: list-length (All (A) ((Listof A) -> Integer)))
(define (list-length l)
  (if (null? l)
      0
      (add1 (list-length (cdr l)))))]

The new type constructor @scheme[All] takes a list of type
variables and a body type.  The type variables are allowed to
appear free in the body of the @scheme[All] form.

}