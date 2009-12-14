#lang scribble/doc

@begin[(require scribble/manual)
       (require (for-label typed-scheme))]

@begin[
(define (item* header . args) (apply item @bold[header]{: } args))
(define-syntax-rule (tmod forms ...) (schememod typed-scheme forms ...))
(define (gtech . x)  (apply tech x #:doc '(lib "scribblings/guide/guide.scrbl")))
(define (rtech . x)  (apply tech x #:doc '(lib "scribblings/reference/reference.scrbl")))
]

@title[#:tag "top"]{@bold{Typed Scheme}: Scheme with Static Types}

@author["Sam Tobin-Hochstadt"]

@section-index["typechecking"]

Typed Scheme is a Scheme-like language, with a type system that
supports common Scheme programming idioms.  Explicit type declarations
are required --- that is, there is no type inference.  The language
supports a number of features from previous work on type systems that
make it easier to type Scheme programs, as well as a novel idea dubbed
@italic{occurrence typing} for case discrimination.

Typed Scheme is also designed to integrate with the rest of your PLT
Scheme system.  It is possible to convert a single module to Typed
Scheme, while leaving the rest of the program unchanged.  The typed
module is protected from the untyped code base via
automatically-synthesized contracts.

Further information on Typed Scheme is available from
@link["http://www.ccs.neu.edu/home/samth/typed-scheme"]{the homepage}.

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

@section{Polymorphism}

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

@section{Variable-Arity Functions: Programming with Rest Arguments}

Typed Scheme can handle some uses of rest arguments.

@subsection{Uniform Variable-Arity Functions}

In Scheme, one can write a function that takes an arbitrary
number of arguments as follows:

@schememod[
scheme
(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))

(sum)
(sum 1 2 3 4)
(sum 1 3)]

The arguments to the function that are in excess to the
non-rest arguments are converted to a list which is assigned
to the rest parameter.  So the examples above evaluate to
@schemeresult[0], @schemeresult[10], and @schemeresult[4].

We can define such functions in Typed Scheme as well:

@schememod[
typed-scheme
(: sum (Number * -> Number))
(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))]

This type can be assigned to the function when each element
of the rest parameter is used at the same type.

@subsection{Non-Uniform Variable-Arity Functions}

However, the rest argument may be used as a heterogeneous list.
Take this (simplified) definition of the Scheme function @scheme[map]:

@schememod[
scheme
(define (map f as . bss)
  (if (or (null? as)
          (ormap null? bss))
      null
      (cons (apply f (car as) (map car bss))
            (apply map f (cdr as) (map cdr bss)))))

(map add1 (list 1 2 3 4))
(map cons (list 1 2 3) (list (list 4) (list 5) (list 6)))
(map + (list 1 2 3) (list 2 3 4) (list 3 4 5) (list 4 5 6))]

Here the different lists that make up the rest argument @scheme[bss]
can be of different types, but the type of each list in @scheme[bss]
corresponds to the type of the corresponding argument of @scheme[f].
We also know that, in order to avoid arity errors, the length of
@scheme[bss] must be one less than the arity of @scheme[f] (as
@scheme[as] corresponds to the first argument of @scheme[f]).
                                                            
The example uses of @scheme[map] evaluate to @schemeresult[(list 2 3 4 5)],
@schemeresult[(list (list 1 4) (list 2 5) (list 3 6))], and
@schemeresult[(list 10 14 18)].

In Typed Scheme, we can define @scheme[map] as follows:

@schememod[
typed-scheme
(: map 
   (All (C A B ...)
        ((A B ... B -> C) (Listof A) (Listof B) ... B
         ->
         (Listof C))))
(define (map f as . bss)
  (if (or (null? as)
          (ormap null? bss))
      null
      (cons (apply f (car as) (map car bss))
            (apply map f (cdr as) (map cdr bss)))))]

Note that the type variable @scheme[B] is followed by an
ellipsis.  This denotes that B is a dotted type variable
which corresponds to a list of types, much as a rest
argument corresponds to a list of values.  When the type
of @scheme[map] is instantiated at a list of types, then
each type @scheme[t] which is bound by @scheme[B] (notated by
the dotted pre-type @scheme[t ... B]) is expanded to a number
of copies of @scheme[t] equal to the length of the sequence
assigned to @scheme[B].  Then @scheme[B] in each copy is
replaced with the corresponding type from the sequence.

So the type of @scheme[(inst map Integer Boolean String Number)]
is

@scheme[((Boolean String Number -> Integer)
         (Listof Boolean) (Listof String) (Listof Number)
         ->
         (Listof Integer))].
