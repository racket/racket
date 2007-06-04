#reader(lib "docreader.ss" "scribble")
@require[(lib "struct.ss" "scribble")]
@require-for-syntax[mzscheme]
@require["mz.ss"]
@require["prog-steps.ss"]

@define[reduces (make-element #f (list 'rarr))]
@define[rspace (make-element "ghost" (list 'rarr))]

@define[*redex (lambda (c)
                 (make-element "highlighted" (list c)))]
@define-syntax[redex (syntax-rules ()
                       [(_ a) (*redex (scheme a))])]


@define[hole (make-element #f (list "[]"))]
@define[(*sub c e) (make-element #f (list c "[" e "]"))]
@define[langle (make-element 'tt (list "<"))]
@define[rangle (make-element 'tt (list ">"))]
@define[comma (make-element 'tt (list ", "))]
@define-syntax[sub (syntax-rules ()
                     [(_ a b) (*sub (scheme a) (scheme b))])]
@define[(*state c e) (make-element #f (list langle c comma e rangle))]
@define-syntax[state (syntax-rules ()
                       [(_ a b) (*state (scheme a) (scheme b))])]

@;------------------------------------------------------------------------
@title{Language Model}

Scheme evaluation can be viewed as the simplification of expressions
to obtain values. For example, just as an elementary-school student
simplifies

@verbatim{  1 + 1 = 2}

Scheme evaluation simplifies

@schemeblock[
(+ 1 1) #, @reduces 2
]

The arrow @reduces above replaces the more traditional @tt{=} to
emphasize that evaluation proceeds in a particular direction towards
simplier expressions. In particular, a @defterm{value} is an
expression that evaluation simplifies no further, such as the number
@scheme[2].

@;------------------------------------------------------------------------
@section{Sub-expression Evaluation}

Some simplifications require more than one step. For example:

@schemeblock[
(- 4 #,(redex (+ 1 1))) #,reduces #,(redex (- 4 2)) #,reduces 2
]

An expression that is not a value can always be partitioned into two
parts: a @defterm{redex}, which is the part that changed in a
single-step simplification (show in blue), and the
@defterm{continuation}, which is the surrounding expression
context. In @scheme[(- 4 (+ 1 1))], the redex is @scheme[(+ 1 1)], and
the continuation is @scheme[(- 4 #, @hole)], where @hole takes the
place of the redex. That is, the continuation says how to ``continue''
after the redex is reduced to a value.

Before some things can be evaluated, some sub-expressions must be
evaluated; for example, in the application @scheme[(- 4 (+ 1 1))], the
application of @scheme[-] cannot be reduced until the sub-expression
@scheme[(+ 1 1)] is reduced.

Thus, the specification of each syntactic form specifies how (some of)
it's sub-expressions are evaluated, and then how the results are
combined to reduce the form away.

The @defterm{dynamic extent} of an expression is the sequence of
evaluation steps during which an expression contains the redex.

@;------------------------------------------------------------------------
@section{Tail Position}

An expression @scheme[_expr1] is in @defterm{tail position} with
respect to an enclosing expression @scheme[_expr2] if, whenever
@scheme[_expr1] becomes a redex, its continuation is the same as was
the enclosing @scheme[_expr2]'s continuation.

For example, the @scheme[(+ 1 1)] expression is @italic{not} in tail
position with respect to @scheme[(- 4 (+ 1 1))]. To illustrate, we use
the notation @sub[_C _expr] to mean the expression that is produced by
substituing @scheme[_expr] in place of @hole in the continuation
@scheme[_C]:

@schemeblock[
#, @sub[_C (- 4 (+ 1 1))] #, @reduces #, @sub[_C (- 4 2)]
]

In this case, the continuation for reducing @scheme[(+ 1 1)] is @sub[_C (+
4 #, @hole)], not just @scheme[_C].

In contrast, @scheme[(+ 1 1)] is in tail position with respect to
@scheme[(if (zero? 0) (+ 1 1) 3)], because, for any continuation @scheme[_C],

@schemeblock[
#, @sub[_C (if (zero? 0) (+ 1 1) 3)] #, @reduces #, @sub[_C (if #t (+ 1 1) 3)] #, @reduces #, @sub[_C (+ 1 1)]
]

The steps in this reduction sequence are driven by the definition of
@scheme[if], and they do not depend on the continuation
@scheme[_C]. The ``then'' branch of an @scheme[if] form is always in
tail position with respect to the @scheme[if] form. Due to a similar
reduction rule for @scheme[if] and @scheme[#f], the ``else'' branch of
an @scheme[if] form is also in tail position.

Tail-position specifications provide a guarantee about the asymtotic
space consumption of a computation. In general, the specification of
tail positions goes with each syntactic form, like @scheme[if].

@;------------------------------------------------------------------------
@section{Multiple Return Values}

A Scheme expression can evaluate to @defterm{multiple values}, in the
same way that a procedure can accept multiple arguments.

Most continuations expect a particular number of result values.
Indeed, most continuations, such as @scheme[(+ #, @hole 1)] expect a
single value. The continuation @scheme[(let-values ([(x y) #, @hole])
_expr)] expects two result values; the first result replaces
@scheme[x] in the body @scheme[_expr], and the second replaces
@scheme[y] in @scheme[_expr]. The continuation @scheme[(begin #, @hole
(+ 1 2))] accepts any number of result values, because it ignores the
result(s).

In general, the specification of a syntactic form inidicates the
number of values that it produces and the number that it expects from
each of its sub-expression. In addtion, some procedures (notably
@scheme[values]) produce multiple values, and some procedures (notably
@scheme[call-with-values]) create continuations internally that accept
a certain number of values.

@;------------------------------------------------------------------------
@section{Top-level and Module Bindings}

Given

@verbatim{  x = 10}

then an algebra student simplifies @tt{x + 1} as follows:

@verbatim{  x + 1 = 10 + 1 = 11}

Scheme works much the same way, in that a set of top-level bindings
are available for substitutions on demand during evaluation. For
example, given

@schemeblock[
(define x 10)
]

then

@schemeblock[
#,(redex (+ x 1)) #,reduces #,(redex (+ 10 1)) #,reduces 11
]

In Scheme, the way definitions appear is just as important as the way
that they are used. Scheme evaluation thus keeps track of both
definitions and the current expression, and it extends the set of
definitions in response to evaluating forms such as @scheme[define].

Each evaluation step, then, takes the current set of definitions and
program to a new set of definitions and program. Before a
@scheme[define] can be moved into the set of definitions, its
right-hand expression must be reduced to a value.

@prog-steps/no-obj[
[{}
 (begin (define x (code:hilite (+ 9 1))) (+ x 1))]
[{}
 (begin (code:hilite (define x 10)) (+ x 1))]
[{(define x 10)}
 (code:hilite (begin #,(void-const) (+ x 1)))]
[{(define x 10)}
 (+ (code:hilite x) 1)]
[{(define x 10)}
 (code:hilite (+ 10 1))]
[{(define x 10)}
 11]
]

Most definitions in PLT Scheme are in modules. In terms of evaluation,
a module is simply a prefix on a defined name, so that different
modules can define the name.

Using @scheme[set!], a program can change the value associated with an
existing top-level binding:

@prog-steps/no-obj[
[{(define x 10)}
 (begin (code:hilite (set! x 8)) x)]
[{(define x 8)}
 (code:hilite (begin #,(void-const) x))]
[{(define x 8)}
 (code:hilite x)]
[{(define x 8)}
 8]
]

@;------------------------------------------------------------------------
@section{Objects and Imperative Update}

In addition to @scheme[set!] for imperative update of top-level
bindings, various procedures enable the modification of elements
within a compound data structure. For example, @scheme[vector-set!]
modifies the content of a vector.

To allow such modifications to data, we must distingiush between
values, which are the results of expressions, and @defterm{objects},
which hold the data referenced by a value.

A few kinds of objects can serve directly as values, including
booleans, @void-const[], and small exact integers. More generally,
however, a value is a reference to an object. For example, a value can
be a reference to a particular vector that currently holds the value
@scheme[10] in its first slot. If an object is modified, then the
modification is visible through all copies of the value that reference
the same object.

In the evaluation model, a set of objects must be carried along with
each step in evaluation, just like the definition set. Operations that
create objects, such as @scheme[vector], add to the set of objects:

@prog-steps[
[{}
 {}
 (begin (define x (code:hilite (vector 10 20)))
        (define y x)
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> #(10 20))}
 {}
 (begin (code:hilite (define x <o1>))
        (define y x)
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> #(10 20))}
 {(define x <o1>)}
 (code:hilite (begin #,(void-const)
                     (define y x)
                     (vector-set! x 0 11)
                     (vector-ref y 0)))]
[{(define <o1> #(10 20))}
 {(define x <o1>)}
 (begin (define y (code:hilite x))
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> #(10 20))}
 {(define x <o1>)}
 (begin (code:hilite (define y <o1>))
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> #(10 20))}
 {(define x <o1>)
  (define y <o1>)}
 (code:hilite (begin #,(void-const)
                     (vector-set! x 0 11)
                     (vector-ref y 0)))]
[{(define <o1> #(10 20))}
 {(define x <o1>)
  (define y <o1>)}
 (begin (vector-set! (code:hilite x) 0 11)
        (vector-ref y 0))]
[{(define <o1> #(10 20))}
 {(define x <o1>)
  (define y <o1>)}
 (begin (code:hilite (vector-set! <o1> 0 11))
        (vector-ref y 0))]
[{(define <o1> #(11 20))}
 {(define x <o1>)
  (define y <o1>)}
 (code:hilite (begin #,(void-const)
                     (vector-ref y 0)))]
[{(define <o1> #(11 20))}
 {(define x <o1>)
  (define y <o1>)}
 (vector-ref (code:hilite y) 0)]
[{(define <o1> #(11 20))}
 {(define x <o1>)
  (define y <o1>)}
 (code:hilite (vector-ref <o1> 0))]
[{(define <o1> #(11 20))}
 {(define x <o1>)
  (define y <o1>)}
 11]
]

The distinction between a top-level binding is an object reference is
crucial. A top-level binding is not a value; each time a binding
expression is evaluated, the value is extracted from the current set
of definitions. An object reference, in contrast is a value, and
therefore needs no further evaluation. The model evaluation steps
above use angle-bracketed @scheme[<o1>] for an object reference to
distinguish it from a variable name.

A direct object reference can never appear in a text-based source
program. A program representation created with
@scheme[datum->syntax-object], however, can embed direct references to
existing objects.

@;------------------------------------------------------------------------
@section{Object Identity and Comparisons}

The @scheme[eq?] operator compares two values, returning @scheme[#t]
when the values refer to the same object. This form of equality is
suitabel for comparing objects that support imperative update (e.g.,
to determine that the effect of modifying an object through one
reference is visible through another reference). Also, an @scheme[eq?]
test evaluates quickly, and @scheme[eq?]-based hashing is more
lightweight than @scheme[equal?]-based hashing in hash tables.

In some cases, however, @scheme[eq?] is unsuitable as a comparison
operator, because the generation of objects is not clearly defined. In
particular, two applications of @scheme[+] to the same two exact
integers may or may not produce results that are @scheme[eq?],
although the results are always @scheme[equal?]. Similarly, evaluation
of a @scheme[lambda] form typically generates a new procedure object,
but it may re-use a procedure object previously generated by the same
source @scheme[lambda] form.

The behavior of a datatype with respect to @scheme[eq?] is generally
specified with the datatype and its associated procedures.

@;------------------------------------------------------------------------
@section{Garbage Collection}

In the program state

@prog-steps[
[{(define <o1> #(10 20))
  (define <o2> #(0))}
 {(define x <o1>)}
 (+ 1 x)]
]

evaluation cannot depend on @scheme[<o2>], because it is not part of
the program to evaluate, and it is not referenced by any definition
that is accessible in the program. The object @scheme[<o2>] may
therefore be removed from the evaluation by @defterm{garbage
collection}.

A few special compound datatypes hold @defterm{weak references} to
objects. Such weak references are treated specialy by the garbage
collector in determining which objects are reachable for the remainder
of the computation. If an object is reachable only via a weak
reference, then the object can be reclaimed, and the weak reference is
replaced by a different value (typically @scheme[#f]).

@;------------------------------------------------------------------------
@section{Procedure Applications and Local Bindings}

Given

@verbatim{  f(x) = x + 10}

then an algebra student simplifies @tt{f(1)} as follows:

@verbatim{  f(7) = 7 + 10 = 17}

The key step in this simplification is take the body of the defined
function @tt{f}, and then replace each @tt{x} with the actual value
@tt{1}.

Scheme procedure application works much the same way. A procedure is
an object, so evaluating @scheme[(f 7)] starts with a variable lookup:

@prog-steps[
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)}
 ((code:hilite f) 7)]
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)}
 (code:hilite (<p1> 7))]
]

Unlike in algebra, however, the value associated with an argument can
be changed in the body of a procedure by using @scheme[set!], as in
the example @scheme[(lambda (x) (begin (set! x 3) x))]. Since the value
associated with @scheme[x] can be changed, an actual value for cannot
be substituted for @scheme[x] when the procedure is applied.

Instead, a new @defterm{location} is created for each variable on each
application. The argument value is placed in the location, and each
insteace of the variable in the procedure body is replaced with the
new location:

@prog-steps[
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)}
 (code:hilite (<p1> 7))]
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)
  (define xloc 7)}
 (+ (code:hilite xloc) 10)]
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)
  (define xloc 7)}
 (code:hilite (+ 7 10))]
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)
  (define xloc 7)}
 17]
]

A location is the same as a top-level binding, but when a location is
generated, it (conceptually) uses a name that has not been used before
and that cannot not be generated again or accessed directly.

Generating a location in this way means that @scheme[set!] evaluates
for local variables in the same way as for top-level bindings, because
the variable is always replaced with a location by the time the
@scheme[set!] form is evaluated:

@prog-steps[
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)}
 ((code:hilite f) 7)]
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)}
 (code:hilite (<p1> 7))]
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)
  (define xloc 7)}
 (begin (code:hilite (set! xloc 3)) xloc)]
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)
  (define xloc 3)}
 (code:hilite (begin #,(void-const) xloc))]
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)
  (define xloc 3)}
 (code:hilite xloc)]
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)
  (define xloc 3)}
 3]
]

The substition and location-generation step of procedure application
requires that the argument is a value. Therefore, in @scheme[((lambda
(x) (+ x 10)) (+ 1 2))], the @scheme[(+ 1 2)] sub-expression must be
simplified to the value @scheme[3], and then @scheme[3] can be placed
into a location for @scheme[x]. In other words, Scheme is a
@defterm{call-by-value} language.

Evaluation of a local binding, such as @scheme[(let ([x (+ 1 2)])
_expr)], is the same as for a procedure call. After @scheme[(+ 1 2)]
produces a value, it is stored in a fresh location that replaces every
instance of @scheme[x] in @scheme[_expr].

@;------------------------------------------------------------------------
@section{Identifiers, Variables, and Locations}

A @defterm{variable} is a placeholder for a value, and an expressions
in an initial program refer to variables. A top-level binding is both
a variable and a location. Any other variable is always replaced by a
location at run-time, so that evaluation of expressions involves only
locations. A single non-top-level variable, such as a procedure
argument, can correspond to multiple locations at different times.

The replacement of a variable with a location during evaluation
implements Scheme's @defterm{lexical scoping}. For example, when the
procedure-argument variable @scheme[x] is replaced by the location
@scheme[xloc], then it is replaced throughout the body of the
procedure, including with any nested @scheme[lambda] forms. As a
result, future references of the variable always access the same
location.

An @defterm{identifier} is source-program entity. Parsing a Scheme
program reveals that some identifiers correspond to variables, some
refer to syntactic forms, and some are quoted to produce a symbol or a
syntax object.

Throughout the documentation, identifiers are typeset to suggest the
way that they are parsed. A black, boldface identifier like
@scheme[lambda] indicates as a reference to a syntactic form. A plain
blue identifer like @schemeidfont{x} is a variable or a reference to
an unspecified top-level definition. A hyperlinked identifier
@scheme[cons] is a reference to a specific top-level definition.

@;------------------------------------------------------------------------
@section{Parsing and Compilation}

The syntax of a Scheme program is defined by

@itemize{

 @item{a @defterm{read} phase that processes a character stream into a
       Scheme value, especially one composed of pairs and symbols,
       and}

 @item{an @defterm{expand} phase that processes the value to finish
       parsing the code.}

}

For details on the read phase, see @secref["mz:reader"]. Source code is
normally read in @scheme[read-syntax] mode, otherwise it must be
converted to syntax using @scheme[datum->syntax-object]; the expand
phase is defined in terms of syntax objects.

Expansion recursively processes a syntax-wrapped datum; for details,
see @secref["mz:expansion"]. Ultimately, expansion leads to the
synactic forms described in @secref["mz:syntax"].

...

@;------------------------------------------------------------------------
@section{Namespaces}


@;------------------------------------------------------------------------
@section{Threads}

