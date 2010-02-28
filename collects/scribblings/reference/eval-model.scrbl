#lang scribble/doc
@(require scribble/struct
          scribble/scheme
          (for-syntax scheme/base)
          "mz.ss"
          "prog-steps.ss")

@(define reduces (make-element #f (list 'rarr)))
@(define rspace (make-element "ghost" (list 'rarr)))

@(define *redex (lambda (c)
                  (make-element highlighted-color (list c))))
@(define-syntax redex
   (syntax-rules () [(_ a) (*redex (scheme a))]))


@(define hole (make-element #f (list "[]")))
@(define (*sub c e) (make-element #f (list c "[" e "]")))
@(define-syntax sub
   (syntax-rules () [(_ a b) (*sub (scheme a) (scheme b))]))
@(define (frame n)
   (make-element variable-color
                 (list "C" (make-element 'subscript (list (format "~a" n))))))
@;{
These are not used; if they do get back in, then it's probably better
to switch to the usual langle/rangle that is used in syntax definitions.
@(define langle (make-element 'tt (list "<")))
@(define rangle (make-element 'tt (list ">")))
@(define comma (make-element 'tt (list ", ")))
@(define (*state c e) (make-element #f (list langle c comma e rangle)))
@(define-syntax state
   (syntax-rules () [(_ a b) (*state (scheme a) (scheme b))]))
;}

@;------------------------------------------------------------------------
@title[#:tag "eval-model"]{Evaluation Model}

Scheme evaluation can be viewed as the simplification of expressions
to obtain values. For example, just as an elementary-school student
simplifies

@verbatim{  1 + 1 = 2}

Scheme evaluation simplifies

@schemeblock[
(+ 1 1) @#,reduces 2
]

The arrow @reduces above replaces the more traditional @tt{=} to
emphasize that evaluation proceeds in a particular direction towards
simpler expressions. In particular, a @deftech{value} is an
expression that evaluation simplifies no further, such as the number
@scheme[2].

@;------------------------------------------------------------------------
@section[#:tag "cont-model"]{Sub-expression Evaluation and Continuations}

Some simplifications require more than one step. For example:

@schemeblock[
(- 4 #,(redex (+ 1 1))) #,reduces #,(redex (- 4 2)) #,reduces 2
]

An expression that is not a @tech{value} can always be partitioned
into two parts: a @deftech{redex}, which is the part that changed in a
single-step simplification (highlighted), and the
@deftech{continuation}, which is the surrounding expression
context. In @scheme[(- 4 (+ 1 1))], the redex is @scheme[(+ 1 1)], and
the continuation is @scheme[(- 4 @#,hole)], where @hole takes the
place of the redex. That is, the continuation says how to ``continue''
after the @tech{redex} is reduced to a @tech{value}.

Before some things can be evaluated, some sub-expressions must be
evaluated; for example, in the application @scheme[(- 4 (+ 1 1))], the
application of @scheme[-] cannot be reduced until the sub-expression
@scheme[(+ 1 1)] is reduced.

Thus, the specification of each syntactic form specifies how (some of)
its sub-expressions are evaluated, and then how the results are
combined to reduce the form away.

The @deftech{dynamic extent} of an expression is the sequence of
evaluation steps during which an expression contains the @tech{redex}.

@;------------------------------------------------------------------------
@section{Tail Position}

An expression @scheme[_expr1] is in @deftech{tail position} with
respect to an enclosing expression @scheme[_expr2] if, whenever
@scheme[_expr1] becomes a redex, its @tech{continuation} is the same
as was the enclosing @scheme[_expr2]'s @tech{continuation}.

For example, the @scheme[(+ 1 1)] expression is @italic{not} in @tech{tail
position} with respect to @scheme[(- 4 (+ 1 1))]. To illustrate, we use
the notation @sub[_C _expr] to mean the expression that is produced by
substituting @scheme[_expr] in place of @hole in the @tech{continuation}
@scheme[_C]:

@schemeblock[
@#,sub[_C (- 4 (+ 1 1))] @#,reduces @#,sub[_C (- 4 2)]
]

In this case, the @tech{continuation} for reducing @scheme[(+ 1 1)] is
@sub[_C (+ 4 @#,hole)], not just @scheme[_C].

In contrast, @scheme[(+ 1 1)] is in @tech{tail position} with respect
to @scheme[(if (zero? 0) (+ 1 1) 3)], because, for any continuation
@scheme[_C],

@schemeblock[
@#,sub[_C (if (zero? 0) (+ 1 1) 3)] @#,reduces @#,sub[_C (if #t (+ 1 1) 3)] @#,reduces @#,sub[_C (+ 1 1)]
]

The steps in this reduction sequence are driven by the definition of
@scheme[if], and they do not depend on the @tech{continuation}
@scheme[_C]. The ``then'' branch of an @scheme[if] form is always in
@tech{tail position} with respect to the @scheme[if] form. Due to a
similar reduction rule for @scheme[if] and @scheme[#f], the ``else''
branch of an @scheme[if] form is also in @tech{tail position}.

@tech{Tail-position} specifications provide a guarantee about the
asymptotic space consumption of a computation. In general, the
specification of @tech{tail positions} goes with each syntactic form,
like @scheme[if].

@;------------------------------------------------------------------------
@section[#:tag "values-model"]{Multiple Return Values}

A Scheme expression can evaluate to @deftech{multiple values}, in the
same way that a procedure can accept multiple arguments.

Most @tech{continuations} expect a particular number of result
@tech{values}.  Indeed, most @tech{continuations}, such as @scheme[(+
@#,hole 1)] expect a single @tech{value}. The @tech{continuation}
@scheme[(let-values ([(x y) @#,hole]) _expr)] expects two result
@tech{values}; the first result replaces @scheme[x] in the body
@scheme[_expr], and the second replaces @scheme[y] in
@scheme[_expr]. The @tech{continuation} @scheme[(begin @#,hole (+ 1
2))] accepts any number of result @tech{values}, because it ignores
the result(s).

In general, the specification of a syntactic form inidicates the
number of @tech{values} that it produces and the number that it
expects from each of its sub-expression. In addtion, some procedures
(notably @scheme[values]) produce multiple @tech{values}, and some
procedures (notably @scheme[call-with-values]) create continuations
internally that accept a certain number of @tech{values}.

@;------------------------------------------------------------------------
@section{Top-Level Variables}

Given

@verbatim{  x = 10}

then an algebra student simplifies @tt{x + 1} as follows:

@verbatim{  x + 1 = 10 + 1 = 11}

Scheme works much the same way, in that a set of @tech{top-level
variables} are available for substitutions on demand during
evaluation. For example, given

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
right-hand expression must be reduced to a @tech{value}.

@prog-steps/no-obj[
[{}
 (begin (define x (code:hilite (+ 9 1))) (+ x 1))]
[{}
 (begin (code:hilite (define x 10)) (+ x 1))]
[{(define x 10)}
 (code:hilite (begin (void) (+ x 1)))]
[{(define x 10)}
 (+ (code:hilite x) 1)]
[{(define x 10)}
 (code:hilite (+ 10 1))]
[{(define x 10)}
 11]
]

Using @scheme[set!], a program can change the value associated with an
existing @tech{top-level variable}:

@prog-steps/no-obj[
[{(define x 10)}
 (begin (code:hilite (set! x 8)) x)]
[{(define x 8)}
 (code:hilite (begin (void) x))]
[{(define x 8)}
 (code:hilite x)]
[{(define x 8)}
 8]
]

@;------------------------------------------------------------------------
@section{Objects and Imperative Update}

In addition to @scheme[set!] for imperative update of @tech{top-level
variables}, various procedures enable the modification of elements
within a compound data structure. For example, @scheme[vector-set!]
modifies the content of a vector.

To allow such modifications to data, we must distingiush between
@tech{values}, which are the results of expressions, and
@deftech{objects}, which hold the data referenced by a value.

A few kinds of @tech{objects} can serve directly as values, including
booleans, @scheme[(void)], and small exact integers. More generally,
however, a @tech{value} is a reference to an @tech{object}. For
example, a @tech{value} can be a reference to a particular vector that
currently holds the value @scheme[10] in its first slot. If an
@tech{object} is modified, then the modification is visible through
all copies of the @tech{value} that reference the same @tech{object}.

In the evaluation model, a set of @tech{objects} must be carried along
with each step in evaluation, just like the definition set. Operations
that create @tech{objects}, such as @scheme[vector], add to the set of
@tech{objects}:

@prog-steps[
[{}
 {}
 (begin (define x (code:hilite (vector 10 20)))
        (define y x)
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> (vector 10 20))}
 {}
 (begin (code:hilite (define x <o1>))
        (define y x)
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> (vector 10 20))}
 {(define x <o1>)}
 (code:hilite (begin (void)
                     (define y x)
                     (vector-set! x 0 11)
                     (vector-ref y 0)))]
[{(define <o1> (vector 10 20))}
 {(define x <o1>)}
 (begin (define y (code:hilite x))
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> (vector 10 20))}
 {(define x <o1>)}
 (begin (code:hilite (define y <o1>))
        (vector-set! x 0 11)
        (vector-ref y 0))]
[{(define <o1> (vector 10 20))}
 {(define x <o1>)
  (define y <o1>)}
 (code:hilite (begin (void)
                     (vector-set! x 0 11)
                     (vector-ref y 0)))]
[{(define <o1> (vector 10 20))}
 {(define x <o1>)
  (define y <o1>)}
 (begin (vector-set! (code:hilite x) 0 11)
        (vector-ref y 0))]
[{(define <o1> (vector 10 20))}
 {(define x <o1>)
  (define y <o1>)}
 (begin (code:hilite (vector-set! <o1> 0 11))
        (vector-ref y 0))]
[{(define <o1> (vector 11 20))}
 {(define x <o1>)
  (define y <o1>)}
 (code:hilite (begin (void)
                     (vector-ref y 0)))]
[{(define <o1> (vector 11 20))}
 {(define x <o1>)
  (define y <o1>)}
 (vector-ref (code:hilite y) 0)]
[{(define <o1> (vector 11 20))}
 {(define x <o1>)
  (define y <o1>)}
 (code:hilite (vector-ref <o1> 0))]
[{(define <o1> (vector 11 20))}
 {(define x <o1>)
  (define y <o1>)}
 11]
]

The distinction between a @tech{top-level variable} and an object
reference is crucial. A @tech{top-level variable} is not a
@tech{value}; each time a @tech{variable} expression is evaluated, the
value is extracted from the current set of definitions. An object
reference, in contrast is a value, and therefore needs no further
evaluation. The model evaluation steps above use angle-bracketed
@scheme[<o1>] for an object reference to distinguish it from a
@tech{variable} name.

A direct object reference can never appear in a text-based source
program. A program representation created with
@scheme[datum->syntax-object], however, can embed direct references to
existing @tech{objects}.

@;------------------------------------------------------------------------
@section[#:tag "model-eq"]{Object Identity and Comparisons}

The @scheme[eq?] operator compares two @tech{values}, returning
@scheme[#t] when the values refer to the same @tech{object}. This form
of equality is suitable for comparing objects that support imperative
update (e.g., to determine that the effect of modifying an object
through one reference is visible through another reference). Also, an
@scheme[eq?]  test evaluates quickly, and @scheme[eq?]-based hashing
is more lightweight than @scheme[equal?]-based hashing in hash tables.

In some cases, however, @scheme[eq?] is unsuitable as a comparison
operator, because the generation of @tech{objects} is not clearly
defined. In particular, two applications of @scheme[+] to the same two
exact integers may or may not produce results that are @scheme[eq?],
although the results are always @scheme[equal?]. Similarly, evaluation
of a @scheme[lambda] form typically generates a new procedure
@tech{object}, but it may re-use a procedure @tech{object} previously
generated by the same source @scheme[lambda] form.

The behavior of a datatype with respect to @scheme[eq?] is generally
specified with the datatype and its associated procedures.

@;------------------------------------------------------------------------
@section[#:tag "gc-model"]{Garbage Collection}

@margin-note/ref{See @secref["memory"] for functions related to
garbage collection.}

In the program state

@prog-steps[
[{(define <o1> (vector 10 20))
  (define <o2> (vector 0))}
 {(define x <o1>)}
 (+ 1 x)]
]

evaluation cannot depend on @scheme[<o2>], because it is not part of
the program to evaluate, and it is not referenced by any definition
that is accessible in the program. The @tech{object} @scheme[<o2>] may
therefore be removed from the evaluation by @deftech{garbage
collection}.

A few special compound datatypes hold @deftech{weak references} to
objects. Such weak references are treated specially by the garbage
collector in determining which @tech{objects} are reachable for the
remainder of the computation. If an @tech{object} is reachable only
via a @tech{weak reference}, then the object can be reclaimed, and the
@tech{weak reference} is replaced by a different @tech{value}
(typically @scheme[#f]).

As a special case, a @tech{fixnum} is always considered reachable by
the garbage collector. Many other values are always reachable due to
the way they are implemented and used: A @tech{character} in the
Latin-1 range is always reachable, because @scheme[equal?] Latin-1
characters are always @scheme[eq?], and all of the Latin-1 characters
are referenced by an internal module. Similarly, @scheme[null],
@scheme[#t], @scheme[#f], @scheme[eof], and @|void-const| and are
always reachable. Values produced by @scheme[quote] remain reachable
when the @scheme[quote] expression itself is reachable.

@;------------------------------------------------------------------------
@section{Procedure Applications and Local Variables}

Given

@verbatim{  f(x) = x + 10}

then an algebra student simplifies @tt{f(1)} as follows:

@verbatim{  f(7) = 7 + 10 = 17}

The key step in this simplification is take the body of the defined
function @tt{f}, and then replace each @tt{x} with the actual
@tech{value} @tt{1}.

Scheme procedure application works much the same way. A procedure is
an @tech{object}, so evaluating @scheme[(f 7)] starts with a
@tech{variable} lookup:

@prog-steps[
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)}
 ((code:hilite f) 7)]
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)}
 (code:hilite (<p1> 7))]
]

Unlike in algebra, however, the @tech{value} associated with an
argument can be changed in the body of a procedure by using
@scheme[set!], as in the example @scheme[(lambda (x) (begin (set! x 3)
x))]. Since the @tech{value} associated with @scheme[x] can be
changed, an actual value for cannot be substituted for @scheme[x] when
the procedure is applied.

Instead, a new @deftech{location} is created for each @tech{variable}
on each application. The argument @tech{value} is placed in the
@tech{location}, and each instance of the @tech{variable} in the
procedure body is replaced with the new @tech{location}:

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

A @tech{location} is the same as a @tech{top-level variable}, but when
a @tech{location} is generated, it (conceptually) uses a name that has
not been used before and that cannot not be generated again or
accessed directly.

Generating a @tech{location} in this way means that @scheme[set!]
evaluates for @tech{local variables} in the same way as for
@tech{top-level variables}, because the @tech{local variable} is
always replaced with a @tech{location} by the time the @scheme[set!]
form is evaluated:

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
 (code:hilite (begin (void) xloc))]
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)
  (define xloc 3)}
 (code:hilite xloc)]
[{(define <p1> (lambda (x) (begin (set! x 3) x)))}
 {(define f <p1>)
  (define xloc 3)}
 3]
]

The substitution and @tech{location}-generation step of procedure
application requires that the argument is a @tech{value}. Therefore,
in @scheme[((lambda (x) (+ x 10)) (+ 1 2))], the @scheme[(+ 1 2)]
sub-expression must be simplified to the @tech{value} @scheme[3], and
then @scheme[3] can be placed into a @tech{location} for
@scheme[x]. In other words, Scheme is a @deftech{call-by-value}
language.

Evaluation of a local-variable form, such as @scheme[(let ([x (+ 1
2)]) _expr)], is the same as for a procedure call. After @scheme[(+ 1
2)] produces a @tech{value}, it is stored in a fresh @tech{location}
that replaces every instance of @scheme[x] in @scheme[_expr].

@;------------------------------------------------------------------------
@section{Variables and Locations}

A @deftech{variable} is a placeholder for a @tech{value}, and an
expressions in an initial program refer to @tech{variables}. A
@deftech{top-level variable} is both a @tech{variable} and a
@tech{location}. Any other @tech{variable} is always replaced by a
@tech{location} at run-time, so that evaluation of expressions
involves only @tech{locations}. A single @deftech{local variable}
(i.e., a non-top-level, non-module-level @tech{variable}), such as a
procedure argument, can correspond to different @tech{locations}
through different instantiations.

For example, in the program

@schemeblock[(define y (+ (let ([x 5]) x) 6))]

both @scheme[y] and @scheme[x] are @tech{variables}. The @scheme[y]
@tech{variable} is a @tech{top-level variable}, and the @scheme[x] is
a @tech{local variable}. When this code is evaluated, a
@tech{location} is created for @scheme[x] to hold the value
@scheme[5], and a @tech{location} is also created for @scheme[y] to
hold the value @scheme[6].

The replacement of a @tech{variable} with a @tech{location} during
evaluation implements Scheme's @deftech{lexical scoping}. For example,
when a procedure-argument @tech{variable} @scheme[x] is replaced by
the @tech{location} @scheme[xloc], then it is replaced throughout the
body of the procedure, including with any nested @scheme[lambda]
forms. As a result, future references of the @tech{variable} always
access the same @tech{location}.

@;------------------------------------------------------------------------
@section[#:tag "module-eval-model"]{Modules and Module-Level Variables}

@margin-note/ref{See @secref["module"] for the syntax of modules.}

Most definitions in PLT Scheme are in modules. In terms of evaluation,
a module is essentially a prefix on a defined name, so that different
modules can define the name. That is, a @deftech{module-level
variable} is like a @tech{top-level variable} from the perspective of
evaluation.

One difference between a module and a top-level definition is that a
module can be declared without instantiating its module-level
definitions. Evaluation of a @scheme[require] @deftech{instantiates}
(i.e., triggers the @deftech{instantiation} of) a declared module,
which creates variables that correspond to its module-level
definitions.

For example, given the module declaration

@schemeblock[
(module m scheme
  (define x 10))
]

the evaluation of @scheme[(require m)] creates the variable @scheme[x]
and installs @scheme[10] as its value. This @scheme[x] is unrelated to
any top-level definition of @scheme[x].

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "module-phase"]{Phases}

A module can be @tech{instantiate}d in multiple @deftech{phases}. A
phase is an integer that, again, is effectively a prefix on the names
of module-level definitions. A top-level @scheme[require]
@tech{instantiates} a module at @tech{phase} 0, if the module is not
already @tech{instantiate}d at phase 0.  A top-level
@scheme[(require (for-syntax ....))] @tech{instantiates} a module at
@tech{phase} 1 (if it is not already @tech{instantiate}d at that
level); @scheme[for-syntax] also has a different binding
effect on further program parsing, as described in
@secref["intro-binding"].

Within a module, some definitions are shifted by a phase already; the
@scheme[define-for-syntax] form is like @scheme[define], but it
defines a variable at relative @tech{phase} 1, instead of relative
@tech{phase} 0. Thus, if the module is @tech{instantiate}d at phase 1,
the variables for @scheme[define-for-syntax] are created at phase 2,
and so on. Moreover, this relative phase acts as another layer of
prefixing, so that a @scheme[define] of @scheme[x] and a
@scheme[define-for-syntax] of @scheme[x] can co-exist in a module
without colliding. Again, the higher phases are mainly related to
program parsing, instead of normal evaluation.

If a module @tech{instantiate}d at @tech{phase} @math{n}
@scheme[require]s another module, then the @scheme[require]d module is
first @tech{instantiate}d at phase @math{n}, and so on
transitively. (Module @scheme[require]s cannot form cycles.) If a
module @tech{instantiate}d at phase @math{n} @scheme[require]s
@scheme[for-syntax] another module, the other module becomes
@deftech{available} at @tech{phase} @math{n+1}, and it may later be
@tech{instantiate}d at @tech{phase} @math{n+1}.  If a module that is
@tech{available} at phase @math{n} for @math{n>0} @scheme[require]s
@scheme[for-template] another module, the other module becomes
@tech{available} at @tech{phase} @math{n-1}, and so
on. @tech{Instantiation}s of @tech{available} modules above
@tech{phase} 0 are triggered on demand as described in
@secref["mod-parse"].

A final distinction among module @tech{instantiations} is that
multiple @tech{instantiations} may exist at @tech{phase} 1 and
higher. These @tech{instantiations} are created by the parsing of
module forms (see @secref["mod-parse"]), and are, again, conceptually
distinguished by prefixes.

Top-level variables can exist in multiple phases in the same way as
within modules. For example, @scheme[define-for-syntax] creates a
@tech{phase} 1 variable. Furthermore, reflective operations like
@scheme[make-base-namespace] and @scheme[eval] provide access to
top-level variables in higher @tech{phases}, while module
@tech{instantiations} (triggered by with @scheme[require]) relative to such
top-levels are in corresponding higher @tech{phase}s.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "module-redeclare"]{Module Re-declarations}

@section-index["modules" "re-define"]

When a module is declared using a name for which a module is already
declared, the new declaration's definitions replace and extend the old
declarations. If a variable in the old declaration has no counterpart
in the new declaration, the old variable continues to exist, but its
binding is not included in the @tech{lexical information} for the
module body. If a new variable definition has a counterpart in the old
declaration, it effectively assigns to the old variable.

If a module is @tech{instantiate}d in any @tech{phase}s before it is
re-declared, each re-declaration of the module is immediately
@tech{instantiate}d in the same @tech{phase}s.

@;------------------------------------------------------------------------
@section[#:tag "mark-model"]{Continuation Frames and Marks}

@margin-note/ref{See @secref["contmarks"] for continuation-mark forms and functions.}

Every continuation @scheme[_C] can be partitioned into
@deftech{continuation frames} @frame[1], @frame[2], ..., @frame["n"]
such that @scheme[_C] = @*sub[@frame[1] @*sub[@frame[2] @*sub["..."
@frame["n"]]]], and no frame @frame["i"] can be itself partitioned
into smaller continuations. Evaluation steps add and remove frames to
the current continuation, typically one at a time.

Each frame is conceptually annotated with a set of
@deftech{continuation marks}. A mark consists of a key and its value;
the key is an arbitrary value, and each frame includes at most one
mark for any key. Various operations set and extract marks from
continuations, so that marks can be used to attach information to a
dynamic extent. For example, marks can be used to record information
for a ``stack trace'' to be used when an exception is raised, or
to implement dynamic scope.

@;------------------------------------------------------------------------
@section[#:tag "prompt-model"]{Prompts, Delimited Continuations, and Barriers}

@margin-note/ref{See @secref["cont"] for continuation and prompt functions.}

A @deftech{prompt} is a special kind of continuation frame that is
annotated with a specific @deftech{prompt tag} (essentially a
continuation mark). Various operations allow the capture of frames in
the continuation from the redex position out to the nearest enclosing
prompt with a particular prompt tag; such a continuation is sometimes
called a @deftech{delimited continuation}. Other operations allow the
current continuation to be extended with a captured continuation
(specifically, a @deftech{composable continuation}). Yet other
operations abort the computation to the nearest enclosing prompt with
a particular tag, or replace the continuation to the nearest enclosing
prompt with another one. When a delimited continuation is captured,
the marks associated with the relevant frames are also captured.

A @deftech{continuation barrier} is another kind of continuation frame
that prohibits certain replacements of the current continuation with
another. Specifically, while an abort is allowed to remove a portion
of the continuation containing a prompt, the continuation can be
replaced by another only when the replacement also includes the
continuation barrier. Certain operations install barriers
automatically; in particular, when an exception handler is called, a
continuation barrier prohibits the continuation of the handler from
capturing the continuation past the exception point.

A @deftech{escape continuation} is essentially a derived concept. It
combines a prompt for escape purposes with a continuation for
mark-gathering purposes. as the name implies, escape continuations are
used only to abort to the point of capture, which means that
escape-continuation aborts can cross continuation barriers.

@;------------------------------------------------------------------------
@section[#:tag "thread-model"]{Threads}

@margin-note/ref{See @secref["concurrency"] for thread and synchronization functions.}

Scheme supports multiple @deftech{threads} of evaluation.  Threads run
concurrently, in the sense that one thread can preempt another without
its cooperation, but threads currently all run on the same processor
(i.e., the same underlying OS process and thread). See also
@secref["futures"].

Threads are created explicitly by functions such as @scheme[thread]. 
In terms of the evaluation model, each step in evaluation actually consists of multiple concurrent
expressions, up to one per thread, rather than a single expression. The expressions all
share the same objects and top-level variables, so that they can
communicate through shared state. Most evaluation steps involve a
single step in a single expression, but certain synchronization
primitives require multiple threads to progress together in one step.

In addition to the state that is shared among all threads, each thread
has its own private state that is accessed through @deftech{thread
cells}. A thread cell is similar to a normal mutable object, but a
change to the value inside a thread cell is seen only when extracting
a value from the cell from the same thread. A thread cell can be
@deftech{preserved}; when a new thread is created, the creating
thread's value for a preserved thread cell serves as the initial value
for the cell in the created thread. For a non-preserved thread cell, a
new thread sees the same initial value (specified when the thread cell
is created) as all other threads.

@;------------------------------------------------------------------------
@section[#:tag "parameter-model"]{Parameters}

@margin-note/ref{See @secref["parameters"] for parameter forms and functions.}

@deftech{Parameters} are essentially a derived concept in Scheme; they
are defined in terms of @tech{continuation marks} and @tech{thread
cells}. However, parameters are also built in, in the sense that some
primitive procedures consult parameter values. For example, the
default output stream for primitive output operations is determined by
a parameter.

A parameter is a setting that is both thread-specific and
continuation-specific. In the empty continuation, each parameter
corresponds to a @tech{preserved} @tech{thread cell}; a corresponding
@deftech{parameter procedure} accesses and sets the thread cell's
value for the current thread.

In a non-empty continuation, a parameter's value is determined through
a @deftech{parameterization} that is associated with the nearest
enclosing continuation frame though a continuation mark (whose key is
not directly accessible). A parameterization maps each parameter to a
preserved thread cell, and the combination of thread cell and current
thread yields the parameter's value. A parameter procedure sets or
accesses the relevant thread cell for its parameter.

Various operations, such as @scheme[parameterize] or
@scheme[call-with-parameterization], install a parameterization into
the current continuation's frame.

@;------------------------------------------------------------------------
@section[#:tag "exn-model"]{Exceptions}

@margin-note/ref{See @secref["exns"] for exception forms, functions, and types.}

@deftech{Exceptions} are essentially a derived concept in Scheme; they
are defined in terms of continuations, prompts, and continuation
marks.  However, exceptions are also built in, in the sense that
primitive forms and procedures may raise exceptions.

An @deftech{exception handler} to catch exceptions can be associated
with a continuation frame though a @tech{continuation mark} (whose key
is not directly accessible). When an exception is raised, the current
continuation's marks determine a chain of @tech{exception handler}
procedures that are consulted to handle the exception. A handler for
uncaught exceptions is designated through a built-in @tech{parameter}.

One potential action of an @tech{exception handler} is to abort the
current @tech{continuation} up to an enclosing @tech{prompt} with a
particular @tech{prompt tag}.  The default handler for uncaught
exceptions, in particular, aborts to a particular tag for which a
prompt is always present, because the prompt is installed in the
outermost frame of the continuation for any new thread.

@;------------------------------------------------------------------------
@section[#:tag "custodian-model"]{Custodians}

@margin-note/ref{See @secref["custodians"] for custodian functions.}

A @deftech{custodian} manages a collection of threads,
@tech{file-stream ports}, TCP ports, @tech{TCP listeners}, @tech{UDP
sockets}, and @tech{byte converters}.  Whenever a thread, etc. is
created, it is placed under the management of the @deftech{current
custodian} as determined by the @scheme[current-custodian]
@tech{parameter}.

@margin-note{In MrEd, custodians also manage eventspaces.}

Except for the root custodian, every @tech{custodian} itself it
managed by a @tech{custodian}, so that custodians form a hierarchy.
Every object managed by a subordinate custodian is also managed by the
custodian's owner.

When a @tech{custodian} is shut down via
@scheme[custodian-shutdown-all], it forcibly and immediately closes
the ports, TCP connections, etc. that it manages, as well as
terminating (or suspending) its threads. A custodian that has been
shut down cannot manage new objects.  If the current custodian is shut
down before a procedure is called to create a managed resource (e.g.,
@scheme[open-input-port], @scheme[thread]), the
@exnraise[exn:fail:contract].

A thread can have multiple managing custodians, and a suspended thread
created with @scheme[thread/suspend-to-kill] can have zero
custodians. Extra custodians become associated with a thread through
@scheme[thread-resume] (see @secref["threadkill"]). When a thread
has multiple custodians, it is not necessarily killed by a
@scheme[custodian-shutdown-all], but shut-down custodians are removed
from the thread's managing set, and the thread is killed when its
managing set becomes empty.

The values managed by a custodian are only weakly held by the
custodian. As a result, a @techlink{will} can be executed for a value that
is managed by a custodian. In addition, a custodian only weakly
references its subordinate custodians; if a subordinate custodian is
unreferenced but has its own subordinates, then the custodian may be
collected, at which point its subordinates become immediately
subordinate to the collected custodian's superordinate custodian.

In addition to the other entities managed by a custodian, a
@deftech{custodian box} created with @scheme[make-custodian-box]
strongly holds onto a value placed in the box until the box's
custodian is shut down. The custodian only weakly retains the box
itself, however (so the box and its content can be collected if there
are no other references to them).

When PLT Scheme is compiled with support for per-custodian memory
accounting (see @scheme[custodian-memory-accounting-available?]), the
@scheme[current-memory-use] procedure can report a custodian-specific
result.  This result determines how much memory is occupied by objects
that are reachable from the custodian's managed values, especially its
threads, and including its sub-custodians' managed values. If an
object is reachable from two custodians where neither is an ancestor
of the other, an object is arbitrarily charged to one of the other,
and the choice can change after each collection; objects reachable
from both a custodian and its descendant, however, are reliably
charged to the custodian and not to the descendants, unless the
custodian can reach the objects only through a descendant custodian or
a descendant's thread.  Reachability for per-custodian accounting does
not include weak references, references to threads managed by other
custodians, references to other custodians, or references to custodian
boxes for other custodians.
