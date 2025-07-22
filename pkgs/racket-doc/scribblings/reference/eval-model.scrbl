#lang scribble/manual

@(require scribble/struct scribble/racket "mz.rkt" "prog-steps.rkt"
          (for-syntax racket/base))

@(define reduces (make-element #f (list 'rarr)))
@(define rspace (make-element "ghost" (list 'rarr)))

@(define *redex (lambda (c)
                  (make-element highlighted-color (list c))))
@(define-syntax redex
   (syntax-rules () [(_ a) (*redex (racket a))]))

@(define hole (make-element #f (list "[]")))
@(define (*sub c e) (make-element #f (list c "[" e "]")))
@(define-syntax sub
   (syntax-rules () [(_ a b) (*sub (racket a) (racket b))]))
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
   (syntax-rules () [(_ a b) (*state (racket a) (racket b))]))
;}

@;------------------------------------------------------------------------
@title[#:tag "eval-model"]{Evaluation Model}

Racket evaluation can be viewed as the simplification of expressions
to obtain values. For example, just as an elementary-school student
simplifies

@verbatim{  1 + 1 = 2}

Racket evaluation simplifies

@racketblock[
(+ 1 1) @#,reduces 2
]

The arrow @reduces replaces the more traditional @tt{=} to
emphasize that evaluation proceeds in a particular direction toward
simpler expressions. In particular, a @deftech{value}, such as the number @racket[2],
is an expression that evaluation simplifies no further.

@;------------------------------------------------------------------------
@section[#:tag "cont-model"]{Sub-expression Evaluation and Continuations}

Some simplifications require more than one step. For example:

@racketblock[
(- 4 #,(redex (+ 1 1))) #,reduces #,(redex (- 4 2)) #,reduces 2
]

An expression that is not a @tech{value} can always be partitioned
into two parts: a @deftech{redex} (``reducible expression''),
which is the part that can change in a
single-step simplification (highlighted), and the
@deftech{continuation}, which is the evaluation
context surrounding the redex. In @racket[(- 4 (+ 1 1))], the redex is @racket[(+ 1 1)], and
the continuation is @racket[(- 4 @#,hole)], where @hole takes the place
of the @tech{redex} as it is reduced. That is, the continuation says how to ``continue''
after the @tech{redex} is reduced to a @tech{value}.

Before some expressions can be evaluated, some or all of their sub-expressions must be
evaluated. For example, in the application @racket[(- 4 (+ 1 1))], the
application of @racket[-] cannot be reduced until the sub-expression
@racket[(+ 1 1)] is reduced.
Thus, the specification of each syntactic form specifies how (some of)
its sub-expressions are evaluated and then how the results are
combined to reduce the form away.

The @deftech{dynamic extent} of an expression is the sequence of
evaluation steps during which the expression contains the @tech{redex}.

@;------------------------------------------------------------------------
@section{Tail Position}

An expression @racket[_expr1] is in @deftech{tail position} with
respect to an enclosing expression @racket[_expr2] if, whenever
@racket[_expr1] becomes a redex, its @tech{continuation} is the same
as was the enclosing @racket[_expr2]'s @tech{continuation}.

For example, the @racket[(+ 1 1)] expression is @italic{not} in @tech{tail
position} with respect to @racket[(- 4 (+ 1 1))]. To illustrate, we use
the notation @sub[_C _expr] to mean the expression that is produced by
substituting @racket[_expr] in place of @hole in some @tech{continuation}
@racket[_C]:

@racketblock[
@#,sub[_C (- 4 (+ 1 1))] @#,reduces @#,sub[_C (- 4 2)]
]

In this case, the @tech{continuation} for reducing @racket[(+ 1 1)] is
@sub[_C (- 4 @#,hole)], not just @racket[_C]. The requirement specified in the first paragraph above is not met.

In contrast, @racket[(+ 1 1)] is in @tech{tail position} with respect
to @racket[(if (zero? 0) (+ 1 1) 3)] because, for any continuation
@racket[_C],

@racketblock[
@#,sub[_C (if (zero? 0) (+ 1 1) 3)] @#,reduces @#,sub[_C (if #t (+ 1 1) 3)] @#,reduces @#,sub[_C (+ 1 1)]
]

The requirement specified in the first paragraph is met.
The steps in this reduction sequence are driven by the definition of
@racket[if], and they do not depend on the @tech{continuation}
@racket[_C]. The ``then'' branch of an @racket[if] form is always in
@tech{tail position} with respect to the @racket[if] form. Due to a
similar reduction rule for @racket[if] and @racket[#f], the ``else''
branch of an @racket[if] form is also in @tech{tail position}.

@tech{Tail-position} specifications provide a guarantee about the
asymptotic space consumption of a computation. In general, the
specification of @tech{tail positions} accompanies the description of
each syntactic form, such as @racket[if].

@;------------------------------------------------------------------------
@section[#:tag "values-model"]{Multiple Return Values}

A Racket expression can evaluate to @deftech{multiple values}, to
provide symmetry with the fact that a procedure can accept multiple arguments.

Most @tech{continuations} expect a certain number of result
@tech{values}, although some @tech{continuations} can accept
an arbitrary number. Indeed, most @tech{continuations}, such as @racket[(+
@#,hole 1)], expect a single @tech{value}. The @tech{continuation}
@racket[(let-values ([(x y) @#,hole]) _expr)] expects two result
@tech{values}; the first result replaces @racket[x] in the body
@racket[_expr], and the second replaces @racket[y] in
@racket[_expr]. The @tech{continuation} @racket[(begin @#,hole (+ 1
2))] accepts any number of result @tech{values}, because it ignores
the result(s).

In general, the specification of a syntactic form indicates the
number of @tech{values} that it produces and the number that it
expects from each of its sub-expressions. In addition, some procedures
(notably @racket[values]) produce multiple @tech{values}, and some
procedures (notably @racket[call-with-values]) create continuations
internally that accept a certain number of @tech{values}.

@;------------------------------------------------------------------------
@section{Top-Level Variables}

Given

@verbatim{  x = 10}

then an algebra student simplifies @tt{x + 1} as follows:

@verbatim{  x + 1 = 10 + 1 = 11}

Racket works much the same way, in that a set of @tech{top-level
variables} (see also @secref["vars-and-locs"]) are available for substitutions on demand during
evaluation. For example, given

@racketblock[
(define x 10)
]

then

@racketblock[
#,(redex (+ x 1)) #,reduces #,(redex (+ 10 1)) #,reduces 11
]

In Racket, the way definitions are created is just as important as the way
they are used. Racket evaluation thus keeps track of both
definitions and the current expression, and it extends the set of
definitions in response to evaluating forms such as @racket[define].

Each evaluation step, then, transforms the current set of definitions and
program into a new set of definitions and program. Before a
@racket[define] can be moved into the set of definitions, its
expression (i.e., its right-hand side) must be reduced to a @tech{value}.
(The left-hand side is not an expression position, and so it is not evaluated.)

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

Using @racket[set!], a program can change the value associated with an
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

In addition to @racket[set!] for imperative update of @tech{top-level
variables}, various procedures enable the modification of elements
within a compound data structure. For example, @racket[vector-set!]
modifies the content of a vector.

To explain such modifications to data, we must distinguish between
@tech{values}, which are the results of expressions, and
@deftech{objects}, which actually hold data.

A few kinds of @tech{objects} can serve directly as values, including
booleans, @racket[(void)], and small exact integers. More generally,
however, a @tech{value} is a reference to an @tech{object} stored somewhere
else. For example, a @tech{value} can refer to a particular vector that
currently holds the value @racket[10] in its first slot. If an
@tech{object} is modified via one @tech{value},
then the modification is visible through
all the @tech{values} that reference the @tech{object}.

In the evaluation model, a set of @tech{objects} must be carried along
with each step in evaluation, just like the definition set. Operations
that create @tech{objects}, such as @racket[vector], add to the set of
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
@tech{value}, so it must be evaluated. Each time
a @tech{variable} expression is evaluated, the
value of the variable is extracted from the current set of definitions. An object
reference, in contrast, is a value and therefore needs no further
evaluation. The evaluation steps above use angle-bracketed
@racket[<o1>] for an object reference to distinguish it from a
@tech{variable} name.

An object reference can never appear directly in a text-based source
program. A program representation created with
@racket[datum->syntax], however, can embed direct references to
existing @tech{objects}.

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

evaluation cannot depend on @racket[<o2>], because it is not part of
the program to evaluate, and it is not referenced by any definition
that is accessible by the program. The object is said to not
be @deftech{reachable}. The @tech{object} @racket[<o2>] may
therefore be removed from the program state by @deftech{garbage
collection}.

A few special compound datatypes hold @deftech{weak references} to
objects. Such weak references are treated specially by the garbage
collector in determining which @tech{objects} are reachable for the
remainder of the computation. If an @tech{object} is reachable @italic{only}
via a @tech{weak reference}, then the object can be reclaimed, and the
@tech{weak reference} is replaced by a different @tech{value}
(typically @racket[#f]).

As a special case, a @tech{fixnum} is always considered reachable by
the garbage collector. Many other values are always reachable due to
the way they are implemented and used: A @tech{character} in the
Latin-1 range is always reachable, because @racket[equal?] Latin-1
characters are always @racket[eq?], and all of the Latin-1 characters
are referenced by an internal module. Similarly, @racket[null],
@racket[#t], @racket[#f], @racket[eof], and @|void-const| are
always reachable. Values produced by @racket[quote] remain reachable
when the @racket[quote] expression itself is reachable.

@;------------------------------------------------------------------------
@section{Procedure Applications and Local Variables}

Given

@verbatim{  f(x) = x + 10}

an algebra student simplifies @tt{f(7)} as follows:

@verbatim{  f(7) = 7 + 10 = 17}

The key step in this simplification is to take the body of the defined
function @tt{f} and replace each @tt{x} with the actual
@tech{value} @tt{7}.

Racket procedure application works much the same way. A procedure is
an @tech{object}, so evaluating @racket[(f 7)] starts with a
@tech{variable} lookup:

@prog-steps[
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)}
 ((code:hilite f) 7)]
[{(define <p1> (lambda (x) (+ x 10)))}
 {(define f <p1>)}
 (code:hilite (<p1> 7))]
]

Unlike in algebra, however, the @tech{value} associated with a procedure
argument variable can be changed in the body of a procedure by using
@racket[set!], as in the example @racket[(lambda (x) (begin (set! x 3)
x))]. Since the @tech{value} associated with argument variable @racket[x] should be
able to change, we cannot just substitute the value in for @racket[x] when
we first apply the procedure.

@margin-note{We do not use the term ``parameter variable'' to refer to
the argument variable names declared with a function. This choice avoids
confusion with @tech{parameters}.}

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
not been used before and that cannot be generated again or
accessed directly.

Generating a @tech{location} in this way means that @racket[set!]
evaluates for @tech{local variables}, including argument
variables, in the same way as for
@tech{top-level variables}, because the @tech{local variable} is
always replaced with a @tech{location} by the time the @racket[set!]
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

The @tech{location}-generation and substitution step of procedure
application requires that the argument is a @tech{value}. Therefore,
in @racket[((lambda (x) (+ x 10)) (+ 1 2))], the @racket[(+ 1 2)]
sub-expression must be simplified to the @tech{value} @racket[3], and
then @racket[3] can be placed into a @tech{location} for
@racket[x]. In other words, Racket is a @deftech{call-by-value}
language.

Evaluation of a local-variable form, such as @racket[(let ([x (+ 1
2)]) _expr)], is the same as for a procedure call. After @racket[(+ 1
2)] produces a @tech{value}, it is stored in a fresh @tech{location}
that replaces every instance of @racket[x] in @racket[_expr].

@;------------------------------------------------------------------------
@section[#:tag "vars-and-locs"]{Variables and Locations}

A @deftech{variable} is a placeholder for a @tech{value}, and
expressions in an initial program refer to @tech{variables}. A
@deftech{top-level variable} is both a @tech{variable} and a
@tech{location}. Any other @tech{variable} is always replaced by a
@tech{location} at run-time; thus, evaluation of expressions
involves only @tech{locations}. A single @deftech{local variable}
(i.e., a non-top-level, non-module-level @tech{variable}), such as an
argument variable, can correspond to different @tech{locations}
during different applications.

For example, in the program

@racketblock[(define y (+ (let ([x 5]) x) 6))]

both @racket[y] and @racket[x] are @tech{variables}. The @racket[y]
@tech{variable} is a @tech{top-level variable}, and the @racket[x] is
a @tech{local variable}. When this code is evaluated, a
@tech{location} is created for @racket[x] to hold the value
@racket[5], and a @tech{location} is also created for @racket[y] to
hold the value @racket[11].

The replacement of a @tech{variable} with a @tech{location} during
evaluation implements Racket's @deftech{lexical scoping}.
@margin-note*{For the purposes of substituting @racket[xloc] for @racket[x],
all variable bindings must use distinct names, so no @racket[x] that
is really a different variable will get replaced. Ensuring that
distinction is one of the jobs of the macro expander; see @secref["syntax-model"].}
For example, when an argument variable @racket[x] is replaced by
the @tech{location} @racket[xloc], it is replaced @italic{throughout} the
body of the procedure, including any nested @racket[lambda]
forms. As a result, future references to the @tech{variable} always
access the same @tech{location}.

@;------------------------------------------------------------------------
@section[#:tag "module-eval-model"]{Modules and Module-Level Variables}

@margin-note/ref{See @secref["module"] for the syntax of modules.}

Most definitions in Racket are in @deftech{modules}. In terms of evaluation,
a module is essentially a prefix on a defined name, so that different
modules can define the same name. That is, a @deftech{module-level
variable} is like a @tech{top-level variable} from the perspective of
evaluation.

One difference between a module and a top-level definition
is that a module can be @deftech[#:key "declare"]{declared}
without instantiating its module-level definitions.
Evaluation of a @racket[require] @deftech{instantiates}
(i.e., triggers the @deftech{instantiation} of) the declared
module, which creates variables that correspond to its
module-level definitions.

For example, given the module declaration

@racketblock[
(module m racket
  (define x 10))
]

the evaluation of @racket[(require 'm)] creates the variable @racket[x]
and installs @racket[10] as its value. This @racket[x] is unrelated to
any top-level definition of @racket[x] (as if it were given a unique,
module-specific prefix).

@;------------------------------------------------------------------------
@subsection[#:tag "module-phase"]{Phases}

@guidealso["phases"]

The purpose of @deftech{phases} is to
address the necessary separation of names defined at execution time versus
names defined at expansion time.

A module can be @tech{instantiate}d in multiple @tech{phases}. A
phase is an integer that, like a module name, is effectively a prefix on the names
of module-level definitions. Phase 0 is the execution-time phase.

A top-level @racket[require]
@tech{instantiates} a module at @tech{phase} 0, if the module is not
already @tech{instantiate}d at phase 0.  A top-level
@racket[(require (for-syntax ....))] @tech{instantiates} a module at
@tech{phase} 1 (if it is not already @tech{instantiate}d at that
phase); @racket[for-syntax] also has a different binding
effect on further program parsing, as described in
@secref["intro-binding"].

Within a module, some definitions are already shifted by a phase: the
@racket[begin-for-syntax] form is similar to @racket[begin], but it
shifts expressions and definitions by a relative @tech{phase} +1. 
Likewise, the @racket[define-for-syntax] form is similar to @racket[define],
but shifts the definition by +1.
Thus, if the module is @tech{instantiate}d at phase 1,
the variables defined with @racket[begin-for-syntax] are created at phase 2,
and so on. Moreover, this relative phase acts as another layer of
prefixing, so that @racket[x] defined with @racket[define] and
@racket[x] defined with @racket[define-for-syntax] can co-exist in a module
without colliding. A @racket[begin-for-syntax] form can be nested
within a @racket[begin-for-syntax] form, in which case the inner definitions and
expressions are in relative @tech{phase} +2, and so on. Higher phases are 
mainly related to program parsing instead of normal evaluation.

If a module @tech{instantiate}d at @tech{phase} @math{n}
@racket[require]s another module, then the @racket[require]d module is
first @tech{instantiate}d at phase @math{n}, and so on
transitively. (Module @racket[require]s cannot form cycles.) If a
module @tech{instantiate}d at phase @math{n} @racket[require]s
another module @racket[_M] @racket[for-syntax], then @racket[_M] becomes
@deftech{available} at @tech{phase} @math{n+1}, and it later may be
@tech{instantiate}d at @tech{phase} @math{n+1}.  If a module that is
@tech{available} at phase @math{n} (for @math{n>0}) @racket[require]s
another module @racket[_M] @racket[for-template], then @racket[_M] becomes
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
within modules. For example, @racket[define] within @racket[begin-for-syntax] creates a
@tech{phase} 1 variable. Furthermore, reflective operations like
@racket[make-base-namespace] and @racket[eval] provide access to
top-level variables in higher @tech{phases}, while module
@tech{instantiations} (triggered by @racket[require]) relative to such
top-levels are in correspondingly higher @tech{phase}s.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "separate-compilation"]{The Separate Compilation Guarantee}

When a module is compiled, its @tech{phase} 1 is instantiated. This
can, in turn, trigger the transitive instantiation of many other
modules at other phases, including phase 1. Racket provides a
guarantee about this instantiation called
@index["separate compilation guarantee"]{``The Separate
Compilation Guarantee''}:

@nested[#:style 'inset]{Any @tech{effects} of the instantiation of the module's phase 1 due
to compilation on the Racket runtime system are @tech{discarded}.}

The guarantee concerns @deftech{effects}. There are two different
kinds of effects: internal and external.

Internal effects are exemplified by mutation.  Mutation is the action
of a function such as @racket[set-box!], which changes the value
contained in the box. The modified box is not observable outside
Racket, so the effect is said to be ``internal.'' By definition,
internal effects are not detectable outside the Racket program.

External effects are exemplified by input/output (I/O). I/O is the
action of a function such as @racket[tcp-connect], which communicates
with the operating system to send network packets outside the
machine running Racket. The transmission of these packets is
observable outside Racket, in particular by the receiving computer
or any routers in between. External effects exist to be detectable
outside the Racket program and are often detectable using physical
processes.

An effect is @deftech{discarded} when it is no longer detectable. For
instance, the mutation of a box from @racket[3] to @racket[4] is
discarded when it ceases to be detectable that it was ever changed and
thus would still contain @racket[3]. Because external effects are
intrinsically observable outside Racket, they are irreversible and
cannot be discarded.

Thus, the Separate Compilation Guarantee only concerns effects like
mutation, because they are exclusively effects ``on the Racket runtime
system'' and not ``on the physical universe.''

Whenever a Racket program calls an @tech{unsafe}
function, the Racket runtime system makes no promises about its
effects. For instance, all foreign calls use
@racketmodname[ffi/unsafe], so all foreign calls are unsafe and their
effects cannot be discarded by Racket.

Finally, The Separate Compilation Guarantee only concerns
instantiations at phase 1 during compilation and not all phase 1
instantiations generally, such as when its phase 1 is required and
used for effects via reflective mechanisms.

The practical consequence of this guarantee is that because effects
are not visible, no module can detect whether a module it
@racket[require]s is already compiled. Thus, it cannot change the
compilation of one module to have already compiled a different module.
In particular, if module A is shared by the phase 1 portion of modules
X and Y, then any internal effects while X is compiled are not visible
during the compilation of Y, regardless of whether X and Y are
compiled during the same execution of Racket's runtime system and
regardless of the order of compilation.

The following set of modules demonstrate this guarantee. First, we
define a module with the ability to observe effects via a
@racket[box]:

@racketblock[
(module box racket/base
  (provide (all-defined-out))
  (define b (box 0)))
]

Next, we define two syntax transformers that use and mutate this box:

@RACKETBLOCK[
(module transformers racket/base
  (provide (all-defined-out))
  (require (for-syntax racket/base 'box))
  (define-syntax (sett stx)
    (set-box! b 2)
    (syntax (void)))
  (define-syntax (gett stx)
    (quasisyntax (unsyntax (unbox b)))))
]

Next, we define a module that uses these transformers:

@racketblock[
(module user racket/base
  (provide (all-defined-out))
  (require 'transformers)
  (sett)
  (define gott (gett)))
]

Finally, we define a second module that uses these
transformers and the @racket[user] module:

@racketblock[
(module test racket/base
  (require 'box 'transformers 'user)
  (displayln gott)
  (displayln (gett))

  (sett)
  (displayln (gett))

  (displayln (unbox b))
  )
]

This module displays:
@itemize[
@item{@litchar["2"], because the @racket[(gett)] in module @racket[user] expanded to @racket[2].}
@item{@litchar["0"], because the effects of compiling @racket[user] were discarded.}
@item{@litchar["2"], because the effect of @racket[(sett)] inside @racket[test] has
not yet been discarded.}
@item{@litchar["0"], because the effects of @racket[sett] at
phase 1 are irrelevant to the phase 0 use of @racket[b] in @racket[(unbox b)].}
]

Furthermore, this display will never change, regardless of which order
these modules are compiled in or whether they are compiled at the same
time or separately.

In contrast, if these modules were changed to store the value of
@racket[b] in a file on the filesystem, then the program would only
display @litchar["2"].

The Separate Compilation Guarantee is described in more detail
in the papers ``Composable and Compilable Macros'' @cite["Flatt02"]
and ``Submodules in Racket'' @cite["Flatt13"], including
informative examples. The paper ``Advanced Macrology and the
implementation of Typed Scheme'' @cite["Culpepper07"] also contains an
extended example of why it is important and how to design effectful
syntactic extensions in its presence.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "cross-phase persistent-modules"]{Cross-Phase Persistent Modules}

Module declarations that fit a highly constrained
form---including a
@racket[(#%declare #:cross-phase-persistent)] form in the
module body---create @deftech{cross-phase persistent}
modules. A @tech{cross-phase persistent} module's
instantiations across all phases share the variables
produced by the first instantiation of the module.
Additionally, @tech{cross-phase persistent} module
instantiations persist across @tech{module registries} when
they share a common module declaration.

@examples[
 (module cross '#%kernel
   (#%declare #:cross-phase-persistent)
   (#%provide x)
   (define-values (x) (gensym)))
 (module noncross '#%kernel
   (#%provide x)
   (define-values (x) (gensym)))
 (define ns (current-namespace))
 (define (same-instance? mod)
   (namespace-require mod)
   (define a
     (parameterize ([current-namespace (make-base-namespace)])
       (namespace-attach-module-declaration ns mod)
       (namespace-require mod)
       (namespace-variable-value 'x)))
   (define b
     (parameterize ([current-namespace (make-base-namespace)])
       (namespace-attach-module-declaration ns mod)
       (namespace-require mod)
       (namespace-variable-value 'x)))
   (eq? a b))
 (same-instance? ''noncross)
 (same-instance? ''cross)
 ]

The intent of a @tech{cross-phase persistent} module is to support values that are
recognizable after @tech{phase} crossings. For example, when a macro
transformer running in phase 1 raises a syntax error as represented by
an @racket[exn:fail:syntax] instance, the instance is recognizable by a
phase-0 exception handler wrapping a call to @racket[eval] or
@racket[expand] that triggered the syntax error, because the
@racket[exn:fail:syntax] structure type is defined by a
@tech{cross-phase persistent} module.

A @tech{cross-phase persistent} module imports only other @tech{cross-phase persistent} modules,
and it contains only definitions that bind variables to functions,
structure types and related functions, or structure-type properties
and related functions. A @tech{cross-phase persistent} module never includes syntax
literals (via @racket[quote-syntax]) or variable references (via
@racket[#%variable-reference]). See @secref["cross-phase persistent-grammar"] for
the syntactic specification of a @tech{cross-phase persistent} module
declaration.

A documented module should be assumed non--@tech{cross-phase persistent} unless it
is specified as @tech{cross-phase persistent} (such as
@racketmodname[racket/kernel]).

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "module-redeclare"]{Module Redeclarations}

@section-index["modules" "re-define"]

When a module is declared using a name with which a module is already
declared, the new declaration's definitions replace and extend the old
declarations. If a variable in the old declaration has no counterpart
in the new declaration, the old variable continues to exist, but its
binding is not included in the @tech{lexical information} for the
module body. If a new variable definition has a counterpart in the old
declaration, it effectively assigns to the old variable.

If a module is @tech{instantiate}d in the current namespace's
@tech{base phase} before the module is redeclared, the redeclaration
of the module is immediately @tech{instantiate}d in that
@tech{phase}.

If the current @tech{inspector} does not manage a module's declaration
inspector (see @secref["modprotect"]), then the module cannot be
redeclared. Similarly, a @tech{cross-phase persistent} module cannot be redeclared.
Even if redeclaration succeeds, instantiation of a module that is
previously instantiated may fail if instantiation for the
redeclaration attempts to modify variables that are constant (see
@racket[compile-enforce-module-constants]).

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection[#:tag "submodules"]{Submodules}

A @racket[module] or @racket[module*] form within a top-level
@racket[module] form declares a @deftech{submodule}. A submodule is
accessed relative to its enclosing module, usually with a
@racket[submod] path. Submodules can be nested to any depth.

Although a submodule is lexically nested within a module, it cannot
necessarily access the bindings of its enclosing module directly.
More specifically, a submodule declared with @racket[module] cannot
@racket[require] from its enclosing module, but the enclosing module
can @racket[require] the submodule. In contrast, a submodule declared
with @racket[module*] conceptually follows its enclosing module, so
can @racket[require] from its enclosing module, but the enclosing
module cannot @racket[require] the submodule. Unless a submodule
imports from its enclosing module or vice versa, then @tech{visits} or
@tech{instantiations} of the two modules are independent, and their
implementations may even be loaded from bytecode sources at different times.

A submodule declared with @racket[module] can import any preceding
submodule declared with @racket[module]. A submodule declared with
@racket[module*] can import any preceding module declared with
@racket[module*] and any submodule declared with @racket[module].

When a submodule declaration has the form @racket[(module* _name #f
....)], then all of the bindings of the enclosing module's bodies are
visible in the submodule's body, and the submodule implicitly imports
the enclosing module. The submodule can @racket[provide] any bindings
that it inherits from its enclosing module.

@;------------------------------------------------------------------------
@section[#:tag "mark-model"]{Continuation Frames and Marks}

@margin-note/ref{See @secref["contmarks"] for continuation-mark forms and functions.}

Every continuation @racket[_C] can be partitioned into
@deftech{continuation frames} @frame[1], @frame[2], ..., @frame["n"]
such that @racket[_C] = @*sub[@frame[1] @*sub[@frame[2] @*sub["..."
@frame["n"]]]], and no frame @frame["i"] can be itself partitioned
into smaller continuations. Evaluation steps add frames to and remove frames from
the current continuation, typically one at a time.

Each frame is conceptually annotated with a set of
@deftech{continuation marks}. A mark consists of a key and its value.
The key is an arbitrary value, and each frame includes at most one
mark for any given key. Various operations set and extract marks from
continuations, so that marks can be used to attach information to a
@tech{dynamic extent}. For example, marks can be used to record information
for a ``stack trace'' to be presented when an exception is raised, or
to implement dynamic scope.

@;------------------------------------------------------------------------
@section[#:tag "prompt-model"]{Prompts, Delimited Continuations, and Barriers}

@margin-note/ref{See @secref["cont"] for continuation and prompt functions.}

A @deftech{prompt} is a special kind of continuation frame that is
annotated with a specific @deftech{prompt tag} (essentially a
@tech{continuation mark}). Various operations allow the capture of frames in
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
another. Specifically, a continuation can be replaced by another only
when the replacement does not introduce any continuation barriers.
A continuation barrier
thus prevents ``downward jumps'' into a continuation that is protected
by a barrier. Certain operations install barriers automatically; in
particular, when an exception handler is called, a continuation
barrier prohibits the continuation of the handler from capturing the
continuation past the exception point.

An @deftech{escape continuation} is essentially a derived concept. It
combines a prompt for escape purposes with a continuation for
mark-gathering purposes. As the name implies, escape continuations are
used only to abort to the point of capture.

@;------------------------------------------------------------------------
@section[#:tag "thread-model"]{Threads}

@margin-note/ref{See @secref["concurrency"] for thread and synchronization functions.}

Racket supports multiple @deftech{threads} of evaluation.  Threads run
concurrently, in the sense that one thread can preempt another without
its cooperation. By default, however, a thread is a @deftech{coroutine thread}
that runs on the same processor
(i.e., the same underlying operating-system process and thread)
as other coroutine threads, at least within the same @tech{place}.

Threads are created explicitly by functions such as @racket[thread]. 
In terms of the evaluation model, each step in evaluation
actually deals with multiple concurrent
expressions, up to one per thread, rather than a single expression. The expressions all
share the same objects and top-level variables, so that they can
communicate through shared state, and @defterm{sequential consistency} @cite["Lamport79"] is
guaranteed among coroutine threads (i.e., the result is consistent with some global sequence
imposed on all evaluation steps across threads). Most evaluation steps involve a
single step in a single thread, but certain synchronization
primitives require multiple threads to progress together in one step; for example,
an exchange of a value through a @tech{channel} progresses in two
threads simultaneously.

Unless otherwise noted, all constant-time procedures and operations
provided by Racket are thread-safe in the sense that they are
@defterm{atomic}: they happen as a single evaluation step.
For example, @racket[set!] assigns to a variable as an atomic action
with respect to all threads, so that no thread can see a
``half-assigned'' variable. Similarly, @racket[vector-set!] assigns to
a vector atomically. Note that the evaluation of a @racket[set!]
expression with its subexpression is not necessarily atomic, because
evaluating the subexpression involves a separate step of evaluation.
Only the assignment action itself (which takes after the subexpression
is evaluated to obtain a value) is atomic. Similarly, a procedure
application can involve multiple steps that are not atomic, even if
the procedure itself performs an atomic action.

The @racket[hash-set!] procedure is not atomic, but the table is
protected by a lock; see @secref["hashtables"] for more information.
Port operations are generally not atomic, but they are thread-safe in
the sense that a byte consumed by one thread from an input port will
not be returned also to another thread, and procedures like
@racket[port-commit-peeked] and @racket[write-bytes-avail] offer
specific concurrency guarantees.

In addition to the state that is shared among all threads, each thread
has its own private state that is accessed through @deftech{thread
cells}. A thread cell is similar to a normal mutable object, but a
change to the value inside a thread cell is seen only when extracting
a value from that cell in the same thread. A thread cell can be
@deftech{preserved}; when a new thread is created, the creating
thread's value for a preserved thread cell serves as the initial value
for the cell in the created thread. For a non-preserved thread cell, a
new thread sees the same initial value (specified when the thread cell
is created) as all other threads.

A @deftech{parallel thread} is like a coroutine thread, but it can run on
a different processor (i.e., a different underlying operating-system
thread). A parallel thread can be created by calling @racket[thread]
with a @racket[#:pool] argument whose value is @racket['own] or a
parallel-thread pool.
Operations provided by Racket remain thread-safe with
parallel threads, but sequential consistency is not guaranteed
across operations and threads. If two parallel threads
share state, each read or write operation to shared state
corresponds to a read or write operation at the virtual-memory level;
Racket does not enforce additional
guarantees about reordering that might be performed at the
virtual-memory level or below, except in the case of operations that
specify such guarantees explicitly (e.g., @racket[box-cas!]). That is,
the host machine's memory model can be exposed by observations
across parallel threads.

The possibility of shared state imposes a cost on some operations,
particularly in the case of sharing among parallel threads, and
using parallel threads can easily make a computation slower than
using coroutine threads when the underlying primitives resort to a
more pessimistic mode.
@tech{Futures} and @tech{places} are alternatives to parallel threads
that provide different trade-offs in sharing constraints and performance.
@tech{Futures} sometimes achieve better performance by limiting
operations that run in parallel; as a result, they can be created and
complete more cheaply, and they can fall back more consistently
to coroutine performance in cases where parallel threads would become
slow. @tech{Places} can sometimes achieve better performance by
limiting sharing (somewhat like separate processes at the
operating-system level), so that operations can proceed more
optimistically. A place has its own set of @tech{coroutine threads}
that it schedules with sequential consistency, but the can run
in parallel to coroutine threads in other places.
Both futures and places include the possibly of
shared state, and they have the same kind of weak ordering on operations
as parallel threads.

@;------------------------------------------------------------------------
@section[#:tag "parameter-model"]{Parameters}

@margin-note/ref{See @secref["parameters"] for parameter forms and functions.}

@deftech{Parameters} are essentially a derived concept in Racket; they
are defined in terms of @tech{continuation marks} and @tech{thread
cells}. However, parameters are also ``built in,'' due to the fact that some
primitive procedures consult parameter values. For example, the
default output stream for primitive output operations is specified by
a parameter.

A parameter is a setting that is both thread-specific and
continuation-specific. In the empty continuation, each parameter
corresponds to a @tech{preserved} @tech{thread cell}; a corresponding
@deftech{parameter procedure} accesses and sets the thread cell's
value for the current thread.

In a non-empty continuation, a parameter's value is determined through
a @deftech{parameterization} that is associated with the nearest
enclosing continuation frame via a continuation mark (whose key is
not directly accessible). A parameterization maps each parameter to a
preserved thread cell, and the combination of the thread cell and the current
thread yields the parameter's value. A @tech{parameter procedure} sets or
accesses the relevant thread cell for its parameter.

Various operations, such as @racket[parameterize] or
@racket[call-with-parameterization], install a parameterization into
the current continuation's frame.

@;------------------------------------------------------------------------
@section[#:tag "exn-model"]{Exceptions}

@margin-note/ref{See @secref["exns"] for exception forms, functions, and types.}

@deftech{Exceptions} are essentially a derived concept in Racket; they
are defined in terms of continuations, prompts, and continuation
marks.  However, exceptions are also ``built in,'' due to the fact that
primitive forms and procedures may raise exceptions.

An @deftech{exception handler} to @deftech{catch} exceptions can be associated
with a continuation frame though a @tech{continuation mark} (whose key
is not directly accessible). When an exception is raised, the current
continuation's marks determine a chain of @tech{exception handler}
procedures that are consulted to handle the exception.
A handler for uncaught exceptions is designated through a built-in @tech{parameter}.

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
sockets}, @tech{byte converters}, and @tech{places}.  Whenever a thread, @|etc|, is
created, it is placed under the management of the @deftech{current
custodian} as determined by the @racket[current-custodian]
@tech{parameter}.

@margin-note{Custodians also manage eventspaces from
             @racketmodname[racket/gui/base].}

Except for the root custodian, every @tech{custodian} itself is
managed by a @tech{custodian}, so that custodians form a hierarchy.
Every object managed by a subordinate custodian is also managed by the
custodian's owner.

When a @tech{custodian} is shut down via
@racket[custodian-shutdown-all], it forcibly and immediately closes
the ports, TCP connections, @|etc|, that it manages, as well as
terminating (or suspending) its threads. A custodian that has been
shut down cannot manage new objects.  After the current custodian is shut
down, if a procedure is called that attempts to create a managed resource (e.g.,
@racket[open-input-file], @racket[thread]), then the
@exnraise[exn:fail:contract].

A thread can have multiple managing custodians, and a suspended thread
created with @racket[thread/suspend-to-kill] can have zero
custodians. Extra custodians become associated with a thread through
@racket[thread-resume] (see @secref["threadkill"]). When a thread
has multiple custodians, it is not necessarily killed by a
@racket[custodian-shutdown-all]. Instead, shut-down custodians are removed
from the thread's managing custodian set, and the thread is killed when its
managing set becomes empty.

The values managed by a custodian are semi-weakly held by the
custodian: a @techlink{will} can be executed for a value that is
managed by a custodian; in addition, weak references via weak
@tech{hash tables}, @tech{ephemerons}, or @tech{weak box}es can be
dropped on the @tech{BC} implementation of Racket, but not on the @tech{CS}
implementation. For all variants, a custodian only weakly
references its subordinate custodians; if a subordinate custodian is
unreferenced but has its own subordinates, then the custodian may be
garbage collected, at which point its subordinates become immediately
subordinate to the collected custodian's superordinate (owner) custodian.

In addition to the other entities managed by a custodian, a
@deftech{custodian box} created with @racket[make-custodian-box]
strongly holds onto a value placed in the box until the box's
custodian is shut down. However, the custodian only weakly retains the box
itself, so the box and its content can be collected if there
are no other references to them.

When Racket is compiled with support for per-custodian memory
accounting (see @racket[custodian-memory-accounting-available?]), the
@racket[current-memory-use] procedure can report a custodian-specific
result.  This result determines how much memory is occupied by objects
that are @tech{reachable} from the custodian's managed values, especially its
threads, and including its sub-custodians' managed values. If an
object is reachable from two custodians where neither is an ancestor
of the other, an object is arbitrarily charged to one or the other,
and the choice can change after each collection; objects reachable
from both a custodian and its descendant, however, are reliably
charged to the custodian and not to the descendants, unless the
custodian can reach the objects only through a descendant custodian or
a descendant's thread.  Reachability for per-custodian accounting does
not include weak references, references to threads managed by other
custodians, references to other custodians, or references to custodian
boxes for other custodians.
