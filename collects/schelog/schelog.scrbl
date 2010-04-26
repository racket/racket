#lang scribble/manual
@(require scribble/eval
          (for-syntax scheme)
          (for-label schelog
                     (except-in scheme _)))

@(define schelog-eval (make-base-eval))
@(schelog-eval '(require schelog))

@title{@bold{Schelog}: Prolog-style logic programming in Scheme}

@author{Dorai Sitaram}

@margin-note{Adapted for Racket by Dorai Sitaram, John Clements, and Jay McCarthy.}

@defmodule[schelog]

Schelog is an @emph{embedding} of
Prolog-style logic programming in Scheme.  ``Embedding''
means you don't lose Scheme: You can use Prolog-style and
conventional Scheme code fragments alongside each other.
Schelog contains the full repertoire of Prolog features,
including meta-logical and second-order (``set'')
predicates, leaving out only those features that could more
easily and more efficiently be done with Scheme
subexpressions.

The Schelog implementation uses the approach to logic
programming described in Felleisen @cite{mf:prolog} and
Haynes @cite{logick}.  In contrast to earlier Lisp simulations of
Prolog @cite{campbell},
which used explicit continuation
arguments to store failure (backtrack) information, the
Felleisen and Haynes model uses the implicit reified
continuations of Scheme as provided by the operator
@scheme[call-with-current-continuation] (aka @scheme[call/cc]).  This
allows Schelog to be an @emph{embedding}, ie, logic
programming is not built as a new language on top of Scheme,
but is used alongside Scheme's other features.  Both styles
of programming may be mixed to any extent that a project
needs.

The Schelog user does not need to know about the
implementation mechanism or about @scheme[call/cc] and
continuations to get on with the business of
doing logic programming with Schelog.

This text is a gentle introduction to Schelog syntax
and programming.  It assumes a working knowledge of
Scheme and an awareness of, if not actual programming
experience with, Prolog.  If you need assistance in
either language, you may consult
@cite["sicp" "tls" "tss" "eopl" "r5rs" "t-y-scheme"] for Scheme, and
@cite["bratko" "ok:prolog" "aop"] for Prolog.
There are doubtless many other excellent books and
online documents available.

@table-of-contents[]

@section[#:tag "simple"]{Simple Goals and Queries}

Schelog objects are the same as Scheme objects.  However, there
are two subsets of these objects that are of special
interest to Schelog: @emph{goals} and @emph{predicates}.  We
will first look at some simple goals.
@secref{predicates} will introduce predicates and ways
of making  complex goals using predicates.

A goal is an object whose truth or falsity we can check.  A
goal that turns out to be true is said to succeed.
A goal that turns out to be false is said to
fail.

Two simple goals that are provided in Schelog are:
@schemeblock[
%true
%fail
]

The  goal @scheme[%true] succeeds.  The goal @scheme[%fail]
always fails.

(The names of all Schelog primitive objects
start with @litchar{%}.  This is to avoid clashes with the names
of conventional Scheme objects of related meaning.
User-created objects in Schelog are not required to
follow this convention.)

A Schelog user can @emph{query} a goal by wrapping it in a
@scheme[%which]-form.

@schemeblock[
(%which () %true)
]

evaluates to @schemeresult[()], indicating success, whereas:

@schemeblock[
(%which () %fail)
]

evaluates to @scheme[#f], indicating failure.

Note 1: The second subexpression of the @scheme[%which]-form
is the empty list @schemeresult[()].  Later (@secref{solving-goals}),
we will see @scheme[%which]es
with other lists as the second subform.

Henceforth, we will use the notation:

@interaction[(eval:alts E 'F)]

to say that @scheme[E] @emph{evaluates to} @scheme[F]. Thus,

@interaction[#:eval schelog-eval (%which () %true)]

@section[#:tag "predicates"]{Predicates}

More interesting goals are created by applying a special
kind of Schelog object called a @emph{predicate} (or
@emph{relation}) to other
Schelog objects.  Schelog comes with some primitive
predicates, such as the arithmetic operators
@scheme[%=:=] and @scheme[%<],
standing for arithmetic ``equal'' and ``less than''
respectively.  For example, the following are some goals
involving these predicates:

@interaction[
 #:eval schelog-eval
 (%which () (%=:= 1 1))
 (%which () (%< 1 2))
 (%which () (%=:= 1 2))
 (%which () (%< 1 1))
 ]

Other arithmetic predicates are
@scheme[%>] (``greater than''),
@scheme[%<=] (``less than or equal''),
@scheme[%>=] (``greater than or equal''), and
@scheme[%=/=] (``not equal'').

Schelog predicates are not to be confused with conventional
Scheme predicates (such as @scheme[<] and @scheme[=]).  Schelog
predicates, when applied to arguments, produce goals
that
may either succeed or fail.  Scheme predicates, when applied
to arguments, yield a boolean value.  Henceforth, we will
use the term ``predicate'' to mean Schelog predicates.
Conventional predicates will be explicitly called ``Scheme
predicates''.

@subsection[#:tag "facts"]{Predicates Introducing Facts}

Users can create their own predicates using the Schelog form
@scheme[%rel].  For example, let's
define the predicate @scheme[%knows]:

@schemeblock+eval[#:eval schelog-eval
(define %knows
  (%rel ()
    [('Odysseus 'TeX)]
    [('Odysseus 'Scheme)]
    [('Odysseus 'Prolog)]
    [('Odysseus 'Penelope)]
    [('Penelope 'TeX)]
    [('Penelope 'Prolog)]
    [('Penelope 'Odysseus)]
    [('Telemachus 'TeX)]
    [('Telemachus 'calculus)]))
]

The expression has the expected meaning.  Each
@emph{clause} in the @scheme[%rel] establishes a @emph{fact}:
Odysseus
knows TeX, Telemachus knows calculus, &c.  In general, if we
apply the predicate to the arguments in any one of its
clauses, we will get a successful goal.  Thus, since
@scheme[%knows] has a clause that reads
@scheme[[('Odysseus 'TeX)]], the goal
@scheme[(%knows 'Odysseus 'TeX)]
will be true.

We can now get answers for the following types of queries:

@interaction[#:eval schelog-eval
(%which ()
  (%knows 'Odysseus 'TeX))
(%which ()
  (%knows 'Telemachus 'Scheme))
]

@subsection[#:tag "rules"]{Predicates with Rules}

Predicates can be more complicated than the above bald
recitation of facts.  The predicate clauses can be @emph{rules}, eg,

@schemeblock+eval[#:eval schelog-eval
(define %computer-literate
  (%rel (person)
    [(person)
      (%knows person 'TeX)
      (%knows person 'Scheme)]
    [(person)
      (%knows person 'TeX)
      (%knows person 'Prolog)]))
]

This defines the predicate
@scheme[%computer-literate] in
terms of the predicate @scheme[%knows].  In effect, a person is
defined as computer-literate if they know TeX and
Scheme, @emph{or} TeX and Prolog.

Note that this use of
@scheme[%rel] employs a local @emph{logic variable} called @scheme[_person].
In general, a @scheme[%rel]-expression can have a list of symbols
as its second subform.  These name new logic variables that
can be used within the body of the @scheme[%rel].

The following query can now be answered:

@interaction[#:eval schelog-eval
(%which ()
  (%computer-literate 'Penelope))
]

Since Penelope knows TeX and Prolog, she is computer-literate.

@subsection[#:tag "solving-goals"]{Solving Goals}

The above queries are yes/no questions.  Logic programming
allows more: We can formulate a goal with @emph{uninstantiated}
logic variables and then ask the querying process to
provide, if possible, values for these variables that cause
the goal to succeed.  For instance, the query:

@interaction[#:eval schelog-eval
(%which (what)
  (%knows 'Odysseus what))
]

asks for an instantiation of the logic variable @scheme[_what]
that satisfies the goal @scheme[(%knows 'Odysseus what)].
In other words, we are asking, ``What does Odysseus know?''

Note that this use of @scheme[%which] --- like @scheme[%rel]
in the definition of @scheme[%computer-literate] ---
uses a local logic
variable, @scheme[_what].  In general, the second subform of
@scheme[%which] can be a list of local logic variables.  The
@scheme[%which]-query returns an answer that is a list of
bindings, one for each logic variable mentioned in its
second subform.  Thus,

@interaction[#:eval schelog-eval
(%which (what)
  (%knows 'Odysseus what))
]

But that is not all that wily Odysseus knows.  Schelog
provides a zero-argument procedure (``thunk'') called
@scheme[%more]
that @emph{retries} the goal in the last
@scheme[%which]-query for a different solution.

@interaction[#:eval schelog-eval
(%more)
]

We can keep pumping for more solutions:

@interaction[#:eval schelog-eval
(%more)
(%more)
(%more)
]

The final @scheme[#f] shows that there are no more
solutions.  This is because there are no more clauses in the
@scheme[%knows] predicate that list Odysseus as knowing anything
else.

@subsection[#:tag "assert"]{Asserting Extra Clauses}

We can add more clauses to a predicate after it has already
been defined with a @scheme[%rel].  Schelog provides the
@scheme[%assert] form for this purpose.  Eg,

@schemeblock+eval[#:eval schelog-eval
(%assert %knows ()
  [('Odysseus 'archery)])
]

tacks on a new clause at the end of the existing clauses
of the @scheme[%knows]
predicate.  Now, the query:

@interaction[#:eval schelog-eval
(%which (what)
  (%knows 'Odysseus what))
]

gives TeX, Scheme, Prolog, and Penelope, as before, but
a subsequent @scheme[(%more)] yields a new result:
@interaction-eval[#:eval schelog-eval (begin (%more) (%more) (%more))]
@interaction[#:eval schelog-eval
(%more)
]

The Schelog form @scheme[%assert-a] is similar to @scheme[%assert] but
adds clauses @emph{before} any of the current clauses.

Both @scheme[%assert] and @scheme[%assert-a] assume that the variable
they are adding to already names a predicate (presumably
defined using @scheme[%rel]).
In order to allow defining a predicate entirely through
@scheme[%assert]s,  Schelog provides an empty predicate value
@scheme[%empty-rel].  @scheme[%empty-rel] takes any number of arguments
and always fails.  A typical use of the
@scheme[%empty-rel] and @scheme[%assert] combination:

@schemeblock+eval[#:eval schelog-eval
(define %parent %empty-rel)

(%assert %parent ()
  [('Laertes 'Odysseus)])

(%assert %parent ()
  [('Odysseus 'Telemachus)]
  [('Penelope 'Telemachus)])
]

(Schelog does not provide a predicate for @emph{retracting}
assertions, since we can keep track of older versions of
predicates using conventional Scheme features (@scheme[let] and @scheme[set!]).)

@subsection[#:tag "local-vars"]{Local Variables}

The local logic variables of @scheme[%rel]- and
@scheme[%which]-expressions are in reality introduced by the
Schelog syntactic form called @scheme[%let].  (@scheme[%rel] and
@scheme[%which] are macros written using @scheme[%let].)

@scheme[%let] introduces new lexically scoped logic variables.
Supposing, instead of

@interaction[#:eval schelog-eval
(%which (what)
  (%knows 'Odysseus what))
]

we had asked

@interaction[#:eval schelog-eval
(%let (what)
  (%which ()
    (%knows 'Odysseus what)))
]

This query, too, succeeds five times, since
Odysseus knows five things.  However, @scheme[%which] emits
bindings only for the local variables that @emph{it}
introduces.  Thus, this query emits @schemeresult[()] five times before
@scheme[(%more)] finally returns @scheme[#f].

@section[#:tag "scheme-w-schelog"]{Using Conventional Scheme Expressions in Schelog}

The arguments of Schelog predicates can be any Scheme
objects.  In particular, composite structures such as lists,
vectors and strings can be used, as also Scheme expressions
using the full array of Scheme's construction and
decomposition operators.  For instance, consider the
following goal:

@schemeblock[
(%member x '(1 2 3))
]

Here, @scheme[%member] is a predicate, @scheme[x] is a logic
variable, and @scheme['(1 2 3)] is a structure.  Given a suitably
intuitive definition for @scheme[%member], the above goal
succeeds for @scheme[x] = @schemeresult[1], @schemeresult[2], and @schemeresult[3].

Now to defining predicates like @scheme[%member]:

@schemeblock[
(define %member
  (%rel (x y xs)
    [(x (cons x xs))]
    [(x (cons y xs))
      (%member x xs)]))
]

Ie, @scheme[%member] is defined with three local variables:
@scheme[x],  @scheme[y], @scheme[xs].  It  has two
clauses, identifying the two ways of determining membership.

The first clause of @scheme[%member] states a fact: For any
@scheme[x], @scheme[x] is a member of a list whose head is also @scheme[x].

The second clause of @scheme[%member] is a rule: @scheme[x] is a
member of a list if we can show that it is a member of the
@emph{tail} of that list.  In other words, the original
@scheme[%member] goal is translated into a @emph{sub}goal, which is also
a @scheme[%member] goal.

Note that the variable @scheme[y] in the definition of
@scheme[%member] occurs only once in the second clause.  As such,
it doesn't need you to make the effort of naming it.  (Names
help only in matching a second occurrence to a first.)  Schelog
lets you use the expression @scheme[(_)] to denote an anonymous
variable.  (Ie, @scheme[_] is a thunk that generates a fresh
anonymous variable at each call.)  The predicate @scheme[%member] can be
rewritten as

@schemeblock[
(define %member
  (%rel (x xs)
    [(x (cons x (_)))]
    [(x (cons (_) xs))
      (%member x xs)]))
]

@subsection[#:tag "constructors"]{Constructors}

We can use constructors --- Scheme procedures for creating
structures --- to simulate data types in Schelog.  For
instance, let's define a natural-number data-type where
@scheme[0] denotes zero, and @scheme[(succ x)] denotes the natural number
whose immediate predecessor is @scheme[x].   The constructor
@scheme[succ] can
be defined in Scheme as:

@schemeblock+eval[#:eval schelog-eval
(define succ
  (lambda (x)
    (vector 'succ x)))
]

Addition and multiplication can be defined as:

@schemeblock+eval[#:eval schelog-eval
(define %add
  (%rel (x y z)
    [(0 y y)]
    [((succ x) y (succ z))
      (%add x y z)]))

(define %times
  (%rel (x y z z1)
    [(0 y 0)]
    [((succ x) y z)
     (%times x y z1)
     (%add y z1 z)]))
]

We can do a lot of arithmetic with this in place.  For
instance, the factorial predicate looks like:

@schemeblock+eval[#:eval schelog-eval
(define %factorial
  (%rel (x y y1)
    [(0 (succ 0))]
    [((succ x) y)
      (%factorial x y1)
      (%times (succ x) y1 y)]))
]

@subsection[#:tag "is"]{@scheme[\%is]}

The above is a very inefficient way to do arithmetic,
especially when the underlying language Scheme offers
excellent arithmetic facilities (including a comprehensive
number ``tower'' and exact rational arithmetic).  One
problem with using Scheme calculations directly in Schelog
clauses is that the expressions used may contain logic
variables that need to be dereferenced.  Schelog provides
the predicate @scheme[%is] that takes care of this.  The goal

@schemeblock[
(%is _X _E)
]

unifies @scheme[_X] with the value of @scheme[_E] considered as a
Scheme expression.  @scheme[_E] can have logic variables, but
usually they should at least be bound, as unbound variables
may not be palatable values to the Scheme operators used in
@scheme[_E].

We can now directly use the numbers of Scheme to write a
more efficient @scheme[%factorial] predicate:

@schemeblock+eval[#:eval schelog-eval
(define %factorial
  (%rel (x y x1 y1)
    [(0 1)]
    [(x y) (%is x1 (- x 1))
           (%factorial x1 y1)
           (%is y (* y1 x))]))
]

A price that this efficiency comes with is that we can
use @scheme[%factorial] only with its first argument already
instantiated.  In many cases, this is not an unreasonable
constraint.  In fact, given this limitation, there is
nothing to prevent us from using Scheme's factorial
directly:

@schemeblock+eval[#:eval schelog-eval
(define %factorial
  (%rel (x y)
    [(x y)
     (%is y (scheme-factorial
	      x))]))
]

or better yet, ``in-line'' any calls to @scheme[%factorial] with
@scheme[%is]-expressions calling @scheme[scheme-factorial], where the
latter is defined in the usual manner:

@schemeblock+eval[#:eval schelog-eval
(define scheme-factorial
  (lambda (n)
    (if (= n 0) 1
        (* n (factorial
	       (- n 1))))))
]

@subsection[#:tag "lexical-scoping"]{Lexical Scoping}

One can use Scheme's lexical scoping to enhance predicate
definition.  Here is a list-reversal predicate defined using
a hidden auxiliary predicate:

@schemeblock+eval[#:eval schelog-eval
(define %reverse
  (letrec
    ([revaux
       (%rel (x y z w)
	 [('() y y)]
	 [((cons x y) z w)
	   (revaux y
	     (cons x z) w)])])
    (%rel (x y)
      [(x y) (revaux x '() y)])))
]

@scheme[(revaux _X _Y _Z)] uses @scheme[_Y] as an accumulator for
reversing @scheme[_X] into @scheme[_Z].  (@scheme[_Y] starts out as @schemeresult[()].
Each head of @scheme[_X] is @scheme[cons]ed on to @scheme[_Y].  Finally, when
@scheme[_X] has wound down to @schemeresult[()], @scheme[_Y] contains the reversed
list and can be returned as @scheme[_Z].)

@scheme[revaux] is used purely as a helper predicate for
@scheme[%reverse], and so it can be concealed within a lexical
contour.  We use @scheme[letrec] instead of @scheme[let] because
@scheme[revaux] is a recursive procedure.

@subsection[#:tag "type-predicates"]{Type Predicates}

Schelog provides a couple of predicates that let the user
probe the type of objects.

The goal
@schemeblock[
(%constant _X)
]

succeeds if @scheme[_X] is an @emph{atomic} object, ie, not a
list or vector.

The predicate @scheme[%compound], the negation of @scheme[%constant],
checks if its argument is  indeed a list or a vector.

The above are merely the logic-programming equivalents of
corresponding Scheme predicates.  Users can use the
predicate @scheme[%is] and Scheme predicates to write more type
checks in Schelog.  Thus, to test if @scheme[_X] is a string, the
following goal could be used:

@schemeblock[
(%is #t (string? _X))
]

User-defined Scheme predicates, in addition to primitive Scheme
predicates, can be thus imported.

@section[#:tag "backtracking"]{Backtracking}

It is helpful to go into the following evaluation (@secref{rules})
in a
little detail:

@schemeblock+eval[#:eval schelog-eval
(%which ()
  (%computer-literate 'Penelope))
]

The starting goal
is:

@(define goal litchar)
@schemeblock[
G0 = (%computer-literate Penelope)
]

(I've taken out the quote because @schemeresult[Penelope] is the result
of evaluating @scheme['Penelope].)

Schelog tries to match this with the head of the first
clause of @scheme[%computer-literate].  It succeeds, generating a
binding  @scheme[[person Penelope]].

But this means it now has two new goals --- @emph{subgoals}
--- to solve.  These are the goals in the body of the
matching clause, with the logic variables substituted by
their instantiations:

@schemeblock[
G1 = (%knows Penelope TeX)
G2 = (%knows Penelope Scheme)
]

For @goal{G1}, Schelog attempts matches with the clauses of
@scheme[%knows], and succeeds at the fifth try.  (There are no
subgoals in this case, because the bodies of these ``fact''
clauses are empty, in contrast to the ``rule'' clauses of
@scheme[%computer-literate].)
Schelog then tries to solve @goal{G2} against the clauses of
@scheme[%knows], and since there is no clause stating that
Penelope knows Scheme, it fails.

All is not lost though.  Schelog now @emph{backtracks} to the
goal that was solved just before, viz., @goal{G1}.  It
@emph{retries} @goal{G1}, ie, tries to solve it in a
different way.
This entails searching down the previously unconsidered
@scheme[%knows]
clauses for @goal{G1}, ie, the sixth onwards.  Obviously,
Schelog fails again, because the fact that Penelope knows
TeX occurs only once.

Schelog now backtracks to the goal before @goal{G1}, ie,
@goal{G0}.  We abandon the current successful match with the
first clause-head of @scheme[%computer-literate], and try the
next clause-head.  Schelog succeeds, again producing a binding
@scheme[[person Penelope]], and two new subgoals:

@schemeblock[
G3 = (%knows Penelope TeX)
G4 = (%knows Penelope Prolog)
]

It is now easy to trace that Schelog finds both @goal{G3} and @goal{G4} to be
true.  Since both of @goal{G0}'s subgoals are true, @goal{G0} is
itself considered true.  And this is what Schelog reports.  The
interested reader can now trace  why the
following query has a different denouement:

@interaction[#:eval schelog-eval
(%which ()
  (%computer-literate 'Telemachus))
]

@section[#:tag "unification"]{Unification}

When we say that a goal matches with a clause-head, we mean
that the predicate and argument positions line up.  Before
making this comparison, Schelog dereferences all already
bound logic variables.  The resulting structures are then
compared to see if they are recursively identical.  Thus,
@scheme[1] unifies with @scheme[1], and @scheme[(list 1 2)] with @scheme['(1 2)]; but @scheme[1] and
@scheme[2] do not unify, and neither do @scheme['(1 2)] and @scheme['(1 3)].

In general, there could be quite a few uninstantiated logic
variables in the compared objects.  Unification will then
endeavor to find the most natural way of binding these
variables so that we arrive at structurally identical
objects.  Thus, @scheme[(list _x 1)], where @scheme[_x] is an unbound logic
variable, unifies with @scheme['(0 1)], producing the
binding
@scheme[[_x 0]].

Unification is thus a goal, and Schelog makes the unification predicate
available  to the user as @scheme[%=].   Eg,

@interaction[#:eval schelog-eval
(%which (x)
  (%= (list x 1) '(0 1)))
]

Schelog also provides the predicate @scheme[%/=], the @emph{negation} of
@scheme[%=].  @scheme[(%/= _X _Y)] succeeds if and only if @scheme[_X] does
@emph{not} unify with @scheme[_Y].

Unification goals constitute the basic subgoals that all
Schelog goals devolve to.  A goal succeeds because all the
eventual unification subgoals that it decomposes to in at
least one of its subgoal-branching succeeded.  It fails
because every possible subgoal-branching was thwarted by the
failure of a crucial unification subgoal.

Going back to the example in @secref{backtracking}, the goal
@scheme[(%computer-literate 'Penelope)] succeeds because
(a) it unified with
@scheme[(%computer-literate person)]; and then (b) with the binding
@scheme[[person Penelope]] in place, @scheme[(%knows person 'TeX)]
unified with @scheme[(%knows 'Penelope 'TeX)] and
@scheme[(%knows person 'Prolog)] unified with @scheme[(%knows 'Penelope 'Prolog)].

In contrast, the goal @scheme[(%computer-literate 'Telemachus)]
fails because, with @scheme[[person Telemachus]],
the subgoals @scheme[(%knows person 'Scheme)] and
@scheme[(%knows person 'Prolog)] have no facts they can
unify with.

@subsection{The Occurs Check}

A robust unification algorithm uses the @deftech{occurs check}, which ensures that a logic variable
isn't bound to a structure that contains itself.  
Not performing the check can cause the unification
to go into an infinite loop in some cases.  On the
other hand, performing the occurs check greatly
increases the time taken by unification, even in cases
that wouldn't require the check.

Schelog uses the global parameter
@scheme[use-occurs-check?] to decide whether to
use the occurs check.  By default, this variable is 
@scheme[#f], ie, Schelog disables the occurs check.  To
enable the check, 

@schemeblock[
(use-occurs-check? #t)
]

@section[#:tag "and-or"]{Conjuctions and Disjunctions}

Goals may be combined using the forms @scheme[%and]
and @scheme[%or]
to form compound goals.  (For @scheme[%not], see @secref{not}.)
Eg,

@interaction[#:eval schelog-eval
(%which (x)
  (%and (%member x '(1 2 3))
        (%< x 3)))
]

gives solutions for @scheme[_x] that satisfy both the
argument goals of the @scheme[%and].
Ie, @scheme[_x] should both be a member of @scheme['(1 2 3)]
@emph{and}  be less than @scheme[3]. Typing @scheme[(%more)] gives another solution:

@interaction[#:eval schelog-eval
(%more)
(%more)
]

There are no more solutions, because @scheme[[x 3]] satisfies
the first but not the second goal.

Similarly, the query

@interaction[#:eval schelog-eval
(%which (x)
  (%or (%member x '(1 2 3))
       (%member x '(3 4 5))))
]

lists all @scheme[_x] that are members of either list.

@interaction[#:eval schelog-eval
(%more)
(%more)
(%more)
(%more)
(%more)
]

(Yes, @scheme[([x 3])] is listed twice.)

We can rewrite the predicate @scheme[%computer-literate]
from @secref{rules} using @scheme[%and] and @scheme[%or]:

@schemeblock+eval[#:eval schelog-eval
(define %computer-literate
  (%rel (person)
    [(person)
     (%or
       (%and (%knows person
	       'TeX)
	     (%knows person
	       'Scheme))
       (%and (%knows person
	       'TeX)
	     (%knows person
	       'Prolog)))]))
]

Or,  more succinctly:

@schemeblock+eval[#:eval schelog-eval
(define %computer-literate
  (%rel (person)
    [(person)
      (%and (%knows person
	      'TeX)
        (%or (%knows person
	       'Scheme)
	     (%knows person
	       'Prolog)))]))
]

We can even dispense with the @scheme[%rel] altogether:

@schemeblock+eval[#:eval schelog-eval
(define %computer-literate
  (lambda (person)
    (%and (%knows person
	    'TeX)
      (%or (%knows person
	     'Scheme)
	(%knows person
	  'Prolog)))))
]

This last looks like a conventional Scheme predicate
definition, and is arguably
the most readable format for a Scheme programmer.

@section[#:tag "lv-manip"]{Manipulating Logic Variables}

Schelog provides special predicates for probing logic
variables, without risking their getting bound.

@subsection[#:tag "var"]{Checking for Variables}

The goal

@schemeblock[
(%== _X _Y)
]

succeeds if @scheme[_X] and @scheme[_Y] are @emph{identical} objects.  This
is not quite the unification predicate @scheme[%=], for @scheme[%==]
doesn't touch unbound objects the way @scheme[%=] does.  Eg,
@scheme[%==] will not equate an unbound logic variable with a
bound one, nor will it equate two unbound logic variables
unless they are the @emph{same} variable.

The predicate @scheme[%/==] is the negation of @scheme[%==].

The goal

@schemeblock[
(%var _X)
]

succeeds if @scheme[_X] isn't completely bound --- ie, it has at
least one unbound logic variable in its innards.

The predicate @scheme[%nonvar] is the negation of @scheme[%var].

@subsection[#:tag "freeze"]{Preserving Variables}

Schelog lets the user protect a term with variables from
unification by allowing that term to be treated as a
(completely) bound object.  The predicates provided for this
purpose are
@scheme[%freeze],
@scheme[%melt], @scheme[%melt-new], and @scheme[%copy].

The goal

@schemeblock[
(%freeze _S _F)
]

unifies @scheme[_F] to the frozen version of @scheme[_S].  Any lack
of bindings in @scheme[_S] are preserved no matter how much you
toss @scheme[_F] about.

The goal

@schemeblock[
(%melt _F _S)
]

retrieves the object frozen in @scheme[_F] into @scheme[_S].

The goal

@schemeblock[
(%melt-new _F _S)
]

is similar to @scheme[%melt],
except that when @scheme[_S] is made,  the unbound variables in
@scheme[_F] are replaced by brand-new unbound variables.

The goal

@schemeblock[
(%copy _S _C)
]

is an abbreviation for @scheme[(%freeze _S _F)]
followed by @scheme[(%melt-new _F _C)].

@section[#:tag "cut"]{The Cut (@scheme[!])}

The cut (called @scheme[!]) is a special goal that is used to
prune backtracking options.  Like the @scheme[%true] goal, the
cut goal too succeeds, when accosted by the Schelog
subgoaling engine.  However, when a further subgoal down the
line fails, and time comes to retry the cut goal, Schelog
will refuse to try alternate clauses for the predicate in
whose definition the cut occurs.  In other words, the cut
causes Schelog to commit to all the decisions made from the
time that the predicate was selected to match a subgoal till
the time the cut was satisfied.

For example, consider again the @scheme[%factorial]
predicate, as defined in @secref{is}:

@schemeblock+eval[#:eval schelog-eval
(define %factorial
  (%rel (x y x1 y1)
    [(0 1)]
    [(x y) (%is x1 (- x 1))
           (%factorial x1 y1)
           (%is y (* y1 x))]))
]

Clearly,

@interaction[#:eval schelog-eval
(%which ()
  (%factorial 0 1))
(%which (n)
  (%factorial 0 n))
]

But what if we asked for @scheme[(%more)] for either query?
Backtracking will try
the second clause of @scheme[%factorial], and sure enough the
clause-head unifies, producing binding @scheme[[x 0]].
We now get three subgoals.  Solving the first, we get @scheme[[x1 -1]], and then we have to solve @scheme[(%factorial -1 y1)].  It
is easy to see there is no end to this, as we fruitlessly
try to get the factorials of numbers that get more and more
negative.

If we placed a cut at the first clause:

@schemeblock[
...
[(0 1) !]
...
]

the attempt to find more solutions for @scheme[(%factorial 0 1)] is nipped in the bud.

Calling @scheme[%factorial] with a @emph{negative} number would still cause an
infinite loop.   To take care of that problem as well, we
use another cut:

@schemeblock+eval[#:eval schelog-eval
(define %factorial
  (%rel (x y x1 y1)
    [(0 1) !]
    [(x y) (%< x 0) ! %fail]
    [(x y) (%is x1 (- x 1))
           (%factorial x1 y1)
           (%is y (* y1 x))]))
]

@interaction[#:eval schelog-eval
(%which ()
  (%factorial 0 1))
(%more)
(%which ()
  (%factorial -1 1))
]

Using @emph{raw} cuts as above can get very confusing.  For this
reason, it is advisable to use it hidden away in
well-understood abstractions.  Two such common abstractions
are the conditional and negation.

@subsection[#:tag "if-then-else"]{Conditional Goals}

An ``if ... then ... else ...'' predicate can be defined
as follows

@schemeblock+eval[#:eval schelog-eval
(define %if-then-else
  (%rel (p q r)
    [(p q r) p ! q]
    [(p q r) r]))
]

(Note that for the first time we have predicate arguments that
are themselves goals.)

Consider the goal

@schemeblock[
G0 = (%if-then-else Gbool Gthen Gelse)
]

We first unify @goal{G0} with the first clause-head,
giving
@scheme[[p Gbool]], @scheme[[q Gthen]], @scheme[[r Gelse]].  @goal{Gbool} can
now either succeed or fail.

Case 1:  If @goal{Gbool} fails, backtracking will cause the
@goal{G0} to unify with the second clause-head.  @scheme[r] is bound
to @goal{Gelse}, and so @goal{Gelse} is tried, as expected.

Case 2: If @goal{Gbool} succeeds, the cut commits to this
clause of the @scheme[%if-then-else].  We now try @goal{Gthen}.  If
@goal{Gthen} should now fail --- or even if we simply retry for
more solutions --- we are guaranteed that the second
clause-head will not be tried.  If it were not for the cut,
@goal{G0} would attempt to unify with the second clause-head, which will
of course succeed, and @goal{Gelse} @emph{will} be tried.

@subsection[#:tag "not"]{Negation as Failure}

Another common abstraction using the cut is @emph{negation}.
The negation of goal @goal{G} is defined as @scheme[(%not G)], where
the predicate @scheme[%not] is defined as follows:

@schemeblock+eval[#:eval schelog-eval
(define %not
  (%rel ()
    [(g) g ! %fail]
    [(g) %true]))
]

Thus, @scheme[g]'s negation is deemed a failure if @scheme[g]
succeeds, and a success if @scheme[g] fails.  This is of course
confusing goal failure with falsity.  In some cases, this
view of negation is actually helpful.

@section[#:tag "set-of"]{Set Predicates}

The goal

@schemeblock[
(%bag-of _X _G _Bag)
]

unifies with @scheme[_Bag] the list of all instantiations of
@scheme[_X] for which @scheme[_G] succeeds.  Thus, the following query
asks for all the things known --- ie, the collection of things
such that someone knows them:

@interaction[#:eval schelog-eval
(%which (things-known)
  (%let (someone x)
    (%bag-of x (%knows someone x)
      things-known)))
]

This is the only solution for this goal:

@interaction[#:eval schelog-eval
(%more)
]

Note that some things --- eg, TeX --- are enumerated
more than once.  This is because more than one person knows
TeX.  To remove duplicates, use the predicate
@scheme[%set-of]
instead of @scheme[%bag-of]:

@interaction[#:eval schelog-eval
(%which (things-known)
  (%let (someone x)
    (%set-of x (%knows someone x)
      things-known)))
]

In the above, the free variable @scheme[_someone] in the
@scheme[%knows]-goal is used as if it
were existentially quantified.  In contrast, Prolog's
versions of
@scheme[%bag-of] and @scheme[%set-of] fix it for each solution of the
set-predicate goal.  We can do it too with some additional
syntax that identifies the free variable.
Eg,

@interaction[#:eval schelog-eval
(%which (someone things-known)
  (%let (x)
    (%bag-of x
      (%free-vars (someone)
        (%knows someone x))
      things-known)))
]

The bag of things known by @emph{one} someone is
returned.  That someone is Odysseus.  The query can be
retried for more solutions, each listing the things known by
a different someone:

@interaction[#:eval schelog-eval
(%more)
(%more)
(%more)
(%more)
]

Schelog also provides two variants of these set predicates,
viz., @scheme[%bag-of-1] and @scheme[%set-of-1].  These act like @scheme[%bag-of]
and @scheme[%set-of] but fail if the resulting bag or set is empty.

@section[#:tag "glossary"]{Glossary of Schelog Primitives}

@; XXX any/c should be unifiable?
@; XXX logic-variable? goal? answer?

@(define-syntax (defpred stx)
   (syntax-case stx ()
     [(_ (id arg ...) pre ...)
        (syntax/loc stx
          (defproc (id arg ...)
            goal?
            pre ...))]))
@(define-syntax-rule (defgoal id pre ...)
   (defthing id goal? pre ...))

@defpred[(%/= [E1 any/c] [E2 any/c])]{@scheme[%/=] is the negation of @scheme[%=].
The goal @scheme[(%/= E1 E2)] succeeds if @scheme[E1] can not be unified
with @scheme[E2].}

@defpred[(%/== [E1 any/c] [E2 any/c])]{
@scheme[%/==] is the negation of @scheme[%==].
The goal @scheme[(%/== E1 E2)] succeeds if @scheme[E1] and @scheme[E2] are not
identical.}

@defpred[(%< [E1 any/c] [E2 any/c])]{
The goal @scheme[(%< E1 E2)] succeeds if @scheme[E1] and @scheme[E2] are bound to
numbers and @scheme[E1] is less than @scheme[E2].}

@defpred[(%<= [E1 any/c] [E2 any/c])]{
The goal @scheme[(%<= E1 E2)] succeeds if @scheme[E1] and @scheme[E2] are bound to
numbers and @scheme[E1] is less than or equal to @scheme[E2].}

@defpred[(%= [E1 any/c] [E2 any/c])]{
The goal @scheme[(%= E1 E2)] succeeds if @scheme[E1] can be unified with
@scheme[E2].  Any resulting bindings for logic variables are kept.}

@defpred[(%=/= [E1 any/c] [E2 any/c])]{
The goal @scheme[(%=/= E1 E2)] succeeds if @scheme[E1] and @scheme[E2] are bound to
numbers and @scheme[E1] is not equal to @scheme[E2].}

@defpred[(%=:= [E1 any/c] [E2 any/c])]{
The goal @scheme[(%=:= E1 E2)] succeeds if @scheme[E1] and @scheme[E2] are bound to
numbers and @scheme[E1] is equal to @scheme[E2].}

@defpred[(%== [E1 any/c] [E2 any/c])]{
The goal @scheme[(%== E1 E2)] succeeds if @scheme[E1] is @emph{identical}
to @scheme[E2].  They should be structurally equal.  If containing
logic variables, they should have the same variables in the
same position.  Unlike a @scheme[%=]-call, this goal will not bind
any logic variables.}

@defpred[(%> [E1 any/c] [E2 any/c])]{
The goal @scheme[(%> E1 E2)] succeeds if @scheme[E1] and @scheme[E2] are bound to
numbers and @scheme[E1] is greater than @scheme[E2].}

@defpred[(%>= [E1 any/c] [E2 any/c])]{
The goal @scheme[(%>= E1 E2)] succeeds if @scheme[E1] and @scheme[E2] are bound to
numbers and @scheme[E1] is greater than or equal to @scheme[E2].}

@defform[(%and G ...) #:contracts ([G goal?])]{
The goal @scheme[(%and G ...)] succeeds if all the goals
@scheme[G], ..., succeed.}

@defpred[(%append [E1 any/c] [E2 any/c] [E3 any/c])]{
The goal @scheme[(%append E1 E2 E3)] succeeds if @scheme[E3] is unifiable
with the list obtained by appending @scheme[E1] and @scheme[E2].}

@defform[(%assert Pname (V ...) clause ...)
         #:contracts ([Pname identifier?]
                      [V identifier?])]{
Adds the clauses
@scheme[clauses], ..., to the @emph{end} of the predicate that is the value of
the Scheme variable @scheme[Pname].  The variables @scheme[V], ..., are
local logic variables for @scheme[clause], ....}

@defform[(%assert-a Pname (V ...) clause ...)
         #:contracts ([Pname identifier?]
                      [V identifier?])]{
Like @scheme[%assert], but adds the new clauses to the @emph{front}
of the existing predicate.}

@defpred[(%bag-of [E1 any/c] [G goal?] [E2 any/c])]{
The goal @scheme[(%bag-of E1 G E2)] unifies with @scheme[E2] the @emph{bag}
(multiset)
of all the
instantiations of @scheme[E1] for which goal @scheme[G] succeeds.}

@defpred[(%bag-of-1 [E1 any/c] [G goal?] [E2 any/c])]{
Similar to @scheme[%bag-of], but fails if the bag is empty.}

@defpred[(%compound [E any/c])]{
The goal @scheme[(%compound E)] succeeds if @scheme[E] is a non-atomic
structure, ie, a vector or a list.}

@defpred[(%constant [E any/c])]{
The goal @scheme[(%compound E)] succeeds if @scheme[E] is an atomic
structure, ie, not a vector or a list.}

@defpred[(%copy [F any/c] [S any/c])]{
The goal @scheme[(%copy F S)] unifies with @scheme[S] a copy of the
frozen structure in @scheme[F].}

@defform[(%cut-delimiter . any)]{
Introduces a cut point. See @secref{cut}.}

@defpred[(%empty-rel [E any/c] ...)]{
The goal @scheme[(%empty-rel E ...)] always fails.  The @emph{value}
@scheme[%empty-rel] is used as a starting value for predicates
that can later be enhanced with @scheme[%assert] and @scheme[%assert-a].}

@defgoal[%fail]{
The goal @scheme[%fail] always fails.}

@defform[(%free-vars (V ...) G)
         #:contracts ([V identifier?]
                      [G goal?])]{
Identifies
the occurrences of the variables @scheme[V], ..., in goal
@scheme[G] as free.  It is used to avoid existential quantification
in calls to set predicates (@scheme[%bag-of], @scheme[%set-of], &c.).}

@defpred[(%freeze [S any/c] [F any/c])]{
The goal @scheme[(%freeze S F)] unifies with @scheme[F] a new frozen
version of the structure in @scheme[S].  Freezing implies that all
the unbound variables are preserved.  @scheme[F] can henceforth be
used as @emph{bound} object with no fear of its variables
getting bound by unification.}

@defpred[(%if-then-else [G1 goal?] [G2 goal?] [G3 goal?])]{
The goal @scheme[(%if-then-else G1 G2 G3)] tries @scheme[G1] first: if it
succeeds, tries @scheme[G2]; if not, tries @scheme[G3].}

@defform[(%is E1 E2)]{
The goal @scheme[(%is E1 E2)] unifies with @scheme[E1] the result of
evaluating @scheme[E2] as a Scheme expression.  @scheme[E2] may contain
logic variables, which are dereferenced automatically.
Fails if @scheme[E2] contains unbound logic variables.}

@defform[(%let (V ...) expr ...)
         #:contracts ([V identifier?])]{
Introduces @scheme[V], ..., as
lexically scoped logic variables to be used in @scheme[expr], ...}

@defpred[(%melt [F any/c] [S any/c])]{
The goal @scheme[(%melt F S)] unifies @scheme[S] with the thawed
(original) form of the frozen structure in @scheme[F].}

@defpred[(%melt-new [F any/c] [S any/c])]{
The goal @scheme[(%melt-new F S)] unifies @scheme[S] with a thawed
@emph{copy} of the frozen structure in @scheme[F].  This means
new logic variables are used for unbound logic variables in
@scheme[F].}

@defpred[(%member [E1 any/c] [E2 any/c])]{
The goal @scheme[(%member E1 E2)] succeeds if @scheme[E1] is a member
of the list in @scheme[E2].}

@defpred[(%nonvar [E any/c])]{
@scheme[%nonvar] is the negation of @scheme[%var].
The goal @scheme[(%nonvar E)] succeeds if @scheme[E] is completely
instantiated, ie, it has no unbound variable in it.}

@defpred[(%not [G goal?])]{
The goal @scheme[(%not G)] succeeds if @scheme[G] fails.}

@defproc[(%more) answer?]{
The thunk @scheme[%more] produces more instantiations of the
variables in the most recent @scheme[%which]-form that satisfy the
goals in that @scheme[%which]-form.  If no more solutions can
be found, @scheme[%more] returns @scheme[#f].}

@defform[(%or G ...) #:contracts ([G goal?])]{
The goal @scheme[(%or G ...)] succeeds if one of @scheme[G], ..., tried
in that order, succeeds.}

@defform/subs[(%rel (V ...) clause ...)
              ([clause [(E ...) G ...]])
              #:contracts ([V identifier?]
                           [E expression?]
                           [G goal?])]{
Creates a predicate object.
Each clause @scheme[C] signifies
that the goal created by applying the predicate object to
anything that matches @scheme[(E ...)] is deemed to succeed if all
the goals @scheme[G], ..., can, in their turn, be shown to succeed.}

@defpred[(%repeat)]{
The goal @scheme[(%repeat)] always succeeds (even on retries).
Used for failure-driven loops.}

@defboolparam[use-occurs-check? on?]{
If this is false (the default), 
Schelog's unification will not use the occurs check.
If it is true, the occurs check is enabled.}

@defpred[(%set-of [E1 any/c] [G goal?] [E2 any/c])]{
The goal @scheme[(%set-of E1 G E2)] unifies with @scheme[E2] the @emph{set}
of all the
instantiations of @scheme[E1] for which goal @scheme[G] succeeds.}

@defpred[(%set-of-1 [E1 any/c] [G goal?] [E2 any/c])]{
Similar to @scheme[%set-of], but fails if the set is empty.}

@defgoal[%true]{
The goal @scheme[%true] succeeds.  Fails on retry.}

@defpred[(%var [E any/c])]{
The goal @scheme[(%var E)] succeeds if @scheme[E] is not completely
instantiated, ie, it has at least one unbound variable in
it.}

@defform[(%which (V ...) G ...)
         #:contracts ([V identifier?]
                      [G goal?])]{
Returns an instantiation
of the variables @scheme[V], ..., that satisfies all of @scheme[G],
...  If @scheme[G], ..., cannot be satisfied, returns @scheme[#f].
Calling the thunk @scheme[%more] produces more
instantiations, if available.}
                                
@defproc[(_) logic-variable?]{
A thunk that produces a new logic variable.  Can be
used in situations where we want a logic variable but
don't want to name it.  (@scheme[%let], in contrast, introduces new
lexical names for the logic variables it creates.)
}

@defidform[!]{
The cut goal, see @secref{cut}.
                  
May only be used syntactically inside @scheme[%cut-delimiter] or @scheme[%rel].}

@bibliography[
 @bib-entry[#:key "sicp" 
                  #:author "Harold Abelson and Gerald Jay Sussman with Julie Sussman"
                  #:title "Structure and Interpretation of Computer Programs (``SICP''), 2nd Edition"
                  #:url "http://mitpress.mit.edu/sicp/full-text/book/book.html"
                  #:date "1996"
                  #:location "MIT Press"
                  #:is-book? #t]
 @bib-entry[#:key "aop" 
                  #:author "Leon Sterling and Ehud Shapiro"
                  #:url "http://mitpress.mit.edu/book-home.tcl?isbn=0262193388"
                  #:title "The Art of Prolog, 2nd Edition"
                  #:location "MIT Press"
                  #:date "1994"
                  #:is-book? #t]
 @bib-entry[#:key "tls" 
                  #:author "Daniel P Friedman and Matthias Felleisen"
                  #:url "http://www.ccs.neu.edu/~matthias/BTLS"
                  #:title "The Little Schemer, 4th Edition"
                  #:location "MIT Press"
                  #:date "1996"
                  #:is-book? #t]
 @bib-entry[#:key "tss"
                  #:author "Daniel P Friedman and Matthias Felleisen"
                  #:url "http://www.ccs.neu.edu/~matthias/BTSS"
                  #:title "The Seasoned Schemer"
                  #:location "MIT Press"
                  #:date "1996"
                  #:is-book? #t]
 @bib-entry[#:key "eopl"
                  #:author "Daniel P Friedman and Mitchell Wand and Christopher T Haynes"
                  #:url "http://mitpress.mit.edu/book-home.tcl?isbn=0262061457"
                  #:title "Essentials of Programming Languages"
                  #:location "MIT Press, McGraw-Hill"
                  #:date "1992"
                  #:is-book? #t]
 @bib-entry[#:key "bratko"
                  #:author "Ivan Bratko"
                  #:title "Prolog Programming for Artificial Intelligence"
                  #:location "Addison-Wesley"
                  #:date "1986"
                  #:is-book? #t]
 @bib-entry[#:key "campbell"
    #:author "J A Campbell (editor)"
    #:title "Implementations of Prolog"
    #:location "Ellis Horwood"
    #:date "1984"
    #:is-book? #t]
 @bib-entry[#:key "ok:prolog"
    #:author "Richard A O'Keefe"
    #:url "http://mitpress.mit.edu/book-home.tcl?isbn=0262150395"
    #:title "The Craft of Prolog"
    #:location "MIT Press"
    #:date "1990"
    #:is-book? #t]
 @bib-entry[#:key "logick"
    #:author "Christopher T Haynes"
    #:title "Logic continuations"
    #:location "J Logic Program, vol 4, 157--176"
    #:date "1987"]
 @bib-entry[#:key "r5rs"
    #:author "Richard Kelsey and William Clinger and Jonathan {Rees (eds)}"
    #:url "http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs.html"
    #:title "Revised^5 Report on the Algorithmic Language Scheme (``R5RS'')"
    #:date "1998"]
 @bib-entry[#:key "t-y-scheme"
   #:author "Dorai Sitaram"
   #:title "Teach Yourself Scheme in Fixnum Days"
   #:url "http://www.ccs.neu.edu/~dorai/t-y-scheme/t-y-scheme.html"]
 @bib-entry[#:key "mf:prolog"
    #:author "Matthias Felleisen"
    #:title "Transliterating Prolog into Scheme"
    #:location "Indiana U Comp Sci Dept Tech Report #182"
    #:date "1985"]
 ]