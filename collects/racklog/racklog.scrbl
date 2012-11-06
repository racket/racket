#lang scribble/manual
@(require scribble/eval
          (for-syntax racket)
          (for-label racklog
                     (except-in racket _)))

@(define racklog-eval (make-base-eval))
@(racklog-eval '(require racklog))

@title{Racklog: Prolog-Style Logic Programming}

@author{Dorai Sitaram}

@margin-note{Adapted from Schelog by Dorai Sitaram for Racket by Dorai Sitaram,
  John Clements, and Jay McCarthy.}

@defmodule[racklog]

Racklog is an @emph{embedding} of
Prolog-style logic programming in Racket.  ``Embedding''
means you don't lose Racket: You can use Prolog-style and
conventional Racket code fragments alongside each other.
Racklog contains the full repertoire of Prolog features,
including meta-logical and second-order (``set'')
predicates, leaving out only those features that could more
easily and more efficiently be done with Racket
subexpressions.

The Racklog implementation uses the approach to logic
programming for Scheme described in Felleisen @cite{mf:prolog} and
Haynes @cite{logick}.  In contrast to earlier Lisp simulations of
Prolog @cite{campbell},
which used explicit continuation
arguments to store failure (backtrack) information, the
Felleisen and Haynes model uses the implicit reified
continuations of Scheme. In Racket these are provided by the operator
@racket[call-with-current-continuation] (aka @racket[call/cc]).  This
allows Racklog to be an @emph{embedding}, ie, logic
programming is not built as a new language on top of Racket,
but is used alongside Racket's other features.  Both styles
of programming may be mixed to any extent that a project
needs.

The Racklog user does not need to know about the
implementation mechanism or about @racket[call/cc] and
continuations to get on with the business of
doing logic programming with Racklog.

This text is a gentle introduction to Racklog syntax
and programming.  It assumes a working knowledge of
Racket and an awareness of, if not actual programming
experience with, Prolog.  If you need assistance for Prolog,
you may consult @cite["bratko" "ok:prolog" "aop"] or 
many other excellent books and
online documents available.

@table-of-contents[]

@section[#:tag "simple"]{Simple Goals and Queries}

Racklog objects are the same as Racket objects.  However, there
are two subsets of these objects that are of special
interest to Racklog: @emph{goals} and @emph{predicates}.  We
will first look at some simple goals.
@secref{predicates} will introduce predicates and ways
of making  complex goals using predicates.

A goal is an object whose truth or falsity we can check.  A
goal that turns out to be true is said to succeed.
A goal that turns out to be false is said to
fail.

Two simple goals that are provided in Racklog are:
@racketblock[
%true
%fail
]

The  goal @racket[%true] succeeds.  The goal @racket[%fail]
always fails.

(The names of all Racklog primitive objects
start with @litchar{%}.  This is to avoid clashes with the names
of conventional Racket objects of related meaning.
User-created objects in Racklog are not required to
follow this convention.)

A Racklog user can @emph{query} a goal by wrapping it in a
@racket[%which]-form.

@racketblock[
(%which () %true)
]

evaluates to @racketresult[()], indicating success, whereas:

@racketblock[
(%which () %fail)
]

evaluates to @racket[#f], indicating failure.

Note 1: The second subexpression of the @racket[%which]-form
is the empty list @racketresult[()].  Later (@secref{solving-goals}),
we will see @racket[%which]es
with other lists as the second subform.

Henceforth, we will use the notation:

@interaction[(eval:alts E 'F)]

to say that @racket[E] @emph{evaluates to} @racket[F]. Thus,

@interaction[#:eval racklog-eval (%which () %true)]

@section[#:tag "predicates"]{Predicates}

More interesting goals are created by applying a special
kind of Racklog object called a @emph{predicate} (or
@emph{relation}) to other
Racklog objects.  Racklog comes with some primitive
predicates, such as the arithmetic operators
@racket[%=:=] and @racket[%<],
standing for arithmetic ``equal'' and ``less than''
respectively.  For example, the following are some goals
involving these predicates:

@interaction[
 #:eval racklog-eval
 (%which () (%=:= 1 1))
 (%which () (%< 1 2))
 (%which () (%=:= 1 2))
 (%which () (%< 1 1))
 ]

Other arithmetic predicates are
@racket[%>] (``greater than''),
@racket[%<=] (``less than or equal''),
@racket[%>=] (``greater than or equal''), and
@racket[%=/=] (``not equal'').

Racklog predicates are not to be confused with conventional
Racket predicates (such as @racket[<] and @racket[=]).  Racklog
predicates, when applied to arguments, produce goals
that
may either succeed or fail.  Racket predicates, when applied
to arguments, yield a boolean value.  Henceforth, we will
use the term ``predicate'' to mean Racklog predicates.
Conventional predicates will be explicitly called ``Racket
predicates''.

@subsection[#:tag "facts"]{Predicates Introducing Facts}

Users can create their own predicates using the Racklog form
@racket[%rel].  For example, let's
define the predicate @racket[%knows]:

@racketblock+eval[#:eval racklog-eval
(define %knows
  (%rel ()
    [('Odysseus 'TeX)]
    [('Odysseus 'Racket)]
    [('Odysseus 'Prolog)]
    [('Odysseus 'Penelope)]
    [('Penelope 'TeX)]
    [('Penelope 'Prolog)]
    [('Penelope 'Odysseus)]
    [('Telemachus 'TeX)]
    [('Telemachus 'calculus)]))
]

The expression has the expected meaning.  Each
@emph{clause} in the @racket[%rel] establishes a @emph{fact}:
Odysseus
knows TeX, Telemachus knows calculus, &c.  In general, if we
apply the predicate to the arguments in any one of its
clauses, we will get a successful goal.  Thus, since
@racket[%knows] has a clause that reads
@racket[[('Odysseus 'TeX)]], the goal
@racket[(%knows 'Odysseus 'TeX)]
will be true.

We can now get answers for the following types of queries:

@interaction[#:eval racklog-eval
(%which ()
  (%knows 'Odysseus 'TeX))
(%which ()
  (%knows 'Telemachus 'Racket))
]

@subsection[#:tag "rules"]{Predicates with Rules}

Predicates can be more complicated than the above bald
recitation of facts.  The predicate clauses can be @emph{rules}, eg,

@racketblock+eval[#:eval racklog-eval
(define %computer-literate
  (%rel (person)
    [(person)
      (%knows person 'TeX)
      (%knows person 'Racket)]
    [(person)
      (%knows person 'TeX)
      (%knows person 'Prolog)]))
]

This defines the predicate
@racket[%computer-literate] in
terms of the predicate @racket[%knows].  In effect, a person is
defined as computer-literate if they know TeX and
Racket, @emph{or} TeX and Prolog.

Note that this use of
@racket[%rel] employs a local @emph{logic variable} called @racket[_person].
In general, a @racket[%rel]-expression can have a list of symbols
as its second subform.  These name new logic variables that
can be used within the body of the @racket[%rel].

The following query can now be answered:

@interaction[#:eval racklog-eval
(%which ()
  (%computer-literate 'Penelope))
]

Since Penelope knows TeX and Prolog, she is computer-literate.

@subsection[#:tag "solving-goals"]{Solving Goals}

The above queries are yes/no questions.  Racklog programming
allows more: We can formulate a goal with @emph{uninstantiated}
logic variables and then ask the querying process to
provide, if possible, values for these variables that cause
the goal to succeed.  For instance, the query:

@interaction[#:eval racklog-eval
(%which (what)
  (%knows 'Odysseus what))
]

asks for an instantiation of the logic variable @racket[_what]
that satisfies the goal @racket[(%knows 'Odysseus what)].
In other words, we are asking, ``What does Odysseus know?''

Note that this use of @racket[%which] --- like @racket[%rel]
in the definition of @racket[%computer-literate] ---
uses a local logic
variable, @racket[_what].  In general, the second subform of
@racket[%which] can be a list of local logic variables.  The
@racket[%which]-query returns an answer that is a list of
bindings, one for each logic variable mentioned in its
second subform.  Thus,

@interaction[#:eval racklog-eval
(%which (what)
  (%knows 'Odysseus what))
]

But that is not all that wily Odysseus knows.  Racklog
provides a zero-argument procedure (``thunk'') called
@racket[%more]
that @emph{retries} the goal in the last
@racket[%which]-query for a different solution.

@interaction[#:eval racklog-eval
(%more)
]

We can keep pumping for more solutions:

@interaction[#:eval racklog-eval
(%more)
(%more)
(%more)
]

The final @racket[#f] shows that there are no more
solutions.  This is because there are no more clauses in the
@racket[%knows] predicate that list Odysseus as knowing anything
else.

@subsection[#:tag "assert"]{Asserting Extra Clauses}

We can add more clauses to a predicate after it has already
been defined with a @racket[%rel].  Racklog provides the
@racket[%assert!] form for this purpose.  Eg,

@racketblock+eval[#:eval racklog-eval
(%assert! %knows ()
  [('Odysseus 'archery)])
]

tacks on a new clause at the end of the existing clauses
of the @racket[%knows]
predicate.  Now, the query:

@interaction[#:eval racklog-eval
(%which (what)
  (%knows 'Odysseus what))
]

gives TeX, Racket, Prolog, and Penelope, as before, but
a subsequent @racket[(%more)] yields a new result:
@interaction-eval[#:eval racklog-eval (begin (%more) (%more) (%more))]
@interaction[#:eval racklog-eval
(%more)
]

The Racklog form @racket[%assert-after!] is similar to @racket[%assert!] but
adds clauses @emph{before} any of the current clauses.

Both @racket[%assert!] and @racket[%assert-after!] assume that the variable
they are adding to already names a predicate (presumably
defined using @racket[%rel]).
In order to allow defining a predicate entirely through
@racket[%assert!]s,  Racklog provides an empty predicate value
@racket[%empty-rel].  @racket[%empty-rel] takes any number of arguments
and always fails.  A typical use of the
@racket[%empty-rel] and @racket[%assert!] combination:

@racketblock+eval[#:eval racklog-eval
(define %parent %empty-rel)

(%assert! %parent ()
  [('Laertes 'Odysseus)])

(%assert! %parent ()
  [('Odysseus 'Telemachus)]
  [('Penelope 'Telemachus)])
]

(Racklog does not provide a predicate for @emph{retracting}
assertions, since we can keep track of older versions of
predicates using conventional Racket features (@racket[let] and @racket[set!]).)

@subsection[#:tag "local-vars"]{Local Variables}

The local logic variables of @racket[%rel]- and
@racket[%which]-expressions are in reality introduced by the
Racklog syntactic form called @racket[%let].  (@racket[%rel] and
@racket[%which] are macros written using @racket[%let].)

@racket[%let] introduces new lexically scoped logic variables.
Supposing, instead of

@interaction[#:eval racklog-eval
(%which (what)
  (%knows 'Odysseus what))
]

we had asked

@interaction[#:eval racklog-eval
(%let (what)
  (%which ()
    (%knows 'Odysseus what)))
]

This query, too, succeeds five times, since
Odysseus knows five things.  However, @racket[%which] emits
bindings only for the local variables that @emph{it}
introduces.  Thus, this query emits @racketresult[()] five times before
@racket[(%more)] finally returns @racket[#f].

@section[#:tag "racket-w-logic"]{Using Conventional Racket Expressions in Racklog}

The arguments of Racklog predicates can be any Racket
objects.  In particular, composite structures such as lists,
vectors, strings, hash tables, etc can be used, as also Racket expressions
using the full array of Racket's construction and
decomposition operators.  For instance, consider the
following goal:

@racketblock[
(%member x '(1 2 3))
]

Here, @racket[%member] is a predicate, @racket[x] is a logic
variable, and @racket['(1 2 3)] is a structure.  Given a suitably
intuitive definition for @racket[%member], the above goal
succeeds for @racket[x] = @racketresult[1], @racketresult[2], and @racketresult[3].

Now to defining predicates like @racket[%member]:

@racketblock[
(define %member
  (%rel (x y xs)
    [(x (cons x xs))]
    [(x (cons y xs))
      (%member x xs)]))
]

Ie, @racket[%member] is defined with three local variables:
@racket[x],  @racket[y], @racket[xs].  It  has two
clauses, identifying the two ways of determining membership.

The first clause of @racket[%member] states a fact: For any
@racket[x], @racket[x] is a member of a list whose head is also @racket[x].

The second clause of @racket[%member] is a rule: @racket[x] is a
member of a list if we can show that it is a member of the
@emph{tail} of that list.  In other words, the original
@racket[%member] goal is translated into a @emph{sub}goal, which is also
a @racket[%member] goal.

Note that the variable @racket[y] in the definition of
@racket[%member] occurs only once in the second clause.  As such,
it doesn't need you to make the effort of naming it.  (Names
help only in matching a second occurrence to a first.)  Racklog
lets you use the expression @racket[(_)] to denote an anonymous
variable.  (Ie, @racket[_] is a thunk that generates a fresh
anonymous variable at each call.)  The predicate @racket[%member] can be
rewritten as

@racketblock[
(define %member
  (%rel (x xs)
    [(x (cons x (_)))]
    [(x (cons (_) xs))
      (%member x xs)]))
]

@subsection[#:tag "constructors"]{Constructors}

We can use constructors --- Racket procedures for creating
structures --- to simulate data types in Racklog.  For
instance, let's define a natural-number data-type where
@racket[0] denotes zero, and @racket[(succ x)] denotes the natural number
whose immediate predecessor is @racket[x].   The constructor
@racket[succ] can
be defined in Racket as:

@racketblock+eval[#:eval racklog-eval
(define succ
  (lambda (x)
    (vector 'succ x)))
]

Addition and multiplication can be defined as:

@racketblock+eval[#:eval racklog-eval
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

@racketblock+eval[#:eval racklog-eval
(define %factorial
  (%rel (x y y1)
    [(0 (succ 0))]
    [((succ x) y)
      (%factorial x y1)
      (%times (succ x) y1 y)]))
]

@subsection[#:tag "is"]{@racket[\%is]}

The above is a very inefficient way to do arithmetic,
especially when the underlying language Racket offers
excellent arithmetic facilities (including a comprehensive
number ``tower'' and exact rational arithmetic).  One
problem with using Racket calculations directly in Racklog
clauses is that the expressions used may contain logic
variables that need to be dereferenced.  Racklog provides
the predicate @racket[%is] that takes care of this.  The goal

@racketblock[
(%is _X _E)
]

unifies @racket[_X] with the value of @racket[_E] considered as a
Racket expression.  @racket[_E] can have logic variables, but
usually they should at least be bound, as unbound variables
may not be palatable values to the Racket operators used in
@racket[_E].

We can now directly use the numbers of Racket to write a
more efficient @racket[%factorial] predicate:

@racketblock+eval[#:eval racklog-eval
(define %factorial
  (%rel (x y x1 y1)
    [(0 1)]
    [(x y) (%is x1 (- x 1))
           (%factorial x1 y1)
           (%is y (* y1 x))]))
]

A price that this efficiency comes with is that we can
use @racket[%factorial] only with its first argument already
instantiated.  In many cases, this is not an unreasonable
constraint.  In fact, given this limitation, there is
nothing to prevent us from using Racket's factorial
directly:

@racketblock+eval[#:eval racklog-eval
(define %factorial
  (%rel (x y)
    [(x y)
     (%is y (racket-factorial
	      x))]))
]

or better yet, ``in-line'' any calls to @racket[%factorial] with
@racket[%is]-expressions calling @racket[racket-factorial], where the
latter is defined in the usual manner:

@racketblock+eval[#:eval racklog-eval
(define racket-factorial
  (lambda (n)
    (if (= n 0) 1
        (* n (factorial
	       (- n 1))))))
]

@subsection[#:tag "lexical-scoping"]{Lexical Scoping}

One can use Racket's lexical scoping to enhance predicate
definition.  Here is a list-reversal predicate defined using
a hidden auxiliary predicate:

@racketblock+eval[#:eval racklog-eval
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

@racket[(revaux _X _Y _Z)] uses @racket[_Y] as an accumulator for
reversing @racket[_X] into @racket[_Z].  (@racket[_Y] starts out as @racketresult[()].
Each head of @racket[_X] is @racket[cons]ed on to @racket[_Y].  Finally, when
@racket[_X] has wound down to @racketresult[()], @racket[_Y] contains the reversed
list and can be returned as @racket[_Z].)

@racket[revaux] is used purely as a helper predicate for
@racket[%reverse], and so it can be concealed within a lexical
contour.  We use @racket[letrec] instead of @racket[let] because
@racket[revaux] is a recursive procedure.

@subsection[#:tag "type-predicates"]{Type Predicates}

Racklog provides a couple of predicates that let the user
probe the type of objects.

The goal
@racketblock[
(%constant _X)
]

succeeds if @racket[_X] is an @emph{atomic} object.

The predicate @racket[%compound], the negation of @racket[%constant],
checks if its argument is not an atomic object.

The above are merely the logic-programming equivalents of
corresponding Racket predicates.  Users can use the
predicate @racket[%is] and Racket predicates to write more type
checks in Racklog.  Thus, to test if @racket[_X] is a string, the
following goal could be used:

@racketblock[
(%is #t (string? _X))
]

User-defined Racket predicates, in addition to primitive Racket
predicates, can be thus imported.

@section[#:tag "backtracking"]{Backtracking}

It is helpful to go into the following evaluation (@secref{rules})
in a
little detail:

@racketblock+eval[#:eval racklog-eval
(%which ()
  (%computer-literate 'Penelope))
]

The starting goal
is:

@(define goal litchar)
@racketblock[
G0 = (%computer-literate Penelope)
]

(I've taken out the quote because @racketresult[Penelope] is the result
of evaluating @racket['Penelope].)

Racklog tries to match this with the head of the first
clause of @racket[%computer-literate].  It succeeds, generating a
binding  @racket[[person . Penelope]].

But this means it now has two new goals --- @emph{subgoals}
--- to solve.  These are the goals in the body of the
matching clause, with the logic variables substituted by
their instantiations:

@racketblock[
G1 = (%knows Penelope TeX)
G2 = (%knows Penelope Racket)
]

For @goal{G1}, Racklog attempts matches with the clauses of
@racket[%knows], and succeeds at the fifth try.  (There are no
subgoals in this case, because the bodies of these ``fact''
clauses are empty, in contrast to the ``rule'' clauses of
@racket[%computer-literate].)
Racklog then tries to solve @goal{G2} against the clauses of
@racket[%knows], and since there is no clause stating that
Penelope knows Racket, it fails.

All is not lost though.  Racklog now @emph{backtracks} to the
goal that was solved just before, viz., @goal{G1}.  It
@emph{retries} @goal{G1}, ie, tries to solve it in a
different way.
This entails searching down the previously unconsidered
@racket[%knows]
clauses for @goal{G1}, ie, the sixth onwards.  Obviously,
Racklog fails again, because the fact that Penelope knows
TeX occurs only once.

Racklog now backtracks to the goal before @goal{G1}, ie,
@goal{G0}.  We abandon the current successful match with the
first clause-head of @racket[%computer-literate], and try the
next clause-head.  Racklog succeeds, again producing a binding
@racket[[person . Penelope]], and two new subgoals:

@racketblock[
G3 = (%knows Penelope TeX)
G4 = (%knows Penelope Prolog)
]

It is now easy to trace that Racklog finds both @goal{G3} and @goal{G4} to be
true.  Since both of @goal{G0}'s subgoals are true, @goal{G0} is
itself considered true.  And this is what Racklog reports.  The
interested reader can now trace  why the
following query has a different denouement:

@interaction[#:eval racklog-eval
(%which ()
  (%computer-literate 'Telemachus))
]

@section[#:tag "unification"]{Unification}

When we say that a goal matches with a clause-head, we mean
that the predicate and argument positions line up.  Before
making this comparison, Racklog dereferences all already
bound logic variables.  The resulting structures are then
compared to see if they are recursively identical.  Thus,
@racket[1] unifies with @racket[1], and @racket[(list 1 2)] with @racket['(1 2)]; but @racket[1] and
@racket[2] do not unify, and neither do @racket['(1 2)] and @racket['(1 3)].

In general, there could be quite a few uninstantiated logic
variables in the compared objects.  Unification will then
endeavor to find the most natural way of binding these
variables so that we arrive at structurally identical
objects.  Thus, @racket[(list _x 1)], where @racket[_x] is an unbound logic
variable, unifies with @racket['(0 1)], producing the
binding
@racket[[_x 0]].

Unification is thus a goal, and Racklog makes the unification predicate
available  to the user as @racket[%=].   Eg,

@interaction[#:eval racklog-eval
(%which (x)
  (%= (list x 1) '(0 1)))
]

Racklog also provides the predicate @racket[%/=], the @emph{negation} of
@racket[%=].  @racket[(%/= _X _Y)] succeeds if and only if @racket[_X] does
@emph{not} unify with @racket[_Y].

Unification goals constitute the basic subgoals that all
Racklog goals devolve to.  A goal succeeds because all the
eventual unification subgoals that it decomposes to in at
least one of its subgoal-branching succeeded.  It fails
because every possible subgoal-branching was thwarted by the
failure of a crucial unification subgoal.

Going back to the example in @secref{backtracking}, the goal
@racket[(%computer-literate 'Penelope)] succeeds because
(a) it unified with
@racket[(%computer-literate person)]; and then (b) with the binding
@racket[[person . Penelope]] in place, @racket[(%knows person 'TeX)]
unified with @racket[(%knows 'Penelope 'TeX)] and
@racket[(%knows person 'Prolog)] unified with @racket[(%knows 'Penelope 'Prolog)].

In contrast, the goal @racket[(%computer-literate 'Telemachus)]
fails because, with @racket[[person . Telemachus]],
the subgoals @racket[(%knows person 'Racket)] and
@racket[(%knows person 'Prolog)] have no facts they can
unify with.

@subsection{The Occurs Check}

A robust unification algorithm uses the @deftech{occurs check}, which ensures that a logic variable
isn't bound to a structure that contains itself.  
Not performing the check can cause the unification
to go into an infinite loop in some cases.  On the
other hand, performing the occurs check greatly
increases the time taken by unification, even in cases
that wouldn't require the check.

Racklog uses the global parameter
@racket[use-occurs-check?] to decide whether to
use the occurs check.  By default, this variable is 
@racket[#f], ie, Racklog disables the occurs check.  To
enable the check, 

@racketblock[
(use-occurs-check? #t)
]

@section[#:tag "and-or"]{Conjuctions and Disjunctions}

Goals may be combined using the forms @racket[%and]
and @racket[%or]
to form compound goals.  (For @racket[%not], see @secref{not}.)
Eg,

@interaction[#:eval racklog-eval
(%which (x)
  (%and (%member x '(1 2 3))
        (%< x 3)))
]

gives solutions for @racket[_x] that satisfy both the
argument goals of the @racket[%and].
Ie, @racket[_x] should both be a member of @racket['(1 2 3)]
@emph{and}  be less than @racket[3]. Typing @racket[(%more)] gives another solution:

@interaction[#:eval racklog-eval
(%more)
(%more)
]

There are no more solutions, because @racket[[x 3]] satisfies
the first but not the second goal.

Similarly, the query

@interaction[#:eval racklog-eval
(%which (x)
  (%or (%member x '(1 2 3))
       (%member x '(3 4 5))))
]

lists all @racket[_x] that are members of either list.

@interaction[#:eval racklog-eval
(%more)
(%more)
(%more)
(%more)
(%more)
]

(Yes, @racket[([x 3])] is listed twice.)

We can rewrite the predicate @racket[%computer-literate]
from @secref{rules} using @racket[%and] and @racket[%or]:

@racketblock+eval[#:eval racklog-eval
(define %computer-literate
  (%rel (person)
    [(person)
     (%or
       (%and (%knows person
	       'TeX)
	     (%knows person
	       'Racket))
       (%and (%knows person
	       'TeX)
	     (%knows person
	       'Prolog)))]))
]

Or,  more succinctly:

@racketblock+eval[#:eval racklog-eval
(define %computer-literate
  (%rel (person)
    [(person)
      (%and (%knows person
	      'TeX)
        (%or (%knows person
	       'Racket)
	     (%knows person
	       'Prolog)))]))
]

We can even dispense with the @racket[%rel] altogether:

@racketblock+eval[#:eval racklog-eval
(define %computer-literate
  (lambda (person)
    (%and (%knows person
	    'TeX)
      (%or (%knows person
	     'Racket)
	(%knows person
	  'Prolog)))))
]

This last looks like a conventional Racket predicate
definition, and is arguably
the most readable format for a Racket programmer.

@section[#:tag "lv-manip"]{Manipulating Racklog Variables}

Racklog provides special predicates for probing logic
variables, without risking their getting bound.

@subsection[#:tag "var"]{Checking for Variables}

The goal

@racketblock[
(%== _X _Y)
]

succeeds if @racket[_X] and @racket[_Y] are @emph{identical} objects.  This
is not quite the unification predicate @racket[%=], for @racket[%==]
doesn't touch unbound objects the way @racket[%=] does.  Eg,
@racket[%==] will not equate an unbound logic variable with a
bound one, nor will it equate two unbound logic variables
unless they are the @emph{same} variable.

The predicate @racket[%/==] is the negation of @racket[%==].

The goal

@racketblock[
(%var _X)
]

succeeds if @racket[_X] isn't completely bound --- ie, it has at
least one unbound logic variable in its innards.

The predicate @racket[%nonvar] is the negation of @racket[%var].

@subsection[#:tag "freeze"]{Preserving Variables}

Racklog lets the user protect a term with variables from
unification by allowing that term to be treated as a
(completely) bound object.  The predicates provided for this
purpose are
@racket[%freeze],
@racket[%melt], @racket[%melt-new], and @racket[%copy].

The goal

@racketblock[
(%freeze _S _F)
]

unifies @racket[_F] to the frozen version of @racket[_S].  Any lack
of bindings in @racket[_S] are preserved no matter how much you
toss @racket[_F] about.

The goal

@racketblock[
(%melt _F _S)
]

retrieves the object frozen in @racket[_F] into @racket[_S].

The goal

@racketblock[
(%melt-new _F _S)
]

is similar to @racket[%melt],
except that when @racket[_S] is made,  the unbound variables in
@racket[_F] are replaced by brand-new unbound variables.

The goal

@racketblock[
(%copy _S _C)
]

is an abbreviation for @racket[(%freeze _S _F)]
followed by @racket[(%melt-new _F _C)].

@section[#:tag "cut"]{The Cut (@racket[!])}

The cut (called @racket[!]) is a special goal that is used to
prune backtracking options.  Like the @racket[%true] goal, the
cut goal too succeeds, when accosted by the Racklog
subgoaling engine.  However, when a further subgoal down the
line fails, and time comes to retry the cut goal, Racklog
will refuse to try alternate clauses for the predicate in
whose definition the cut occurs.  In other words, the cut
causes Racklog to commit to all the decisions made from the
time that the predicate was selected to match a subgoal till
the time the cut was satisfied.

For example, consider again the @racket[%factorial]
predicate, as defined in @secref{is}:

@racketblock+eval[#:eval racklog-eval
(define %factorial
  (%rel (x y x1 y1)
    [(0 1)]
    [(x y) (%is x1 (- x 1))
           (%factorial x1 y1)
           (%is y (* y1 x))]))
]

Clearly,

@interaction[#:eval racklog-eval
(%which ()
  (%factorial 0 1))
(%which (n)
  (%factorial 0 n))
]

But what if we asked for @racket[(%more)] for either query?
Backtracking will try
the second clause of @racket[%factorial], and sure enough the
clause-head unifies, producing binding @racket[[x . 0]].
We now get three subgoals.  Solving the first, we get @racket[[x1 . -1]], and then we have to solve @racket[(%factorial -1 y1)].  It
is easy to see there is no end to this, as we fruitlessly
try to get the factorials of numbers that get more and more
negative.

If we placed a cut at the first clause:

@racketblock[
...
[(0 1) !]
...
]

the attempt to find more solutions for @racket[(%factorial 0 1)] is nipped in the bud.

Calling @racket[%factorial] with a @emph{negative} number would still cause an
infinite loop.   To take care of that problem as well, we
use another cut:

@racketblock+eval[#:eval racklog-eval
(define %factorial
  (%rel (x y x1 y1)
    [(0 1) !]
    [(x y) (%< x 0) ! %fail]
    [(x y) (%is x1 (- x 1))
           (%factorial x1 y1)
           (%is y (* y1 x))]))
]

@interaction[#:eval racklog-eval
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

@racketblock+eval[#:eval racklog-eval
(define %if-then-else
  (%rel (p q r)
    [(p q r) p ! q]
    [(p q r) r]))
]

(Note that for the first time we have predicate arguments that
are themselves goals.)

Consider the goal

@racketblock[
G0 = (%if-then-else Gbool Gthen Gelse)
]

We first unify @goal{G0} with the first clause-head,
giving
@racket[[p . Gbool]], @racket[[q . Gthen]], @racket[[r . Gelse]].  @goal{Gbool} can
now either succeed or fail.

Case 1:  If @goal{Gbool} fails, backtracking will cause the
@goal{G0} to unify with the second clause-head.  @racket[r] is bound
to @goal{Gelse}, and so @goal{Gelse} is tried, as expected.

Case 2: If @goal{Gbool} succeeds, the cut commits to this
clause of the @racket[%if-then-else].  We now try @goal{Gthen}.  If
@goal{Gthen} should now fail --- or even if we simply retry for
more solutions --- we are guaranteed that the second
clause-head will not be tried.  If it were not for the cut,
@goal{G0} would attempt to unify with the second clause-head, which will
of course succeed, and @goal{Gelse} @emph{will} be tried.

@subsection[#:tag "not"]{Negation as Failure}

Another common abstraction using the cut is @emph{negation}.
The negation of goal @goal{G} is defined as @racket[(%not G)], where
the predicate @racket[%not] is defined as follows:

@racketblock+eval[#:eval racklog-eval
(define %not
  (%rel ()
    [(g) g ! %fail]
    [(g) %true]))
]

Thus, @racket[g]'s negation is deemed a failure if @racket[g]
succeeds, and a success if @racket[g] fails.  This is of course
confusing goal failure with falsity.  In some cases, this
view of negation is actually helpful.

@section[#:tag "set-of"]{Set Predicates}

The goal

@racketblock[
(%bag-of _X _G _Bag)
]

unifies with @racket[_Bag] the list of all instantiations of
@racket[_X] for which @racket[_G] succeeds.  Thus, the following query
asks for all the things known --- ie, the collection of things
such that someone knows them:

@interaction[#:eval racklog-eval
(%which (things-known)
  (%let (someone x)
    (%bag-of x (%knows someone x)
      things-known)))
]

This is the only solution for this goal:

@interaction[#:eval racklog-eval
(%more)
]

Note that some things --- eg, TeX --- are enumerated
more than once.  This is because more than one person knows
TeX.  To remove duplicates, use the predicate
@racket[%set-of]
instead of @racket[%bag-of]:

@interaction[#:eval racklog-eval
(%which (things-known)
  (%let (someone x)
    (%set-of x (%knows someone x)
      things-known)))
]

In the above, the free variable @racket[_someone] in the
@racket[%knows]-goal is used as if it
were existentially quantified.  In contrast, Prolog's
versions of
@racket[%bag-of] and @racket[%set-of] fix it for each solution of the
set-predicate goal.  We can do it too with some additional
syntax that identifies the free variable.
Eg,

@interaction[#:eval racklog-eval
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

@interaction[#:eval racklog-eval
(%more)
(%more)
(%more)
(%more)
]

Racklog also provides two variants of these set predicates,
viz., @racket[%bag-of-1] and @racket[%set-of-1].  These act like @racket[%bag-of]
and @racket[%set-of] but fail if the resulting bag or set is empty.

@section{Racklog Module Language}

@defmodulelang[@racketmodname[racklog] #:module-paths (racklog/lang/lang)]

This module language accepts the syntax of Datalog (except clauses need not be safe) and compiles each predicate to a relation.

The accepted syntax is available in the @secref[#:doc '(lib "datalog/scribblings/datalog.scrbl")]{datalog} documentation.

@section[#:tag "glossary"]{Glossary of Racklog Primitives}

@(define-syntax (defpred stx)
   (syntax-case stx ()
     [(_ (id arg ...) pre ...)
        (syntax/loc stx
          (defproc (id arg ...)
            goal/c
            pre ...))]))
@(define-syntax-rule (defgoal id pre ...)
   (defthing id goal/c pre ...))

@subsection{Racket Predicates}

@defproc[(logic-var? [x any/c]) boolean?]{Identifies a logic variable.}

@defproc[(atomic-struct? [x any/c]) boolean?]{
  Identifies structures that the @racket[(current-inspector)] cannot inspect.}

@defproc[(atom? [x any/c]) boolean?]{
  Identifies atomic values that may appear in Racklog programs. Equivalent to
  the contract @racket[(or/c boolean? number? string? bytes? char? symbol?
  regexp? pregexp? byte-regexp? byte-pregexp? keyword? null? procedure? void?
  set? atomic-struct?)].}

@defproc[(compound-struct? [x any/c]) boolean?]{
  Identifies structures that the @racket[(current-inspector)] can inspect.}

@defproc[(compound? [x any/c]) boolean?]{
  Identifies compound values that may appear in Racklog programs. Equivalent to
  the contract
  @racket[(or/c pair? vector? mpair? box? hash? compound-struct?)].}

@defproc[(unifiable? [x any/c]) boolean?]{
  Identifies values that may appear in Racklog programs. Essentialy either an
  @racket[atom?], @racket[logic-var?], or @racket[compound?] that contains
  @racket[unifiable?]s.}

@defproc[(answer-value? [x any/c]) boolean?]{
  Identifies values that may appear in @racket[answer?]. Essentially
  @racket[unifiable?]s that do not contain @racket[logic-var?]s.}

@defproc[(answer? [x any/c]) boolean?]{
  Identifies answers returned by @racket[%more] and @racket[%which]. Equivalent
  to the contract
  @racket[(or/c false/c (listof (cons/c symbol? answer-value?)))].}

@defthing[goal/c contract?]{A contract for goals.}

@subsection{User Interface}

@defform[(%which (V ...) G ...)
         #:contracts ([V identifier?]
                      [G goal/c])]{
Returns an @racket[answer?]
of the variables @racket[V], ..., that satisfies all of @racket[G],
...  If @racket[G], ..., cannot be satisfied, returns @racket[#f].
Calling the thunk @racket[%more] produces more
instantiations, if available.}

@defproc[(%more) answer?]{
The thunk @racket[%more] produces more instantiations of the
variables in the most recent @racket[%which]-form that satisfy the
goals in that @racket[%which]-form.  If no more solutions can
be found, @racket[%more] returns @racket[#f].}

@defform[(%find-all (V ...) G ...)
         #:contracts ([V identifier?]
                      [G goal/c])]{
Like @racket[(list (%which (V ...) G ...) (%more) ...)] with as many @racket[(%more)]s as there are answers. (This will not terminate if there are an infinite number of answers.)
}

@subsection{Relations}

@defform/subs[(%rel (V ...) clause ...)
              ([clause [(E ...) G ...]])
              #:contracts ([V identifier?]
                           [E expression?]
                           [G goal/c])]{
Returns a predicate function.  Each clause @racket[C] signifies that
the goal created by applying the predicate object to anything that
matches @racket[(E ...)] is deemed to succeed if all the goals
@racket[G], ..., can, in their turn, be shown to succeed. The
variables @racket[V], ..., are local logic variables for
@racket[clause], ....}

@defpred[(%empty-rel [E unifiable?] ...)]{
The goal @racket[(%empty-rel E ...)] always fails.  The @emph{value}
@racket[%empty-rel] is used as a starting value for predicates
that can later be enhanced with @racket[%assert!] and @racket[%assert-after!].}

@defform[(%assert! Pname (V ...) clause ...)
         #:contracts ([Pname identifier?]
                      [V identifier?])]{
Adds the clauses
@racket[clauses], ..., to the @emph{end} of the predicate that is the value of
the Racket variable @racket[Pname].  The variables @racket[V], ..., are
local logic variables for @racket[clause], ....}

@defform[(%assert-after! Pname (V ...) clause ...)
         #:contracts ([Pname identifier?]
                      [V identifier?])]{
Like @racket[%assert!], but adds the new clauses to the @emph{front}
of the existing predicate.}

@subsection{Racklog Variables}

@defproc[(_) logic-var?]{
A thunk that produces a new logic variable.  Can be
used in situations where we want a logic variable but
don't want to name it.  (@racket[%let], in contrast, introduces new
lexical names for the logic variables it creates.)
}

@defform[(%let (V ...) expr ...)
         #:contracts ([V identifier?])]{
Introduces @racket[V], ..., as
lexically scoped logic variables to be used in @racket[expr], ...}

@subsection{Cut}

@defform[(%cut-delimiter . any)]{
Introduces a cut point. See @secref{cut}.}

@defidform[!]{
The cut goal, see @secref{cut}.

May only be used syntactically inside @racket[%cut-delimiter] or @racket[%rel].}

@subsection{Racklog Operators}

@defgoal[%fail]{
The goal @racket[%fail] always fails.}

@defgoal[%true]{
The goal @racket[%true] succeeds.  Fails on retry.}

@defpred[(%repeat)]{
The goal @racket[(%repeat)] always succeeds (even on retries).
Useful for failure-driven loops.}

@defform[(%and G ...) #:contracts ([G goal/c])]{
The goal @racket[(%and G ...)] succeeds if all the goals
@racket[G], ..., succeed.}

@defform[(%or G ...) #:contracts ([G goal/c])]{
The goal @racket[(%or G ...)] succeeds if one of @racket[G], ..., tried
in that order, succeeds.}

@defpred[(%not [G goal/c])]{
The goal @racket[(%not G)] succeeds if @racket[G] fails.}

@defpred[(%if-then-else [G1 goal/c] [G2 goal/c] [G3 goal/c])]{
The goal @racket[(%if-then-else G1 G2 G3)] tries @racket[G1] first: if it
succeeds, tries @racket[G2]; if not, tries @racket[G3].}

@subsection{Unification}

@defpred[(%= [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%= E1 E2)] succeeds if @racket[E1] can be unified with
@racket[E2].  Any resulting bindings for logic variables are kept.}

@defpred[(%/= [E1 unifiable?] [E2 unifiable?])]{@racket[%/=] is the negation of @racket[%=].
The goal @racket[(%/= E1 E2)] succeeds if @racket[E1] can not be unified
with @racket[E2].}

@defpred[(%== [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%== E1 E2)] succeeds if @racket[E1] is @emph{identical}
to @racket[E2].  They should be structurally equal.  If containing
logic variables, they should have the same variables in the
same position.  Unlike a @racket[%=]-call, this goal will not bind
any logic variables.}

@defpred[(%/== [E1 unifiable?] [E2 unifiable?])]{
@racket[%/==] is the negation of @racket[%==].
The goal @racket[(%/== E1 E2)] succeeds if @racket[E1] and @racket[E2] are not
identical.}

@defform[(%is E1 E2)]{
The goal @racket[(%is E1 E2)] unifies with @racket[E1] the result of
evaluating @racket[E2] as a Racket expression.  @racket[E2] may contain
logic variables, which are dereferenced automatically.
Fails if @racket[E2] contains unbound logic variables.}

@defparam[use-occurs-check? on? boolean?]{
If this is false (the default), 
Racklog's unification will not use the occurs check.
If it is true, the occurs check is enabled.}

@subsection{Numeric Predicates}

@defpred[(%< [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%< E1 E2)] succeeds if @racket[E1] and @racket[E2] are bound to
numbers and @racket[E1] is less than @racket[E2].}

@defpred[(%<= [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%<= E1 E2)] succeeds if @racket[E1] and @racket[E2] are bound to
numbers and @racket[E1] is less than or equal to @racket[E2].}

@defpred[(%=/= [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%=/= E1 E2)] succeeds if @racket[E1] and @racket[E2] are bound to
numbers and @racket[E1] is not equal to @racket[E2].}

@defpred[(%=:= [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%=:= E1 E2)] succeeds if @racket[E1] and @racket[E2] are bound to
numbers and @racket[E1] is equal to @racket[E2].}

@defpred[(%> [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%> E1 E2)] succeeds if @racket[E1] and @racket[E2] are bound to
numbers and @racket[E1] is greater than @racket[E2].}

@defpred[(%>= [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%>= E1 E2)] succeeds if @racket[E1] and @racket[E2] are bound to
numbers and @racket[E1] is greater than or equal to @racket[E2].}

@subsection{List Predicates}

@defpred[(%append [E1 unifiable?] [E2 unifiable?] [E3 unifiable?])]{
The goal @racket[(%append E1 E2 E3)] succeeds if @racket[E3] is unifiable
with the list obtained by appending @racket[E1] and @racket[E2].}

@defpred[(%member [E1 unifiable?] [E2 unifiable?])]{
The goal @racket[(%member E1 E2)] succeeds if @racket[E1] is a member
of the list in @racket[E2].}

@subsection{Set Predicates}

@defpred[(%set-of [E1 unifiable?] [G goal/c] [E2 unifiable?])]{
The goal @racket[(%set-of E1 G E2)] unifies with @racket[E2] the @emph{set}
of all the
instantiations of @racket[E1] for which goal @racket[G] succeeds.}

@defpred[(%set-of-1 [E1 unifiable?] [G goal/c] [E2 unifiable?])]{
Similar to @racket[%set-of], but fails if the set is empty.}

@defpred[(%bag-of [E1 unifiable?] [G goal/c] [E2 unifiable?])]{
The goal @racket[(%bag-of E1 G E2)] unifies with @racket[E2] the @emph{bag}
(multiset)
of all the
instantiations of @racket[E1] for which goal @racket[G] succeeds.}

@defpred[(%bag-of-1 [E1 unifiable?] [G goal/c] [E2 unifiable?])]{
Similar to @racket[%bag-of], but fails if the bag is empty.}

@defform[(%free-vars (V ...) G)
         #:contracts ([V identifier?]
                      [G goal/c])]{
Identifies
the occurrences of the variables @racket[V], ..., in goal
@racket[G] as free.  It is used to avoid existential quantification
in calls to set predicates (@racket[%bag-of], @racket[%set-of], &c.).}

@subsection{Racklog Predicates}

@defpred[(%compound [E unifiable?])]{
The goal @racket[(%compound E)] succeeds if @racket[E] is a compound
value.}

@defpred[(%constant [E unifiable?])]{
The goal @racket[(%constant E)] succeeds if @racket[E] is an atomic
value.}

@defpred[(%var [E unifiable?])]{
The goal @racket[(%var E)] succeeds if @racket[E] is not completely
instantiated, ie, it has at least one unbound variable in
it.}

@defpred[(%nonvar [E unifiable?])]{
@racket[%nonvar] is the negation of @racket[%var].
The goal @racket[(%nonvar E)] succeeds if @racket[E] is completely
instantiated, ie, it has no unbound variable in it.}

@subsection{Racklog Variable Manipulation}

@defpred[(%freeze [S unifiable?] [F unifiable?])]{
The goal @racket[(%freeze S F)] unifies with @racket[F] a new frozen
version of the structure in @racket[S].  Freezing implies that all
the unbound variables are preserved.  @racket[F] can henceforth be
used as @emph{bound} object with no fear of its variables
getting bound by unification.}

@defpred[(%melt [F unifiable?] [S unifiable?])]{
The goal @racket[(%melt F S)] unifies @racket[S] with the thawed
(original) form of the frozen structure in @racket[F].}

@defpred[(%melt-new [F unifiable?] [S unifiable?])]{
The goal @racket[(%melt-new F S)] unifies @racket[S] with a thawed
@emph{copy} of the frozen structure in @racket[F].  This means
new logic variables are used for unbound logic variables in
@racket[F].}

@defpred[(%copy [F unifiable?] [S unifiable?])]{
The goal @racket[(%copy F S)] unifies with @racket[S] a copy of the
frozen structure in @racket[F].}

@bibliography[
 @bib-entry[#:key "aop" 
                  #:author "Leon Sterling and Ehud Shapiro"
                  #:url "http://mitpress.mit.edu/book-home.tcl?isbn=0262193388"
                  #:title "The Art of Prolog, 2nd Edition"
                  #:location "MIT Press"
                  #:date "1994"
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
 @bib-entry[#:key "mf:prolog"
    #:author "Matthias Felleisen"
    #:title "Transliterating Prolog into Scheme"
    #:location "Indiana U Comp Sci Dept Tech Report #182"
    #:date "1985"]
 ]


@close-eval[racklog-eval]
