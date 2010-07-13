#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (for-label racket/base
                     racket/contract
                     "../main.rkt")
          "utils.rkt")

@title{Tutorial}

Start DrRacket and type

@racketmod[datalog]

in the Definitions window. Click @onscreen{Run}, then click in the REPL.

@racketinput[]

@tech{Facts} are stored in tables. If the name of the table is @litchar["parent"], and 
@litchar["john"] is the parent of @litchar["douglas"], store the fact in the database with
this:

@racketinput[#,(tt "parent(john, douglas).")]

Each item in the parenthesized list following the name of the table is called a @deftech{term}.
A term can be either a logical @racket[variable] or a @racket[constant].
Thus far, all the terms shown have been constant terms.

A query can be used to see if a particular row is in a table. Type this to see if @litchar["john"]
is the parent of @litchar["douglas"]:

@racketinput[#,(tt "parent(john, douglas)?")]
@racketblock[#,(racketresultfont (tt "parent(john, douglas)."))]

Type this to see if @litchar["john"] is the parent of @litchar["ebbon"]:

@racketinput[#,(tt "parent(john, ebbon)?")]

The query produced no results because @litchar["john"] is not the parent of @litchar["ebbon"]. Let's add more rows.

@racketinput[#,(tt "parent(bob, john).")]
@racketinput[#,(tt "parent(ebbon, bob).")]

Type the following to list all rows in the @litchar["parent"] table:

@racketinput[#,(tt "parent(A, B)?")]
@racketblock[#,(racketresultfont (tt "parent(john, douglas)."))]
@racketblock[#,(racketresultfont (tt "parent(bob, john)."))]
@racketblock[#,(racketresultfont (tt "parent(ebbon, bob)."))]

Type the following to list all the children of @litchar["john"]:

@racketinput[#,(tt "parent(john, B)?")]
@racketblock[#,(racketresultfont (tt "parent(john, douglas)."))]

A term that begins with a capital letter is a logical variable.When producing a set of answers, the
Datalog interpreter lists all rows that match the query when each variable in the query is substituted
for a constant. The following example produces no answers, as there are no substitutions for the variable
@litchar["A"] that produce a fact in the database. This is because no one is the parent of oneself.

@racketinput[#,(tt "parent(A, A)?")]

A deductive database can use rules of inference to derive new facts. Consider the following rule:

@racketinput[#,(tt "ancestor(A, B) :- parent(A, B).")]

The rule says that if A is the parent of B, then A is an ancestor of B.
The other rule defining an ancestor says that if A is the parent of C,
C is an ancestor of B, then A is an ancestor of B.

@racketinput[#,(tt "ancestor(A, B) :-")
             #,(tt "  parent(A, C),")
             #,(tt "  ancestor(C, B).")]

In the interpreter, DrRacket knows that the clause is not complete, so by pressing Return, it doesn't interpret the line.

Rules are used to answer queries just as is done for facts.

@racketinput[#,(tt "ancestor(A, B)?")]
@racketblock[#,(racketresultfont (tt "ancestor(ebbon, bob)."))]
@racketblock[#,(racketresultfont (tt "ancestor(bob, john)."))]
@racketblock[#,(racketresultfont (tt "ancestor(john, douglas)."))]
@racketblock[#,(racketresultfont (tt "ancestor(bob, douglas)."))]
@racketblock[#,(racketresultfont (tt "ancestor(ebbon, john)."))]
@racketblock[#,(racketresultfont (tt "ancestor(ebbon, douglas)."))]
@racketinput[#,(tt "ancestor(X,john)?")]
@racketblock[#,(racketresultfont (tt "ancestor(bob, john)."))]
@racketblock[#,(racketresultfont (tt "ancestor(ebbon, john)."))]

A fact or a rule can be retracted from the database using tilde syntax:

@racketinput[#,(tt "parent(bob, john)~")]
@racketinput[#,(tt "parent(A, B)?")]
@racketblock[#,(racketresultfont (tt "parent(john, douglas)."))]
@racketblock[#,(racketresultfont (tt "parent(ebbon, bob)."))]
@racketinput[#,(tt "ancestor(A, B)?")]
@racketblock[#,(racketresultfont (tt "ancestor(ebbon, bob)."))]
@racketblock[#,(racketresultfont (tt "ancestor(john, douglas)."))]

Unlike Prolog, the order in which clauses are asserted is irrelevant. All queries terminate, and every possible answer is derived.

@racketinput[#,(tt "q(X) :- p(X).")]
@racketinput[#,(tt "q(a).")]
@racketinput[#,(tt "p(X) :- q(X).")]
@racketinput[#,(tt "q(X)?")]
@racketblock[#,(racketresultfont (tt "q(a)."))]
