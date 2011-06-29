#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          "utils.rkt")

@title[#:tag "top"]{Datalog: Deductive Database Programming}
@author[(author+email "Jay McCarthy" "jay@racket-lang.org")]

@link["http://en.wikipedia.org/wiki/Datalog"]{Datalog} is 
@itemize[
 @item{a declarative logic language in which each
formula is a function-free Horn clause, and every variable
in the head of a clause must appear in the body of the clause.}
 @item{a lightweight deductive database system where queries and database updates are expressed
       in the logic language.}
]

The use of Datalog syntax and an implementation based
on tabling intermediate results ensures that all queries terminate.

@table-of-contents[]

@section[#:tag "datalog"]{Datalog Module Language}

@defmodulelang[@racketmodname[datalog] #:module-paths (datalog/lang/reader)]

In Datalog input, whitespace characters are ignored except when they separate adjacent tokens or when they occur in strings.
Comments are also considered to be whitespace. The character @litchar["%"] introduces a comment, which extends to the next line break.
Comments do not occur inside strings.

A variable is a sequence of Unicode "Uppercase" and "Lowercase" letters, digits, and the underscore character. A variable must begin with a Unicode "Uppercase" letter.

An identifier is a sequence of printing characters that does not contain any of the following characters: @litchar["("], @litchar["`"],
@litchar["'"], @litchar[")"], @litchar["="], @litchar[":"], @litchar["."], @litchar["~"], @litchar["?"], @litchar["\""], @litchar["%"], and space.
An identifier must not begin with a Latin capital letter. Note that the characters that start punctuation are forbidden in identifiers,
but the hyphen character is allowed.

A string is a sequence of characters enclosed in double quotes. Characters other than double quote, newline, and backslash may be directly
included in a string. The remaining characters may be specified using escape characters, @litchar["\\\""], @litchar["\\\n"], and
@litchar["\\\\"] respectively.

A literal, is a predicate symbol followed by an optional parenthesized list of comma separated terms. A predicate symbol is either an identifier
or a string. A term is either a variable or a constant. As with predicate symbols, a constant is either an identifier or a string. As a special case,
two terms separated by @litchar["="] (@litchar["!="]) is a literal for the equality (inequality) predicate.
The following are literals:
@verbatim[#:indent 4 #<<END
parent(john, douglas)
zero-arity-literal
"="(3,3)
""(-0-0-0,&&&,***,"\00")
END
]

A clause is a head literal followed by an optional body. A body is a comma separated list of literals. A clause without a body is called a @deftech{fact},
and a rule when it has one. The punctuation @litchar[":-"] separates the head of a rule from its body. A clause is safe if every variable in its head
occurs in some literal in its body. The following are safe clauses:

@verbatim[#:indent 4 #<<END
parent(john, douglas)
ancestor(A, B) :-
    parent(A, B)
ancestor(A, B) :-
    parent(A, C),
    ancestor(C, B)
END
]

A program is a sequence of zero or more statements. A statement is an assertion, a retraction, or a query. An assertion is a clause followed by
a period, and it adds the clause to the database if it is safe. A retraction is a clause followed by a tilde, and it removes the clause from
the database. A query is a literal followed by a question mark. 

The following BNF describes the syntax of Datalog.

@BNF[
 (list (nonterm "program")
       (kleenestar (nonterm "statement")))
 (list (nonterm "statement")
       (nonterm "assertion")
       (nonterm "retraction")
       (nonterm "query"))
 (list (nonterm "assertion")
       (BNF-seq (nonterm "clause") (litchar ".")))
 (list (nonterm "retraction")
       (BNF-seq (nonterm "clause") (litchar "~")))
 (list (nonterm "query")
       (BNF-seq (nonterm "literal") (litchar "?")))
 (list (nonterm "clause")
       (BNF-seq (nonterm "literal") (litchar ":-") (nonterm "body"))
       (nonterm "literal"))
 (list (nonterm "body")
       (BNF-seq (nonterm "literal") (litchar ",") (nonterm "body"))
       (nonterm "literal"))
 (list (nonterm "literal")
       (BNF-seq (nonterm "predicate-sym") (litchar "(") (litchar ")"))
       (BNF-seq (nonterm "predicate-sym") (litchar "(") (nonterm "terms") (litchar ")"))
       (nonterm "predicate-sym")
       (BNF-seq (nonterm "term") (litchar "=") (nonterm "term"))
       (BNF-seq (nonterm "term") (litchar "!=") (nonterm "term")))
 (list (nonterm "predicate-sym")
       (nonterm "IDENTIFIER")
       (nonterm "STRING"))
 (list (nonterm "terms")
       (nonterm "term")
       (BNF-seq (nonterm "term") (litchar ",") (nonterm "terms")))
 (list (nonterm "term")
       (nonterm "VARIABLE")
       (nonterm "constant"))
 (list (nonterm "constant")
       (nonterm "IDENTIFIER")
       (nonterm "STRING"))
]

The effect of running a Datalog program is to modify the database as directed
by its statements, and then to return the literals designated by the query.
The modified database is provided as @racket[theory].

The following is a program:

@verbatim[#:indent 4 #<<END
     #lang datalog
     edge(a, b). edge(b, c). edge(c, d). edge(d, a).
     path(X, Y) :- edge(X, Y).
     path(X, Y) :- edge(X, Z), path(Z, Y).
     path(X, Y)?
END
]

The Datalog REPL accepts new statements that are executed as if they were in the original program text.

@include-section["tutorial.scrbl"]

@section{Parenthetical Datalog Module Language}
@(require (for-label datalog
                     racket))

@defmodulelang[datalog/sexp]

The semantics of this language is the same as the normal Datalog language, except it uses the parenthetical syntax described in @secref{interop}.

All identifiers in @racketmodname[racket/base] are available for use as predicate symbols or constant values. Top-level identifiers and datums are not otherwise allowed in the program. The program may contain @racket[require] expressions.

The following is a program:
@racketmod[datalog/sexp

(! (edge a b))
(! (edge b c))
(! (edge c d))
(! (edge d a))
(! (:- (path X Y)
       (edge X Y)))
(! (:- (path X Y)
       (edge X Z)
       (path Z Y)))
(? (path X Y))]

This is also a program:
@racketmod[datalog/sexp
(require racket/math)

(? (sqr 4 :- X))]

The Parenthetical Datalog REPL accepts new statements that are executed as if they were in the original program text, except @racket[require] is not allowed.

@include-section["racket.scrbl"]

@section{Acknowledgments}

This package was once structurally based on Dave Herman's @racketmodname[(planet dherman/javascript)] library and
John Ramsdell's @link["http://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html"]{Datalog library}. 

The package uses the tabled logic programming algorithm described in
@link["http://scholar.google.com/scholar?q=author:%22Chen%22+intitle:%22Efficient+top-down+computation+of+queries+under+the+...%22+&oi=scholarr"]{Efficient Top-Down Computation of Queries under the Well-Founded Semantics} by W. Chen, T. Swift, and D. S. Warren.
Another important reference is 
@link["http://portal.acm.org/citation.cfm?id=227597"]{Tabled Evaluation with Delaying for General Logic Programs} by W. Chen and D. S. Warren.
Datalog is described in 
@link["http://doi.ieeecomputersociety.org/10.1109/69.43410"]{What You Always Wanted to Know About Datalog (And Never Dared to Ask)}
by Stefano Ceri, Georg Gottlob, and Letizia Tanca.
