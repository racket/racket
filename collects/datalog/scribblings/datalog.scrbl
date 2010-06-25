#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (for-label racket/base
                     racket/contract
                     "../main.rkt")
          "utils.rkt")

@title[#:tag "top"]{@bold{Datalog}: Deductive database programming}
@author[(author+email "Jay McCarthy" "jay@racket-lang.org")]

This package contains a lightweight deductive database system. Queries and database updates are expressed
using @link["http://en.wikipedia.org/wiki/Datalog"]{Datalog}---a declarative logic language in which each
formula is a function-free Horn clause, and every variable
in the head of a clause must appear in the body of the clause. The use of Datalog syntax and an implementation based
on tabling intermediate results ensures that all queries terminate.

@table-of-contents[]

@section{Datalog Module Language}

@defmodulelang[datalog]

XXX Progress

@subsection{Datalog Syntax}

In Datalog input, whitespace characters are ignored except when they separate adjacent tokens or when they occur in strings.
Comments are also considered to be whitespace. The character @litchar["%"] introduces a comment, which extends to the next line break.
Comments do not occur inside strings.

A variable is a sequence of Latin capital and small letters, digits, and the underscore character. A variable must begin with a Latin capital letter.

An identifier is a sequence of printing characters that does not contain any of the following characters: @litchar["("], @litchar["`"],
@litchar["'"], @litchar[")"], @litchar["="], @litchar[":"], @litchar["."], @litchar["~"], @litchar["?"], @litchar["\""], @litchar["%"], and space.
An identifier must not begin with a Latin capital letter. Note that the characters that start punctuation are forbidden in identifiers,
but the hyphen character is allowed.

A string is a sequence of characters enclosed in double quotes. Characters other than double quote, newline, and backslash may be directly
included in a string. The remaining characters may be specified using escape characters, @litchar["\\\""], @litchar["\\\n"], and
@litchar["\\\\"] respectively.

A literal, is a predicate symbol followed by an optional parenthesized list of comma separated terms. A predicate symbol is either an identifier
or a string. A term is either a variable or a constant. As with predicate symbols, a constant is either an identifier or a string. As a special case,
two terms separated by @litchar["="] is a literal for the equality predicate.
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
the database. A query is a literal followed by a question mark. The effect of reading a Datalog program is to modify the database as directed
by its statements, and then to return the literal designated as the query. If no query is specified, a reader returns a literal know to have no
answers. The following is a program:

@verbatim[#:indent 4 #<<END
     edge(a, b). edge(b, c). edge(c, d). edge(d, a).
     path(X, Y) :- edge(X, Y).
     path(X, Y) :- edge(X, Z), path(Z, Y).
     path(X, Y)?
END
]

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
       (BNF-seq (nonterm "term") (litchar "=") (nonterm "term")))
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

@section{Parenthetical Datalog Module Language}

@defmodulelang[datalog/sexp]

XXX Progress

@subsection{Parenthetical Datalog Syntax}

@racketgrammar*[
 #:literals (:- ! ~ ? unquote)
 [program (begin statement ...)]
 [statement assertion
            retraction
            query]
 [assertion (! clause)]
 [retraction (~ clause)]
 [query (? literal)]
 [clause (:- literal literal ...)
         literal]
 [literal (datum term ...)]
 [term variable
       constant]
 [variable ,symbol]
 [constant datum]
 [datum symbol
        string]]

@include-section["racket.scrbl"]

@section{Acknowledgments}

This package is structurally based on Dave Herman's @racketmodname[(planet dherman/javascript)] library and
John Ramsdell's @link["http://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html"]{Datalog library}. 

The package uses the tabled logic programming algorithm described in
@link["http://scholar.google.com/scholar?q=author:%22Chen%22+intitle:%22Efficient+top-down+computation+of+queries+under+the+...%22+&oi=scholarr"]{Efficient Top-Down Computation of Queries under the Well-Founded Semantics} by W. Chen, T. Swift, and D. S. Warren.
Another important reference is 
@link["http://portal.acm.org/citation.cfm?id=227597"]{Tabled Evaluation with Delaying for General Logic Programs} by W. Chen and D. S. Warren.
Datalog is described in 
@link["http://doi.ieeecomputersociety.org/10.1109/69.43410"]{What You Always Wanted to Know About Datalog (And Never Dared to Ask)}
by Stefano Ceri, Georg Gottlob, and Letizia Tanca.