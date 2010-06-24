#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (for-label racket/base
                     racket/contract
                     "../main.rkt")
          "utils.rkt")

@title[#:tag "top"]{@bold{Datalog} for PLT Scheme}
@author[(author+email "Jay McCarthy" "jay@racket-lang.org")]

This package contains a lightweight deductive database system. Queries and database updates are expressed
using @link["http://en.wikipedia.org/wiki/Datalog"]{Datalog}---a declarative logic language in which each
formula is a function-free Horn clause, and every variable
in the head of a clause must appear in the body of the clause. The use of Datalog syntax and an implementation based
on tabling intermediate results ensures that all queries terminate.

@table-of-contents[]

@section{Datalog for PLT Scheme}

@subsection{Getting Started}

The easiest way to get started using Datalog for PLT Scheme is with the main module:

@defmodule[datalog]

This module provides everything in the entire package. Subsequent sections of this
manual describe the functionality of the individual libraries included, which can also be
required individually.

@examples[#:eval the-eval
          (define example-program
            #<<END
ancestor(A, B) :-
    parent(A, B).
ancestor(A, B) :-
    parent(A, C),
    D = C,
    ancestor(D, B).
parent(john, douglas).
parent(bob, john).
parent(ebbon, bob).
ancestor(A, B)?
END
            )
          (pretty-print
           (format-program
            (parse-program
             (open-input-string
              example-program))))
          (void
           (eval-program/fresh
            (parse-program
             (open-input-string
              example-program))))]

@subsection{Datalog Language for DrScheme}

Installing this PLaneT package also automatically registers a DrScheme language called @tt{Datalog},
filed under @tt{Experimental Languages}. Selecting the @tt{Datalog} language from DrScheme's
@tt{Language} menu allows you to create and load Datalog programs and interact with Datalog at
the REPL.

Datalog is also available as a module language. This can be used by beginning a Datalog source file with the line:

@defmodulelang[datalog]

You can omit the PLaneT version numbers if you prefer. Programs without the version number
do not need to be updated when this PLaneT package is upgraded. However, it is then the
responsibility of the programmer to address any backwards-incompatible changes to the
Datalog semantics.

@section{Tutorial}

Start DrScheme and choose the @tt{Datalog} language from DrScheme's
@tt{Language} menu under @tt{Experimental Languages}. Click @onscreen{Run}, then
click in the REPL.

@racketinput[]

@tech{Facts} are stored in tables. If the name of the table is @litchar["parent"], and 
@litchar["john"] is the parent of @litchar["douglas"], store the fact in the database with
this:

@racketinput[#,(tt "parent(john, douglas).")]

Each item in the parenthesized list following the name of the table is called a @tech{term}.
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

In the interpreter, DrScheme knows that the clause is not complete, so by pressing Return, it doesn't interpret the line.

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

@section{Abstract Syntax}

This library provides the structures that represent Datalog syntax. It can be required via:

@defmodule[datalog/ast]

@defthing[srcloc/c contract?]{
 Contract for the third argument to @racket[datum->syntax].
 
 Equivalent to
 @racketblock[
 (or/c syntax?
       false/c
       (list/c any/c
               (or/c exact-positive-integer? #f)
               (or/c exact-nonnegative-integer? #f)
               (or/c exact-nonnegative-integer? #f)
               (or/c exact-positive-integer? #f)))
 ]
}

@defthing[datum/c contract?]{
 Contract for @deftech{datum}s.
 Equivalent to @racket[(or/c string? symbol?)].
}

@defproc[(datum-equal? [d1 datum/c] [d2 datum/c])
         boolean?]{
 Equivalent to @racket[(equal? d1 d2)].
            
 @examples[#:eval the-eval
           (datum-equal? 'sym1 'sym2)
           (datum-equal? 'sym1 'sym1)
           (datum-equal? "str1" "str2")
           (datum-equal? "str1" "str1")]
}
                  
@defstruct[variable ([srcloc srcloc/c]
                     [sym symbol?])]{
 A logic variable in Datalog. 
 (This structure does not enforce the requirements for what characters variables can contain, so if you print one out,
 it might not be parseable, but it will be executeable.)
}
                                      
@defproc[(variable-equal? [v1 variable?] [v2 variable?])
         boolean?]{
 Equivalent to @racket[(equal? v1 v2)] modulo source location.
            
 @examples[#:eval the-eval
           (variable-equal? (make-variable #f 'sym) 
                            (make-variable #'sym 'sym))
           (variable-equal? (make-variable #f 'sym1) 
                            (make-variable #f 'sym2))]
}
                  
@defstruct[constant ([srcloc srcloc/c]
                     [datum datum/c])]{
 A constant in Datalog.
 (This structure does not enforce the requirements for what characters constants can contain, so if you print one out,
 it might not be parseable, but it will be executeable.)
}
                                      
@defproc[(constant-equal? [c1 constant?] [c2 constant?])
         boolean?]{
 Equivalent to @racket[(equal? c1 c2)] modulo source location.
            
 @examples[#:eval the-eval
           (constant-equal? (make-constant #f 'sym)
                            (make-constant #'sym 'sym))
           (constant-equal? (make-constant #f 'sym) 
                            (make-constant #f "str"))]
}
                                      
@defthing[term/c contract?]{
 Contract for @deftech{term}s. Equivalent to @racket[(or/c variable? constant?)].
}

@defproc[(term-equal? [t1 term/c] [t2 term/c])
         boolean?]{
 Equivalent to @racket[(equal? t1 t2)] modulo source location.
            
 @examples[#:eval the-eval
           (term-equal? (make-constant #f 'sym) (make-constant #'sym 'sym))
           (term-equal? (make-constant #f 'sym) (make-constant #f "str"))]
}

@defstruct[literal ([srcloc srcloc/c]
                    [predicate datum/c]
                    [terms (listof term/c)])]{
 A literal in Datalog.
}
                                             
@defproc[(literal-equal? [l1 literal?] [l2 literal?])
         boolean?]{
 Equivalent to @racket[(equal? l1 l2)] modulo source location.

 @examples[#:eval the-eval
  (literal-equal? (make-literal #f 'ancestor (list))
                  (make-literal #'ancestor 'ancestor (list)))
  (literal-equal? (make-literal #f 'ancestor (list))
                  (make-literal #f 'parent (list)))
  (literal-equal? (make-literal #f 'ancestor (list))
                  (make-literal #f 'ancestor 
                                (list (make-constant #f 'jack))))]
}
                                             
@defstruct[clause ([srcloc srcloc/c]
                   [head literal?]
                   [body (listof literal?)])]{
 A Datalog clause.
}
                                             
@defproc[(clause-equal? [c1 clause?] [c2 clause?])
         boolean?]{
 Equivalent to @racket[(equal? c1 c2)] modulo source location.
            
 @examples[#:eval the-eval
  (clause-equal? 
   (make-clause #f (make-literal #f 'ancestor (list)) (list))
   (make-clause #'clause 
                (make-literal #f 'ancestor (list)) (list)))
  (clause-equal? 
   (make-clause #f (make-literal #f 'ancestor (list)) (list))
   (make-clause #f (make-literal #f 'parent (list)) (list)))]
}
                  
@defstruct[assertion ([srcloc srcloc/c]
                      [clause clause?])]{
 A Datalog assertion.
}

@defstruct[retraction ([srcloc srcloc/c]
                       [clause clause?])]{
 A Datalog retraction.
}
                                        
@defstruct[query ([srcloc srcloc/c]
                  [clause clause?])]{
 A Datalog query.
}

@defthing[statement/c contract?]{
 Contract for @deftech{statement}s.
 Equivalent to @racket[(or/c assertion? retraction? query?)].
}

@defthing[program/c contract?]{
 Contract for @deftech{program}s.
 Equivalent to @racket[(listof statement/c)].
}
                                 
@section{Lexing and Parsing}

This library provides facilities for parsing Datalog source. It can be required via:

@defmodule[datalog/parse]

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

@subsection{Parser API}

@defproc[(parse-literal [ip input-port?])
         literal?]{
 Parses a @racket[literal] from @racket[ip].
        
 @examples[#:eval the-eval
  (parse-literal (open-input-string "parent(john,douglas)"))
  (parse-literal (open-input-string "zero-arity-literal"))
  (parse-literal (open-input-string "\"=\"(3,3)"))
  (parse-literal 
   (open-input-string "\"\"(-0-0-0,&&&,***,\"\00\")"))
  (parse-literal (open-input-string "3 = 3"))]
}
                  
@defproc[(parse-clause [ip input-port?])
         clause?]{
 Parses a @racket[clause] from @racket[ip].
        
 @examples[#:eval the-eval
  (parse-clause
   (open-input-string "parent(john, douglas)"))
  (parse-clause
   (open-input-string "ancestor(A, B) :- parent(A, B)"))
  (parse-clause
   (open-input-string 
    (string-append "ancestor(A, B) :- parent(A, C),"
                   "ancestor(C, B)")))]
}
                 
@defproc[(parse-statement [ip input-port?])
         statement/c]{
 Parses a @tech{statement} from @racket[ip].
        
 @examples[#:eval the-eval
  (parse-statement
   (open-input-string "parent(john, douglas)."))
  (parse-statement
   (open-input-string "parent(john, douglas)~"))
  (parse-statement
   (open-input-string "parent(john, douglas)?"))]
}
                     
@defproc[(parse-program [ip input-port?])
         program/c]{
 Parses a @tech{program} from @racket[ip].
        
 @examples[#:eval the-eval
  (parse-program
   (open-input-string
    (string-append 
     "edge(a, b). edge(b, c)."
     "edge(c, d). edge(d, a)."
     "path(X, Y) :- edge(X, Y)."
     "path(X, Y) :- edge(X, Z), path(Z, Y)."
     "path(X, Y)?")))]
}

@section{Parenthetical Datalog}

This package recognizes an alternative, Scheme-like front-end syntax for Datalog. It can be required via:

@defmodule[datalog/sexp]

@subsection{Parenthetical Datalog Syntax}

@racketgrammar*[
 #:literals (:- ! ~ ?)
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

@subsection{Parethetical Parser API}

@defproc[(stx->term [stx syntax?])
         term/c]{
 Parses @racket[stx] as a @tech{term}.
}
@defproc[(stx->literal [stx syntax?])
         literal?]{
 Parses @racket[stx] as a @racket[literal].
}
@defproc[(stx->clause [stx syntax?])
         clause?]{
 Parses @racket[stx] as a @racket[clause].
}
@defproc[(stx->statement [stx syntax?])
         statement/c]{
 Parses @racket[stx] as a @tech{statement}.
}
@defproc[(stx->program [stx syntax?])
         program/c]{
 Parses @racket[stx] as a @tech{program}.
}
                
@defproc[(sexp->term [sexp sexpr?]) term/c]{@racket[stx->term] composed with @racket[datum->syntax].}
@defproc[(sexp->literal [sexp sexpr?]) literal?]{@racket[stx->literal] composed with @racket[datum->syntax].}
@defproc[(sexp->clause [sexp sexpr?]) clause?]{@racket[stx->clause] composed with @racket[datum->syntax].}
@defproc[(sexp->statement [sexp sexpr?]) statement/c]{@racket[stx->statement] composed with @racket[datum->syntax].}
@defproc[(sexp->program [sexp sexpr?]) program/c]{@racket[stx->program] composed with @racket[datum->syntax].}

@section{Pretty-Printing}

This library provides facilities for pretty-printing Datalog source. It can be required via:

@defmodule[datalog/pretty]

@defproc[(format-datum [d datum/c])
         doc?]{
 Formats a @tech{datum}.
         
 @examples[#:eval the-eval
  (pretty-print (format-datum 'sym))
  (pretty-print (format-datum "str"))]
}
              
@defproc[(format-variable [v variable?])
         doc?]{
 Formats a @racket[variable].
         
 @examples[#:eval the-eval
  (pretty-print (format-variable (make-variable #f 'Ancestor)))]
}

@defproc[(format-constant [c constant?])
         doc?]{
 Formats a @racket[constant].

 @examples[#:eval the-eval
  (pretty-print (format-constant (make-constant #f 'joseph)))
  (pretty-print (format-constant (make-constant #f "whom")))]
}
 
@defproc[(format-term [t term/c])
         doc?]{
 Formats a @tech{term}.

 @examples[#:eval the-eval
  (pretty-print (format-term (make-variable #f 'Ancestor)))
  (pretty-print (format-term (make-constant #f 'joseph)))
  (pretty-print (format-term (make-constant #f "whom")))]
}

@defproc[(format-literal [l literal?])
         doc?]{
 Formats a @racket[literal].
         
 @examples[#:eval the-eval
  (pretty-print (format-literal (make-literal #f 'true (list))))
  (pretty-print 
   (format-literal
    (make-literal #f 'ancestor
     (list (make-variable #f 'A) (make-constant #f 'jay)))))
  (pretty-print 
   (format-literal
    (make-literal #f '=
     (list (make-constant #f 'joseph) (make-constant #f 'jay)))))]
}
              
@defproc[(format-literals [ls (listof literal?)])
         doc?]{
 Formats a list of @racket[literal]s as @racket[assertion]s for formatting @racket[prove] results.
         
 @examples[#:eval the-eval
  (pretty-print 
   (format-literals
    (list 
     (make-literal #f 'true (list))
     (make-literal #f 'ancestor
      (list (make-constant #f 'joseph) (make-constant #f 'jay)))
     (make-literal #f '=
      (list (make-constant #f 'joseph) (make-constant #f 'jay))))))]
}

@defproc[(format-clause [c clause?])
         doc?]{
 Formats a @racket[clause].
         
 @examples[#:eval the-eval
  (pretty-print
   (format-clause
    (make-clause
     #f (make-literal #f 'ancestor
         (list (make-constant #f 'joseph) 
               (make-constant #f 'jay)))
     (list))))
  (pretty-print
   (format-clause
    (make-clause
     #f (make-literal 
         #f 'ancestor 
         (list (make-constant #f 'A) (make-constant #f 'B)))
     (list (make-literal 
            #f 'parent
            (list (make-constant #f 'A) (make-constant #f 'B)))))))
  (pretty-print
   (format-clause
    (make-clause
     #f (make-literal 
         #f 'ancestor 
         (list (make-constant #f 'A) (make-constant #f 'B)))
     (list (make-literal
            #f 'parent
            (list (make-constant #f 'A) (make-constant #f 'C)))
           (make-literal
            #f 'ancestor
            (list (make-constant #f 'C) (make-constant #f 'B)))))))]
}
              
@defproc[(format-assertion [a assertion?])
         doc?]{
 Formats a @racket[assertion].

 @examples[#:eval the-eval
  (pretty-print
   (format-assertion
    (make-assertion
     #f (make-clause
      #f (make-literal #f 'ancestor
          (list (make-constant #f 'joseph) 
                (make-constant #f 'jay)))
      (list)))))]
}

@defproc[(format-retraction [r retraction?])
         doc?]{
 Formats a @racket[retraction].

 @examples[#:eval the-eval
  (pretty-print
   (format-retraction
    (make-retraction
     #f (make-clause
      #f (make-literal #f 'ancestor
          (list (make-constant #f 'joseph) 
                (make-constant #f 'jay)))
      (list)))))]
}
              
@defproc[(format-query [q query?])
         doc?]{
 Formats a @racket[query].

 @examples[#:eval the-eval
  (pretty-print
   (format-query
    (make-query
     #f (make-literal #f 'ancestor
         (list (make-constant #f 'joseph) 
               (make-constant #f 'jay))))))]
}

@defproc[(format-statement [s statement/c])
         doc?]{
 Formats a @tech{statement}.

 @examples[#:eval the-eval
  (pretty-print
   (format-statement
    (make-query
     #f (make-literal #f 'ancestor
         (list (make-constant #f 'joseph) 
               (make-constant #f 'jay))))))]
}

@defproc[(format-program [p program/c])
         doc?]{
 Formats a @tech{program}.

 @examples[#:eval the-eval
  (pretty-print
   (format-program
    (list
     (make-assertion
      #f (make-clause
       #f (make-literal #f 'ancestor
           (list (make-constant #f 'joseph) 
                 (make-constant #f 'jay)))
       (list)))
     (make-query
      #f (make-literal #f 'ancestor
          (list (make-constant #f 'joseph) 
                (make-constant #f 'jay)))))))]
}

@section{Runtime System}

This library implements the Datalog runtime system. It can be required via:

@defmodule[datalog/runtime]

@defthing[theory/c contract?]{
 A contract for @deftech{theories}.
}

@defthing[immutable-theory/c contract?]{
 A contract for immutable @tech{theories}.
}   

@defthing[mutable-theory/c contract?]{
 A contract for mutable @tech{theories}.
}

@defproc[(make-mutable-theory)
         mutable-theory/c]{
 Constructs a mutable @tech{theory}.
}
                          
@defproc[(make-immutable-theory)
         immutable-theory/c]{
 Constructs a immutable @tech{theory}.
}
                          
@defproc[(safe-clause? [c clause?])
         boolean?]{
 Determines if a @racket[clause] is safe.
 A @racket[clause] is safe if every @racket[variable] in its head occurs in some @racket[literal] in its body.
 
 @examples[#:eval the-eval
  (safe-clause? 
   (parse-clause (open-input-string "ancestor(joseph,jay)")))
  (safe-clause?
   (parse-clause 
    (open-input-string "ancestor(A,B) :- parent(A,B)")))
  (safe-clause?
   (parse-clause
    (open-input-string "ancestor(A,B) :- parent(A,jay)")))]
}

@defproc[(assume [thy immutable-theory/c]
                 [c safe-clause?])
         immutable-theory/c]{
 Adds @racket[c] to @racket[thy] in a persistent way.
}

@defproc[(retract [thy immutable-theory/c]
                  [c clause?])
         immutable-theory/c]{
 Removes @racket[c] from @racket[thy] in a persistent way.
}

@defproc[(assume! [thy mutable-theory/c]
                  [c safe-clause?])
         mutable-theory/c]{
 Adds @racket[c] to @racket[thy].
}

@defproc[(retract! [thy mutable-theory/c]
                   [c clause?])
         mutable-theory/c]{
 Removes @racket[c] from @racket[thy].
}

@defproc[(prove [thy theory/c]
                [l literal?])
         (listof literal?)]{
 Attempts to prove @racket[l] using the @tech{theory} @racket[thy], returning all
 the results of the query.
 
 @examples[#:eval the-eval
  (pretty-print
   (format-literals
    (prove 
     (assume 
      (make-immutable-theory)
      (parse-clause (open-input-string "parent(joseph1,joseph2)")))
     (parse-literal 
      (open-input-string "parent(joseph1,joseph2)")))))
  (pretty-print
   (format-literals
    (prove
     (retract
      (assume
       (make-immutable-theory)
       (parse-clause 
        (open-input-string "parent(joseph1,joseph2)")))
      (parse-clause (open-input-string "parent(joseph1,joseph2)")))
     (parse-literal 
      (open-input-string "parent(joseph1,joseph2)")))))
  (pretty-print
   (format-literals
    (prove
     (assume
      (make-immutable-theory)
      (parse-clause (open-input-string "parent(joseph1,joseph2)")))
     (parse-literal (open-input-string "parent(A,B)")))))]
}

@section{Evaluation}

This library provides facilities for evaluating Datalog. It can be required via:

@defmodule[datalog/eval]

@defthing[current-theory (parameter/c mutable-theory/c)]{
 The @tech{theory} used by @racket[eval-program] and @racket[eval-stmt].
}

@defproc[(eval-program [p program/c])
         void]{
 Evaluates @racket[p] using @racket[(current-theory)] as the @tech{theory}, printing query answers as it goes.
           
 This will raise a syntax error if given an @racket[assertion] of a @racket[clause] that is not a @racket[safe-clause?].
 
 @examples[#:eval the-eval
  (parameterize ([current-theory (make-mutable-theory)])
    (eval-program
     (parse-program
      (open-input-string
       (string-append
        "edge(a, b). edge(b, c). edge(c, d). edge(d, a)."
        "path(X, Y) :- edge(X, Y)."
        "path(X, Y) :- edge(X, Z), path(Z, Y)."
        "path(X, Y)?")))))
  (eval-program
   (parse-program
    (open-input-string
     "path(X, Y) :- edge(X, a).")))]
}
              
@defproc[(eval-statement [s statement/c])
         (or/c void (listof literal?))]{
 Evaluates @racket[s] using @racket[(current-theory)] as the @tech{theory}.
           
 This will raise a syntax error if given an @racket[assertion] of a @racket[clause] that is not a @racket[safe-clause?].

 @examples[#:eval the-eval
  (parameterize ([current-theory (make-mutable-theory)])
    (eval-statement
     (parse-statement
      (open-input-string
       "edge(a, b).")))
    (eval-statement
     (parse-statement
      (open-input-string
       "path(X, Y) :- edge(X, Y).")))
    (eval-statement
     (parse-statement
      (open-input-string
       "path(X, Y)?"))))
  (eval-statement
   (parse-statement
    (open-input-string
     "path(X, Y) :- edge(X, a).")))]
}
                                       
@defproc[(eval-program/fresh [p program/c])
         immutable-theory/c]{
 Evaluates @racket[p] in a fresh @tech{theory} and returns the final @tech{theory}, printing query answers as it goes.

 This will raise a syntax error if given an @racket[assertion] of a @racket[clause] that is not a @racket[safe-clause?].

 @examples[#:eval the-eval
  (void
   (eval-program/fresh
    (parse-program
     (open-input-string
      (string-append
       "edge(a, b). edge(b, c). edge(c, d). edge(d, a)."
       "path(X, Y) :- edge(X, Y)."
       "path(X, Y) :- edge(X, Z), path(Z, Y)."
       "path(X, Y)?")))))
  (eval-program/fresh
   (parse-program
    (open-input-string
     "path(X, Y) :- edge(X, a).")))]
}                                        
                            
@index-section[]

@section{Acknowledgments}

This package is based on Dave Herman's @racketmodname[(planet dherman/javascript)] library and
John Ramsdell's @link["http://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html"]{Datalog library}. 

The package uses the tabled logic programming algorithm described in
@link["http://scholar.google.com/scholar?q=author:%22Chen%22+intitle:%22Efficient+top-down+computation+of+queries+under+the+...%22+&oi=scholarr"]{Efficient Top-Down Computation of Queries under the Well-Founded Semantics} by W. Chen, T. Swift, and D. S. Warren.
Another important reference is 
@link["http://portal.acm.org/citation.cfm?id=227597"]{Tabled Evaluation with Delaying for General Logic Programs} by W. Chen and D. S. Warren.
Datalog is described in 
@link["http://doi.ieeecomputersociety.org/10.1109/69.43410"]{What You Always Wanted to Know About Datalog (And Never Dared to Ask)}
by Stefano Ceri, Georg Gottlob, and Letizia Tanca.