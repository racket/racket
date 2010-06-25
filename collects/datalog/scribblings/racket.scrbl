#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (for-label racket/base
                     racket/contract
                     "../main.rkt")
          "utils.rkt")

@title{Racket Interoperability}

@defmodule[datalog]

The Datalog database can be directly used by Racket programs through this API.

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
          (format-program
            (parse-program
             (open-input-string
              example-program)))
          (void
           (eval-program/fresh
            (parse-program
             (open-input-string
              example-program))))]


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
                  [literal literal?])]{
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

@section{Datalog Parsing}

This library provides facilities for parsing Datalog source. It can be required via:

@defmodule[datalog/parse]

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
                   
@section{Parenthetical Datalog Parsing}

This package recognizes an alternative, Scheme-like front-end syntax for Datalog. It can be required via:

@defmodule[datalog/sexp]

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
         string?]{
 Formats a @tech{datum}.
         
 @examples[#:eval the-eval
  (format-datum 'sym)
  (format-datum "str")]
}
              
@defproc[(format-variable [v variable?])
         string?]{
 Formats a @racket[variable].
         
 @examples[#:eval the-eval
  (format-variable (make-variable #f 'Ancestor))]
}

@defproc[(format-constant [c constant?])
         string?]{
 Formats a @racket[constant].

 @examples[#:eval the-eval
  (format-constant (make-constant #f 'joseph))
  (format-constant (make-constant #f "whom"))]
}
 
@defproc[(format-term [t term/c])
         string?]{
 Formats a @tech{term}.

 @examples[#:eval the-eval
  (format-term (make-variable #f 'Ancestor))
  (format-term (make-constant #f 'joseph))
  (format-term (make-constant #f "whom"))]
}

@defproc[(format-literal [l literal?])
         string?]{
 Formats a @racket[literal].
         
 @examples[#:eval the-eval
  (format-literal (make-literal #f 'true (list)))
  (format-literal
    (make-literal #f 'ancestor
     (list (make-variable #f 'A) (make-constant #f 'jay))))
  (format-literal
    (make-literal #f '=
     (list (make-constant #f 'joseph) (make-constant #f 'jay))))]
}
              
@defproc[(format-literals [ls (listof literal?)])
         string?]{
 Formats a list of @racket[literal]s as @racket[assertion]s for formatting @racket[prove] results.
         
 @examples[#:eval the-eval
  (format-literals
    (list 
     (make-literal #f 'true (list))
     (make-literal #f 'ancestor
      (list (make-constant #f 'joseph) (make-constant #f 'jay)))
     (make-literal #f '=
      (list (make-constant #f 'joseph) (make-constant #f 'jay)))))]
}

@defproc[(format-clause [c clause?])
         string?]{
 Formats a @racket[clause].
         
 @examples[#:eval the-eval
  (format-clause
    (make-clause
     #f (make-literal #f 'ancestor
         (list (make-constant #f 'joseph) 
               (make-constant #f 'jay)))
     (list)))
  (format-clause
    (make-clause
     #f (make-literal 
         #f 'ancestor 
         (list (make-constant #f 'A) (make-constant #f 'B)))
     (list (make-literal 
            #f 'parent
            (list (make-constant #f 'A) (make-constant #f 'B))))))
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
            (list (make-constant #f 'C) (make-constant #f 'B))))))]
}
              
@defproc[(format-assertion [a assertion?])
         string?]{
 Formats a @racket[assertion].

 @examples[#:eval the-eval
  (format-assertion
    (make-assertion
     #f (make-clause
      #f (make-literal #f 'ancestor
          (list (make-constant #f 'joseph) 
                (make-constant #f 'jay)))
      (list))))]
}

@defproc[(format-retraction [r retraction?])
         string?]{
 Formats a @racket[retraction].

 @examples[#:eval the-eval
  (format-retraction
    (make-retraction
     #f (make-clause
      #f (make-literal #f 'ancestor
          (list (make-constant #f 'joseph) 
                (make-constant #f 'jay)))
      (list))))]
}
              
@defproc[(format-query [q query?])
         string?]{
 Formats a @racket[query].

 @examples[#:eval the-eval
  (format-query
    (make-query
     #f (make-literal #f 'ancestor
         (list (make-constant #f 'joseph) 
               (make-constant #f 'jay)))))]
}

@defproc[(format-statement [s statement/c])
         string?]{
 Formats a @tech{statement}.

 @examples[#:eval the-eval
  (format-statement
    (make-query
     #f (make-literal #f 'ancestor
         (list (make-constant #f 'joseph) 
               (make-constant #f 'jay)))))]
}

@defproc[(format-program [p program/c])
         string?]{
 Formats a @tech{program}.

 @examples[#:eval the-eval
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
                (make-constant #f 'jay))))))]
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
  (format-literals
    (prove 
     (assume 
      (make-immutable-theory)
      (parse-clause (open-input-string "parent(joseph1,joseph2)")))
     (parse-literal 
      (open-input-string "parent(joseph1,joseph2)"))))
  (format-literals
    (prove
     (retract
      (assume
       (make-immutable-theory)
       (parse-clause 
        (open-input-string "parent(joseph1,joseph2)")))
      (parse-clause (open-input-string "parent(joseph1,joseph2)")))
     (parse-literal 
      (open-input-string "parent(joseph1,joseph2)"))))
  (format-literals
    (prove
     (assume
      (make-immutable-theory)
      (parse-clause (open-input-string "parent(joseph1,joseph2)")))
     (parse-literal (open-input-string "parent(A,B)"))))]
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

@defproc[(eval-top-level-statement [s statement/c])
         void]{
 Evaluates @racket[s] using @racket[(current-theory)] as the @tech{theory}, printing query answers if @racket[s] is a query.
           
 This will raise a syntax error if given an @racket[assertion] of a @racket[clause] that is not a @racket[safe-clause?].
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