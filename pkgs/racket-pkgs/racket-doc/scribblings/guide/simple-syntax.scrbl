#lang scribble/doc
@(require scribble/manual scribble/eval scribble/bnf "guide-utils.rkt")

@(define ex-eval (make-base-eval))

@title[#:tag "syntax-overview"]{Simple Definitions and Expressions}

A program module is written as

@racketblock[
@#,BNF-seq[@litchar{#lang} @nonterm{langname} @kleenestar{@nonterm{topform}}]
]

where a @nonterm{topform} is either a @nonterm{definition} or an
@nonterm{expr}. The @tech{REPL} also evaluates @nonterm{topform}s.

In syntax specifications, text with a gray background, such as
@litchar{#lang}, represents literal text. Whitespace must appear
between such literals and nonterminals like @nonterm{id},
except that whitespace is not required before or after @litchar{(},
@litchar{)}, @litchar{[}, or @litchar{]}.  A @index['("comments")]{comment}, which starts
with @litchar{;} and runs until the end of the line, is treated the
same as whitespace.

@refdetails["parse-comment"]{different forms of comments}

Following the usual conventions, @kleenestar{} in a grammar means zero
or more repetitions of the preceding element, @kleeneplus{} means one
or more repetitions of the preceding element, and @BNF-group{} groups
a sequence as an element for repetition.

@(define val-defn-stx
   @BNF-seq[@litchar{(}@litchar{define} @nonterm{id} @nonterm{expr} @litchar{)}])
@(define fun-defn-stx
   @BNF-seq[@litchar{(}@litchar{define} @litchar{(} @nonterm{id} @kleenestar{@nonterm{id}} @litchar{)}
                  @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define fun-defn2-stx
   @BNF-seq[@litchar{(}@litchar{define} @litchar{(} @nonterm{id} @kleenestar{@nonterm{id}} @litchar{)}
            @kleenestar{@nonterm{definition}} @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define app-expr-stx @BNF-seq[@litchar{(} @nonterm{id} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define app2-expr-stx @BNF-seq[@litchar{(} @nonterm{expr} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define if-expr-stx @BNF-seq[@litchar{(} @litchar{if} @nonterm{expr} @nonterm{expr} @nonterm{expr} @litchar{)}])

@(define lambda-expr-stx @BNF-seq[@litchar{(} @litchar{lambda} @litchar{(} @kleenestar{@nonterm{id}} @litchar{)}
                                              @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define lambda2-expr-stx
   @BNF-seq[@litchar{(} @litchar{lambda} @litchar{(} @kleenestar{@nonterm{id}} @litchar{)}
            @kleenestar{@nonterm{definition}} @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define and-expr-stx @BNF-seq[@litchar{(} @litchar{and} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define or-expr-stx @BNF-seq[@litchar{(} @litchar{or} @kleenestar{@nonterm{expr}} @litchar{)}])
@(define cond-expr-stx @BNF-seq[@litchar{(} @litchar{cond}
                                @kleenestar{@BNF-group[@litchar{[} @nonterm{expr} @kleenestar{@nonterm{expr}} @litchar{]}]}
                                @litchar{)}])
@(define (make-let-expr-stx kw)
   @BNF-seq[@litchar{(} kw @litchar{(}
            @kleenestar{@BNF-group[@litchar{[} @nonterm{id} @nonterm{expr} @litchar{]}]}
            @litchar{)}
            @kleeneplus{@nonterm{expr}} @litchar{)}])
@(define let-expr-stx (make-let-expr-stx @litchar{let}))
@(define let*-expr-stx (make-let-expr-stx @litchar{let*}))

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Definitions}

A definition of the form

@moreguide["define"]{definitions}

@racketblock[@#,val-defn-stx]

binds @nonterm{id} to the result of @nonterm{expr}, while

@racketblock[@#,fun-defn-stx]

binds the first @nonterm{id} to a function (also called a
@defterm{procedure}) that takes arguments as named by the remaining
@nonterm{id}s. In the function case, the @nonterm{expr}s are the body
of the function. When the function is called, it returns the result of
the last @nonterm{expr}.

@defexamples[
#:eval ex-eval
(code:line (define pie 3)             (code:comment @#,t{defines @racket[pie] to be @racket[3]}))
(code:line (define (piece str)        (code:comment @#,t{defines @racket[piece] as a function})
             (substring str 0 pie))   (code:comment @#,t{ of one argument}))
pie
(piece "key lime")
]

Under the hood, a function definition is really the same as a
non-function definition, and a function name does not have to be
used in a function call. A function is just another kind of value,
though the printed form is necessarily less complete than the printed
form of a number or string.

@examples[
#:eval ex-eval
piece
substring
]

A function definition can include multiple expressions for the
function's body. In that case, only the value of the last expression
is returned when the function is called. The other expressions are
evaluated only for some side-effect, such as printing.

@defexamples[
#:eval ex-eval
(define (bake flavor)
  (printf "pre-heating oven...\n")
  (string-append flavor " pie"))
(bake "apple")
]

Racket programmers prefer to avoid side-effects, so a definition usually
has just one expression in its body. It's
important, though, to understand that multiple expressions are allowed
in a definition body, because it explains why the following
@racket[nobake] function fails to include its argument in its result:

@def+int[
#:eval ex-eval
(define (nobake flavor)
  string-append flavor "jello")
(nobake "green")
]

Within @racket[nobake], there are no parentheses around
@racket[string-append flavor "jello"], so they are three separate
expressions instead of one function-call expression. The expressions
@racket[string-append] and @racket[flavor] are evaluated, but the
results are never used. Instead, the result of the function is just
the result of the final expression, @racket["jello"].

@; ----------------------------------------------------------------------
@section[#:tag "indentation"]{An Aside on Indenting Code}

Line breaks and indentation are not significant for parsing Racket
programs, but most Racket programmers use a standard set of conventions
to make code more readable. For example, the body of a definition is
typically indented under the first line of the definition. Identifiers
are written immediately after an open parenthesis with no extra space,
and closing parentheses never go on their own line.

DrRacket automatically indents according to the standard style when
you type Enter in a program or @tech{REPL} expression. For example, if you
hit Enter after typing @litchar{(define (greet name)}, then DrRacket
automatically inserts two spaces for the next line.  If you change a
region of code, you can select it in DrRacket and hit Tab, and
DrRacket will re-indent the code (without inserting any line breaks).
Editors like Emacs offer a Racket or Scheme mode with similar indentation
support.

Re-indenting not only makes the code easier to read, it gives you
extra feedback that your parentheses match in the way that you
intended. For example, if you leave out a closing parenthesis after
the last argument to a function, automatic indentation starts the
next line under the first argument, instead of under the
@racket[define] keyword:

@racketblock[
(define (halfbake flavor
                  (string-append flavor " creme brulee")))
]

In this case, indentation helps highlight the mistake. In other cases,
where the indentation may be normal while an open parenthesis has no
matching close parenthesis, both @exec{racket} and DrRacket use the
source's indentation to suggest where a parenthesis might be missing.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Identifiers}

Racket's syntax for identifiers is especially liberal. Excluding the
special characters

@moreguide["binding"]{identifiers}

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["{"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{#} @litchar{|} @litchar{\}
}

and except for the sequences of characters that make number constants,
almost any sequence of non-whitespace characters forms an
@nonterm{id}. For example @racketid[substring] is an
identifier. Also, @racketid[string-append] and @racketid[a+b] are
identifiers, as opposed to arithmetic expressions. Here are several
more examples:

@racketblock[
@#,racketid[+]
@#,racketid[Hfuhruhurr]
@#,racketid[integer?]
@#,racketid[pass/fail]
@#,racketid[john-jacob-jingleheimer-schmidt]
@#,racketid[a-b-c+1-2-3]
]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Function Calls@aux-elem{ (Procedure Applications)}}

We have already seen many function calls, which are called
@defterm{procedure applications} in more traditional
terminology. The syntax of a function call is

@moreguide["application"]{function calls}

@racketblock[
#,app-expr-stx
]

where the number of @nonterm{expr}s determines the number of
arguments supplied to the function named by @nonterm{id}.

The @racketmodname[racket] language pre-defines many function
identifiers, such as @racket[substring] and
@racket[string-append]. More examples are below.

In example Racket code throughout the documentation, uses of
pre-defined names are hyperlinked to the reference manual. So, you can
click on an identifier to get full details about its use.

@interaction[
(code:line (string-append "rope" "twine" "yarn")  (code:comment @#,t{append strings}))
(code:line (substring "corduroys" 0 4)            (code:comment @#,t{extract a substring}))
(code:line (string-length "shoelace")             (code:comment @#,t{get a string's length}))
(code:line (string? "Ceci n'est pas une string.") (code:comment @#,t{recognize strings}))
(string? 1)
(code:line (sqrt 16)                              (code:comment @#,t{find a square root}))
(sqrt -16)
(code:line (+ 1 2)                                (code:comment @#,t{add numbers}))
(code:line (- 2 1)                                (code:comment @#,t{subtract numbers}))
(code:line (< 2 1)                                (code:comment @#,t{compare numbers}))
(>= 2 1)
(code:line (number? "c'est une number")           (code:comment @#,t{recognize numbers}))
(number? 1)
(code:line (equal? 6 "half dozen")                (code:comment @#,t{compare anything}))
(equal? 6 6)
(equal? "half dozen" "half dozen")
]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Conditionals with @racket[if], @racket[and], @racket[or], and @racket[cond]}

The next simplest kind of expression is an @racket[if] conditional:

@racketblock[
#,if-expr-stx
]

@moreguide["conditionals"]{conditionals}

The first @nonterm{expr} is always evaluated. If it produces a
non-@racket[#f] value, then the second @nonterm{expr} is
evaluated for the result of the whole @racket[if] expression, otherwise
the third @nonterm{expr} is evaluated for the result.

@examples[
(if (> 2 3)
    "bigger"
    "smaller")
]

@def+int[
(define (reply s)
  (if (equal? "hello" (substring s 0 5))
      "hi!"
      "huh?"))
(reply "hello racket")
(reply "\u03BBx:(\u03BC\u03B1.\u03B1\u2192\u03B1).xx")
]

Complex conditionals can be formed by nesting @racket[if]
expressions. For example, you could make the @racket[reply] function
work when given non-strings:

@racketblock[
(define (reply s)
  (if (string? s)
      (if (equal? "hello" (substring s 0 5))
          "hi!"
          "huh?")
      "huh?"))
]

Instead of duplicating the @racket["huh?"] case, this function is
better written as

@racketblock[
(define (reply s)
  (if (if (string? s)
          (equal? "hello" (substring s 0 5))
          #f)
      "hi!"
      "huh?"))
]

but these kinds of nested @racket[if]s are difficult to read.  Racket
provides more readable shortcuts through the @racket[and] and
@racket[or] forms, which work with any number of expressions:

@moreguide["and+or"]{@racket[and] and @racket[or]}

@racketblock[
#,and-expr-stx
#,or-expr-stx
]

The @racket[and] form short-circuits: it stops and returns @racket[#f]
when an expression produces @racket[#f], otherwise it keeps
going. The @racket[or] form similarly short-circuits when it
encounters a true result.

@defexamples[
(define (reply s)
  (if (and (string? s)
           (>= (string-length s) 5)
           (equal? "hello" (substring s 0 5)))
      "hi!"
      "huh?"))
(reply "hello racket")
(reply 17)
]

Another common pattern of nested @racket[if]s involves a sequence of
tests, each with its own result:

@racketblock[
(define (reply-more s)
  (if (equal? "hello" (substring s 0 5))
      "hi!"
      (if (equal? "goodbye" (substring s 0 7))
          "bye!"
          (if (equal? "?" (substring s (- (string-length s) 1)))
              "I don't know"
              "huh?"))))
]

The shorthand for a sequence of tests is the @racket[cond] form:

@moreguide["cond"]{@racket[cond]}

@racketblock[
#,cond-expr-stx
]

A @racket[cond] form contains a sequence of clauses between square
brackets. In each clause, the first @nonterm{expr} is a test
expression. If it produces true, then the clause's remaining
@nonterm{expr}s are evaluated, and the last one in the clause provides
the answer for the entire @racket[cond] expression; the rest of the
clauses are ignored. If the test @nonterm{expr} produces @racket[#f],
then the clause's remaining @nonterm{expr}s are ignored, and
evaluation continues with the next clause. The last clause can use
@racket[else] as a synonym for a @racket[#t] test expression.

Using @racket[cond], the @racket[reply-more] function can be more
clearly written as follows:

@def+int[
(define (reply-more s)
  (cond
   [(equal? "hello" (substring s 0 5))
    "hi!"]
   [(equal? "goodbye" (substring s 0 7))
    "bye!"]
   [(equal? "?" (substring s (- (string-length s) 1)))
    "I don't know"]
   [else "huh?"]))
(reply-more "hello racket")
(reply-more "goodbye cruel world")
(reply-more "what is your favorite color?")
(reply-more "mine is lime green")
]

The use of square brackets for @racket[cond] clauses is a
convention. In Racket, parentheses and square brackets are actually
interchangeable, as long as @litchar{(} is matched with @litchar{)} and
@litchar{[} is matched with @litchar{]}. Using square brackets in a
few key places makes Racket code even more readable.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Function Calls, Again}

In our earlier grammar of function calls, we oversimplified.  The
actual syntax of a function call allows an arbitrary
expression for the function, instead of just an @nonterm{id}:

@moreguide["application"]{function calls}

@racketblock[
#,app2-expr-stx
]

The first @nonterm{expr} is often an @nonterm{id}, such
as @racket[string-append] or @racket[+], but it can be anything that
evaluates to a function. For example, it can be a conditional
expression:

@def+int[
(define (double v)
  ((if (string? v) string-append +) v v))
(double "mnah")
(double 5)
]

Syntactically, the first expression in a function call could
even be a number---but that leads to an error, since a number is not a
function.

@interaction[(1 2 3 4)]

When you accidentally omit a function name or when you use
extra parentheses around an expression, you'll most often get an ``expected
a procedure'' error like this one.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Anonymous Functions with @racket[lambda]}

Programming in Racket would be tedious if you had to name all of your
numbers. Instead of writing @racket[(+ 1 2)], you'd have to write

@moreguide["lambda"]{@racket[lambda]}

@interaction[
(define a 1)
(define b 2)
(+ a b)
]

It turns out that having to name all your functions can be tedious,
too. For example, you might have a function @racket[twice] that takes
a function and an argument. Using @racket[twice] is convenient if you
already have a name for the function, such as @racket[sqrt]:

@def+int[
#:eval ex-eval
(define (twice f v)
  (f (f v)))
(twice sqrt 16)
]

If you want to call a function that is not yet defined, you could
define it, and then pass it to @racket[twice]:

@def+int[
#:eval ex-eval
(define (louder s)
  (string-append s "!"))
(twice louder "hello")
]

But if the call to @racket[twice] is the only place where
@racket[louder] is used, it's a shame to have to write a whole
definition. In Racket, you can use a @racket[lambda] expression to
produce a function directly. The @racket[lambda] form is followed by
identifiers for the function's arguments, and then the function's
body expressions:

@racketblock[
#,lambda-expr-stx
]

Evaluating a @racket[lambda] form by itself produces a function:

@interaction[(lambda (s) (string-append s "!"))]

Using @racket[lambda], the above call to @racket[twice] can be
re-written as

@interaction[
#:eval ex-eval
(twice (lambda (s) (string-append s "!"))
       "hello")
(twice (lambda (s) (string-append s "?!"))
       "hello")
]

Another use of @racket[lambda] is as a result for a function that
generates functions:

@def+int[
#:eval ex-eval
(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))
(twice (make-add-suffix "!") "hello")
(twice (make-add-suffix "?!") "hello")
(twice (make-add-suffix "...") "hello")
]

Racket is a @defterm{lexically scoped} language, which means that
@racket[s2] in the function returned by @racket[make-add-suffix]
always refers to the argument for the call that created the
function. In other words, the @racket[lambda]-generated function
``remembers'' the right @racket[s2]:

@interaction[
#:eval ex-eval
(define louder (make-add-suffix "!"))
(define less-sure (make-add-suffix "?"))
(twice less-sure "really")
(twice louder "really")
]

We have so far referred to definitions of the form @racket[(define
@#,nonterm{id} @#,nonterm{expr})] as ``non-function
definitions.'' This characterization is misleading, because the
@nonterm{expr} could be a @racket[lambda] form, in which case
the definition is equivalent to using the ``function'' definition
form. For example, the following two definitions of @racket[louder]
are equivalent:

@defs+int[
#:eval ex-eval
[(define (louder s)
   (string-append s "!"))
 code:blank
 (define louder
   (lambda (s)
     (string-append s "!")))]
louder
]

Note that the expression for @racket[louder] in the second case is an
``anonymous'' function written with @racket[lambda], but, if
possible, the compiler infers a name, anyway, to make printing and
error reporting as informative as possible.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section[#:tag "local-binding-intro"]{Local Binding with
         @racket[define], @racket[let], and @racket[let*]}

It's time to retract another simplification in our grammar of
Racket. In the body of a function, definitions can appear before the
body expressions:

@moreguide["intdefs"]{local (internal) definitions}

@racketblock[
#,fun-defn2-stx
#,lambda2-expr-stx
]

Definitions at the start of a function body are local to the
function body.

@defexamples[
(define (converse s)
  (define (starts? s2) (code:comment @#,t{local to @racket[converse]})
    (define len2 (string-length s2))  (code:comment @#,t{local to @racket[starts?]})
    (and (>= (string-length s) len2)
         (equal? s2 (substring s 0 len2))))
  (cond
   [(starts? "hello") "hi!"]
   [(starts? "goodbye") "bye!"]
   [else "huh?"]))
(converse "hello!")
(converse "urp")
(eval:alts (code:line starts? (code:comment @#,t{outside of @racket[converse], so...}))
           (parameterize ([current-namespace (make-base-namespace)]) (eval 'starts?)))
]

Another way to create local bindings is the @racket[let] form. An
advantage of @racket[let] is that it can be used in any expression
position. Also, @racket[let] binds many identifiers at once, instead
of requiring a separate @racket[define] for each identifier.

@moreguide["intdefs"]{@racket[let] and @racket[let*]}

@racketblock[
#,let-expr-stx
]

Each binding clause is an @nonterm{id} and an
@nonterm{expr} surrounded by square brackets, and the
expressions after the clauses are the body of the @racket[let]. In
each clause, the @nonterm{id} is bound to the result of the
@nonterm{expr} for use in the body.

@interaction[
(let ([x (random 4)]
      [o (random 4)])
  (cond
   [(> x o) "X wins"]
   [(> o x) "O wins"]
   [else "cat's game"]))
]

The bindings of a @racket[let] form are available only in the body of
the @racket[let], so the binding clauses cannot refer to each
other. The @racket[let*] form, in contrast, allows later clauses to
use earlier bindings:

@interaction[
(let* ([x (random 4)]
       [o (random 4)]
       [diff (number->string (abs (- x o)))])
  (cond
   [(> x o) (string-append "X wins by " diff)]
   [(> o x) (string-append "O wins by " diff)]
   [else "cat's game"]))
]

@; ----------------------------------------------------------------------

@close-eval[ex-eval]
