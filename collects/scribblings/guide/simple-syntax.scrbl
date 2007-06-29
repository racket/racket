#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require[(lib "bnf.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "syntax-overview"]{Simple Definitions and Expressions}

A program module is written as

@schemeblock[
#, @BNF-seq[@litchar{#module} @nonterm{langname} @kleenestar{@nonterm{topform}}]
]

where a @nonterm{topform} is either a @nonterm{definition} or an
@nonterm{expr}. The @tech{REPL} also evaluates @nonterm{topform}s.

In syntax specifications, text with a gray background, such as
@litchar{#module}, represents literal text. Whitespace must appear
between separate such literals and nonterminals like @nonterm{id},
except that whitespace is not required before or after @litchar{(},
@litchar{)}, @litchar{[}, or @litchar{]}.  A comment, which starts
with @litchar{;} and runs until the end of the line, is treated the
same as whitespace.

Following the usual conventions, @kleenestar{} in a grammar means zero
or more repetitions of the preceding element, @kleeneplus{} means one
or more repetitions of the preceding element, and @BNF-group{} groups
a sequence as an element for repetition.

@define[val-defn-stx @BNF-seq[@litchar{(}@litchar{define} @nonterm{id} @nonterm{expr} @litchar{)}]]
@define[fun-defn-stx
         @BNF-seq[@litchar{(}@litchar{define} @litchar{(} @nonterm{id} @kleenestar{@nonterm{id}} @litchar{)}
                  @kleeneplus{@nonterm{expr}} @litchar{)}]]
@define[fun-defn2-stx
   @BNF-seq[@litchar{(}@litchar{define} @litchar{(} @nonterm{id} @kleenestar{@nonterm{id}} @litchar{)}
                  @kleenestar{@nonterm{definition}} @kleeneplus{@nonterm{expr}} @litchar{)}]]
@define[app-expr-stx @BNF-seq[@litchar{(} @nonterm{id} @kleenestar{@nonterm{expr}} @litchar{)}]]
@define[app2-expr-stx @BNF-seq[@litchar{(} @nonterm{expr} @kleenestar{@nonterm{expr}} @litchar{)}]]
@define[if-expr-stx @BNF-seq[@litchar{(} @litchar{if} @nonterm{expr} @nonterm{expr} @nonterm{expr} @litchar{)}]]

@define[lambda-expr-stx @BNF-seq[@litchar{(} @litchar{lambda} @litchar{(} @kleenestar{@nonterm{id}} @litchar{)}
                                              @kleeneplus{@nonterm{expr}} @litchar{)}]]
@define[lambda2-expr-stx
         @BNF-seq[@litchar{(} @litchar{lambda} @litchar{(} @kleenestar{@nonterm{id}} @litchar{)}
                  @kleenestar{@nonterm{definition}} @kleeneplus{@nonterm{expr}} @litchar{)}]]
@define[and-expr-stx @BNF-seq[@litchar{(} @litchar{and} @kleenestar{@nonterm{expr}} @litchar{)}]]
@define[or-expr-stx @BNF-seq[@litchar{(} @litchar{or} @kleenestar{@nonterm{expr}} @litchar{)}]]
@define[cond-expr-stx @BNF-seq[@litchar{(} @litchar{cond}
                                              @kleenestar{@BNF-group[@litchar{[} @nonterm{expr} @kleenestar{@nonterm{expr}} @litchar{]}]}
                                              @litchar{)}]]
@define[(make-let-expr-stx kw)
         @BNF-seq[@litchar{(} kw @litchar{(}
                      @kleenestar{@BNF-group[@litchar{[} @nonterm{id} @nonterm{expr} @litchar{]}]}
                      @litchar{)}
                   @kleeneplus{@nonterm{expr}} @litchar{)}]]
@define[let-expr-stx (make-let-expr-stx @litchar{let})]
@define[let*-expr-stx (make-let-expr-stx @litchar{let*})]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Definitions}

A definition of the form

@moreguide["guide:define"]{definitions}

@schemeblock[#, @val-defn-stx]

binds @nonterm{id} to the result of @nonterm{expr}, while

@schemeblock[#, @fun-defn-stx]

binds the first @nonterm{id} to a function (also called a
@defterm{procedure}) that takes arguments as named by the remaining
@nonterm{id}s. In the function case, the @nonterm{expr}s are the body
of the function. When the function is called, it returns the result of
the last @nonterm{expr}.

@defexamples[
(code:line (define five 5)            (code:comment #, @t{defines @scheme[five] to be @scheme[5]}))
(code:line (define (piece str)        (code:comment #, @t{defines @scheme[piece] as a function})
             (substring str 0 five))  (code:comment #, @t{of one argument}))
five
(piece "hello world")
]

Under the hood, a function definition is really the same as a
non-function definition, and a function name does not have to be
used in a function call. A function is just another kind of value,
though the printed form is necessarily less complete than the printed
form of a number or string.

@examples[
piece
substring
]

A function definition can include multiple expressions for the
function's body. In that case, only the value of the last expression
is returned when the function is called. The other expressions are
evaluated only for some side-effect, such as printing.

@defexamples[
(define (greet name)
  (printf "returning a greeting for ~a...\n" name)
  (string-append "hello " name))
(greet "universe")
]

Scheme programmers prefer to avoid assignment statements; it's
important, though, to understand that multiple expressions are allowed
in a definition body, because it explains why the following
@scheme[nogreet] function simply returns its argument:

@def+int[
(define (nogreet name)
  string-append "hello " name)
(nogreet "world")
]

Withing @scheme[nogreet], there are no parentheses around
@scheme[string-append "hello " name], so they are three separate
expressions instead of one function-call expression. The expressions
@scheme[string-append] and @scheme["hello "] are evaluated, but the
results are never used. Instead, the result of the function is just
the result of the expression @scheme[name].

@; ----------------------------------------------------------------------
@section[#:tag "indentation"]{An Aside on Indenting Code}

Line breaks and indentation are not significant for parsing Scheme
programs, but most Scheme programmer use a standard set of conventions
to make code more readable. For example, the body of a definition is
typically indented under the first line of the definition. Identifiers
are written immediately after an open parenthesis with no extra space,
and closing parentheses never go on their own line.

DrScheme automatically indents according to the standard style when
you type Enter in a program or @tech{REPL} expression. For example, if you
hit Enter after typing @litchar{(define (greet name)}, then DrScheme
automatically inserts two spaces for the next line.  If you change a
region of code, you can select it in DrScheme and hit Tab, and
DrScheme will re-indent the code (without inserting any line breaks).
Editors like Emacs offer a Scheme mode with similar indentation
support.

Re-indenting not only makes the code easier to read, it gives you
extra feedback that your parentheses are matched in the way that you
intended. For example, if you leave out a closing parenthesis after
the last argument to a function, automatic indentation starts the
next line under the first argument, instead of under the
@scheme[define] keyword:

@schemeblock[
(define (nogreet name
                 (string-append "hello " name)))
]

Furthermore, when an open parenthesis has no matching close
parenthesis in a program, both @exec{mzscheme} and DrScheme use the
source's indentation information to suggest where it might be missing.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Identifiers}

Scheme's syntax for identifiers is especially liberal. Excluding the
special characters

@moreguide["guide:binding"]{identifiers}

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["{"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{#} @litchar["|"] @litchar["\\"]
}

and except for the sequences of characters that make number constants,
almost any sequence of non-whitespace characters forms an
@nonterm{id}. For example @schemeid[substring] is an
identifier. Also, @schemeid[string-append] and @schemeid[a+b] are
identifiers, as opposed to arithmetic expressions. Here are several
more examples:

@schemeblock[
#, @schemeid[+]
#, @schemeid[Apple]
#, @schemeid[integer?]
#, @schemeid[call/cc]
#, @schemeid[call-with-composable-continuation]
#, @schemeid[x-1+3i]
]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Function Calls@aux-elem{ (Procedure Applications)}}

We have already seen many function calls---or @defterm{procedure
applications} in more traditional Scheme terminology. The syntax of a
function call is

@moreguide["guide:application"]{function calls}

@schemeblock[
#, app-expr-stx
]

where the number of @nonterm{expr}s determines the number of
arguments supplied to the function named by @nonterm{id}.

The @schememodname[big] language pre-defines many function
identifiers, such as @scheme[substring] and
@scheme[string-append]. More examples are below.

In example Scheme code throughout the documentation, uses of
pre-defined names are hyperlinked to the reference manual. So, you can
click on an identifier to get full details about its use.

@interaction[
(code:line (string-append "hello"  " "  "scheme") (code:comment #, @t{append strings}))
(code:line (substring "hello scheme" 6 12)        (code:comment #, @t{extract a substring}))
(code:line (string-length "scheme")               (code:comment #, @t{get a string's length}))
(code:line (string? "hello scheme")               (code:comment #, @t{recognize strings}))
(string? 1)
(code:line (sqrt 16)                              (code:comment #, @t{find a square root}))
(sqrt -16)
(code:line (+ 1 2)                                (code:comment #, @t{add numbers}))
(code:line (- 2 1)                                (code:comment #, @t{subtract numbers}))
(code:line (< 2 1)                                (code:comment #, @t{compare numbers}))
(>= 2 1)
(code:line (number? "hello scheme")               (code:comment #, @t{recognize numbers}))
(number? 1)
(code:line (equal? 1 "hello")                     (code:comment #, @t{compare anything}))
(equal? 1 1)
]

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Conditionals with @scheme[if], @scheme[and], @scheme[or], and @scheme[cond]}

The next simplest kind of expression is an @scheme[if] conditional:

@schemeblock[
#, if-expr-stx
]

@moreguide["guide:conditionals"]{conditionals}

The first @nonterm{expr} is always evaluted. If it produces a
non-@scheme[#f] value, then the second @nonterm{expr} is
evaluted for the result of the whole @scheme[if] expression, otherwise
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
(reply "hello scheme")
(reply "\u03BBx:(\u03BC\u03B1.\u03B1\u2192\u03B1).xx")
]

Complex conditionals can be formed by nesting @scheme[if]
expressions. For example, you could make the @scheme[reply] function
work when given non-strings:

@schemeblock[
(define (reply s)
  (if (string? s)
      (if (equal? "hello" (substring s 0 5)
          "hi!"
          "huh?"))
      "huh?"))
]

Instead of duplicating the @scheme["huh?"] case, this function is
better written as

@schemeblock[
(define (reply s)
  (if (if (string? s)
          (equal? "hello" (substring s 0 5)
          #f))
      "hi!"
      "huh?"))
]

but these kinds of nested @scheme[if]s are difficult to read.  Scheme
provides more readable shortcuts through the @scheme[and] and
@scheme[or] forms, which work with any number of expressions:

@moreguide["guide:and+or"]{@scheme[and] and @scheme[or]}

@schemeblock[
#, and-expr-stx
#, or-expr-stx
]

The @scheme[and] form short-circuits: it stops and returns @scheme[#f]
when and expression produces @scheme[#f], otherwise it keeps
going. The @scheme[or] form similarly short-circuits when it
encounters a true result.

@defexamples[
(define (reply s)
  (if (and (string? s)
           (>= (string-length s) 5)
           (equal? "hello" (substring s 0 5)))
      "hi!"
      "huh?"))
(reply "hello scheme")
(reply 17)
]

Another common pattern of nested @scheme[if]s involves a sequence of
tests, each with its own result:

@schemeblock[
(define (reply-more s)
  (if (equal? "hello" (substring s 0 5))
      "hi!"
      (if (equal? "goodbye" (substring s 0 7))
          "bye!"
          (if (equal? "?" (substring s (- (string-length s) 1)))
              "I don't know"
              "huh?"))))
]

The shorthand for a sequence of tests is the @scheme[cond] form:

@moreguide["guide:cond"]{@scheme[cond]}

@schemeblock[
#, cond-expr-stx
]

A @scheme[cond] form contains a sequence of clauses between square
brackets. In each clause, the first @nonterm{expr} is a test
expression. If it produces true, then the clause's remaining
@nonterm{expr}s are evaluated, and the last one in the clause provides
the answer for the entire @scheme[cond] expression; the rest of the
clauses are ignored. If the test @nonterm{expr} produces @scheme[#f],
then the clause's remaining @nonterm{expr}s are ignored, and
evaluation continues with the next clause. The last clause can use
@scheme[else] as a synonym for a @scheme[#t] test expression.

Using @scheme[cond], the @scheme[reply-more] function can be more
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
(reply-more "hello scheme")
(reply-more "goodbye cruel world")
(reply-more "what is your favorite color?")
(reply-more "mine is lime green")
]

The use of square brackets for @scheme[cond] clauses is a
convention. In Scheme, parentheses and square brackets are actually
interchangable, as long as @litchar{(} is matched with @litchar{)} and
@litchar{[} is matched with @litchar{]}. Using square brackets in a
few key places makes Scheme code even more readable.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Function Calls, Again}

In our earlier grammar of function calls, we oversimplified.  The
actual syntax of a function call allows an arbitrary
expression for the function, instead of just an @nonterm{id}:

@moreguide["guide:application"]{function calls}

@schemeblock[
#, app2-expr-stx
]

The first @nonterm{expr} is often an @nonterm{id}, such
as @scheme[string-append] or @scheme[+], but it can be anything that
evaluates to a function. For example, it can be a conditional
expression:

@def+int[
(define (double v)
  ((if (string? v) string-append +) v v))
(double "hello")
(double 5)
]

Syntactically, the first expression in a function call could
even be a number---but that leads to an error, since a number is not a
function.

@interaction[(1 2 3 4)]

When you accidentally omit a function name or when you use
parentheses around an expression, you'll most often get an ``expected
a procedure'' error like this one.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section{Anonymous Functions with @scheme[lambda]}

Programming in Scheme would be tedious if you had to name all of your
numbers. Instead of writing @scheme[(+ 1 2)], you'd have to write

@moreguide["guide:lambda"]{@scheme[lambda]}

@interaction[
(define a 1)
(define b 2)
(+ a b)
]

It turns out that having to name all your functions can be tedious,
too. For example, you might have a function @scheme[twice] that takes
a function and an argument. Using @scheme[twice] is convenient if you
already have a name for the function, such as @scheme[sqrt]:

@def+int[
(define (twice f v)
  (f (f v)))
(twice sqrt 16)
]

If you want to call a function that is not yet defined, you could
define it, and then pass it to @scheme[twice]:

@def+int[
(define (louder s)
  (string-append s "!"))
(twice louder "hello")
]

But if the call to @scheme[twice] is the only place where
@scheme[louder] is used, it's a shame to have to write a whole
definition. In Scheme, you can use a @scheme[lambda] expression to
produce a function directly. The @scheme[lambda] form is followed by
identifiers for the function's arguments, and then the function's
body expressions:

@schemeblock[
#, lambda-expr-stx
]

Evaluating a @scheme[lambda] form by itself produces a function:

@interaction[(lambda (s) (string-append s "!"))]

Using @scheme[lambda], the above call to @scheme[twice] can be
re-written as

@interaction[
(twice (lambda (s) (string-append s "!"))
       "hello")
(twice (lambda (s) (string-append s "?!"))
       "hello")
]

Another use of @scheme[lambda] is as a result for a function that
generates functions:

@def+int[
(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))
(twice (make-add-suffix "!") "hello")
(twice (make-add-suffix "?!") "hello")
(twice (make-add-suffix "...") "hello")
]

Scheme is a @defterm{lexically scoped} language, which means that
@scheme[s2] in the function returned by @scheme[make-add-suffix]
always refers to the argument for the call that created the
function. In other words, the @scheme[lambda]-generated function
``remembers'' the right @scheme[s2]:

@interaction[
(define louder (make-add-suffix "!"))
(define less-sure (make-add-suffix "?"))
(twice less-sure "really")
(twice louder "really")
]

We have so far referred to definitions of the form @scheme[(define #,
@nonterm{id} #, @nonterm{expr})] as ``non-function
definitions.'' This characterization is misleading, because the
@nonterm{expr} could be a @scheme[lambda] form, in which case
the definition is equivalent to using the ``function'' definition
form. For example, the following two definitions of @scheme[louder]
are equivalent:

@defs+int[
[(define (louder s)
   (string-append s "!"))
 code:blank
 (define louder
   (lambda (s)
     (string-append s "!")))]
louder
]

Note that the expression for @scheme[louder] in the second case is an
``anonymous'' function written with @scheme[lambda], but, if
possible, the compiler infers a name, anyway, to make printing and
error reporting as informative as possible.

@;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@section[#:tag "local-binding-intro"]{Local Binding with
         @scheme[define], @scheme[let], and @scheme[let*]}

It's time to retract another simplification in our grammar of
Scheme. In the body of a function, definitions can appear before the
body expressions:

@moreguide["guide:intdefs"]{local (internal) definitions}

@schemeblock[
#, fun-defn2-stx
#, lambda2-expr-stx
]

Definitions at the start of a function body are local to the
function body.

@defexamples[
(define (converse s)
  (define (starts? s2) (code:comment #, @t{local to @scheme[converse]})
    (define len2 (string-length s2))  (code:comment #, @t{local to @scheme[starts?]})
    (and (>= (string-length s) len2)
         (equal? s2 (substring s 0 len2))))
  (cond
   [(starts? "hello") "hi!"]
   [(starts? "goodbye") "bye!"]
   [else "huh?"]))
(converse "hello!")
(converse "urp")
(code:line starts? (code:comment #, @t{outside of @scheme[converse], so...}))
]

Another way to create local bindings is the @scheme[let] form. An
advantage of @scheme[let] is that it can be used in any expression
position. Also, @scheme[let] binds many identifiers at once, instead
of requiring a separate @scheme[define] for each identifier.

@moreguide["guide:intdefs"]{@scheme[let] and @scheme[let*]}

@schemeblock[
#, let-expr-stx
]

Each binding clause is an @nonterm{id} and a
@nonterm{expr} surrounded by square brackets, and the
expressions after the clauses are the body of the @scheme[let]. In
each clause, the @nonterm{id} is bound to the result of the
@nonterm{expr} for use in the body.

@interaction[
(let ([x 1]
      [y 2])
  (format "adding ~s and ~s produces ~s" x y (+ x y)))
]

The bindings of a @scheme[let] form are available only in the body of
the @scheme[let], so the binding clauses cannot refer to each
other. The @scheme[let*] form, in contrast, allows later clauses to
use earlier bindings:

@interaction[
(let* ([x 1]
       [y 2]
       [z (+ x y)])
  (format "adding ~s and ~s produces ~s" x y z))
]
