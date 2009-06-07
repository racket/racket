#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "pattern-macros"]{Pattern-Based Macros}

A @deftech{pattern-based macro} replaces any code that matches a
pattern to an expansion that uses parts of the original syntax that
match parts of the pattern.

@; ----------------------------------------

@section{@scheme[define-syntax-rule]}

The simplest way to create a macro is to use
@scheme[define-syntax-rule]:

@specform[(define-syntax-rule pattern template)]

As a running example, consider the @scheme[swap] macro, which swaps
the values stored in two variables. It can be implemented using
@scheme[define-syntax-rule] as follows:

@margin-note{The macro is ``un-Schemely'' in the sense that it
involves side effects on variables---but the point of macros is to let
you add syntactic forms that some other language designer might not
approve.}

@schemeblock[
(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))
]

The @scheme[define-syntax-rule] form binds a macro that matches a
single pattern. The pattern must always start with an open parenthesis
followed by an identifier, which is @scheme[swap] in this case. After
the initial identifier, other identifiers are @deftech{macro pattern
variables} that can match anything in a use of the macro. Thus, this
macro matches the for @scheme[(swap _form_1 _form_2)] for any
@scheme[_form_1] and @scheme[_form_2].

@margin-note{Macro pattern variables similar to pattern variables for
 @scheme[match]. See @secref["match"].}

After the pattern in @scheme[define-syntax-rule] is the
@deftech{template}. The template is used in place of a form that
matches the pattern, except that each instance of a pattern variable
in the template is replaced with the part of the macro use the pattern
variable matched. For example, in

@schemeblock[(swap first last)]

the pattern variable @scheme[x] matches @scheme[first] and @scheme[y]
matches @scheme[last], so that the expansion is

@schemeblock[
  (let ([tmp first])
    (set! first last)
    (set! last tmp))
]

@; ----------------------------------------

@section{Lexical Scope}

Suppose that we use the @scheme[swap] macro to swap variables named
@scheme[tmp] and @scheme[other]:

@schemeblock[
(let ([tmp 5]
      [other 6])
  (swap tmp other)
  (list tmp other))
]

The result of the above expression should be @schemeresult[(6 5)]. The
naive expansion of this use of @scheme[swap], however, is

@schemeblock[
(let ([tmp 5]
      [other 6])
  (let ([tmp tmp])
    (set! tmp other)
    (set! other tmp))
  (list tmp other))
]

whose result is @schemeresult[(5 6)]. The problem is that the naive
expansion confuses the @scheme[tmp] in the context where @scheme[swap]
is used with the @scheme[tmp] that is in the macro template.

Scheme doesn't produce the naive expansion for the above use of
@scheme[swap]. Instead, it produces

@schemeblock[
(let ([tmp 5]
      [other 6])
  (let ([tmp_1 tmp])
    (set! tmp other)
    (set! other tmp_1))
  (list tmp other))
]

with the correct result in @schemeresult[(6 5)]. Similarly, in the
example

@schemeblock[
(let ([set! 5]
      [other 6])
  (swap set! other)
  (list set! other))
]

the expansion is 

@schemeblock[
(let ([set!_1 5]
      [other 6])
  (let ([tmp_1 tmp])
    (set! set!_1 other)
    (set! other tmp_1))
  (list set!_1 other))
]

so that the local @scheme[set!] binding doesn't interfere with the
assignments introduced by the macro template.

In other words, Scheme's pattern-based macros automatically maintain
lexical scope, so macro implementors can reason about variable
reference in macros and macro uses in the same way as for functions
and function calls.

@; ----------------------------------------

@section{@scheme[define-syntax] and @scheme[syntax-rules]}

The @scheme[define-syntax-rule] form binds a macro that matches a
single pattern, but Scheme's macro system supports transformers that
match multiple patterns starting with the same identifier. To write
such macros, the programmer much use the more general
@scheme[define-syntax] form along with the @scheme[syntax-rules]
transformer form:

@specform[#:literals (syntax-rules)
          (define-syntax id
            (syntax-rules (literal-id ...)
              [pattern template]
              ...))]

@margin-note{The @scheme[define-syntax-rule] form is itself a macro
 that expands into @scheme[define-syntax] with a @scheme[syntax-rules]
 form that contains only one pattern and template.}

For example, suppose we would like a @scheme[rotate] macro that
generalizes @scheme[swap] to work on either two or three identifiers,
so that

@schemeblock[
(let ([red 1] [green 2] [blue 3])
  (rotate red green)      (code:comment @#,t{swaps})
  (rotate red green blue) (code:comment @#,t{rotates left})
  (list red green blue))
]

produces @schemeresult[(1 3 2)]. We can implement @scheme[rotate]
using @scheme[syntax-rules]:

@schemeblock[
(define-syntax rotate
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c) (begin
                     (swap a b)
                     (swap b c))]))
]

The expression @scheme[(rotate red green)] matches the first pattern
in the @scheme[syntax-rules] form, so it expands to @scheme[(swap red
green)]. The expression @scheme[(rotate a b c)] matches the second
pattern, so it expands to @scheme[(begin (swap red green) (swap green
blue))].

@; ----------------------------------------

@section{Matching Sequences}

A better @scheme[rotate] macro would allow any number of identifiers,
instead of just two or three. To match a use of @scheme[rotate] with
any number of identifiers, we need a pattern form that has something
like a Kleene star. In a Scheme macro pattern, a star is written as
@scheme[...].

To implement @scheme[rotate] with @scheme[...], we need a base case to
handle a single identifier, and an inductive case to handle more than
one identifier:

@schemeblock[
(define-syntax rotate
  (syntax-rules ()
    [(rotate a) (void)]
    [(rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...))]))
]

When a pattern variable like @scheme[c] is followed by @scheme[...] in
a pattern, then it must be followed by @scheme[...] in a template,
too. The pattern variable effectively matches a sequence of zero or
more forms, and it is replaced in the template by the same sequence.

Both versions of @scheme[rotate] so far are a bit inefficient, since
pairwise swapping keeps moving the value from the first variable into
every variable in the sequence until it arrives at the last one. A
more efficient @scheme[rotate] would move the first value directly to
the last variable. We can use @scheme[...] patterns to implement the
more efficient variant using a helper macro:

@schemeblock[
(define-syntax rotate
  (syntax-rules ()
    [(rotate a c ...)
     (shift-to (c ... a) (a c ...))]))

(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (from0 from ...) (to0 to ...))
     (let ([tmp from0])
       (set! to from) ...
       (set! to0 tmp))]))
]

In the @scheme[shift-to] macro, @scheme[...] in the template follows
@scheme[(set! to from)], which causes the @scheme[(set! to from)]
expression to be duplicated as many times as necessary to use each
identifier matched in the @scheme[to] and @scheme[from]
sequences. (The number of @scheme[to] and @scheme[from] matches must
be the same, otherwise the macro expansion fails with an error.)

@; ----------------------------------------

@section{Identifier Macros}

Given our macro definitions, the @scheme[swap] or @scheme[rotate]
identifiers must be used after an open parenthesis, otherwise a syntax
error is reported:

@interaction-eval[(define-syntax swap (syntax-rules ()))]

@interaction[(+ swap 3)]

An @deftech{identifier macro} works in any expression. For example, we
can define @scheme[clock] as an identifier macro that expands to
@scheme[(get-clock)], so @scheme[(+ clock 3)] would expand to
@scheme[(+ (get-clock) 3)]. An identifier macro also cooperates with
@scheme[set!], and we can define @scheme[clock] so that @scheme[(set!
clock 3)] expands to @scheme[(put-clock! 3)].

The @scheme[syntax-id-rules] form is like @scheme[syntax-rules], but
it creates a transformer that acts as an identifier macro:

@specform[#:literals (syntax-id-rules)
          (define-syntax id
            (syntax-id-rules (literal-id ...)
              [pattern template]
              ...))]

Unlike a @scheme[syntax-rules] form, the @scheme[_pattern]s are not
required to start with an open parenthesis. Also, @scheme[set!] is
typically used as a literal to match a use of @scheme[set!] in the
pattern (as opposed to being a pattern variable.

@schemeblock[
(define-syntax clock
  (syntax-id-rules (set!)
    [(set! clock e) (put-clock! e)]
    [(clock a ...) ((get-clock) a ...)]
    [clock (get-clock)]))
]

The @scheme[(clock a ...)] pattern is needed because, when an
identifier macro is used after an open parenthesis, the macro
transformer is given the whole form, like with a non-identifier macro.
Put another way, the @scheme[syntax-rules] form is essentially a
special case of the @scheme[syntax-id-rules] form with errors in the
@scheme[set!] and lone-identifier cases.

@; ----------------------------------------

@section{Macro-Generating Macros}

Suppose that we have many identifier like @scheme[clock] that we'd
like to redirect to accessor and mutator functions like
@scheme[get-clock] and @scheme[put-clock!]. We'd like to be able to
just write

@schemeblock[
(define-get/put-id clock get-clock put-clock!)
]

Naturally, we can implement @scheme[define-get/put-id] as a macro:

@schemeblock[
(define-syntax-rule (define-get/put-id id get put!)
  (define-syntax clock
    (syntax-id-rules (set!)
      [(set! clock e) (put-clock! e)]
      [(clock a (... ...)) ((get-clock) a (... ...))]
      [clock (get-clock)])))
]

The @scheme[define-get/put-id] macro is a @deftech{macro-generating
macro}.  The only non-obvious part of its definition is the
@scheme[(... ...)], which ``quotes'' @scheme[...] so that it takes its
usual role in the generated macro, instead of the generating macro.

@; ----------------------------------------

@section[#:tag "pattern-macro-example"]{Extended Example: Call-by-Reference Functions}

We can use pattern-matching macros to add a form to Scheme
for defining first-order @deftech{call-by-reference} functions. When a
call-by-reference function body mutates its formal argument, the
mutation applies to variables that are supplied as actual arguments in
a call to the function.

For example, if @scheme[define-cbr] is like @scheme[define] except
that it defines a call-by-reference function, then

@schemeblock[
(define-cbr (f a b)
  (swap a b))

(let ([x 1] [y 2])
  (f x y)
  (list x y))
]

produces @schemeresult[(2 1)]. 

We will implement call-by-reference functions by having function calls
supply accessor and mutators for the arguments, instead of supplying
argument values directly. In particular, for the function @scheme[f]
above, we'll generate

@schemeblock[
(define (do-f get-a get-b put-a! put-b!)
  (define-get/put-id a get-a put-a!)
  (define-get/put-id b get-b put-b!)
  (swap a b))
]

and redirect a function call @scheme[(f x y)] to

@schemeblock[
(do-f (lambda () x) 
      (lambda () y)
      (lambda (v) (set! x v)) 
      (lambda (v) (set! y v)))
]

Clearly, then @scheme[define-cbr] is a macro-generating macro, which
binds @scheme[f] to a macro that expands to a call of @scheme[do-f].
That is, @scheme[(define-cbr (f a b) (swap ab))] needs to generate the
definition

@schemeblock[
(define-syntax f
  (syntax-rules ()
    [(id actual ...)
     (do-f (lambda () actual) 
           ...
           (lambda (v) 
             (set! actual v))
           ...)]))
]

At the same time, @scheme[define-cbr] needs to define @scheme[do-f]
using the body of @scheme[f], this second part is slightly more
complex, so we defer most it to a @scheme[define-for-cbr] helper
module, which lets us write @scheme[define-cbr] easily enough:


@schemeblock[
(define-syntax-rule (define-cbr (id arg ...) body)
  (begin
    (define-syntax id
      (syntax-rules ()
        [(id actual (... ...))
         (do-f (lambda () actual) 
               (... ...)
               (lambda (v) 
                 (set! actual v))
               (... ...))]))
    (define-for-cbr do-f (arg ...)
      () (code:comment @#,t{explained below...})
      body)))
]

Our remaining task is to define @scheme[define-for-cbr] so that it
converts

@schemeblock[
(define-for-cbr do-f (a b) () (swap a b))
]

to the function definition @scheme[do-f] above. Most of the work is
generating a @scheme[define-get/put-id] declaration for each argument,
@scheme[a] ad @scheme[b], and putting them before the body. Normally,
that's an easy task for @scheme[...] in a pattern and template, but
this time there's a catch: we need to generate the names
@scheme[get-a] and @scheme[put-a!] as well as @scheme[get-b] and
@scheme[put-b!], and the pattern language provides no way to
synthesize identifiers based on existing identifiers.

As it turns out, lexical scope gives us a way around this problem. The
trick is to iterate expansions of @scheme[define-for-cbr] once for
each argument in the function, and that's why @scheme[define-cbr]
starts with an apparently useless @scheme[()] after the argument
list. We need to keep track of all the arguments seen so far and the
@scheme[get] and @scheme[put] names generated for each, in addition to
the arguments left to process. After we've processed all the
identifiers, then we have all the names we need.

Here is the definition of @scheme[define-for-cbr]:

@schemeblock[
(define-syntax define-for-cbr
  (syntax-rules ()
    [(define-for-cbr do-f (id0 id ...)
       (gens ...) body)
     (define-for-cbr do-f (id ...) 
       (gens ... (id0 get put)) body)]
    [(define-for-cbr do-f ()
       ((id get put) ...) body)
     (define (do-f get ... put ...)
       (define-get/put-id id get put) ...
       body)]))
]

Step-by-step, expansion proceeds as follows:

@schemeblock[
(define-for-cbr do-f (a b)
  () (swap a b))
=> (define-for-cbr do-f (b)
     ([a get_1 put_1]) (swap a b))
=> (define-for-cbr do-f ()
     ([a get_1 put_1] [b get_2 put_2]) (swap a b))
=> (define (do-f get_1 get_2 put_1 put_2)
     (define-get/put-id a get_1 put_1)
     (define-get/put-id b get_2 put_2)
     (swap a b))
]

The ``subscripts'' on @scheme[get_1], @scheme[get_2],
@scheme[put_1], and @scheme[put_2] are inserted by the macro
expander to preserve lexical scope, since the @scheme[get]
generated by each iteration of @scheme[define-for-cbr] should not
bind the @scheme[get] generated by a different iteration. In
other words, we are essentially tricking the macro expander into
generating fresh names for us, but the technique illustrates some
of the surprising power of pattern-based macros with automatic
lexical scope.

The last expression eventually expands to just

@schemeblock[
(define (do-f get_1 get_2 put_1 put_2)
  (let ([tmp (get_1)])
    (put_1 (get_2))
    (put_2 tmp)))
]

which implements the call-by-name function @scheme[f].

To summarize, then, we can add call-by-reference functions to
Scheme with just three small pattern-based macros:
@scheme[define-cbr], @scheme[define-for-cbr], and
@scheme[define-get/put-id].

