#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define swap-eval (make-base-eval))

@title[#:tag "pattern-macros"]{Pattern-Based Macros}

A @deftech{pattern-based macro} replaces any code that matches a
pattern to an expansion that uses parts of the original syntax that
match parts of the pattern.

@; ----------------------------------------

@section{@racket[define-syntax-rule]}

The simplest way to create a macro is to use
@racket[define-syntax-rule]:

@specform[(define-syntax-rule pattern template)]

As a running example, consider the @racket[swap] macro, which swaps
the values stored in two variables. It can be implemented using
@racket[define-syntax-rule] as follows:

@margin-note{The macro is ``un-Rackety'' in the sense that it
involves side effects on variables---but the point of macros is to let
you add syntactic forms that some other language designer might not
approve.}

@racketblock[
(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))
]

The @racket[define-syntax-rule] form binds a macro that matches a
single pattern. The pattern must always start with an open parenthesis
followed by an identifier, which is @racket[swap] in this case. After
the initial identifier, other identifiers are @deftech{macro pattern
variables} that can match anything in a use of the macro. Thus, this
macro matches the form @racket[(swap _form1 _form2)] for any
@racket[_form_1] and @racket[_form_2].

@margin-note{Macro pattern variables similar to pattern variables for
 @racket[match]. See @secref["match"].}

After the pattern in @racket[define-syntax-rule] is the
@deftech{template}. The template is used in place of a form that
matches the pattern, except that each instance of a pattern variable
in the template is replaced with the part of the macro use the pattern
variable matched. For example, in

@racketblock[(swap first last)]

the pattern variable @racket[x] matches @racket[first] and @racket[y]
matches @racket[last], so that the expansion is

@racketblock[
  (let ([tmp first])
    (set! first last)
    (set! last tmp))
]

@; ----------------------------------------

@section{Lexical Scope}

Suppose that we use the @racket[swap] macro to swap variables named
@racket[tmp] and @racket[other]:

@racketblock[
(let ([tmp 5]
      [other 6])
  (swap tmp other)
  (list tmp other))
]

The result of the above expression should be @racketresult[(6 5)]. The
naive expansion of this use of @racket[swap], however, is

@racketblock[
(let ([tmp 5]
      [other 6])
  (let ([tmp tmp])
    (set! tmp other)
    (set! other tmp))
  (list tmp other))
]

whose result is @racketresult[(5 6)]. The problem is that the naive
expansion confuses the @racket[tmp] in the context where @racket[swap]
is used with the @racket[tmp] that is in the macro template.

Racket doesn't produce the naive expansion for the above use of
@racket[swap]. Instead, it produces

@racketblock[
(let ([tmp 5]
      [other 6])
  (let ([tmp_1 tmp])
    (set! tmp other)
    (set! other tmp_1))
  (list tmp other))
]

with the correct result in @racketresult[(6 5)]. Similarly, in the
example

@racketblock[
(let ([set! 5]
      [other 6])
  (swap set! other)
  (list set! other))
]

the expansion is 

@racketblock[
(let ([set!_1 5]
      [other 6])
  (let ([tmp_1 set!_1])
    (set! set!_1 other)
    (set! other tmp_1))
  (list set!_1 other))
]

so that the local @racket[set!] binding doesn't interfere with the
assignments introduced by the macro template.

In other words, Racket's pattern-based macros automatically maintain
lexical scope, so macro implementors can reason about variable
reference in macros and macro uses in the same way as for functions
and function calls.

@; ----------------------------------------

@section{@racket[define-syntax] and @racket[syntax-rules]}

The @racket[define-syntax-rule] form binds a macro that matches a
single pattern, but Racket's macro system supports transformers that
match multiple patterns starting with the same identifier. To write
such macros, the programmer must use the more general
@racket[define-syntax] form along with the @racket[syntax-rules]
transformer form:

@specform[#:literals (syntax-rules)
          (define-syntax id
            (syntax-rules (literal-id ...)
              [pattern template]
              ...))]

@margin-note{The @racket[define-syntax-rule] form is itself a macro
 that expands into @racket[define-syntax] with a @racket[syntax-rules]
 form that contains only one pattern and template.}

For example, suppose we would like a @racket[rotate] macro that
generalizes @racket[swap] to work on either two or three identifiers,
so that

@racketblock[
(let ([red 1] [green 2] [blue 3])
  (rotate red green)      (code:comment @#,t{swaps})
  (rotate red green blue) (code:comment @#,t{rotates left})
  (list red green blue))
]

produces @racketresult[(1 3 2)]. We can implement @racket[rotate]
using @racket[syntax-rules]:

@racketblock[
(define-syntax rotate
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c) (begin
                     (swap a b)
                     (swap b c))]))
]

The expression @racket[(rotate red green)] matches the first pattern
in the @racket[syntax-rules] form, so it expands to @racket[(swap red
green)]. The expression @racket[(rotate a b c)] matches the second
pattern, so it expands to @racket[(begin (swap red green) (swap green
blue))].

@; ----------------------------------------

@section{Matching Sequences}

A better @racket[rotate] macro would allow any number of identifiers,
instead of just two or three. To match a use of @racket[rotate] with
any number of identifiers, we need a pattern form that has something
like a Kleene star. In a Racket macro pattern, a star is written as
@racket[...].

To implement @racket[rotate] with @racket[...], we need a base case to
handle a single identifier, and an inductive case to handle more than
one identifier:

@racketblock[
(define-syntax rotate
  (syntax-rules ()
    [(rotate a) (void)]
    [(rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...))]))
]

When a pattern variable like @racket[c] is followed by @racket[...] in
a pattern, then it must be followed by @racket[...] in a template,
too. The pattern variable effectively matches a sequence of zero or
more forms, and it is replaced in the template by the same sequence.

Both versions of @racket[rotate] so far are a bit inefficient, since
pairwise swapping keeps moving the value from the first variable into
every variable in the sequence until it arrives at the last one. A
more efficient @racket[rotate] would move the first value directly to
the last variable. We can use @racket[...] patterns to implement the
more efficient variant using a helper macro:

@racketblock[
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

In the @racket[shift-to] macro, @racket[...] in the template follows
@racket[(set! to from)], which causes the @racket[(set! to from)]
expression to be duplicated as many times as necessary to use each
identifier matched in the @racket[to] and @racket[from]
sequences. (The number of @racket[to] and @racket[from] matches must
be the same, otherwise the macro expansion fails with an error.)

@; ----------------------------------------

@section{Identifier Macros}

Given our macro definitions, the @racket[swap] or @racket[rotate]
identifiers must be used after an open parenthesis, otherwise a syntax
error is reported:

@interaction-eval[#:eval swap-eval (define-syntax swap (syntax-rules ()))]

@interaction[#:eval swap-eval (+ swap 3)]

An @deftech{identifier macro} is a pattern-matching macro that
works in any expression. For example, we
can define @racket[clock] as an identifier macro that expands to
@racket[(get-clock)], so @racket[(+ clock 3)] would expand to
@racket[(+ (get-clock) 3)]. An identifier macro also cooperates with
@racket[set!], and we can define @racket[clock] so that @racket[(set!
clock 3)] expands to @racket[(put-clock! 3)].

The @racket[syntax-id-rules] form is like @racket[syntax-rules], but
it creates a transformer that acts as an identifier macro:

@specform[#:literals (syntax-id-rules)
          (define-syntax id
            (syntax-id-rules (literal-id ...)
              [pattern template]
              ...))]

Unlike a @racket[syntax-rules] form, the @racket[_pattern]s are not
required to start with an open parenthesis. In addition,
@racket[syntax-id-rules] cooperates specially with @racket[set!], so
that @racket[set!] invokes the macro when @racket[_id] is the target
of an assignment; consequently, @racket[set!] is typically used as a
literal with @racket[syntax-id-rules] to match such uses of @racket[set!].

@racketblock[
(define-syntax clock
  (syntax-id-rules (set!)
    [(set! clock e) (put-clock! e)]
    [(clock a ...) ((get-clock) a ...)]
    [clock (get-clock)]))

(define-values (get-clock put-clock!)
  (let ([private-clock 0])
    (values (lambda () private-clock)
            (lambda (v) (set! private-clock v)))))
]

The @racket[(clock a ...)] pattern is needed because, when an
identifier macro is used after an open parenthesis, the macro
transformer is given the whole form, like with a non-identifier macro.
Put another way, the @racket[syntax-rules] form is essentially a
special case of the @racket[syntax-id-rules] form with errors in the
@racket[set!] and lone-identifier cases.

@; ----------------------------------------

@section{Macro-Generating Macros}

Suppose that we have many identifiers like @racket[clock] that we'd
like to redirect to accessor and mutator functions like
@racket[get-clock] and @racket[put-clock!]. We'd like to be able to
just write

@racketblock[
(define-get/put-id clock get-clock put-clock!)
]

Naturally, we can implement @racket[define-get/put-id] as a macro:

@racketblock[
(define-syntax-rule (define-get/put-id id get put!)
  (define-syntax id
    (syntax-id-rules (set!)
      [(set! id e) (put! e)]
      [(id a (... ...)) ((get) a (... ...))]
      [id (get)])))
]

The @racket[define-get/put-id] macro is a @deftech{macro-generating
macro}.  The only non-obvious part of its definition is the
@racket[(... ...)], which ``quotes'' @racket[...] so that it takes its
usual role in the generated macro, instead of the generating macro.

@; ----------------------------------------

@section[#:tag "pattern-macro-example"]{Extended Example: Call-by-Reference Functions}

We can use pattern-matching macros to add a form to Racket
for defining first-order @deftech{call-by-reference} functions. When a
call-by-reference function body mutates its formal argument, the
mutation applies to variables that are supplied as actual arguments in
a call to the function.

For example, if @racket[define-cbr] is like @racket[define] except
that it defines a call-by-reference function, then

@racketblock[
(define-cbr (f a b)
  (swap a b))

(let ([x 1] [y 2])
  (f x y)
  (list x y))
]

produces @racketresult[(2 1)]. 

We will implement call-by-reference functions by having function calls
supply accessor and mutators for the arguments, instead of supplying
argument values directly. In particular, for the function @racket[f]
above, we'll generate

@racketblock[
(define (do-f get-a get-b put-a! put-b!)
  (define-get/put-id a get-a put-a!)
  (define-get/put-id b get-b put-b!)
  (swap a b))
]

and redirect a function call @racket[(f x y)] to

@racketblock[
(do-f (lambda () x)
      (lambda () y)
      (lambda (v) (set! x v))
      (lambda (v) (set! y v)))
]

Clearly, then @racket[define-cbr] is a macro-generating macro, which
binds @racket[f] to a macro that expands to a call of @racket[do-f].
That is, @racket[(define-cbr (f a b) (swap a b))] needs to generate the
definition

@racketblock[
(define-syntax f
  (syntax-rules ()
    [(id actual ...)
     (do-f (lambda () actual)
           ...
           (lambda (v)
             (set! actual v))
           ...)]))
]

At the same time, @racket[define-cbr] needs to define @racket[do-f]
using the body of @racket[f], this second part is slightly more
complex, so we defer most it to a @racket[define-for-cbr] helper
module, which lets us write @racket[define-cbr] easily enough:


@racketblock[
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

Our remaining task is to define @racket[define-for-cbr] so that it
converts

@racketblock[
(define-for-cbr do-f (a b) () (swap a b))
]

to the function definition @racket[do-f] above. Most of the work is
generating a @racket[define-get/put-id] declaration for each argument,
@racket[a] and @racket[b], and putting them before the body. Normally,
that's an easy task for @racket[...] in a pattern and template, but
this time there's a catch: we need to generate the names
@racket[get-a] and @racket[put-a!] as well as @racket[get-b] and
@racket[put-b!], and the pattern language provides no way to
synthesize identifiers based on existing identifiers.

As it turns out, lexical scope gives us a way around this problem. The
trick is to iterate expansions of @racket[define-for-cbr] once for
each argument in the function, and that's why @racket[define-cbr]
starts with an apparently useless @racket[()] after the argument
list. We need to keep track of all the arguments seen so far and the
@racket[get] and @racket[put] names generated for each, in addition to
the arguments left to process. After we've processed all the
identifiers, then we have all the names we need.

Here is the definition of @racket[define-for-cbr]:

@racketblock[
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

@racketblock[
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

The ``subscripts'' on @racket[get_1], @racket[get_2],
@racket[put_1], and @racket[put_2] are inserted by the macro
expander to preserve lexical scope, since the @racket[get]
generated by each iteration of @racket[define-for-cbr] should not
bind the @racket[get] generated by a different iteration. In
other words, we are essentially tricking the macro expander into
generating fresh names for us, but the technique illustrates some
of the surprising power of pattern-based macros with automatic
lexical scope.

The last expression eventually expands to just

@racketblock[
(define (do-f get_1 get_2 put_1 put_2)
  (let ([tmp (get_1)])
    (put_1 (get_2))
    (put_2 tmp)))
]

which implements the call-by-name function @racket[f].

To summarize, then, we can add call-by-reference functions to
Racket with just three small pattern-based macros:
@racket[define-cbr], @racket[define-for-cbr], and
@racket[define-get/put-id].

@; -----------------------------------------------------------------

@close-eval[swap-eval]
