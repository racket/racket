#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label (only-in syntax/parse ...+)))

@(define-syntax-rule (defdummy id)
   (defidentifier (quote-syntax id)
                  #:form? #t #:index? #f #:show-libs? #f))

@title[#:tag "stxparse-intro"]{Introduction}

@;{Dummy declaration}
@declare-exporting[syntax/scribblings/parse/parse-dummy-bindings]

This section provides an introduction to writing robust macros with
@racket[syntax-parse] and syntax classes.

As a running example we use the following task: write a macro named
@racket[mylet] that has the same syntax and behavior as Racket's
@racket[let] form. The macro should produce good error messages when
used incorrectly.

Here is the specification of @racket[mylet]'s syntax:

@;{bleh!}
@specform[#:literals (mylet)
   (code:line (@#,(defdummy mylet) ([var-id rhs-expr] ...) body ...+)
              (mylet loop-id ([var-id rhs-expr] ...) body ...+))]

For simplicity, we handle only the first case for now. We return to
the second case later in the introduction.

The macro can be implemented very simply using
@racket[define-syntax-rule]:

@myinteraction[
(define-syntax-rule (mylet ([var rhs] ...) body ...)
  ((lambda (var ...) body ...) rhs ...))
]

When used correctly, the macro works, but it behaves very badly in the
presence of errors. In some cases, the macro merely fails with an
uninformative error message; in others, it blithely accepts illegal
syntax and passes it along to @racket[lambda], with strange
consequences:

@myinteraction[
(mylet ([a 1] [b 2]) (+ a b))
(mylet (b 2) (sub1 b))
(mylet ([1 a]) (add1 a))
(mylet ([#:x 1] [y 2]) (* x y))
]

These examples of illegal syntax are not to suggest that a typical
programmer would make such mistakes attempting to use
@racket[mylet]. At least, not often, not after an initial learning
curve. But macros are also used by inexpert programmers and as targets
of other macros (or code generators), and many macros are far more
complex than @racket[mylet]. Macros must validate their syntax and
report appropriate errors. Furthermore, the macro writer benefits from
the @emph{machine-checked} specification of syntax in the form of more
readable, maintainable code.

We can improve the error behavior of the macro by using
@racket[syntax-parse]. First, we import @racket[syntax-parse] into the
@tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{transformer environment},
since we will use it to implement a macro transformer.

@myinteraction[(require (for-syntax syntax/parse))]

The following is the syntax specification above transliterated into a
@racket[syntax-parse] macro definition. It behaves no better than the
version using @racket[define-syntax-rule] above.

@myinteraction[
(define-syntax (mylet stx)
  (syntax-parse stx
    [(_ ([var-id rhs-expr] ...) body ...+)
     #'((lambda (var-id ...) body ...) rhs-expr ...)]))
]

One minor difference is the use of @racket[...+] in the pattern;
@racket[...] means match zero or more repetitions of the preceding
pattern; @racket[...+] means match one or more. Only @racket[...] may
be used in the template, however.

The first step toward validation and high-quality error reporting is
annotating each of the macro's pattern variables with the @tech{syntax
class} that describes its acceptable syntax. In @racket[mylet], each
variable must be an @racket[identifier] (@racket[id] for short) and
each right-hand side must be an @racket[expr] (expression). An
@tech{annotated pattern variable} is written by concatenating the
pattern variable name, a colon character, and the syntax class
name.@margin-note*{For an alternative to the ``colon'' syntax, see the
@racket[~var] pattern form.}

@myinteraction[
(define-syntax (mylet stx)
  (syntax-parse stx
    [(_ ((var:id rhs:expr) ...) body ...+)
     #'((lambda (var ...) body ...) rhs ...)]))
]
Note that the syntax class annotations do not appear in the template
(i.e., @racket[var], not @racket[var:id]).

The syntax class annotations are checked when we use the macro.
@myinteraction[
(mylet ([a 1] [b 2]) (+ a b))
(mylet (["a" 1]) (add1 a))
]
The @racket[expr] syntax class does not actually check that the term
it matches is a valid expression---that would require calling that
macro expander. Instead, @racket[expr] just means not a keyword.
@myinteraction[
(mylet ([a #:whoops]) 1)
]
Also, @racket[syntax-parse] knows how to report a few kinds of errors
without any help:
@myinteraction[
(mylet ([a 1 2]) (* a a))
]
There are other kinds of errors, however, that this macro does not
handle gracefully:
@myinteraction[
(mylet (a 1) (+ a 2))
]
It's too much to ask for the macro to respond, ``This expression is
missing a pair of parentheses around @racket[(a 1)].'' The pattern
matcher is not that smart. But it can pinpoint the source of the
error: when it encountered @racket[a] it was expecting what we might
call a ``binding pair,'' but that term is not in its vocabulary yet.

To allow @racket[syntax-parse] to synthesize better errors, we must
attach @emph{descriptions} to the patterns we recognize as discrete
syntactic categories. One way of doing that is by defining new syntax
classes:@margin-note*{Another way is the @racket[~describe] pattern
form.}

@myinteraction[
(define-syntax (mylet stx)

  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (syntax-parse stx
    [(_ (b:binding ...) body ...+)
     #'((lambda (b.var ...) body ...) b.rhs ...)]))
]

Note that we write @racket[b.var] and @racket[b.rhs] now. They are the
@tech{nested attributes} formed from the annotated pattern variable
@racket[b] and the attributes @racket[var] and @racket[rhs] of the
syntax class @racket[binding].

Now the error messages can talk about ``binding pairs.''
@myinteraction[
(mylet (a 1) (+ a 2))
]
Errors are still reported in more specific terms when possible:
@myinteraction[
(mylet (["a" 1]) (+ a 2))
]

There is one other constraint on the legal syntax of
@racket[mylet]. The variables bound by the different binding pairs
must be distinct. Otherwise the macro creates an illegal
@racket[lambda] form:
@myinteraction[
(mylet ([a 1] [a 2]) (+ a a))
]

Constraints such as the distinctness requirement are expressed as side
conditions, thus:
@myinteraction[
(define-syntax (mylet stx)

  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (syntax-parse stx
    [(_ (b:binding ...) body ...+)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(b.var ...)))
                 "duplicate variable name"
     #'((lambda (b.var ...) body ...) b.rhs ...)]))
]
@myinteraction[
(mylet ([a 1] [a 2]) (+ a a))
]
The @racket[#:fail-when] keyword is followed by two expressions: the
condition and the error message. When the condition evaluates to
anything but @racket[#f], the pattern fails. Additionally, if the
condition evaluates to a syntax object, that syntax object is used to
pinpoint the cause of the failure.

Syntax classes can have side conditions, too. Here is the macro
rewritten to include another syntax class representing a ``sequence of
distinct binding pairs.''
@myinteraction[
(define-syntax (mylet stx)

  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (define-syntax-class distinct-bindings
    #:description "sequence of distinct binding pairs"
    (pattern (b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
                         "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...)))

  (syntax-parse stx
    [(_ bs:distinct-bindings . body)
     #'((lambda (bs.var ...) . body) bs.rhs ...)]))
]
Here we've introduced the @racket[#:with] clause. A @racket[#:with]
clause matches a pattern with a computed term. Here we use it to bind
@racket[var] and @racket[rhs] as attributes of
@racket[distinct-bindings]. By default, a syntax class only exports
its patterns' pattern variables as attributes, not their nested
attributes.@margin-note*{The alternative would be to explicitly declare
the attributes of @racket[distinct-bindings] to include the nested
attributes @racket[b.var] and @racket[b.rhs], using the
@racket[#:attribute] option. Then the macro would refer to
@racket[bs.b.var] and @racket[bs.b.rhs].}

Alas, so far the macro only implements half of the functionality
offered by Racket's @racket[let]. We must add the
``named-@racket[let]'' form.  That turns out to be as simple as adding
a new clause:

@myinteraction[
(define-syntax (mylet stx)

  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (define-syntax-class distinct-bindings
    #:description "sequence of distinct binding pairs"
    (pattern (b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
                         "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...)))

  (syntax-parse stx
    [(_ bs:distinct-bindings body ...+)
     #'((lambda (bs.var ...) body ...) bs.rhs ...)]
    [(_ loop:id bs:distinct-bindings body ...+)
     #'(letrec ([loop (lambda (bs.var ...) body ...)])
         (loop bs.rhs ...))]))
]
We are able to reuse the @racket[distinct-bindings] syntax class, so
the addition of the ``named-@racket[let]'' syntax requires only three
lines.

But does adding this new case affect @racket[syntax-parse]'s ability
to pinpoint and report errors?
@myinteraction[
(mylet ([a 1] [b 2]) (+ a b))
(mylet (["a" 1]) (add1 a))
(mylet ([a #:whoops]) 1)
(mylet ([a 1 2]) (* a a))
(mylet (a 1) (+ a 2))
(mylet ([a 1] [a 2]) (+ a a))
]
The error reporting for the original syntax seems intact. We should
verify that the named-@racket[let] syntax is working, that
@racket[syntax-parse] is not simply ignoring that clause.
@myinteraction[
(mylet loop ([a 1] [b 2]) (+ a b))
(mylet loop (["a" 1]) (add1 a))
(mylet loop ([a #:whoops]) 1)
(mylet loop ([a 1 2]) (* a a))
(mylet loop (a 1) (+ a 2))
(mylet loop ([a 1] [a 2]) (+ a a))
]

How does @racket[syntax-parse] decide which clause the programmer was
attempting, so it can use it as a basis for error reporting? After
all, each of the bad uses of the named-@racket[let] syntax are also
bad uses of the normal syntax, and vice versa. And yet the macro doen
not produce errors like ``@racket[mylet]: expected sequence of
distinct binding pairs at: @racket[loop].''

The answer is that @racket[syntax-parse] records a list of all the
potential errors (including ones like @racket[loop] not matching
@racket[distinct-binding]) along with the @emph{progress} made before
each error. Only the error with the most progress is reported.

For example, in this bad use of the macro,
@myinteraction[
(mylet loop (["a" 1]) (add1 a))
]
there are two potential errors: expected @racket[distinct-bindings] at
@racket[loop] and expected @racket[identifier] at @racket["a"]. The
second error occurs further in the term than the first, so it is
reported. 

For another example, consider this term:
@myinteraction[
(mylet (["a" 1]) (add1 a))
]
Again, there are two potential errors: expected @racket[identifier] at
@racket[(["a" 1])] and expected @racket[identifier] at
@racket["a"]. They both occur at the second term (or first argument,
if you prefer), but the second error occurs deeper in the
term. Progress is based on a left-to-right traversal of the syntax.

A final example: consider the following:
@myinteraction[
(mylet ([a 1] [a 2]) (+ a a))
]
There are two errors again: duplicate variable name at @racket[([a 1]
[a 2])] and expected @racket[identifier] at @racket[([a 1] [a
2])]. Note that as far as @racket[syntax-parse] is concerned, the
progress associated with the duplicate error message is the second
term (first argument), not the second occurrence of @racket[a]. That's
because the check is associated with the entire
@racket[distinct-bindings] pattern. It would seem that both errors
have the same progress, and yet only the first one is reported. The
difference between the two is that the first error is from a
@emph{post-traversal} check, whereas the second is from a normal
(i.e., pre-traversal) check. A post-traveral check is considered to
have made more progress than a pre-traversal check of the same term;
indeed, it also has greater progress than any failure @emph{within}
the term.

It is, however, possible for multiple potential errors to occur with
the same progress. Here's one example:
@myinteraction[
(mylet "not-even-close")
]
In this case @racket[syntax-parse] reports both errors.

Even with all of the annotations we have added to our macro, there are
still some misuses that defy @racket[syntax-parse]'s error reporting
capabilities, such as this example:
@myinteraction[
(mylet)
]
The philosophy behind @racket[syntax-parse] is that in these
situations, a generic error such as ``bad syntax'' is justified. The
use of @racket[mylet] here is so far off that the only informative
error message would include a complete recapitulation of the syntax of
@racket[mylet]. That is not the role of error messages, however; it is
the role of documentation.

This section has provided an introduction to syntax classes, side
conditions, and progress-ordered error reporting. But
@racket[syntax-parse] has many more features. Continue to the
@secref{stxparse-examples} section for samples of other features in
working code, or skip to the subsequent sections for the complete
reference documentation.
