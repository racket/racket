#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "binding"]{Identifiers and Binding}

The context of an expression determines the meaning of identifiers
that appear in the expression. In particular, starting a module with
the language @racketmodname[racket], as in

@racketmod[racket]

means that, within the module, the identifiers described in this guide
start with the meaning described here: @racket[cons] refers to the
function that creates a pair, @racket[car] refers to the function
that extracts the first element of a pair, and so on.

@guideother{@secref["symbols"] introduces the syntax of
identifiers.}

Forms like @racket[define], @racket[lambda], and @racket[let]
associate a meaning with one or more identifiers; that is, they
@defterm{bind} identifiers. The part of the program for which the
binding applies is the @defterm{scope} of the binding. The set of
bindings in effect for a given expression is the expression's
@defterm{environment}.

For example, in

@racketmod[
racket

(define f
  (lambda (x)
    (let ([y 5])
      (+ x y))))

(f 10)
]

the @racket[define] is a binding of @racket[f], the @racket[lambda]
has a binding for @racket[x], and the @racket[let] has a binding for
@racket[y]. The scope of the binding for @racket[f] is the entire
module; the scope of the @racket[x] binding is @racket[(let ([y 5]) (+
x y))]; and the scope of the @racket[y] binding is just @racket[(+ x
y)]. The environment of @racket[(+ x y)] includes bindings for
@racket[y], @racket[x], and @racket[f], as well as everything in
@racketmodname[racket].

A module-level @racket[define] can bind only identifiers that are not
already bound within the module. For example, @racket[(define cons 1)]
is a syntax error in a @racketmodname[racket] module, since @racket[cons]
is provided by @racketmodname[racket]. A local @racket[define] or other
binding forms, however, can give a new local binding for an identifier
that already has a binding; such a binding @defterm{shadows} the
existing binding.

@defexamples[
(define f
  (lambda (append)
    (define cons (append "ugly" "confusing"))
    (let ([append 'this-was])
      (list append cons))))
(f list)
]

Even identifiers like @racket[define] and @racket[lambda] get their
meanings from bindings, though they have @defterm{transformer}
bindings (which means that they indicate syntactic forms) instead of
value bindings. Since @racket[define] has a transformer binding, the
identifier @racketidfont{define} cannot be used by itself to get a
value. However, the normal binding for @racketidfont{define} can be
shadowed.

@examples[
define
(eval:alts (let ([@#,racketidfont{define} 5]) @#,racketidfont{define}) (let ([define 5]) define))
]

Shadowing standard bindings in this way is rarely a good idea, but the
possibility is an inherent part of Racket's flexibility.
