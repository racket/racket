#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "binding"]{Identifiers and Binding}

The context of an expression determines the meaning of identifiers
that appear in the expression. In particular, starting a module with
the language @schememodname[scheme], as in

@schememod[scheme]

means that, within the module, the identifiers described in this guide
start with the meaning described here: @scheme[cons] refers to the
function that creates a pair, @scheme[car] refers to the function
that extracts the first element of a pair, and so on.

@guideother{@secref["symbols"] introduces the syntax of
identifiers.}

Forms like @scheme[define], @scheme[lambda], and @scheme[let]
associate a meaning with one or more identifiers; that is, they
@defterm{bind} identifiers. The part of the program for which the
binding applies is the @defterm{scope} of the binding. The set of
bindings in effect for a given expression is the expression's
@defterm{environment}.

For example, in

@schememod[
scheme

(define f
  (lambda (x)
    (let ([y 5])
      (+ x y))))

(f 10)
]

the @scheme[define] is a binding of @scheme[f], the @scheme[lambda]
has a binding for @scheme[x], and the @scheme[let] has a binding for
@scheme[y]. The scope of the binding for @scheme[f] is the entire
module; the scope of the @scheme[x] binding is @scheme[(let ([y 5]) (+
x y))]; and the scope of the @scheme[y] binding is just @scheme[(+ x
y)]. The environment of @scheme[(+ x y)] includes bindings for
@scheme[y], @scheme[x], and @scheme[f], as well as everything in
@schememodname[scheme].

A module-level @scheme[define] can bind only identifiers that are not
already bound within the module. For example, @scheme[(define cons 1)]
is a syntax error in a @schememodname[scheme] module, since @scheme[cons]
is provided by @schememodname[scheme]. A local @scheme[define] or other
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

Even identifiers like @scheme[define] and @scheme[lambda] get their
meanings from bindings, though they have @defterm{transformer}
bindings (which means that they indicate syntactic forms) instead of
value bindings. Since @scheme[define] has a transformer binding, the
identifier @schemeidfont{define} cannot be used by itself to get a
value. However, the normal binding for @schemeidfont{define} can be
shadowed.

@examples[
define
(eval:alts (let ([@#,schemeidfont{define} 5]) @#,schemeidfont{define}) (let ([define 5]) define))
]

Shadowing standard bindings in this way is rarely a good idea, but the
possibility is an inherent part of Scheme's flexibility.
