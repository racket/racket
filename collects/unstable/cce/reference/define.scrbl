#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/define))

@title[#:style 'quiet #:tag "cce-define"]{Definitions}

@defmodule[unstable/cce/define]

This module provides macros for creating and manipulating definitions.

@section{Interleaving Definitions and Expressions}

@defform[(block def-or-expr ...)]{

This expression establishes a lexically scoped block (i.e. an internal
definition context) in which definitions and expressions may be interleaved.
Its result is that of the last term (after @scheme[begin]-splicing), executed in
tail position, if the term is an expression; if there are no terms, or the last
term is a definition, its result is @scheme[(void)].

This form is equivalent to @scheme[(begin-with-definitions def-or-expr ...)].

@defexamples[
#:eval (evaluator 'unstable/cce/define)
(define (intersection list-one list-two)
  (block

    (define hash-one (make-hash))
    (for ([x (in-list list-one)])
      (hash-set! hash-one x #t))

    (define hash-two (make-hash))
    (for ([x (in-list list-two)])
      (hash-set! hash-two x #t))

    (for/list ([x (in-hash-keys hash-one)]
               #:when (hash-has-key? hash-two x))
      x)))

(intersection (list 1 2 3) (list 2 3 4))
]

}

@section{Deferred Evaluation in Modules}

@defform[(at-end expr)]{

When used at the top level of a module, evaluates @scheme[expr] at the end of
the module.  This can be useful for calling functions before their definitions.

@defexamples[
#:eval (evaluator 'unstable/cce/define)
(module Failure scheme
  (f 5)
  (define (f x) x))
(require 'Failure)
(module Success scheme
  (require unstable/cce/define)
  (at-end (f 5))
  (define (f x) x))
(require 'Success)
]

}

@section{Conditional Binding}

@deftogether[(
@defform*[[(define-if-unbound x e)
           (define-if-unbound (f . args) body ...)]]
@defform[(define-values-if-unbound [x ...] e)]
@defform*[[(define-syntax-if-unbound x e) 
           (define-syntax-if-unbound (f . args) body ...)]]
@defform[(define-syntaxes-if-unbound [x ...] e)]
)]{

These forms define each @scheme[x] (or @scheme[f]) if no such binding exists, or
do nothing if the name(s) is(are) already bound.  The
@scheme[define-values-if-unbound] and @scheme[define-syntaxes-if-unbound] forms
raise a syntax error if some of the given names are bound and some are not.

These are useful for writing programs that are portable across versions of PLT
Scheme with different bindings, to provide an implementation of a binding for
versions that do not have it but use the built-in one in versions that do.

@defexamples[
#:eval (evaluator 'unstable/cce/define)
(define-if-unbound x 1)
x
(define y 2)
(define-if-unbound y 3)
y
]

}

@section{Renaming Definitions}

@defform[(define-renamings [new old] ...)]{

This form establishes a rename transformer for each @scheme[new] identifier,
redirecting it to the corresponding @scheme[old] identifier.

@defexamples[
#:eval (evaluator 'unstable/cce/define)
(define-renamings [def define] [lam lambda])
(def plus (lam (x y) (+ x y)))
(plus 1 2)
]

}

@section{Forward Declarations}

@defform[(declare-names x ...)]{

This form provides forward declarations of identifiers to be defined later.  It
is useful for macros which expand to mutually recursive definitions, including
forward references, that may be used at the PLT Scheme top level.

}

@section{Definition Shorthands}

@defform[(define-with-parameter name parameter)]{

Defines the form @scheme[name] as a shorthand for setting the parameter
@scheme[parameter].  Specifically, @scheme[(name value body ...)] is equivalent
to @scheme[(parameterize ([parameter value]) body ...)].

@defexamples[
#:eval (evaluator 'unstable/cce/define)
(define-with-parameter with-input current-input-port)
(with-input (open-input-string "Tom Dick Harry") (read))
]

}

@defform[(define-single-definition define-one-name define-many-name)]{

This form defines a marco @scheme[define-one-name] as a single identifier
definition form with function shorthand like @scheme[define] and
@scheme[define-syntax], based on an existing macro @scheme[define-many-name]
which works like @scheme[define-values] or @scheme[define-syntaxes].

@defexamples[
#:eval (evaluator 'unstable/cce/define)
(define-single-definition define-like define-values)
(define-like x 0)
x
(define-like (f a b c) (printf "~s, ~s\n" a b) c)
(f 1 2 3)
]

}

@section{Effectful Transformation}

@defform[(in-phase1 e)]{

This form executes @scheme[e] during phase 1 (the syntax transformation phase)
relative to its context, during pass 1 if it occurs in a head expansion
position.

}

@defform[(in-phase1/pass2 e)]{

This form executes @scheme[e] during phase 1 (the syntax transformation phase)
relative to its context, during pass 2 (after head expansion).

}
