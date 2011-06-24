#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket unstable/define
                     (only-in mzlib/etc define-syntax-set)))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/define (for-syntax racket/base)))

@title{Definitions}

@defmodule[unstable/define]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

Provides macros for creating and manipulating definitions.

@section{Deferred Evaluation in Modules}

@defform[(at-end expr)]{

When used at the top level of a module, evaluates @racket[expr] at the end of
the module.  This can be useful for calling functions before their definitions.

@defexamples[
#:eval the-eval
(module Failure scheme
  (f 5)
  (define (f x) x))
(require 'Failure)
(module Success scheme
  (require unstable/define)
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

Define each @racket[x] (or @racket[f]) if no such binding exists, or
do nothing if the name(s) is(are) already bound.  The
@racket[define-values-if-unbound] and @racket[define-syntaxes-if-unbound] forms
raise a syntax error if some of the given names are bound and some are not.

These are useful for writing programs that are portable across versions of
Racket with different bindings, to provide an implementation of a binding for
versions that do not have it but use the built-in one in versions that do.

@defexamples[
#:eval the-eval
(define-if-unbound x 1)
x
(define y 2)
(define-if-unbound y 3)
y
]

}

@section{Renaming Definitions}

@deftogether[(
@defform[(define-renaming new old)]
@defform[(define-renamings [new old] ...)]
)]{

Establishes a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{rename transformer}
for each @racket[new] identifier, redirecting it to the corresponding
@racket[old] identifier.

@defexamples[
#:eval the-eval
(define-renaming use #%app)
(define-renamings [def define] [lam lambda])
(def plus (lam (x y) (use + x y)))
(use plus 1 2)
]

}

@section{Forward Declarations}

@defform[(declare-names x ...)]{

Provides forward declarations of identifiers to be defined later.  It
is useful for macros which expand to mutually recursive definitions, including
forward references, that may be used at the Racket top level.

}

@section{Definition Shorthands}

@defform[(define-with-parameter name parameter)]{

Defines the form @racket[name] as a shorthand for setting the parameter
@racket[parameter].  Specifically, @racket[(name value body ...)] is equivalent
to @racket[(parameterize ([parameter value]) body ...)].

@defexamples[
#:eval the-eval
(define-with-parameter with-input current-input-port)
(with-input (open-input-string "Tom Dick Harry") (read))
]

}

@defform[(define-single-definition define-one-name define-many-name)]{

Defines a marco @racket[define-one-name] as a single identifier
definition form with function shorthand like @racket[define] and
@racket[define-syntax], based on an existing macro @racket[define-many-name]
which works like @racket[define-values] or @racket[define-syntaxes].

@defexamples[
#:eval the-eval
(define-single-definition define-like define-values)
(define-like x 0)
x
(define-like (f a b c) (printf "~s, ~s\n" a b) c)
(f 1 2 3)
]

}

@section{Macro Definitions}

@defform/subs[
(define-syntax-block (macro-decl ...) body ...)
([macro-decl macro-id [macro-id expander-id]])
]{

Defines a syntax transformer for each @racket[macro-id] based on the local
definition of each @racket[expander-id]
(defaulting to @racket[macro-id]@racket[/proc]) in @racket[body ...].
Especially useful for mutually recursive expander functions and phase 1 macro
definitions.  Subsumes the behavior of @racket[define-syntax-set].

@defexamples[
#:eval the-eval
(define-syntax-block
    ([implies expand-implies]
     nand)

  (define-syntax-rule (==> pattern template)
    (syntax-rules () [pattern template]))

  (define expand-implies (==> (_ a b) (or (not a) b)))
  (define nand/proc (==> (_ a ...) (not (and a ...)))))
(implies #t (printf "True!\n"))
(implies #f (printf "False!\n"))
(nand #t #t (printf "All True!\n"))
(nand #t #f (printf "Some False!\n"))
(define-syntax-block (undefined-macro)
  (define irrelevant "Whoops!"))
]
}

@section{Effectful Transformation}

@defform[(in-phase1 e)]{

Executes @racket[e] during phase 1 (the syntax transformation phase)
relative to its context, during pass 1 if it occurs in a head expansion
position.

}

@defform[(in-phase1/pass2 e)]{

Executes @racket[e] during phase 1 (the syntax transformation phase)
relative to its context, during pass 2 (after head expansion).

}

@(close-eval the-eval)
