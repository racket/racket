#lang scribble/manual
@(require scribble/struct scribble/decode scribble/eval "utils.rkt"
          (for-label racket/base racket/contract syntax/kerncase
                     unstable/syntax))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/syntax (for-syntax racket/base unstable/syntax)))

@title[#:tag "syntax"]{Syntax}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/syntax]

@defproc[(explode-module-path-index [mpi module-path-index?])
         (listof (or/c module-path? resolved-module-path? #f))]{

Unfolds @racket[mpi] using @racket[module-path-index-split], returning
a list of the relative module paths together with the terminal
resolved module path or @racket[#f] for the ``self'' module.

@examples[#:eval the-eval
(explode-module-path-index (car (identifier-binding #'lambda)))
(explode-module-path-index (caddr (identifier-binding #'lambda)))
(explode-module-path-index (car (identifier-binding #'define-values)))
]
}

@defform[(phase-of-enclosing-module)]{

Returns the phase level of the module in which the form occurs (and
for the instantiation of the module in which the form is
executed). For example, if a module is required directly by the
``main'' module (or the top level), its phase level is 0. If a module
is required for-syntax by the ``main'' module (or the top level), its
phase level is 1.

@examples[#:eval the-eval
(module helper racket
  (require unstable/syntax)
  (displayln (phase-of-enclosing-module)))
(require 'helper)
(require (for-meta 1 'helper))
]
}

@defproc[(make-variable-like-transformer [reference-stx syntax?]
                                         [setter-stx (or/c syntax? #f) #f])
         set!-transformer?]{

Creates a transformer that replaces references to the macro identifier
with @racket[reference-stx]. Uses of the macro in operator position
are interpreted as an application with @racket[reference-stx] as the
function and the arguments as given.

If the macro identifier is used as the target of a @racket[set!] form,
then the @racket[set!] form expands into the application of
@racket[setter-stx] to the @racket[set!] expression's right-hand side,
if @racket[setter-stx] is syntax; otherwise, the identifier is
considered immutable and a syntax error is raised.

@examples[#:eval the-eval
(define the-box (box add1))
(define-syntax op
  (make-variable-like-transformer
   #'(unbox the-box)
   #'(lambda (v) (set-box! the-box v))))
(op 5)
(set! op 0)
op
]

}

@;{----}

@addition{@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]}
@defproc[(format-unique-id [lctx (or/c syntax? #f)]
                           [fmt string?]
                           [v (or/c string? symbol? identifier? keyword? char? number?)] ...
                           [#:source src (or/c syntax? #f) #f]
                           [#:props props (or/c syntax? #f) #f]
                           [#:cert cert (or/c syntax? #f) #f])
         identifier?]{
Like @racket[format-id], but returned identifiers are guaranteed to be unique.
}
@defproc[(syntax-within? [a syntax?] [b syntax?])
         boolean?]{
Returns true is syntax @racket[a] is within syntax @racket[b] in the source.
Bounds are inclusive.
}

@;{----}

@addition{@author+email["Sam Tobin-Hochstadt" "samth@racket-lang.org"]}

@defproc[(syntax-map [f (-> syntax? A)] [stxl syntax?] ...) (listof A)]{
Performs @racket[(map f (syntax->list stxl) ...)].

@examples[#:eval the-eval
(syntax-map syntax-e #'(a b c))]
}

@addition[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@section{Syntax Object Source Locations}

@deftogether[(
@defproc[(syntax-source-directory [stx syntax?]) (or/c path? #f)]
@defproc[(syntax-source-file-name [stx syntax?]) (or/c path? #f)]
)]{

These produce the directory and file name, respectively, of the path with which
@racket[stx] is associated, or @racket[#f] if @racket[stx] is not associated
with a path.

@defexamples[
#:eval the-eval
(define loc
  (list (build-path "/tmp" "dir" "somewhere.rkt")
        #f #f #f #f))
(define stx1 (datum->syntax #f 'somewhere loc))
(syntax-source-directory stx1)
(syntax-source-file-name stx1)
(define stx2 (datum->syntax #f 'nowhere #f))
(syntax-source-directory stx2)
(syntax-source-directory stx2)
]
}

@close-eval[the-eval]
