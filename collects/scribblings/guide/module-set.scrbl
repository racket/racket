#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "module-set"]{Assignment and Redefinition}

The use of @racket[set!] on variables defined within a module is
limited to the body of the defining module. That is, a module is
allowed to change the value of its own definitions, and such changes
are visible to importing modules. However, an importing context is not
allowed to change the value of an imported binding.

@examples[
(module m racket
  (provide counter increment!)
  (define counter 0)
  (define (increment!)
    (set! counter (add1 counter))))
(require 'm)
(eval:alts counter (eval 'counter))
(eval:alts (increment!) (eval '(increment!)))
(eval:alts counter (eval 'counter))
(eval:alts (set! counter -1) (eval '(set! counter -1)))
]

As the above example illustrates, a module can always grant others the
ability to change its exports by providing a mutator function, such as
@racket[increment!].

The prohibition on assignment of imported variables helps support
modular reasoning about programs. For example, in the module,

@racketblock[
(module m racket
  (provide rx:fish fishy-string?)
  (define rx:fish #rx"fish")
  (define (fishy-string? s)
    (regexp-match? s rx:fish)))
]

the function @racket[fishy-string?] will always match strings that
contain ``fish'', no matter how other modules use the @racket[rx:fish]
binding.  For essentially the same reason that it helps programmers,
the prohibition on assignment to imports also allows many programs to
be executed more efficiently.

Along the same lines, when a module contains no @racket[set!] of a
particular identifier that is defined within the module, then the
identifier is considered a @defterm{constant} that cannot be
changed---not even by re-declaring the module.

Consequently, re-declaration of a module is not generally allowed.
For file-based modules, simply changing the file does not lead to a
re-declaration in any case, because file-based modules are loaded on
demand, and the previously loaded declarations satisfy future
requests. It is possible to use Racket's reflection support to
re-declare a module, however, and non-file modules can be re-declared
in the @tech{REPL}; in such cases, the re-declaration may fail if it
involves the re-definition of a previously constant binding.

@interaction[
(module m racket
  (define pie 3.141597))
(require 'm)
(module m racket
  (define pie 3))
]

For exploration and debugging purposes, the Racket reflective layer
provides a @racket[compile-enforce-module-constants] parameter
to disable the enforcement of constants.

@interaction[
(compile-enforce-module-constants #f)
(module m2 racket
  (provide pie)
  (define pie 3.141597))
(require 'm2)
(module m2 racket
  (provide pie)
  (define pie 3))
(compile-enforce-module-constants #t)
(eval:alts pie (eval 'pie))
]
