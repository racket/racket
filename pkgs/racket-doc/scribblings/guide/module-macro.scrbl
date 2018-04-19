#lang scribble/doc
@(require scribble/manual
          scribble/example
          "guide-utils.rkt")

@(define noisy-eval (make-base-eval))

@title[#:tag "module-macro"]{Modules and Macros}

Racket's module system cooperates closely with Racket's @tech{macro}
system for adding new syntactic forms to Racket. For example, in the
same way that importing @racketmodname[racket/base] introduces syntax
for @racket[require] and @racket[lambda], importing other modules can
introduce new syntactic forms (in addition to more traditional kinds
of imports, such as functions or constants).

We introduce macros in more detail later, in @secref["macros"], but
here's a simple example of a module that defines a pattern-based
macro:

@examples[
#:eval noisy-eval
#:no-result
(module noisy racket
  (provide define-noisy)

  (define-syntax-rule (define-noisy (id arg ...) body)
    (define (id arg ...)
      (show-arguments (quote id) (list arg ...))
      body))

  (define (show-arguments name args)
    (printf "calling ~s with arguments ~e" name args)))
]

The @racket[define-noisy] binding provided by this module is a
@tech{macro} that acts like @racket[define] for a function, but it
causes each call to the function to print the arguments that are
provided to the function:

@examples[
#:label #f
#:eval noisy-eval
(require 'noisy)
(define-noisy (f x y)
  (+ x y))
(f 1 2)
]

Roughly, the @racket[define-noisy] form works by replacing

@racketblock[(define-noisy (f x y)
               (+ x y))]

with

@racketblock[(define (f x y)
               (show-arguments 'f (list x y))
               (+ x y))]

Since @racket[show-arguments] isn't provided by the @racket[noisy]
module, however, this literal textual replacement is not quite right.
The actual replacement correctly tracks the origin of identifiers like
@racket[show-arguments], so they can refer to other definitions in the
place where the macro is defined---even if those identifiers are not
available at the place where the macro is used.

There's more to the macro and module interaction than identifier
binding. The @racket[define-syntax-rule] form is itself a macro, and
it expands to compile-time code that implements the transformation
from @racket[define-noisy] into @racket[define]. The module system
keeps track of which code needs to run at compile and which needs to
run normally, as explained more in @secref["stx-phases"] and
@secref["macro-module"].

@; ----------------------------------------------------------------------

@close-eval[noisy-eval]
