#lang scribble/manual
@(require (for-label racket/base
                     racket/sequence))

@title{@racket[for] Clause Transforms}

@defmodule[syntax/for-transform]{The @racketmodname[syntax/for-transform] module
provides a helper function that gives access to the sequence transformers 
defined by @racket[define-sequence-syntax]. This is what the @racket[for] forms
use and enables faster 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence} traversal
than what the sequence interface provides.}

@defproc[(expand-for-clause [orig-stx syntax?] [clause syntax?]) syntax?]{

Expands a @racket[for] clause of the form @racket[[(x ...) seq-expr]], where
@racket[x] are identifiers, to:

@racketblock[
(([(outer-id ...) outer-expr] ...)
 outer-check
 ([loop-id loop-expr] ...)
 pos-guard
 ([(inner-id ...) inner-expr] ...)
 pre-guard
 post-guard
 (loop-arg ...))]

which can then be spliced into the appropriate iterations. See @racket[:do-in]
for more information.

The first argument @racket[orig-stx] is used only for reporting syntax errors.
}
