#lang scribble/manual
@(require (for-label racket/base
                     racket/sequence
                     syntax/unsafe/for-transform))

@title{Unsafe @racket[for] Clause Transforms}

@defmodule[syntax/unsafe/for-transform]{
The @racketmodname[syntax/unsafe/for-transform] module provides a helper
function that gives access to the sequence transformers defined by 
@racket[define-sequence-syntax]. This is what the @racket[for] forms use and
enables faster 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{sequence} traversal
than what the sequence interface provides.

The output may use unsafe operations.}

@defproc[(expand-for-clause* [orig-stx syntax?] [clause syntax?]) syntax?]{

Expands a @racket[for] clause of the form @racket[[(x ...) seq-expr]], where
@racket[x] are identifiers, to:

@racketblock[
(([(outer-id ...) outer-expr] ...)
 outer-check
 ([loop-id loop-expr] ...)
 pos-guard
 ([(inner-id ...) inner-expr] ...)
 inner-check
 pre-guard
 post-guard
 (loop-arg ...))]

which can then be spliced into the appropriate iterations. See @racket[:do-in]
for more information.

The result may use unsafe operations.

The first argument @racket[orig-stx] is used only for reporting syntax errors.

@history[#:added "8.10.0.3"]}


@defproc[(expand-for-clause [orig-stx syntax?] [clause syntax?]) syntax?]{

Like @racket[expand-for-clause*], but the result omits a
@racket[inner-check] part:

@racketblock[
(([(outer-id ...) outer-expr] ...)
 outer-check
 ([loop-id loop-expr] ...)
 pos-guard
 ([(inner-id ...) inner-expr] ...)
 pre-guard
 post-guard
 (loop-arg ...))]

If a clause expands to a @racket[inner-check] clauses that is not
ignorable, @racket[expand-for-clause] reports an error. An ignorable
clause is @racket[(void)] or a @racket[begin] form wrapping ignorable
clauses.

}
