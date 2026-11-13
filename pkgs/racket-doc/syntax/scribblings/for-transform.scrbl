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

Expands a @racket[for] clause of the form @racket[[(_x ...) _seq-expr]], where
@racket[_x]s are identifiers, to:

@racketblock[
(([(_outer-id ...) _outer-expr] ...)
 _outer-defn-or-expr
 ([_loop-id _loop-expr] ...)
 _pos-guard
 ([(_inner-id ...) _inner-expr] ...)
 _inner-defn-or-expr
 _pre-guard
 _post-guard
 (_loop-arg ...))]

which can then be spliced into the appropriate iterations. See @racket[:do-in]
for more information.

The result may use unsafe operations.

The first argument @racket[orig-stx] is used only for reporting syntax errors.

@history[#:added "8.10.0.3"]}


@defproc[(expand-for-clause [orig-stx syntax?] [clause syntax?]) syntax?]{

Like @racket[expand-for-clause*], but the result omits a
@racket[_inner-defn-or-expr] part:

@racketblock[
(([(_outer-id ...) _outer-expr] ...)
 _outer-defn-or-expr
 ([_loop-id _loop-expr] ...)
 _pos-guard
 ([(_inner-id ...) _inner-expr] ...)
 _pre-guard
 _post-guard
 (_loop-arg ...))]

If a clause expands to a @racket[_inner-defn-or-expr] clauses that is not
ignorable, @racket[expand-for-clause] reports an error. An ignorable
clause is @racket[(void)] or a @racket[begin] form wrapping ignorable
clauses.

}
