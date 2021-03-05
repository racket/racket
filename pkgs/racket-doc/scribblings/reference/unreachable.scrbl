#lang scribble/doc
@(require "mz.rkt" (for-label racket/unreachable racket/unsafe/ops))

@title[#:tag "unreachable"]{Unreachable Expressions}

@defproc[(assert-unreachable) none/c]{

Reports an assertion failure by raising @racket[exn:fail:contract],
which is useful as a safe counterpart to
@racket[unsafe-assert-unreachable].

@history[#:added "8.0.0.11"]}


@section[#:tag "with-unreachable"]{Customized Unreachable Reporting}

@note-lib-only[racket/unreachable]

@history[#:added "8.0.0.11"]

@defform[(with-assert-unreachable
           body ...+)]{

Similar to @racket[(assert-unreachable)], asserts that the
@racket[body] forms should not be reached.

Unless the expression is part of a module that includes
@racket[(#%declare #:unsafe)], then it is equivalent to
@racket[(let-values () body ...+)]. The intent is that the
@racket[body] forms will raise @racket[exn:fail:contract].

When a @racket[with-assert-unreachable] expression is part of a module
with @racket[(#%declare #:unsafe)], then it is equivalent to
@racket[(unsafe-assert-unreachable)].}
