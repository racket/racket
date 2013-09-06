#lang scribble/manual
@(require (for-label racket/base
                     syntax/quote))

@title{Parsing @racket[for] Bodies}

@defmodule[syntax/for-body]{The @racketmodname[syntax/for-body] module
provides a helper function for @racket[for]-like syntactic forms that
wrap the body of the form while expanding to another @racket[for]-like
form, and the wrapper should apply only after the last
@racket[#:break] or @racket[#:final] clause in the body.}

@defproc[(split-for-body [stx syntax?] [body-stxes syntax?]) syntax?]{

The @racket[body-stxes] argument must have the form
@racket[(_pre-body ... _post-body ...)], and it is rewritten into
@racket[((_pre-body ...) (_post-body ...))] such that
@racket[(_post-body ...)] is as large as possible without containing a
@racket[#:break] or @racket[#:final] clause.

The @racket[stx] argument is used only for reporting syntax errors.

Use @racket[split-for-body] instead of assuming that the last form in
a @racket[for]-like form's body can be wrapped separately. In
particular, the last form might contain definitions that need to be
spliced in the same definition context as earlier forms to create
mutually-recursive definitions.}
