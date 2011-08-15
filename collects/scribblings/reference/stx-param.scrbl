#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/stxparam racket/stxparam-exptime racket/splicing))

@title[#:tag "stxparam"]{Syntax Parameters}

@note-lib-only[racket/stxparam]

@defform[(define-syntax-parameter id expr)]{

Binds @racket[id] as syntax to a @deftech{syntax
parameter}. The @racket[expr] is an expression in the
@tech{transformer environment} that serves as the default value for
the @tech{syntax parameter}. The value is typically obtained by a transformer
using @racket[syntax-parameter-value].

The @racket[id] can be used with @racket[syntax-parameterize]
or @racket[syntax-parameter-value] (in a transformer). If
@racket[expr] produces a procedure of one argument or a
@racket[make-set!-transformer] result, then @racket[id] can be
used as a macro. If @racket[expr] produces a
@racket[rename-transformer] result, then @racket[id] can be
used as a macro that expands to a use of the target identifier, but
@racket[syntax-local-value] of @racket[id] does not produce
the target's value.}

@defform[(syntax-parameterize ([id expr] ...) body-expr ...+)]{

@margin-note/ref{See also @racket[splicing-syntax-parameterize].}

Each @racket[id] must be bound to a @tech{syntax parameter} using
@racket[define-syntax-parameter]. Each @racket[expr] is an expression
in the @tech{transformer environment}. During the expansion of the
@racket[body-expr]s, the value of each @racket[expr] is bound to the
corresponding @racket[id].

If an @racket[expr] produces a procedure of one argument or a
@racket[make-set!-transformer] result, then its @racket[id]
can be used as a macro during the expansion of the
@racket[body-expr]s. If @racket[expr] produces a
@racket[rename-transformer] result, then @racket[id] can be
used as a macro that expands to a use of the target identifier, but
@racket[syntax-local-value] of @racket[id] does not produce
the target's value.}

@; ----------------------------------------------------------------------

@section{Syntax Parameter Inspection}

@defmodule*/no-declare[(racket/stxparam-exptime)]

@declare-exporting[racket/stxparam-exptime racket/stxparam]

@defproc[(syntax-parameter-value [id-stx syntax?]) any]{

This procedure is intended for use in a @tech{transformer
environment}, where @racket[id-stx] is an identifier bound in the
normal environment to a @tech{syntax parameter}. The result is the current
value of the @tech{syntax parameter}, as adjusted by
@racket[syntax-parameterize] form.

This binding is provided @racket[for-syntax] by
@racketmodname[racket/stxparam], since it is normally used in a
transformer. It is provided normally by
@racketmodname[racket/stxparam-exptime].}


@defproc[(make-parameter-rename-transformer [id-stx syntax?]) any]{

This procedure is intended for use in a transformer, where
@racket[id-stx] is an identifier bound to a @tech{syntax parameter}. The
result is a transformer that behaves as @racket[id-stx], but that cannot
be used with @racket[syntax-parameterize] or
@racket[syntax-parameter-value].

Using @racket[make-parameter-rename-transformer] is analogous to
defining a procedure that calls a parameter. Such a procedure can be
exported to others to allow access to the parameter value, but not to
change the parameter value. Similarly,
@racket[make-parameter-rename-transformer] allows a @tech{syntax parameter}
to be used as a macro, but not changed.

The result of @racket[make-parameter-rename-transformer] is not
treated specially by @racket[syntax-local-value], unlike the result
of @racket[make-rename-transformer].

This binding is provided @racket[for-syntax] by
@racketmodname[racket/stxparam], since it is normally used in a
transformer. It is provided normally by
@racketmodname[racket/stxparam-exptime].}
