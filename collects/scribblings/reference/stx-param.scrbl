#lang scribble/doc
@(require "mz.ss"
          (for-label racket/stxparam
                     racket/stxparam-exptime
                     racket/splicing))

@title[#:tag "stxparam"]{Syntax Parameters}

@note-lib-only[racket/stxparam]

@defform[(define-syntax-parameter id expr)]{

Binds @scheme[id] as syntax to a @deftech{syntax
parameter}. The @scheme[expr] is an expression in the
@tech{transformer environment} that serves as the default value for
the @tech{syntax parameter}. The value is typically obtained by a transformer
using @scheme[syntax-parameter-value].

The @scheme[id] can be used with @scheme[syntax-parameterize]
or @scheme[syntax-parameter-value] (in a transformer). If
@scheme[expr] produces a procedure of one argument or a
@scheme[make-set!-transformer] result, then @scheme[id] can be
used as a macro. If @scheme[expr] produces a
@scheme[rename-transformer] result, then @scheme[id] can be
used as a macro that expands to a use of the target identifier, but
@scheme[syntax-local-value] of @scheme[id] does not produce
the target's value.}

@defform[(syntax-parameterize ((id expr) ...) body-expr ...+)]{

@margin-note/ref{See also @scheme[splicing-syntax-parameterize].}

Each @scheme[id] must be bound to a @tech{syntax parameter} using
@scheme[define-syntax-parameter]. Each @scheme[expr] is an expression
in the @tech{transformer environment}. During the expansion of the
@scheme[body-expr]s, the value of each @scheme[expr] is bound to the
corresponding @scheme[id].

If an @scheme[expr] produces a procedure of one argument or a
@scheme[make-set!-transformer] result, then its @scheme[id]
can be used as a macro during the expansion of the
@scheme[body-expr]s. If @scheme[expr] produces a
@scheme[rename-transformer] result, then @scheme[id] can be
used as a macro that expands to a use of the target identifier, but
@scheme[syntax-local-value] of @scheme[id] does not produce
the target's value.}

@; ----------------------------------------------------------------------

@section{Syntax Parameter Inspection}

@defmodule*/no-declare[(racket/stxparam-exptime)]

@declare-exporting[racket/stxparam-exptime racket/stxparam]

@defproc[(syntax-parameter-value [id-stx syntax?]) any]{

This procedure is intended for use in a @tech{transformer
environment}, where @scheme[id-stx] is an identifier bound in the
normal environment to a @tech{syntax parameter}. The result is the current
value of the @tech{syntax parameter}, as adjusted by
@scheme[syntax-parameterize] form.

This binding is provided @scheme[for-syntax] by
@schememodname[racket/stxparam], since it is normally used in a
transformer. It is provided normally by
@schememodname[racket/stxparam-exptime].}


@defproc[(make-parameter-rename-transformer [id-stx syntax?]) any]{

This procedure is intended for use in a transformer, where
@scheme[id-stx] is an identifier bound to a @tech{syntax parameter}. The
result is transformer that behaves as @scheme[id-stx], but that cannot
be used with @scheme[syntax-parameterize] or
@scheme[syntax-parameter-value].

Using @scheme[make-parameter-rename-transformer] is analogous to
defining a procedure that calls a parameter. Such a procedure can be
exported to others to allow access to the parameter value, but not to
change the parameter value. Similarly,
@scheme[make-parameter-rename-transformer] allows a @tech{syntax parameter}
to used as a macro, but not changed.

The result of @scheme[make-parameter-rename-transformer] is not
treated specially by @scheme[syntax-local-value], unlike the result
of @scheme[make-rename-transformer].

This binding is provided @scheme[for-syntax] by
@schememodname[racket/stxparam], since it is normally used in a
transformer. It is provided normally by
@schememodname[racket/stxparam-exptime].}
