#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/unsafe/undefined))

@title[#:tag "unsafe-undefined"]{Unsafe Undefined}

@note-lib-only[racket/unsafe/undefined]

The constant @racket[unsafe-undefined] is used internally as a
placeholder value. For example, it is used by @racket[letrec] as a
value for a variable that has not yet been assigned a value.  Unlike
the @racket[undefined] value exported by @racket[racket/undefined],
however, the @racket[unsafe-undefined] value should not leak as the
result of a safe expression. Expression results that potentially
produce @racket[unsafe-undefined] can be guarded by
@racket[check-not-unsafe-undefined], so that an exception can be
raised instead of producing an @racket[undefined] value.

The @racket[unsafe-undefined] value is always @racket[eq?] to itself.

@history[#:added "6.0.1.2"]

@defthing[unsafe-undefined any/c]{

The unsafe ``undefined'' constant.}


@defproc[(check-not-unsafe-undefined [v any/c] [sym symbol?])
         (and/c any/c (not/c (one-of/c unsafe-undefined)))]{

Checks whether @racket[v] is @racket[unsafe-undefined], and raises
@racket[exn:fail:contract:variable] in that case with an error message
along the lines of ``@racket[sym]: undefined; use before
initialization.''  If @racket[v] is not @racket[unsafe-undefined],
then @racket[v] is returned.}

@defproc[(check-not-unsafe-undefined/assign [v any/c] [sym symbol?])
         (and/c any/c (not/c (one-of/c unsafe-undefined)))]{

The same as @racket[check-not-unsafe-undefined], except that the error
message (if any) is along the lines of ``@racket[sym]: undefined;
assignment before initialization.''}


@defproc[(chaperone-struct-unsafe-undefined [v any/c]) any/c]{

Chaperones @racket[v] if it is a structure (as viewed through some
@tech{inspector}). Every access of a field in the structure is checked
to prevent returning @racket[unsafe-undefined]. Similarly, every
assignment to a field in the structure is checked (unless the check
disabled as described below) to prevent assignment of a field whose
current value is @racket[unsafe-undefined].

When a field access would otherwise produce @racket[unsafe-undefined]
or when a field assignment would replace @racket[unsafe-undefined], the
@racket[exn:fail:contract] exception is raised.

The chaperone's field-assignment check is disabled whenever
@racket[(continuation-mark-set-first #f
prop:chaperone-unsafe-undefined)] returns @racket[unsafe-undefined].
Thus, a field-initializing assignment---one that is intended to replace the
@racket[unsafe-undefined] value of a field---should be wrapped with
@racket[(with-continuation-mark prop:chaperone-unsafe-undefined
unsafe-undefined ....)].}


@defthing[prop:chaperone-unsafe-undefined struct-type-property?]{

A @tech{structure type property} that causes a structure type's
constructor to produce a @tech{chaperone} of an instance
in the same way as @racket[chaperone-struct-unsafe-undefined].

The property value should be a list of symbols used as field names,
but the list should be in reverse order of the structure's fields.
When a field access or assignment would produce or replace
@racket[unsafe-undefined], the @racket[exn:fail:contract:variable]
exception is raised if a field name is provided by the structure
property's value, otherwise the @racket[exn:fail:contract] exception
is raised.}
