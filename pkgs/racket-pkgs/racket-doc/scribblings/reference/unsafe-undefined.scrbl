#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/unsafe/undefined))

@title[#:tag "unsafe-undefined"]{Unsafe Undefined}

@note-lib[racket/unsafe/undefined]

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

@history[#:added "6.0.0.6"]

@defproc[(unsafe-undefined? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is the constant
@racket[unsafe-undefined], @racket[#f] otherwise.}


@defthing[unsafe-undefined unsafe-undefined?]{

The unsafe ``undefined'' constant.}


@defproc[(check-not-unsafe-undefined [v any/c] [sym symbol?])
         (and/c any/c (not/c unsafe-undefined?))]{

Checks whether @racket[v] is @racket[unsafe-undefined], and raises
@racket[exn:fail:contract:variable] in that case with an error message
along the lines of ``@racket[sym]: variable used before its
definition.''  If @racket[v] is not @racket[unsafe-undefined], then
@racket[v] is returned.}

@defthing[prop:chaperone-unsafe-undefined struct-type-property?]{

A @tech{structure type property} that causes a structure type's
constructor to produce a @tech{chaperone} of an instance where every
access of a field in the structure is checked to prevent returning
@racket[unsafe-undefined].

The property value should be a list of symbols used as field names,
but the list should be in reverse order of the structure's fields.
When a field access would otherwise produce @racket[unsafe-undefined],
the @racket[exn:fail:contract:variable] exception is raised if a field
name is provided by the structure property's value, otherwise the
@racket[exn:fail:contract] exception is raised.}
