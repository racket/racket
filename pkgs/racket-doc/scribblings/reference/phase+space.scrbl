#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/phase+space))

@title[#:tag "phase+space"]{Phase and Space Utilities}

@note-lib-only[racket/phase+space]

The @racketmodname[racket/phase+space] library provides functions for
manipulating combined representations of @tech{phase levels} and
@tech{binding spaces}, particularly as used for @tech{require
transformers} and @tech{provide transformers}.

When @racket[identifier-binding] (and related functions, like
@racket[identifier-transformer-binding]),
@racket[syntax-local-module-exports],
@racket[syntax-local-module-required-identifiers],
@racket[module-compiled-exports], or @racket[module->exports] produces
a phase--space combination (or phase--space shift combination), then
two such values that are @racket[equal?] will be @racket[eqv?].

@history[#:added "8.2.0.3"]

@defproc[(phase? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a valid representation of a
@tech{phase level}, @racket[#f] otherwise. A valid representation
is either an exact integer representing a numbered
phase level or @racket[#f] representing the @tech{label phase level}.}

@defproc[(space? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a valid representation of a
@tech{binding space}, @racket[#f] otherwise. A valid representation
is either an @tech{interned} symbol representing the space whose
scope is accessed via @racket[make-interned-syntax-introducer], or
@racket[#f] representing the default binding space.}

@defproc[(phase+space? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a valid representation of a
@tech{phase level} and @tech{binding space} combination,
@racket[#f] otherwise. The possible
representations are as follows:

@itemlist[

 @item{a phase (in the sense of @racket[phase?]) by itself, which
       represents that phase plus the default binding space}

 @item{a pair whose @racket[car] is a phase and whose @racket[cdr] is
       a non-@racket[#f] space (in the sense of @racket[space?])}
       
]}


@defproc[(phase+space [phase phase?] [space space?]) phase+space?]{

Returns a value to represent the combination of @racket[phase] and
@racket[space].}


@deftogether[(
@defproc[(phase+space-phase [p+s phase+space?]) phase?]
@defproc[(phase+space-space [p+s phase+space?]) phase?]
)]{

Extracts the @tech{phase level} or @tech{binding space} component from
a combination.}


@defproc[(phase+space-shift? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a valid representation of a
@tech{phase level} shift and @tech{binding space} shift combination,
@racket[#f] otherwise. A
shift can be applied to a combination of a phase level and binding
space using @racket[phase+shift+]. The possible representations of a
shift are as follows:

@itemlist[

 @item{exact integer --- represents an amount to shift a phase level
                         and no change to the binding space}

 @item{@racket[#f] --- represents a shift to the @tech{label phase level}
                       and no change to the binding space}

 @item{a pair whose @racket[car] is an exact integer or @racket[#f],
       and whose @racket[cdr] is a space (in the sense of
       @racket[space?]) --- represents a phase level shift in the
       @racket[car] and a change to the binding space that is in the
       @racket[cdr]}

]}

@defproc[(phase+space+ [p+s phase+space?] [shift phase+space-shift?])
         phase+space?]{

Applies @racket[shift] to @racket[p+s] to produce a new combination of
@tech{phase level} and @tech{binding space}.}

@defproc[(phase+space-shift+ [shift phase+space?] [additional-shift phase+space-shift?])
         phase+space-shift?]{

Composes @racket[shift] and @racket[additional-shift] to produce a new
shift that behaves the same as applying @racket[shift] followed by
@racket[additional-shift].}
