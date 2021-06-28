#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "stxcerts"]{Syntax Taints}

@guideintro["stx-certs"]{syntax taints}

A @deftech{tainted} identifier is rejected by the macro expander for
use as either a binding or expression. If a syntax object @racket[_stx] is
@tech{tainted}, then any syntax object in the result of
@racket[(syntax-e _stx)] is @tech{tainted}, and @racket[datum->syntax]
with @racket[_stx] as its first argument produces a @tech{tainted}
syntax object. Any syntax object in the result of @racket[(syntax-property _stx _key)]
is also tainted if it is in a position within the value that would be
reached by @racket[datum->syntax]'s conversion. Taints cannot be removed.

A syntax object is tainted when it is included in an exception by the
macro expander or when it is produced by a function like
@racket[expand] using a @tech{code inspector} that is not the original
code inspector. The function @racket[syntax-taint] also returns a
tainted syntax object.

Previous versions of Racket included a notion of @defterm{arming} and
@defterm{disarming} syntax to trigger taints or avoid taints. That
indirection is no longer supported, and the operations
@racket[syntax-arm], @racket[syntax-disarm], @racket[syntax-rearm], and
@racket[syntax-protect] now have no effect on their arguments. Along
similar lines, the syntax properties (see @secref["stxprops"])
@indexed-racket['taint-mode] and @indexed-racket['certify-mode] were
formerly used to control syntax arming and are no longer specifically
recognized by the macro expander.

@defproc[(syntax-tainted? [stx syntax?]) boolean?]{

Returns @racket[#t] if @racket[stx] is @tech{tainted}, @racket[#f]
otherwise.}


@defproc[(syntax-arm [stx syntax?]
                     [inspector (or/c inspector? #f) #f]
                     [use-mode? any/c #f])
         syntax?]{

Returns @racket[stx].

@history[#:changed "8.2.0.4" @elem{Changed to just return @racket[stx] instead
                                   of returning ``armed'' syntax.}]}


@defproc[(syntax-protect [stx syntax?]) syntax?]{

Returns @racket[stx].

@history[#:changed "8.2.0.4" @elem{Changed to just return @racket[stx] instead
                                   of returning ``armed'' syntax.}]}


@defproc[(syntax-disarm [stx syntax?]
                        [inspector (or/c inspector? #f)])
         syntax?]{

Returns @racket[stx].

@history[#:changed "8.2.0.4" @elem{Changed to just return @racket[stx] instead
                                   of potentially ``disarming'' syntax.}]}


@defproc[(syntax-rearm [stx syntax?]
                       [from-stx syntax?]
                       [use-mode? any/c #f])
         syntax?]{

Returns @racket[stx].

@history[#:changed "8.2.0.4" @elem{Changed to just return @racket[stx] instead
                                   of potentially ``arming'' syntax.}]}


@defproc[(syntax-taint [stx syntax?]) syntax?]{

Returns @tech{tainted} version of @racket[stx], which is @racket[stx]
if it is already tainted.}
