#lang scribble/doc
@(require "common.rkt" (for-label syntax/moddep))

@title[#:tag "modread"]{Reading Module Source Code}

@defmodule[syntax/modread]

@defproc[(with-module-reading-parameterization [thunk (-> any)]) any]{

Calls @racket[thunk] with all reader parameters reset to their default
values.}

@defproc[(check-module-form [stx (or/c syntax? eof-object?)] 
                            [expected-module-sym symbol?]
                            [source-v (or/c string? false/c)])
         (or/c syntax? false/c)]{

Inspects @racket[stx] to check whether evaluating it will declare a
module---at least if @racket[module] is bound in the top-level to
Racket's @racket[module]. The syntax object @racket[stx] can contain a
compiled expression. Also, @racket[stx] can be an end-of-file, on the
grounds that @racket[read-syntax] can produce an end-of-file.

The @racket[expected-module-sym] argument is currently ignored. In
previous versions, the module form @racket[stx] was obliged to declare
a module who name matched @racket[expected-module-sym].

If @racket[stx] can declare a module in an appropriate top-level, then
the @racket[check-module-form] procedure returns a syntax object that
certainly will declare a module (adding explicit context to the
leading @racket[module] if necessary) in any top-level. Otherwise, if
@racket[source-v] is not @racket[#f], a suitable exception is raised
using the @racket[write] form of the source in the message; if
@racket[source-v] is @racket[#f], @racket[#f] is returned.

If @racket[stx] is eof or eof wrapped as a syntax object, then an
error is raised or @racket[#f] is returned.}

