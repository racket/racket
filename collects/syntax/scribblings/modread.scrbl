#lang scribble/doc
@(require "common.ss"
          (for-label syntax/moddep))

@title[#:tag "modread"]{Reading Module Source Code}

@defmodule[syntax/modread]

@defproc[(with-module-reading-parameterization [thunk (-> any)]) any]{

Calls @scheme[thunk] with all reader parameters reset to their default
values.}

@defproc[(check-module-form [stx (or/c syntax? eof-object?)] 
                            [expected-module-sym symbol?]
                            [source-v (or/c string? false/c)])
         (or/c syntax? false/c)]{

Inspects @scheme[stx] to check whether evaluating it will declare a
module named @scheme[expected-module-sym]---at least if @scheme[module] is bound
in the top-level to Racket's @scheme[module]. The syntax object @scheme[stx] can
contain a compiled expression. Also, @scheme[stx] can be an end-of-file, on
the grounds that @scheme[read-syntax] can produce an end-of-file.

If @scheme[stx] can declare a module in an appropriate top-level, then
the @scheme[check-module-form] procedure returns a syntax object that
certainly will declare a module (adding explicit context to the
leading @scheme[module] if necessary) in any top-level. Otherwise, if
@scheme[source-v] is not @scheme[#f], a suitable exception is raised
using the @scheme[write] form of the source in the message; if
@scheme[source-v] is @scheme[#f], @scheme[#f] is returned.

If @scheme[stx] is eof or eof wrapped as a syntax object, then an
error is raised or @scheme[#f] is returned.}

