#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "debugging"]{Debugging}

Racket's built-in debugging support is limited to context (i.e.,
``stack trace'') information that is printed with an exception. In
some cases, disabling the @tech{JIT} compiler can affect context
information. The @racketmodname[errortrace] library supports more
consistent (independent of the @tech{JIT} compiler) and precise context
information.  The @racketmodname[racket/trace] library provides simple
tracing support. Finally, the @seclink[#:doc '(lib
"scribblings/drracket/drracket.scrbl") "top"]{DrRacket} programming environment
provides much more debugging support.

@include-section["trace.scrbl"]
