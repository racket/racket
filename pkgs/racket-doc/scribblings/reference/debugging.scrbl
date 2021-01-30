#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "debugging"]{Debugging}

Racket's built-in debugging support is limited to context (i.e.,
``stack trace'') information that is printed with an exception. In
some cases, for @tech{BC} implementation of Racket, disabling the
@tech{JIT} compiler can affect context information. For the @tech{CS}
implementation of Racket, setting the @envvar-indexed{PLT_CS_DEBUG} environment
variable causes compilation to record expression-level context
information, instead of just function-level information.

The @racketmodname[errortrace] library supports more consistent
(independent of the compiler) and precise context
information. The @racketmodname[racket/trace] library provides simple
tracing support. Finally, the @seclink[#:doc '(lib
"scribblings/drracket/drracket.scrbl") "top" #:indirect? #t]{DrRacket}
programming environment provides much more debugging support.

@include-section["trace.scrbl"]
