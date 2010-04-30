#lang scribble/doc
@(require "web-server.rkt")

@; ------------------------------------------------------------
@title[#:tag "lang/web-cells"]{Stateless Web Cells}
@(require (for-label web-server/lang/web-cells))

@defmodule[web-server/lang/web-cells]{The
@racketmodname[web-server/lang/web-cells] library provides the same
API as @racketmodname[web-server/servlet/web-cells], but in a way
compatible with the Web Language. The one difference is that
@racket[make-web-cell] is syntax, rather than a function.

@deftogether[(
@defproc[(web-cell? [v any/c])
         boolean?]
@defform[(make-web-cell default-expr)]
@defproc[(web-cell-ref [wc web-cell?])
         any/c]
@defproc[(web-cell-shadow [wc web-cell?]
                          [v any/c])
         void]
)]{

See @racketmodname[web-server/servlet/web-cells].}
  
}
