#lang scribble/doc
@(require "web-server.ss")

@; ------------------------------------------------------------
@title[#:tag "lang/web-cells.ss"]{Stateless Web Cells}
@(require (for-label web-server/lang/web-cells))

@defmodule[web-server/lang/web-cells]{The
@schememodname[web-server/lang/web-cells] library provides the same
API as @schememodname[web-server/servlet/web-cells], but in a way
compatible with the Web Language. The one difference is that
@scheme[make-web-cell] is syntax, rather than a function.

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

See @schememodname[web-server/servlet/web-cells].}
  
}