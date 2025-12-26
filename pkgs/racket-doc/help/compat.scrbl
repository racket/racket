#lang scribble/doc
@(require scribble/manual
          (for-label racket
                     help/search))

@(module main-id racket/base
   (require scribble/manual
            (for-label racket
                       help))

   (provide (all-defined-out))

   (define main-send-main-page (racket send-main-page))
   (define main-send-language-family-page (racket send-language-family-page))
   (define main-perform-search (racket main-perform-search)))
@(require 'main-id)

@title{Compatibility Library}

@defmodule[help/search]{
The @racketmodname[help/search] module reexports the bindings of
@racketmodname[help].}

@defthing[send-main-page procedure?]{
  See @|main-send-main-page|.

  @history[#:changed "1.2" @elem{Added @racket[get-doc-open-url] support.}
           #:changed "1.60" @elem{Added the @racket[query-table] argument.}]}

@defthing[send-language-family-page procedure?]{
  See @|main-send-language-family-page|.


  @history[#:added "1.60"]}

@defthing[perform-search procedure?]{
  See @|main-perform-search|.

  @history[#:changed "1.60" @elem{Added the @racket[language-family] argument.}]
}
