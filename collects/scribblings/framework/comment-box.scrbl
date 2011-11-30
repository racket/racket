#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Comment Box}

@defclass[comment-box:snip% editor-snip:decorated% (readable-snip<%>)]{
  This snip implements the comment boxes that you see in
  DrRacket.

  @defmethod*[#:mode override (((make-editor) (is-a?/c text%)))]{
    Makes an instance of
    @racketblock[(racket:text-mixin text:keymap%)]}

  @defmethod*[#:mode override (((make-snip) (is-a?/c comment-snip%)))]{
    Returns an instance of the @racket[comment-snip%] class.}

  @defmethod*[#:mode override (((get-corner-bitmap) (is-a?/c bitmap%)))]{
    Returns the semicolon bitmap from the file
    @racketblock[(build-path (collection-path "icons") "semicolon.gif")]}

  @defmethod*[#:mode override (((get-position) (symbols (quote left-top) (quote top-right))))]{
    Returns @racket['left-top]}

  @defmethod*[#:mode override (((get-text) string))]{
    Returns the same string as the super method, but with
    newlines replaced by newline-semicolon-space.}

  @defmethod*[#:mode override (((get-menu) (is-a?/c popup-menu%)))]{
    Returns a menu with a single item to change the box into
    semicolon comments.}
}

@(include-previously-extracted "main-extracts.rkt" #rx"^comment-box:")
