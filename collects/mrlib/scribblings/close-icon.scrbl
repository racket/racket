#lang scribble/doc
@(require "common.rkt"
          (for-label mrlib/close-icon
                     scheme/gui scheme/runtime-path scheme/include))

@title{Close Icon}

@defmodule[mrlib/close-icon]{The @racket[close-icon%] class
provides a clickable close button icon.}

@defclass[close-icon% canvas% ()]{
 @defconstructor[([parent (is-a? area-container<%>)]
                  [callback (-> any) void]
                  [bg-color (or/c #f string (is-a?/c color%)) #f]
                  [horizontal-pad positive-integer? 4]
                  [vertical-pad positive-integer? 4])]{
   The @racket[callback] is called when the close icon is clicked.

   If @racket[bg-color] is specified, it is used as the background color of the icon.
 }
}
