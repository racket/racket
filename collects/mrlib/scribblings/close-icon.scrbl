#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/close-icon
                     scheme/gui
                     scheme/runtime-path
                     scheme/include))

@title{Close Icon}

@defmodule[mrlib/close-icon]{The @scheme[close-icon%] class
provides a clickable close button icon.}

@defclass[close-icon% canvas% ()]{
 @defconstructor[([parent (is-a? area-container<%>)]
                  [callback (-> any) void]
                  [horizontal-pad positive-integer? 4]
                  [vertical-pad positive-integer? 4])]{
   The @scheme[callback] is called when the close icon is clicked.
 }
}
