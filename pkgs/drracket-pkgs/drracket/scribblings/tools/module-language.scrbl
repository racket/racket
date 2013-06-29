#lang scribble/doc
@(require "common.rkt")
@(tools-title "module-language")

@definterface[drracket:language:module-language<%> ()]{

The only language that implements this interface is DrRacket's ``Use
the language declared in the source'' language.

  @defmethod[(get-users-language-name) string]{
  Returns the name of the language that is declared in the source, as a string.
  }
}

@(tools-include "module-language")
