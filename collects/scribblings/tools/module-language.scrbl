#lang scribble/doc
@(require "common.ss")
@(tools-title "module-language")

@definterface[drracket:language:module-language<%> ()]{

The only language that implements this interface is DrScheme's ``Use the language declared in the source'' language,
i.e., the ``Module'' language.

  @defmethod[(get-users-language-name) string]{
  Returns the name of the language that is declared in the source, as a string.
  }
}

@(tools-include "module-language")
