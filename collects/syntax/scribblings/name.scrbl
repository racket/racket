#lang scribble/doc
@(require "common.ss"
          (for-label syntax/name))

@title[#:tag "name"]{Extracting Inferred Names}

@defmodule[syntax/name]

@defproc[(syntax-local-infer-name [stx syntax?]) any/c]{

Similar to @scheme[syntax-local-name] except that @scheme[stx] is
checked for an @scheme['inferred-name] property (which overrides any
inferred name). If neither @scheme[syntax-local-name] nor
@scheme['inferred-name] produce a name, then a name is constructed
from the source-location information in @scheme[stx], if any. If no
name can be constructed, the result is @scheme[#f].}
