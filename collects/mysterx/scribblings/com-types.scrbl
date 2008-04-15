#lang scribble/doc
@(require "common.ss")

@title[#:tag "com-types"]{COM Types}

@defproc[(com-object-type [obj com-object?]) com-type?]{

  Returns a type for a COM object.}

@defproc[(com-is-a? [obj com-object?] [type com-type?]) boolean?]{

  Return @scheme[#t] if @scheme[obj] is of the
  type @scheme[type].}

