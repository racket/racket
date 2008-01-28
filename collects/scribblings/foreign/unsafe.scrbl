#lang scribble/doc
@(require "utils.ss")

@title{Macros for Unsafety}

@defform[(unsafe!)]{

Makes most of the bindings documented in this module available. See
@secref["intro"] for information on why this declaration is required.}


@defform/subs[#:literals (unsafe rename-out)
              (provide* provide-star-spec ...)
              ([provide-star-spec (unsafe id)
                                  (unsafe (rename-out [id external-id]))
                                  provide-spec])]{

Like @scheme[provide], but @scheme[id]s under @scheme[unsafe] are not
actually provided. Instead, they are collected for introduction into
an importing module via a macro created by @scheme[define-unsafer].

Providing users with unsafe operations without using this facility
should be considered a bug in your code.}

@defform[(define-unsafer id)]{

Cooperates with @scheme[provide*] to define @scheme[id] as a
@scheme[unsafe!]-like form that introduces definitions for each
binding provided as @scheme[unsafe].  The @scheme[define-unsafer] form
must occur after all the @scheme[provide*] forms to which it refers.}
