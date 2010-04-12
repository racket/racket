#lang scribble/doc
@(require "common.ss"
          (for-label mzlib/unit))

@(begin
  (define-syntax-rule (bind id)
    (begin
     (require (for-label scheme/unit))
     (define id (scheme struct))))
  (bind scheme-struct)
  (define-syntax-rule (bindc id)
    (begin
     (require (for-label scheme/unit))
     (define id (scheme struct/ctc))))
  (bindc scheme-struct/ctc))

@mzlib[#:mode title unit]

The @schememodname[mzlib/unit] library mostly re-provides
@schememodname[scheme/unit], except for @scheme-struct and
@scheme-struct/ctc from @schememodname[scheme/unit].

@defform/subs[(struct id (field-id ...) omit-decl ...)
              ([omit-decl -type
                          -selectors
                          -setters
                          -constructor])]{

A signature form like @scheme-struct from @schememodname[scheme/unit],
but with a different syntax for the options that limit exports.}

@defform/subs[(struct/ctc id ([field-id contract-expr] ...) omit-decl ...)
              ([omit-decl -type
                          -selectors
                          -setters
                          -constructor])]{

A signature form like @scheme-struct/ctc from @schememodname[scheme/unit],
but with a different syntax for the options that limit exports.}

@deftogether[(
@defidform[struct~s]
@defidform[struct~s/ctc]
)]{

The same as @|scheme-struct| and @|scheme-struct/ctc| from
@schememodname[scheme/unit].}

@deftogether[(
@defidform[struct~r]
@defidform[struct~r/ctc]
)]{

Like @scheme[struct~s] and @scheme[struct~s/ctc], but the constructor is
named the same as the type, instead of with  @schemeidfont{make-} prefix.}


