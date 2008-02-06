#lang scribble/doc
@(require "common.ss"
          (for-label mzlib/pconvert
                     mzlib/pconvert-prop))

@mzlib[#:mode title pconvert-prop]

@deftogether[(
@defthing[prop:print-convert-constructor-name property?]
@defproc[(print-convert-named-constructor? [v any/c]) any]
@defproc[(print-convert-constructor-name [v any/c]) any]
)]{

The @scheme[prop:print-convert-constructor-name] property can be given
a symbol value for a structure type. In that case, for
constructor-style print conversion via @scheme[print-convert],
instances of the structure are shown using the symbol as the
constructor name. Otherwise, the constructor name is determined by
prefixing @schemeidfont{make-} onto the result of @scheme[object-name].

The @scheme[print-convert-named-constructor?] predicate recognizes
instances of structure types that have the
@scheme[prop:print-convert-constructor-name] property, and
@scheme[print-convert-constructor-name] extracts the property value.}

