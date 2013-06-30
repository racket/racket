#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/pconvert
                     mzlib/pconvert-prop))

@mzlib[#:mode title pconvert-prop]


@deftogether[(
@defthing[prop:print-converter property?]
@defproc[(print-converter? [v any/c]) any]
@defproc[(print-converter-proc [v print-converter?]) (any/c (any/c . -> . any/c) . -> . any/c)]
)]{

The @racket[prop:print-converter] property can be given a procedure
value for a structure type. In that case, for constructor-style print
conversion via @racket[print-convert], instances of the structure are
converted by calling the procedure that is the property's value. The
procedure is called with the value to convert and a procedure to
recursively convert nested values. The result should be an
S-expression for the converted value.

The @racket[print-converter?] predicate recognizes instances of
structure types that have the @racket[prop:print-converter] property,
and @racket[print-converter-proc] extracts the property value.}


@deftogether[(
@defthing[prop:print-convert-constructor-name property?]
@defproc[(print-convert-named-constructor? [v any/c]) any]
@defproc[(print-convert-constructor-name [v print-convert-named-constructor?]) any]
)]{

The @racket[prop:print-convert-constructor-name] property can be given
a symbol value for a structure type. In that case, for
constructor-style print conversion via @racket[print-convert],
instances of the structure are shown using the symbol as the
constructor name. 

The @racket[prop:print-converter] property takes precedence over
@racket[prop:print-convert-constructor-name]. If neither is attached
to a structure type, its instances are converted using a constructor
name that is @racketidfont{make-} prefixed onto the result of
@racket[object-name].

The @racket[print-convert-named-constructor?] predicate recognizes
instances of structure types that have the
@racket[prop:print-convert-constructor-name] property, and
@racket[print-convert-constructor-name] extracts the property value.}
