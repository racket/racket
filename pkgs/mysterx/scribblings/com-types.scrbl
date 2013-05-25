#lang scribble/doc
@(require "common.rkt")

@title[#:tag "com-types"]{COM Types}

In the result of a function like @racket[com-method-type], a type
@indexed-racket['mx-any] standards for a character, real number,
string, boolean, COM currency (as in @racket[com-currency?]), COM date
(as in @racket[com-date?]), COM scode value (as in
@racket[com-scode?]), COM IUnknown value (as in
@racket[com-iunknown?], or COM object (as in @racket[com-object?]).}

@defproc[(com-object-type [obj com-object?]) com-type?]{

  Returns a type for a COM object.}

@defproc[(com-is-a? [obj com-object?] [type com-type?]) boolean?]{

  Return @racket[#t] if @racket[obj] is of the
  type @racket[type].}

@defproc[(com-currency? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is a COM currency value,
  @racket[#f] otherwise.}

@defproc[(com-currency->number [curr com-currency?]) real?]{

  Returns a number for @racket[curr].}

@defproc[(number->com-currency [n real?]) com-currency?]{

  Converts a number to a COM currency value. A currency value is
  repsented with a 64-bit two's-complement integer, though @racket[n]
  may contain decimal digits.  If @racket[n] is too large, an
  exception is raised.}
  
@defproc[(com-date? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is a COM date value, @racket[#f]
  otherwise.}

@defproc[(com-date->date [d com-date?]) date?]{

  Converts a COM date to an instance of the @racket[date] structure
  type. In the result, the @racket[dst?] field is always @racket[#f],
  and the @racket[time-zone-offset] field is @racket[0].}

@defproc[(date->com-date [d date?]) com-date?]{

  Converts a @racket[date] instance to a COM date value.}
  
@defproc[(com-scode? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is a COM scode value, @racket[#f]
  otherwise.}

@defproc[(com-scode->number [sc com-scode?]) integer?]{

  Converts a COM scode value to an integer.}

@defproc[(number->com-scode [n integer?]) com-scode?]{

  Converts a number to a COM scode value.  The number must be
  representable as a 32-bit two's-complement number, otherwise an
  exception is raised.}

@defproc[(com-iunknown? [v any/c]) boolean?]{

  Returns @racket[#t] if @racket[v] is a COM IUnknown value,
  @racket[#f] otherwise.}

@defthing[com-omit any/c]{

Used with @racket[com-invoke] to represent an argument that is not
provided.}
