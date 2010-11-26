#lang scribble/doc
@(require scribble/manual
          (for-label file/convertible))

@title[#:tag "convertible"]{Convertible: Data-Conversion Protocol}

@defmodule[file/convertible]

The @schememodname[file/convertible] library provides a protocol to
mediate between providers of data in different possible formats and
consumers of the formats. For example, a datatype that implements
@racket[prop:convertible] might be able to convert itself to a GIF or
PDF stream, in which case it would produce data for
@racket['gif-bytes] or @racket['pdf-bytes] requests.

Any symbol can be used for a conversion request, but the following
should be considered standard:

@itemlist[
 #:style 'compact

 @item{@scheme['text] --- a string for human-readable text}
 @item{@scheme['gif-bytes] --- a byte string containing a GIF image encoding}
 @item{@scheme['png-bytes] --- a byte string containing a PNG image encoding}
 @item{@scheme['ps-bytes] --- a byte string containing a PostScript document}
 @item{@scheme['eps-bytes] --- a byte string containing an Encapsulated PostScript document}
 @item{@scheme['pdf-bytes] --- a byte string containing a PDF document}
]

@defthing[prop:convertible struct-type-property?]{

A property whose value should be a procedure of three arguments. The
procedure is called when a structure with the property is passed to
@racket[convert]; the first argument to the procedure is the
structure, the second argument is a symbol for the requested
conversion, and the third argument is a value to return (typically
@racket[#f] if the conversion is not supported. The procedure's result
depends on the requested conversion.}

@defproc[(convertible? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] supports the conversion protocol,
@racket[#f] otherwise.}

@defproc[(convert [v convertible?] [request symbol?] [default any/c #f])
         any]{


Requests a data conversion from @racket[v], where @racket[request]
indicates the type of requested data and @racket[default] is the value
that the converter should return if it cannot produce data in the
format indicated by @racket[request].}

