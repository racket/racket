#lang scribble/doc
@(require scribble/manual
          (for-label file/convertible racket/base racket/contract))

@title[#:tag "convertible"]{Convertible: Data-Conversion Protocol}

@defmodule[file/convertible]

The @racketmodname[file/convertible] library provides a protocol to
mediate between providers of data in different possible formats and
consumers of the formats. For example, a datatype that implements
@racket[prop:convertible] might be able to convert itself to a GIF or
PDF stream, in which case it would produce data for
@racket['gif-bytes] or @racket['pdf-bytes] requests.

Any symbol can be used for a conversion request, but the following
should be considered standard:

@itemlist[
 #:style 'compact

 @item{@racket['text] --- a string for human-readable text}
 @item{@racket['gif-bytes] --- a byte string containing a GIF image encoding}
 @item{@racket['png-bytes] --- a byte string containing a PNG image encoding}
 @item{@racket['png-bytes+bounds] --- a list containing a byte string and four numbers; 
        the byte string contains a PNG document, and the four numbers
        are sizing information for the image: the width, height,
        descent (included in the height), and extra vertical top space
        (included in the height), in that order}
 @item{@racket['png-bytes+bounds8] --- a list containing a byte string
        and eight numbers; like @racket['png-bytes+bounds], but where
        the image encoded that is in the byte string can be padded in
        each direction (to allow the drawn region to extend beyond
        it's ``bounding box''), where the extra four numbers in the
        list specify the amount of padding that was added to the
        image: left, right, top, and bottom}
 @item{@racket['png@2x-bytes] --- like @racket['png-bytes], but for an
        image that is intended for drawing at @racket[1/2] scale}
 @item{@racket['png@2x-bytes+bounds] --- like
        @racket['png-bytes+bounds], but for an image that is intended
        for drawing at @racket[1/2] scale, where the numbers in the result
        list are already scaled (e.g, the byte string encodes an image that
        is twice as wide as the first number in the resulting list)}
 @item{@racket['png@2x-bytes+bounds8] --- like @racket['png-bytes+bounds8],
        but but for an image that is intended for drawing at
        @racket[1/2] scale, and where the numbers in the result
        list are already scaled}
 @item{@racket['svg-bytes] --- a byte string containing a SVG image encoding}
 @item{@racket['svg-bytes+bounds] --- like @racket['png-bytes+bounds], but
        for an SVG image}
 @item{@racket['svg-bytes+bounds8] --- like @racket['png-bytes+bounds8], but
        for an SVG image}
 @item{@racket['ps-bytes] --- a byte string containing a PostScript document}
 @item{@racket['eps-bytes] --- a byte string containing an Encapsulated PostScript
        document}
 @item{@racket['eps-bytes+bounds] --- like @racket['png-bytes+bounds], but,
        but for an Encapsulated PostScript document}
 @item{@racket['eps-bytes+bounds8] --- like @racket['png-bytes+bounds8], but,
        but for an Encapsulated PostScript document}
 @item{@racket['pdf-bytes] --- a byte string containing a PDF document}
 @item{@racket['pdf-bytes+bounds] --- like @racket['png-bytes+bounds], but,
        but for an PDF document}
 @item{@racket['pdf-bytes+bounds8] --- like @racket['png-bytes+bounds8], but,
        but for an PDF document}
]

@defthing[prop:convertible 
          (struct-type-property/c
           (->i ([v convertible?] [request symbol?] [default default/c])
                [result
                 (case request
                   [(text)
                    (or/c string? default/c)]
                   [(gif-bytes
                     png-bytes
                     png@2x-bytes
                     ps-bytes
                     eps-bytes
                     pdf-bytes
                     svg-bytes)
                    (or/c bytes? default/c)]
                   [(png-bytes+bounds
                     png@2x-bytes+bounds
                     eps-bytes+bounds
                     pdf-bytes+bounds)
                    (or/c (list/c bytes?
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?)))
                          default/c)]
                   [(png-bytes+bounds8
                     png@2x-bytes+bounds8
                     eps-bytes+bounds8
                     pdf-bytes+bounds8)
                    (or/c (list/c bytes?
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?)))
                          default/c)]
                   [else (or/c opaque-default/c any/c)])]))]{

A property whose value is invoked by @racket[convert].

The @racket[_v] argument to the procedure is the
structure, the @racket[_request] argument is a symbol for the requested
conversion, and the @racket[_default] argument is a value to return (typically
@racket[#f] if the conversion is not supported). The procedure's result
depends on the requested conversion, as above.

The @racket[default/c] contract is one generated by @racket[new-α/c].}

@defproc[(convertible? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] supports the conversion protocol,
@racket[#f] otherwise.}

@defproc[(convert [v convertible?] [request symbol?] [default any/c #f])
         (case request
           [(text)
            (or/c string? default/c)]
           [(gif-bytes 
             png-bytes
             png@2x-bytes
             ps-bytes
             eps-bytes
             pdf-bytes
             svg-bytes)
            (or/c bytes? default/c)]
           [(png-bytes+bounds
             png@2x-bytes+bounds
             eps-bytes+bounds
             pdf-bytes+bounds)
            (or/c (list/c bytes?
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?)))
                  default/c)]
           [(png-bytes+bounds8
             png@2x-bytes+bounds8
             eps-bytes+bounds8
             pdf-bytes+bounds8)
            (or/c (list/c bytes?
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?))
                          (and/c real? (not/c negative?)))
                  default/c)]
           [else (or/c opaque-default/c any/c)])]{

Requests a data conversion from @racket[v], where @racket[request]
indicates the type of requested data and @racket[default] is the value
that the converter should return if it cannot produce data in the
format indicated by @racket[request].

The @racket[default/c] contract is one created by @racket[new-α/c]
and it guarantees that the result of @racket[convert] is the given
default argument (or @racket[#f] if one is not supplied).}
