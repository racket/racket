#lang scribble/doc
@(require "common.rkt" (for-label syntax/readerr))

@title[#:tag "readerr"]{Raising @racket[exn:fail:read]}

@defmodule[syntax/readerr]

@defproc[(raise-read-error [msg-string string?] 
                           [source any/c]
                           [line (or/c number? false/c)]
                           [col (or/c number? false/c)]
                           [pos (or/c number? false/c)]
                           [span (or/c number? false/c)]
                           [#:extra-srclocs extra-srclocs (listof srcloc?) '()]) 
         any]{

Creates and raises an @racket[exn:fail:read] exception, using
@racket[msg-string] as the base error message.

Source-location information is added to the error message using the
last five arguments and the @racket[extra-srclocs]
(if the @racket[error-print-source-location]
parameter is set to @racket[#t]). The @racket[source] argument is an
arbitrary value naming the source location---usually a file path
string. Each of the @racket[line], @racket[pos] arguments is
@racket[#f] or a positive exact integer representing the location
within @racket[source-name] (as much as known), @racket[col] is a
non-negative exact integer for the source column (if known), and
@racket[span] is @racket[#f] or a non-negative exact integer for an
item range starting from the indicated position.

The usual location values should point at the beginning of whatever it
is you were reading, and the span usually goes to the point the error
was discovered.}

@defproc[(raise-read-eof-error [msg-string string?] 
                               [source any/c]
                               [line (or/c number? false/c)]
                               [col (or/c number? false/c)]
                               [pos (or/c number? false/c)]
                               [span (or/c number? false/c)]) 
         any]{

Like @racket[raise-read-error], but raises @racket[exn:fail:read:eof]
instead of @racket[exn:fail:read].}
