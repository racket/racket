#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "bytestrings"]{Bytes and Byte Strings}

A @deftech{byte} is an exact integer between @racket[0] and
@racket[255], inclusive. The @racket[byte?] predicate recognizes
numbers that represent bytes.

@examples[
(byte? 0)
(byte? 256)
]

A @deftech{byte string} is similar to a string---see
@secref["strings"]---but its content is a sequence of bytes
instead of characters. Byte strings can be used in applications that
process pure ASCII instead of Unicode text. The printed form of a
byte string supports such uses in particular, because a byte string
prints like the ASCII decoding of the byte string, but prefixed with a
@litchar{#}. Unprintable ASCII characters or non-ASCII bytes in the
byte string are written with octal notation.

@refdetails/gory["parse-string"]{the syntax of byte strings}

@examples[
#"Apple"
(bytes-ref #"Apple" 0)
(make-bytes 3 65)
(define b (make-bytes 2 0))
b
(bytes-set! b 0 1)
(bytes-set! b 1 255)
b
]

The @racket[display] form of a byte string writes its raw bytes to the
current output port (see @secref["i/o"]). Technically,
@racket[display] of a normal (i.e,. character) string prints the UTF-8
encoding of the string to the current output port, since output is
ultimately defined in terms of bytes; @racket[display] of a byte
string, however, writes the raw bytes with no encoding. Along the same
lines, when this documentation shows output, it technically shows the
UTF-8-decoded form of the output.

@examples[
(display #"Apple")
(eval:alts (code:line (display @#,racketvalfont{"\316\273"})  (code:comment @#,t{same as @racket["\316\273"]}))
           (display "\316\273"))
(code:line (display #"\316\273") (code:comment @#,t{UTF-8 encoding of @elem["\u03BB"]}))
]

For explicitly converting between strings and byte strings, Racket
supports three kinds of encodings directly: UTF-8, Latin-1, and the
current locale's encoding. General facilities for byte-to-byte
conversions (especially to and from UTF-8) fill the gap to support
arbitrary string encodings.

@examples[
(bytes->string/utf-8 #"\316\273")
(bytes->string/latin-1 #"\316\273")
(code:line
 (parameterize ([current-locale "C"])  (code:comment @#,elem{C locale supports ASCII,})
   (bytes->string/locale #"\316\273")) (code:comment @#,elem{only, so...}))
(let ([cvt (bytes-open-converter "cp1253" (code:comment @#,elem{Greek code page})
                                 "UTF-8")]
      [dest (make-bytes 2)])
  (bytes-convert cvt #"\353" 0 1 dest)
  (bytes-close-converter cvt)
  (bytes->string/utf-8 dest))
]

@refdetails["bytestrings"]{byte strings and byte-string procedures}
