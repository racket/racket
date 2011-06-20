#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "characters"]{Characters}

A Racket @deftech{character} corresponds to a Unicode @defterm{scalar
value}. Roughly, a scalar value is an unsigned integer whose
representation fits into 21 bits, and that maps to some notion of a
natural-language character or piece of a character. Technically, a
scalar value is a simpler notion than the concept called a
``character'' in the Unicode standard, but it's an approximation that
works well for many purposes. For example, any accented Roman letter
can be represented as a scalar value, as can any common Chinese character.

Although each Racket character corresponds to an integer, the
character datatype is separate from numbers. The
@racket[char->integer] and @racket[integer->char] procedures convert
between scalar-value numbers and the corresponding character.

A printable character normally prints as @litchar{#\} followed
by the represented character. An unprintable character normally prints
as @litchar{#\u} followed by the scalar value as hexadecimal
number. A few characters are printed specially; for example, the space
and linefeed characters print as @racket[#\space] and
@racket[#\newline], respectively.

@refdetails/gory["parse-character"]{the syntax of characters}

@examples[
(integer->char 65)
(char->integer #\A)
#\u03BB
(eval:alts @#,racketvalfont["#\\u03BB"] #\u03BB)
(integer->char 17)
(char->integer #\space)
]

The @racket[display] procedure directly writes a character to the
current output port (see @secref["i/o"]), in contrast to the
character-constant syntax used to print a character result.

@examples[
#\A
(display #\A)
]

Racket provides several classification and conversion procedures on
characters. Beware, however, that conversions on some Unicode
characters work as a human would expect only when they are in a string
(e.g., upcasing ``@elem["\uDF"]'' or downcasing ``@elem["\u03A3"]'').

@examples[
(char-alphabetic? #\A)
(char-numeric? #\0)
(char-whitespace? #\newline)
(char-downcase #\A)
(char-upcase #\uDF)
]

The @racket[char=?] procedure compares two or more characters, and
@racket[char-ci=?] compares characters ignoring case. The
@racket[eqv?] and @racket[equal?] procedures behave the same as
@racket[char=?] on characters; use @racket[char=?] when you want to
more specifically declare that the values being compared are
characters.

@examples[
(char=? #\a #\A)
(char-ci=? #\a #\A)
(eqv? #\a #\A)
]

@refdetails["characters"]{characters and character procedures}
