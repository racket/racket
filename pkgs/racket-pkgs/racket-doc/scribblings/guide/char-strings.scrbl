#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "strings"]{Strings (Unicode)}

A @deftech{string} is a fixed-length array of
@seclink["characters"]{characters}. It prints using doublequotes,
where doublequote and backslash characters within the string are
escaped with backslashes. Other common string escapes are supported,
including @litchar{\n} for a linefeed, @litchar{\r} for a
carriage return, octal escapes using @litchar{\} followed by up
to three octal digits, and hexadecimal escapes with @litchar{\u}
(up to four digits).  Unprintable characters in a string are normally
shown with @litchar{\u} when the string is printed.

@refdetails/gory["parse-string"]{the syntax of strings}

The @racket[display] procedure directly writes the characters of a
string to the current output port (see @secref["i/o"]), in contrast
to the string-constant syntax used to print a string result.

@examples[
"Apple"
(eval:alts @#,racketvalfont{"\u03BB"} "\u03BB")
(display "Apple")
(display "a \"quoted\" thing")
(display "two\nlines")
(eval:alts (display @#,racketvalfont{"\u03BB"}) (display "\u03BB"))
]

A string can be mutable or immutable; strings written directly as
expressions are immutable, but most other strings are mutable. The
@racket[make-string] procedure creates a mutable string given a length
and optional fill character. The @racket[string-ref] procedure
accesses a character from a string (with 0-based indexing); the
@racket[string-set!]  procedure changes a character in a mutable
string.

@examples[
(string-ref "Apple" 0)
(define s (make-string 5 #\.))
s
(string-set! s 2 #\u03BB)
s
]

String ordering and case operations are generally
@defterm{locale-independent}; that is, they work the same for all
users. A few @defterm{locale-dependent} operations are provided that
allow the way that strings are case-folded and sorted to depend on the
end-user's locale. If you're sorting strings, for example, use
@racket[string<?] or @racket[string-ci<?] if the sort result should be
consistent across machines and users, but use @racket[string-locale<?]
or @racket[string-locale-ci<?] if the sort is purely to order strings
for an end user.

@examples[
(string<? "apple" "Banana")
(string-ci<? "apple" "Banana")
(string-upcase "Stra\xDFe")
(parameterize ([current-locale "C"])
  (string-locale-upcase "Stra\xDFe"))
]

For working with plain ASCII, working with raw bytes, or
encoding/decoding Unicode strings as bytes, use
@seclink["bytestrings"]{byte strings}.

@refdetails["strings"]{strings and string procedures}
