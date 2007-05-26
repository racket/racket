#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{Strings (Unicode)}

A @defterm{string} is a fixed-length array of
@seclink["characters"]{characters}. It prints using doublequotes,
where doublequote and backslash characters within the string are
escaped with backslashes. Other common string escapes are supported,
incluing @schemefont["\\n"] for a linefeed, @schemefont["\\r"] for a
carriage return, octal escapes using @schemefont["\\"] followed by up
to three octal digits, and hexadimal escapes with @schemefont["\\u"]
(up to four digits).  Unprintable characters in a string normally
shown with @schemefont["\\u"] when the string is printed.

@refdetails["mz:parse-string"]{the syntax of strings}

The @scheme[display] procedure directly writes the characters of a
string to the current output stream, in contrast to the
string-constant syntax used to print a string result.

@examples[
"Apple"
(eval:alts #, @schemevalfont{"\u03BB"} "\u03BB")
(display "Apple")
(display "a \"quoted\" thing")
(display "two\nlines")
(eval:alts (display #, @schemevalfont{"\u03BB"}) (display "\u03BB"))
]

A string can be mutable or immutable; strings written as constant
expressions are immutable, but most other strings are mutable. The
@scheme[string] procedure creates a mutable string given content
characters. The @scheme[string-ref] procedure accesses a character
from a string (with 0-based indexing); the @scheme[string-set!]
procedure changes a character in a mutable string.

@examples[
(string-ref "Apple" 0)
(define s (string #\A #\p #\p #\l #\e))
s
(string-set! s 3 #\u03BB)
s
]

@refdetails["mz:strings"]{strings and string procedures}
