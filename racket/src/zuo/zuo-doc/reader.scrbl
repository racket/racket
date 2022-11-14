#lang scribble/manual

@(define ((line chars) . expl)
   (list (hspace 1) (litchar chars) expl))

@(define (litchars s)
   (tabular
     #:sep (hspace 1)
     (list (cons (hspace 1) (map (lambda (c) (litchar (string c))) (string->list s))))))

@title[#:tag "reader"]{Zuo S-Expression Reader}

The Zuo reader recognizes a subset (roughly) of Racket S-expression
notation. The reader works in terms of bytes as characters, not
Unicode. It reads and potentially recurs based on a starting character
sequence after skipping ASCII whitespace characters:

@tabular[
 #:sep @hspace[1]
 (list
  @@line{;}{starts a line comment}
  @@line{#!}{starts a line comment; @litchar{\} at then end of a line to the next}
  @@line{#;}{comments out the next S-expression}
  @@line{(}{starts a pair or list; see @secref["read-list"]}
  @@line{[}{starts a pair or list; see @secref["read-list"]}
  @@line{.}{creates a pair when delimited afterward; see @secref["read-list"]}
  @@line{"}{starts a string; see @secref["read-string"]}
  @@line{#"}{starts a string; see @secref["read-string"]}
  @@line{#t}{starts a boolean; see @secref["read-boolean"]}
  @@line{#f}{starts a boolean; see @secref["read-boolean"]}
  @@line{'}{creates a list with @racket[quote] and the next S-expression}
  @@line{`}{creates a list with @racket[quasiquote] and the next S-expression}
  @@line{,}{creates a list with @racket[unquote] and the next S-expression}
  @@line|{,@}|{creates a list with @racket[unquote-splicing] and the next S-expression})
]

A @litchar{#} followed by any other character is not allowed. Other
starting character sequences either create a number (see
@secref["read-number"]) or symbol (see @secref["read-symbol"]), or
they are disallowed. Any character that is not allowed with a symbol
(see @secref["read-symbol"]) counts as a delimiter.

@section[#:tag "read-number"]{Reading Numbers}

A Zuo number starts optionally @litchar{-} and then one or more
decimal digits. It must be delimited afterward. The resulting integer
must fit into a 64-bit two's complement representation. Any number of
leading @litchar{0}s is allowed.

Zuo does not support floating-point numbers, and it does not allow
@litchar{+} at the beginning of a number. Such sequences will parse as
symbols.

@section[#:tag "read-symbol"]{Reading Symbols}

A symbol can include any ASCII digit, alphabetic character for the
following characters:

@litchars|{~!@#$%^&*-_=+:<>?/.}|

Although @litchar{#} is allowed within a symbol, a symbol cannot start with
@litchar{#}. A sequence that optional starts @litchar{-} with one or
more digits up to a delimited is parsed as a number (see
@secref["read-number"]) instead of a symbol. A delimited @litchar{.}
is not a symbol.

@section[#:tag "read-string"]{Reading Strings}

A string starts @litchar{#"} or @litchar{"} and ends with a matching
@litchar{"}. Any character is allowed in a string, except for a
newline or return character. The following escapes are supported with
the usual meaning: @litchar{\"}, @litchar{\\}, @litchar{\n},
@litchar{\r}, @litchar{\t}, and @litchar{\} followed by one to three
octal digits.

@section[#:tag "read-list"]{Reading Lists}

An @litchar{(} or @litchar{[} starts a list or pair that runs until
the matching @litchar{)} or @litchar{]}, respectively. S-expressions
between the matching pair are the elements of the list or pair. If a
delimited @litchar{.} appears where a list element is expected, then
exactly one S-expressioon must appear afterward within the matching
pair.

@section[#:tag "read-boolean"]{Reading Booleans}

A boolean is @litchar{#t}, @litchar{#f}, @litchar{#true}, or
@litchar{#false}, and it must be followed by a delimiter. Any other
sequence after @litchar{#}, other than a string or line comment, is an
error.
