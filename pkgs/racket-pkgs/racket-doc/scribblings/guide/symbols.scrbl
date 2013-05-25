#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "symbols"]{Symbols}

A @deftech{symbol} is an atomic value that prints like an identifier
preceded with @litchar{'}.  An expression that starts with @litchar{'}
and continues with an identifier produces a symbol value.

@examples[
'a
(symbol? 'a)
]

For any sequence of characters, exactly one corresponding symbol is
@defterm{interned}; calling the @racket[string->symbol] procedure, or
@racket[read]ing a syntactic identifier, produces an interned
symbol. Since interned symbols can be cheaply compared with
@racket[eq?] (and thus @racket[eqv?] or @racket[equal?]), they serve
as a convenient values to use for tags and enumerations.

Symbols are case-sensitive. By using a @racketfont{#ci} prefix or in
other ways, the reader can be made to case-fold character sequences to
arrive at a symbol, but the reader preserves case by default.

@examples[
(eq? 'a 'a)
(eq? 'a (string->symbol "a"))
(eq? 'a 'b)
(eq? 'a 'A)
(eval:alts @#,elem{@racketfont{#ci}@racketvalfont{@literal{'A}}} #ci'A)
]

Any string (i.e., any character sequence) can be supplied to
@racket[string->symbol] to obtain the corresponding symbol. For reader
input, any character can appear directly in an identifier, except for
whitespace and the following special characters:

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["{"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;} @litchar{#} @litchar{|} @litchar{\}
}

Actually, @litchar{#} is disallowed only at the beginning of a symbol,
and then only if not followed by @litchar{%}; otherwise, @litchar{#} is
allowed, too. Also, @litchar{.} by itself is not a symbol.

Whitespace or special characters can be included in an identifier by
quoting them with @litchar{|} or @litchar{\}. These quoting
mechanisms are used in the printed form of identifiers that contain
special characters or that might otherwise look like numbers.

@examples[
(string->symbol "one, two")
(string->symbol "6")
]

@refdetails/gory["parse-symbol"]{the syntax of symbols}

The @racket[write] function prints a symbol without a @litchar{'}
prefix. The @racket[display] form of a symbol is the same as the
corresponding string.

@examples[
(write 'Apple)
(display 'Apple)
(write '|6|)
(display '|6|)
]

The @racket[gensym] and @racket[string->uninterned-symbol] procedures
generate fresh @defterm{uninterned} symbols that are not equal
(according to @racket[eq?]) to any previously interned or uninterned
symbol. Uninterned symbols are useful as fresh tags that cannot be
confused with any other value.

@examples[
(define s (gensym))
(eval:alts s 'g42)
(eval:alts (eq? s 'g42) #f)
(eq? 'a (string->uninterned-symbol "a"))
]


@refdetails["symbols"]{symbols}
