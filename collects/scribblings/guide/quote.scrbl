#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "quote"]{Quoting: @scheme[quote] and @schemevalfont{'}}

@refalso["quote"]{@scheme[quote]}

The @scheme[quote] form produces a constant:

@specform[(#,(schemekeywordfont "quote") datum)]

The syntax of a @scheme[datum] is technically specified as anything
that the @scheme[read] function parses as a single element. The value
of the @scheme[quote] form is the same value that @scheme[read] would
produce given @scheme[_datum].

To a good approximation, the resulting value is such that
@scheme[_datum] is the value's printed representation. Thus, it can be
a symbol, a boolean, a number, a (character or byte) string, a
character, a keyword, an empty list, a pair (or list) containing more
such values, a vector containing more such values, a hash table
containing more such values, or a box containing another such value.

@examples[
(eval:alts (#,(schemekeywordfont "quote") apple) 'apple)
(eval:alts (#,(schemekeywordfont "quote") #t) #t)
(eval:alts (#,(schemekeywordfont "quote") 42) 42)
(eval:alts (#,(schemekeywordfont "quote") "hello") "hello")
(eval:alts (#,(schemekeywordfont "quote") ()) '())
(eval:alts (#,(schemekeywordfont "quote") ((1 2 3) #2("z" x) . the-end)) '((1 2 3) #2("z" x) . the-end))
(eval:alts (#,(schemekeywordfont "quote") (1 2 #,(schemeparenfont ".") (3))) '(1 2 . (3)))
]

As the last example above shows, the @scheme[_datum] does not have to
be the normalized printed form of a value. A
@scheme[_datum] cannot be a printed representation that starts with
@litchar{#<}, however, so it cannot be @|void-const|,
@|undefined-const|, or a procedure.

The @scheme[quote] form is rarely used for a @scheme[_datum] that is a
boolean, number, or string by itself, since the printed forms of those
values can already be used as constants. The @scheme[quote] form is
more typically used for symbols and lists, which have other meanings
(identifiers, function calls, etc.) when not quoted.

An expression

@specform[(quote @#,schemevarfont{datum})]

is a shorthand for

@schemeblock[
(#,(schemekeywordfont "quote") #,(scheme _datum))
]

and this shorthand is almost always used instead of
@scheme[quote]. The shorthand applies even within the @scheme[_datum],
so it can produce a list containing @scheme[quote].

@refdetails["parse-quote"]{the @schemevalfont{'} shorthand}

@examples[
'apple
'"hello"
'(1 2 3)
(display '(you can 'me))
]
