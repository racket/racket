#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "quote"]{Quoting: @racket[quote] and @racketvalfont{@literal{'}}}

@refalso["quote"]{@racket[quote]}

The @racket[quote] form produces a constant:

@specform[(#,(racketkeywordfont "quote") datum)]

The syntax of a @racket[datum] is technically specified as anything
that the @racket[read] function parses as a single element. The value
of the @racket[quote] form is the same value that @racket[read] would
produce given @racket[_datum].

The @racket[_datum] can be a symbol, a boolean, a number, a (character
or byte) string, a character, a keyword, an empty list, a pair (or
list) containing more such values, a vector containing more such
values, a hash table containing more such values, or a box containing
another such value.

@examples[
(eval:alts (#,(racketkeywordfont "quote") apple) 'apple)
(eval:alts (#,(racketkeywordfont "quote") #t) #t)
(eval:alts (#,(racketkeywordfont "quote") 42) 42)
(eval:alts (#,(racketkeywordfont "quote") "hello") "hello")
(eval:alts (#,(racketkeywordfont "quote") ()) '())
(eval:alts (#,(racketkeywordfont "quote") ((1 2 3) #2("z" x) . the-end)) '((1 2 3) #2("z" x) . the-end))
(eval:alts (#,(racketkeywordfont "quote") (1 2 #,(racketparenfont ".") (3))) '(1 2 . (3)))
]

As the last example above shows, the @racket[_datum] does not have to
match the normalized printed form of a value. A @racket[_datum] cannot
be a printed representation that starts with @litchar{#<}, so it
cannot be @|void-const|, @|undefined-const|, or a procedure.

The @racket[quote] form is rarely used for a @racket[_datum] that is a
boolean, number, or string by itself, since the printed forms of those
values can already be used as constants. The @racket[quote] form is
more typically used for symbols and lists, which have other meanings
(identifiers, function calls, etc.) when not quoted.

An expression

@specform[(quote @#,racketvarfont{datum})]

is a shorthand for

@racketblock[
(#,(racketkeywordfont "quote") #,(racket _datum))
]

and this shorthand is almost always used instead of
@racket[quote]. The shorthand applies even within the @racket[_datum],
so it can produce a list containing @racket[quote].

@refdetails["parse-quote"]{the @racketvalfont{'} shorthand}

@examples[
'apple
'"hello"
'(1 2 3)
(display '(you can 'me))
]
