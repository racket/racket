#lang scribble/doc
@(require scribble/manual scribble/eval racket/list "guide-utils.rkt"

          (for-label racket/list
                     (only-in racket/class is-a?)))

@(define list-eval (make-base-eval))
@(interaction-eval #:eval list-eval (require racket/list))

@title{Pairs, Lists, and Racket Syntax}

The @racket[cons] function actually accepts any two values, not just
a list for the second argument. When the second argument is not
@racket[empty] and not itself produced by @racket[cons], the result prints
in a special way. The two values joined with @racket[cons] are printed
between parentheses, but with a dot (i.e., a period surrounded by
whitespace) in between:

@interaction[(cons 1 2) (cons "banana" "split")]

Thus, a value produced by @racket[cons] is not always a list. In
general, the result of @racket[cons] is a @defterm{pair}. The more
traditional name for the @racket[cons?] function is @racket[pair?],
and we'll use the traditional name from now on.

The name @racket[rest] also makes less sense for non-list pairs; the
more traditional names for @racket[first] and @racket[rest] are
@racket[car] and @racket[cdr], respectively. (Granted, the traditional
names are also nonsense. Just remember that ``a'' comes before ``d,''
and @racket[cdr] is pronounced ``could-er.'')

@examples[
#:eval list-eval
(car (cons 1 2))
(cdr (cons 1 2))
(pair? empty)
(pair? (cons 1 2))
(pair? (list 1 2 3))
]

@close-eval[list-eval]

Racket's pair datatype and its relation to lists is essentially a
historical curiosity, along with the dot notation for printing and the
funny names @racket[car] and @racket[cdr]. Pairs are deeply wired into
to the culture, specification, and implementation of Racket, however,
so they survive in the language.

You are perhaps most likely to encounter a non-list pair when making a
mistake, such as accidentally reversing the arguments to
@racket[cons]:

@interaction[(cons (list 2 3) 1) (cons 1 (list 2 3))]

Non-list pairs are used intentionally, sometimes. For example, the
@racket[make-hash] function takes a list of pairs, where the
@racket[car] of each pair is a key and the @racket[cdr] is an
arbitrary value.

The only thing more confusing to new Racketeers than non-list pairs is
the printing convention for pairs where the second element @italic{is}
a pair, but @italic{is not} a list:

@interaction[(cons 0 (cons 1 2))]

In general, the rule for printing a pair is as follows: use the dot
notation always, but if the dot is immediately followed by an open
parenthesis, then remove the dot, the open parenthesis, and the
matching close parenthesis. Thus, @racketresultfont{'(0 . (1 . 2))}
becomes @racketresult['(0 1 . 2)], and
@racketresultfont{'(1 . (2 . (3 . ())))} becomes @racketresult['(1 2 3)].

@;------------------------------------------------------------------------
@section[#:tag "quoting-lists"]{Quoting Pairs and Symbols with @racket[quote]}

A list prints with a quote mark before it, but if an element of a list
is itself a list, then no quote mark is printed for the inner list:

@interaction[
(list (list 1) (list 2 3) (list 4))
]

For nested lists, especially, the @racket[quote] form lets you write a
list as an expression in essentially the same way that the list
prints:

@interaction[
(eval:alts (@#,racket[quote] ("red" "green" "blue")) '("red" "green" "blue"))
(eval:alts (@#,racket[quote] ((1) (2 3) (4))) '((1) (2 3) (4)))
(eval:alts (@#,racket[quote] ()) '())
]

The @racket[quote] form works with the dot notation, too, whether the
quoted form is normalized by the dot-parenthesis elimination rule or
not:

@interaction[
(eval:alts (@#,racket[quote] (1 . 2)) '(1 . 2))
(eval:alts (@#,racket[quote] (0 @#,racketparenfont{.} (1 . 2))) '(0 . (1 . 2)))
]

Naturally, lists of any kind can be nested:

@interaction[
(list (list 1 2 3) 5 (list "a" "b" "c"))
(eval:alts (@#,racket[quote] ((1 2 3) 5 ("a" "b" "c"))) '((1 2 3) 5 ("a" "b" "c")))
]

If you wrap an identifier with @racket[quote], then you get output
that looks like an identifier, but with a @litchar{'} prefix:

@interaction[
(eval:alts (@#,racket[quote] jane-doe) 'jane-doe)
]

A value that prints like a quoted identifier is a @defterm{symbol}. In the
same way that parenthesized output should not be confused with
expressions, a printed symbol should not be confused with an
identifier. In particular, the symbol @racket[(@#,racket[quote]
@#,racketidfont{map})] has nothing to do with the @racketidfont{map}
identifier or the predefined function that is bound to
@racket[map], except that the symbol and the identifier happen
to be made up of the same letters.

Indeed, the intrinsic value of a symbol is nothing more than its
character content. In this sense, symbols and strings are almost the
same thing, and the main difference is how they print. The functions
@racket[symbol->string] and @racket[string->symbol] convert between
them.

@examples[
map
(eval:alts (@#,racket[quote] @#,racketidfont{map}) 'map)
(eval:alts (symbol? (@#,racket[quote] @#,racketidfont{map})) (symbol? 'map))
(symbol? map)
(procedure? map)
(string->symbol "map")
(eval:alts (symbol->string (@#,racket[quote] @#,racketidfont{map})) (symbol->string 'map))
]

In the same way that @racket[quote] for a list automatically applies
itself to nested lists, @racket[quote] on a parenthesized sequence of
identifiers automatically applies itself to the identifiers to create
a list of symbols:

@interaction[
(eval:alts (car (@#,racket[quote] (@#,racketidfont{road} @#,racketidfont{map}))) (car '(road map)))
(eval:alts (symbol? (car (@#,racket[quote] (@#,racketidfont{road} @#,racketidfont{map})))) (symbol? (car '(road map))))
]

When a symbol is inside a list that is printed with
@litchar{'}, the @litchar{'} on the symbol is omitted, since
@litchar{'} is doing the job already:

@interaction[
(eval:alts (@#,racket[quote] (@#,racketidfont{road} @#,racketidfont{map})) '(road map))
]

The @racket[quote] form has no effect on a literal expression such as
a number or string:

@interaction[
(eval:alts (@#,racket[quote] 42) 42)
(eval:alts (@#,racket[quote] "on the record") "on the record")
]

@;------------------------------------------------------------------------
@section{Abbreviating @racket[quote] with @racketvalfont{'}}

As you may have guessed, you can abbreviate a use of
@racket[quote] by just putting @litchar{'} in front of a form to
quote:

@interaction[
'(1 2 3)
'road
'((1 2 3) road ("a" "b" "c"))
]

In the documentation, @litchar{'} within an expression is printed in green along with the
form after it, since the combination is an expression that is a
constant. In DrRacket, only the @litchar{'} is colored green. DrRacket
is more precisely correct, because the meaning of @racket[quote] can
vary depending on the context of an expression. In the documentation,
however, we routinely assume that standard bindings are in scope, and
so we paint quoted forms in green for extra clarity.

A @litchar{'} expands to a @racket[quote] form in quite a literal
way. You can see this if you put a @litchar{'} in front of a form that has a
@litchar{'}:

@interaction[
(car ''road)
(eval:alts (car '(@#,racketvalfont{quote} @#,racketvalfont{road})) 'quote)
]

The @litchar{'} abbreviation works in output as well as input. The
@tech{REPL}'s printer recognizes the symbol @racket['quote] as the
first element of a two-element list when printing output, in which
case it uses @racketidfont{'} to print the output:

@interaction[
(eval:alts (@#,racketvalfont{quote} (@#,racketvalfont{quote} @#,racketvalfont{road})) ''road)
(eval:alts '(@#,racketvalfont{quote} @#,racketvalfont{road}) ''road)
''road
]

@; FIXME:
@; warning about how "quote" creates constant data, which is subtly
@; different than what "list" creates

@;------------------------------------------------------------------------
@section[#:tag "lists-and-syntax"]{Lists and Racket Syntax}

Now that you know the truth about pairs and lists, and now that you've
seen @racket[quote], you're ready to understand the main way in which
we have been simplifying Racket's true syntax.

The syntax of Racket is not defined directly in terms of character
streams. Instead, the syntax is determined by two layers:

@itemize[

 @item{a @deftech{reader} layer, which turns a sequence of characters
       into lists, symbols, and other constants; and}

 @item{an @deftech{expander} layer, which processes the lists, symbols,
       and other constants to parse them as an expression.}

]

The rules for printing and reading go together. For example, a list is
printed with parentheses, and reading a pair of parentheses produces a
list. Similarly, a non-list pair is printed with the dot notation, and
a dot on input effectively runs the dot-notation rules in reverse to
obtain a pair.

One consequence of the read layer for expressions is that you can use
the dot notation in expressions that are not quoted forms:

@interaction[
(eval:alts (+ 1 . @#,racket[(2)]) (+ 1 2))
]

This works because @racket[(+ 1 . @#,racket[(2)])] is just another
way of writing @racket[(+ 1 2)]. It is practically never a good idea
to write application expressions using this dot notation; it's just a
consequence of the way Racket's syntax is defined.

Normally, @litchar{.} is allowed by the reader only with a
parenthesized sequence, and only before the last element of the
sequence. However, a pair of @litchar{.}s can also appear around a
single element in a parenthesized sequence, as long as the element is
not first or last. Such a pair triggers a reader conversion that moves
the element between @litchar{.}s to the front of the list. The
conversion enables a kind of general infix notation:

@interaction[
(1 . < . 2)
'(1 . < . 2)
]

This two-dot convention is non-traditional, and it has essentially
nothing to do with the dot notation for non-list pairs. Racket
programmers use the infix convention sparingly---mostly for asymmetric
binary operators such as @racket[<] and @racket[is-a?].
