#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scheme/list
          "guide-utils.ss"

          (for-label scheme/list))

@(define list-eval (make-base-eval))
@(interaction-eval #:eval list-eval (require scheme/list))

@title{Pairs, Lists, and Scheme Syntax}

The @scheme[cons] function actually accepts any two values, not just
a list for the second argument. When the second argument is not
@scheme[empty] and not itself produced by @scheme[cons], the result prints
in a special way. The two values joined with @scheme[cons] are printed
between parentheses, but with a dot (i.e., a period surrounded by
whitespace) in between:

@interaction[(cons 1 2) (cons "banana" "split")]

Thus, a value produced by @scheme[cons] is not always a list. In
general, the result of @scheme[cons] is a @defterm{pair}. The more
traditional name for the @scheme[cons?] function is @scheme[pair?],
and we'll use the traditional name from now on.

The name @scheme[rest] also makes less sense for non-list pairs; the
more traditional names for @scheme[first] and @scheme[rest] are
@scheme[car] and @scheme[cdr], respectively. (Granted, the traditional
names are also nonsense. Just remember that ``a'' comes before ``d,''
and @scheme[cdr] is pronounced ``could-er.'')

@examples[
#:eval list-eval
(car (cons 1 2))
(cdr (cons 1 2))
(pair? empty)
(pair? (cons 1 2))
(pair? (list 1 2 3))
]

@close-eval[list-eval]

Scheme's pair datatype and its relation to lists is essentially a
historical curiosity, along with the dot notation for printing and the
funny names @scheme[car] and @scheme[cdr]. Pairs are deeply wired into
to the culture, specification, and implementation of Scheme, however,
so they survive in the language.

You are perhaps most likely to encounter a non-list pair when making a
mistake, such as accidentally reversing the arguments to
@scheme[cons]:

@interaction[(cons (list 2 3) 1) (cons 1 (list 2 3))]

Non-list pairs are used intentionally, sometimes. For example, the
@scheme[make-immutable-hash] function takes a list of pairs, where the
@scheme[car] of each pair is a key and the @scheme[cdr] is an
arbitrary value.

The only thing more confusing to new Schemers than non-list pairs is
the printing convention for pairs where the second element @italic{is}
a pair, but @italic{is not} a list:

@interaction[(cons 0 (cons 1 2))]

In general, the rule for printing a pair is as follows: use the dot
notation always, but if the dot is immediately followed by an open
parenthesis, then remove the dot, the open parenthesis, and the
matching close parenthesis. Thus, @schemeresultfont{(0 . (1 . 2))}
becomes @schemeresult[(0 1 . 2)], and
@schemeresultfont{(1 . (2 . (3 . ())))} becomes @schemeresult[(1 2 3)].

@;------------------------------------------------------------------------
@section[#:tag "quoting-lists"]{Quoting Pairs and Symbols with @scheme[quote]}

After you see

@interaction[
(list (list 1) (list 2) (list 3))
]

enough times, you'll wish (or you're already wishing) that there was a
way to write just @scheme[((1) (2) (3))] and have it mean the list of
lists that prints as @schemeresult[((1) (2) (3))]. The @scheme[quote]
form does exactly that:

@interaction[
(eval:alts (@#,scheme[quote] ((1) (2) (3))) '((1) (2) (3)))
(eval:alts (@#,scheme[quote] ("red" "green" "blue")) '("red" "green" "blue"))
(eval:alts (@#,scheme[quote] ()) '())
]

The @scheme[quote] form works with the dot notation, too, whether the
quoted form is normalized by the dot-parenthesis elimination rule or
not:

@interaction[
(eval:alts (@#,scheme[quote] (1 . 2)) '(1 . 2))
(eval:alts (@#,scheme[quote] (0 @#,schemeparenfont{.} (1 . 2))) '(0 . (1 . 2)))
]

Naturally, lists of any kind can be nested:

@interaction[
(list (list 1 2 3) 5 (list "a" "b" "c"))
(eval:alts (@#,scheme[quote] ((1 2 3) 5 ("a" "b" "c"))) '((1 2 3) 5 ("a" "b" "c")))
]

If you wrap an identifier with @scheme[quote], then you get output
that looks like an identifier:

@interaction[
(eval:alts (@#,scheme[quote] jane-doe) 'jane-doe)
]

A value that prints like an identifier is a @defterm{symbol}. In the
same way that parenthesized output should not be confused with
expressions, a printed symbol should not be confused with an
identifier. In particular, the symbol @scheme[(@#,scheme[quote]
@#,schemeidfont{map})] has nothing to do with the @schemeidfont{map}
identifier or the predefined function that is bound to
@scheme[map], except that the symbol and the identifier happen
to be made up of the same letters.

Indeed, the intrinsic value of a symbol is nothing more than its
character content. In this sense, symbols and strings are almost the
same thing, and the main difference is how they print. The functions
@scheme[symbol->string] and @scheme[string->symbol] convert between
them.

@examples[
map
(eval:alts (@#,scheme[quote] @#,schemeidfont{map}) 'map)
(eval:alts (symbol? (@#,scheme[quote] @#,schemeidfont{map})) (symbol? 'map))
(symbol? map)
(procedure? map)
(string->symbol "map")
(eval:alts (symbol->string (@#,scheme[quote] @#,schemeidfont{map})) (symbol->string 'map))
]

When @scheme[quote] is used on a parenthesized sequence of
identifiers, it creates a list of symbols:

@interaction[
(eval:alts (@#,scheme[quote] (@#,schemeidfont{road} @#,schemeidfont{map})) '(road map))
(eval:alts (car (@#,scheme[quote] (@#,schemeidfont{road} @#,schemeidfont{map}))) (car '(road map)))
(eval:alts (symbol? (car (@#,scheme[quote] (@#,schemeidfont{road} @#,schemeidfont{map})))) (symbol? (car '(road map))))
]

@;------------------------------------------------------------------------
@section{Abbreviating @scheme[quote] with @schemevalfont{'}}

If @scheme[(@#,scheme[quote] (1 2 3))] still seems like too much
typing, you can abbreviate by just putting @litchar{'} in front of
@scheme[(1 2 3)]:

@interaction[
'(1 2 3)
'road
'((1 2 3) road ("a" "b" "c"))
]

In the documentation, @litchar{'} is printed in green along with the
form after it, since the combination is an expression that is a
constant. In DrScheme, only the @litchar{'} is colored green. DrScheme
is more precisely correct, because the meaning of @scheme[quote] can
vary depending on the context of an expression. In the documentation,
however, we routinely assume that standard bindings are in scope, and
so we paint quoted forms in green for extra clarity.

A @litchar{'} expands to a @scheme[quote] form in quite a literal
way. You can see this if you put a @litchar{'} in front of a form that has a
@litchar{'}:

@interaction[
(eval:alts (car '(@#,schemevalfont{quote} @#,schemevalfont{road})) 'quote)
(car ''road)
]

Beware, however, that the @tech{REPL}'s printer recognizes the symbol
@schemeidfont{quote} when printing output, and then it uses
@schemeidfont{'} in the output:

@interaction[
'road
''road
(eval:alts '(@#,schemevalfont{quote} @#,schemevalfont{road}) ''road)
]

@; FIXME:
@; warning about how "quote" creates constant data, which is subtly
@; different than what "list" creates

@;------------------------------------------------------------------------
@section[#:tag "lists-and-syntax"]{Lists and Scheme Syntax}

Now that you know the truth about pairs and lists, and now that you've
seen @scheme[quote], you're ready to understand the main way in which
we have been simplifying Scheme's true syntax.

The syntax of Scheme is not defined directly in terms of character
streams. Instead, the syntax is determined by two layers:

@itemize[

 @item{a @defterm{read} layer, which turns a sequence of characters
       into lists, symbols, and other constants; and}

 @item{an @defterm{expand} layer, which processes the lists, symbols,
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
(eval:alts (+ 1 . @#,scheme[(2)]) (+ 1 2))
]

This works because @scheme[(+ 1 . @#,scheme[(2)])] is just another
way of writing @scheme[(+ 1 2)]. It is practically never a good idea
to write application expressions using this dot notation; it's just a
consequence of the way Scheme's syntax is defined.

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
nothing to do with the dot notation for non-list pairs. PLT Scheme
programmers use the infix convention sparingly---mostly for asymmetric
binary operators such as @scheme[<] and @scheme[is-a?].
