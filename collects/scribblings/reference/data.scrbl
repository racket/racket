#lang scribble/doc
@(require "mz.ss")

@title[#:style 'toc #:tag "data"]{Datatypes}

Each pre-defined datatype comes with a set of procedures for
manipulating instances of the datatype.

@local-table-of-contents[#:style 'immediate-only]

@; ------------------------------------------------------------
@include-section["booleans.scrbl"]

@; ------------------------------------------------------------
@include-section["numbers.scrbl"]

@; ------------------------------------------------------------
@include-section["strings.scrbl"]

@; ------------------------------------------------------------
@include-section["bytes.scrbl"]

@; ------------------------------------------------------------
@include-section["chars.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "symbols"]{Symbols}

@guideintro["symbols"]{symbols}

@section-index["symbols" "generating"]
@section-index["symbols" "unique"]

A @deftech{symbol} is like an immutable string, but symbols are
normally @deftech{interned}, so that two symbols with the same
character content are normally @scheme[eq?]. All symbols produced by
the default reader (see @secref["parse-symbol"]) are interned.

The two procedures @scheme[string->uninterned-symbol] and
@scheme[gensym] generate @deftech{uninterned} symbols, i.e., symbols
that are not @scheme[eq?], @scheme[eqv?], or @scheme[equal?] to any
other symbol, although they may print the same as other symbols.

Regular (interned) symbols are only weakly held by the internal symbol
table. This weakness can never affect the result of an @scheme[eq?],
@scheme[eqv?], or @scheme[equal?] test, but a symbol may disappear
when placed into a weak box (see @secref["weakbox"]) used as the key
in a weak @tech{hash table} (see @secref["hashtables"]), or used as an
ephemeron key (see @secref["ephemerons"]).

@defproc[(symbol? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
 a symbol, @scheme[#f] otherwise.}

@examples[(symbol? 'Apple) (symbol? 10)]


@defproc[(symbol->string [sym symbol?]) symbol?]{Returns a freshly
 allocated mutable string whose characters are the same as in
 @scheme[sym].}

@examples[(symbol->string 'Apple)]


@defproc[(string->symbol [str string?]) symbol?]{Returns an
 @tech{interned} symbol whose characters are the same as in
 @scheme[str].}

@examples[(string->symbol "Apple") (string->symbol "1")]


@defproc[(string->uninterned-symbol [str string?]) symbol?]{Like
 @scheme[(string->symbol str)], but the resulting symbol is a new
 @tech{uninterned} symbol. Calling @scheme[string->uninterned-symbol]
 twice with the same @scheme[str] returns two distinct symbols.}

@examples[(string->uninterned-symbol "Apple") (eq? 'a (string->uninterned-symbol "a"))]


@defproc[(gensym [base (or/c string? symbol?) "g"]) symbol?]{Returns a
 new @tech{uninterned} symbol with an automatically-generated name. The
 optional @scheme[base] argument is a prefix symbol or string.}

@examples[(gensym "apple")]

@; ------------------------------------------------------------
@include-section["regexps.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "keywords"]{Keywords}

@guideintro["keywords"]{keywords}

A @deftech{keyword} is like an @tech{interned} symbol, but its printed
form starts with @litchar{#:}, and a keyword cannot be used as an
identifier. Furthermore, a keyword by itself is not a valid
expression, though a keyword can be @scheme[quote]d to form an
expression that produces the symbol.

Two keywords are @scheme[eq?] if and only if they print the same.

Like symbols, keywords are only weakly held by the internal keyword
table; see @secref["symbols"] for more information.

@defproc[(keyword? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a keyword, @scheme[#f] otherwise.}

@defproc[(keyword->string [keyword keyword?]) string?]{

Returns a string for the @scheme[display]ed form of @scheme[keyword],
not including the leading @litchar{#:}.}

@defproc[(string->keyword [str string?]) keyword]{

Returns a keyword whose @scheme[display]ed form is the same as that of
@scheme[str], but with a leading @litchar{#:}.}

@defproc[(keyword<? [a-keyword keyword?][b-keyword keyword?] ...+) boolean?]{

Returns @scheme[#t] if the arguments are sorted, where the comparison
for each pair of keywords is the same as using
@scheme[keyword->string] and @scheme[string<?].}

@; ----------------------------------------------------------------------
@include-section["pairs.scrbl"]

@; ----------------------------------------------------------------------
@include-section["mpairs.scrbl"]

@; ----------------------------------------------------------------------
@include-section["vectors.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "boxes"]{Boxes}

@guideintro["boxes"]{boxes}

A @deftech{box} is like a single-element vector, normally used as
minimal mutable storage.

@defproc[(box? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a box, @scheme[#f] otherwise.}


@defproc[(box [v any/c]) box?]{

Returns a new mutable box that contains @scheme[v].}


@defproc[(box-immutable [v any/c]) (and/c box? immutable?)]{

Returns a new immutable box that contains @scheme[v].}


@defproc[(unbox [box box?]) any/c]{

Returns the content of @scheme[box].}


For any @scheme[v], @scheme[(unbox (box v))] returns @scheme[v].


@defproc[(set-box! [box (and/c box? (not/c immutable?))]
                   [v any/c]) void?]{

Sets the content of @scheme[box] to @scheme[v].}

@; ----------------------------------------------------------------------
@include-section["hashes.scrbl"]

@; ----------------------------------------------------------------------
@include-section["sequences.scrbl"]

@; ----------------------------------------------------------------------
@include-section["dicts.scrbl"]

@; ----------------------------------------------------------------------
@include-section["procedures.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "void"]{Void and Undefined}

The constant @|void-const| is returned by most forms and procedures
that have a side-effect and no useful result. The constant
@|undefined-const| is used as the initial value for @scheme[letrec]
bindings.

@defproc[(void? [v any/c]) void?]{Returns @scheme[#t] if @scheme[v] is the
 constant @|void-const|, @scheme[#f] otherwise.}


@defproc[(void [v any/c] ...) void?]{Returns the constant @|void-const|. Each
 @scheme[v] argument is ignored.}

