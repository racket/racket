#lang scribble/doc
@(require "mz.rkt")

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
character content are normally @racket[eq?]. All symbols produced by
the default reader (see @secref["parse-symbol"]) are interned.

The two procedures @racket[string->uninterned-symbol] and
@racket[gensym] generate @deftech{uninterned} symbols, i.e., symbols
that are not @racket[eq?], @racket[eqv?], or @racket[equal?] to any
other symbol, although they may print the same as other symbols.

The procedure @racket[string->unreadable-symbol] returns an
@deftech{unreadable symbol} that is partially interned.  The default
reader (see @secref["parse-symbol"]) never produces a unreadable
symbol, but two calls to @racket[string->unreadable-symbol] with
@racket[equal?] strings produce @racket[eq?] results. An unreadable
symbol can print the same as an interned or uninterned
symbol. Unreadable symbols are useful in expansion and
compilation to avoid collisions with symbols that appear in the
source; they are usually not generated directly, but they can appear
in the result of functions like @racket[identifier-binding].

Interned and unreadable symbols are only weakly held by the internal
symbol table. This weakness can never affect the result of an
@racket[eq?], @racket[eqv?], or @racket[equal?] test, but a symbol may
disappear when placed into a weak box (see @secref["weakbox"]) used as
the key in a weak @tech{hash table} (see @secref["hashtables"]), or
used as an ephemeron key (see @secref["ephemerons"]).

@defproc[(symbol? [v any/c]) boolean?]{Returns @racket[#t] if @racket[v] is
 a symbol, @racket[#f] otherwise.

@examples[(symbol? 'Apple) (symbol? 10)]}


@defproc[(symbol-interned? [sym symbol?]) boolean?]{Returns @racket[#t] if @racket[sym] is
 @tech{interned}, @racket[#f] otherwise.

@examples[(symbol-interned? 'Apple)
          (symbol-interned? (gensym))
          (symbol-interned? (string->unreadable-symbol "Apple"))]}

@defproc[(symbol-unreadable? [sym symbol?]) boolean?]{Returns @racket[#t] if @racket[sym] is
 an @tech{unreadable symbol}, @racket[#f] otherwise.

@examples[(symbol-unreadable? 'Apple)
          (symbol-unreadable? (gensym))
          (symbol-unreadable? (string->unreadable-symbol "Apple"))]}

@defproc[(symbol->string [sym symbol?]) string?]{Returns a freshly
 allocated mutable string whose characters are the same as in
 @racket[sym].

@examples[(symbol->string 'Apple)]}


@defproc[(string->symbol [str string?]) symbol?]{Returns an
 @tech{interned} symbol whose characters are the same as in
 @racket[str].

@examples[(string->symbol "Apple") (string->symbol "1")]}


@defproc[(string->uninterned-symbol [str string?]) symbol?]{Like
 @racket[(string->symbol str)], but the resulting symbol is a new
 @tech{uninterned} symbol. Calling @racket[string->uninterned-symbol]
 twice with the same @racket[str] returns two distinct symbols.

@examples[(string->uninterned-symbol "Apple")
          (eq? 'a (string->uninterned-symbol "a"))
          (eq? (string->uninterned-symbol "a")
               (string->uninterned-symbol "a"))]}


@defproc[(string->unreadable-symbol [str string?]) symbol?]{Like
 @racket[(string->symbol str)], but the resulting symbol is a new
 @tech{unreadable symbol}. Calling @racket[string->unreadable-symbol]
 twice with equivalent @racket[str]s returns the same symbol, but
 @racket[read] never produces the symbol.

@examples[(string->unreadable-symbol "Apple")
          (eq? 'a (string->unreadable-symbol "a"))
          (eq? (string->unreadable-symbol "a")
               (string->unreadable-symbol "a"))]}


@defproc[(gensym [base (or/c string? symbol?) "g"]) symbol?]{Returns a
 new @tech{uninterned} symbol with an automatically-generated name. The
 optional @racket[base] argument is a prefix symbol or string.}

@examples[(gensym "apple")]

@; ------------------------------------------------------------
@include-section["regexps.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "keywords"]{Keywords}

@guideintro["keywords"]{keywords}

A @deftech{keyword} is like an @tech{interned} symbol, but its printed
form starts with @litchar{#:}, and a keyword cannot be used as an
identifier. Furthermore, a keyword by itself is not a valid
expression, though a keyword can be @racket[quote]d to form an
expression that produces the symbol.

Two keywords are @racket[eq?] if and only if they print the same.

Like symbols, keywords are only weakly held by the internal keyword
table; see @secref["symbols"] for more information.

@defproc[(keyword? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a keyword, @racket[#f] otherwise.}

@defproc[(keyword->string [keyword keyword?]) string?]{

Returns a string for the @racket[display]ed form of @racket[keyword],
not including the leading @litchar{#:}.}

@defproc[(string->keyword [str string?]) keyword?]{

Returns a keyword whose @racket[display]ed form is the same as that of
@racket[str], but with a leading @litchar{#:}.}

@defproc[(keyword<? [a-keyword keyword?] [b-keyword keyword?] ...+) boolean?]{

Returns @racket[#t] if the arguments are sorted, where the comparison
for each pair of keywords is the same as using
@racket[keyword->string] and @racket[string<?].}

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

Returns @racket[#t] if @racket[v] is a box, @racket[#f] otherwise.}


@defproc[(box [v any/c]) box?]{

Returns a new mutable box that contains @racket[v].}


@defproc[(box-immutable [v any/c]) (and/c box? immutable?)]{

Returns a new immutable box that contains @racket[v].}


@defproc[(unbox [box box?]) any/c]{

Returns the content of @racket[box].}


For any @racket[v], @racket[(unbox (box v))] returns @racket[v].


@defproc[(set-box! [box (and/c box? (not/c immutable?))]
                   [v any/c]) void?]{

Sets the content of @racket[box] to @racket[v].}

@; ----------------------------------------------------------------------
@include-section["hashes.scrbl"]

@; ----------------------------------------------------------------------
@include-section["sequences.scrbl"]

@; ----------------------------------------------------------------------
@include-section["dicts.scrbl"]

@; ----------------------------------------------------------------------
@include-section["sets.scrbl"]

@; ----------------------------------------------------------------------
@include-section["procedures.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "void"]{Void and Undefined}

The constant @|void-const| is returned by most forms and procedures
that have a side-effect and no useful result. The constant
@|undefined-const| is used as the initial value for @racket[letrec]
bindings. 

The @|void-const| value is always @racket[eq?] to itself, and the
@|undefined-const| value is also @racket[eq?] to itself.

@defproc[(void? [v any/c]) void?]{Returns @racket[#t] if @racket[v] is the
 constant @|void-const|, @racket[#f] otherwise.}


@defproc[(void [v any/c] ...) void?]{Returns the constant @|void-const|. Each
 @racket[v] argument is ignored.}

