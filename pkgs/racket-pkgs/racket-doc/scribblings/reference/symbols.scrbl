#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "symbols"]{Symbols}

@guideintro["symbols"]{symbols}

@section-index["symbols" "generating"]
@section-index["symbols" "unique"]

A @deftech{symbol} is like an immutable string, but symbols are
normally @tech{interned}, so that two symbols with the same
character content are normally @racket[eq?]. All symbols produced by
the default reader (see @secref["parse-symbol"]) are @tech{interned}.

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

@see-read-print["symbol"]{symbols}

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


@defproc[(symbol<? [a-sym symbol?] [b-sym symbol?] ...) boolean?]{

Returns @racket[#t] if the arguments are sorted, where the comparison
for each pair of symbols is the same as using
@racket[symbol->string] with @racket[string->bytes/utf-8] and
@racket[bytes<?].}
