#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:style 'toc]{Core Datatypes}

Each of the built-in datatypes comes with a set of procedures for
manipulating members of the datatype.

@local-table-of-contents[]

@; ------------------------------------------------------------

@section[#:tag "booleans"]{Booleans}

True and false are represented by the values @scheme[#t] and
@scheme[#f], respectively, though operations that depend a boolean
value typically treat anything other than @scheme[#f] as true.

See also: @scheme[and], @scheme[or], @scheme[andmap], @scheme[ormap].


@defproc[(boolean? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is @scheme[#t] or @scheme[#f],
@scheme[#f] otherwise.}


@defproc[(equal? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @scheme[equal?] if and only if they are @scheme[eqv?],
unless otherwise specified for a particular datatype.

Datatypes with further specification of @scheme[equal?] include strings,
byte strings, numbers, pairs, vectors, and hash tables.}


@defproc[(eqv? [v1 any/c] [v2 any/c]) boolean?]{

Two values are @scheme[eqv?] if and only if they are @scheme[eq?],
unless otherwise specified for a particular datatype.

The number and character datatypes are the only ones for which
@scheme[eqv?] differs from @scheme[eq?].}


@defproc[(eq? [v1 any/c] [v2 any/c]) boolean?]{

Return @scheme[#t] if @scheme[v1] and @scheme[v2] refer to the same
object, @scheme[#f] otherwise. See also @secref["mz:model-eq"].}


@; ------------------------------------------------------------
@include-section["numbers.scrbl"]

@; ------------------------------------------------------------
@include-section["strings.scrbl"]

@; ------------------------------------------------------------
@include-section["bytes.scrbl"]

@; ------------------------------------------------------------
@include-section["chars.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "mz:symbols"]{Symbols}

@guideintro["guide:symbols"]{symbols}

A symbol is like an immutable string, but symbols are normally
@deftech{interned}, so that two symbols with the same character
content are normally @scheme[eq?]. All symbols produced by the default
reader (see @secref["mz:parse-symbol"]) are interned.

@index['("symbols" "generating")]{@index['("symbols" "unique")]{The}} two
procedures @scheme[string->uninterned-symbol] and @scheme[gensym]
generate @deftech{uninterned} symbols, i.e., symbols that are not
@scheme[eq?], @scheme[eqv?], or @scheme[equal?] to any other symbol,
although they may print the same as other symbols.

Regular (interned) symbols are only weakly held by the internal symbol
table. This weakness can never affect the result of an @scheme[eq?],
@scheme[eqv?], or @scheme[equal?] test, but a symbol may disappear
when placed into a weak box (see @secref["mz:weakbox"]) used as the key
in a weak hash table (see @secref["mz:hashtables"]), or used as an
ephemeron key (see @secref["mz:ephemerons"]).

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
@section[#:tag "mz:keywords"]{Keywords}

@guideintro["guide:keywords"]{keywords}

A keyword is like an @tech{interned} symbol, but its printed form
starts with @litchar{#:}, and a keyword cannot be used as an
identifier. Furthermore, a keyword by itself is not a valid
expression, though a keyword can be @scheme[quote]d to form an
expression that produces the symbol.

Two keywords are @scheme[eq?] if and only if they print the same.

Like symbols, keywords are only weakly held by the internal keyword
table; see @secref["mz:symbols"] for more information.

@item{@defproc[(keyword? [v any/c]) any] returns @scheme[#t] if @scheme[v] is a
keyword, @scheme[#f] otherwise.}

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
@section[#:tag "mz:pairs"]{Pairs and Lists}

A list can be used as a single-valued sequence (see
@secref["mz:sequences"]). The elements of the list serve as elements
of the sequence. See also @scheme[in-list].

@; ----------------------------------------
@subsection{Pair Constructors, Selectors, and Mutators}

@defproc[(cons [a any/c] [d any/c]) pair?]{Returns a pair whose first
element is @scheme[a] and second element is @scheme[d].}

@defproc[(pair? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
a pair, @scheme[#f] otherwise.}

@defproc[(cons? [v any/c]) boolean?]{The same as @scheme[(pair? v)].}

@defproc[(car [p pair?]) any/c]{Returns the first element of the
pair @scheme[p].}

@defproc[(cdr [p pair?]) any/c]{Returns the second element of the
pair @scheme[p].}

@defproc[(first [p pair?]) any/c]{The same as @scheme[(car p)].}

@defproc[(rest [p pair?]) any/c]{The same as @scheme[(cdr p)].}

@defthing[null null?]{The empty list.}

@defthing[empty null?]{The empty list.}

@defproc[(null? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
the empty list, @scheme[#f] otherwise.}

@defproc[(empty? [v any/c]) boolean?]{The same as @scheme[(null? v)].}

@defproc[(list [v any/c] ...) list?]{Returns a newly allocated list
containing the @scheme[v]s as its elements.}

@defproc[(map [proc procedure?] [lst (listof any/c)] ...+) (listof
 any/c)]{Applies @scheme[proc] to the elements of the @scheme[lst]s from the
 first elements to the last, returning @scheme[#f] as soon as any
 application returns @scheme[#f]. The @scheme[proc] argument must accept
 the same number of arguments as the number of supplied @scheme[lst]s,
 and all @scheme[lst]s must have the same number of elements.
 The result is a list containing each result of @scheme[proc].}

@defproc[(andmap [proc procedure?] [lst (listof any/c)] ...+)
 any]{Similar to @scheme[map], except that

@itemize{

 @item{the result is @scheme[#f] if any application of @scheme[proc] produces
       @scheme[#f], in which case @scheme[proc] is not applied to later
       elements of the @scheme[lst]s; or}

 @item{the result is that of @scheme[proc] applied to the last elements
       of the @scheme[lsts]s; more specifically, the application of
       @scheme[proc] to the last elements in the @scheme[lst]s is in tail
       position with respect to the @scheme[andmap] call.}

}

If the @scheme[lst]s are empty, then @scheme[#t] is returned.}

@examples[
(andmap positive? '(1 2 3))
(andmap positive? '(1 2 a))
(andmap positive? '(1 -2 a))
(andmap + '(1 2 3) '(4 5 6))
]

@defproc[(ormap [proc procedure?] [lst (listof any/c)] ...+)
any]{Similar to @scheme[map], except that

@itemize{

 @item{the result is @scheme[#f] if every application of @scheme[proc] produces
       @scheme[#f]; or}

 @item{the result of the first applciation of @scheme[proc] to produces a
       value other than @scheme[#f], in which case @scheme[proc] is not
       applied to later elements of the @scheme[lst]s; more specifically,
       the application of @scheme[proc] to the last elements in the
       @scheme[lst]s is in tail position with respect to the
       @scheme[andmap] call.}

}

If the @scheme[lst]s are empty, then @scheme[#f] is returned.}

@examples[
(ormap eq? '(a b c) '(a b c))
(ormap positive? '(1 2 a))
(ormap + '(1 2 3) '(4 5 6))
]

@defproc[(for-each [proc procedure?] [lst (listof any/c)] ...+)
 void?]{Similar to @scheme[map], but @scheme[proc] is called only for
 its effect, and its result (which can be any number of values) is
 ignored.}


@; ------------------------------------------------------------
@section[#:tag "mz:vectors"]{Vectors}

A vector can be used as a single-valued sequence (see
@secref["mz:sequences"]). The elements of the vector serve as elements
of the sequence. See also @scheme[in-vector].


@; ------------------------------------------------------------
@section[#:tag "mz:boxes"]{Boxes}

@; ----------------------------------------------------------------------
@section[#:tag "mz:hashtables"]{Hash Tables}

A hash table can be used as a two-valued sequence (see
@secref["mz:sequences"]). The keys and values of the hash table serve
as elements of the sequence (i.e., each element is a key and its
associated value). If a mapping is added to or removed from the hash
table during iteration, then an iteration step may fail with
@scheme[exn:fail:contract], or the iteration may skip or duplicate
keys and values.  See also @scheme[in-hash-table],
@scheme[in-hash-table-keys], @scheme[in-hash-table-values], and
@scheme[in-hash-table-pairs].


@; ----------------------------------------------------------------------
@include-section["sequences.scrbl"]

@; ----------------------------------------------------------------------
@include-section["procedures.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "void"]{Void and Undefined}

The constant @|void-const| is returned by most forms and procedures
that have a side-effect and no useful result. The constant
@|undefined-const| is used as the initial value for @scheme[letrec]
bindings.

@defproc[(void [v any/c] ...) void?]{Returns the constant @|void-const|. Each
 @scheme[v] argument is ignored.}

@defproc[(void? [v any/c]) void?]{Returns @scheme[#t] if @scheme[v] is the
 constant @|void-const|, @scheme[#f] otherwise.}


