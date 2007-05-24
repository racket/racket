#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title{Core Datatypes}

Each of the built-in datatypes comes with a set of procedures for
manipulating members of the datatype.

@; ------------------------------------------------------------

@section[#:tag "booleans"]{Booleans}

True and false are represented by the values @scheme[#t] and
@scheme[#f], respectively, though operations that depend a boolean
value typically treat anything other than @scheme[#f] as true.

See also: @scheme[and], @scheme[or], @scheme[andmap], @scheme[ormap].

@defproc[(boolean? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v]
is @scheme[#t] or @scheme[#f], @scheme[#f] otherwise.}

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

A symbol is like an immutable string, but symbols are normally
@index["interned symbols"]{@defterm{interned}}, so that two symbols
with the same character content are normally @scheme[eq?]. All symbols
produced by the default reader (see @secref["parse-symbol"]) are
interned.

@index['("symbols" "generating")]{@index['("symbols" "unique")]{The}} two
procedures @scheme[string->uninterned-symbol] and @scheme[gensym]
generate @idefterm{uninterned symbols}, i.e., a symbols that are not
@scheme[eq?], @scheme[eqv?], or @scheme[equal?] to any other symbol,
although they may print the same as other symbols.

Regular (interned) symbols are only weakly held by the internal symbol
table. This weakness can never affect the result of an @scheme[eq?],
@scheme[eqv?], or @scheme[equal?] test, but a symbol may disappear
when placed into a weak box (see @secref["weakbox"]) used as the key
in a weak hash table (see @secref["hashtable"]), or used as an
ephemeron key (see @secref["ephemeron"]).

@defproc[(symbol? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
 a symbol, @scheme[#f] otherwise.}

@examples[(symbol? 'Apple) (symbol? 10)]


@defproc[(symbol->string [sym symbol?]) symbol?]{Returns a freshly
 allocated mutable string whose characters are the same as in
 @scheme[sym].}

@examples[(symbol->string 'Apple)]


@defproc[(string->symbol [str string?]) symbol?]{Returns an interned
 symbol whose characters are the same as in @scheme[str].}

@examples[(string->symbol "Apple") (string->symbol "1")]


@defproc[(string->uninterned-symbol [str string?]) symbol?]{Like
 @scheme[(string->symbol str)], but the resulting symbol is a new
 uninterned symbol. Calling @scheme[string->uninterned-symbol] twice
 with the same @scheme[str] returns two distinct symbols.}

@examples[(string->uninterned-symbol "Apple") (eq? 'a (string->uninterned-symbol "a"))]


@defproc[(gensym [base (or/c string? symbol?) "g"]) symbol?]{Returns a
 new uninterned symbol with an automatically-generated name. The
 optional @scheme[base] argument is a prefix symbol or string.}

@examples[(gensym "apple")]

@; ------------------------------------------------------------
@section[#:tag "keywords"]{Keywords}

@; ----------------------------------------------------------------------
@section[#:tag "pairs"]{Pairs and Lists}

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

@defproc[(list [v any/c] ...0) list?]{Returns a newly allocated list
containing the @scheme[v]s as its elements.}

@defproc[(map [proc procedure?] [lst (listof any/c)] ...1) (listof
 any/c)]{Applies @scheme[proc] to the elements of the @scheme[lst]s from the
 first elements to the last, returning @scheme[#f] as soon as any
 application returns @scheme[#f]. The @scheme[proc] argument must accept
 the same number of arguments as the number of supplied @scheme[lst]s,
 and all @scheme[lst]s must have the same number of elements.
 The result is a list containing each result of @scheme[proc].}

@defproc[(andmap [proc procedure?] [lst (listof any/c)] ...1)
 any]{Similar to @scheme[map], except that

@itemize{

 @item{the result is @scheme[#f] if any application of @scheme[proc] produces
       @scheme[#f], in which case @scheme[proc] is not applied to later
       elements of the @scheme[lst]s; or}

 @item {the result is that of @scheme[proc] applied to the last elements
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

@defproc[(ormap [proc procedure?] [lst (listof any/c)] ...1)
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

@defproc[(for-each [proc procedure?] [lst (listof any/c)] ...1)
 void?]{Similar to @scheme[map], but @scheme[proc] is called only for
 its effect, and its result (which can be any number of values) is
 ignored.}


@; ----------------------------------------------------------------------
@section[#:tag "hashtables"]{Hash Tables}

@section[#:tag "procedures"]{Procedures}

@section[#:tag "promises"]{Promises}

@; ----------------------------------------------------------------------
@section[#:tag "void"]{Void and Undefined}

@defproc[(void [v any/c] ...) void?]{Returns the constant @void-const[]. Each
 @scheme[v] argument is ignored.}

@defproc[(void? [v any/c]) void?]{Returns @scheme[#t] if @scheme[v] is the
 constant @void-const[], @scheme[#f] otherwise.}


