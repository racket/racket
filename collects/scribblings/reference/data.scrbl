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


@defproc[(not [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is @scheme[#f], @scheme[#f] otherwise.
}


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
object, @scheme[#f] otherwise. See also @secref["model-eq"].}


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

A symbol is like an immutable string, but symbols are normally
@deftech{interned}, so that two symbols with the same character
content are normally @scheme[eq?]. All symbols produced by the default
reader (see @secref["parse-symbol"]) are interned.

The two procedures @scheme[string->uninterned-symbol] and
@scheme[gensym] generate @deftech{uninterned} symbols, i.e., symbols
that are not @scheme[eq?], @scheme[eqv?], or @scheme[equal?] to any
other symbol, although they may print the same as other symbols.

Regular (interned) symbols are only weakly held by the internal symbol
table. This weakness can never affect the result of an @scheme[eq?],
@scheme[eqv?], or @scheme[equal?] test, but a symbol may disappear
when placed into a weak box (see @secref["weakbox"]) used as the key
in a weak hash table (see @secref["hashtables"]), or used as an
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

A keyword is like an @tech{interned} symbol, but its printed form
starts with @litchar{#:}, and a keyword cannot be used as an
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
@include-section["pairs-scrbl.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "vectors"]{Vectors}

A vector is a fixed-length array with constant-time access and update
of the vector slots, which are numbered from @scheme[0] to one less
than the number of slots in the vector.

Two vectors are @scheme[equal?] if they have the same length, and if
the values in corresponding slots of the the vectors are
@scheme[equal?].

A vector can be @defterm{mutable} or @defterm{immutable}. When an
immutable vector is provided to a procedure like @scheme[vector-set!],
the @exnraise[exn:fail:contract]. Vectors generated by the default
reader (see @secref["parse-string"]) are immutable.

A vector can be used as a single-valued sequence (see
@secref["sequences"]). The elements of the vector serve as elements
of the sequence. See also @scheme[in-vector].

@defproc[(vector? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a vector, @scheme[#f] otherwise.}


@defproc[(make-vector [size nonnegative-exact-integer?]
                      [v any/c 0]) vector?]{

Returns a mutable vector with @scheme[size] slots, where all slots are
initialized to contain @scheme[v].}


@defproc[(vector [v any/c] ...) vector?]{

Returns a mutable vector with as many slots as provided @scheme[v]s,
where the slots are initialized to contain the given @scheme[v]s in
order.}


@defproc[(vector-immutable [v any/c] ...) (and/c vector?
                                                 immutable?)]{

Returns an immutable vector with as many slots as provided
@scheme[v]s, where the slots are contain the given @scheme[v]s in
order.}



@defproc[(vector-length [vec vector?]) nonnegative-exact-integer?]{

Returns the length of @scheme[vec] (i.e., the number of slots in the
vector).}

@defproc[(vector-ref [vec vector?][pos nonnegative-exact-integer?]) any/c]{

Returns the element in slot @scheme[pos] of @scheme[vec]. The first
slot is position @scheme[0], and the last slot is one less than
@scheme[(vector-length vec)].}

@defproc[(vector-set! [vec (and/c vector? (not/c immutable?))]
                      [pos nonnegative-exact-integer?]
                      [v any/c])
         void?]{

Updates the slot @scheme[pos] of @scheme[vec] to contain @scheme[v].}


@defproc[(vector->list [vec vector?])
         list?]{

Returns a list with the same length and elements as @scheme[vec].}


@defproc[(list->vector [lst list?])
         vector?]{

Returns a mutable vector with the same length and elements as
@scheme[lst].}


@defproc[(vector->immutable-vector [vec vector?])
         (and/c vector? immutable?)]{

Returns an immutable vector with the same length and elements as @scheme[vec].
If @scheme[vec] is itself immutable, then it is returned as the result.}


@defproc[(vector-fill! [vec (and/c vector? (not/c immutable?))]
                       [v any/c])
         void?]{

Changes all slots of @scheme[vec] to contain @scheme[v].}


@defproc[(vector->values [vec vector?]
                         [start-pos nonnegative-exact-integer? 0]
                         [end-pos nonnegative-exact-integer? (vector-length vec)])
         any]{

Returns @math{@scheme[end-pos] - @scheme[start-pos]} values, which are
the elements of @scheme[vec] from @scheme[start-pos] (inclusive) to
@scheme[end-pos] (exclusive). If @scheme[start-pos] or
@scheme[end-pos] are greater than @scheme[(vector-length vec)], or if
@scheme[end-pos] is less than @scheme[start-pos], the
@exnraise[exn:fail:contract].}

@; ------------------------------------------------------------
@section[#:tag "boxes"]{Boxes}

@defproc[(box? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a box, @scheme[#f] otherwise.}


@defproc[(box [v any/c]) box?]{

Returns a new mutable box that contains @scheme[v].}


@defproc[(box-immutable [v any/c]) (and/c box? immutable?)]{

Returns a new immutable box that contains @scheme[v].}


@defproc[(unbox [box box?]) any/c]{

Returns the content of @scheme[box]. For any @scheme[v],
@scheme[(unbox (box @scheme[v]))] returns @scheme[v].}


@defproc[(set-box! [box (and/c box? (not/c immutable?))]
                   [v any/c]) void?]{

Sets the content of @scheme[box] to @scheme[v].}


@; ----------------------------------------------------------------------
@section[#:tag "hashtables"]{Hash Tables}

A hash table can be used as a two-valued sequence (see
@secref["sequences"]). The keys and values of the hash table serve
as elements of the sequence (i.e., each element is a key and its
associated value). If a mapping is added to or removed from the hash
table during iteration, then an iteration step may fail with
@scheme[exn:fail:contract], or the iteration may skip or duplicate
keys and values.  See also @scheme[in-hash-table],
@scheme[in-hash-table-keys], @scheme[in-hash-table-values], and
@scheme[in-hash-table-pairs].

Two hash tables are @scheme[equal?] if they are created with the same
flags, and if they map the same keys to @scheme[equal?] values (where
``same key'' means either @scheme[eq?] or @scheme[equal?], depending
on the way the hash table compares keys).

@bold{Caveats concerning concurrent modification:} A hash table can be
manipulated with @scheme[hash-table-get], @scheme[hash-table-put!],
and @scheme[hash-table-remove!]  concurrently by multiple threads, and
the operations are protected by a table-specific semaphore as
needed. Two caveats apply, however:

 @itemize{

  @item{If a thread is terminated while applying
  @scheme[hash-table-get], @scheme[hash-table-put!], or
  @scheme[hash-table-remove!] to a hash table that uses
  @scheme[equal?]  comparisons, all current and future operations on
  the hash table block indefinitely.}

  @item{The @scheme[hash-table-map], @scheme[hash-table-for-each], and
  @scheme[hash-table-count] procedures do not use the table's
  semaphore. Consequently, if a hash table is extended with new keys
  by another thread while a map, for-each, or count is in process,
  arbitrary key--value pairs can be dropped or duplicated in the map
  or for-each. Similarly, if a map or for-each procedure itself
  extends the table, arbitrary key--value pairs can be dropped or
  duplicated. However, key mappings can be deleted or remapped by any
  thread with no adverse affects (i.e., the change does not affect a
  traversal if the key has been seen already, otherwise the traversal
  skips a deleted key or uses the remapped key's new value).}

 }

@bold{Caveat concerning mutable keys:} If a key into an
@scheme[equal?]-based hash table is mutated (e.g., a key string is
modified with @scheme[string-set!]), then the hash table's behavior
for put and get operations becomes unpredictable.


@defproc[(hash-table? [v any/c] [flag (one-of/c 'weak 'equal)] ...) 
         hash-table?]{

Returns @scheme[#t] if @scheme[v] was created by
@scheme[make-hash-table] or @scheme[make-immutable-hash-table] with
the given @scheme[flag]s (or more), @scheme[#f] otherwise. Each
provided @scheme[flag] must be distinct; if the second @scheme[flag]
is redundant, the @exnraise[exn:fail:contract].}


@defproc[(make-hash-table [flag (one-of/c 'weak 'equal)] ...) 
         hash-table?]{

Creates and returns a new hash table. If provided, each @scheme[flag]
must one of the following:

 @itemize{

  @item{@indexed-scheme['weak] --- creates a hash table with weakly-held
   keys (see @secref["weakbox"]).}

  @item{@indexed-scheme['equal] --- creates a hash table that compares
   keys using @scheme[equal?] instead of @scheme[eq?] (needed, for
   example, when using strings as keys).}

 }

By default, key comparisons use @scheme[eq?]. If the second
@scheme[flag] is redundant, the @exnraise[exn:fail:contract].}


@defproc[(make-immutable-hash-table [assocs (listof pair?)]
                                    [flag (one-of/c 'equal)]
                                    ...)
         (and/c hash-table? immutable?)]{

Creates an immutable hash table. In each element of @scheme[assocs],
the @scheme[car] of each pair is a key, and the @scheme[cdr] is the
corresponding value. The mappings are added to the table in the order
that they appear in @scheme[assocs], so later mappings can hide
earlier mappings. If the optional @scheme[flag] argument is provided,
it must be @scheme['equal], and the created hash table compares keys
with @scheme[equal?]; otherwise, the created table compares keys with
@scheme[eq?].}


@defproc[(hash-table-put! [hash-table (and/c hash-table?
                                             (not/c immutable?))]
                          [key any/c]
                          [v any/c]) void?]{

Maps @scheme[key] to @scheme[v] in @scheme[hash-table], overwriting
any existing mapping for @scheme[key].}

@defproc[(hash-table-get [hash-table hash-table?]
                         [key any/c]
                         [failure-result any/c (lambda () (raise (make-exn:fail ....)))])
         any]{

Returns the value for @scheme[key] in @scheme[hash-table]. If no value
is found for @scheme[key], then @scheme[failure-result] determines the
result: 

@itemize{

 @item{If @scheme[failure-result] is a procedure, it is called
       (through a tail call) with no arguments to produce the result.}

 @item{Otherwise, @scheme[failure-result] is returned as the result.}

}}


@defproc[(hash-table-remove! [hash-table (and/c hash-table?
                                             (not/c immutable?))]
                             [key any/c])
         void?]{

Removes any existing mapping for @scheme[key] in @scheme[hash-table].}


@defproc[(hash-table-map [hash-table hash-table?]
                         [proc (any/c any/c . -> . any/c)])
         (listof any/c)]{

Applies the procedure @scheme[proc] to each element in
@scheme[hash-table] in an unspecified order, accumulating the results
into a list. The procedure @scheme[proc] is called each time with a
key and its value. See the caveat above about concurrent
modification.}


@defproc[(hash-table-for-each [hash-table hash-table?]
                              [proc (any/c any/c . -> . any)])
         void?]{

Applies @scheme[proc] to each element in @scheme[hash-table] (for the
side-effects of @scheme[proc]) in an unspecified order. The procedure
@scheme[proc] is called each time with a key and its value. See the
caveat above about concurrent modification.}


@defproc[(hash-table-count [hash-table hash-table?])
         nonnegative-exact-integer?]{

Returns the number of keys mapped by @scheme[hash-table]. If
@scheme[hash-table] is not created with @scheme['weak], then the
result is computed in constant time and atomically. If
@scheme[hash-table] is created with @scheme['weak], see the caveat
above about concurrent modification.}


@defproc[(hash-table-iterate-first [hash-table hash-table?])
         (o/c false/c nonnegative-exact-integer?)]{

Returns @scheme[#f] if @scheme[hash-table] contains no elements,
otherwise it returns an integer that is a index to the first element
in the hash table; ``first'' refers to an unspecified ordering of the
table elements, and the index values are not necessarily consecutive
integers. This index is guaranteed to refer to the first item only as
long as no items are added to or removed from @scheme[hash-table].}

@defproc[(hash-table-iterate-next [hash-table hash-table?]
                                  [prev nonnegative-exact-integer?])
         (o/c false/c nonnegative-exact-integer?)]{

Returns either an integer that is an index to the element in
@scheme[hash-table] after the element indexed by @scheme[pos] (which
is not necessarily one more than @scheme[pos]) or @scheme[#f] if
@scheme[pos] refers to the last element in @scheme[hash-table]. If
@scheme[pos] is not a valid index, then the
@exnraise[exn:fail:contract]. The result index is guaranteed to refer
to its item only as long as no items are added to or removed from
@scheme[hash-table].}


@defproc[(hash-table-iterate-key [hash-table hash-table?]
                                 [pos nonnegative-exact-integer?])
         any]{

Returns the key for the element in @scheme[hash-table] at index
@scheme[pos]. If @scheme[pos] is not a valid index for
@scheme[hash-table], the @exnraise[exn:fail:contract].}


@defproc[(hash-table-iterate-value [hash-table hash-table?]
                                   [pos nonnegative-exact-integer?])
         any]{

Returns the value for the element in @scheme[hash-table] at index
@scheme[pos]. If @scheme[pos] is not a valid index for
@scheme[hash-table], the @exnraise[exn:fail:contract].}


@defproc[(hash-table-copy [hash-table hash-table?]) 
         (and/c hash-table?
                (not/c immutable?))]{

Returns a mutable hash table with the same mappings, same
key-comparison mode, and same key-holding strength as
@scheme[hash-table].}


@defproc[(eq-hash-code [v any/c]) exact-integer?]{

Returns an exact integer; for any two @scheme[eq?] values, the
returned integer is the same. Furthermore, for the result integer
@scheme[k] and any other exact integer @scheme[j], @scheme[(= k j)]
implies @scheme[(eq? k j)].}


@defproc[(equal-hash-code [v any/c]) exact-integer?]{

Returns an exact integer; for any two @scheme[equal?] values, the
returned integer is the same.  Furthermore, for the result integer
@scheme[k] and any other exact integer @scheme[j], @scheme[(= k j)]
implies @scheme[(eq? k j)]. If @scheme[v] contains a cycle through
pairs, vectors, boxes, and/or inspectable structure fields, then
@scheme[equal-hash-code] applied to @scheme[v] will loop
indefinitely.}

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

@defproc[(void? [v any/c]) void?]{Returns @scheme[#t] if @scheme[v] is the
 constant @|void-const|, @scheme[#f] otherwise.}


@defproc[(void [v any/c] ...) void?]{Returns the constant @|void-const|. Each
 @scheme[v] argument is ignored.}

