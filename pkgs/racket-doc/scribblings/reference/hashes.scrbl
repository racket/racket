#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "hashtables"]{Hash Tables}

@(define (see-also-caveats)
   @t{See also the @concurrency-caveat[] and the @mutable-key-caveat[] above.})
@(define (see-also-concurrency-caveat)
   @t{See also the @concurrency-caveat[] above.})
@(define (see-also-mutable-key-caveat)
   @t{See also the @mutable-key-caveat[] above.})

@guideintro["hash-tables"]{hash tables}

A @deftech{hash table} (or simply @deftech{hash}) maps each of its
keys to a single value. For a given hash table, keys are equivalent
via @racket[equal?], @racket[equal-always?], @racket[eqv?], or
@racket[eq?], and keys are retained either strongly, weakly
(see @secref["weakbox"]), or like @tech{ephemerons}.
A hash table is also either mutable or immutable.
Immutable hash tables support effectively constant-time access and
update, just like mutable hash tables; the constant on immutable
operations is usually larger, but the functional nature of immutable
hash tables can pay off in certain algorithms. Use @racket[immutable?]
to check whether a hash table is immutable.

@margin-note{Immutable hash tables actually provide @math{O(log N)}
access and update. Since @math{N} is limited by the address space so
that @math{log N} is limited to less than 30 or 62 (depending on the
platform), @math{log N} can be treated reasonably as a constant.}

For @racket[equal?]-based hashing, the built-in hash functions on
@tech{strings}, @tech{pairs}, @tech{lists}, @tech{vectors},
@tech{prefab} or transparent @tech{structures}, @|etc|, take time
proportional to the size of the value. The hash code for a compound
data structure, such as a list or vector, depends on hashing each item
of the container, but the depth of such recursive hashing is
limited (to avoid potential problems with cyclic data). For a
non-@tech{list} @tech{pair}, both @racket[car] and @racket[cdr]
hashing is treated as a deeper hash, but the @racket[cdr] of a
@tech{list} is treated as having the same hashing depth as the list.

A hash table can be used as a two-valued @tech{sequence} (see
@secref["sequences"]). The keys and values of the hash table serve as
elements of the sequence (i.e., each element is a key and its
associated value). If a mapping is added to or removed from the hash
table during iteration, then an iteration step may fail with
@racket[exn:fail:contract], or the iteration may skip or duplicate
keys and values.  See also @racket[in-hash], @racket[in-hash-keys],
@racket[in-hash-values], and @racket[in-hash-pairs].

Two hash tables cannot be @racket[equal?] unless they have the same
mutability, use the same key-comparison procedure (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]), both hold
keys strongly, weakly, or like @tech{ephemerons}.
Empty immutable hash tables are @racket[eq?]
when they are @racket[equal?].

@history[#:changed "7.2.0.9" @elem{Made empty immutable hash tables
                                   @racket[eq?] when they are
                                   @racket[equal?].}]

@elemtag['(caveat "concurrency")]{@bold{Caveats concerning concurrent
modification:}} A mutable hash table can be manipulated with
@racket[hash-ref], @racket[hash-set!], and @racket[hash-remove!]
concurrently by multiple threads, and the operations are protected by
a table-specific semaphore as needed. Several caveats apply, however:

 @itemize[

  @item{If a thread is terminated while applying @racket[hash-ref],
  @racket[hash-ref-key], @racket[hash-set!], @racket[hash-remove!],
  @racket[hash-ref!], @racket[hash-update!], or @racket[hash-clear!]
  to a hash table that
  uses @racket[equal?], @racket[equal-always?], or @racket[eqv?] key
  comparisons, all current and future operations on the hash table may
  block indefinitely.}

  @item{The @racket[hash-map], @racket[hash-for-each], and @racket[hash-clear!] procedures do
  not use the table's semaphore to guard the traversal as a whole
  (if a traversal is needed, in the case of @racket[hash-clear!]).
  Changes by one thread to a hash table can affect the keys and values
  seen by another thread part-way through its traversal of the same
  hash table.}

 @item{The @racket[hash-update!] and @racket[hash-ref!] functions 
 use a table's semaphore
 independently for the @racket[hash-ref] and @racket[hash-set!] parts
 of their functionality, which means that the update as a whole is not
 ``atomic.''}

 @item{Adding a mutable hash table as a key in itself is trouble on
  the grounds that the key is being mutated (see the caveat below),
  but it is also a kind of concurrent use of the hash table: computing
  a hash table's hash code may require waiting on the table's
  semaphore, but the semaphore is already held for modifying the hash
  table, so the hash-table addition can block indefinitely.}

 ]

@elemtag['(caveat "mutable-keys")]{@bold{Caveat concerning mutable
keys:}} If a key in an @racket[equal?]-based hash table is mutated
(e.g., a key string is modified with @racket[string-set!]), then the
hash table's behavior for insertion and lookup operations becomes
unpredictable.

A literal or printed hash table starts with @litchar{#hash},
@litchar{#hashalw}, @litchar{#hasheqv}, or
@litchar{#hasheq}. @see-read-print["hashtable"]{hash tables}

@defproc[(hash? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{hash table}, @racket[#f]
otherwise.

See also @racket[immutable-hash?] and @racket[mutable-hash?].}

@defproc[(hash-equal? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] compares keys with @racket[equal?],
@racket[#f] if it compares with @racket[eq?], @racket[eqv?], or
@racket[equal-always?].}

@defproc[(hash-equal-always? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] compares keys with
@racket[equal-always?], @racket[#f] if it compares with @racket[eq?],
@racket[eqv?], or @racket[equal?].

@history[#:added "8.5.0.3"]}

@defproc[(hash-eqv? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] compares keys with @racket[eqv?],
@racket[#f] if it compares with @racket[equal?],
@racket[equal-always?], or @racket[eq?].}

@defproc[(hash-eq? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] compares keys with @racket[eq?],
@racket[#f] if it compares with @racket[equal?],
@racket[equal-always?], or @racket[eqv?].}


@defproc[(hash-strong? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] retains its keys strongly,
@racket[#f] if it retains keys weakly or like @tech{ephemerons}.

@history[#:added "8.0.0.10"]}


@defproc[(hash-weak? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] retains its keys weakly,
@racket[#f] if it retains keys strongly or like @tech{ephemerons}.}


@defproc[(hash-ephemeron? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] retains its keys like
@tech{ephemerons}, @racket[#f] if it retains keys strongly or merely
weakly.

@history[#:added "8.0.0.10"]}


@deftogether[(
@defproc[(hash [key any/c] [val any/c] ... ...) (and/c hash? hash-equal? immutable? hash-strong?)]
@defproc[(hashalw [key any/c] [val any/c] ... ...)
         (and/c hash? hash-equal-always? immutable? hash-strong?)]
@defproc[(hasheq [key any/c] [val any/c] ... ...) (and/c hash? hash-eq? immutable? hash-strong?)]
@defproc[(hasheqv [key any/c] [val any/c] ... ...) (and/c hash? hash-eqv? immutable? hash-strong?)]
)]{

Creates an immutable hash table with each given @racket[key] mapped to
the following @racket[val]; each @racket[key] must have a @racket[val],
so the total number of arguments to @racket[hash] must be even.

The @racket[hash] procedure creates a table where keys are compared
with @racket[equal?], @racket[hashalw] creates a table where keys are compared with
@racket[equal-always?], @racket[hasheq] procedure creates a table where
keys are compared with @racket[eq?], @racket[hasheqv] procedure
creates a table where keys are compared with @racket[eqv?].

The @racket[key] to @racket[val] mappings are added to the table in
the order that they appear in the argument list, so later mappings can
hide earlier mappings if the @racket[key]s are equal.

@history[#:changed "8.5.0.3" @elem{Added @racket[hashalw].}]}

@deftogether[(
@defproc[(make-hash [assocs (listof pair?) null]) (and/c hash? hash-equal? (not/c immutable?) hash-strong?)]
@defproc[(make-hashalw [assocs (listof pair?) null])
         (and/c hash? hash-equal-always? (not/c immutable?) hash-strong?)]
@defproc[(make-hasheqv [assocs (listof pair?) null]) (and/c hash? hash-eqv? (not/c immutable?) hash-strong?)]
@defproc[(make-hasheq [assocs (listof pair?) null]) (and/c hash? hash-eq? (not/c immutable?) hash-strong?)]
)]{

Creates a mutable hash table that holds keys strongly. 

The @racket[make-hash] procedure creates a table where keys are
compared with @racket[equal?], @racket[make-hasheq] procedure creates
a table where keys are compared with @racket[eq?],
@racket[make-hasheqv] procedure creates a table where keys are
compared with @racket[eqv?], and @racket[make-hashalw] creates a table
where keys are compared with @racket[equal-always?].

The table is initialized with the content of @racket[assocs].  In each
element of @racket[assocs], the @racket[car] is a key, and the
@racket[cdr] is the corresponding value. The mappings are added to the
table in the order that they appear in @racket[assocs], so later
mappings can hide earlier mappings.

See also @racket[make-custom-hash].

@history[#:changed "8.5.0.3" @elem{Added @racket[make-hashalw].}]}

@deftogether[(
@defproc[(make-weak-hash [assocs (listof pair?) null]) (and/c hash? hash-equal? (not/c immutable?) hash-weak?)]
@defproc[(make-weak-hashalw [assocs (listof pair?) null])
         (and/c hash? hash-equal-always? (not/c immutable?) hash-weak?)]
@defproc[(make-weak-hasheqv [assocs (listof pair?) null]) (and/c hash? hash-eqv? (not/c immutable?) hash-weak?)]
@defproc[(make-weak-hasheq [assocs (listof pair?) null]) (and/c hash? hash-eq? (not/c immutable?) hash-weak?)]
)]{

Like @racket[make-hash], @racket[make-hasheq],
@racket[make-hasheqv], and @racket[make-hashalw], but creates a
mutable hash table that holds keys weakly.

Beware that values in a weak hash table are retained normally. If a value in
the table refers back to its key, then the table will retain the value
and therefore the key; the mapping will never be removed from the
table even if the key becomes otherwise inaccessible. To avoid that
problem, use an ephemeron hash table as created by
@racket[make-ephemeron-hash], @racket[make-ephemeron-hashalw],
@racket[make-ephemeron-hasheqv], or @racket[make-ephemeron-hasheq].
For values that do not refer to keys,
there is a modest extra cost to using an ephemeron hash table instead
of a weak hash table, but prefer an ephemeron hash table when in
doubt.

@history[#:changed "8.5.0.3" @elem{Added @racket[make-weak-hashalw].}]}


@deftogether[(
@defproc[(make-ephemeron-hash [assocs (listof pair?) null]) (and/c hash? hash-equal? (not/c immutable?) hash-ephemeron?)]
@defproc[(make-ephemeron-hashalw [assocs (listof pair?) null])
         (and/c hash? hash-equal-always? (not/c immutable?) hash-ephemeron?)]
@defproc[(make-ephemeron-hasheqv [assocs (listof pair?) null]) (and/c hash? hash-eqv? (not/c immutable?) hash-ephemeron?)]
@defproc[(make-ephemeron-hasheq [assocs (listof pair?) null]) (and/c hash? hash-eq? (not/c immutable?) hash-ephemeron?)]
)]{

Like @racket[make-hash], @racket[make-hasheq],
@racket[make-hasheqv], and @racket[make-hashalw],
but creates a mutable hash table that holds
key-value combinations in the same way as an @tech{ephemeron}.

Using an ephemeron hash table is like using a weak hash table and
mapping each key to a @tech{ephemeron} that pairs the key and value.
An advantage of an ephemeron hash table is that the value need not be
extracted with @racket[ephemeron-value] from the result of functions
like @racket[hash-ref]. An ephemeron hash table might also be
represented more compactly than a weak hash table with explicit
@tech{ephemeron} values.

@history[#:added "8.0.0.10"
         #:changed "8.5.0.3" @elem{Added @racket[make-ephemeron-hashalw].}]}

@deftogether[(
@defproc[(make-immutable-hash [assocs (listof pair?) null])
         (and/c hash? hash-equal? immutable? hash-strong?)]
@defproc[(make-immutable-hashalw [assocs (listof pair?) null])
         (and/c hash? hash-equal-always? immutable? hash-strong?)]
@defproc[(make-immutable-hasheqv [assocs (listof pair?) null])
         (and/c hash? hash-eqv? immutable? hash-strong?)]
@defproc[(make-immutable-hasheq [assocs (listof pair?) null])
         (and/c hash? hash-eq? immutable? hash-strong?)]
)]{

Like @racket[hash], @racket[hashalw], @racket[hasheq], and
@racket[hasheqv], but accepts
the key--value mapping in association-list form like
@racket[make-hash], @racket[make-hashalw], @racket[make-hasheq], and
@racket[make-hasheqv].

@history[#:changed "8.5.0.3" @elem{Added @racket[make-immutable-hashalw].}]}


@defproc[(hash-set! [hash (and/c hash? (not/c immutable?))]
                    [key any/c]
                    [v any/c]) void?]{

Maps @racket[key] to @racket[v] in @racket[hash], overwriting
any existing mapping for @racket[key].

@see-also-caveats[]}

@defproc[(hash-set*! [hash (and/c hash? (not/c immutable?))]
                     [key any/c]
                     [v any/c]
                     ...
                     ...) void?]{

Maps each @racket[key] to each @racket[v] in @racket[hash], overwriting
any existing mapping for each @racket[key]. Mappings are added from the left, so
later mappings overwrite earlier mappings.

@see-also-caveats[]}


@defproc[(hash-set [hash (and/c hash? immutable?)]
                   [key any/c]
                   [v any/c])
          (and/c hash? immutable?)]{

Functionally extends @racket[hash] by mapping @racket[key] to
@racket[v], overwriting any existing mapping for @racket[key], and
returning the extended hash table.

@see-also-mutable-key-caveat[]}

@defproc[(hash-set* [hash (and/c hash? immutable?)]
                    [key any/c]
                    [v any/c]
                    ...
                    ...)
          (and/c hash? immutable?)]{

Functionally extends @racket[hash] by mapping each @racket[key] to
@racket[v], overwriting any existing mapping for each @racket[key], and
returning the extended hash table. Mappings are added from the left, so
later mappings overwrite earlier mappings.

@see-also-mutable-key-caveat[]}

@defproc[(hash-ref [hash hash?]
                   [key any/c]
                   [failure-result failure-result/c
                                   (lambda ()
                                     (raise (make-exn:fail:contract ....)))])
         any]{

Returns the value for @racket[key] in @racket[hash]. If no value
is found for @racket[key], then @racket[failure-result] determines the
result: 

@itemize[

 @item{If @racket[failure-result] is a procedure, it is called
       (through a tail call) with no arguments to produce the result.}

 @item{Otherwise, @racket[failure-result] is returned as the result.}

]

@examples[
#:eval the-eval
(eval:error (hash-ref (hash) "hi"))
(hash-ref (hash) "hi" 5)
(hash-ref (hash) "hi" (lambda () "flab"))
(hash-ref (hash "hi" "bye") "hi")
(eval:error (hash-ref (hash "hi" "bye") "no"))
]

@see-also-caveats[]}

@defproc[(hash-ref-key [hash hash?]
                       [key any/c]
                       [failure-result failure-result/c
                                       (lambda ()
                                         (raise (make-exn:fail:contract ....)))])
         any]{

Returns the key held by @racket[hash] that is equivalent to @racket[key]
according to @racket[hash]'s key-comparison function. If no key is found,
then @racket[failure-result] is used as in @racket[hash-ref] to determine
the result.

If @racket[hash] is not an @tech{impersonator}, then the returned key,
assuming it is found, will be @racket[eq?]-equivalent to the one
actually retained by @racket[hash]:

@examples[
#:eval the-eval
(define original-key "hello")
(define key-copy (string-copy original-key))

(equal? original-key key-copy)
(eq? original-key key-copy)

(define table (make-hash))
(hash-set! table original-key 'value)

(eq? (hash-ref-key table "hello") original-key)
(eq? (hash-ref-key table "hello") key-copy)
]

If a mutable hash is updated multiple times using keys that are
not @racket[eq?]-equivalent but are equivalent according to the
hash's key-comparison procedure, the hash retains the first one:

@examples[
#:eval the-eval
(define original-key "hello")
(define key-copy (string-copy original-key))

(define table (make-hash))
(hash-set! table original-key 'one)
(hash-set! table key-copy 'two)

(eq? (hash-ref-key table "hello") original-key)
(eq? (hash-ref-key table "hello") key-copy)
]

Conversely, an immutable hash retains the key that was most-recently
used to update it:
@examples[
#:eval the-eval
(define original-key "hello")
(define key-copy (string-copy original-key))

(define table0 (hash))
(define table1 (hash-set table0 original-key 'one))
(define table2 (hash-set table1 key-copy 'two))

(eq? (hash-ref-key table2 "hello") original-key)
(eq? (hash-ref-key table2 "hello") key-copy)
]

If @racket[hash] is an @tech{impersonator}, then the returned key
will be determined as described in the documentation to
@racket[impersonate-hash].

@see-also-caveats[]

@history[#:added "7.4.0.3"]}

@defproc[(hash-ref! [hash hash?] [key any/c] [to-set failure-result/c])
         any]{

Returns the value for @racket[key] in @racket[hash].  If no value is
found for @racket[key], then @racket[to-set] determines the result as
in @racket[hash-ref] (i.e., it is either a thunk that computes a value
or a plain value), and this result is stored in @racket[hash] for the
@racket[key].  (Note that if @racket[to-set] is a thunk, it is not
invoked in tail position.)

@see-also-caveats[]}


@defproc[(hash-has-key? [hash hash?] [key any/c])
         boolean?]{

Returns @racket[#t] if @racket[hash] contains a value for the given
@racket[key], @racket[#f] otherwise.}


@defproc[(hash-update! [hash (and/c hash? (not/c immutable?))]
                       [key any/c]
                       [updater (any/c . -> . any/c)]
                       [failure-result failure-result/c
                        (lambda ()
                          (raise (make-exn:fail:contract ....)))])
         void?]{

 Updates the value mapped by @racket[key] in @racket[hash] by applying @racket[updater] to the value.
 The value returned by @racket[updater] becomes the new mapping for @racket[key], overwriting the
 original value in @racket[hash].

 @(examples
   #:eval the-eval
   (eval:no-prompt
    (define h (make-hash))
    (hash-set! h 'a 5))

   (hash-update! h 'a add1)
   h)

 The optional @racket[failure-result] argument is used when no mapping exists for @racket[key]
 already, in the same manner as in @racket[hash-ref].

 @(examples
   #:eval the-eval
   (eval:no-prompt
    (define h (make-hash)))
 
   (eval:error (hash-update! h 'b add1))
   (hash-update! h 'b add1 0)
   h)

 @see-also-caveats[]}


@defproc[(hash-update [hash (and/c hash? immutable?)]
                      [key any/c]
                      [updater (any/c . -> . any/c)]
                      [failure-result failure-result/c
                       (lambda ()
                         (raise (make-exn:fail:contract ....)))])
         (and/c hash? immutable?)]{

 Functionally updates the value mapped by @racket[key] in @racket[hash] by applying @racket[updater]
 to the value and returning a new hash table. The value returned by @racket[updater] becomes the new
 mapping for @racket[key] in the returned hash table.

 @(examples
   #:eval the-eval
   (eval:no-prompt
    (define h (hash 'a 5)))
   
   (hash-update h 'a add1))

 The optional @racket[failure-result] argument is used when no mapping exists for @racket[key]
 already, in the same manner as in @racket[hash-ref].

 @(examples
   #:eval the-eval
   (eval:no-prompt
    (define h (hash)))
   
   (eval:error (hash-update h 'b add1))
   (hash-update h 'b add1 0))

 @see-also-mutable-key-caveat[]}


@defproc[(hash-remove! [hash (and/c hash? (not/c immutable?))]
                       [key any/c])
         void?]{

Removes any existing mapping for @racket[key] in @racket[hash].

@see-also-caveats[]}


@defproc[(hash-remove [hash (and/c hash? immutable?)]
                      [key any/c])
         (and/c hash? immutable?)]{

Functionally removes any existing mapping for @racket[key] in
@racket[hash], returning @racket[hash] (i.e., a result @racket[eq?] to
@racket[hash]) if @racket[key] is not present in @racket[hash].

@see-also-mutable-key-caveat[]}


@defproc[(hash-clear! [hash (and/c hash? (not/c immutable?))])
         void?]{

Removes all mappings from @racket[hash].

If @racket[hash] is not an @tech{impersonator}, then all mappings are
removed in constant time. If @racket[hash] is an @tech{impersonator},
then each key is removed one-by-one using @racket[hash-remove!].

@see-also-caveats[]}


@defproc[(hash-clear [hash (and/c hash? immutable?)])
         (and/c hash? immutable?)]{

Functionally removes all mappings from @racket[hash].

If @racket[hash] is not a @tech{chaperone}, then clearing is
equivalent to creating a new @tech{hash table}, and the operation is
performed in constant time.  If @racket[hash] is a @tech{chaperone},
then each key is removed one-by-one using @racket[hash-remove].}


@defproc[(hash-copy-clear
          [hash hash?]
          [#:kind kind (or/c #f 'immutable 'mutable 'weak 'ephemeron) #f])
         hash?]{

Produces an empty @tech{hash table} with the same key-comparison
procedure as @racket[hash], with either the given @racket[kind]
or the same kind as the given @racket[hash].

If @racket[kind] is not supplied or @racket[#f], produces a hash
table of the same kind and mutability as the given @racket[hash].
If @racket[kind] is @racket['immutable], @racket['mutable],
@racket['weak], or @racket['ephemeron], produces a table that's
immutable, mutable with strongly-held keys, mutable with
weakly-held keys, or mutable with ephemeron-held keys
respectively.

@history[#:changed "8.5.0.2" @elem{Added the @racket[kind] argument.}]}



@defproc[(hash-map [hash hash?]
                   [proc (any/c any/c . -> . any/c)]
                   [try-order? any/c #f])
         (listof any/c)]{

Applies the procedure @racket[proc] to each element in
@racket[hash] in an unspecified order, accumulating the results
into a list. The procedure @racket[proc] is called each time with a
key and its value, and the procedure's individual results appear in
order in the result list.

If a hash table is extended with new keys (either through
@racket[proc] or by another thread) while a @racket[hash-map] or
@racket[hash-for-each] traversal is in process, arbitrary key--value
pairs can be dropped or duplicated in the traversal. Key mappings can
be deleted or remapped (by any thread) with no adverse affects; the
change does not affect a traversal if the key has been seen already,
otherwise the traversal skips a deleted key or uses the remapped key's
new value.

@see-also-concurrency-caveat[]

If @racket[try-order?] is true, then the order of keys and values
passed to @racket[proc] is normalized under certain
circumstances---including when every key is one of the following and
with the following order (earlier bullets before later):

@itemlist[
 @item{@tech{booleans} sorted @racket[#f] before @racket[#t];}
 @item{@tech{characters} sorted by @racket[char<?];}
 @item{@tech{real numbers} sorted by @racket[<];}
 @item{@tech{symbols} sorted with @tech{uninterned} symbols before
       @tech{unreadable symbols} before @tech{interned} symbols,
       then sorted by @racket[symbol<?];}
 @item{@tech{keywords} sorted by @racket[keyword<?];}
 @item{@tech{strings} sorted by @racket[string<?];}
 @item{@tech{byte strings} sorted by @racket[bytes<?];}
 @item{@racket[null];}
 @item{@|void-const|; and}
 @item{@racket[eof].}
]

@history[#:changed "6.3" @elem{Added the @racket[try-order?] argument.}
         #:changed "7.1.0.7" @elem{Added guarantees for @racket[try-order?].}]}

@defproc[(hash-map/copy
          [hash hash?]
          [proc (any/c any/c . -> . (values any/c any/c))]
          [#:kind kind (or/c #f 'immutable 'mutable 'weak 'ephemeron) #f])
         hash?]{

Applies the procedure @racket[proc] to each element in
@racket[hash] in an unspecified order, accumulating the results
into a new hash with the same key-comparison procedure as
@racket[hash], with either the given @racket[kind] or the same
kind as the given @racket[hash].

If @racket[kind] is not supplied or @racket[#f], produces a hash
table of the same kind and mutability as the given @racket[hash].
If @racket[kind] is @racket['immutable], @racket['mutable],
@racket['weak], or @racket['ephemeron], produces a table that's
immutable, mutable with strongly-held keys, mutable with
weakly-held keys, or mutable with ephemeron-held keys
respectively.

@examples[
#:eval the-eval
(hash-map/copy #hash((a . "apple") (b . "banana"))
               (lambda (k v) (values k (string-upcase v))))
(define frozen-capital
  (hash-map/copy (make-hash '((a . "apple") (b . "banana")))
                 (lambda (k v) (values k (string-upcase v)))
                 #:kind 'immutable))
frozen-capital
(immutable? frozen-capital)
]

@history[#:added "8.5.0.2"]}

@defproc[(hash-keys [hash hash?] [try-order? any/c #f])
         (listof any/c)]{
Returns a list of the keys of @racket[hash] in an unspecified order.

If @racket[try-order?] is true, then the order of keys is normalized under
certain circumstances.  See @racket[hash-map] for further explanations on
@racket[try-order?] and on information about modifying @racket[hash] during
@racket[hash-keys]. @see-also-concurrency-caveat[]

@history[#:changed "8.3.0.11" @elem{Added the @racket[_try-order?] argument.}]}

@defproc[(hash-values [hash hash?] [try-order? any/c #f])
         (listof any/c)]{
Returns a list of the values of @racket[hash] in an unspecified order.

If @racket[try-order?] is true, then the order of values is normalized under
certain circumstances, based on the ordering of the associated keys.
See @racket[hash-map] for further explanations on @racket[try-order?] and on
information about modifying @racket[hash] during
@racket[hash-values]. @see-also-concurrency-caveat[]

@history[#:changed "8.3.0.11" @elem{Added the @racket[_try-order?] argument.}]}

@defproc[(hash->list [hash hash?] [try-order? any/c #f])
         (listof (cons/c any/c any/c))]{
Returns a list of the key--value pairs of @racket[hash] in an unspecified order.

If @racket[try-order?] is true, then the order of keys and values is normalized
under certain circumstances. See @racket[hash-map] for further explanations on
@racket[try-order?] and on information about modifying @racket[hash] during
@racket[hash->list]. @see-also-concurrency-caveat[]

@history[#:changed "8.3.0.11" @elem{Added the @racket[_try-order?] argument.}]}

@defproc[(hash-keys-subset? [hash1 hash?] [hash2 hash?])
         boolean?]{
Returns @racket[#t] if the keys of @racket[hash1] are a subset of or
the same as the keys of @racket[hash2]. The hash tables must both use
the same key-comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]), otherwise the
@exnraise[exn:fail:contract].

Using @racket[hash-keys-subset?] on immutable hash tables can be much
faster than iterating through the keys of @racket[hash1] to make sure
that each is in @racket[hash2].

@history[#:added "6.5.0.8"]}

@defproc[(hash-for-each [hash hash?]
                        [proc (any/c any/c . -> . any)]
                        [try-order? any/c #f])
         void?]{

Applies @racket[proc] to each element in @racket[hash] (for the
side-effects of @racket[proc]) in an unspecified order. The procedure
@racket[proc] is called each time with a key and its value.

See @racket[hash-map] for information about @racket[try-order?] and
about modifying @racket[hash] within @racket[proc].
@see-also-concurrency-caveat[]

@history[#:changed "6.3" @elem{Added the @racket[try-order?] argument.}
         #:changed "7.1.0.7" @elem{Added guarantees for @racket[try-order?].}]}


@defproc[(hash-count [hash hash?])
         exact-nonnegative-integer?]{

Returns the number of keys mapped by @racket[hash].

For the @tech{CS} implementation of Racket, the result is always
computed in constant time and atomically. For the @tech{BC} implementation
of Racket, the result is computed in constant time and atomically only if
@racket[hash] does not retain keys weakly or like an @tech{ephemeron},
otherwise, a traversal is required to count the keys.}


@defproc[(hash-empty? [hash hash?]) boolean?]{

Equivalent to @racket[(zero? (hash-count hash))].}


@defproc[(hash-iterate-first [hash hash?])
         (or/c #f exact-nonnegative-integer?)]{

Returns @racket[#f] if @racket[hash] contains no elements, otherwise
it returns an integer that is an index to the first element in the hash
table; ``first'' refers to an unspecified ordering of the table
elements, and the index values are not necessarily consecutive
integers.

For a mutable @racket[hash], this index is guaranteed to refer to the
first item only as long as no items are added to or removed from
@racket[hash]. More generally, an index is guaranteed to be a
@deftech{valid hash index} for a given hash table only as long it
comes from @racket[hash-iterate-first] or @racket[hash-iterate-next],
and only as long as the hash table is not modified. In the case of a
hash table with weakly held keys or keys held like @tech{ephemerons},
the hash table can be implicitly modified by the garbage collector
(see @secref["gc-model"]) when it discovers that the key is not
reachable.}


@defproc[(hash-iterate-next [hash hash?]
                            [pos exact-nonnegative-integer?])
         (or/c #f exact-nonnegative-integer?)]{

Returns either an integer that is an index to the element in
@racket[hash] after the element indexed by @racket[pos] (which is not
necessarily one more than @racket[pos]) or @racket[#f] if @racket[pos]
refers to the last element in @racket[hash].

If @racket[pos] is not a @tech{valid hash index} of @racket[hash],
then the result may be @racket[#f] or it may be the next later index
that remains valid. The latter result is guaranteed if a hash table
has been modified only by the removal of keys.

@history[#:changed "7.0.0.10" @elem{Handle an invalid index by returning @scheme[#f]
                                    instead of raising @racket[exn:fail:contract].}]}


@deftogether[(
@defproc[(hash-iterate-key [hash hash?]
                           [pos exact-nonnegative-integer?])
         any/c]
@defproc[#:link-target? #f
         (hash-iterate-key [hash hash?]
                           [pos exact-nonnegative-integer?]
                           [bad-index-v any/c])
         any/c]
)]{
         
Returns the key for the element in @racket[hash] at index
@racket[pos].

If @racket[pos] is not a @tech{valid hash index} for @racket[hash],
the result is @racket[bad-index-v] if provided, otherwise the
@exnraise[exn:fail:contract].

@history[#:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}]}


@deftogether[(
@defproc[(hash-iterate-value [hash hash?]
                             [pos exact-nonnegative-integer?])
         any]
@defproc[#:link-target? #f
         (hash-iterate-value [hash hash?]
                             [pos exact-nonnegative-integer?]
                             [bad-index-v any/c])
         any]
)]{

Returns the value for the element in @racket[hash] at index
@racket[pos].

If @racket[pos] is not a @tech{valid hash index} for @racket[hash],
the result is @racket[bad-index-v] if provided, otherwise the
@exnraise[exn:fail:contract].

@history[#:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}]}



@deftogether[(
@defproc[(hash-iterate-pair [hash hash?]
                            [pos exact-nonnegative-integer?])
         (cons any/c any/c)]
@defproc[#:link-target? #f
         (hash-iterate-pair [hash hash?]
                            [pos exact-nonnegative-integer?]
                            [bad-index-v any/c])
         (cons any/c any/c)]
)]{

Returns a pair containing the key and value for the element 
in @racket[hash] at index @racket[pos].

If @racket[pos] is not a @tech{valid hash index} for @racket[hash],
the result is @racket[(cons bad-index-v bad-index-v)] if
@racket[bad-index-v] is provided, otherwise the
@exnraise[exn:fail:contract].

@history[#:added "6.4.0.5"
         #:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}]}


@deftogether[(
@defproc[(hash-iterate-key+value [hash hash?]
                                 [pos exact-nonnegative-integer?])
         (values any/c any/c)]
@defproc[#:link-target? #f
         (hash-iterate-key+value [hash hash?]
                                 [pos exact-nonnegative-integer?]
                                 [bad-index-v any/c])
         (values any/c any/c)]
)]{

Returns the key and value for the element in @racket[hash] at index
@racket[pos].

If @racket[pos] is not a @tech{valid hash index} for @racket[hash],
the result is @racket[(values bad-index-v bad-index-v)] if
@racket[bad-index-v] is provided, otherwise the
@exnraise[exn:fail:contract].

@history[#:added "6.4.0.5"
         #:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}]}


@defproc[(hash-copy [hash hash?]) 
         (and/c hash? (not/c immutable?))]{

Returns a mutable hash table with the same mappings, same
key-comparison mode, and same key-holding strength as @racket[hash].}

@;------------------------------------------------------------------------
@section{Additional Hash Table Functions}

@note-lib-only[racket/hash]

@(require (for-label racket/hash))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/hash))

@defproc[(hash-union [h0 (and/c hash? immutable?)]
                     [h hash?] ...
                     [#:combine combine
                                (-> any/c any/c any/c)
                                (lambda _ (error 'hash-union ....))]
                     [#:combine/key combine/key
                                    (-> any/c any/c any/c any/c)
                                    (lambda (k a b) (combine a b))])
         (and/c hash? immutable?)]{

Computes the union of @racket[h0] with each hash table @racket[h] by functional
update, adding each element of each @racket[h] to @racket[h0] in turn.  For each
key @racket[k] and value @racket[v], if a mapping from @racket[k] to some value
@racket[v0] already exists, it is replaced with a mapping from @racket[k] to
@racket[(combine/key k v0 v)].

@examples[
#:eval the-eval
(hash-union (make-immutable-hash '([1 . one]))
            (make-immutable-hash '([2 . two]))
            (make-immutable-hash '([3 . three])))
(hash-union (make-immutable-hash '([1 . (one uno)] [2 . (two dos)]))
            (make-immutable-hash '([1 . (eins un)] [2 . (zwei deux)]))
            #:combine/key (lambda (k v1 v2) (append v1 v2)))
]

}

@defproc[(hash-union! [h0 (and/c hash? (not/c immutable?))]
                      [h hash?] ...
                      [#:combine combine
                                 (-> any/c any/c any/c)
                                 (lambda _ (error 'hash-union ....))]
                      [#:combine/key combine/key
                                     (-> any/c any/c any/c any/c)
                                     (lambda (k a b) (combine a b))])
         void?]{

Computes the union of @racket[h0] with each hash table @racket[h] by mutable
update, adding each element of each @racket[h] to @racket[h0] in turn.  For each
key @racket[k] and value @racket[v], if a mapping from @racket[k] to some value
@racket[v0] already exists, it is replaced with a mapping from @racket[k] to
@racket[(combine/key k v0 v)].

@examples[
#:eval the-eval
(define h (make-hash))
h
(hash-union! h (make-immutable-hash '([1 . (one uno)] [2 . (two dos)])))
h
(hash-union! h
             (make-immutable-hash '([1 . (eins un)] [2 . (zwei deux)]))
             #:combine/key (lambda (k v1 v2) (append v1 v2)))
h
]

}

@defproc[(hash-intersect [h0 (and/c hash? immutable?)]
			 [h hash?] ...
                         [#:combine combine
                                    (-> any/c any/c any/c)
                                    (lambda _ (error 'hash-intersect ...))]
                         [#:combine/key combine/key
                                     	(-> any/c any/c any/c any/c)
                                     	(lambda (k a b) (combine a b))])
	 (and/c hash? immutable?)]{

Constructs the hash table which is the intersection of @racket[h0]
with every hash table @racket[h].  In the resulting hash table, a key
@racket[k] is mapped to a combination of the values to which
@racket[k] is mapped in each of the hash tables.  The final values are
computed by stepwise combination of the values appearing in each of
the hash tables by applying @racket[(combine/key k v vi)] or
@racket[(combine v vi)], where @racket[vi] is the value to which
@racket[k] is mapped in the i-th hash table @racket[h], and
@racket[v] is the accumulation of the values from the previous steps.
The comparison predicate of the first argument (@racket[eq?],
@racket[eqv?], @racket[equal-always?], @racket[equal?]) determines the
one for the result.

@examples[
#:eval the-eval
(hash-intersect (make-immutable-hash '((a . 1) (b . 2) (c . 3)))
		(make-immutable-hash '((a . 4) (b . 5)))
		#:combine +)
(hash-intersect (make-immutable-hash '((a . 1) (b . 2) (c . 3)))
		(make-immutable-hash '((a . 4) (b . 5)))
		#:combine/key
		(lambda (k v1 v2) (if (eq? k 'a) (+ v1 v2) (- v1 v2))))
]


@history[#:added "7.9.0.1"]}

@(close-eval the-eval)
