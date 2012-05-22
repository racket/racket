#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "hashtables"]{Hash Tables}

@(define (concurrency-caveat)
  @elemref['(caveat "concurrency")]{caveats concerning concurrent modification})
@(define (mutable-key-caveat)
  @elemref['(caveat "mutable-keys")]{caveat concerning mutable keys})

@(define (see-also-caveats)
   @t{See also the @concurrency-caveat[] and the @mutable-key-caveat[] above.})
@(define (see-also-concurrency-caveat)
   @t{See also the @concurrency-caveat[] above.})
@(define (see-also-mutable-key-caveat)
   @t{See also the @mutable-key-caveat[] above.})

@guideintro["hash-tables"]{hash tables}

A @deftech{hash table} (or simply @deftech{hash}) maps each of its
keys to a single value. For a given hash table, keys are equivalent
via @racket[equal?], @racket[eqv?], or @racket[eq?], and keys are
retained either strongly or weakly (see @secref["weakbox"]). A hash
table is also either mutable or immutable. Immutable hash tables
support effectively constant-time access and update, just like mutable
hash tables; the constant on immutable operations is usually larger,
but the functional nature of immutable hash tables can pay off in
certain algorithms.

@margin-note{Immutable hash tables actually provide @math{O(log N)}
access and update. Since @math{N} is limited by the address space so
that @math{log N} is limited to less than 30 or 62 (depending on the
platform), @math{log N} can be treated reasonably as a constant.}

A hash table can be used as a two-valued @tech{sequence} (see
@secref["sequences"]). The keys and values of the hash table serve as
elements of the sequence (i.e., each element is a key and its
associated value). If a mapping is added to or removed from the hash
table during iteration, then an iteration step may fail with
@racket[exn:fail:contract], or the iteration may skip or duplicate
keys and values.  See also @racket[in-hash], @racket[in-hash-keys],
@racket[in-hash-values], and @racket[in-hash-pairs].

Two hash tables cannot be @racket[equal?] unless they use the same
key-comparison procedure (@racket[equal?], @racket[eqv?], or
@racket[eq?]), both hold keys strongly or weakly, and have the same
mutability.

@elemtag['(caveat "concurrency")]{@bold{Caveats concerning concurrent
modification:}} A mutable hash table can be manipulated with
@racket[hash-ref], @racket[hash-set!], and @racket[hash-remove!]
concurrently by multiple threads, and the operations are protected by
a table-specific semaphore as needed. Three caveats apply, however:

 @itemize[

  @item{If a thread is terminated while applying @racket[hash-ref],
  @racket[hash-set!], @racket[hash-remove!], @racket[hash-ref!],
  or @racket[hash-update!] to a hash table that
  uses @racket[equal?] or @racket[eqv?] key comparisons, all current
  and future operations on the hash table may block indefinitely.}

  @item{The @racket[hash-map] and @racket[hash-for-each] procedures do
  not use the table's semaphore to guard the traversal as a whole.
  Changes by one thread to a hash table can affect the keys and values
  seen by another thread part-way through its traversal of the same
  hash table.}

 @item{The @racket[hash-update!] and @racket[hash-ref!] functions 
 use a table's semaphore
 independently for the @racket[hash-ref] and @racket[hash-set!] parts
 of their functionality, which means that the update as a whole is not
 ``atomic.''}

 ]

@elemtag['(caveat "mutable-keys")]{@bold{Caveat concerning mutable
keys:}} If a key in an @racket[equal?]-based hash table is mutated
(e.g., a key string is modified with @racket[string-set!]), then the
hash table's behavior for insertion and lookup operations becomes
unpredictable.

A literal or printed hash table starts with @litchar{#hash},
@litchar{#hasheqv}, or
@litchar{#hasheq}. @see-read-print["hashtable"]{hash tables}

@defproc[(hash? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{hash table}, @racket[#f]
otherwise.}

@defproc[(hash-equal? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] compares keys with @racket[equal?],
@racket[#f] if it compares with @racket[eq?] or @racket[eqv?].}

@defproc[(hash-eqv? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] compares keys with @racket[eqv?],
@racket[#f] if it compares with @racket[equal?] or @racket[eq?].}

@defproc[(hash-eq? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] compares keys with @racket[eq?],
@racket[#f] if it compares with @racket[equal?] or @racket[eqv?].}


@defproc[(hash-weak? [hash hash?]) boolean?]{

Returns @racket[#t] if @racket[hash] retains its keys weakly,
@racket[#f] if it retains keys strongly.}

@deftogether[(
@defproc[(hash [key any/c] [val any/c] ... ...) (and/c hash? hash-equal? immutable?)]
@defproc[(hasheq [key any/c] [val any/c] ... ...) (and/c hash? hash-eq? immutable?)]
@defproc[(hasheqv [key any/c] [val any/c] ... ...) (and/c hash? hash-eqv? immutable?)]
)]{

Creates an immutable hash table with each given @racket[key] mapped to
the following @racket[val]; each @racket[key] must have a @racket[val],
so the total number of arguments to @racket[hash] must be even.

The @racket[hash] procedure creates a table where keys are compared
with @racket[equal?], @racket[hasheq] procedure creates a table where
keys are compared with @racket[eq?], and @racket[hasheqv] procedure
creates a table where keys are compared with @racket[eqv?].

The @racket[key] to @racket[val] mappings are added to the table in
the order that they appear in the argument list, so later mappings can
hide earlier mappings if the @racket[key]s are equal.}

@deftogether[(
@defproc[(make-hash [assocs (listof pair?) null]) (and/c hash? hash-equal?)]
@defproc[(make-hasheqv [assocs (listof pair?) null]) (and/c hash? hash-eqv?)]
@defproc[(make-hasheq [assocs (listof pair?) null]) (and/c hash? hash-eq?)]
)]{

Creates a mutable hash table that holds keys strongly. 

The @racket[make-hash] procedure creates a table where keys are
compared with @racket[equal?], @racket[make-hasheq] procedure creates
a table where keys are compared with @racket[eq?], and
@racket[make-hasheqv] procedure creates a table where keys are
compared with @racket[eqv?].

The table is initialized with the content of @racket[assocs].  In each
element of @racket[assocs], the @racket[car] is a key, and the
@racket[cdr] is the corresponding value. The mappings are added to the
table in the order that they appear in @racket[assocs], so later
mappings can hide earlier mappings.

See also @racket[make-custom-hash].}

@deftogether[(
@defproc[(make-weak-hash [assocs (listof pair?) null]) (and/c hash? hash-equal? hash-weak?)]
@defproc[(make-weak-hasheqv [assocs (listof pair?) null]) (and/c hash? hash-eqv? hash-weak?)]
@defproc[(make-weak-hasheq [assocs (listof pair?) null]) (and/c hash? hash-eq? hash-weak?)]
)]{

Like @racket[make-hash], @racket[make-hasheq], and
@racket[make-hasheqv], but creates a mutable hash table that holds
keys weakly.}

@deftogether[(
@defproc[(make-immutable-hash [assocs (listof pair?) null])
         (and/c hash? hash-equal? immutable?)]
@defproc[(make-immutable-hasheqv [assocs (listof pair?) null])
         (and/c hash? hash-eqv? immutable?)]
@defproc[(make-immutable-hasheq [assocs (listof pair?) null])
         (and/c hash? hash-eq? immutable?)]
)]{

Like @racket[hash], @racket[hasheq], and @racket[hasheqv], but accepts
the key--value mapping in association-list form like
@racket[make-hash], @racket[make-hasheq], and @racket[make-hasheqv].}


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
                   [failure-result any/c (lambda () 
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

@see-also-caveats[]}

@defproc[(hash-ref! [hash hash?] [key any/c] [to-set any/c])
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
                       [failure-result any/c (lambda () 
                                               (raise (make-exn:fail:contract ....)))])
         void?]{

Composes @racket[hash-ref] and @racket[hash-set!] to update an
existing mapping in @racket[hash], where the optional
@racket[failure-result] argument is used as in @racket[hash-ref] when
no mapping exists for @racket[key] already. See the caveat above about
concurrent updates.

@see-also-caveats[]}


@defproc[(hash-update [hash (and/c hash? immutable?)]
                      [key any/c]
                      [updater (any/c . -> . any/c)]
                      [failure-result any/c (lambda () 
                                              (raise (make-exn:fail:contract ....)))])
          (and/c hash? immutable?)]{

Composes @racket[hash-ref] and @racket[hash-set] to functionally
update an existing mapping in @racket[hash], where the optional
@racket[failure-result] argument is used as in @racket[hash-ref] when
no mapping exists for @racket[key] already.

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
@racket[hash], returning the fresh hash table.

@see-also-mutable-key-caveat[]}


@defproc[(hash-map [hash hash?]
                   [proc (any/c any/c . -> . any/c)])
         (listof any/c)]{

Applies the procedure @racket[proc] to each element in
@racket[hash] in an unspecified order, accumulating the results
into a list. The procedure @racket[proc] is called each time with a
key and its value.

If a hash table is extended with new keys (either through
@racket[proc] or by another thread) while a @racket[hash-map] or
@racket[hash-for-each] traversal is in process, arbitrary key--value
pairs can be dropped or duplicated in the traversal. Key mappings can
be deleted or remapped (by any thread) with no adverse affects; the
change does not affect a traversal if the key has been seen already,
otherwise the traversal skips a deleted key or uses the remapped key's
new value.

@see-also-concurrency-caveat[]}

@defproc[(hash-keys [hash hash?])
         (listof any/c)]{
Returns a list of the keys of @racket[hash] in an unspecified order.

See @racket[hash-map] for information about modifying @racket[hash]
during @racket[hash-keys]. @see-also-concurrency-caveat[]}

@defproc[(hash-values [hash hash?])
         (listof any/c)]{
Returns a list of the values of @racket[hash] in an unspecified order.

See @racket[hash-map] for information about modifying @racket[hash]
during @racket[hash-values]. @see-also-concurrency-caveat[]}

@defproc[(hash->list [hash hash?])
         (listof (cons/c any/c any/c))]{
Returns a list of the key--value pairs of @racket[hash] in an unspecified order.

See @racket[hash-map] for information about modifying @racket[hash]
during @racket[hash->list]. @see-also-concurrency-caveat[]}

@defproc[(hash-for-each [hash hash?]
                        [proc (any/c any/c . -> . any)])
         void?]{

Applies @racket[proc] to each element in @racket[hash] (for the
side-effects of @racket[proc]) in an unspecified order. The procedure
@racket[proc] is called each time with a key and its value.

See @racket[hash-map] for information about modifying @racket[hash]
within @racket[proc]. @see-also-concurrency-caveat[]}


@defproc[(hash-count [hash hash?])
         exact-nonnegative-integer?]{

Returns the number of keys mapped by @racket[hash]. Unless @racket[hash]
retains keys weakly, the result is computed in
constant time and atomically. If @racket[hash] retains it keys weakly, a
traversal is required to count the keys.}


@defproc[(hash-iterate-first [hash hash?])
         (or/c #f exact-nonnegative-integer?)]{

Returns @racket[#f] if @racket[hash] contains no elements, otherwise
it returns an integer that is an index to the first element in the hash
table; ``first'' refers to an unspecified ordering of the table
elements, and the index values are not necessarily consecutive
integers. For a mutable @racket[hash], this index is guaranteed to
refer to the first item only as long as no items are added to or
removed from @racket[hash].}

@defproc[(hash-iterate-next [hash hash?]
                            [pos exact-nonnegative-integer?])
         (or/c #f exact-nonnegative-integer?)]{

Returns either an integer that is an index to the element in
@racket[hash] after the element indexed by @racket[pos] (which is not
necessarily one more than @racket[pos]) or @racket[#f] if @racket[pos]
refers to the last element in @racket[hash]. If @racket[pos] is not a
valid index, then the @exnraise[exn:fail:contract]. For a mutable
@racket[hash], the result index is guaranteed to refer to its item
only as long as no items are added to or removed from @racket[hash].}


@defproc[(hash-iterate-key [hash hash?]
                           [pos exact-nonnegative-integer?])
         any]{

Returns the key for the element in @racket[hash] at index
@racket[pos]. If @racket[pos] is not a valid index for
@racket[hash], the @exnraise[exn:fail:contract].}


@defproc[(hash-iterate-value [hash hash?]
                             [pos exact-nonnegative-integer?])
         any]{

Returns the value for the element in @racket[hash] at index
@racket[pos]. If @racket[pos] is not a valid index for
@racket[hash], the @exnraise[exn:fail:contract].}


@defproc[(hash-copy [hash hash?]) 
         (and/c hash? (not/c immutable?))]{

Returns a mutable hash table with the same mappings, same
key-comparison mode, and same key-holding strength as @racket[hash].}


@defproc[(eq-hash-code [v any/c]) fixnum?]{

Returns a @tech{fixnum}; for any two calls with @racket[eq?] values,
the returned number is the same.

@margin-note{Equal @tech{fixnums} are always @racket[eq?].}}


@defproc[(eqv-hash-code [v any/c]) fixnum?]{

Returns a @tech{fixnum}; for any two calls with @racket[eqv?] values,
the returned number is the same.}


@defproc[(equal-hash-code [v any/c]) fixnum?]{

Returns a @tech{fixnum}; for any two calls with @racket[equal?] values,
the returned number is the same. A hash code is computed even when
@racket[v] contains a cycle through pairs, vectors, boxes, and/or
inspectable structure fields. See also @racket[gen:equal+hash].}


@defproc[(equal-secondary-hash-code [v any/c]) fixnum?]{

Like @racket[equal-hash-code], but computes a secondary value suitable
for use in double hashing.}
