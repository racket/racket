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

  @item{The @racket[hash-map], @racket[hash-for-each], and @racket[hash-clear!] procedures do
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
keys weakly.

Beware that values in the table are retained normally. If a value in
the table refers back to its key, then the table will retain the value
and therefore the key; the mapping will never be removed from the
table even if the key becomes otherwise inaccessible. To avoid that
problem, instead of mapping the key to the value, map the key to an
@tech{ephemeron} that pairs the key and value.}

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
                   [failure-result (failure-result/c any/c)
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
                       [failure-result (failure-result/c any/c)
                                       (lambda ()
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
                      [failure-result (failure-result/c any/c)
                                      (lambda ()
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


@defproc[(hash-copy-clear [hash hash?]) hash?]{

Produces an empty @tech{hash table} with the same key-comparison
procedure and mutability of @racket[hash].}



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

If @racket[try-order?] is true, then the order of keys and values
passed to @racket[proc] is normalized under certain circumstances,
such as when the keys are all symbols and @racket[hash] is not an
@tech{impersonator}.

@see-also-concurrency-caveat[]

@history[#:changed "6.3" @elem{Added the @racket[try-order?] argument.}]}

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
                        [proc (any/c any/c . -> . any)]
                        [try-order? any/c #f])
         void?]{

Applies @racket[proc] to each element in @racket[hash] (for the
side-effects of @racket[proc]) in an unspecified order. The procedure
@racket[proc] is called each time with a key and its value.

See @racket[hash-map] for information about @racket[try-order?] and
about modifying @racket[hash] within @racket[proc].
@see-also-concurrency-caveat[]

@history[#:changed "6.3" @elem{Added the @racket[try-order?] argument.}]}


@defproc[(hash-count [hash hash?])
         exact-nonnegative-integer?]{

Returns the number of keys mapped by @racket[hash]. Unless @racket[hash]
retains keys weakly, the result is computed in
constant time and atomically. If @racket[hash] retains it keys weakly, a
traversal is required to count the keys.}


@defproc[(hash-empty? [hash hash?]) boolean?]{

Equivalent to @racket[(zero? (hash-count hash))].}


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

@defproc[(hash-iterate-pair [hash hash?]
                           [pos exact-nonnegative-integer?])
         (cons any any)]{

Returns a pair containing the key and value for the element 
in @racket[hash] at index
@racket[pos]. If @racket[pos] is not a valid index for
@racket[hash], the @exnraise[exn:fail:contract].}

@history[#:added "6.4.0.5"]

@defproc[(hash-iterate-key+value [hash hash?]
                           [pos exact-nonnegative-integer?])
         (values any any)]{

Returns the key and value for the element in @racket[hash] at index
@racket[pos]. If @racket[pos] is not a valid index for
@racket[hash], the @exnraise[exn:fail:contract].}

@history[#:added "6.4.0.5"]

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
inspectable structure fields. See also @racket[gen:equal+hash].

For any @racket[v] that could be produced by @racket[read], if
@racket[v2] is produced by @racket[read] for the same input
characters, the @racket[(equal-hash-code v)] is the same as
@racket[(equal-hash-code v2)] --- even if @racket[v] and @racket[v2]
do not exist at the same time (and therefore could not be compared by
calling @racket[equal?]).

@history[#:changed "6.4.0.12" @elem{Strengthened guarantee for @racket[read]able values.}]}


@defproc[(equal-secondary-hash-code [v any/c]) fixnum?]{

Like @racket[equal-hash-code], but computes a secondary value suitable
for use in double hashing.}

@;------------------------------------------------------------------------
@section{Additional Hash Table Functions}

@note-lib-only[racket/hash]

@(require (for-label racket/hash))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/hash))

@defproc[(hash-union [h0 (and/c hash? hash-can-functional-set?)]
                     [h hash?] ...
                     [#:combine combine
                                (-> any/c any/c any/c)
                                (lambda _ (error 'hash-union ....))]
                     [#:combine/key combine/key
                                    (-> any/c any/c any/c any/c)
                                    (lambda (k a b) (combine a b))])
         (and/c hash? hash-can-functional-set?)]{

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
            (make-immutable-hash '([1 . (ein une)] [2 . (zwei deux)]))
            #:combine/key (lambda (k v1 v2) (append v1 v2)))
]

}

@defproc[(hash-union! [h0 (and/c hash? hash-mutable?)]
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
             (make-immutable-hash '([1 . (ein une)] [2 . (zwei deux)]))
             #:combine/key (lambda (k v1 v2) (append v1 v2)))
h
]

}

@(close-eval the-eval)
