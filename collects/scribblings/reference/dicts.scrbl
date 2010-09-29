#lang scribble/doc
@(require "mz.ss"
          scribble/eval)

@(define dict-eval (make-base-eval))
@(interaction-eval #:eval dict-eval (require racket/dict))

@title[#:tag "dicts"]{Dictionaries}

A @deftech{dictionary} is an instance of a datatype that maps keys to
values. The following datatypes are all dictionaries:

@itemize[

 @item{@techlink{hash tables};}

 @item{@techlink{vectors} (using only exact integers as keys);}

 @item{@techlink{lists} of @techlink{pairs} (an @deftech{association
       list} using @scheme[equal?] to compare keys); and}

 @item{@techlink{structures} whose types have the @scheme[prop:dict]
       property.}

]

A dictionary can be used as a two-valued sequence (see
@secref["sequences"]). The associations of the dictionary serve as elements
of the sequence. See also @scheme[in-dict], @scheme[in-dict-keys], and @scheme[in-dict-values].

@note-lib[racket/dict]

@defproc[(dict? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{dictionary}, @scheme[#f]
otherwise.

Beware that @scheme[dict?] is not a constant-time test on pairs, since
checking that @scheme[v] is an @tech{association list} may require
traversing the list.

@examples[
#:eval dict-eval
(dict? #hash((a . "apple")))
(dict? '#("apple" "banana"))
(dict? '("apple" "banana"))
(dict? '((a . "apple") (b . "banana")))
]}


@defproc[(dict-mutable? [d dict?]) boolean?]{

Returns @scheme[#t] if @scheme[d] is mutable via @scheme[dict-set!]
and maybe @scheme[dict-remove!], @scheme[#f] otherwise.

@examples[
#:eval dict-eval
(dict-mutable? #hash((a . "apple")))
(dict-mutable? (make-hash))
(dict-mutable? '#("apple" "banana"))
(dict-mutable? (vector "apple" "banana"))
(dict-mutable? '((a . "apple") (b . "banana")))
]}



@defproc[(dict-can-remove-keys? [d dict?]) boolean?]{

Returns @scheme[#t] if @scheme[d] supports removing mappings via
@scheme[dict-remove!] and/or @scheme[dict-remove], @scheme[#f]
otherwise.

@examples[
#:eval dict-eval
(dict-can-remove-keys? #hash((a . "apple")))
(dict-can-remove-keys? '#("apple" "banana"))
(dict-can-remove-keys? '((a . "apple") (b . "banana")))
]}


@defproc[(dict-can-functional-set? [d dict?]) boolean?]{

Returns @scheme[#t] if @scheme[d] supports functional update via
@scheme[dict-set] and maybe @scheme[dict-remove], @scheme[#f]
otherwise.

@examples[
#:eval dict-eval
(dict-can-functional-set? #hash((a . "apple")))
(dict-can-functional-set? (make-hash))
(dict-can-functional-set? '#("apple" "banana"))
(dict-can-functional-set? '((a . "apple") (b . "banana")))
]}


@defproc[(dict-set! [dict (and/c dict? (not/c immutable?))]
                    [key any/c]
                    [v any/c]) void?]{

Maps @scheme[key] to @scheme[v] in @scheme[dict], overwriting any
existing mapping for @scheme[key]. The update can fail with a
@scheme[exn:fail:contract] exception if @scheme[dict] is not mutable
or if @scheme[key] is not an allowed key for the dictionary (e.g., not
an exact integer in the appropriate range when @scheme[dict] is a
@tech{vector}).

@examples[
#:eval dict-eval
(define h (make-hash))
(dict-set! h 'a "apple")
h
(define v (vector #f #f #f))
(dict-set! v 0 "apple")
v
]}

@defproc[(dict-set*! [dict (and/c dict? (not/c immutable?))]
                     [key any/c]
                     [v any/c]
                     ...
                     ...) void?]{

Maps each @scheme[key] to each @scheme[v] in @scheme[dict], overwriting any
existing mapping for each @scheme[key]. The update can fail with a
@scheme[exn:fail:contract] exception if @scheme[dict] is not mutable
or if any @scheme[key] is not an allowed key for the dictionary (e.g., not
an exact integer in the appropriate range when @scheme[dict] is a
@tech{vector}). The update takes place from the left, so later mappings overwrite
earlier mappings.

@examples[
#:eval dict-eval
(define h (make-hash))
(dict-set*! h 'a "apple" 'b "banana")
h
(define v1 (vector #f #f #f))
(dict-set*! v1 0 "apple" 1 "banana")
v1
(define v2 (vector #f #f #f))
(dict-set*! v2 0 "apple" 0 "banana")
v2
]}


@defproc[(dict-set [dict (and/c dict? immutable?)]
                   [key any/c]
                   [v any/c])
          (and/c dict? immutable?)]{

Functionally extends @scheme[dict] by mapping @scheme[key] to
@scheme[v], overwriting any existing mapping for @scheme[key], and
returning an extended dictionary. The update can fail with a
@scheme[exn:fail:contract] exception if @scheme[dict] does not support
functional extension or if @scheme[key] is not an allowed key for the
dictionary.

@examples[
#:eval dict-eval
(dict-set #hash() 'a "apple")
(dict-set #hash((a . "apple") (b . "beer")) 'b "banana")
(dict-set '() 'a "apple")
(dict-set '((a . "apple") (b . "beer")) 'b "banana")
]}

@defproc[(dict-set* [dict (and/c dict? immutable?)]
                    [key any/c]
                    [v any/c]
                    ...
                    ...)
          (and/c dict? immutable?)]{

Functionally extends @scheme[dict] by mapping each @scheme[key] to
each @scheme[v], overwriting any existing mapping for each @scheme[key], and
returning an extended dictionary. The update can fail with a
@scheme[exn:fail:contract] exception if @scheme[dict] does not support
functional extension or if any @scheme[key] is not an allowed key for the
dictionary. The update takes place from the left, so later mappings overwrite
earlier mappings.

@examples[
#:eval dict-eval
(dict-set* #hash() 'a "apple" 'b "beer")
(dict-set* #hash((a . "apple") (b . "beer")) 'b "banana" 'a "anchor")
(dict-set* '() 'a "apple" 'b "beer")
(dict-set* '((a . "apple") (b . "beer")) 'b "banana" 'a "anchor")
(dict-set* '((a . "apple") (b . "beer")) 'b "banana" 'b "balistic")
]}

@defproc[(dict-has-key? [dict dict?] [key any/c])
         boolean?]{

Returns @scheme[#t] if @scheme[dict] contains a value for the given
@scheme[key], @scheme[#f] otherwise.

@examples[
#:eval dict-eval
(dict-has-key? #hash((a . "apple") (b . "beer")) 'a)
(dict-has-key? #hash((a . "apple") (b . "beer")) 'c)
(dict-has-key? '((a . "apple") (b . "banana")) 'b)
(dict-has-key? #("apple" "banana") 1)
(dict-has-key? #("apple" "banana") 3)
(dict-has-key? #("apple" "banana") -3)
]}

@defproc[(dict-ref [dict dict?]
                   [key any/c]
                   [failure-result any/c (lambda () (raise (make-exn:fail ....)))])
         any]{

Returns the value for @scheme[key] in @scheme[dict]. If no value
is found for @scheme[key], then @scheme[failure-result] determines the
result: 

@itemize[

 @item{If @scheme[failure-result] is a procedure, it is called
       (through a tail call) with no arguments to produce the result.}

 @item{Otherwise, @scheme[failure-result] is returned as the result.}

]

@examples[
#:eval dict-eval
(dict-ref #hash((a . "apple") (b . "beer")) 'a)
(dict-ref #hash((a . "apple") (b . "beer")) 'c)
(dict-ref #hash((a . "apple") (b . "beer")) 'c #f)
(dict-ref '((a . "apple") (b . "banana")) 'b)
(dict-ref #("apple" "banana") 1)
(dict-ref #("apple" "banana") 3 #f)
(dict-ref #("apple" "banana") -3 #f)
]}

@defproc[(dict-ref! [dict dict?]
                    [key any/c]
                    [to-set any/c])
         any]{

Returns the value for @scheme[key] in @scheme[dict]. If no value
is found for @scheme[key], then @scheme[to-set] determines the
result as in @scheme[dict-ref] (i.e., it is either a thunk that computes a value
or a plain value), and this result is stored in @scheme[dict] for the
@scheme[key].  (Note that if @scheme[to-set] is a thunk, it is not
invoked in tail position.)

@examples[
#:eval dict-eval
(dict-ref! (make-hasheq '((a . "apple") (b . "beer"))) 'a)
(dict-ref! (make-hasheq '((a . "apple") (b . "beer"))) 'c 'cabbage)
(define h (make-hasheq '((a . "apple") (b . "beer"))))
(dict-ref h 'c)
(dict-ref! h 'c (λ () 'cabbage))
(dict-ref h 'c)
]}

@defproc[(dict-update! [dict (and/c dict? (not/c immutable?))]
                       [key any/c]
                       [updater (any/c . -> . any/c)]
                       [failure-result any/c (lambda () (raise (make-exn:fail ....)))]) void?]{

Composes @scheme[dict-ref] and @scheme[dict-set!] to update an
existing mapping in @scheme[dict].

@examples[
#:eval dict-eval
(define h (make-hash))
(dict-update! h 'a add1)
(dict-update! h 'a add1 0)
h
(define v (vector #f #f #f))
(dict-update! v 0 not)
v
]}


@defproc[(dict-update [dict dict?]
                      [key any/c]
                      [updater (any/c . -> . any/c)]
                      [failure-result any/c (lambda () (raise (make-exn:fail ....)))])
          (and/c dict? immutable?)]{

Composes @scheme[dict-ref] and @scheme[dict-set] to functionally
update an existing mapping in @scheme[dict].

@examples[
#:eval dict-eval
(dict-update #hash() 'a add1)
(dict-update #hash() 'a add1 0)
(dict-update #hash((a . "apple") (b . "beer")) 'b string-length)
]}


@defproc[(dict-remove! [dict (and/c dict? (not/c immutable?))]
                       [key any/c])
         void?]{

Removes any existing mapping for @scheme[key] in @scheme[dict]. The
update can fail if @scheme[dict] is not mutable or does not support
removing keys (as is the case for @tech{vectors}, for example).

@examples[
#:eval dict-eval
(define h (make-hash))
(dict-set! h 'a "apple")
h
(dict-remove! h 'a)
h]}


@defproc[(dict-remove [dict (and/c dict? immutable?)]
                      [key any/c])
         (and/c dict? immutable?)]{

Functionally removes any existing mapping for @scheme[key] in
@scheme[dict], returning the updated dictionary.  The update can fail
if @scheme[dict] does not support functional update or does not
support removing keys.

@examples[
#:eval dict-eval
(define h #hash())
(define h (dict-set h 'a "apple"))
h
(dict-remove h 'a)
h
(dict-remove h 'z)
(dict-remove '((a . "apple") (b . "banana")) 'a)
]}


@defproc[(dict-map [dict dict?]
                   [proc (any/c any/c . -> . any/c)])
         (listof any/c)]{

Applies the procedure @scheme[proc] to each element in
@scheme[dict] in an unspecified order, accumulating the results
into a list. The procedure @scheme[proc] is called each time with a
key and its value.

@examples[
#:eval dict-eval
(dict-map #hash((a . "apple") (b . "banana")) vector)
]}


@defproc[(dict-for-each [dict dict?]
                        [proc (any/c any/c . -> . any)])
         void?]{

Applies @scheme[proc] to each element in @scheme[dict] (for the
side-effects of @scheme[proc]) in an unspecified order. The procedure
@scheme[proc] is called each time with a key and its value.

@examples[
#:eval dict-eval
(dict-for-each #hash((a . "apple") (b . "banana")) 
               (lambda (k v)
                 (printf "~a = ~s\n" k v)))
]}


@defproc[(dict-count [dict dict?])
         exact-nonnegative-integer?]{

Returns the number of keys mapped by @scheme[dict], usually in
constant time.

@examples[
#:eval dict-eval
(dict-count #hash((a . "apple") (b . "banana")))
(dict-count #("apple" "banana"))
]}


@defproc[(dict-iterate-first [dict dict?]) any/c]{

Returns @scheme[#f] if @scheme[dict] contains no elements, otherwise
it returns a non-@scheme[#f] value that is a index to the first
element in the dict table; ``first'' refers to an unspecified ordering
of the dictionary elements. For a mutable @scheme[dict], this index is
guaranteed to refer to the first item only as long as no mappings are
added to or removed from @scheme[dict].

@examples[
#:eval dict-eval
(dict-iterate-first #hash((a . "apple") (b . "banana")))
(dict-iterate-first #hash())
(dict-iterate-first #("apple" "banana"))
(dict-iterate-first '((a . "apple") (b . "banana")))
]}


@defproc[(dict-iterate-next [dict dict?]
                            [pos any/c])
         any/c]{

Returns either a non-@scheme[#f] that is an index to the element in
@scheme[dict] after the element indexed by @scheme[pos] or @scheme[#f]
if @scheme[pos] refers to the last element in @scheme[dict]. If
@scheme[pos] is not a valid index, then the
@exnraise[exn:fail:contract]. For a mutable @scheme[dict], the result
index is guaranteed to refer to its item only as long as no items are
added to or removed from @scheme[dict]. The @scheme[dict-iterate-next]
operation should take constant time.

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(define i (dict-iterate-first h))
i
(dict-iterate-next h i)
(dict-iterate-next h (dict-iterate-next h i))
]}


@defproc[(dict-iterate-key [dict dict?]
                           [pos any/c])
         any]{

Returns the key for the element in @scheme[dict] at index
@scheme[pos]. If @scheme[pos] is not a valid index for @scheme[dict],
the @exnraise[exn:fail:contract]. The @scheme[dict-iterate-key]
operation should take constant time.

@examples[
#:eval dict-eval
(define h '((a . "apple") (b . "banana")))
(define i (dict-iterate-first h))
(dict-iterate-key h i)
(dict-iterate-key h (dict-iterate-next h i))
]}



@defproc[(dict-iterate-value [dict dict?]
                             [pos any/c])
         any]{

Returns the value for the element in @scheme[dict] at index
@scheme[pos]. If @scheme[pos] is not a valid index for @scheme[dict],
the @exnraise[exn:fail:contract]. The @scheme[dict-iterate-key]
operation should take constant time.

@examples[
#:eval dict-eval
(define h '((a . "apple") (b . "banana")))
(define i (dict-iterate-first h))
(dict-iterate-value h i)
(dict-iterate-value h (dict-iterate-next h i))
]}


@defproc[(in-dict [dict dict?]) sequence?]{ Returns a @tech{sequence}
whose each element is two values: a key and corresponding value from
@scheme[dict].

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([(k v) (in-dict h)])
  (format "~a = ~s" k v))
]}


@defproc[(in-dict-keys [dict dict?]) sequence?]{
Returns a sequence whose elements are the keys of @scheme[dict].

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([k (in-dict-keys h)])
  k)
]}

@defproc[(in-dict-values [dict dict?]) sequence?]{
Returns a sequence whose elements are the values of @scheme[dict].

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([v (in-dict-values h)])
  v)
]}

@defproc[(in-dict-pairs [dict dict?]) sequence?]{ Returns a sequence
whose elements are pairs, each containing a key and its value from
@scheme[dict] (as opposed to using @scheme[in-dict], which gets the
key and value as separate values for each element).

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([p (in-dict-pairs h)])
  p)
]}

@defproc[(dict-keys [dict dict?]) list?]{ 
Returns a list of the keys from
@scheme[dict] in an unspecified order.

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(dict-keys h)
]}

@defproc[(dict-values [dict dict?]) list?]{ 
Returns a list of the values from
@scheme[dict] in an unspecified order.

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(dict-values h)
]}

@defproc[(dict->list [dict dict?]) list?]{ 
Returns a list of the associations from
@scheme[dict] in an unspecified order.

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(dict->list h)
]}

@defthing[prop:dict struct-type-property?]{

A @tech{structure type property} (see @secref["structprops"]) that
supplies dictionary-operation implementations for a structure
type. The property value must be a vector of ten procedures (some
optional) that are applied only to instances of the structure type
that has the property:

@itemize[

 @item{@scheme[_ref] : a procedure like @scheme[dict-ref] that accepts
       either two or three arguments}

 @item{@scheme[_set!] : a procedure like @scheme[dict-set!] that accepts
       three arguments, or @scheme[#f] if mutation is not supported}

 @item{@scheme[_set] : a procedure like @scheme[dict-set] that accepts
       three arguments and returns an updated dictionary, or
       @scheme[#f] if functional update is not supported}

 @item{@scheme[_remove!] : a procedure like @scheme[dict-remove!] that
       accepts two arguments, or @scheme[#f] if mutation is not
       supported or if key removal is not supported}

 @item{@scheme[_remove] : a procedure like @scheme[dict-remove] that
       accepts two arguments and returns an updated dictionary, or
       @scheme[#f] if functional update or key removal is not
       supported}

 @item{@scheme[_count] : a procedure like @scheme[dict-count] that accepts
       one argument}

 @item{@scheme[_iterate-first] : a procedure like
       @scheme[dict-iterate-first] that accepts one argument}

 @item{@scheme[_iterate-next] : a procedure like
       @scheme[dict-iterate-next] that accepts two arguments; the
       procedure is responsible for checking that the second argument
       is a valid position for the first argument}

 @item{@scheme[_iterate-key] : a procedure like
       @scheme[dict-iterate-key] that accepts two arguments; the
       procedure is responsible for checking that the second argument
       is a valid position for the first argument}

 @item{@scheme[_iterate-value] : a procedure like
       @scheme[dict-iterate-value] that accepts two arguments; the
       procedure is responsible for checking that the second argument
       is a valid position for the first argument}

]}

@;{

@defthing[prop:dict/contract]{


}

@deftogether[[
@defproc[(dict-key-contract [d dict?]) contract?]
@defproc[(dict-value-contract [d dict?]) contract?]]]{

Returns the contract that @racket[d] imposes on its keys or values,
respectively, if @racket[d] implements the @racket[prop:dict/contract]
interface.
}
}

@;{------------------------------------------------------------}

@deftogether[(
@defproc[(make-custom-hash [eql? (any/c any/c . -> . any/c)]
                           [hash-proc (any/c . -> . exact-integer?)]
                           [hash2-proc (any/c . -> . exact-integer?)])
         dict?]
@defproc[(make-immutable-custom-hash [eql? (any/c any/c . -> . any/c)]
                           [hash-proc (any/c . -> . exact-integer?)]
                           [hash2-proc (any/c . -> . exact-integer?)])
         dict?]
@defproc[(make-weak-custom-hash [eql? (any/c any/c . -> . any/c)]
                           [hash-proc (any/c . -> . exact-integer?)]
                           [hash2-proc (any/c . -> . exact-integer?)])
         dict?]
)]{

Creates a dictionary that is implemented in terms of a hash table
where keys are compared with @scheme[eql?] and hashed with
@scheme[hash-proc] and @scheme[hash2-proc]. See
@scheme[prop:equal+hash] for information on suitable equality and
hashing functions.

The @scheme[make-custom-hash] and @scheme[make-weak-custom-hash]
functions create a mutable dictionary that does not support functional
update, while @scheme[make-immutable-custom-hash] creates an immutable
dictionary that supports functional update. The dictionary created by
@scheme[make-weak-custom-hash] retains its keys weakly, like the result
of @scheme[make-weak-hash].

Dictionaries created by @scheme[make-custom-hash] and company are
@scheme[equal?] when they have the same mutability and key strength,
the associated procedures are @scheme[equal?], and the key--value
mappings are the same when keys and values are compared with
@scheme[equal?].

@examples[
#:eval dict-eval
(define h (make-custom-hash (lambda (a b)
                              (string=? (format "~a" a)
                                        (format "~a" b)))
                            (lambda (a)
                              (equal-hash-code 
                               (format "~a" a)))))
(dict-set! h 1 'one)
(dict-ref h "1")
]}

@; ----------------------------------------------------------------------

@close-eval[dict-eval]

