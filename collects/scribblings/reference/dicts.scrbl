#lang scribble/doc
@(require "mz.rkt" scribble/eval (for-label racket/generic))

@(define dict-eval (make-base-eval))
@(interaction-eval #:eval dict-eval (require racket/dict racket/generic))

@title[#:tag "dicts"]{Dictionaries}

A @deftech{dictionary} is an instance of a datatype that maps keys to
values. The following datatypes are all dictionaries:

@itemize[

 @item{@techlink{hash tables};}

 @item{@techlink{vectors} (using only exact integers as keys);}

 @item{@techlink{lists} of @techlink{pairs} (an @deftech{association
       list} using @racket[equal?] to compare keys); and}

 @item{@techlink{structures} whose types implement the @racket[gen:dict]
       @tech{generic interface}.}

]

A dictionary can be used as a two-valued sequence (see
@secref["sequences"]). The associations of the dictionary serve as elements
of the sequence. See also @racket[in-dict], @racket[in-dict-keys], and @racket[in-dict-values].

@note-lib[racket/dict]

@defproc[(dict? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{dictionary}, @racket[#f]
otherwise.

Beware that @racket[dict?] is not a constant-time test on pairs, since
checking that @racket[v] is an @tech{association list} may require
traversing the list.

@examples[
#:eval dict-eval
(dict? #hash((a . "apple")))
(dict? '#("apple" "banana"))
(dict? '("apple" "banana"))
(dict? '((a . "apple") (b . "banana")))
]}


@defproc[(dict-mutable? [d dict?]) boolean?]{

Returns @racket[#t] if @racket[d] is mutable via @racket[dict-set!]
and maybe @racket[dict-remove!], @racket[#f] otherwise.

@examples[
#:eval dict-eval
(dict-mutable? #hash((a . "apple")))
(dict-mutable? (make-hash))
(dict-mutable? '#("apple" "banana"))
(dict-mutable? (vector "apple" "banana"))
(dict-mutable? '((a . "apple") (b . "banana")))
]}



@defproc[(dict-can-remove-keys? [d dict?]) boolean?]{

Returns @racket[#t] if @racket[d] supports removing mappings via
@racket[dict-remove!] and/or @racket[dict-remove], @racket[#f]
otherwise.

@examples[
#:eval dict-eval
(dict-can-remove-keys? #hash((a . "apple")))
(dict-can-remove-keys? '#("apple" "banana"))
(dict-can-remove-keys? '((a . "apple") (b . "banana")))
]}


@defproc[(dict-can-functional-set? [d dict?]) boolean?]{

Returns @racket[#t] if @racket[d] supports functional update via
@racket[dict-set] and maybe @racket[dict-remove], @racket[#f]
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

Maps @racket[key] to @racket[v] in @racket[dict], overwriting any
existing mapping for @racket[key]. The update can fail with a
@racket[exn:fail:contract] exception if @racket[dict] is not mutable
or if @racket[key] is not an allowed key for the dictionary (e.g., not
an exact integer in the appropriate range when @racket[dict] is a
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

Maps each @racket[key] to each @racket[v] in @racket[dict], overwriting any
existing mapping for each @racket[key]. The update can fail with a
@racket[exn:fail:contract] exception if @racket[dict] is not mutable
or if any @racket[key] is not an allowed key for the dictionary (e.g., not
an exact integer in the appropriate range when @racket[dict] is a
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

Functionally extends @racket[dict] by mapping @racket[key] to
@racket[v], overwriting any existing mapping for @racket[key], and
returning an extended dictionary. The update can fail with a
@racket[exn:fail:contract] exception if @racket[dict] does not support
functional extension or if @racket[key] is not an allowed key for the
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

Functionally extends @racket[dict] by mapping each @racket[key] to
each @racket[v], overwriting any existing mapping for each @racket[key], and
returning an extended dictionary. The update can fail with a
@racket[exn:fail:contract] exception if @racket[dict] does not support
functional extension or if any @racket[key] is not an allowed key for the
dictionary. The update takes place from the left, so later mappings overwrite
earlier mappings.

@examples[
#:eval dict-eval
(dict-set* #hash() 'a "apple" 'b "beer")
(dict-set* #hash((a . "apple") (b . "beer")) 'b "banana" 'a "anchor")
(dict-set* '() 'a "apple" 'b "beer")
(dict-set* '((a . "apple") (b . "beer")) 'b "banana" 'a "anchor")
(dict-set* '((a . "apple") (b . "beer")) 'b "banana" 'b "ballistic")
]}

@defproc[(dict-has-key? [dict dict?] [key any/c])
         boolean?]{

Returns @racket[#t] if @racket[dict] contains a value for the given
@racket[key], @racket[#f] otherwise.

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

Returns the value for @racket[key] in @racket[dict]. If no value
is found for @racket[key], then @racket[failure-result] determines the
result: 

@itemize[

 @item{If @racket[failure-result] is a procedure, it is called
       (through a tail call) with no arguments to produce the result.}

 @item{Otherwise, @racket[failure-result] is returned as the result.}

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

Returns the value for @racket[key] in @racket[dict]. If no value
is found for @racket[key], then @racket[to-set] determines the
result as in @racket[dict-ref] (i.e., it is either a thunk that computes a value
or a plain value), and this result is stored in @racket[dict] for the
@racket[key].  (Note that if @racket[to-set] is a thunk, it is not
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

Composes @racket[dict-ref] and @racket[dict-set!] to update an
existing mapping in @racket[dict], where the optional @racket[failure-result]
argument is used as in @racket[dict-ref] when no mapping exists for 
@racket[key] already.

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

Composes @racket[dict-ref] and @racket[dict-set] to functionally
update an existing mapping in @racket[dict], where the optional @racket[failure-result]
argument is used as in @racket[dict-ref] when no mapping exists for 
@racket[key] already.

@examples[
#:eval dict-eval
(dict-update #hash() 'a add1)
(dict-update #hash() 'a add1 0)
(dict-update #hash((a . "apple") (b . "beer")) 'b string-length)
]}


@defproc[(dict-remove! [dict (and/c dict? (not/c immutable?))]
                       [key any/c])
         void?]{

Removes any existing mapping for @racket[key] in @racket[dict]. The
update can fail if @racket[dict] is not mutable or does not support
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

Functionally removes any existing mapping for @racket[key] in
@racket[dict], returning the fresh dictionary.  The update can fail
if @racket[dict] does not support functional update or does not
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

Applies the procedure @racket[proc] to each element in
@racket[dict] in an unspecified order, accumulating the results
into a list. The procedure @racket[proc] is called each time with a
key and its value.

@examples[
#:eval dict-eval
(dict-map #hash((a . "apple") (b . "banana")) vector)
]}


@defproc[(dict-for-each [dict dict?]
                        [proc (any/c any/c . -> . any)])
         void?]{

Applies @racket[proc] to each element in @racket[dict] (for the
side-effects of @racket[proc]) in an unspecified order. The procedure
@racket[proc] is called each time with a key and its value.

@examples[
#:eval dict-eval
(dict-for-each #hash((a . "apple") (b . "banana")) 
               (lambda (k v)
                 (printf "~a = ~s\n" k v)))
]}


@defproc[(dict-count [dict dict?])
         exact-nonnegative-integer?]{

Returns the number of keys mapped by @racket[dict], usually in
constant time.

@examples[
#:eval dict-eval
(dict-count #hash((a . "apple") (b . "banana")))
(dict-count #("apple" "banana"))
]}


@defproc[(dict-iterate-first [dict dict?]) any/c]{

Returns @racket[#f] if @racket[dict] contains no elements, otherwise
it returns a non-@racket[#f] value that is an index to the first
element in the dict table; ``first'' refers to an unspecified ordering
of the dictionary elements. For a mutable @racket[dict], this index is
guaranteed to refer to the first item only as long as no mappings are
added to or removed from @racket[dict].

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

Returns either a non-@racket[#f] that is an index to the element in
@racket[dict] after the element indexed by @racket[pos] or @racket[#f]
if @racket[pos] refers to the last element in @racket[dict]. If
@racket[pos] is not a valid index, then the
@exnraise[exn:fail:contract]. For a mutable @racket[dict], the result
index is guaranteed to refer to its item only as long as no items are
added to or removed from @racket[dict]. The @racket[dict-iterate-next]
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

Returns the key for the element in @racket[dict] at index
@racket[pos]. If @racket[pos] is not a valid index for @racket[dict],
the @exnraise[exn:fail:contract]. The @racket[dict-iterate-key]
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

Returns the value for the element in @racket[dict] at index
@racket[pos]. If @racket[pos] is not a valid index for @racket[dict],
the @exnraise[exn:fail:contract]. The @racket[dict-iterate-key]
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
@racket[dict].

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([(k v) (in-dict h)])
  (format "~a = ~s" k v))
]}


@defproc[(in-dict-keys [dict dict?]) sequence?]{
Returns a sequence whose elements are the keys of @racket[dict].

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([k (in-dict-keys h)])
  k)
]}

@defproc[(in-dict-values [dict dict?]) sequence?]{
Returns a sequence whose elements are the values of @racket[dict].

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([v (in-dict-values h)])
  v)
]}

@defproc[(in-dict-pairs [dict dict?]) sequence?]{ Returns a sequence
whose elements are pairs, each containing a key and its value from
@racket[dict] (as opposed to using @racket[in-dict], which gets the
key and value as separate values for each element).

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(for/list ([p (in-dict-pairs h)])
  p)
]}

@defproc[(dict-keys [dict dict?]) list?]{ 
Returns a list of the keys from
@racket[dict] in an unspecified order.

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(dict-keys h)
]}

@defproc[(dict-values [dict dict?]) list?]{ 
Returns a list of the values from
@racket[dict] in an unspecified order.

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(dict-values h)
]}

@defproc[(dict->list [dict dict?]) list?]{ 
Returns a list of the associations from
@racket[dict] in an unspecified order.

@examples[
#:eval dict-eval
(define h #hash((a . "apple") (b . "banana")))
(dict->list h)
]}

@defthing[gen:dict any/c]{

A @tech{generic interface} (see @secref["struct-generics"]) that
supplies dictionary method implementations for a structure type.
To supply method implementations, the @racket[#:methods] form should be used.
The provided implementations are applied only to instances of the structure
type. The following methods can be implemented:

@itemize[

 @item{@racket[dict-ref] : accepts either two or three arguments}

 @item{@racket[dict-set!] : accepts three arguments, left unimplemented if
       mutation is not supported}

 @item{@racket[dict-set] : accepts three arguments and returns an updated
       dictionary, left unimplemented if functional update is not supported}

 @item{@racket[dict-remove!] : accepts two arguments, left unimplemented if
       mutation is not supported or if key removal is not supported}

 @item{@racket[dict-remove] : accepts two arguments and returns an updated
       dictionary, left unimplemented if functional update or key removal is
       not supported}

 @item{@racket[dict-count] : accepts one argument}

 @item{@racket[dict-iterate-first] : accepts one argument}

 @item{@racket[dict-iterate-next] : accepts two arguments; the
       procedure is responsible for checking that the second argument
       is a valid position for the first argument}

 @item{@racket[dict-iterate-key] : accepts two arguments; the
       procedure is responsible for checking that the second argument
       is a valid position for the first argument}

 @item{@racket[dict-iterate-value] : accepts two arguments; the
       procedure is responsible for checking that the second argument
       is a valid position for the first argument}
]

@examples[#:eval dict-eval
(struct alist (v)
  #:methods gen:dict
  [(define (dict-ref dict key
                     [default (lambda () (error "key not found" key))])
     (cond [(assoc key (alist-v dict)) => cdr]
           [else (if (procedure? default) (default) default)]))
   (define (dict-set dict key val)
     (alist (cons (cons key val) (alist-v dict))))
   (define (dict-remove dict key)
     (define al (alist-v dict))
     (remove* (assoc key al) al))
   (define (dict-count dict #:default [x #f])
     (or x
         (length (remove-duplicates (alist-v dict) #:key car))))]) (code:comment "etc. other methods")

  (define d1 (alist '((1 . a) (2 . b))))
  (dict? d1)
  (dict-ref d1 1)
]

}

@defthing[prop:dict struct-type-property?]{
  A deprecated structure type property used to define custom extensions
  to the dictionary API. Use @racket[gen:dict] instead. Accepts a vector
  of 10 procedures with the same arguments as the methods of
  @racket[gen:dict].
}

@defthing[prop:dict/contract struct-type-property?]{

A structure type property for defining dictionaries with
contracts. The value associated with @racket[prop:dict/contract] must
be a list of two immutable vectors:

@racketblock[
(list _dict-vector
      (vector _type-key-contract
              _type-value-contract
              _type-iter-contract
              _instance-key-contract
              _instance-value-contract
              _instance-iter-contract))
]

The first vector must be a vector of 10 procedures which match the
@racket[gen:dict] @tech{generic interface} (in addition, it must be an
immutable vector). The second vector must contain six elements; each
of the first three is a contract for the dictionary type's keys,
values, and positions, respectively. Each of the second three is
either @racket[#f] or a procedure used to extract the contract from
a dictionary instance.
}

@deftogether[[
@defproc[(dict-key-contract [d dict?]) contract?]
@defproc[(dict-value-contract [d dict?]) contract?]
@defproc[(dict-iter-contract [d dict?]) contract?]]]{

Returns the contract that @racket[d] imposes on its keys, values, or
iterators, respectively, if @racket[d] implements the
@racket[prop:dict/contract] interface.
}

@;{------------------------------------------------------------}

@deftogether[(
@defproc[(make-custom-hash [eql? (any/c any/c . -> . any/c)]
                           [hash-proc (any/c . -> . exact-integer?)]
                           [hash2-proc (any/c . -> . exact-integer?) (lambda (v) 10001)])
         dict?]
@defproc[(make-immutable-custom-hash [eql? (any/c any/c . -> . any/c)]
                           [hash-proc (any/c . -> . exact-integer?)]
                           [hash2-proc (any/c . -> . exact-integer?) (lambda (v) 10001)])
         dict?]
@defproc[(make-weak-custom-hash [eql? (any/c any/c . -> . any/c)]
                           [hash-proc (any/c . -> . exact-integer?)]
                           [hash2-proc (any/c . -> . exact-integer?) (lambda (v) 10001)])
         dict?]
)]{

Creates a dictionary that is implemented in terms of a hash table
where keys are compared with @racket[eql?] and hashed with
@racket[hash-proc] and @racket[hash2-proc]. See
@racket[gen:equal+hash] for information on suitable equality and
hashing functions.

The @racket[make-custom-hash] and @racket[make-weak-custom-hash]
functions create a mutable dictionary that does not support functional
update, while @racket[make-immutable-custom-hash] creates an immutable
dictionary that supports functional update. The dictionary created by
@racket[make-weak-custom-hash] retains its keys weakly, like the result
of @racket[make-weak-hash].

Dictionaries created by @racket[make-custom-hash] and company are
@racket[equal?] when they have the same mutability and key strength,
the associated procedures are @racket[equal?], and the key--value
mappings are the same when keys and values are compared with
@racket[equal?].

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

