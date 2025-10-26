#lang scribble/doc
@(require "mz.rkt" (for-label racket/set))

@title[#:tag "sets"]{Sets}
@(define set-eval (make-base-eval))
@examples[#:hidden #:eval set-eval (require racket/set)]

@(define (hash-set-caveats)
   @elem{For @tech{hash sets}, see also the @concurrency-caveat[]
         for hash tables, which applies to
         hash sets.})

A @deftech{set} represents a collection of distinct elements.  The following
datatypes are all sets:

@itemize[

  @item{@techlink{hash sets};}

  @item{@techlink{lists} using @racket[equal?] to compare elements; and}

  @item{@techlink{structures} whose types implement the @racket[gen:set]
        @tech{generic interface}.}

]

@note-lib[racket/set]

@section{Hash Sets}

A @deftech{hash set} is a set whose elements are compared via @racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?] and partitioned
via @racket[equal-hash-code], @racket[equal-always-hash-code],
@racket[eqv-hash-code], or @racket[eq-hash-code].  A hash set is either
immutable or mutable; mutable hash sets retain their elements either strongly
or weakly.

@margin-note{Like operations on immutable hash tables, ``constant time'' hash
set operations actually require @math{O(log N)} time for a set of size
@math{N}.}

A hash set can be used as a @tech{stream} (see @secref["streams"]) and thus as
a single-valued @tech{sequence} (see @secref["sequences"]). The elements of the
set serve as elements of the stream or sequence. If an element is added to or
removed from the hash set during iteration, then an iteration step may fail
with @racket[exn:fail:contract], or the iteration may skip or duplicate
elements. See also @racket[in-set].

Two hash sets are @racket[equal?] when they use the same element-comparison
procedure (@racket[equal?], @racket[equal-always?], @racket[eqv?], or
@racket[eq?]), both hold elements strongly or weakly, have the same
mutability, and have equivalent elements.
Immutable hash sets support effectively constant-time access and
update, just like mutable hash sets; the constant on immutable operations is
usually larger, but the functional nature of immutable hash sets can pay off in
certain algorithms.

All hash sets @impl{implement} @racket[set->stream],
@racket[set-empty?], @racket[set-member?], @racket[set-count],
@racket[subset?], @racket[proper-subset?], @racket[set-map],
@racket[set-for-each], @racket[set-copy], @racket[set-copy-clear],
@racket[set->list], and @racket[set-first].  Immutable hash sets in
addition @impl{implement} @racket[set-add], @racket[set-remove],
@racket[set-clear], @racket[set-union], @racket[set-intersect],
@racket[set-subtract], and @racket[set-symmetric-difference].  Mutable
hash sets in addition @impl{implement} @racket[set-add!],
@racket[set-remove!], @racket[set-clear!], @racket[set-union!],
@racket[set-intersect!], @racket[set-subtract!], and
@racket[set-symmetric-difference!].

Operations on sets that contain elements that are mutated are
unpredictable in much the same way that @tech{hash table} operations are
unpredictable when keys are mutated.

@deftogether[(
@defproc[(set-equal? [x any/c]) boolean?]
@defproc[(set-equal-always? [x any/c]) boolean?]
@defproc[(set-eqv? [x any/c]) boolean?]
@defproc[(set-eq? [x any/c]) boolean?]
)]{

Returns @racket[#t] if @racket[x] is a @tech{hash set} that compares
elements with @racket[equal?], @racket[equal-always?], @racket[eqv?],
or @racket[eq?], respectively; returns @racket[#f] otherwise.

@history[#:changed "8.5.0.3" @elem{Added @racket[set-equal-always?].}]}

@deftogether[(
@defproc[(set? [x any/c]) boolean?]
@defproc[(set-mutable? [x any/c]) boolean?]
@defproc[(set-weak? [x any/c]) boolean?]
)]{

Returns @racket[#t] if @racket[x] is a @tech{hash set} that is respectively
immutable, mutable with strongly-held keys, or mutable with weakly-held keys;
returns @racket[#f] otherwise.

}

@deftogether[(
@defproc[(set [v any/c] ...) (and/c generic-set? set-equal? set?)]
@defproc[(setalw [v any/c] ...)
         (and/c generic-set? set-equal-always? set?)]
@defproc[(seteqv [v any/c] ...) (and/c generic-set? set-eqv? set?)]
@defproc[(seteq [v any/c] ...) (and/c generic-set? set-eq? set?)]
@defproc[(mutable-set [v any/c] ...) (and/c generic-set? set-equal? set-mutable?)]
@defproc[(mutable-setalw [v any/c] ...)
         (and/c generic-set? set-equal-always? set-mutable?)]
@defproc[(mutable-seteqv [v any/c] ...) (and/c generic-set? set-eqv? set-mutable?)]
@defproc[(mutable-seteq [v any/c] ...) (and/c generic-set? set-eq? set-mutable?)]
@defproc[(weak-set [v any/c] ...) (and/c generic-set? set-equal? set-weak?)]
@defproc[(weak-setalw [v any/c] ...)
         (and/c generic-set? set-equal-always? set-weak?)]
@defproc[(weak-seteqv [v any/c] ...) (and/c generic-set? set-eqv? set-weak?)]
@defproc[(weak-seteq [v any/c] ...) (and/c generic-set? set-eq? set-weak?)]
)]{

Creates a @tech{hash set} with the given @racket[v]s as elements.  The
elements are added in the order that they appear as arguments, so in the case
of sets that use @racket[equal?], @racket[equal-always?], or @racket[eqv?],
an earlier element may be replaced by a later element that is
@racket[equal?], @racket[equal-always?], or @racket[eqv?], but not
@racket[eq?].

@history[#:changed "8.5.0.3" @elem{Added @racket[setalw],
                                   @racket[mutable-setalw], and @racket[weak-setalw].}]}

@deftogether[(
@defproc[(list->set [lst list?]) (and/c generic-set? set-equal? set?)]
@defproc[(list->setalw [lst list?])
         (and/c generic-set? set-equal-always? set?)]
@defproc[(list->seteqv [lst list?]) (and/c generic-set? set-eqv? set?)]
@defproc[(list->seteq [lst list?]) (and/c generic-set? set-eq? set?)]
@defproc[(list->mutable-set [lst list?]) (and/c generic-set? set-equal? set-mutable?)]
@defproc[(list->mutable-setalw [lst list?])
         (and/c generic-set? set-equal-always? set-mutable?)]
@defproc[(list->mutable-seteqv [lst list?]) (and/c generic-set? set-eqv? set-mutable?)]
@defproc[(list->mutable-seteq [lst list?]) (and/c generic-set? set-eq? set-mutable?)]
@defproc[(list->weak-set [lst list?]) (and/c generic-set? set-equal? set-weak?)]
@defproc[(list->weak-setalw [lst list?])
         (and/c generic-set? set-equal-always? set-weak?)]
@defproc[(list->weak-seteqv [lst list?]) (and/c generic-set? set-eqv? set-weak?)]
@defproc[(list->weak-seteq [lst list?]) (and/c generic-set? set-eq? set-weak?)]
)]{

Creates a @tech{hash set} with the elements of the given @racket[lst] as
the elements of the set.  Equivalent to @racket[(apply set lst)],
@racket[(apply setalw lst)], @racket[(apply seteqv lst)],
@racket[(apply seteq lst)], and so on, respectively.

@history[#:changed "8.5.0.3" @elem{Added @racket[list->setalw],
                                   @racket[list->mutable-setalw], and @racket[list->weak-setalw].}]}

@deftogether[(
@defform[(for/set (for-clause ...) body ...+)]
@defform[(for/seteq (for-clause ...) body ...+)]
@defform[(for/seteqv (for-clause ...) body ...+)]
@defform[(for/setalw (for-clause ...) body ...+)]
@defform[(for*/set (for-clause ...) body ...+)]
@defform[(for*/seteq (for-clause ...) body ...+)]
@defform[(for*/seteqv (for-clause ...) body ...+)]
@defform[(for*/setalw (for-clause ...) body ...+)]
@defform[(for/mutable-set (for-clause ...) body ...+)]
@defform[(for/mutable-seteq (for-clause ...) body ...+)]
@defform[(for/mutable-seteqv (for-clause ...) body ...+)]
@defform[(for/mutable-setalw (for-clause ...) body ...+)]
@defform[(for*/mutable-set (for-clause ...) body ...+)]
@defform[(for*/mutable-seteq (for-clause ...) body ...+)]
@defform[(for*/mutable-seteqv (for-clause ...) body ...+)]
@defform[(for*/mutable-setalw (for-clause ...) body ...+)]
@defform[(for/weak-set (for-clause ...) body ...+)]
@defform[(for/weak-seteq (for-clause ...) body ...+)]
@defform[(for/weak-seteqv (for-clause ...) body ...+)]
@defform[(for/weak-setalw (for-clause ...) body ...+)]
@defform[(for*/weak-set (for-clause ...) body ...+)]
@defform[(for*/weak-seteq (for-clause ...) body ...+)]
@defform[(for*/weak-seteqv (for-clause ...) body ...+)]
@defform[(for*/weak-setalw (for-clause ...) body ...+)]
)]{

Analogous to @racket[for/list] and @racket[for*/list], but to
construct a @tech{hash set} instead of a list.

@history[#:changed "8.5.0.3" @elem{Added @racket[for/setalw],
                                   @racket[for/mutable-setalw], and @racket[for/weak-setalw].}]}


@deftogether[(
@defproc[(in-immutable-set [st set?]) sequence?]
@defproc[(in-mutable-set [st set-mutable?]) sequence?]
@defproc[(in-weak-set [st set-weak?]) sequence?]
)]{

Explicitly converts a specific kind of @tech{hash set} to a sequence for 
use with @racket[for] forms.

As with @racket[in-list] and some other sequence constructors,
@racket[in-immutable-set] performs better when it appears directly in a
@racket[for] clause.

These sequence constructors are compatible with
@secref["Custom_Hash_Sets" #:doc '(lib "scribblings/reference/reference.scrbl")].

@history[#:added "6.4.0.7"]
}

@section{Set Predicates and Contracts}

@defproc[(generic-set? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{set}; returns @racket[#f]
otherwise.

@examples[
#:eval set-eval
(generic-set? (list 1 2 3))
(generic-set? (set 1 2 3))
(generic-set? (mutable-seteq 1 2 3))
(generic-set? (vector 1 2 3))
]

}

@defproc[(set-implements? [st generic-set?] [sym symbol?] ...) boolean?]{

Returns @racket[#t] if @racket[st] implements all of the methods from
@racket[gen:set] named by the @racket[sym]s; returns @racket[#f] otherwise.
Fallback implementations do not affect the result; @racket[st] may support the
given methods via fallback implementations yet produce @racket[#f].

@examples[
#:eval set-eval
(set-implements? (list 1 2 3) 'set-add)
(set-implements? (list 1 2 3) 'set-add!)
(set-implements? (set 1 2 3) 'set-add)
(set-implements? (set 1 2 3) 'set-add!)
(set-implements? (mutable-seteq 1 2 3) 'set-add)
(set-implements? (mutable-seteq 1 2 3) 'set-add!)
(set-implements? (weak-seteqv 1 2 3) 'set-remove 'set-remove!)
]

}

@defproc[(set-implements/c [sym symbol?] ...) flat-contract?]{

Recognizes sets that support all of the methods from @racket[gen:set]
named by the @racket[sym]s.

}

@defproc[(set/c [elem/c chaperone-contract?]
                [#:cmp cmp
                 (or/c 'dont-care 'equal 'equal-always 'eqv 'eq)
                 'dont-care]
                [#:kind kind 
                 (or/c 'dont-care 'immutable 'mutable 'weak 'mutable-or-weak)
                 'immutable]
                [#:lazy? lazy? any/c
                 (not (and (equal? kind 'immutable)
                           (flat-contract? elem/c)))]
                [#:equal-key/c equal-key/c contract? any/c])
         contract?]{

  Constructs a contract that recognizes sets whose elements match
  @racket[elem/c].

  If @racket[kind] is @racket['immutable], @racket['mutable], or
  @racket['weak], the resulting contract accepts only @tech{hash sets} that
  are respectively immutable, mutable with strongly-held keys, or mutable with
  weakly-held keys.  If @racket[kind] is @racket['mutable-or-weak], the
  resulting contract accepts any mutable @tech{hash sets}, regardless of
  key-holding strength.

  If @racket[cmp] is @racket['equal], @racket['equal-always], @racket['eqv],
  or @racket['eq], the resulting contract accepts only @tech{hash sets} that
  compare elements using @racket[equal?], @racket[equal-always?],
  @racket[eqv?], or @racket[eq?], respectively.

  If @racket[cmp] is @racket['eqv] or @racket['eq], then @racket[elem/c] must
  be a @tech{flat contract}.

  If @racket[cmp] and @racket[kind] are both @racket['dont-care], then the
  resulting contract will accept any kind of set, not just @tech{hash
  sets}.

 If @racket[lazy?] is not @racket[#f], then the elements of the set are not checked
 immediately by the contract and only the set itself is checked (according to the
 @racket[cmp] and @racket[kind] arguments). If @racket[lazy?] is
 @racket[#f], then the elements are checked immediately by the contract.
 The @racket[lazy?] argument is ignored when the set contract accepts generic sets
 (i.e., when @racket[cmp] and @racket[kind] are both @racket['dont-care]); in that
 case, the value being checked in that case is a @racket[list?], then the contract
 is not lazy otherwise the contract is lazy.
 
 If @racket[kind] allows mutable sets (i.e., is @racket['dont-care],
 @racket['mutable], @racket['weak], or
 @racket['mutable-or-weak]) and @racket[lazy?] is @racket[#f], then the elements
 are checked both immediately and when they are accessed from the set.

 The @racket[equal-key/c] contract is used when values are passed to the comparison
 and hashing functions used internally.
 
 The result contract will be a @tech{flat contract} when @racket[elem/c]
 and @racket[equal-key/c] are both @tech{flat contracts},
 @racket[lazy?] is @racket[#f], and @racket[kind] is @racket['immutable].
 The result will be a @tech{chaperone contract} when @racket[elem/c] is a
 @tech{chaperone contract}.

 @history[#:changed "8.3.0.9" @elem{Added support for random generation.}
          #:changed "8.5.0.3" @elem{Added @racket['equal-always] support for @racket[cmp].}]
}

@section{Generic Set Interface}


@defidform[gen:set]{

A @tech{generic interface} (see @secref["struct-generics"]) that
supplies set method implementations for a structure type via the
@racket[#:methods] option of @racket[struct] definitions.  This interface can
be used to implement any of the methods documented as
@secref["set-methods"].

A set should also be a @tech{sequence}, but @racket[gen:set] by itself
does not by itself imply @racket[prop:sequence]. A use of
@racket[gen:set] typically should be combined with a use of
@racket[prop:sequence] with @racket[in-set] or a more specific sequence
constructor. Note that @racket[in-set] requires @supp{supporting}
@racket[set->stream] (e.g., by implementing @racket[set->stream] or
another supporting combination, such as @racket[set-first], @racket[set-remove], and
@racket[set-empty?]).

@examples[
#:eval set-eval
(struct binary-set [integer]
  #:transparent
  #:methods gen:set
  [(define (set-member? st i)
     (bitwise-bit-set? (binary-set-integer st) i))
   (define (set-add st i)
     (binary-set (bitwise-ior (binary-set-integer st)
                              (arithmetic-shift 1 i))))
   (define (set-remove st i)
     (binary-set (bitwise-and (binary-set-integer st)
                              (bitwise-not (arithmetic-shift 1 i)))))
   (define (set-first st)
     (sub1 (integer-length (binary-set-integer st))))
   (define (set-empty? st)
     (= (binary-set-integer st) 0))]
  #:property prop:sequence in-set)
(define bset (binary-set 5))
bset
(generic-set? bset)
(set-member? bset 0)
(set-member? bset 1)
(set-member? bset 2)
(set-add bset 4)
(set-remove bset 2)
(set-first bset)
(require racket/sequence)
(sequence->list bset)
]

}

@subsection[#:tag "set-methods"]{Set Methods}

@(define (supp . args) (apply tech #:key "supported generic method" args))
@(define (impl . args) (apply tech #:key "implemented generic method" args))

The methods of @racket[gen:set] can be classified into three categories, as determined by their fallback implementations:

@itemlist[#:style 'ordered
          @item{methods with no fallbacks,}
          @item{methods whose fallbacks depend on other, non-fallback methods,}
          @item{and methods whose fallbacks can depend on either fallback or non-fallback methods.}]

As an example, implementing the following methods would guarantee that all the methods in @racket[gen:set] would at least have a fallback method:

@itemlist[@item{@racket[set-member?]}
          @item{@racket[set-add]}
          @item{@racket[set-add!]}
          @item{@racket[set-remove]}
          @item{@racket[set-remove!]}
          @item{@racket[set-first]}
          @item{@racket[set-empty?]}
          @item{@racket[set-copy-clear]}]

There may be other such subsets of methods that would guarantee at least a fallback for every method.

@defproc[(set-member? [st generic-set?] [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is in @racket[st], @racket[#f]
otherwise. Has no fallback.

}

@defproc[(set-add [st generic-set?] [v any/c]) generic-set?]{

Produces a set that includes @racket[v] plus all elements of
@racket[st]. This operation runs in constant time for @tech{hash sets}. Has no fallback.

}

@defproc[(set-add! [st generic-set?] [v any/c]) void?]{

Adds the element @racket[v] to @racket[st].  This operation runs in constant
time for @tech{hash sets}. Has no fallback.

@hash-set-caveats[]}


@defproc[(set-remove [st generic-set?] [v any/c]) generic-set?]{

Produces a set that includes all elements of @racket[st] except
@racket[v]. This operation runs in constant time for @tech{hash sets}. Has no fallback.

}

@defproc[(set-remove! [st generic-set?] [v any/c]) void?]{

Removes the element @racket[v] from @racket[st].  This operation runs in constant
time for @tech{hash sets}. Has no fallback.

@hash-set-caveats[]}


@defproc[(set-empty? [st generic-set?]) boolean?]{

Returns @racket[#t] if @racket[st] has no members; returns @racket[#f]
otherwise.

Supported for any @racket[st] that @impl{implements} @racket[set->stream] or
@racket[set-count].

}

@defproc[(set-count [st generic-set?]) exact-nonnegative-integer?]{

Returns the number of elements in @racket[st].

Supported for any @racket[st] that @supp{supports} @racket[set->stream].

}

@defproc[(set-first [st (and/c generic-set? (not/c set-empty?))]) any/c]{

Produces an unspecified element of @racket[st]. Multiple uses of
@racket[set-first] on @racket[st] produce the same result.

Supported for any @racket[st] that @impl{implements} @racket[set->stream].

}


@defproc[(set-rest [st (and/c generic-set? (not/c set-empty?))]) generic-set?]{

Produces a set that includes all elements of @racket[st] except
@racket[(set-first st)].

Supported for any @racket[st] that @impl{implements} @racket[set-remove] and either
@racket[set-first] or @racket[set->stream].

}

@defproc[(set->stream [st generic-set?]) stream?]{

Produces a stream containing the elements of @racket[st].

Supported for any @racket[st] that @impl{implements}:
@itemlist[@item{@racket[set->list]}
          @item{@racket[in-set]}
          @item{@racket[set-empty?], @racket[set-first], @racket[set-rest]}
          @item{@racket[set-empty?], @racket[set-first], @racket[set-remove]}
          @item{@racket[set-count], @racket[set-first], @racket[set-rest]}
          @item{@racket[set-count], @racket[set-first], @racket[set-remove]}]
}

@defproc[(set-copy [st generic-set?]) generic-set?]{

Produces a new, mutable set of the same type and with the same elements as
@racket[st].

Supported for any @racket[st] that @supp{supports} @racket[set->stream] and 
@impl{implements} @racket[set-copy-clear] and @racket[set-add!].

}

@defproc[(set-copy-clear [st generic-set?]) (and/c generic-set? set-empty?)]{

Produces a new, empty set of the same type, mutability, and key strength as
@racket[st].

A difference between @racket[set-copy-clear] and @racket[set-clear] is
that the latter conceptually iterates @racket[set-remove] on the given
set, and so it preserves any contract on the given set. The
@racket[set-copy-clear] function produces a new set without any
contracts.

The @racket[set-copy-clear] function must call concrete set constructors
and thus has no generic fallback.
}

@defproc[(set-clear [st generic-set?]) (and/c generic-set? set-empty?)]{

Produces a set like @racket[st] but with all elements removed.

Supported for any @racket[st] that @impl{implements} @racket[set-remove] and @supp{supports}
@racket[set->stream].

}

@defproc[(set-clear! [st generic-set?]) void?]{

Removes all elements from @racket[st].

Supported for any @racket[st] that @impl{implements} @racket[set-remove!] and either
@supp{supports} @racket[set->stream] or @impl{implements} @racket[set-first] and either @racket[set-count] or @racket[set-empty?].

@hash-set-caveats[]}


@defproc[(set-union [st0 generic-set?] [st generic-set?] ...) generic-set?]{

Produces a set of the same type as @racket[st0] that includes the elements from
@racket[st0] and all of the @racket[st]s.

If @racket[st0] is a list, each @racket[st] must also be a list.  This
operation runs on lists in time proportional to the total size of the
@racket[st]s times the size of the result.

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
total size of all of the sets except the largest immutable set.

At least one set must be provided to @racket[set-union] to determine the type
of the resulting set (list, hash set, etc.).  If there is a case where
@racket[set-union] may be applied to zero arguments, instead pass an empty set
of the intended type as the first argument.

Supported for any @racket[st] that @impl{implements}  @racket[set-add] and @supp{supports} @racket[set->stream].

@examples[#:eval set-eval
(set-union (set))
(set-union (seteq))
(set-union (set 1 2) (set 2 3))
(set-union (list 1 2) (list 2 3))
(eval:error (set-union (set 1 2) (seteq 2 3))) (code:comment "Sets of different types cannot be unioned")
]}

@defproc[(set-union! [st0 generic-set?] [st generic-set?] ...) void?]{

Adds the elements from all of the @racket[st]s to @racket[st0].

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
total size of the @racket[st]s.

Supported for any @racket[st] that @impl{implements} @racket[set-add!] and @supp{supports} @racket[set->stream].

@hash-set-caveats[]}

@defproc[(set-intersect [st0 generic-set?] [st generic-set?] ...) generic-set?]{

Produces a set of the same type as @racket[st0] that includes the elements from
@racket[st0] that are also contained by all of the @racket[st]s.

If @racket[st0] is a list, each @racket[st] must also be a list.  This
operation runs on lists in time proportional to the total size of the
@racket[st]s times the size of @racket[st0].

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
size of the smallest immutable set.

Supported for any @racket[st] that @impl{implements} either @racket[set-remove] or
both @racket[set-clear] and @racket[set-add], and @supp{supports} @racket[set->stream].

}

@defproc[(set-intersect! [st0 generic-set?] [st generic-set?] ...) void?]{

Removes every element from @racket[st0] that is not contained by all of the
@racket[st]s.

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
size of @racket[st0].

Supported for any @racket[st] that @impl{implements} @racket[set-remove!] and @supp{supports} @racket[set->stream].

@hash-set-caveats[]}


@defproc[(set-subtract [st0 generic-set?] [st generic-set?] ...) generic-set?]{

Produces a set of the same type as @racket[st0] that includes the elements from
@racket[st0] that are not contained by any of the @racket[st]s.

If @racket[st0] is a list, each @racket[st] must also be a list.  This
operation runs on lists in time proportional to the total size of the
@racket[st]s times the size of @racket[st0].

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
size of @racket[st0].

Supported for any @racket[st] that @impl{implements} either @racket[set-remove] or
both @racket[set-clear] and @racket[set-add], and @supp{supports} @racket[set->stream].

}

@defproc[(set-subtract! [st0 generic-set?] [st generic-set?] ...) void?]{

Removes every element from @racket[st0] that is contained by any of the
@racket[st]s.

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
size of @racket[st0].

Supported for any @racket[st] that @impl{implements} @racket[set-remove!] and @supp{supports} @racket[set->stream].

@hash-set-caveats[]}


@defproc[(set-symmetric-difference [st0 generic-set?] [st generic-set?] ...) generic-set?]{

Produces a set of the same type as @racket[st0] that includes all of the
elements contained an odd number of times in @racket[st0] and the
@racket[st]s.

If @racket[st0] is a list, each @racket[st] must also be a list.  This
operation runs on lists in time proportional to the total size of the
@racket[st]s times the size of @racket[st0].

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
total size of all of the sets except the largest immutable set.

Supported for any @racket[st] that @impl{implements} @racket[set-remove] or both @racket[set-clear] and @racket[set-add], and @supp{supports} @racket[set->stream].

@examples[#:eval set-eval
(set-symmetric-difference (set 1) (set 1 2) (set 1 2 3))
]

}

@defproc[(set-symmetric-difference! [st0 generic-set?] [st generic-set?] ...) void?]{

Adds and removes elements of @racket[st0] so that it includes all of the
elements contained an odd number of times in the @racket[st]s and the
original contents of @racket[st0].

If @racket[st0] is a @tech{hash set}, each @racket[st] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
total size of the @racket[st]s.

Supported for any @racket[st] that @impl{implements} @racket[set-remove!] and @supp{supports} @racket[set->stream].

@hash-set-caveats[]}


@defproc[(set=? [st generic-set?] [st2 generic-set?]) boolean?]{

Returns @racket[#t] if @racket[st] and @racket[st2] contain the same
members; returns @racket[#f] otherwise.

If @racket[st] is a list, then @racket[st2] must also be a list.  This
operation runs on lists in time proportional to the size of @racket[st] times
the size of @racket[st2].

If @racket[st] is a @tech{hash set}, then @racket[st2] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
size of @racket[st] plus the size of @racket[st2].

Supported for any @racket[st] and @racket[st2] that both @supp{support}
@racket[subset?]; also supported for any if @racket[st2] that @impl{implements}
@racket[set=?] regardless of @racket[st].

@examples[#:eval set-eval
(set=? (list 1 2) (list 2 1))
(set=? (set 1) (set 1 2 3))
(set=? (set 1 2 3) (set 1))
(set=? (set 1 2 3) (set 1 2 3))
(set=? (seteq 1 2) (mutable-seteq 2 1))
(eval:error (set=? (seteq 1 2) (seteqv 1 2))) (code:comment "Sets of different types cannot be compared")
]

}

@defproc[(subset? [st generic-set?] [st2 generic-set?]) boolean?]{

@index["set-subset?"]{Returns} @racket[#t] if @racket[st2] contains every member of @racket[st];
returns @racket[#f] otherwise.

If @racket[st] is a list, then @racket[st2] must also be a list.  This
operation runs on lists in time proportional to the size of @racket[st] times
the size of @racket[st2].

If @racket[st] is a @tech{hash set}, then @racket[st2] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
size of @racket[st].

Supported for any @racket[st] that @supp{supports} @racket[set->stream].

@examples[#:eval set-eval
(subset? (set 1) (set 1 2 3))
(subset? (set 1 2 3) (set 1))
(subset? (set 1 2 3) (set 1 2 3))
]

}

@defproc[(proper-subset? [st generic-set?] [st2 generic-set?]) boolean?]{

Returns @racket[#t] if @racket[st2] contains every member of @racket[st] and at
least one additional element; returns @racket[#f] otherwise.

If @racket[st] is a list, then @racket[st2] must also be a list.  This
operation runs on lists in time proportional to the size of @racket[st] times
the size of @racket[st2].

If @racket[st] is a @tech{hash set}, then @racket[st2] must also be a
@tech{hash set} that uses the same comparison function (@racket[equal?],
@racket[equal-always?], @racket[eqv?], or @racket[eq?]).
The mutability and key strength of the hash
sets may differ.  This operation runs on hash sets in time proportional to the
size of @racket[st] plus the size of @racket[st2].

Supported for any @racket[st] and @racket[st2] that both @supp{support}
@racket[subset?].

@examples[#:eval set-eval
(proper-subset? (set 1) (set 1 2 3))
(proper-subset? (set 1 2 3) (set 1))
(proper-subset? (set 1 2 3) (set 1 2 3))
]

}

@defproc[(set->list [st generic-set?]) list?]{

Produces a list containing the elements of @racket[st].

Supported for any @racket[st] that @supp{supports} @racket[set->stream].

}

@defproc[(set-map [st generic-set?]
                  [proc (any/c . -> . any/c)])
         (listof any/c)]{

Applies the procedure @racket[proc] to each element in
@racket[st] in an unspecified order, accumulating the results
into a list.

Supported for any @racket[st] that @supp{supports} @racket[set->stream].

}


@defproc[(set-for-each [st generic-set?]
                       [proc (any/c . -> . any)])
         void?]{

Applies @racket[proc] to each element in @racket[st] (for the
side-effects of @racket[proc]) in an unspecified order.

Supported for any @racket[st] that @supp{supports} @racket[set->stream].

}

@defproc[(in-set [st generic-set?]) sequence?]{

Explicitly converts a set to a sequence for use with @racket[for] and
other forms.

Supported for any @racket[st] that @supp{supports} @racket[set->stream].

}

@defproc[(impersonate-hash-set [st (or/c set-mutable? set-weak?)]
                               [inject-proc (or/c #f (-> set? any/c any/c))]
                               [add-proc (or/c #f (-> set? any/c any/c))]
                               [shrink-proc (or/c #f (-> set? any/c any/c))]
                               [extract-proc (or/c #f (-> set? any/c any/c))]
                               [clear-proc (or/c #f (-> set? any)) #f]
                               [equal-key-proc (or/c #f (-> set? any/c any/c)) #f]
                               [prop impersonator-property?]
                               [prop-val any/c] ... ...)
         (and/c (or/c set-mutable? set-weak?) impersonator?)]{
 Impersonates @racket[st], redirecting various set operations via the given procedures.

 The @racket[inject-proc] procedure
 is called whenever an element is temporarily put into the set for the purposes
 of comparing it with other elements that may already be in the set. For example,
 when evaluating @racket[(set-member? s e)], @racket[e] will be passed to the
 @racket[inject-proc] before comparing it with other elements of @racket[s].

 The @racket[add-proc] procedure is called when adding an element to a set, e.g.,
 via @racket[set-add] or @racket[set-add!]. The result of the @racket[add-proc] is
 stored in the set.

 The @racket[shrink-proc] procedure is called when building a new set with
 one fewer element. For example, when evaluating @racket[(set-remove s e)]
 or @racket[(set-remove! s e)],
 an element is removed from a set, e.g.,
 via @racket[set-remove] or @racket[set-remove!]. The result of the @racket[shrink-proc]
 is the element actually removed from the set.
 
 The @racket[extract-proc] procedure is called when an element is pulled out of
 a set, e.g., by @racket[set-first]. The result of the @racket[extract-proc] is
 the element actually produced by from the set.

 The @racket[clear-proc] is called by @racket[set-clear] and @racket[set-clear!]
 and if it returns (as opposed to escaping, perhaps via raising an exception),
 the clearing operation is permitted. Its result is ignored. If @racket[clear-proc]
 is @racket[#f], then clearing is done element by element (via calls into the other
 supplied procedures).

 The @racket[equal-key-proc] is called when an element's hash code is needed of when an
 element is supplied to the underlying equality in the set. The result of
 @racket[equal-key-proc] is used when computing the hash or comparing for equality.
 
 If any of the @racket[inject-proc], @racket[add-proc], @racket[shrink-proc], or
 @racket[extract-proc] arguments are  @racket[#f], then they all must be @racket[#f],
 the @racket[clear-proc] and @racket[equal-key-proc] must also be @racket[#f],
 and there must be at least one property supplied.
 
 Pairs of @racket[prop] and @racket[prop-val] (the number of arguments to
 @racket[impersonate-hash-set] must be odd) add @tech{impersonator properties} or
 override impersonator property values of @racket[st].
}

@defproc[(chaperone-hash-set [st (or/c set? set-mutable? set-weak?)]
                             [inject-proc (or/c #f (-> set? any/c any/c))]
                             [add-proc (or/c #f (-> set? any/c any/c))]
                             [shrink-proc (or/c #f (-> set? any/c any/c))]
                             [extract-proc (or/c #f (-> set? any/c any/c))]
                             [clear-proc (or/c #f (-> set? any)) #f]
                             [equal-key-proc (or/c #f (-> set? any/c any/c)) #f]
                             [prop impersonator-property?]
                             [prop-val any/c] ... ...)
         (and/c (or/c set? set-mutable? set-weak?) chaperone?)]{
 Chaperones @racket[st]. Like @racket[impersonate-hash-set] but with
 the constraints that the results of the @racket[inject-proc],
 @racket[add-proc], @racket[shrink-proc], @racket[extract-proc], and
 @racket[equal-key-proc] must be
 @racket[chaperone-of?] their second arguments. Also, the input
 may be an @racket[immutable?] set.
}

@section{Custom Hash Sets}

@defform[(define-custom-set-types name 
                                  optional-predicate
                                  comparison-expr
                                  optional-hash-functions)
         #:grammar ([optional-predicate
                     (code:line)
                     (code:line #:elem? predicate-expr)]
                    [optional-hash-functions
                     (code:line)
                     (code:line hash1-expr)
                     (code:line hash1-expr hash2-expr)])]{

Creates a new hash set type based on the given comparison @racket[comparison-expr],
hash functions @racket[hash1-expr] and @racket[hash2-expr], and element
predicate @racket[predicate-expr]; the interfaces for these functions are the
same as in @racket[make-custom-set-types].  The new set type has three
variants: immutable, mutable with strongly-held elements, and mutable with
weakly-held elements.

Defines seven names:

@itemize[
@item{@racket[name]@racketidfont{?} recognizes instances of the new type,}
@item{@racketidfont{immutable-}@racket[name]@racketidfont{?} recognizes
      immutable instances of the new type,}
@item{@racketidfont{mutable-}@racket[name]@racketidfont{?} recognizes
      mutable instances of the new type with strongly-held elements,}
@item{@racketidfont{weak-}@racket[name]@racketidfont{?} recognizes
      mutable instances of the new type with weakly-held elements,}
@item{@racketidfont{make-immutable-}@racket[name] constructs
      immutable instances of the new type,}
@item{@racketidfont{make-mutable-}@racket[name] constructs
      mutable instances of the new type with strongly-held elements, and}
@item{@racketidfont{make-weak-}@racket[name] constructs
      mutable instances of the new type with weakly-held elements.}
]

The constructors all accept a stream as an optional argument, providing
initial elements.

@examples[
#:eval set-eval
(define-custom-set-types string-set
                         #:elem? string?
                         string=?
                         string-length)
(define imm
  (make-immutable-string-set '("apple" "banana")))
(define mut
  (make-mutable-string-set '("apple" "banana")))
(generic-set? imm)
(generic-set? mut)
(set? imm)
(generic-set? imm)
(string-set? imm)
(string-set? mut)
(immutable-string-set? imm)
(immutable-string-set? mut)
(set-member? imm "apple")
(set-member? mut "banana")
(equal? imm mut)
(set=? imm mut)
(set-remove! mut "banana")
(set-member? mut "banana")
(equal? (set-remove (set-remove imm "apple") "banana")
        (make-immutable-string-set))
]

}

@defproc[(make-custom-set-types
          [eql?
           (or/c (any/c any/c . -> . any/c)
                 (any/c any/c (any/c any/c . -> . any/c) . -> . any/c))]
          [hash1
           (or/c (any/c . -> . exact-integer?)
                 (any/c (any/c . -> . exact-integer?) . -> . exact-integer?))
           (const 1)]
          [hash2
           (or/c (any/c . -> . exact-integer?)
                 (any/c (any/c . -> . exact-integer?) . -> . exact-integer?))
           (const 1)]
          [#:elem? elem? (any/c . -> . boolean?) (const #true)]
          [#:name name symbol? 'custom-set]
          [#:for who symbol? 'make-custom-set-types])
         (values (any/c . -> . boolean?)
                 (any/c . -> . boolean?)
                 (any/c . -> . boolean?)
                 (any/c . -> . boolean?)
                 (->* [] [stream?] generic-set?)
                 (->* [] [stream?] generic-set?)
                 (->* [] [stream?] generic-set?))]{

Creates a new set type based on the given comparison function @racket[eql?],
hash functions @racket[hash1] and @racket[hash2], and predicate @racket[elem?].
The new set type has variants that are immutable, mutable with strongly-held
elements, and mutable with weakly-held elements.  The given @racket[name] is
used when printing instances of the new set type, and the symbol @racket[who]
is used for reporting errors.

The comparison function @racket[eql?] may accept 2 or 3 arguments.  If it
accepts 2 arguments, it given two elements to compare them.  If it accepts 3
arguments and does not accept 2 arguments, it is also given a recursive
comparison function that handles data cycles when comparing sub-parts of the
elements.

The hash functions @racket[hash1] and @racket[hash2] may accept 1 or 2
arguments.  If either hash function accepts 1 argument, it is applied to a
element to compute the corresponding hash value.  If either hash function
accepts 2 arguments and does not accept 1 argument, it is also given a
recursive hash function that handles data cycles when computing hash values of
sub-parts of the elements.

The predicate @racket[elem?] must accept 1 argument and is used to recognize
valid elements for the new set type.

Produces seven values:

@itemize[
@item{a predicate recognizing all instances of the new set type,}
@item{a predicate recognizing weak instances,}
@item{a predicate recognizing mutable instances,}
@item{a predicate recognizing immutable instances,}
@item{a constructor for weak instances,}
@item{a constructor for mutable instances, and}
@item{a constructor for immutable instances.}
]

See @racket[define-custom-set-types] for an example.

}

@close-eval[set-eval]
