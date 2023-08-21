#lang scribble/manual
@(require (only-in scribblings/style/shared compare0)
          "mz.rkt"
          (for-label racket/hash-code))


@title{Equality}


Equality is the concept of whether two values are ``the same.'' Racket supports
a few different kinds of equality by default, although @racket[equal?] is
preferred for most uses.

@defproc[(equal? [v1 any/c] [v2 any/c]) boolean?]{

 Two values are @racket[equal?] if and only if they are @racket[eqv?],
 unless otherwise specified for a particular datatype.

 Datatypes with further specification of @racket[equal?] include
 strings, byte strings, pairs, mutable pairs, vectors, boxes, hash
 tables, and inspectable structures. In the last six cases, equality
 is recursively defined; if both @racket[v1] and @racket[v2] contain
 reference cycles, they are equal when the infinite unfoldings of the
 values would be equal. See also @racket[gen:equal+hash] and
 @racket[prop:impersonator-of].

 @(examples
   (equal? 'yes 'yes)
   (equal? 'yes 'no)
   (equal? (* 6 7) 42)
   (equal? (expt 2 100) (expt 2 100))
   (equal? 2 2.0)
   (let ([v (mcons 1 2)]) (equal? v v))
   (equal? (mcons 1 2) (mcons 1 2))
   (equal? (integer->char 955) (integer->char 955))
   (equal? (make-string 3 #\z) (make-string 3 #\z))
   (equal? #t #t))}


@defproc[(equal-always? [v1 any/c] [v2 any/c]) boolean?]{

 Indicates whether @racket[v1] and @racket[v2] are equal and will always stay
 equal independent of @emph{mutations}. Generally, for two values to be equal-always, corresponding
 immutable values within @racket[v1] and @racket[v2] must be @racket[equal?],
 while corresponding mutable values within them must be @racket[eq?].
 @margin-note*{Precedents for this operator in other languages include
 @tt{egal} @cite["Baker93"].}

 Two values @racket[v1] and @racket[v2] are @racket[equal-always?] if and only
 if there exists a third value @racket[_v3] such that @racket[v1] and
 @racket[v2] are both chaperones of @racket[_v3], meaning
 @racket[(chaperone-of? v1 _v3)] and @racket[(chaperone-of? v2 _v3)] are both
 true.

 For values that include no chaperones or other impersonators,
 @racket[v1] and @racket[v2] can be considered equal-always
 if they are @racket[equal?], except that corresponding mutable
 vectors, boxes, hash tables, strings, byte strings, @tech{mutable pairs}, and
 mutable structures within
 @racket[v1] and @racket[v2] must be @racket[eq?], and equality on structures
 can be specialized for @racket[equal-always?] through @racket[gen:equal-mode+hash].

 @(examples
   (equal-always? 'yes 'yes)
   (equal-always? 'yes 'no)
   (equal-always? (* 6 7) 42)
   (equal-always? (expt 2 100) (expt 2 100))
   (equal-always? 2 2.0)
   (equal-always? (list 1 2) (list 1 2))
   (let ([v (mcons 1 2)]) (equal-always? v v))
   (equal-always? (mcons 1 2) (mcons 1 2))
   (equal-always? (integer->char 955) (integer->char 955))
   (equal-always? (make-string 3 #\z) (make-string 3 #\z))
   (equal-always? (string->immutable-string (make-string 3 #\z))
                  (string->immutable-string (make-string 3 #\z)))
   (equal-always? #t #t))

@history[#:added "8.5.0.3"]}


@defproc[(eqv? [v1 any/c] [v2 any/c]) boolean?]{

 Two values are @racket[eqv?] if and only if they are @racket[eq?],
 unless otherwise specified for a particular datatype.

 The @tech{number} and @tech{character} datatypes are the only ones for which
 @racket[eqv?] differs from @racket[eq?]. Two numbers are @racket[eqv?] when
 they have the same exactness, precision, and are both equal and non-zero, both
 @racketvalfont{+0.0}, both @racketvalfont{+0.0f0}, both @racketvalfont{-0.0},
 both @racketvalfont{-0.0f0}, both @racketvalfont{+nan.0}, or both
 @racketvalfont{+nan.f}---considering real and imaginary components separately
 in the case of @tech{complex numbers}. Two characters are @racket[eqv?] when
 their @racket[char->integer] results are equal.

 Generally, @racket[eqv?] is identical to @racket[equal?] except that the former
 cannot recursively compare the contents of compound data types (such as lists
 and structs) and cannot be customized by user-defined data types. The use of
 @racket[eqv?] is lightly discouraged in favor of @racket[equal?].

 @(examples
   (eqv? 'yes 'yes)
   (eqv? 'yes 'no)
   (eqv? (* 6 7) 42)
   (eqv? (expt 2 100) (expt 2 100))
   (eqv? 2 2.0)
   (let ([v (mcons 1 2)]) (eqv? v v))
   (eqv? (mcons 1 2) (mcons 1 2))
   (eqv? (integer->char 955) (integer->char 955))
   (eqv? (make-string 3 #\z) (make-string 3 #\z))
   (eqv? #t #t))}


@defproc[(eq? [v1 any/c] [v2 any/c]) boolean?]{

 Return @racket[#t] if @racket[v1] and @racket[v2] refer to the same
 object, @racket[#f] otherwise. As a special case among @tech{numbers},
 two @tech{fixnums} that are @racket[=] are also the same according
 to @racket[eq?]. See also @secref["model-eq"].

 @(examples
   (eq? 'yes 'yes)
   (eq? 'yes 'no)
   (eq? (* 6 7) 42)
   (eq? (expt 2 100) (expt 2 100))
   (eq? 2 2.0)
   (let ([v (mcons 1 2)]) (eq? v v))
   (eq? (mcons 1 2) (mcons 1 2))
   (eq? (integer->char 955) (integer->char 955))
   (eq? (make-string 3 #\z) (make-string 3 #\z))
   (eq? #t #t))}


@defproc[
 (equal?/recur [v1 any/c] [v2 any/c] [recur-proc (any/c any/c -> any/c)])
 boolean?]{

 Like @racket[equal?], but using @racket[recur-proc] for recursive
 comparisons (which means that reference cycles are not handled
 automatically). Non-@racket[#f] results from @racket[recur-proc] are
 converted to @racket[#t] before being returned by
 @racket[equal?/recur].

 @(examples
   (equal?/recur 1 1 (lambda (a b) #f))
   (equal?/recur '(1) '(1) (lambda (a b) #f))
   (equal?/recur '#(1 1 1) '#(1 1.2 3/4)
                 (lambda (a b) (<= (abs (- a b)) 0.25))))}


@defproc[
 (equal-always?/recur [v1 any/c] [v2 any/c] [recur-proc (any/c any/c -> any/c)])
 boolean?]{

 Like @racket[equal-always?], but using @racket[recur-proc] for recursive
 comparisons (which means that reference cycles are not handled
 automatically). Non-@racket[#f] results from @racket[recur-proc] are
 converted to @racket[#t] before being returned by
 @racket[equal-always?/recur].

 @(examples
   (equal-always?/recur 1 1 (lambda (a b) #f))
   (equal-always?/recur '(1) '(1) (lambda (a b) #f))
   (equal-always?/recur (vector-immutable 1 1 1) (vector-immutable 1 1.2 3/4)
                        (lambda (a b) (<= (abs (- a b)) 0.25))))}


@section[#:tag "model-eq"]{Object Identity and Comparisons}


The @racket[eq?] operator compares two @tech{values}, returning
@racket[#t] when the values refer to the same @tech{object}. This form
of equality is suitable for comparing objects that support imperative
update (e.g., to determine that the effect of modifying an object
through one reference is visible through another reference). Also, an
@racket[eq?] test evaluates quickly, and @racket[eq?]-based hashing
is more lightweight than @racket[equal?]-based hashing in hash tables.

In some cases, however, @racket[eq?] is unsuitable as a comparison
operator, because the generation of @tech{objects} is not clearly
defined. In particular, two applications of @racket[+] to the same two
exact integers may or may not produce results that are @racket[eq?],
although the results are always @racket[equal?]. Similarly, evaluation
of a @racket[lambda] form typically generates a new procedure
@tech{object}, but it may re-use a procedure @tech{object} previously
generated by the same source @racket[lambda] form.

The behavior of a datatype with respect to @racket[eq?] is generally
specified with the datatype and its associated procedures.


@section{Equality and Hashing}


All comparable values have at least one @deftech{hash code} --- an arbitrary
integer (more specifically a @tech{fixnum}) computed by applying a hash function
to the value. The defining property of these hash codes is that @bold{equal
 values have equal hash codes}. Note that the reverse is not true: two unequal
values can still have equal hash codes. Hash codes are useful for various
indexing and comparison operations, especially in the implementation of
@tech{hash tables}. See @secref["hashtables"] for more information.


@defproc[(equal-hash-code [v any/c]) fixnum?]{

 Returns a @tech{hash code} consistent with @racket[equal?]. For any two calls
 with @racket[equal?] values, the returned number is the same. A hash code is
 computed even when @racket[v] contains a cycle through pairs, vectors, boxes,
 and/or inspectable structure fields. Additionally, user-defined data types can
 customize how this hash code is computed by implementing
 @racket[gen:equal+hash] or @racket[gen:equal-mode+hash].

 For any @racket[v] that could be produced by @racket[read], if @racket[v2] is
 produced by @racket[read] for the same input characters, the
 @racket[(equal-hash-code v)] is the same as @racket[(equal-hash-code v2)] ---
 even if @racket[v] and @racket[v2] do not exist at the same time (and therefore
 could not be compared by calling @racket[equal?]).

 @history[
 #:changed "6.4.0.12"
 @elem{Strengthened guarantee for @racket[read]able values.}]}

@defproc[(equal-hash-code/recur [v any/c] [recur-proc (-> any/c exact-integer?)])
         fixnum?]{
 Like @racket[equal-hash-code], but using @racket[recur-proc] for recursive
 hashing within @racket[v].

 @examples[
   (define (rational-hash x)
     (cond
       [(rational? x) (equal-hash-code (inexact->exact x))]
       [else (equal-hash-code/recur x rational-hash)]))
   (= (rational-hash 0.0) (rational-hash -0.0))
   (= (rational-hash 1.0) (rational-hash -1.0))
   (= (rational-hash (list (list (list 4.0 0.0) 9.0) 6.0))
      (rational-hash (list (list (list 4 0) 9) 6)))
 ]

 @history[#:added "8.8.0.9"]}

@defproc[(equal-secondary-hash-code [v any/c]) fixnum?]{

 Like @racket[equal-hash-code], but computes a secondary @tech{hash code}
 suitable for use in double hashing.}


@defproc[(equal-always-hash-code [v any/c]) fixnum?]{

 Returns a @tech{hash code} consistent with @racket[equal-always?]. For any two
 calls with @racket[equal-always?] values, the returned number is the same.

 As @racket[equal-always-hash-code] traverses @racket[v], immutable
 values within @racket[v] are hashed with @racket[equal-hash-code],
 while mutable values within @racket[v] are hashed with @racket[eq-hash-code].}


@defproc[(equal-always-hash-code/recur [v any/c]
                                       [recur-proc (-> any/c exact-integer?)])
         fixnum?]{
 Like @racket[equal-always-hash-code], but using @racket[recur-proc] for
 recursive hashing within @racket[v].

 @history[#:added "8.8.0.9"]}

@defproc[(equal-always-secondary-hash-code [v any/c]) fixnum?]{

 Like @racket[equal-always-hash-code], but computes a secondary @tech{hash code}
 suitable for use in double hashing.}


@defproc[(eq-hash-code [v any/c]) fixnum?]{

 Returns a @tech{hash code} consistent with @racket[eq?]. For any two calls with
 @racket[eq?] values, the returned number is the same.

 @margin-note{Equal @tech{fixnums} are always @racket[eq?].}}


@defproc[(eqv-hash-code [v any/c]) fixnum?]{

 Returns a @tech{hash code} consistent with @racket[eqv?]. For any two calls
 with @racket[eqv?] values, the returned number is the same.}


@section{Implementing Equality for Custom Types}


@defthing[gen:equal+hash any/c]{
 A @tech{generic interface} (see @secref["struct-generics"]) for types that can
 be compared for equality using @racket[equal?]. The following methods must be
 implemented:

 @itemize[

 @item{@racket[_equal-proc :
               (any/c any/c (any/c any/c . -> . boolean?)  . -> . any/c)] ---
   tests whether the first two arguments are equal, where both values are
   instances of the structure type to which the generic interface is associated
   (or a subtype of the structure type).

   The third argument is an @racket[equal?]  predicate to use for
   recursive equality checks; use the given predicate instead of
   @racket[equal?] to ensure that data cycles are handled
   properly and to work with @racket[equal?/recur] (but beware
   that an arbitrary function can be provided to
   @racket[equal?/recur] for recursive checks, which means that
   arguments provided to the predicate might be exposed to
   arbitrary code).

   The @racket[_equal-proc] is called for a pair of structures
   only when they are not @racket[eq?], and only when they both
   have a @racket[gen:equal+hash] value inherited from the same
   structure type. With this strategy, the order in which
   @racket[equal?] receives two structures does not matter. It
   also means that, by default, a structure sub-type inherits the
   equality predicate of its parent, if any.}

 @item{@racket[_hash-proc :
               (any/c (any/c . -> . exact-integer?) . -> . exact-integer?)] ---
   computes a hash code for the given structure, like @racket[equal-hash-code].
   The first argument is an instance of the structure type (or one of its
   subtypes) to which the generic interface is associated.

   The second argument is an @racket[equal-hash-code]-like procedure to use for
   recursive hash-code computation; use the given procedure instead of
   @racket[equal-hash-code] to ensure that data cycles are handled properly.

   Although the result of @racket[_hash-proc] can be any exact
   integer, it will be truncated for most purposes to a @tech{fixnum}
   (e.g., for the result of @racket[equal-hash-code]). Roughly,
   truncation uses @racket[bitwise-and] to take the lower bits of the
   number. Thus, variation in the hash-code computation should be
   reflected in the fixnum-compatible bits of @racket[_hash-proc]'s
   result. Consumers of a hash code are expected to use variation
   within the fixnum range appropriately, and producers are @emph{not}
   responsible to reflect variation in hash codes across the full
   range of bits that fit within a fixnum.}

 @item{@racket[_hash2-proc :
               (any/c (any/c . -> . exact-integer?) . -> . exact-integer?)] ---
   computes a secondary hash code for the given structure. This procedure is
   like @racket[_hash-proc], but analogous to
   @racket[equal-secondary-hash-code].}]

 Take care to ensure that @racket[_hash-proc] and @racket[_hash2-proc]
 are consistent with @racket[_equal-proc]. Specifically,
 @racket[_hash-proc] and @racket[_hash2-proc] should produce the same
 value for any two structures for which @racket[_equal-proc] produces a
 true value.

 The @racket[_equal-proc] is not only used for
 @racket[equal?], it is also used for @racket[equal?/recur],
 and @racket[impersonator-of?]. Furthermore, if the structure type
 has no mutable fields, @racket[_equal-proc] is used for @racket[equal-always?], and
 @racket[chaperone-of?]. Likewise @racket[_hash-proc] and
 @racket[_hash2-proc] are used for
 @racket[equal-always-hash-code] and
 @racket[equal-always-secondary-hash-code], respectively, when
 the structure type has no mutable fields.
 Instances of these methods should follow the guidelines in
 @secref["Honest_Custom_Equality"] to implement all of these
 operations reasonably. In particular, these methods should
 not access mutable data unless the struct is declared
 mutable.

 When a structure type has no @racket[gen:equal+hash] or
 @racket[gen:equal-mode+hash] implementation, then
 transparent structures (i.e., structures with an @tech{inspector} that
 is controlled by the current @tech{inspector}) are @racket[equal?]
 when they are instances of the same structure type (not counting
 sub-types), and when they have @racket[equal?] field values.  For
 transparent structures, @racket[equal-hash-code] and
 @racket[equal-secondary-hash-code] (in the case of no mutable fields)
 derive hash code using the field
 values. For a transparent structure type with at least one mutable field,
 @racket[equal-always?] is the same as @racket[eq?], and an
 @racket[equal-secondary-hash-code] result is based only on @racket[eq-hash-code].
 For opaque structure types, @racket[equal?] is the same as
 @racket[eq?], and @racket[equal-hash-code] and
 @racket[equal-secondary-hash-code] results are based only on
 @racket[eq-hash-code]. If a structure has a @racket[prop:impersonator-of]
 property, then the @racket[prop:impersonator-of] property takes precedence over
 @racket[gen:equal+hash] if the property value's procedure returns a
 non-@racket[#f] value when applied to the structure.

 @(examples
   (eval:no-prompt
    (define (farm=? farm1 farm2 recursive-equal?)
      (and (= (farm-apples farm1)
              (farm-apples farm2))
           (= (farm-oranges farm1)
              (farm-oranges farm2))
           (= (farm-sheep farm1)
              (farm-sheep farm2))))

    (define (farm-hash-code farm recursive-equal-hash)
      (+ (* 10000 (farm-apples farm))
         (* 100 (farm-oranges farm))
         (* 1 (farm-sheep farm))))

    (define (farm-secondary-hash-code farm recursive-equal-hash)
      (+ (* 10000 (farm-sheep farm))
         (* 100 (farm-apples farm))
         (* 1 (farm-oranges farm))))

    (struct farm (apples oranges sheep)
      #:methods gen:equal+hash
      [(define equal-proc farm=?)
       (define hash-proc  farm-hash-code)
       (define hash2-proc farm-secondary-hash-code)])

    (define eastern-farm (farm 5 2 20))
    (define western-farm (farm 18 6 14))
    (define northern-farm (farm 5 20 20))
    (define southern-farm (farm 18 6 14)))

   (equal? eastern-farm western-farm)
   (equal? eastern-farm northern-farm)
   (equal? western-farm southern-farm))

 @history[#:changed "8.7.0.5"
          @elem{Added a check so that omitting any of
                @racket[_equal-proc], @racket[_hash-proc], and @racket[_hash2-proc]
                is now a syntax error.}]}


@defthing[gen:equal-mode+hash any/c]{
 A @tech{generic interface} (see @secref["struct-generics"]) for types that
 may specify differences between @racket[equal?] and @racket[equal-always?].
 The following methods must be implemented:

 @itemlist[

 @item{@racket[_equal-mode-proc :
               (any/c any/c (any/c any/c . -> . boolean?) boolean? . -> . any/c)] ---
   the first two arguments are the values to compare, the third argument is an
   equality function to use for recursive comparisons, and the last argument is
   the mode: @racket[#t] for an @racket[equal?] or @racket[impersonator-of?]
   comparison or @racket[#f] for an @racket[equal-always?] or
   @racket[chaperone-of?] comparison.}

 @item{@racket[_hash-mode-proc :
               (any/c (any/c . -> . exact-integer?) boolean? . -> . exact-integer?)] ---
   the first argument is the value to compute a hash code for, the second
   argument is a hashing function to use for recursive hashing, and the last
   argument is the mode: @racket[#t] for @racket[equal?] hashing or @racket[#f]
   for @racket[equal-always?] hashing.}]

 The @racket[_hash-mode-proc] implementation is used both for a
 primary hash code and secondary hash code.

 When implementing these methods, follow the guidelines in
 @secref["Honest_Custom_Equality"]. In particular, these
 methods should only access mutable data if the ``mode'' argument
 is true to indicate @racket[equal?] or @racket[impersonator-of?].

 Implementing @racket[gen:equal-mode+hash] is most useful for types that
 specify differences between @racket[equal?] and @racket[equal-always?], such
 as a structure type that wraps mutable data with getter and setter procedures:
 @(examples
   (define (get gs) ((getset-getter gs)))
   (define (set gs new) ((getset-setter gs) new))
   (struct getset (getter setter)
      #:methods gen:equal-mode+hash
      [(define (equal-mode-proc self other rec mode)
         (and mode (rec (get self) (get other))))
       (define (hash-mode-proc self rec mode)
         (if mode (rec (get self)) (eq-hash-code self)))])

   (define x 1)
   (define y 2)
   (define gsx (getset (lambda () x) (lambda (new) (set! x new))))
   (define gsy (getset (lambda () y) (lambda (new) (set! y new))))
   (eval:check (equal? gsx gsy) #f)
   (eval:check (equal-always? gsx gsy) #f)
   (set gsx 3)
   (set gsy 3)
   (eval:check (equal? gsx gsy) #t)
   (eval:check (equal-always? gsx gsy) #f)
   (eval:check (equal-always? gsx gsx) #t))

@history[#:added "8.5.0.3"
         #:changed "8.7.0.5"
         @elem{Added a check so that omitting either
               @racket[_equal-mode-proc] or @racket[_hash-mode-proc]
               is now a syntax error.}]}


@defthing[prop:equal+hash struct-type-property?]{

 A @tech{structure type property} (see @secref["structprops"])
 that supplies an equality predicate and hashing functions for a structure
 type. Using the @racket[prop:equal+hash] property is an alternative to
 using the @racket[gen:equal+hash] or @racket[gen:equal-mode+hash]
 @tech{generic interface}.

 A @racket[prop:equal+hash] property value is a list of either three
 procedures @racket[(list _equal-proc _hash-proc _hash2-proc)] or two
 procedures @racket[(list _equal-mode-proc _hash-mode-proc)]:

 @itemlist[

  @item{The three-procedure case corresponds to the procedures of
        @racket[gen:equal-hash]:

         @itemlist[
           @item{@racket[_equal-proc : (any/c any/c (any/c any/c . -> . boolean?)  . -> . any/c)]}

           @item{@racket[_hash-proc : (any/c (any/c . -> . exact-integer?) . -> . exact-integer?)]}

           @item{@racket[_hash2-proc : (any/c (any/c . -> . exact-integer?) . -> . exact-integer?)]}
        ]}

  @item{The two-procedure case corresponds to the procedures of
  @racket[gen:equal-mode-hash]:

       @itemlist[
         @item{@racket[_equal-mode-proc : (any/c any/c (any/c any/c . -> . boolean?) boolean? . -> . any/c)]}

          @item{@racket[_hash-mode-proc : (any/c (any/c . -> . exact-integer?) boolean? . -> . exact-integer?)]}

        ]}

]

When implementing these methods, follow the guidelines in
@secref["Honest_Custom_Equality"]. In particular, these
methods should only access mutable data if the struct is
declared mutable or the mode is true.

@history[#:changed "8.5.0.3" @elem{Added support for two-procedure values to customize @racket[equal-always?].}]}

@section[#:tag "Honest_Custom_Equality"]{Honest Custom Equality}

Since the @racket[_equal-proc] or @racket[_equal-mode-proc]
is used for more than just @racket[equal?], instances of
them should follow certain guidelines to make sure that they work
correctly for @racket[equal-always?], @racket[chaperone-of?],
and @racket[impersonator-of?].

Due to the differences between these operations, avoid
calling @racket[equal?] within them. Instead, use the third
argument to ``recur'' on the pieces, which allows
@racket[equal?/recur] to work properly, lets the other
operations behave in their own distinct ways on the pieces,
and enables some cycle detection.

@compare0[
@racketblock0[
  (define (equal-proc self other rec)
    (rec (fish-size self) (fish-size other)))
]

@racketblock0[
  (define (equal-proc self other rec)
    (equal? (fish-size self) (fish-size other)))
]
]

Don't use the third argument to ``recur'' on counts of
elements.
When a data structure cares about discrete numbers, it can
use @racket[=] on those, not @racket[equal?] or ``recur''.
Using ``recur'' on counts is bad when a ``recur'' argument
from @racket[equal?/recur] is too tolerant on numbers within
some range of each other.

@compare0[
@racketblock0[
  (define (equal-proc self other rec)
    (and (= (tuple-length self) (tuple-length other))
         (for/and ([i (in-range (tuple-length self))])
           (rec ((tuple-getter self) i)
                ((tuple-getter other) i)))))
]

@racketblock0[
  (define (equal-proc self other rec)
    (and (rec (tuple-length self) (tuple-length other))
         (for/and ([i (in-range (tuple-length self))])
           (rec ((tuple-getter self) i)
                ((tuple-getter other) i)))))
]
]

The operations @racket[equal?] and @racket[equal-always?]
should be symmetric, so @racket[_equal-proc] instances
should not change their answer when the arguments swap:

@compare0[
@racketblock0[
  (define (equal-proc self other rec)
    (rec (fish-size self) (fish-size other)))
]

@racketblock0[
  (define (equal-proc self other rec)
    (<= (fish-size self) (fish-size other)))
]
]

However, the operations @racket[chaperone-of?] and
@racket[impersonator-of?] are @emph{not} symmetric, so when
calling the third argument to ``recur'' on pieces, pass the
pieces in the same order they came in:

@compare0[
@racketblock0[
  (define (equal-proc self other rec)
    (rec (fish-size self) (fish-size other)))
]

@racketblock0[
  (define (equal-proc self other rec)
    (rec (fish-size other) (fish-size self)))
]
]

The operations @racket[equal-always?] and
@racket[chaperone-of?] shouldn't change on mutation, so
@racket[_equal-proc] instances should not access
potentially-mutable data.
This includes avoiding @racket[string=?], since strings can
be mutable.
Type-specific equality functions for immutable types, such
as @racket[symbol=?], are fine.

@compare0[#:left "fine" #:right "bad"
@racketblock0[
  (define (equal-proc self other rec)
    (code:comment "symbols are immutable: no problem")
    (symbol=? (thing-name self) (thing-name other)))
]

@racketblock0[
  (define (equal-proc self other rec)
    (code:comment "strings can be mutable: accesses mutable data")
    (string=? (thing-name self) (thing-name other)))
]
]

Declaring a struct as mutable makes @racket[equal-always?]
and @racket[chaperone-of?] avoid using @racket[_equal-proc],
so @racket[_equal-proc] instances are free to access mutable
data if the struct is declared mutable:

@compare0[
@racketblock0[
  (struct mcell (value) #:mutable
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (rec (mcell-value self)
            (mcell-value other)))
     (define (hash-proc self rec)
       (+ (eq-hash-code struct:mcell)
          (rec (mcell-value self))))
     (define (hash2-proc self rec)
       (+ (eq-hash-code struct:mcell)
          (rec (mcell-value self))))])
]

@racketblock0[
  (struct mcell (box)
    (code:comment "not declared mutable,")
    (code:comment "but represents mutable data anyway")
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (rec (unbox (mcell-box self))
            (unbox (mcell-box other))))
     (define (hash-proc self rec)
       (+ (eq-hash-code struct:mcell)
          (rec (unbox (mcell-value self)))))
     (define (hash2-proc self rec)
       (+ (eq-hash-code struct:mcell)
          (rec (unbox (mcell-value self)))))])
]
]

Another way for a struct to control access to mutable data
is by implementing @racket[gen:equal-mode+hash] instead of
@racket[gen:equal+hash].
When the mode is true, @racket[_equal-mode-proc] instances
are free to access mutable data, and when the mode is false,
they shouldn't:

@compare0[#:left "also good" #:right "still bad"
@racketblock0[
  (struct mcell (value) #:mutable
    (code:comment "only accesses mutable data when mode is true")
    #:methods gen:equal-mode+hash
    [(define (equal-mode-proc self other rec mode)
       (and mode
            (rec (mcell-value self)
                 (mcell-value other))))
     (define (hash-mode-proc self rec mode)
       (if mode
           (+ (eq-hash-code struct:mcell)
              (rec (mcell-value self)))
           (eq-hash-code self)))])
]

@racketblock0[
  (struct mcell (value) #:mutable
    (code:comment "accesses mutable data ignoring mode")
    #:methods gen:equal-mode+hash
    [(define (equal-mode-proc self other rec mode)
       (rec (mcell-value self)
            (mcell-value other)))
     (define (hash-mode-proc self rec mode)
       (+ (eq-hash-code struct:mcell)
          (rec (mcell-value self))))])
]
]

@section{Combining Hash Codes}

@note-lib-only[racket/hash-code]

@history[#:added "8.8.0.5"]

@defproc[(hash-code-combine [hc exact-integer?] ...) fixnum?]{
  Combines the @racket[hc]s into a @tech{hash code} that
  depends on the order of the inputs.
  Useful for combining the hash codes of different fields in
  a structure.

  @examples[
    (require racket/hash-code)
    (struct ordered-triple (fst snd thd)
      #:methods gen:equal+hash
      [(define (equal-proc self other rec)
         (and (rec (ordered-triple-fst self) (ordered-triple-fst other))
              (rec (ordered-triple-snd self) (ordered-triple-snd other))
              (rec (ordered-triple-thd self) (ordered-triple-thd other))))
       (define (hash-proc self rec)
         (hash-code-combine (eq-hash-code struct:ordered-triple)
                            (rec (ordered-triple-fst self))
                            (rec (ordered-triple-snd self))
                            (rec (ordered-triple-thd self))))
       (define (hash2-proc self rec)
         (hash-code-combine (eq-hash-code struct:ordered-triple)
                            (rec (ordered-triple-fst self))
                            (rec (ordered-triple-snd self))
                            (rec (ordered-triple-thd self))))])
    (equal? (ordered-triple 'A 'B 'C) (ordered-triple 'A 'B 'C))
    (= (equal-hash-code (ordered-triple 'A 'B 'C))
       (equal-hash-code (ordered-triple 'A 'B 'C)))
    (equal? (ordered-triple 'A 'B 'C) (ordered-triple 'C 'B 'A))
    (= (equal-hash-code (ordered-triple 'A 'B 'C))
       (equal-hash-code (ordered-triple 'C 'B 'A)))
    (equal? (ordered-triple 'A 'B 'C) (ordered-triple 'C 'A 'B))
    (= (equal-hash-code (ordered-triple 'A 'B 'C))
       (equal-hash-code (ordered-triple 'C 'A 'B)))
  ]

  With one argument, @racket[(hash-code-combine hc)] mixes
  the hash code so that it isn't just @racket[hc].

  @examples[
    (require racket/hash-code)
    (struct wrap (value)
      #:methods gen:equal+hash
      [(define (equal-proc self other rec)
         (rec (wrap-value self) (wrap-value other)))
       (define (hash-proc self rec)
         (code:comment "demonstrates `hash-code-combine` with only one argument")
         (code:comment "but it's good to combine `(eq-hash-code struct:wrap)` too")
         (hash-code-combine (rec (wrap-value self))))
       (define (hash2-proc self rec)
         (hash-code-combine (rec (wrap-value self))))])
    (equal? (wrap 'A) (wrap 'A))
    (= (equal-hash-code (wrap 'A))
       (equal-hash-code (wrap 'A)))
    (equal? (wrap 'A) 'A)
    (= (equal-hash-code (wrap 'A))
       (equal-hash-code 'A))
  ]
}

@defproc[(hash-code-combine-unordered [hc exact-integer?] ...) fixnum?]{
  Combines the @racket[hc]s into a @tech{hash code} that
  @emph{does not} depend on the order of the inputs.
  Useful for combining the hash codes of elements of an
  unordered set.

  @examples[
    (require racket/hash-code)
    (struct flip-triple (left mid right)
      #:methods gen:equal+hash
      [(define (equal-proc self other rec)
         (and (rec (flip-triple-mid self) (flip-triple-mid other))
              (or
               (and (rec (flip-triple-left self) (flip-triple-left other))
                    (rec (flip-triple-right self) (flip-triple-right other)))
               (and (rec (flip-triple-left self) (flip-triple-right other))
                    (rec (flip-triple-right self) (flip-triple-left other))))))
       (define (hash-proc self rec)
         (hash-code-combine (eq-hash-code struct:flip-triple)
                            (rec (flip-triple-mid self))
                            (hash-code-combine-unordered
                             (rec (flip-triple-left self))
                             (rec (flip-triple-right self)))))
       (define (hash2-proc self rec)
         (hash-code-combine (eq-hash-code struct:flip-triple)
                            (rec (flip-triple-mid self))
                            (hash-code-combine-unordered
                             (rec (flip-triple-left self))
                             (rec (flip-triple-right self)))))])
    (equal? (flip-triple 'A 'B 'C) (flip-triple 'A 'B 'C))
    (= (equal-hash-code (flip-triple 'A 'B 'C))
       (equal-hash-code (flip-triple 'A 'B 'C)))
    (equal? (flip-triple 'A 'B 'C) (flip-triple 'C 'B 'A))
    (= (equal-hash-code (flip-triple 'A 'B 'C))
       (equal-hash-code (flip-triple 'C 'B 'A)))
    (equal? (flip-triple 'A 'B 'C) (flip-triple 'C 'A 'B))
    (= (equal-hash-code (flip-triple 'A 'B 'C))
       (equal-hash-code (flip-triple 'C 'A 'B)))
    (struct rotate-triple (rock paper scissors)
      #:methods gen:equal+hash
      [(define (equal-proc self other rec)
         (or
          (and (rec (rotate-triple-rock self) (rotate-triple-rock other))
               (rec (rotate-triple-paper self) (rotate-triple-paper other))
               (rec (rotate-triple-scissors self) (rotate-triple-scissors other)))
          (and (rec (rotate-triple-rock self) (rotate-triple-paper other))
               (rec (rotate-triple-paper self) (rotate-triple-scissors other))
               (rec (rotate-triple-scissors self) (rotate-triple-rock other)))
          (and (rec (rotate-triple-rock self) (rotate-triple-scissors other))
               (rec (rotate-triple-paper self) (rotate-triple-rock other))
               (rec (rotate-triple-scissors self) (rotate-triple-paper other)))))
       (define (hash-proc self rec)
         (define r (rec (rotate-triple-rock self)))
         (define p (rec (rotate-triple-paper self)))
         (define s (rec (rotate-triple-scissors self)))
         (hash-code-combine
          (eq-hash-code struct:rotate-triple)
          (hash-code-combine-unordered
           (hash-code-combine r p)
           (hash-code-combine p s)
           (hash-code-combine s r))))
       (define (hash2-proc self rec)
         (define r (rec (rotate-triple-rock self)))
         (define p (rec (rotate-triple-paper self)))
         (define s (rec (rotate-triple-scissors self)))
         (hash-code-combine
          (eq-hash-code struct:rotate-triple)
          (hash-code-combine-unordered
           (hash-code-combine r p)
           (hash-code-combine p s)
           (hash-code-combine s r))))])
    (equal? (rotate-triple 'A 'B 'C) (rotate-triple 'A 'B 'C))
    (= (equal-hash-code (rotate-triple 'A 'B 'C))
       (equal-hash-code (rotate-triple 'A 'B 'C)))
    (equal? (rotate-triple 'A 'B 'C) (rotate-triple 'C 'B 'A))
    (= (equal-hash-code (rotate-triple 'A 'B 'C))
       (equal-hash-code (rotate-triple 'C 'B 'A)))
    (equal? (rotate-triple 'A 'B 'C) (rotate-triple 'C 'A 'B))
    (= (equal-hash-code (rotate-triple 'A 'B 'C))
       (equal-hash-code (rotate-triple 'C 'A 'B)))
  ]
}

@defproc[(hash-code-combine* [hc exact-integer?] ...
                             [hcs (listof exact-integer?)])
         fixnum?]{
  @; Note: this is exactly the same description as append* and string-append*

  Like @racket[hash-code-combine], but the last argument is
  used as a list of arguments for @racket[hash-code-combine],
  so @racket[(hash-code-combine* hc ... hcs)] is the same as
  @racket[(apply hash-code-combine hc ... hcs)].
  In other words, the relationship between
  @racket[hash-code-combine] and @racket[hash-code-combine*]
  is similar to the one between @racket[list] and
  @racket[list*].
}

@defproc[(hash-code-combine-unordered* [hc exact-integer?] ...
                                       [hcs (listof exact-integer?)])
         fixnum?]{
  @; Note: this is exactly the same description as append* and string-append*

  Like @racket[hash-code-combine-unordered], but the last
  argument is used as a list of arguments for
  @racket[hash-code-combine-unordered], so
  @racket[(hash-code-combine-unordered* hc ... hcs)] is the same
  as @racket[(apply hash-code-combine-unordered hc ... hcs)].
  In other words, the relationship between
  @racket[hash-code-combine-unordered] and
  @racket[hash-code-combine-unordered*] is similar to the
  one between @racket[list] and @racket[list*].
}
