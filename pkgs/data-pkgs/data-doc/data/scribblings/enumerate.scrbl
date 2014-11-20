#lang scribble/manual
@(require scribble/eval
          (for-label data/enumerate
                     racket/contract
                     racket/base))

@title{Enumerations}

@(define the-eval (make-base-eval))
@(the-eval '(require data/enumerate racket/string))

@defmodule[data/enumerate]

@author[@author+email["Max S. New" "maxsnew@gmail.com"]]

This library defines @deftech{enumerations}. Enumerations are
bijections between the natural numbers (or a prefix thereof) and a
data-type. Most of the bijections defined in this library guarantee
that the enumeration is efficient, meaning that the decoding a number
is linear in the bits of the number.

@; XXX explain fairness
@; XXX add citations

@defproc[(enum [size (or/c exact-nonnegative-integer? +inf.0)] 
               [from (-> exact-nonnegative-integer? any/c)]
               [to (-> any/c exact-nonnegative-integer?)])
         enum?]{

Constructs an @tech{enumeration} of size @racket[size] where
@racket[from] is the decoding function and @racket[to] is the encoding
function. }

@defproc[(enum? [x any/c]) boolean?]{

Identifies a value as an @tech{enumeration}.}

@defproc[(size [e enum?]) (or/c exact-nonnegative-integer? +inf.0)]{

Returns the size of an @tech{enumeration}.}

@defproc[(decode [e enum?] [n exact-nonnegative-integer?]) any/c]{

Uses @racket[e] to decode @racket[n].}

@defproc[(to-nat [e enum?] [x any/c]) exact-nonnegative-integer?]{

Uses @racket[e] to encode @racket[x].}

@defthing[nat/e enum?]{

An @tech{enumeration} of the natural numbers.
   
@examples[#:eval the-eval
(decode nat/e 5)
(to-nat nat/e 5)
]}

@defproc[(map/e [f (-> a ... b)]
                [inv-f (-> b (values a ...))]
                [e enum?] ...+) enum?]{

Uses @racket[f] and @racket[inv-f] around the encoding and decoding
functions of @racket[e].

@examples[#:eval the-eval
(define map-1/e
  (map/e number->string string->number nat/e))
(decode map-1/e 5)
(to-nat map-1/e "5")
(define map-2/e
  (map/e (λ (x y) 
           (string-join
            (list (number->string x)
                  (number->string y))))
         (λ (x)
           (apply values (map string->number (string-split x))))
         nat/e nat/e))
(decode map-2/e 5)
(to-nat map-2/e "1 2")
]}

@defproc[(filter/e [e enum?] [p (-> any/c boolean?)]) enum?]{

Returns an @tech{enumeration} identical to @racket[e] except that only
elements where @racket[p] returns true are included. The encoding
function and the size are wrong in the result and this is inefficient,
so only use it for very small enumerations.

@examples[#:eval the-eval
(define filter-1/e
  (filter/e nat/e even?))
(decode filter-1/e 5)
(to-nat filter-1/e 8)
]}

@defproc[(except/e [e enum?] [x any/c] ...) enum?]{

Returns an @tech{enumeration} identical to @racket[e] except that all
@racket[x] are not included in the decoding and cannot be encoded
(converted to a natural number).

@examples[#:eval the-eval
(define except-1/e
  (except/e nat/e 3))
(decode except-1/e 5)
(to-nat except-1/e 8)
]}

@defproc[(to-stream [e enum?]) stream?]{

Returns a stream of the values in @racket[e].

@examples[#:eval the-eval
(to-stream map-2/e)
]}

@defproc[(approximate [e enum?] [n exact-nonnegative-integer?]) list?]{

Returns a list of the first @racket[n] values in @racket[e].

@examples[#:eval the-eval
(approximate map-2/e 5)
]}

@defproc[(to-list [e enum?]) list?]{

Returns a list of the @racket[n] values in @racket[e]. This will
diverge if @racket[e] is infinite.

@examples[#:eval the-eval
(to-list (take/e map-2/e 5))
]}

@defproc[(take/e [e enum?] [n exact-nonnegative-integer?]) enum?]{

Identical to @racket[e] but only includes the first @racket[n] values.

@examples[#:eval the-eval
(decode (take/e map-2/e 5) 3)
(to-nat (take/e map-2/e 5) "1 1")
]}

@defproc[(slice/e [e enum?] [lo exact-nonnegative-integer?] [hi exact-nonnegative-integer?]) enum?]{

Identical to @racket[e] but only includes the values between
@racket[lo] and @racket[hi].

@examples[#:eval the-eval
(to-list (slice/e map-2/e 5 10))
]}

@defproc[(below/e [max exact-nonnegative-integer?]) enum?]{

An @tech{enumeration} of the first @racket[max] naturals.

@examples[#:eval the-eval
(to-list (below/e 10))
]}

@defthing[empty/e enum?]{

An empty @tech{enumeration}.

@examples[#:eval the-eval
(to-list empty/e)
]}

@defproc[(const/e [x any/c]) enum?]{

An @tech{enumeration} of exactly @racket[x].

@examples[#:eval the-eval
(to-list (const/e 42))
]}

@defproc[(from-list/e [xs list?]) enum?]{

An @tech{enumeration} of each @racket[eq?] value in
@racket[xs]. @racket[xs] should not contain duplicates, but
@racket[from-list/e] doesn't check.

@examples[#:eval the-eval
(to-list (from-list/e '("Brian" "Jenny" "Ki" "Ted")))
]}

@defproc[(fin/e [x any/c] ...) enum?]{

An @tech{enumeration} of each @racket[x]. If a value is duplicated in
the input, it is only in the enumeration once.

@examples[#:eval the-eval
(to-list (fin/e "Brian" "Jenny" "Ki" "Ted"))
]}

@defthing[int/e enum?]{

An @tech{enumeration} of the integers.

@examples[#:eval the-eval
(approximate int/e 10)
]}

@defproc[(disj-sum/e [e-p (cons/c enum? (-> any/c boolean?))] ...) enum?]{

An @tech{enumeration} of the disjoint sum of the enumerations given in
@racket[e-p]. Each @racket[e-p] is a pair of an enumeration and a
predicate identifying elements of it. Only one or zero predicates
should return true on any value.

@examples[#:eval the-eval
(approximate (disj-sum/e (cons nat/e exact-nonnegative-integer?) (cons map-1/e string?)) 10)
]}

@defproc[(disj-append/e [e-p (cons/c enum? (-> any/c boolean?))] ...+) enum?]{

An @tech{enumeration} of the disjoint sum of the enumerations given in
@racket[e-p] that fully enumerates each enumeration before enumerating
the next. @racket[e-p] are formatted as in @racket[disj-sum/e]. All
but the last enumeration should be finite.

@examples[#:eval the-eval
(approximate (disj-append/e (cons (take/e nat/e 4) exact-nonnegative-integer?)
                            (cons map-1/e string?))
             10)
]}

@defproc[(fin-cons/e [x enum?] [y enum?]) enum?]{

An @tech{enumeration} of pairs of the values from @racket[x] and
@racket[y]. Both enumerations should be finite.

@examples[#:eval the-eval
(approximate (fin-cons/e (take/e nat/e 4) (take/e nat/e 5)) 5)
]}

@defproc[(cons/e [x enum?] [y enum?]) enum?]{

An @tech{enumeration} of pairs of the values from @racket[x] and
@racket[y].

@examples[#:eval the-eval
(approximate (cons/e (take/e nat/e 4) (take/e nat/e 5)) 5)
(approximate (cons/e nat/e (take/e nat/e 5)) 5)
(approximate (cons/e (take/e nat/e 4) nat/e) 5)
(approximate (cons/e nat/e nat/e) 5)
]}

@defproc[(elegant-cons/e [x enum?] [y enum?]) enum?]{

An @tech{enumeration} of pairs of the values from @racket[x] and
@racket[y]. The enumeration is in a different order than
@racket[cons/e]. Both enumerations should be infinite.

@examples[#:eval the-eval
(approximate (elegant-cons/e nat/e nat/e) 5)
]}

@defproc[(traverse/e [f (-> any/c enum?)] [xs (listof any/c)]) enum?]{

Constructs an @tech{enumeration} that simulatenously enumerates each
of the enumerations returned by @racket[f] applied to each element of
@racket[xs].

@examples[#:eval the-eval
(define traverse-1/e
  (traverse/e (λ (x) (map/e (λ (n) (cons x n))
                            (λ (y) (cdr y))
                            nat/e))
              '("Brian" "Jenny" "Ted" "Ki")))
(approximate traverse-1/e 5)
(to-nat traverse-1/e 
        '(("Brian" . 11) ("Jenny" . 15) ("Ted" . 16) ("Ki" . 7)))
]}

@defproc[(hash-traverse/e [f (-> any/c enum?)] [xs (listof any/c)]) enum?]{

Constructs an @tech{enumeration} that simulatenously enumerates each
of the enumerations returned by @racket[f] applied to each value of
@racket[xs].

@examples[#:eval the-eval
(define hash-traverse-1/e
  (hash-traverse/e (λ (n) (below/e n))
                   (hash "Brian" 5 "Jenny" 15 "Ted" 25 "Ki" 30)))
(approximate hash-traverse-1/e 5)
(to-nat hash-traverse-1/e
        '#hash(("Brian" . 4) ("Jenny" . 1) ("Ted" . 16) ("Ki" . 7)))
]}

@defproc[(dep/e [a enum?] [b (-> any/c enum?)]) enum?]{

Constructs an @tech{enumeration} of pairs of values @racket[a] and
@racket[(b a)].

@examples[#:eval the-eval
(define dep-1/e
  (dep/e nat/e (λ (a) (below/e a))))
(approximate dep-1/e 5)
(to-nat dep-1/e (cons 17 10))
]}

@defproc[(dep2/e [n (or/c exact-nonnegative-integer? +inf.0)] [a enum?] [b (-> any/c enum?)]) enum?]{

Like @racket[dep2/e] but requires the size of the resulting
enumeration be given as @racket[n]. This is more efficient than
@racket[dep/e], particularly when @racket[n] is finite.

@examples[#:eval the-eval
(define (! n)
  (if (zero? n) 1 (* n (! (sub1 n)))))
(define dep2-1/e
  (dep2/e (+ (! 1) (! 2) (! 3) (! 4) (! 5))
          (below/e 5)
          (λ (a) (below/e a))))
(approximate dep2-1/e 5)
(to-nat dep2-1/e (cons 4 3))
]}

@defproc[(fold-enum [f (-> (listof a) b enum?)] [bs (listof b)]) enum?]{

This is like @racket[foldr], but @racket[f] returns
@tech{enumerations} of @racket[_a]s and assumes that the accumulator
is initialized to @racket['()].

@examples[#:eval the-eval
(define fold-enum-1/e
  (fold-enum (λ (as b)
               (below/e (+ (foldr + 0 as) b)))
             (list 1 2 3)))
(approximate fold-enum-1/e 5)
(to-nat fold-enum-1/e (list 0 1 1))
]}

@defproc[(range/e [lo exact-nonnegative-integer?] [hi exact-nonnegative-integer?]) enum?]{

An @tech{enumeration} of the naturals between @racket[lo] and @racket[hi].

@examples[#:eval the-eval
(approximate (range/e 42 64) 5)
]}

@defproc[(thunk/e [size (or/c exact-nonnegative-integer? +inf.0)] [ep (-> enum?)]) enum?]{

A delayed @tech{enumeration} identical to @racket[ep]. This is
typically used with @racket[fix/e].

@examples[#:eval the-eval
(approximate (thunk/e +inf.0 (λ () nat/e)) 5)
]}

@defproc*[([(fix/e [f (-> enum? enum?)]) enum?]
           [(fix/e [size (or/c exact-nonnegative-integer? +inf.0)] [f (-> enum? enum?)]) enum?])]{

An @tech{enumeration} calculated as the fixed-point of @racket[f]. If
@racket[size] is not given, it is assumed to be @racket[+inf.0].

@examples[#:eval the-eval
(approximate 
 (fix/e
  +inf.0 
  (λ (self) (disj-sum/e (cons (fin/e '()) null?)
                        (cons (cons/e nat/e self) pair?))))
 5)
]}

@defproc*[([(many/e [e enum?]) enum?]
           [(many/e [e enum?] [n exact-nonnegative-integer?]) enum?])]{

An @tech{enumeration} of lists of length @racket[n] of values
enumerated by @racket[e]. If @racket[n] is not given, lists of any
size are enumerated.

@examples[#:eval the-eval
(approximate (many/e nat/e) 5)
(approximate (many/e nat/e 5) 5)
]}

@defproc[(many1/e [e enum?]) enum?]{

An @tech{enumeration} of non-empty lists of values enumerated by
@racket[e].

@examples[#:eval the-eval
(approximate (many1/e nat/e) 5)
]}

@defproc[(cantor-vec/e [e enum?] ...) enum?]{

An @tech{enumeration} of vectors of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (cantor-vec/e (fin/e "Brian" "Jenny" "Ki" "Ted") 
                           nat/e
                           (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(vec/e [e enum?] ...) enum?]{

An @tech{enumeration} of vectors of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (vec/e (fin/e "Brian" "Jenny" "Ki" "Ted") 
                    nat/e
                    (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(box-vec/e [e enum?] ...) enum?]{

An @tech{enumeration} of vectors of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (box-vec/e (fin/e "Brian" "Jenny" "Ki" "Ted") 
                        nat/e
                        (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(inf-fin-fair-list/e [e enum?] ...) enum?]{

An @tech{enumeration} of lists of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (inf-fin-fair-list/e
              (fin/e "Brian" "Jenny" "Ki" "Ted") 
              nat/e
              (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(mixed-box-tuples/e [es (listof enum?)]) enum?]{

An @tech{enumeration} of lists of values enumerated by the
enumerations in @racket[es].

@examples[#:eval the-eval
(approximate (mixed-box-tuples/e
              (list (fin/e "Brian" "Jenny" "Ki" "Ted") 
                    nat/e
                    (fin/e "Terra" "Locke" "Edgar" "Mash")))
             5)
]}

@defproc[(inf-fin-cons/e [x enum?] [y enum?]) enum?]{

An @tech{enumeration} of pairs of the values from @racket[x] and
@racket[y]. One of the enumerations must be finite.

@examples[#:eval the-eval
(approximate (inf-fin-cons/e (take/e nat/e 4) (take/e nat/e 5)) 5)
(approximate (inf-fin-cons/e nat/e (take/e nat/e 5)) 5)
(approximate (inf-fin-cons/e (take/e nat/e 4) nat/e) 5)
]}

@defproc[(list/e [e enum?] ...) enum?]{

An @tech{enumeration} of lists of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (list/e
              (fin/e "Brian" "Jenny" "Ki" "Ted") 
              nat/e
              (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(nested-cons-list/e [e enum?] ...) enum?]{

An @tech{enumeration} of lists of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (nested-cons-list/e
              (fin/e "Brian" "Jenny" "Ki" "Ted") 
              nat/e
              (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(cantor-list/e [e enum?] ...) enum?]{

An @tech{enumeration} of lists of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (cantor-list/e
              (fin/e "Brian" "Jenny" "Ki" "Ted") 
              nat/e
              (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(box-list/e [e enum?] ...) enum?]{

An @tech{enumeration} of lists of values enumerated by the
@racket[e].

@examples[#:eval the-eval
(approximate (box-list/e
              (fin/e "Brian" "Jenny" "Ki" "Ted") 
              nat/e
              (fin/e "Terra" "Locke" "Edgar" "Mash"))
             5)
]}

@defproc[(prime-length-box-list/e [es (listof enum?)]) enum?]{

An @tech{enumeration} of lists of values enumerated by the
enumerations in @racket[es].

@examples[#:eval the-eval
(approximate (prime-length-box-list/e
              (list (fin/e "Brian" "Jenny" "Ki" "Ted") 
                    nat/e
                    (fin/e "Terra" "Locke" "Edgar" "Mash")))
             5)
]}

@defproc[(box-tuples/e [k exact-nonnegative-integer?]) enum?]{

An @tech{enumeration} of tuples of naturals of length @racket[k].

@examples[#:eval the-eval
(approximate (box-tuples/e 3)
             5)
]}

@defproc[(bounded-list/e [k exact-nonnegative-integer?] [n exact-nonnegative-integer?]) enum?]{

An @tech{enumeration} of tuples of naturals up to @racket[n] of length @racket[k].

@examples[#:eval the-eval
(approximate (bounded-list/e 3 2)
             5)
]}

@defproc[(nat+/e [lo exact-nonnegative-integer?]) enum?]{

An @tech{enumeration} of tuples of naturals of larger than @racket[lo].

@examples[#:eval the-eval
(approximate (nat+/e 42)
             5)
]}

@defproc[(fail/e [e exn?]) enum?]{

An @tech{enumeration} raises @racket[e] if @racket[decode] or
@racket[to-nat] is called with on.

@examples[#:eval the-eval
(approximate 
 (fail/e
  (exn:fail "Don't do that!"
            (current-continuation-marks)))
 5)
]}

@defthing[char/e enum?]{

An @tech{enumeration} of characters.

@examples[#:eval the-eval
(approximate char/e 5)
]}

@defthing[string/e enum?]{

An @tech{enumeration} of strings.

@examples[#:eval the-eval
(approximate string/e 5)
]}

@defthing[from-1/e enum?]{

An @tech{enumeration} of naturals starting from @racket[1].

@examples[#:eval the-eval
(approximate from-1/e 5)
]}

@defthing[integer/e enum?]{

An @tech{enumeration} of integers.

@examples[#:eval the-eval
(approximate integer/e 5)
]}

@defthing[float/e enum?]{

An @tech{enumeration} of flonums.

@examples[#:eval the-eval
(approximate float/e 5)
]}

@defthing[real/e enum?]{

An @tech{enumeration} of reals.

@examples[#:eval the-eval
(approximate real/e 5)
]}

@defthing[non-real/e enum?]{

An @tech{enumeration} of non-real numbers.

@examples[#:eval the-eval
(approximate non-real/e 5)
]}

@defthing[num/e enum?]{

An @tech{enumeration} of numbers.

@examples[#:eval the-eval
(approximate num/e 5)
]}

@defthing[bool/e enum?]{

An @tech{enumeration} of booleans.

@examples[#:eval the-eval
(to-list bool/e)
]}

@defthing[symbol/e enum?]{

An @tech{enumeration} of symbols.

@examples[#:eval the-eval
(approximate symbol/e 5)
]}

@defthing[base/e enum?]{

An @tech{enumeration} of atomic Racket values.

@examples[#:eval the-eval
(approximate base/e 5)
]}

@defthing[any/e enum?]{

An @tech{enumeration} of S-expressions.

@examples[#:eval the-eval
(approximate any/e 5)
]}

@close-eval[the-eval]
