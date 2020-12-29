#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/unsafe/ops
                     racket/flonum
                     racket/fixnum
                     racket/extflonum
                     (only-in ffi/vector
                              f64vector?
                              f64vector-ref
                              f64vector-set!
                              u16vector?
                              u16vector-ref
                              u16vector-set!
                              s16vector?
                              s16vector-ref
                              s16vector-set!)))

@title[#:tag "unsafe"]{Unsafe Operations}

@defmodule[racket/unsafe/ops]

All functions and forms provided by @racketmodname[racket/base] and
@racketmodname[racket] check their arguments to ensure that the
arguments conform to contracts and other constraints. For example,
@racket[vector-ref] checks its arguments to ensure that the first
argument is a vector, that the second argument is an exact integer,
and that the second argument is between @racket[0] and one less than
the vector's length, inclusive.

Functions provided by @racketmodname[racket/unsafe/ops] are
@deftech{unsafe}. They have certain constraints, but the constraints
are not checked, which allows the system to generate and execute
faster code. If arguments violate an unsafe function's constraints,
the function's behavior and result is unpredictable, and the entire
system can crash or become corrupted.

All of the exported bindings of @racketmodname[racket/unsafe/ops] are
protected in the sense of @racket[protect-out], so access to unsafe
operations can be prevented by adjusting the code inspector (see
@secref["modprotect"]).

@section{Unsafe Numeric Operations}

@deftogether[(
@defproc[(unsafe-fx+ [a fixnum?] ...) fixnum?]
@defproc[(unsafe-fx- [a fixnum?] [b fixnum?] ...) fixnum?]
@defproc[(unsafe-fx* [a fixnum?] ...) fixnum?]
@defproc[(unsafe-fxquotient  [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxremainder [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxmodulo    [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxabs       [a fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Unchecked versions of @racket[fx+], @racket[fx-],
@racket[fx*], @racket[fxquotient],
@racket[fxremainder], @racket[fxmodulo], and
@racket[fxabs]. 

@history[#:changed "7.0.0.13" @elem{Allow zero or more arguments for @racket[unsafe-fx+] and @racket[unsafe-fx*]
                                    and allow one or more arguments for @racket[unsafe-fx-].}]}


@deftogether[(
@defproc[(unsafe-fxand [a fixnum?] ...) fixnum?]
@defproc[(unsafe-fxior [a fixnum?] ...) fixnum?]
@defproc[(unsafe-fxxor [a fixnum?] ...) fixnum?]
@defproc[(unsafe-fxnot [a fixnum?]) fixnum?]
@defproc[(unsafe-fxlshift [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxrshift [a fixnum?] [b fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Unchecked versions of @racket[fxand], @racket[fxior], @racket[fxxor],
@racket[fxnot], @racket[fxlshift], and @racket[fxrshift].

@history[#:changed "7.0.0.13" @elem{Allow zero or more arguments for
                                    @racket[unsafe-fxand], @racket[unsafe-fxior],
                                    and @racket[unsafe-fxxor].}]}


@deftogether[(
@defproc[(unsafe-fx+/wraparound [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fx-/wraparound [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fx*/wraparound [a fixnum?] [b fixnum?]) fixnum?]
@defproc[(unsafe-fxlshift/wraparound [a fixnum?] [b fixnum?]) fixnum?]
)]{

For @tech{fixnums}: Unchecked versions of @racket[fx+/wraparound],
@racket[fx-/wraparound], @racket[fx*/wraparound], and
@racket[fxlshift/wraparound].

@history[#:added "7.9.0.6"]}


@deftogether[(
@defproc[(unsafe-fx=   [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(unsafe-fx<   [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(unsafe-fx>   [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(unsafe-fx<=  [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(unsafe-fx>=  [a fixnum?] [b fixnum?] ...) boolean?]
@defproc[(unsafe-fxmin [a fixnum?] [b fixnum?] ...) fixnum?]
@defproc[(unsafe-fxmax [a fixnum?] [b fixnum?] ...) fixnum?]
)]{

For @tech{fixnums}: Unchecked versions of @racket[fx=], @racket[fx<],
 @racket[fx>], @racket[fx<=], @racket[fx>=],
 @racket[fxmin], and @racket[fxmax].

@history[#:changed "7.0.0.13" @elem{Allow one or more argument,
                                    instead of allowing just two.}]}


@deftogether[(
@defproc[(unsafe-fl+   [a flonum?] ...) flonum?]
@defproc[(unsafe-fl-   [a flonum?] [b flonum?] ...) flonum?]
@defproc[(unsafe-fl*   [a flonum?] ...) flonum?]
@defproc[(unsafe-fl/   [a flonum?] [b flonum?] ...) flonum?]
@defproc[(unsafe-flabs [a flonum?]) flonum?]
)]{

For @tech{flonums}: Unchecked versions of @racket[fl+], @racket[fl-],
@racket[fl*], @racket[fl/], and @racket[flabs].

@history[#:changed "7.0.0.13" @elem{Allow zero or more arguments for @racket[unsafe-fl+] and @racket[unsafe-fl*]
                                    and one or more arguments for @racket[unsafe-fl-] and @racket[unsafe-fl/].}]}


@deftogether[(
@defproc[(unsafe-fl=   [a flonum?] [b flonum?] ...) boolean?]
@defproc[(unsafe-fl<   [a flonum?] [b flonum?] ...) boolean?]
@defproc[(unsafe-fl>   [a flonum?] [b flonum?] ...) boolean?]
@defproc[(unsafe-fl<=  [a flonum?] [b flonum?] ...) boolean?]
@defproc[(unsafe-fl>=  [a flonum?] [b flonum?] ...) boolean?]
@defproc[(unsafe-flmin [a flonum?] [b flonum?] ...) flonum?]
@defproc[(unsafe-flmax [a flonum?] [b flonum?] ...) flonum?]
)]{

For @tech{flonums}: Unchecked versions of @racket[fl=], @racket[fl<],
@racket[fl>], @racket[fl<=], @racket[fl>=], @racket[flmin], and
@racket[flmax].

@history[#:changed "7.0.0.13" @elem{Allow one or more argument,
                                    instead of allowing just two.}]}


@deftogether[(
@defproc[(unsafe-flround [a flonum?]) flonum?]
@defproc[(unsafe-flfloor [a flonum?]) flonum?]
@defproc[(unsafe-flceiling [a flonum?]) flonum?]
@defproc[(unsafe-fltruncate [a flonum?]) flonum?]
)]{

For @tech{flonums}: Unchecked (potentially) versions of
@racket[flround], @racket[flfloor], @racket[flceiling], and
@racket[fltruncate]. Currently, these bindings are simply aliases for
the corresponding safe bindings.}


@defproc[(unsafe-flsingle [a flonum?]) flonum?]{

For @tech{flonums}: Unchecked (potentially) version of
@racket[flsingle].

@history[#:added "7.8.0.7"]}


@deftogether[(
@defproc[(unsafe-flsin [a flonum?]) flonum?]
@defproc[(unsafe-flcos [a flonum?]) flonum?]
@defproc[(unsafe-fltan [a flonum?]) flonum?]
@defproc[(unsafe-flasin [a flonum?]) flonum?]
@defproc[(unsafe-flacos [a flonum?]) flonum?]
@defproc[(unsafe-flatan [a flonum?]) flonum?]
@defproc[(unsafe-fllog [a flonum?]) flonum?]
@defproc[(unsafe-flexp [a flonum?]) flonum?]
@defproc[(unsafe-flsqrt [a flonum?]) flonum?]
@defproc[(unsafe-flexpt [a flonum?] [b flonum?]) flonum?]
)]{

For @tech{flonums}: Unchecked (potentially) versions of
@racket[flsin], @racket[flcos], @racket[fltan], @racket[flasin],
@racket[flacos], @racket[flatan], @racket[fllog], @racket[flexp],
@racket[flsqrt], and @racket[flexpt]. Currently, some of these
bindings are simply aliases for the corresponding safe bindings.}


@deftogether[(
@defproc[(unsafe-make-flrectangular [a flonum?] [b flonum?])
         (and/c complex?
                (lambda (c) (flonum? (real-part c)))
                (lambda (c) (flonum? (imag-part c))))]
@defproc[(unsafe-flreal-part [a (and/c complex?
                                       (lambda (c) (flonum? (real-part c)))
                                       (lambda (c) (flonum? (imag-part c))))])
         flonum?]
@defproc[(unsafe-flimag-part [a (and/c complex?
                                       (lambda (c) (flonum? (real-part c)))
                                       (lambda (c) (flonum? (imag-part c))))])
         flonum?]
)]{

For @tech{flonums}: Unchecked versions of @racket[make-flrectangular],
@racket[flreal-part], and @racket[flimag-part].}


@deftogether[(
@defproc[(unsafe-fx->fl [a fixnum?]) flonum?]
@defproc[(unsafe-fl->fx [a flonum?]) fixnum?]
)]{
Unchecked versions of @racket[fx->fl] and @racket[fl->fx].

@history[#:changed "7.7.0.8" @elem{Changed @racket[unsafe-fl->fx] to truncate.}]}


@defproc[(unsafe-flrandom [rand-gen pseudo-random-generator?]) (and flonum? (>/c 0) (</c 1))]{

Unchecked version of @racket[flrandom].
}


@section{Unsafe Character Operations}

@deftogether[(
@defproc[(unsafe-char=?   [a char?] [b char?] ...) boolean?]
@defproc[(unsafe-char<?   [a char?] [b char?] ...) boolean?]
@defproc[(unsafe-char>?   [a char?] [b char?] ...) boolean?]
@defproc[(unsafe-char<=?  [a char?] [b char?] ...) boolean?]
@defproc[(unsafe-char>=?  [a char?] [b char?] ...) boolean?]
@defproc[(unsafe-char->integer [a char?]) fixnum?]
)]{

Unchecked versions of @racket[char=?], @racket[char<?], @racket[char>?],
@racket[char<=?], @racket[char>=?], and @racket[char->integer].

@history[#:added "7.0.0.14"]}



@section[#:tag "Unsafe Data Extraction"]{Unsafe Compound-Data Operations}

@deftogether[(
@defproc[(unsafe-car [p pair?]) any/c]
@defproc[(unsafe-cdr [p pair?]) any/c]
@defproc[(unsafe-mcar [p mpair?]) any/c]
@defproc[(unsafe-mcdr [p mpair?]) any/c]
@defproc[(unsafe-set-mcar! [p mpair?] [v any/c]) void?]
@defproc[(unsafe-set-mcdr! [p mpair?] [v any/c]) void?]
)]{

Unsafe variants of @racket[car], @racket[cdr], @racket[mcar],
@racket[mcdr], @racket[set-mcar!], and @racket[set-mcdr!].}


@defproc[(unsafe-cons-list [v any/c] [rest list?]) (and/c pair? list?)]{

Unsafe variant of @racket[cons] that produces a pair that claims to be
a list---without checking whether @racket[rest] is a list.}


@deftogether[(
@defproc[(unsafe-list-ref [lst pair?] [pos (and/c exact-nonnegative-integer? fixnum?)]) any/c]
@defproc[(unsafe-list-tail [lst any/c] [pos (and/c exact-nonnegative-integer? fixnum?)]) any/c]
)]{

Unsafe variants of @racket[list-ref] and @racket[list-tail], where
@racket[pos] must be a @tech{fixnum}, and @racket[lst] must start with
at least @racket[(add1 pos)] (for @racket[unsafe-list-ref]) or
@racket[pos] (for @racket[unsafe-list-tail]) pairs.}


@deftogether[(
@defproc[(unsafe-set-immutable-car! [p pair?] [v any/c]) void?]
@defproc[(unsafe-set-immutable-cdr! [p pair?] [v any/c]) void?]
)]{

As their oxymoronic names should suggest, there is @emph{no generally
correct way} to use these functions. They may be useful nevertheless,
as a last resort, in settings where pairs are used in a constrained
way and when making correct assumptions about Racket's implementation
(including limits on the compiler's optimizations).

Some pitfalls of using @racket[unsafe-set-immutable-car!] and
@racket[unsafe-set-immutable-cdr!]:

@itemlist[

 @item{Functions that consume a pair may take advantage of
       immutability, such as computing a list's length once and
       expecting the list to retain that length, or checking a list
       against a contract and expecting the contract to hold
       thereafter.}

 @item{The result of @racket[list?] for a pair may be cached
       internally, so that changing the @racket[cdr] of a pair from a
       list to a non-list or vice versa may cause @racket[list?] to
       produce the wrong value---for the mutated pair or for another
       pair that reaches the mutated pair.}

 @item{The compiler may reorder or even optimize away a call to
       @racket[car] or @racket[cdr] on the grounds that pairs are
       immutable, in which case a @racket[unsafe-set-immutable-car!]
       or @racket[unsafe-set-immutable-cdr!] may not have an effect on
       the use of @racket[car] or @racket[cdr].}

]

@history[#:added "7.9.0.18"]}

@deftogether[(
@defproc[(unsafe-unbox [b box?]) fixnum?]
@defproc[(unsafe-set-box! [b box?] [k fixnum?]) void?]
@defproc[(unsafe-unbox* [v (and/c box? (not/c impersonator?))]) any/c]
@defproc[(unsafe-set-box*! [v (and/c box? (not/c impersonator?))] [val any/c]) void?]
)]{

Unsafe versions of @racket[unbox] and @racket[set-box!], where the
@schemeidfont{box*} variants can be faster but do not work on
@tech{impersonators}.}

@defproc[(unsafe-box*-cas! [loc box?] [old any/c] [new any/c]) boolean?]{
  Unsafe version of @racket[box-cas!].  Like @racket[unsafe-set-box*!], it does
  not work on @tech{impersonators}.
}

@deftogether[(
@defproc[(unsafe-vector-length [v vector?]) fixnum?]
@defproc[(unsafe-vector-ref [v vector?] [k fixnum?]) any/c]
@defproc[(unsafe-vector-set! [v vector?] [k fixnum?] [val any/c]) void?]
@defproc[(unsafe-vector*-length [v (and/c vector? (not/c impersonator?))]) fixnum?]
@defproc[(unsafe-vector*-ref [v (and/c vector? (not/c impersonator?))] [k fixnum?]) any/c]
@defproc[(unsafe-vector*-set! [v (and/c vector? (not/c impersonator?))] [k fixnum?] [val any/c]) void?]
@defproc[(unsafe-vector*-cas! [v (and/c vector? (not/c impersonator?))] [k fixnum?] [old-val any/c] [new-val any/c]) boolean?]
)]{

Unsafe versions of @racket[vector-length], @racket[vector-ref],
@racket[vector-set!], and @racket[vector-cas!], where the @schemeidfont{vector*} variants can be
faster but do not work on @tech{impersonators}.

A vector's size can never be larger than a @tech{fixnum}, so even
@racket[vector-length] always returns a fixnum.

@history[#:changed "6.11.0.2" @elem{Added @racket[unsafe-vector*-cas!].}]}


@defproc[(unsafe-vector*->immutable-vector! [v (and/c vector? (not/c impersonator?))]) (and/c vector? immutable?)]{

Similar to @racket[vector->immutable-vector], but potentially destroys
@racket[v] and reuses it space, so @racket[v] must not be used after
calling @racket[unsafe-vector*->immutable-vector!].

@history[#:added "7.7.0.6"]}


@deftogether[(
@defproc[(unsafe-string-length [str string?]) fixnum?]
@defproc[(unsafe-string-ref [str string?] [k fixnum?])
         (and/c char? (lambda (ch) (<= 0 (char->integer ch) 255)))]
@defproc[(unsafe-string-set! [str (and/c string? (not/c immutable?))] [k fixnum?] [ch char?]) void?]
)]{

Unsafe versions of @racket[string-length], @racket[string-ref], and
@racket[string-set!]. The @racket[unsafe-string-ref] procedure can be used
only when the result will be a Latin-1 character. A string's size can
never be larger than a @tech{fixnum} (so even @racket[string-length]
always returns a fixnum).}

@defproc[(unsafe-string->immutable-string! [str string?]) (and/c string? immutable?)]{

Similar to @racket[string->immutable-string], but potentially destroys
@racket[str] and reuses it space, so @racket[str] must not be used
after calling @racket[unsafe-string->immutable-string!].

@history[#:added "7.7.0.6"]}


@deftogether[(
@defproc[(unsafe-bytes-length [bstr bytes?]) fixnum?]
@defproc[(unsafe-bytes-ref [bstr bytes?] [k fixnum?]) byte?]
@defproc[(unsafe-bytes-set! [bstr (and/c bytes? (not/c immutable?))] [k fixnum?] [b byte?]) void?]
@defproc[(unsafe-bytes-copy! [dest (and/c bytes? (not/c immutable?))]
                             [dest-start fixnum?]
                             [src bytes?]
                             [src-start fixnum? 0]
                             [src-end fixnum? (bytes-length src)])
         void?]
)]{

Unsafe versions of @racket[bytes-length], @racket[bytes-ref],
@racket[bytes-set!], and @racket[bytes-copy!].
A bytes's size can never be larger than a
@tech{fixnum} (so even @racket[bytes-length] always returns a
fixnum).

@history[#:changed "7.5.0.15" @elem{Added @racket[unsafe-bytes-copy!].}]}


@defproc[(unsafe-bytes->immutable-bytes! [bstr bytes?]) (and/c bytes? immutable?)]{

Similar to @racket[bytes->immutable-bytes], but potentially destroys
@racket[bstr] and reuses it space, so @racket[bstr] must not be used
after calling @racket[unsafe-bytes->immutable-bytes!].

@history[#:added "7.7.0.6"]}


@deftogether[(
@defproc[(unsafe-fxvector-length [v fxvector?]) fixnum?]
@defproc[(unsafe-fxvector-ref [v fxvector?] [k fixnum?]) fixnum?]
@defproc[(unsafe-fxvector-set! [v fxvector?] [k fixnum?] [x fixnum?]) void?]
)]{

Unsafe versions of @racket[fxvector-length], @racket[fxvector-ref], and
@racket[fxvector-set!]. A @tech{fxvector}'s size can never be larger than a
@tech{fixnum} (so even @racket[fxvector-length] always returns a
fixnum).}


@deftogether[(
@defproc[(unsafe-flvector-length [v flvector?]) fixnum?]
@defproc[(unsafe-flvector-ref [v flvector?] [k fixnum?]) flonum?]
@defproc[(unsafe-flvector-set! [v flvector?] [k fixnum?] [x flonum?]) void?]
)]{

Unsafe versions of @racket[flvector-length], @racket[flvector-ref], and
@racket[flvector-set!]. A @tech{flvector}'s size can never be larger than a
@tech{fixnum} (so even @racket[flvector-length] always returns a
fixnum).}


@deftogether[(
@defproc[(unsafe-f64vector-ref [vec f64vector?] [k fixnum?]) flonum?]
@defproc[(unsafe-f64vector-set! [vec f64vector?] [k fixnum?] [n flonum?]) void?]
)]{

Unsafe versions of @racket[f64vector-ref] and
@racket[f64vector-set!].}


@deftogether[(
@defproc[(unsafe-s16vector-ref [vec s16vector?] [k fixnum?]) (integer-in -32768 32767)]
@defproc[(unsafe-s16vector-set! [vec s16vector?] [k fixnum?] [n (integer-in -32768 32767)]) void?]
)]{

Unsafe versions of @racket[s16vector-ref] and
@racket[s16vector-set!].}


@deftogether[(
@defproc[(unsafe-u16vector-ref [vec u16vector?] [k fixnum?]) (integer-in 0 65535)]
@defproc[(unsafe-u16vector-set! [vec u16vector?] [k fixnum?] [n (integer-in 0 65535)]) void?]
)]{

Unsafe versions of @racket[u16vector-ref] and
@racket[u16vector-set!].}


@deftogether[(
@defproc[(unsafe-struct-ref [v any/c] [k fixnum?]) any/c]
@defproc[(unsafe-struct-set! [v any/c] [k fixnum?] [val any/c]) void?]
@defproc[(unsafe-struct*-ref [v (not/c impersonator?)] [k fixnum?]) any/c]
@defproc[(unsafe-struct*-set! [v (not/c impersonator?)] [k fixnum?] [val any/c]) void?]
@defproc[(unsafe-struct*-cas! [v (not/c impersonator?)] [k fixnum?] [old-val any/c] [new-val any/c]) boolean?]
)]{

Unsafe field access and update for an instance of a structure
type, where the @schemeidfont{struct*} variants can be
faster but do not work on @tech{impersonators}.
The index @racket[k] must be between @racket[0] (inclusive) and
the number of fields in the structure (exclusive). In the case of
@racket[unsafe-struct-set!], @racket[unsafe-struct*-set!], and @racket[unsafe-struct*-cas!], the
field must be mutable. The @racket[unsafe-struct*-cas!] operation
is analogous to @racket[box-cas!] to perform an atomic compare-and-set.

@history[#:changed "6.11.0.2" @elem{Added @racket[unsafe-struct*-cas!].}]}

@deftogether[(
@defproc[(unsafe-mutable-hash-iterate-first
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))])
	  (or/c #f any/c)]
@defproc[(unsafe-mutable-hash-iterate-next
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c])
	  (or/c #f any/c)]
@defproc[(unsafe-mutable-hash-iterate-key
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]) 
	  any/c]
@defproc[#:link-target? #f
         (unsafe-mutable-hash-iterate-key
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]
          [bad-index-v any/c]) 
	  any/c]
@defproc[(unsafe-mutable-hash-iterate-value
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]) 
	  any/c]
@defproc[#:link-target? #f
         (unsafe-mutable-hash-iterate-value
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]
          [bad-index-v any/c]) 
	  any/c]
@defproc[(unsafe-mutable-hash-iterate-key+value
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]) 
	  (values any/c any/c)]
@defproc[#:link-target? #f
         (unsafe-mutable-hash-iterate-key+value
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]
          [bad-index-v any/c])
	  (values any/c any/c)]
@defproc[(unsafe-mutable-hash-iterate-pair
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]) 
	  pair?]
@defproc[#:link-target? #f
         (unsafe-mutable-hash-iterate-pair
          [hash (and/c hash? (not/c immutable?) (not/c hash-weak?))]
	  [pos any/c]
          [bad-index-v any/c]) 
	  pair?]
@defproc[(unsafe-immutable-hash-iterate-first
          [hash (and/c hash? immutable?)])
	  (or/c #f any/c)]
@defproc[(unsafe-immutable-hash-iterate-next
          [hash (and/c hash? immutable?)]
	  [pos any/c])
	  (or/c #f any/c)]
@defproc[(unsafe-immutable-hash-iterate-key
          [hash (and/c hash? immutable?)]
	  [pos any/c]) 
	  any/c]
@defproc[#:link-target? #f
         (unsafe-immutable-hash-iterate-key
          [hash (and/c hash? immutable?)]
	  [pos any/c]
          [bad-index-v any/c]) 
	  any/c]
@defproc[(unsafe-immutable-hash-iterate-value
          [hash (and/c hash? immutable?)]
	  [pos any/c])
	  any/c]
@defproc[#:link-target? #f
         (unsafe-immutable-hash-iterate-value
          [hash (and/c hash? immutable?)]
	  [pos any/c]
          [bad-index-v any/c])
	  any/c]
@defproc[(unsafe-immutable-hash-iterate-key+value
          [hash (and/c hash? immutable?)]
	  [pos any/c])
	  (values any/c any/c)]
@defproc[#:link-target? #f
         (unsafe-immutable-hash-iterate-key+value
          [hash (and/c hash? immutable?)]
	  [pos any/c]
          [bad-index-v any/c])
	  (values any/c any/c)]
@defproc[(unsafe-immutable-hash-iterate-pair
          [hash (and/c hash? immutable?)]
	  [pos any/c])
	  pair?]
@defproc[#:link-target? #f
         (unsafe-immutable-hash-iterate-pair
          [hash (and/c hash? immutable?)]
	  [pos any/c]
          [bad-index-v any/c])
	  pair?]
@defproc[(unsafe-weak-hash-iterate-first
          [hash (and/c hash? hash-weak?)])
	  (or/c #f any/c)]
@defproc[(unsafe-weak-hash-iterate-next
          [hash (and/c hash? hash-weak?)]
	  [pos any/c])
	  (or/c #f any/c)]
@defproc[(unsafe-weak-hash-iterate-key
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]) 
	  any/c]
@defproc[#:link-target? #f
         (unsafe-weak-hash-iterate-key
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]
          [bad-index-v any/c]) 
	  any/c]
@defproc[(unsafe-weak-hash-iterate-value
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]) 
	  any/c]
@defproc[#:link-target? #f
         (unsafe-weak-hash-iterate-value
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]
          [bad-index-v any/c]) 
	  any/c]
@defproc[(unsafe-weak-hash-iterate-key+value
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]) 
	  (values any/c any/c)]
@defproc[#:link-target? #f
         (unsafe-weak-hash-iterate-key+value
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]
          [bad-index-v any/c]) 
	  (values any/c any/c)]
@defproc[(unsafe-weak-hash-iterate-pair
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]) 
	  pair?]
@defproc[#:link-target? #f
         (unsafe-weak-hash-iterate-pair
          [hash (and/c hash? hash-weak?)]
	  [pos any/c]
          [bad-index-v any/c]) 
	  pair?]
)]{
Unsafe versions of @racket[hash-iterate-key] and similar procedures.
These operations support @tech{chaperones} and @tech{impersonators}.

Each unsafe ...@code{-first} and ...@code{-next} procedure may return,
instead of a number index, an internal representation of a view into
the hash structure, enabling faster iteration. The result of these
...@code{-first} and ...@code{-next} functions should be given as
@racket[pos] to the corresponding unsafe accessor functions.

If the @racket[pos] provided to an accessor function for a mutable
@racket[hash] was formerly a @tech{valid hash index} but is no longer
a @tech{valid hash index} for @racket[hash], and if
@racket[bad-index-v] is not provided, then the
@exnraise[exn:fail:contract]. No behavior is specified for a
@racket[pos] that was never a @tech{valid hash index} for
@racket[hash]. Note that @racket[bad-index-v] argument is technically
not useful for the @code{unsafe-immutable-hash-iterate-} functions,
since an index cannot become invalid for an immutable @racket[hash].

@history[#:added "6.4.0.6"
         #:changed "7.0.0.10" @elem{Added the optional @racket[bad-index-v] argument.}]}

@defproc[(unsafe-make-srcloc [source any/c]
                             [line (or/c exact-positive-integer? #f)]
                             [column (or/c exact-nonnegative-integer? #f)]
                             [position (or/c exact-positive-integer? #f)]
                             [span (or/c exact-nonnegative-integer? #f)])
         srcloc?]{

Unsafe version of @racket[srcloc].

@history[#:added "7.2.0.10"]}

@; ------------------------------------------------------------------------

@section[#:tag "unsafeextfl"]{Unsafe Extflonum Operations}

@deftogether[(
@defproc[(unsafe-extfl+   [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(unsafe-extfl-   [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(unsafe-extfl*   [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(unsafe-extfl/   [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(unsafe-extflabs [a extflonum?]) extflonum?]
)]{

Unchecked versions of @racket[extfl+], @racket[extfl-],
@racket[extfl*], @racket[extfl/], and @racket[extflabs].}


@deftogether[(
@defproc[(unsafe-extfl=   [a extflonum?] [b extflonum?]) boolean?]
@defproc[(unsafe-extfl<   [a extflonum?] [b extflonum?]) boolean?]
@defproc[(unsafe-extfl>   [a extflonum?] [b extflonum?]) boolean?]
@defproc[(unsafe-extfl<=  [a extflonum?] [b extflonum?]) boolean?]
@defproc[(unsafe-extfl>=  [a extflonum?] [b extflonum?]) boolean?]
@defproc[(unsafe-extflmin [a extflonum?] [b extflonum?]) extflonum?]
@defproc[(unsafe-extflmax [a extflonum?] [b extflonum?]) extflonum?]
)]{

Unchecked versions of @racket[extfl=], @racket[extfl<],
@racket[extfl>], @racket[extfl<=], @racket[extfl>=], @racket[extflmin], and
@racket[extflmax].}


@deftogether[(
@defproc[(unsafe-extflround [a extflonum?]) extflonum?]
@defproc[(unsafe-extflfloor [a extflonum?]) extflonum?]
@defproc[(unsafe-extflceiling [a extflonum?]) extflonum?]
@defproc[(unsafe-extfltruncate [a extflonum?]) extflonum?]
)]{

Unchecked (potentially) versions of @racket[extflround],
@racket[extflfloor], @racket[extflceiling], and
@racket[extfltruncate]. Currently, these bindings are simply aliases
for the corresponding safe bindings.}


@deftogether[(
@defproc[(unsafe-extflsin [a extflonum?]) extflonum?]
@defproc[(unsafe-extflcos [a extflonum?]) extflonum?]
@defproc[(unsafe-extfltan [a extflonum?]) extflonum?]
@defproc[(unsafe-extflasin [a extflonum?]) extflonum?]
@defproc[(unsafe-extflacos [a extflonum?]) extflonum?]
@defproc[(unsafe-extflatan [a extflonum?]) extflonum?]
@defproc[(unsafe-extfllog [a extflonum?]) extflonum?]
@defproc[(unsafe-extflexp [a extflonum?]) extflonum?]
@defproc[(unsafe-extflsqrt [a extflonum?]) extflonum?]
@defproc[(unsafe-extflexpt [a extflonum?] [b extflonum?]) extflonum?]
)]{

Unchecked (potentially) versions of @racket[extflsin],
@racket[extflcos], @racket[extfltan], @racket[extflasin],
@racket[extflacos], @racket[extflatan], @racket[extfllog],
@racket[extflexp], @racket[extflsqrt], and
@racket[extflexpt]. Currently, some of these bindings are simply
aliases for the corresponding safe bindings.}


@deftogether[(
@defproc[(unsafe-fx->extfl [a fixnum?]) extflonum?]
@defproc[(unsafe-extfl->fx [a extflonum?]) fixnum?]
)]{
Unchecked (potentially) versions of @racket[fx->extfl] and @racket[extfl->fx].

@history[#:changed "7.7.0.8" @elem{Changed @racket[unsafe-fl->fx] to truncate.}]}

@deftogether[(
@defproc[(unsafe-extflvector-length [v extflvector?]) fixnum?]
@defproc[(unsafe-extflvector-ref [v extflvector?] [k fixnum?]) extflonum?]
@defproc[(unsafe-extflvector-set! [v extflvector?] [k fixnum?] [x extflonum?]) void?]
)]{

Unchecked versions of @racket[extflvector-length], @racket[extflvector-ref], and
@racket[extflvector-set!]. A @tech{extflvector}'s size can never be larger than a
@tech{fixnum} (so even @racket[extflvector-length] always returns a
fixnum).}

@; ------------------------------------------------------------------------

@section{Unsafe Impersonators and Chaperones}

@defproc[(unsafe-impersonate-procedure [proc procedure?]
                                       [replacement-proc procedure?]
                                       [prop impersonator-property?]
                                       [prop-val any] ... ...)
         (and/c procedure? impersonator?)]{

 Like @racket[impersonate-procedure], but assumes that
 @racket[replacement-proc] calls @racket[proc] itself. When the result
 of @racket[unsafe-impersonate-procedure] is applied to arguments, the
 arguments are passed on to @racket[replacement-proc] directly,
 ignoring @racket[proc]. At the same time, @racket[impersonator-of?]
 reports @racket[#t] when given the result of
 @racket[unsafe-impersonate-procedure] and @racket[proc].

 If @racket[proc] is itself an impersonator that is derived from
 @racket[impersonate-procedure*] or @racket[chaperone-procedure*],
 beware that @racket[replacement-proc] will not be able to call it
 correctly. Specifically, the impersonator produced by
 @racket[unsafe-impersonate-procedure] will not get passed to a
 wrapper procedure that was supplied to
 @racket[impersonate-procedure*] or @racket[chaperone-procedure*] to
 generate @racket[proc].

 Finally, unlike @racket[impersonate-procedure],
 @racket[unsafe-impersonate-procedure] does not specially handle
 @racket[impersonator-prop:application-mark] as a @racket[prop].

 The unsafety of @racket[unsafe-impersonate-procedure] is limited to
 the above differences from @racket[impersonate-procedure]. The
 contracts on the arguments of @racket[unsafe-impersonate-procedure] are
 checked when the arguments are supplied.

 As an example, assuming that @racket[f] accepts a single argument and
 is not derived from @racket[impersonate-procedure*] or
 @racket[chaperone-procedure*], then
 @racketblock[(λ (f)
                (unsafe-impersonate-procedure
                 f
                 (λ (x)
                   (if (number? x)
                       (error 'no-numbers!)
                       (f x)))))]
 is equivalent to
 @racketblock[(λ (f)
                (impersonate-procedure
                 f
                 (λ (x)
                   (if (number? x)
                       (error 'no-numbers!)
                       x))))]
 
 Similarly, with the same assumptions about @racket[f], the following
 two procedures @racket[_wrap-f1] and
 @racket[_wrap-f2] are almost equivalent; they differ only
 in the error message produced when their arguments are
 functions that return multiple values (and that they update
 different global variables). The version using @racket[unsafe-impersonate-procedure]
 will signal an error in the @racket[let] expression about multiple
 return values, whereas the one using @racket[impersonate-procedure] signals
 an error from @racket[impersonate-procedure] about multiple return values.
 @racketblock[(define log1-args '())
              (define log1-results '())
              (define wrap-f1
                (λ (f)
                  (impersonate-procedure
                   f
                   (λ (arg)
                     (set! log1-args (cons arg log1-args))
                     (values (λ (res)
                               (set! log1-results (cons res log1-results))
                               res)
                             arg)))))
              
              (define log2-args '())
              (define log2-results '())
              (define wrap-f2
                (λ (f)
                  (unsafe-impersonate-procedure
                   f
                   (λ (arg)
                     (set! log2-args (cons arg log2-args))
                     (let ([res (f arg)])
                       (set! log2-results (cons res log2-results))
                       res)))))]

 @history[#:added "6.4.0.4"]
}


@defproc[(unsafe-chaperone-procedure [proc procedure?]
                                     [wrapper-proc procedure?]
                                     [prop impersonator-property?]
                                     [prop-val any] ... ...)
         (and/c procedure? chaperone?)]{
 Like @racket[unsafe-impersonate-procedure], but creates a @tech{chaperone}.
 Since @racket[wrapper-proc] will be called in lieu of @racket[proc],
 @racket[wrapper-proc] is assumed to return a chaperone of the value that
 @racket[proc] would return.

 @history[#:added "6.4.0.4"]
}

@defproc[(unsafe-impersonate-vector [vec vector?]
                                    [replacement-vec (and/c vector? (not/c impersonator?))]
                                    [prop impersonator-property?]
                                    [prop-val any/c] ... ...)
         (and/c vector? impersonator?)]{
 Like @racket[impersonate-vector], but instead of going through interposition procedures, all
 accesses to the impersonator are dispatched to @racket[replacement-vec].

 The result of @racket[unsafe-impersonate-vector] is an impersonator of @racket[vec].

 @history[#:added "6.9.0.2"]
}
@defproc[(unsafe-chaperone-vector [vec vector?]
                                  [replacement-vec (and/c vector? (not/c impersonator?))]
                                  [prop impersonator-property?]
                                  [prop-val any/c] ... ...)
         (and/c vector? chaperone?)]{
 Like @racket[unsafe-impersonate-vector], but the result of @racket[unsafe-chaperone-vector] is a
 chaperone of @racket[vec].

 @history[#:added "6.9.0.2"]
}

@; ------------------------------------------------------------------------

@include-section["unsafe-undefined.scrbl"]
