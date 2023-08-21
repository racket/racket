#lang scribble/doc
@(require "mz.rkt")

@(define (UCat x) x)

@title[#:tag "characters"]{Characters}

@guideintro["characters"]{characters}

@deftech{Characters} range over Unicode
@index['("scalar value")]{scalar values}, which includes
characters whose values range from @racketvalfont{#x0} to
@racketvalfont{#x10FFFF}, but not including @racketvalfont{#xD800} to
@racketvalfont{#xDFFF}. The scalar values are a subset of the Unicode
@index['("code point")]{code points}.

Two characters are @racket[eqv?] if they correspond to the same scalar
value. For each scalar value less than 256, character values that are
@racket[eqv?] are also @racket[eq?]. Characters produced by the default
reader are @tech{interned} in @racket[read-syntax] mode.

@see-read-print["character"]{characters}

@history[#:changed "6.1.1.8" @elem{Updated from Unicode 5.0.1 to Unicode 7.0.0.}]

@; ----------------------------------------
@section{Characters and Scalar Values}

@defproc[(char? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is a character, @racket[#f]
otherwise.}


@defproc[(char->integer [char char?]) exact-integer?]{

Returns a character's code-point number.

@mz-examples[(char->integer #\A)]}


@defproc[(integer->char [k (and/c exact-integer?
                                  (or/c (integer-in 0 @#,racketvalfont{#xD7FF})
                                        (integer-in @#,racketvalfont{#xE000} @#,racketvalfont{#x10FFFF})))])
         char?]{

Return the character whose code-point number is @racket[k]. For
@racket[k] less than @racket[256], the result is the same object for
the same @racket[k].

@mz-examples[(integer->char 65)]}


@defproc[(char-utf-8-length [char char?]) (integer-in 1 6)]{

Produces the same result as @racket[(bytes-length (string->bytes/utf-8
(string char)))].}


@; ----------------------------------------
@section{Character Comparisons}

@defproc[(char=? [char1 char?] [char2 char?] ...) boolean?]{

Returns @racket[#t] if all of the arguments are @racket[eqv?].

@mz-examples[(char=? #\a #\a)
          (char=? #\a #\A #\a)]

@history/arity[]}

@(define (char-sort direction folded?)
   (if folded?
     @elem{Like @racket[char-ci<?], but checks whether the arguments would be @direction after case-folding.}
     @elem{Like @racket[char<?], but checks whether the arguments are @|direction|.}))

@defproc[(char<? [char1 char?] [char2 char?] ...) boolean?]{

Returns @racket[#t] if the arguments are sorted increasing, where
two characters are ordered by their scalar values, @racket[#f]
otherwise.

@mz-examples[(char<? #\A #\a)
             (char<? #\a #\A)
             (char<? #\a #\b #\c)]

@history/arity[]}

@defproc[(char<=? [char1 char?] [char2 char?] ...) boolean?]{
 @char-sort["nondecreasing" #f]

@mz-examples[(char<=? #\A #\a)
             (char<=? #\a #\A)
             (char<=? #\a #\b #\b)]

@history/arity[]}

@defproc[(char>? [char1 char?] [char2 char?] ...) boolean?]{
 @char-sort["decreasing" #f]

@mz-examples[(char>? #\A #\a)
             (char>? #\a #\A)
             (char>? #\c #\b #\a)]

@history/arity[]}

@defproc[(char>=? [char1 char?] [char2 char?] ...) boolean?]{
 @char-sort["nonincreasing" #f]

@mz-examples[(char>=? #\A #\a)
             (char>=? #\a #\A)
             (char>=? #\c #\b #\b)]

@history/arity[]}


@defproc[(char-ci=? [char1 char?] [char2 char?] ...) boolean?]{
 Returns @racket[#t] if all of the arguments are @racket[eqv?] after
 locale-insensitive case-folding via @racket[char-foldcase].

@mz-examples[(char-ci=? #\A #\a)
             (char-ci=? #\a #\a #\a)]

@history/arity[]}

@defproc[(char-ci<? [char1 char?] [char2 char?] ...) boolean?]{
 Like @racket[char<?], but checks whether the arguments would be in
 increasing order if each was first case-folded using
 @racket[char-foldcase] (which is locale-insensitive).

@mz-examples[(char-ci<? #\A #\a)
             (char-ci<? #\a #\b)
             (char-ci<? #\a #\b #\c)]

@history/arity[]}

@defproc[(char-ci<=? [char1 char?] [char2 char?] ...) boolean?]{
 @char-sort["nondecreasing" #t]

@mz-examples[(char-ci<=? #\A #\a)
             (char-ci<=? #\a #\A)
             (char-ci<=? #\a #\b #\b)]

@history/arity[]}

@defproc[(char-ci>? [char1 char?] [char2 char?] ...) boolean?]{
 @char-sort["decreasing" #t]

@mz-examples[(char-ci>? #\A #\a)
             (char-ci>? #\b #\A)
             (char-ci>? #\c #\b #\a)]

@history/arity[]}

@defproc[(char-ci>=? [char1 char?] [char2 char?] ...) boolean?]{
 @char-sort["nonincreasing" #t]

@mz-examples[(char-ci>=? #\A #\a)
             (char-ci>=? #\a #\A)
             (char-ci>=? #\c #\b #\b)]

@history/arity[]}

@; ----------------------------------------
@section{Classifications}

@defproc[(char-alphabetic? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char] has the Unicode ``Alphabetic''
property.}

@defproc[(char-lower-case? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char] has the Unicode ``Lowercase''
property.}


@defproc[(char-upper-case? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char] has the Unicode ``Uppercase''
property.}

@defproc[(char-title-case? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char]'s Unicode general category is
@UCat{Lt}, @racket[#f] otherwise.}

@defproc[(char-numeric? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char] has a Unicode ``Numeric_Type''
property value that is not @litchar{None}.}

@defproc[(char-symbolic? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char]'s Unicode general category is
@UCat{Sm}, @UCat{Sc}, @UCat{Sk}, or @UCat{So}, @racket[#f] otherwise.}

@defproc[(char-punctuation? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char]'s Unicode general category is
@UCat{Pc}, @UCat{Pd}, @UCat{Ps}, @UCat{Pe}, @UCat{Pi}, @UCat{Pf}, or
@UCat{Po}, @racket[#f] otherwise.}

@defproc[(char-graphic? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char]'s Unicode general category is
@UCat{Ll}, @UCat{Lm}, @UCat{Lo}, @UCat{Lt}, @UCat{Lu}, @UCat{Nd}, @UCat{Nl}, @UCat{No}, 
@UCat{Mn}, @UCat{Mc}, or @UCat{Me}, or if one of the following produces
@racket[#t] when applied to @racket[char]: @racket[char-alphabetic?],
@racket[char-numeric?], @racket[char-symbolic?], or
@racket[char-punctuation?].}

@defproc[(char-whitespace? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char] has the Unicode ``White_Space''
property.}

@defproc[(char-blank? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char]'s Unicode general category is
@UCat{Zs} or if @racket[char] is @racket[#\tab]. (These correspond to
horizontal whitespace.)}

@defproc[(char-iso-control? [char char?]) boolean?]{

Return @racket[#t] if @racket[char] is between @racket[#\u0000] and
@racket[#\u001F] inclusive or @racket[#\u007F] and @racket[#\u009F]
inclusive.}

@defproc[(char-extended-pictographic? [char char?]) boolean?]{

Returns @racket[#t] if @racket[char] has the Unicode ``Extended_Pictographic''
property.

@history[#:added "8.6.0.1"]}

@defproc[(char-general-category [char char?]) symbol?]{

Returns a symbol representing the character's Unicode general
category, which is @indexed-racket['lu], @indexed-racket['ll],
@indexed-racket['lt], @indexed-racket['lm], @indexed-racket['lo],
@indexed-racket['mn], @indexed-racket['mc], @indexed-racket['me],
@indexed-racket['nd], @indexed-racket['nl], @indexed-racket['no],
@indexed-racket['ps], @indexed-racket['pe], @indexed-racket['pi],
@indexed-racket['pf], @indexed-racket['pd], @indexed-racket['pc],
@indexed-racket['po], @indexed-racket['sc], @indexed-racket['sm],
@indexed-racket['sk], @indexed-racket['so], @indexed-racket['zs],
@indexed-racket['zp], @indexed-racket['zl], @indexed-racket['cc],
@indexed-racket['cf], @indexed-racket['cs], @indexed-racket['co], or
@indexed-racket['cn].}

@defproc[(char-grapheme-break-property [char char?]) ?]{

Returns the Unicode graheme-break property for @racket[char], which is
@indexed-racket['Other], @indexed-racket['CR], @indexed-racket['LF],
@indexed-racket['Control], @indexed-racket['Extend],
@indexed-racket['ZWJ], @indexed-racket['Regional_Indicator],
@indexed-racket['Prepend], @indexed-racket['SpacingMark],
@indexed-racket['L], @indexed-racket['V], @indexed-racket['T],
@indexed-racket['LV], or @indexed-racket['LVT].

@history[#:added "8.6.0.1"]}

@defproc[(make-known-char-range-list) 
         (listof (list/c exact-nonnegative-integer?
                         exact-nonnegative-integer?
                         boolean?))]{

Produces a list of three-element lists, where each three-element list
represents a set of consecutive code points for which the Unicode
standard specifies character properties. Each three-element list
contains two integers and a boolean; the first integer is a starting
code-point value (inclusive), the second integer is an ending
code-point value (inclusive), and the boolean is @racket[#t] when all
characters in the code-point range have identical results for all of
the character predicates above, have analogous transformations
(shifting by the same amount, if any, in code-point space) for
@racket[char-downcase], @racket[char-upcase], and
@racket[char-titlecase], and have the same
decomposition--normalization behavior.
The three-element lists are ordered in
the overall result list such that later lists represent larger
code-point values, and all three-element lists are separated from
every other by at least one code-point value that is not specified by
Unicode.}


@; ----------------------------------------
@section{Character Conversions}

@defproc[(char-upcase [char char?]) char?]{

Produces a character consistent with the 1-to-1 code point mapping
defined by Unicode. If @racket[char] has no upcase mapping,
@racket[char-upcase] produces @racket[char].

@margin-note{String procedures, such as @racket[string-upcase], handle
the case where Unicode defines a locale-independent mapping from the
code point to a code-point sequence (in addition to the 1-1 mapping on
scalar values).}

@mz-examples[
(char-upcase #\a)
(char-upcase #\u03BB)
(char-upcase #\space)
]}


@defproc[(char-downcase [char char?]) char?]{

Like @racket[char-upcase], but for the Unicode downcase mapping.

@mz-examples[
(char-downcase #\A)
(char-downcase #\u039B)
(char-downcase #\space)
]}

@defproc[(char-titlecase [char char?]) char?]{

Like @racket[char-upcase], but for the Unicode titlecase mapping.

@mz-examples[
(char-upcase #\a)
(char-upcase #\u03BB)
(char-upcase #\space)
]}

@defproc[(char-foldcase [char char?]) char?]{

Like @racket[char-upcase], but for the Unicode case-folding mapping.

@mz-examples[
(char-foldcase #\A)
(char-foldcase #\u03A3)
(char-foldcase #\u03c2)
(char-foldcase #\space)
]}

@; ----------------------------------------
@section{Character Grapheme-Cluster Streaming}

@defproc[(char-grapheme-step [char char?]
                             [state fixnum?])
         (values boolean? fixnum?)]{

Encodes a state machine for Unicode's grapheme-cluster specification
on a sequence of code points. It accepts a character for the next code
point in a sequence, and it returns two values: whether a (single)
grapheme cluster has terminated since the most recently reported
termination (or the start of the stream), and a new state to be used
with @racket[char-grapheme-step] and the next character.

A value of @racket[0] for @racket[state] represents the initial state
or a state where no characters are pending toward a new boundary.
Thus, if a sequence of characters is exhausted and accumulated
@racket[state] is not @racket[0], then the end of the stream creates
one last grapheme-cluster boundary. When
@racket[char-grapheme-step] produces a true value as its first
result and a non-@racket[0] value as its second result, then the given
@racket[char] must be the only character pending toward the next
grapheme cluster (by the rules of Unicode grapheme clustering).

The @racket[char-grapheme-step] procedure will produce a
result for any fixnum @scheme[state], but the meaning of a
non-@racket[0] @scheme[state] is specified only in that providing such
a state produced by @racket[char-grapheme-step] in another
call to @racket[char-grapheme-step] continues detecting
grapheme-cluster boundaries in the sequence.

See also @racket[string-grapheme-span] and
@racket[string-grapheme-count].

@mz-examples[
(char-grapheme-step #\a 0)
(let*-values ([(consumed? state) (char-grapheme-step #\a 0)]
              [(consumed? state) (char-grapheme-step #\b state)])
  (values consumed? state))
(let*-values ([(consumed? state) (char-grapheme-step #\return 0)]
              [(consumed? state) (char-grapheme-step #\newline state)])
  (values consumed? state))
(eval:alts
 (let*-values ([(consumed? state) (char-grapheme-step #\a 0)]
               [(consumed? state) (char-grapheme-step @#,racketvalfont{#\u300} state)])
   (values consumed? state))
 (let*-values ([(consumed? state) (char-grapheme-step #\a 0)]
               [(consumed? state) (char-grapheme-step #\u300 state)])
   (values consumed? state)))
]

@history[#:added "8.6.0.2"]}

