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

@; ----------------------------------------
@section{Characters and Scalar Values}

@defproc[(char? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is a character, @racket[#f]
otherwise.}


@defproc[(char->integer [char char?]) exact-integer?]{

Returns a character's code-point number.

@mz-examples[(char->integer #\A)]}


@defproc[(integer->char [k (and/c exact-integer?
                                  (or/c (integer-in 0 #xD7FF)
                                        (integer-in #xE000 #x10FFFF)))])
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

@defproc[(char=? [char1 char?] [char2 char?] ...+) boolean?]{

Returns @racket[#t] if all of the arguments are @racket[eqv?].

@mz-examples[(char=? #\a #\a)
          (char=? #\a #\A #\a)]}

@(define (char-sort direction folded?)
   (if folded?
     @elem{Like @racket[char-ci<?], but checks whether the arguments would be @direction after case-folding.}
     @elem{Like @racket[char<?], but checks whether the arguments are @|direction|.}))

@defproc[(char<? [char1 char?] [char2 char?] ...+) boolean?]{

Returns @racket[#t] if the arguments are sorted increasing, where
two characters are ordered by their scalar values, @racket[#f]
otherwise.

@mz-examples[(char<? #\A #\a)
             (char<? #\a #\A)
             (char<? #\a #\b #\c)]}

@defproc[(char<=? [char1 char?] [char2 char?] ...+) boolean?]{
 @char-sort["nondecreasing" #f]

@mz-examples[(char<=? #\A #\a)
             (char<=? #\a #\A)
             (char<=? #\a #\b #\b)]}

@defproc[(char>? [char1 char?] [char2 char?] ...+) boolean?]{
 @char-sort["decreasing" #f]

@mz-examples[(char>? #\A #\a)
             (char>? #\a #\A)
             (char>? #\c #\b #\a)]}

@defproc[(char>=? [char1 char?] [char2 char?] ...+) boolean?]{
 @char-sort["nonincreasing" #f]

@mz-examples[(char>=? #\A #\a)
             (char>=? #\a #\A)
             (char>=? #\c #\b #\b)]}


@defproc[(char-ci=? [char1 char?] [char2 char?] ...+) boolean?]{
 Returns @racket[#t] if all of the arguments are @racket[eqv?] after
 locale-insensitive case-folding via @racket[char-foldcase].

@mz-examples[(char-ci=? #\A #\a)
             (char-ci=? #\a #\a #\a)]}

@defproc[(char-ci<? [char1 char?] [char2 char?] ...+) boolean?]{
 Like @racket[char<?], but checks whether the arguments would be in
 increasing order if each was first case-folded using
 @racket[char-foldcase] (which is locale-insensitive).

@mz-examples[(char-ci<? #\A #\a)
             (char-ci<? #\a #\b)
             (char-ci<? #\a #\b #\c)]}

@defproc[(char-ci<=? [char1 char?] [char2 char?] ...+) boolean?]{
 @char-sort["nondecreasing" #t]

@mz-examples[(char-ci<=? #\A #\a)
             (char-ci<=? #\a #\A)
             (char-ci<=? #\a #\b #\b)]}

@defproc[(char-ci>? [char1 char?] [char2 char?] ...+) boolean?]{
 @char-sort["decreasing" #t]

@mz-examples[(char-ci>? #\A #\a)
             (char-ci>? #\b #\A)
             (char-ci>? #\c #\b #\a)]}

@defproc[(char-ci>=? [char1 char?] [char2 char?] ...+) boolean?]{
 @char-sort["nonincreasing" #t]

@mz-examples[(char-ci>=? #\A #\a)
             (char-ci>=? #\a #\A)
             (char-ci>=? #\c #\b #\b)]}

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

Returns @racket[#t] if @racket[char] has the Unicode ``Numeric''
property.}

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
the character predicates above. The three-element lists are ordered in
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
