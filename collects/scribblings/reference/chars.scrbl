#lang scribble/doc
@(require "mz.ss")

@(define (UCat x) x)

@title[#:tag "characters"]{Characters}

@guideintro["characters"]{characters}

@deftech{Characters} range over Unicode
@index['("scalar value")]{scalar values}, which includes
characters whose values range from @schemevalfont{#x0} to
@schemevalfont{#x10FFFF}, but not including @schemevalfont{#xD800} to
@schemevalfont{#xDFFF}. The scalar values are a subset of the Unicode
@index['("code point")]{code points}.

Two characters are @scheme[eqv?] if they correspond to the same scalar
value. For each scalar value less than 256, character values that are
@scheme[eqv?] are also @scheme[eq?].

@; ----------------------------------------
@section{Characters and Scalar Values}

@defproc[(char? [v any/c]) boolean?]{

Return @scheme[#t] if @scheme[v] is a character, @scheme[#f]
otherwise.}


@defproc[(char->integer [char char?]) exact-integer?]{

Returns a character's code-point number.

@mz-examples[(char->integer #\A)]}


@defproc[(integer->char [k (and/c exact-integer?
                                  (or/c (integer-in 0 #xD7FF)
                                        (integer-in #xE000 #x10FFFF)))])
         char?]{

Return the character whose code-point number is @scheme[k]. For
@scheme[k] less than @scheme[256], the result is the same object for
the same @scheme[k].

@mz-examples[(integer->char 65)]}


@defproc[(char-utf-8-length [char char?]) (integer-in 1 6)]{

Produces the same result as @scheme[(bytes-length (string->bytes/utf-8
(string char)))].}


@; ----------------------------------------
@section{Character Comparisons}

@defproc[(char=? [char1 char?] [char2 char?] ...+) boolean?]{

Returns @scheme[#t] if all of the arguments are @scheme[eqv?].

@mz-examples[(char=? #\a #\a)
          (char=? #\a #\A #\a)]}

@(define (char-sort direction folded?)
   (if folded?
     @elem{Like @scheme[char-ci<?], but checks whether the arguments would be @direction after case-folding.}
     @elem{Like @scheme[char<?], but checks whether the arguments are @|direction|.}))

@defproc[(char<? [char1 char?] [char2 char?] ...+) boolean?]{

Returns @scheme[#t] if the arguments are sorted increasing, where
two characters are ordered by their scalar values, @scheme[#f]
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
 Returns @scheme[#t] if all of the arguments are @scheme[eqv?] after
 locale-insensitive case-folding via @scheme[char-foldcase].

@mz-examples[(char-ci=? #\A #\a)
             (char-ci=? #\a #\a #\a)]}

@defproc[(char-ci<? [char1 char?] [char2 char?] ...+) boolean?]{
 Like @scheme[char<?], but checks whether the arguments would be in
 increasing order if each was first case-folded using
 @scheme[char-foldcase] (which is locale-insensitive).

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

Returns @scheme[#t] if @scheme[char] has the Unicode ``Alphabetic''
property.}

@defproc[(char-lower-case? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char] has the Unicode ``Lowercase''
property.}


@defproc[(char-upper-case? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char] has the Unicode ``Uppercase''
property.}

@defproc[(char-title-case? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char]'s Unicode general category is
@UCat{Lt}, @scheme[#f] otherwise.}

@defproc[(char-numeric? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char] has the Unicode ``Numeric''
property.}

@defproc[(char-symbolic? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char]'s Unicode general category is
@UCat{Sm}, @UCat{Sc}, @UCat{Sk}, or @UCat{So}, @scheme[#f] otherwise.}

@defproc[(char-punctuation? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char]'s Unicode general category is
@UCat{Pc}, @UCat{Pd}, @UCat{Ps}, @UCat{Pe}, @UCat{Pi}, @UCat{Pf}, or
@UCat{Po}, @scheme[#f] otherwise.}

@defproc[(char-graphic? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char]'s Unicode general category is
@UCat{Mn}, @UCat{Mc}, @UCat{Me}, or if one of the following produces
@scheme[#t] when applied to @scheme[char]: @scheme[char-alphabetic?],
@scheme[char-numeric?], @scheme[char-symbolic?], or
@scheme[char-punctuation?].}

@defproc[(char-whitespace? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char] has the Unicode ``White_Space''
property.}

@defproc[(char-blank? [char char?]) boolean?]{

Returns @scheme[#t] if @scheme[char]'s Unicode general category is
@UCat{Zs} or if @scheme[char] is @scheme[#\tab]. (These correspond to
horizontal whitespace.)}

@defproc[(char-iso-control? [char char?]) boolean?]{

Return @scheme[#t] if @scheme[char] is between @scheme[#\u0000] and
@scheme[#\u001F] inclusive or @scheme[#\u007F] and @scheme[#\u009F]
inclusive.}

@defproc[(char-general-category [char char?]) symbol?]{

Returns a symbol representing the character's Unicode general
category, which is @indexed-scheme['lu], @indexed-scheme['ll],
@indexed-scheme['lt], @indexed-scheme['lm], @indexed-scheme['lo],
@indexed-scheme['mn], @indexed-scheme['mc], @indexed-scheme['me],
@indexed-scheme['nd], @indexed-scheme['nl], @indexed-scheme['no],
@indexed-scheme['ps], @indexed-scheme['pe], @indexed-scheme['pi],
@indexed-scheme['pf], @indexed-scheme['pd], @indexed-scheme['pc],
@indexed-scheme['po], @indexed-scheme['sc], @indexed-scheme['sm],
@indexed-scheme['sk], @indexed-scheme['so], @indexed-scheme['zs],
@indexed-scheme['zp], @indexed-scheme['zl], @indexed-scheme['cc],
@indexed-scheme['cf], @indexed-scheme['cs], @indexed-scheme['co], or
@indexed-scheme['cn].}

@defproc[(make-known-char-range-list) 
         (listof (list/c exact-nonnegative-integer?
                         exact-nonnegative-integer?
                         boolean?))]{

Produces a list of three-element lists, where each three-element list
represents a set of consecutive code points for which the Unicode
standard specifies character properties. Each three-element list
contains two integers and a boolean; the first integer is a starting
code-point value (inclusive), the second integer is an ending
code-point value (inclusive), and the boolean is @scheme[#t] when all
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
defined by Unicode. If @scheme[char] has no upcase mapping,
@scheme[char-upcase] produces @scheme[char].

@margin-note{String procedures, such as @scheme[string-upcase], handle
the case where Unicode defines a locale-independent mapping from the
code point to a code-point sequence (in addition to the 1-1 mapping on
scalar values).}

@mz-examples[
(char-upcase #\a)
(char-upcase #\u03BB)
(char-upcase #\space)
]}


@defproc[(char-downcase [char char?]) char?]{

Like @scheme[char-upcase], but for the Unicode downcase mapping.

@mz-examples[
(char-downcase #\A)
(char-downcase #\u039B)
(char-downcase #\space)
]}

@defproc[(char-titlecase [char char?]) char?]{

Like @scheme[char-upcase], but for the Unicode titlecase mapping.

@mz-examples[
(char-upcase #\a)
(char-upcase #\u03BB)
(char-upcase #\space)
]}

@defproc[(char-foldcase [char char?]) char?]{

Like @scheme[char-upcase], but for the Unicode case-folding mapping.

@mz-examples[
(char-foldcase #\A)
(char-foldcase #\u03A3)
(char-foldcase #\u03c2)
(char-foldcase #\space)
]}
