#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          (for-label racket/base
                     racket/contract
                     racket/math
                     unstable/cat
                     unstable/contract))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/math unstable/cat)))

@title[#:tag "cat"]{Converting Values to Strings}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/cat]

This module provides a few functions for converting Racket values to strings. In
addition to features like padding and numeric formatting, the functions have the
virtue of being shorter and more pleasant to type than @racket[format] (with
format string), @racket[number->string], or @racket[string-append].

@defproc[(cat [v any/c] ...
              [#:width width (or/c exact-nonnegative-integer? #f) #f]
              [#:limit limit (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
              [#:limit-marker limit-marker string? "..."]
              [#:pad-to pad-to exact-nonnegative-integer? (or width 0)]
              [#:align align (or/c 'left 'center 'right) 'left]
              [#:padding padding non-empty-string? " "]
              [#:left-padding left-padding non-empty-string? padding]
              [#:right-padding right-padding non-empty-string? padding])
         string?]{

Converts each @racket[v] to a string in @racket[display] mode---that
is, like @racket[(format "~a" v)]---then concatentates the results and
pads or truncates the string to be at least @racket[pad-to] characters
and at most @racket[limit] characters.

@interaction[#:eval the-eval
(cat "north")
(cat 'south)
(cat #"east")
(cat #\w "e" 'st)
(cat (list "red" 'green #"blue"))
(cat 17)
(cat #e1e20)
(cat pi)
(cat (expt 6.1 87))
]

The @racket[cat] function is primarily useful for strings, numbers, and other
atomic data. The @racket[catp] and @racket[catw] functions are better suited to
compound data.

Let @racket[_s] be the concatenated string forms of the
@racket[v]s. If @racket[_s] is longer than @racket[limit] characters,
it is truncated to exactly @racket[limit] characters. If @racket[_s]
is shorter than @racket[pad-to] characters, it is padded to exactly
@racket[pad-to] characters. Otherwise @racket[_s] is returned
unchanged. If @racket[pad-to] is greater than @racket[limit], an
exception is raised.

If @racket[_s] is longer than @racket[limit] characters, it is truncated and the
end of the string is replaced with @racket[limit-marker]. If
@racket[limit-marker] is longer than @racket[limit], an exception is raised.

@interaction[#:eval the-eval
(cat "abcde" #:limit 5)
(cat "abcde" #:limit 4)
(cat "abcde" #:limit 4 #:limit-marker "*")
(cat "abcde" #:limit 4 #:limit-marker "")
(cat "The quick brown fox" #:limit 15 #:limit-marker "")
(cat "The quick brown fox" #:limit 15 #:limit-marker "...")
]

If @racket[_s] is shorter than @racket[pad-to], it is padded to at
least @racket[pad-to] characters. If @racket[align] is
@racket['left], then only right padding is added; if @racket[align]
is @racket['right], then only left padding is added; and if
@racket[align] is @racket['center], then roughly equal amounts of
left padding and right padding are added.

Padding is specified as a non-empty string. Left padding consists of
@racket[left-padding] repeated in its entirety as many times as
possible followed by a @emph{prefix} of @racket[left-padding] to fill
the remaining space. In contrast, right padding consists of a
@emph{suffix} of @racket[right-padding] followed by a number of copies
of @racket[right-padding] in its entirety. Thus left padding starts
with the start of @racket[left-padding] and right padding ends with
the end of @racket[right-padding].

@interaction[#:eval the-eval
(cat "apple" #:pad-to 20 #:align 'left)
(cat "pear" #:pad-to 20 #:align 'left #:right-padding " .")
(cat "plum" #:pad-to 20 #:align 'right #:left-padding ". ")
(cat "orange" #:pad-to 20 #:align 'center
              #:left-padding "- " #:right-padding " -")
]

Use @racket[width] to set both @racket[limit] and @racket[pad-to]
simultaneously, ensuring that the resulting string is exactly
@racket[width] characters long:

@interaction[#:eval the-eval
(cat "terse" #:width 6)
(cat "loquacious" #:width 6)
]
}

@;{----------------------------------------}

@defproc[(catp [v any/c]
               [#:width width (or/c exact-nonnegative-integer? #f) #f]
               [#:limit limit (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
               [#:limit-marker limit-marker string? "..."]
               [#:pad-to pad-to exact-nonnegative-integer? (or width 0)]
               [#:align align (or/c 'left 'center 'right) 'left]
               [#:padding padding non-empty-string? " "]
               [#:left-padding left-padding non-empty-string? padding]
               [#:right-padding right-padding non-empty-string? padding])
         string?]{

Like @racket[cat], but converts a single @racket[v] to a string in
@racket[print] mode---that is, like @racket[(format "~v" v)].

@interaction[#:eval the-eval
(catp "north")
(catp 'south)
(catp #"east")
(catp #\w)
(catp (list "red" 'green #"blue"))
]

Use @racket[catp] to produce text that talks about Racket values.

@interaction[#:eval the-eval
(let ([nums (for/list ([i 10]) i)])
  (cat "The even numbers in " (catp nums) 
       " are " (catp (filter even? nums)) "."))
]
}

@;{----------------------------------------}

@defproc[(catw [v any/c]
               [#:width width (or/c exact-nonnegative-integer? #f) #f]
               [#:limit limit (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
               [#:limit-marker limit-marker string? "..."]
               [#:pad-to pad-to exact-nonnegative-integer? (or width 0)]
               [#:align align (or/c 'left 'center 'right) 'left]
               [#:padding padding non-empty-string? " "]
               [#:left-padding left-padding non-empty-string? padding]
               [#:right-padding right-padding non-empty-string? padding])
         string?]{

Like @racket[cat], but converts a single @racket[v] to a string in
@racket[write] mode---that is, like @racket[(format "~s" v)].

@interaction[#:eval the-eval
(catw "north")
(catw 'south)
(catw #"east")
(catw #\w)
(catw (list "red" 'green #"blue"))
]
}

@;{----------------------------------------}

@defproc[(catn [x rational?]
               [#:sign sign
                       (or/c #f '+ '++ 'parens
                             (let ([ind (or/c string? (list/c string? string?))])
                               (list/c ind ind ind)))
                       #f]
               [#:base base
                       (or/c (integer-in 2 36) (list/c 'up (integer-in 2 36)))
                       10]
               [#:precision precision
                            (or/c exact-nonnegative-integer?
                                  (list/c '= exact-nonnegative-integer?)) 
                            3]
               [#:pos/exp-range 
                pos/exp-range
                (list/c (or/c exact-integer? +inf.0)
                        (or/c exact-integer? -inf.0))
                (list -inf.0 +inf.0)]
               [#:exp-precision exp-precision
                                (or/c exact-nonnegative-integer?
                                      (list/c '= exact-nonnegative-integer?))
                                5]
               [#:exp-format-exponent exp-format-exponent
                (or/c #f string? (-> exact-integer? string?))
                #f]
               [#:pad-digits-to pad-digits-to exact-positive-integer? 1]
               [#:digits-padding digits-padding non-empty-string? " "])
         string?]{

Converts the rational number @racket[x] to a string in either
positional or exponential notation. The exactness or inexactness of
@racket[x] does not affect its formatting.

Numbers whose order of magnitude (with respect to @racket[base]) fall
strictly within @racket[pos/exp-range] are formatted using positional
notation. More precisely, if the following condition holds:

@racketblock[(or (zero? x)
                 (< (expt base (car pos/exp-range))
                    (abs x) 
                    (expt base (cadr pos/exp-range))))]

then the result is equivalent to 

@racketblock[(catnp x
                    #:sign sign-mode
                    #:base base
                    #:precision precision
                    #:pad-digits-to pad-digits-to
                    #:digits-padding digits-padding)]

Otherwise, the number is formatted in exponential notation, and the
result is equivalent to

@racketblock[(catne x
                    #:sign sign-mode
                    #:base base
                    #:precision exp-precision
                    #:format-exponent exp-format-exponent
                    #:pad-digits-to pad-digits-to
                    #:digits-padding digits-padding)]

@examples[#:eval the-eval
(catn 999 #:pos/exp-range '(0 3))
(catn 1000 #:pos/exp-range '(0 3))
(catn 0.9876 #:pos/exp-range '(0 3))
]

Note that the default value of @racket[pos/exp-range] ensures that
positional notation will be used for any rational @racket[x].
}


@defproc[(catnp [x rational?]
                [#:sign sign
                        (or/c #f '+ '++ 'parens
                              (let ([ind (or/c string? (list/c string? string?))])
                                (list/c ind ind ind)))
                        #f]
                [#:base base
                        (or/c (integer-in 2 36) (list/c 'up (integer-in 2 36)))
                        10]
                [#:precision precision
                             (or/c exact-nonnegative-integer?
                                   (list/c '= exact-nonnegative-integer?)) 
                             3]
                [#:pad-digits-to pad-digits-to exact-positive-integer? 1]
                [#:digits-padding digits-padding non-empty-string? " "])
         string?]{

Formats the rational number @racket[x] using positional notation according to
the following arguments:

@itemize[

@item{@racket[precision] controls the number of digits after the decimal point
(or more accurately, the
@hyperlink["http://en.wikipedia.org/wiki/Radix_point"]{radix point}). If
@racket[precision] is a natural number, then up to @racket[precision] digits are
displayed, but trailing zeroes are dropped, and if all digits after the decimal
point are dropped the decimal point is also dropped. If @racket[precision] is
@racket[(list '= _digits)], then exactly @racket[_digits] digits after the
decimal point are used, and the decimal point is never dropped.

@interaction[#:eval the-eval
(catnp pi)
(catnp pi #:precision 4)
(catnp pi #:precision 0)
(catnp 1.5 #:precision 4)
(catnp 1.5 #:precision '(= 4))
(catnp 50 #:precision 2)
(catnp 50 #:precision '(= 2))
(catnp 50 #:precision '(= 0))
]}

@item{@racket[pad-digits-to]: if @racket[x] would normally be printed
with fewer than @racket[pad-digits-to] digits (including the decimal
point but not including the sign indicator), the output is left-padded
using @racket[digits-padding].

@interaction[#:eval the-eval
(catnp 17)
(catnp 17 #:pad-digits-to 4)
(catnp -42 #:pad-digits-to 4)
(catnp 1.5 #:pad-digits-to 4)
(catnp 1.5 #:precision 4 #:pad-digits-to 10)
(catnp 1.5 #:precision '(= 4) #:pad-digits-to 10)
]}

@item{@racket[digits-padding] specifies the string used to pad the
number to at least @racket[pad-digits-to] characters (not including the
sign indicator). The padding is placed between the sign and the normal
digits of @racket[x].

@interaction[#:eval the-eval
(catnp 17 #:pad-digits-to 4 #:digits-padding "0")
(catnp -42 #:pad-digits-to 4 #:digits-padding "0")
]}

@item{@racket[sign] controls how the sign of the number is
indicated.
  @itemlist[

  @item{If @racket[sign] is @racket[#f] (the default), no sign output is
  generated if @racket[x] is either positive or zero, and a minus sign is
  prefixed if @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (catnp x))
  ]}

  @item{If @racket[sign] is @racket['+], no sign output is generated if
  @racket[x] is zero, a plus sign is prefixed if @racket[x] is positive, and a
  minus sign is prefixed if @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (catnp x #:sign '+))
  ]}

  @item{If @racket[sign] is @racket['++], a plus sign is prefixed if @racket[x]
  is zero or positive, and a minus sign is prefixed if @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (catnp x #:sign '++))
  ]}

  @item{If @racket[sign] is @racket['parens], no sign output is generated if
  @racket[x] is zero or positive, and the number is enclosed in parentheses if
  @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (catnp x #:sign 'parens))
  ]}

  @item{If @racket[sign] is @racket[(list _pos-ind _zero-ind _neg-ind)], then
  @racket[_pos-ind], @racket[_zero-ind], and @racket[_neg-ind] are used to
  indicate positive, zero, and negative numbers, respectively. Each indicator is
  either a string to be used as a prefix or a list containing two strings: a
  prefix and a suffix.

  @interaction[#:eval the-eval
  (let ([sign-table '(("" " up") "an even " ("" " down"))])
    (for/list ([x '(17 0 -42)]) (catnp x #:sign sign-table)))
  ]

  The default behavior is equivalent to @racket['("" "" "-")]; the
  @racket['parens] mode is equivalent to @racket['("" "" ("(" ")"))].
  }
]}

@item{@racket[base] controls the base that @racket[x] is formatted in. If
@racket[base] is a number greater than @racket[10], then lower-case letters are
used. If @racket[base] is @racket[(list 'up _base*)] and @racket[_base*] is
greater than @racket[10], then upper-case letters are used.

@interaction[#:eval the-eval
(catnp 100 #:base 7)
(catnp 4.5 #:base 2)
(catnp 3735928559 #:base 16)
(catnp 3735928559 #:base '(up 16))
]}

]
}


@defproc[(catne [x rational?]
                [#:sign sign
                        (or/c #f '+ '++ 'parens
                              (let ([ind (or/c string? (list/c string? string?))])
                                (list/c ind ind ind)))
                        #f]
                [#:base base
                        (or/c (integer-in 2 36) (list/c 'up (integer-in 2 36)))
                        10]
                [#:precision precision
                             (or/c exact-nonnegative-integer?
                                   (list/c '= exact-nonnegative-integer?)) 
                             5]
                [#:format-exponent
                 format-exponent
                 (or/c #f string? (-> exact-integer? string?))
                 #f]
                [#:pad-digits-to pad-digits-to exact-positive-integer? 1]
                [#:digits-padding digits-padding non-empty-string? " "])
         string?]{

Formats the rational number @racket[x] in exponential notation according to the
following arguments:

@itemlist[

@item{@racket[base], @racket[sign] are interpreted as in positional notation,
described above, except that they apply only to the significand, not the
exponent.

@interaction[#:eval the-eval
(catne -100 #:base 2)
]}

@item{@racket[format-exponent] determines how the exponent is displayed. 

If @racket[format-exponent] is a string, the exponent is displayed with an
explicit sign (as with a @racket[sign-mode] of @racket['++]) and at least two
digits, separated from the significand by the ``exponent marker''
@racket[format-exponent]:

@interaction[#:eval the-eval
(catne 1234 #:format-exponent "E")
]

If @racket[format-exponent] is @racket[#f], the ``exponent marker'' is
@racket["e"] if @racket[base] is @racket[10] and a string involving
@racket[base] otherwise:

@interaction[#:eval the-eval
(catne 1234)
(catne 1234 #:base 8)
]

If @racket[format-exponent] is a procedure, it is applied to the exponent and
the resulting string is appended to the significand:

@interaction[#:eval the-eval
(catne 1234 #:format-exponent (lambda (e) (format "E~a" e)))
]}

@item{@racket[precision] determines how many digits after the radix point the
significand contains. Like the @racket[precision] argument of @racket[catnp],
the form @racket[(list '= _digits)] causes trailing zeroes to be retained.

@interaction[#:eval the-eval
(catne 12345 #:precision 3)
(catne 12345 #:precision 2)
(catne 10000 #:precision 2)
(catne 10000 #:precision '(= 2))
]}

@item{@racket[pad-digits-to] and @racket[digits-padding] are interpreted as in
positional notation.

@interaction[#:eval the-eval
(catne 12345 #:pad-digits-to 12 #:digits-padding " ")
]}

]

@;{
Note that unlike @racket[string->number] (and thus @racket[cat]), @racket[catn]
does not use exponential notation for large (or small) inexact numbers.  Large
numbers will be displayed with a large number of digits, many of which are not
significant.

@interaction[#:eval the-eval
(cat (expt 6.1 87))
(catn (expt 6.1 87))
(code:line (catn (+ (expt 6.1 87) 1000)) (code:comment "how many of these digits are significant?"))
]}
}

@(close-eval the-eval)
