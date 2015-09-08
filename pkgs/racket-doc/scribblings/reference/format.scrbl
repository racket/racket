#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          "mz.rkt"
          (for-label racket/contract
                     racket/math
                     racket/format
                     racket/string))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/math racket/format)))

@title[#:tag "format"]{Converting Values to Strings}

@note-lib[racket/format]

The @racketmodname[racket/format] library provides functions for
converting Racket values to strings. In addition to features like
padding and numeric formatting, the functions have the virtue of being
shorter than @racket[format] (with format string),
@racket[number->string], or @racket[string-append].

@defproc[(~a [v any/c] ...
             [#:separator separator string? ""]
             [#:width width (or/c exact-nonnegative-integer? #f) #f]
             [#:max-width max-width (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
             [#:min-width min-width exact-nonnegative-integer? (or width 0)]
             [#:limit-marker limit-marker string? ""]
             [#:align align (or/c 'left 'center 'right) 'left]
             [#:pad-string pad-string non-empty-string? " "]
             [#:left-pad-string left-pad-string non-empty-string? pad-string]
             [#:right-pad-string right-pad-string non-empty-string? pad-string])
         string?]{

Converts each @racket[v] to a string in @racket[display] mode---that
is, like @racket[(format "~a" v)]---then concatentates the results
with @racket[separator] between consecutive items, and then pads or
truncates the string to be at least @racket[min-width] characters and
at most @racket[max-width] characters.

@interaction[#:eval the-eval
(~a "north")
(~a 'south)
(~a #"east")
(~a #\w "e" 'st)
(~a (list "red" 'green #"blue"))
(~a 17)
(eval:alts (~a @#,(racketvalfont "#e1e20")) (~a #e1e20))
(~a pi)
(~a (expt 6.1 87))
]

The @racket[~a] function is primarily useful for strings, numbers, and other
atomic data. The @racket[~v] and @racket[~s] functions are better suited to
compound data.

Let @racket[_s] be the concatenated string forms of the @racket[v]s
plus separators. If @racket[_s] is longer than @racket[max-width]
characters, it is truncated to exactly @racket[max-width]
characters. If @racket[_s] is shorter than @racket[min-width]
characters, it is padded to exactly @racket[min-width]
characters. Otherwise @racket[_s] is returned unchanged. If
@racket[min-width] is greater than @racket[max-width], an exception is
raised.

If @racket[_s] is longer than @racket[max-width] characters, it is
truncated and the end of the string is replaced with
@racket[limit-marker]. If @racket[limit-marker] is longer than
@racket[max-width], an exception is raised.

@interaction[#:eval the-eval
(~a "abcde" #:max-width 5)
(~a "abcde" #:max-width 4)
(~a "abcde" #:max-width 4 #:limit-marker "*")
(~a "abcde" #:max-width 4 #:limit-marker "...")
(~a "The quick brown fox" #:max-width 15 #:limit-marker "")
(~a "The quick brown fox" #:max-width 15 #:limit-marker "...")
]

If @racket[_s] is shorter than @racket[min-width], it is padded to at
least @racket[min-width] characters. If @racket[align] is
@racket['left], then only right padding is added; if @racket[align]
is @racket['right], then only left padding is added; and if
@racket[align] is @racket['center], then roughly equal amounts of
left padding and right padding are added.

Padding is specified as a non-empty string. Left padding consists of
@racket[left-pad-string] repeated in its entirety as many times as
possible followed by a @emph{prefix} of @racket[left-pad-string] to fill
the remaining space. In contrast, right padding consists of a
@emph{suffix} of @racket[right-pad-string] followed by a number of copies
of @racket[right-pad-string] in its entirety. Thus left padding starts
with the start of @racket[left-pad-string] and right padding ends with
the end of @racket[right-pad-string].

@interaction[#:eval the-eval
(~a "apple" #:min-width 20 #:align 'left)
(~a "pear" #:min-width 20 #:align 'left #:right-pad-string " .")
(~a "plum" #:min-width 20 #:align 'right #:left-pad-string ". ")
(~a "orange" #:min-width 20 #:align 'center
              #:left-pad-string "- " #:right-pad-string " -")
]

Use @racket[width] to set both @racket[max-width] and @racket[min-width]
simultaneously, ensuring that the resulting string is exactly
@racket[width] characters long:

@interaction[#:eval the-eval
(~a "terse" #:width 6)
(~a "loquacious" #:width 6)
]
}

@;{----------------------------------------}

@defproc[(~v [v any/c] ...
             [#:separator separator string? " "]
             [#:width width (or/c exact-nonnegative-integer? #f) #f]
             [#:max-width max-width (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
             [#:min-width min-width exact-nonnegative-integer? (or width 0)]
             [#:limit-marker limit-marker string? "..."]
             [#:align align (or/c 'left 'center 'right) 'left]
             [#:pad-string pad-string non-empty-string? " "]
             [#:left-pad-string left-pad-string non-empty-string? pad-string]
             [#:right-pad-string right-pad-string non-empty-string? pad-string])
         string?]{

Like @racket[~a], but each value is converted like @racket[(format
"~v" v)], the default separator is @racket[" "], and the default limit
marker is @racket["..."].

@interaction[#:eval the-eval
(~v "north")
(~v 'south)
(~v #"east")
(~v #\w)
(~v (list "red" 'green #"blue"))
]

Use @racket[~v] to produce text that talks about Racket values.

@interaction[#:eval the-eval
(let ([nums (for/list ([i 10]) i)])
  (~a "The even numbers in " (~v nums) 
      " are " (~v (filter even? nums)) "."))
]}

@;{----------------------------------------}

@defproc[(~s [v any/c] ...
             [#:separator separator string? " "]
             [#:width width (or/c exact-nonnegative-integer? #f) #f]
             [#:max-width max-width (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
             [#:min-width min-width exact-nonnegative-integer? (or width 0)]
             [#:limit-marker limit-marker string? "..."]
             [#:align align (or/c 'left 'center 'right) 'left]
             [#:pad-string pad-string non-empty-string? " "]
             [#:left-pad-string left-pad-string non-empty-string? pad-string]
             [#:right-pad-string right-pad-string non-empty-string? pad-string])
         string?]{

Like @racket[~a], but each value is converted like @racket[(format
"~s" v)], the default separator is @racket[" "], and the default limit
marker is @racket["..."].

@interaction[#:eval the-eval
(~s "north")
(~s 'south)
(~s #"east")
(~s #\w)
(~s (list "red" 'green #"blue"))
]
}

@;{----------------------------------------}

@defproc[(~e [v any/c] ...
             [#:separator separator string? " "]
             [#:width width (or/c exact-nonnegative-integer? #f) #f]
             [#:max-width max-width (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
             [#:min-width min-width exact-nonnegative-integer? (or width 0)]
             [#:limit-marker limit-marker string? "..."]
             [#:align align (or/c 'left 'center 'right) 'left]
             [#:pad-string pad-string non-empty-string? " "]
             [#:left-pad-string left-pad-string non-empty-string? pad-string]
             [#:right-pad-string right-pad-string non-empty-string? pad-string])
         string?]{

Like @racket[~a], but each value is converted like @racket[(format
"~e" v)], the default separator is @racket[" "], and the default limit
marker is @racket["..."].

@interaction[#:eval the-eval
(~e "north")
(~e 'south)
(~e #"east")
(~e #\w)
(~e (list "red" 'green #"blue"))
]

}

@;{----------------------------------------}

@defproc[(~r   [x rational?]
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
                            6]
               [#:notation notation
                           (or/c 'positional 'exponential
                                 (-> rational? (or/c 'positional 'exponential)))
                           'positional]
               [#:format-exponent format-exponent
                (or/c #f string? (-> exact-integer? string?))
                #f]
               [#:min-width min-width exact-positive-integer? 1]
               [#:pad-string pad-string non-empty-string? " "])
         string?]{

Converts the rational number @racket[x] to a string in either
positional or exponential notation, depending on
@racket[notation]. The exactness or inexactness of @racket[x] does not
affect its formatting.

The optional arguments control number formatting:

@itemize[

@item{@racket[notation] --- determines whether the number is printed
in positional or exponential notation. If @racket[notation] is a
function, it is applied to @racket[x] to get the notation to be used.

@interaction[#:eval the-eval
(~r 12345)
(~r 12345 #:notation 'exponential)
(let ([pick-notation
       (lambda (x)
         (if (or (< (abs x) 0.001) (> (abs x) 1000))
             'exponential
             'positional))])
  (for/list ([i (in-range 1 5)])
    (~r (expt 17 i) #:notation pick-notation)))
]
}

@item{@racket[precision] --- controls the number of digits after the
decimal point (or more accurately, the
@hyperlink["http://en.wikipedia.org/wiki/Radix_point"]{radix point}).
When @racket[x] is formatted in exponential form, @racket[precision]
applies to the significand.

If @racket[precision] is a natural number, then up to @racket[precision] digits are
displayed, but trailing zeroes are dropped, and if all digits after the decimal
point are dropped the decimal point is also dropped. If @racket[precision] is
@racket[(list '= _digits)], then exactly @racket[_digits] digits after the
decimal point are used, and the decimal point is never dropped.

@interaction[#:eval the-eval
(~r pi)
(~r pi #:precision 4)
(~r pi #:precision 0)
(~r 1.5 #:precision 4)
(~r 1.5 #:precision '(= 4))
(~r 50 #:precision 2)
(~r 50 #:precision '(= 2))
(~r 50 #:precision '(= 0))
]}

@item{@racket[min-width] --- if @racket[x] would normally be printed
with fewer than @racket[min-width] digits (including the decimal
point but not including the sign indicator), the digits are left-padded
using @racket[pad-string].

@interaction[#:eval the-eval
(~r 17)
(~r 17 #:min-width 4)
(~r -42 #:min-width 4)
(~r 1.5 #:min-width 4)
(~r 1.5 #:precision 4 #:min-width 10)
(~r 1.5 #:precision '(= 4) #:min-width 10)
(eval:alts (~r @#,(racketvalfont "#e1e10") #:min-width 6)
           (~r #e1e10 #:min-width 6))
]}

@item{@racket[pad-string] --- specifies the string used to pad the
number to at least @racket[min-width] characters (not including the
sign indicator). The padding is placed between the sign and the normal
digits of @racket[x].

@interaction[#:eval the-eval
(~r 17 #:min-width 4 #:pad-string "0")
(~r -42 #:min-width 4 #:pad-string "0")
]}

@item{@racket[sign] --- controls how the sign of the number is
indicated:
  @itemlist[

  @item{If @racket[sign] is @racket[#f] (the default), no sign output is
  generated if @racket[x] is either positive or zero, and a minus sign is
  prefixed if @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (~r x))
  ]}

  @item{If @racket[sign] is @racket['+], no sign output is generated if
  @racket[x] is zero, a plus sign is prefixed if @racket[x] is positive, and a
  minus sign is prefixed if @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (~r x #:sign '+))
  ]}

  @item{If @racket[sign] is @racket['++], a plus sign is prefixed if @racket[x]
  is zero or positive, and a minus sign is prefixed if @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (~r x #:sign '++))
  ]}

  @item{If @racket[sign] is @racket['parens], no sign output is generated if
  @racket[x] is zero or positive, and the number is enclosed in parentheses if
  @racket[x] is negative.

  @interaction[#:eval the-eval
  (for/list ([x '(17 0 -42)]) (~r x #:sign 'parens))
  ]}

  @item{If @racket[sign] is @racket[(list _pos-ind _zero-ind _neg-ind)], then
  @racket[_pos-ind], @racket[_zero-ind], and @racket[_neg-ind] are used to
  indicate positive, zero, and negative numbers, respectively. Each indicator is
  either a string to be used as a prefix or a list containing two strings: a
  prefix and a suffix.

  @interaction[#:eval the-eval
  (let ([sign-table '(("" " up") "an even " ("" " down"))])
    (for/list ([x '(17 0 -42)]) (~r x #:sign sign-table)))
  ]

  The default behavior is equivalent to @racket['("" "" "-")]; the
  @racket['parens] mode is equivalent to @racket['("" "" ("(" ")"))].
  }
]}

@item{@racket[base] --- controls the base that @racket[x] is formatted in. If
@racket[base] is a number greater than @racket[10], then lower-case letters are
used. If @racket[base] is @racket[(list 'up _base*)] and @racket[_base*] is
greater than @racket[10], then upper-case letters are used.

@interaction[#:eval the-eval
(~r 100 #:base 7)
(~r 4.5 #:base 2)
(~r 3735928559 #:base 16)
(~r 3735928559 #:base '(up 16))
(~r 3735928559 #:base '(up 16) #:notation 'exponential)
]}

@item{@racket[format-exponent] --- determines how the exponent is displayed. 

If @racket[format-exponent] is a string, the exponent is displayed with an
explicit sign (as with a @racket[sign] of @racket['++]) and at least two
digits, separated from the significand by the ``exponent marker''
@racket[format-exponent]:

@interaction[#:eval the-eval
(~r 1234 #:notation 'exponential #:format-exponent "E")
]

If @racket[format-exponent] is @racket[#f], the ``exponent marker'' is
@racket["e"] if @racket[base] is @racket[10] and a string involving
@racket[base] otherwise:

@interaction[#:eval the-eval
(~r 1234 #:notation 'exponential)
(~r 1234 #:notation 'exponential #:base 8)
]

If @racket[format-exponent] is a procedure, it is applied to the exponent and
the resulting string is appended to the significand:

@interaction[#:eval the-eval
(~r 1234 #:notation 'exponential
         #:format-exponent (lambda (e) (format "E~a" e)))
]}

]
}

@; ----------------------------------------

@deftogether[(
@defproc[(~.a [v any/c] ...
              [#:separator separator string? ""]
              [#:width width (or/c exact-nonnegative-integer? #f) #f]
              [#:max-width max-width (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
              [#:min-width min-width exact-nonnegative-integer? (or width 0)]
              [#:limit-marker limit-marker string? ""]
              [#:align align (or/c 'left 'center 'right) 'left]
              [#:pad-string pad-string non-empty-string? " "]
              [#:left-pad-string left-pad-string non-empty-string? pad-string]
              [#:right-pad-string right-pad-string non-empty-string? pad-string])
         string?]
@defproc[(~.v [v any/c] ...
              [#:separator separator string? " "]
              [#:width width (or/c exact-nonnegative-integer? #f) #f]
              [#:max-width max-width (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
              [#:min-width min-width exact-nonnegative-integer? (or width 0)]
              [#:limit-marker limit-marker string? "..."]
              [#:align align (or/c 'left 'center 'right) 'left]
              [#:pad-string pad-string non-empty-string? " "]
              [#:left-pad-string left-pad-string non-empty-string? pad-string]
              [#:right-pad-string right-pad-string non-empty-string? pad-string])
         string?]
@defproc[(~.s [v any/c] ...
              [#:separator separator string? " "]
              [#:width width (or/c exact-nonnegative-integer? #f) #f]
              [#:max-width max-width (or/c exact-nonnegative-integer? +inf.0) (or width +inf.0)]
              [#:min-width min-width exact-nonnegative-integer? (or width 0)]
              [#:limit-marker limit-marker string? "..."]
              [#:align align (or/c 'left 'center 'right) 'left]
              [#:pad-string pad-string non-empty-string? " "]
              [#:left-pad-string left-pad-string non-empty-string? pad-string]
              [#:right-pad-string right-pad-string non-empty-string? pad-string])
         string?]
)]{

Like @racket[~a], @racket[~v], and @racket[~s], but each @racket[v] is
formatted like @racket[(format "~.a" v)], @racket[(format "~.v" v)],
and @racket[(format "~.s" v)], respectively.}


@; ----------------------------------------

@(close-eval the-eval)
