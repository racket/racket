#lang at-exp racket 

;; weed out: string-copy, eqv?, struct? -- no need for beginners, move to advanced 
;; eq? is questionable, but okay if someone uses BSL to teach not out of HtDP 
;; 


(require mzlib/etc mzlib/list mzlib/math syntax/docprovide
         (for-syntax "firstorder.rkt")
         (for-syntax syntax/parse)
         "provide-and-scribble.rkt")

;; Implements the procedures:
(require "teachprims.rkt" "teach.rkt" lang/posn lang/imageeq scribble/manual)

(define-syntax (provide-and-wrap stx)
  (syntax-parse stx 
    [(provide-and-wrap wrap doc-tag:id (title (defproc (name args ...) range w ...) ...) ...)
     (with-syntax ([((f ...) ...) (map generate-temporaries (syntax->list #'((name ...) ...)))]
                   [((internal-name ...) ...)
                    (map (lambda (x)
                           (map (lambda (n) 
                                  (syntax-parse n
                                    [(internal-name:id external-name:id) #'internal-name]
                                    [n:id #'n]))
                                (syntax->list x)))
                         (syntax->list #'((name ...) ...)))]
                   [((external-name ...) ...)
                    (map (lambda (x)
                           (map (lambda (n) 
                                  (syntax-parse n
                                    [(internal-name:id external-name:id) #'external-name]
                                    [n:id #'n]))
                                (syntax->list x)))
                         (syntax->list #'((name ...) ...)))])
       #'(begin ;; create two modules: 
           ;; one that makes definitions first-order 
           (module+ with-wrapper 
                    (wrap f internal-name) ... ...
                    (provide-and-scribble 
                     doc-tag (title (defproc ((f external-name) args ...) range w ...) ...) ...))
           ;; and one that doesn't
           (module+ without-wrapper 
                    (provide-and-scribble
                     doc-tag (title (defproc (name args ...) range w ...) ...) ...))))]))

(define-syntax (in-rator-position-only stx)
  (syntax-case stx ()
    [(_ new-name orig-name)
     (let ([new (syntax new-name)]
           [orig (syntax orig-name)])
       (cond
         ;; Some things are not really functions:
         [(memq (syntax-e orig) '(pi e null eof))
          #'(define new-name orig-name)]
         [else 
          #'(define-syntax new-name 
              (make-first-order
               (lambda (stx)
                 (syntax-case stx ()
                   [(id . args) (syntax/loc stx (beginner-app orig-name . args))]
                   [_else
                    (raise-syntax-error
                     #f
                     "expected a function call but there is no open parenthesis before this function"
                     stx)]))
               #'orig-name))]))]))

;; procedures with documentation:
(provide-and-wrap
 in-rator-position-only
 procedures
 
 ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
  @defproc[(number? [n any/c]) boolean?]{Determines whether some value is a number.}
  @defproc[(= [x number][y number][z number] ...) boolean?]{Compares numbers for equality.}
  @defproc[(< [x real][y real][z real] ...) boolean?]{Compares real numbers for less-than.}
  @defproc[(> [x real][y real][z real] ...) boolean?]{Compares real numbers for greater-than.}
  @defproc[(<= [x real][y real][z real] ...) boolean?]{Compares real numbers for less-than or equality.}
  @defproc[(>= [x real][y real][z real] ...) boolean?]{Compares real numbers for greater-than or equality.}
  @defproc[((beginner-+ +) [x number][y number][z number] ...) number]{Evaluates the sum of the input numbers.}
  @defproc[(- [x number][y number] ...) number]{subtracts the second (and following) number(s) from the first ; negate the number if there is only one argument.}
  @defproc[((beginner-* *) [x number][y number][z number] ...) number]{Evaluates the product of all of the input numbers.}
  @defproc[((beginner-/ /) [x number][y number][z number] ...) number]{
                                                                       Divides the first by the second (and all following) number(s) ;
                                                                       try (/ 3 4) and (/ 3 2 2)  only the first number can be zero.}
  @defproc[(max [x real][y real] ...) real]{Determines the largest number.}
  @defproc[(min [x real][y real] ...) real]{Determines the smallest number.     }
  @defproc[(quotient [x integer][y integer]) integer]{
                                                      Divides the second integer---also called divisor---into the first---known as 
                                                      dividend---to obtain the quotient; try (quotient 3 4) and (quotient 4 3).}
  @defproc[(remainder [x integer][y integer]) integer]{Determines the remainder of dividing the first by the second integer (exact or inexact).}
  @defproc[(modulo [x integer][y integer]) integer]{Finds the remainder of the division of the first number by the second ; try (modulo 4 3) (modulo 4 -3).}
  @defproc[((beginner-sqr sqr) [x number]) number]{Evaluates the square of a number.}
  @defproc[(sqrt [x number]) number]{Evaluates the square root of a number.}
  @defproc[(integer-sqrt [x number]) integer]{Evaluates the integer (exact or inexact) square root of a number.}
  @defproc[(expt [x number][y number]) number]{Evaluates the power of the first to the second number.}
  @defproc[(abs [x real]) real]{Evaluates the absolute value of a real number.}
  @defproc[(sgn [x real]) (union 1 #i1.0 0 #i0.0 -1 #i-1.0)]{Evaluates the sign of a real number.}
  
  ;; fancy numeric 
  @defproc[(exp [x number]) number]{Evaluates e raised to a number.}
  @defproc[(log [x number]) number]{Evaluates the base-e logarithm of a number.}
  
  ;; trigonometry
  @defproc[(sin [x number]) number]{Evaluates the sine of a number (radians).}
  @defproc[(cos [x number]) number]{Evaluates the cosine of a number (radians).}
  @defproc[(tan [x number]) number]{Evaluates the tangent of a number (radians).}
  @defproc[(asin [x number]) number]{Evaluates the arcsine (inverse of sin) of a number.}
  @defproc[(acos [x number]) number]{Evaluates the arccosine (inverse of cos) of a number.}
  @defproc[(atan [x number] [y number]) number]{Evaluates the arctan of the given number or the ratio of the two given numbers.}
  @defproc[(sinh [x number]) number]{Evaluates the hyperbolic sine of a number.}
  @defproc[(cosh [x number]) number]{Evaluates the hyperbolic cosine of a number.}
  
  @defproc[(exact? [x number]) boolean?]{Determines whether some number is exact.}
  
  @defproc[(integer? [x any/c]) boolean?]{Determines whether some value is an integer (exact or inexact).}
  
  @defproc[(zero? [x number]) boolean?]{Determines if some value is zero or not. }
  @defproc[(positive? [x number]) boolean?]{Determines if some value is strictly larger than zero.}
  @defproc[(negative? [x number]) boolean?]{Determines if some value is strictly smaller than zero.}
  @defproc[(odd? [x integer]) boolean?]{Determines if some integer (exact or inexact) is odd or not.}
  @defproc[(even? [x integer]) boolean?]{Determines if some integer (exact or inexact) is even or not.}
  
  @defproc[(add1 [x number]) number]{Evaluates a number one larger than a given number.}
  @defproc[(sub1 [x number]) number]{Evaluates a number one smaller than a given number.}
  @defproc[(lcm [x integer][y integer] ...) integer]{Evaluates the least common multiple of two integers (exact or inexact).}
  @defproc[(gcd [x integer][y integer] ...) integer]{Evaluates the greatest common divisior of two integers (exact or inexact).}
  @defproc[(rational? [x any/c]) boolean?]{Determines whether some value is a rational number.}
  @defproc[(numerator [x rational?]) integer]{Evaluates the numerator of a rational.}
  @defproc[(denominator [x rational?]) integer]{Evaluates the denominator of a rational.}
  @defproc[(inexact? [x number]) boolean?]{Determines whether some number is inexact.}
  @defproc[(real? [x any/c]) boolean?]{Determines whether some value is a real number.}
  @defproc[(floor [x real]) integer]{Determines the closest integer (exact or inexact) below a real number.}
  @defproc[(ceiling [x real]) integer]{Determines the closest integer (exact or inexact) above a real number.}
  @defproc[(round [x real]) integer]{Rounds a real number to an integer (rounds to even to break ties).}
  @defproc[(complex? [x any/c]) boolean?]{Determines whether some value is complex.}
  @defproc[(make-polar [x real real]) number]{Creates a complex from a magnitude and angle.}
  @defproc[(make-rectangular [x real][y real]) number]{Creates a complex from a real and an imaginary part.}
  @defproc[(real-part [x number]) real]{Extracts the real part from a complex number.}
  @defproc[(imag-part [x number]) real]{Extracts the imaginary part from a complex number.}
  @defproc[(magnitude [x number]) real]{Determines the magnitude of a complex number.}
  @defproc[(angle [x number]) real]{Extracts the angle from a complex number.}
  @defproc[(conjugate [x number]) number]{Evaluates the conjugate of a complex number.}
  @defproc[(exact->inexact [x number]) number]{Converts an exact number to an inexact one.}
  @defproc[(inexact->exact [x number]) number]{Approximates an inexact number by an exact one.}
  @defproc[(number->string [x number]) string]{Converts a number to a string.}
  @defproc[(integer->char [x integer]) char]{Lookups the character that corresponds to the given integer (exact only!) in the ASCII table (if any).}
  @defproc[((beginner-random random) [x integer]) integer]{Generates a random natural number less than some given integer (exact only!).}
  @defproc[(current-seconds) integer]{Evaluates the current time in seconds elapsed (since a platform-specific starting date).}
  @defproc[(e) real]{Euler's number.}
  @defproc[(pi) real]{The ratio of a circle's circumference to its diameter.})
 
 ("Booleans"
  @defproc[(boolean? [x any/c]) boolean?]{Determines whether some value is a boolean.}
  @defproc[(boolean=? [x boolean?][y boolean?]) boolean?]{Determines whether two booleans are equal.}
  @defproc[(false? [x any/c]) boolean?]{Determines whether a value is false.}
  @defproc[((beginner-not not) [x boolean?]) boolean?]{Evaluates the negation of a boolean value.})
 
 ("Symbols"
  @defproc[(symbol? [x any/c]) boolean?]{Determines whether some value is a symbol.}
  @defproc[(symbol=? [x symbol][y symbol]) boolean?]{Determines whether two symbols are equal.}
  @defproc[(symbol->string [x symbol]) string]{Converts a symbol to a string. })
 
 ("Lists"
  @defproc[(cons? [x any/c]) boolean?]{Determines whether some value is a constructed list.}
  @defproc[(empty? [x any/c]) boolean?]{Determines whether some value is the empty list.}
  @defproc[(null? [x any/c]) boolean?]{Determines whether some value is the empty list.}
  @defproc[(null) list]{Another name for the empty list}
  @defproc[((beginner-cons cons) [x any/x][y list?]) list?]{Constructs a list.}
  @defproc[((beginner-first first) [x cons?]) any/c]{Selects the first item of a non-empty list.}
  @defproc[((beginner-car car) [x cons?]) any/c]{Selects the first item of a non-empty list.}
  @defproc[((beginner-rest rest) [x cons?]) any/c]{Selects the rest of a non-empty list.}
  @defproc[((beginner-cdr cdr) [x cons?]) any/c]{Selects the rest of a non-empty list.}
  @defproc[(second [x list?]) any/c]{Selects the second item of a non-empty list.}
  @defproc[(cadr [x list?]) any/c]{Selects the second item of a non-empty list.}
  @defproc[(cdar [x list?]) list?]{Selects the rest of a non-empty list in a list. }
  @defproc[(caar [x list?]) any/c]{Selects the first item of the first list in a list.}
  @defproc[(cddr [x list?]) list? ]{Selects the rest of the rest of a list.}
  @defproc[(third [x list?]) any/c]{Selects the third item of a non-empty list.}
  @defproc[(caddr [x list?]) any/c]{Selects the third item of a non-empty list.}
  @defproc[(caadr [x list?]) any/c]{Selects the rest of the first list in the first list of a list.}
  @defproc[(caaar [x list?]) any/c]{Selects the first item of the first list in the first list of a list.}
  @defproc[(cdaar [x list?]) any/c]{Selects the rest of the first list in the first list of a list.}
  @defproc[(cdadr [x list?]) any/c]{Selects the rest of the first list in the rest of a list.}
  @defproc[(cadar [x list?]) any/c]{Selects the second item of the first list of a list.}
  @defproc[(cddar [x list?]) any/c]{Selects the rest of the rest of the first list of a list.}
  @defproc[(cdddr [x list?]) any/c]{Selects the rest of the rest of the rest of a list.}
  @defproc[(fourth [x list?]) any/c]{Selects the fourth item of a non-empty list.}
  @defproc[(cadddr [x list?]) any/c]{Selects the fourth item of a non-empty list.}
  @defproc[(fifth [x list?]) any/c]{Selects the fifth item of a non-empty list.}
  @defproc[(sixth [x list?]) any/c]{Selects the sixth item of a non-empty list.}
  @defproc[(seventh [x list?]) any/c]{Selects the seventh item of a non-empty list.}
  @defproc[(eighth [x list?]) any/c]{Selects the eighth item of a non-empty list.}
  @defproc[(list-ref [x list?][i natural?]) any/c]{Extracts the indexed item from the list.}
  @defproc[(list [x any/c] ... ) list?]{Constructs a list of its arguments.}
  @defproc[(make-list [i natural-number] [x any/c]) list?]{Constructs a list of k (the first argument) copies of x (the second argument).}
  @defproc[((beginner-list* list*) [x any/c]  ... [l list?]) list?]{Constructs a list by adding multiple items to a list.}
  @defproc[((beginner-range range) [x number][y number][z number]) list?]{(range start end step) constructs a list of numbers by _step_ping from _start_ to _end_ list.}
  @defproc[((beginner-append append) [x list?][y list?][z list?]  ...) list?]{Creates a single list from several, by juxtaposition of the items.}
  @defproc[(length (l list?)) natural-number?]{Evaluates the number of items on a list.}
  @defproc[(memq [x any/c][l list?]) (or/c false list?)]{Determines whether some value is on some list if so, it produces the
                                                         suffix of the list that starts with x if not, it produces false.  (It
                                                         compares values with the eq? predicate.)}
  @defproc[(memv [x any/c][l list?]) (or/c false list)]{Determines whether some value is on the list if so, it produces the
                                                        suffix of the list that starts with x if not, it produces false. (It
                                                        compares values with the eqv? predicate.)}
  @defproc[((beginner-member? member?) [x any/c][l list?]) boolean?]{Determines whether some value is on the list (comparing values with equal?).}
  @defproc[((beginner-member member) [x any/c][l list?]) boolean?]{Determines whether some value is on the list (comparing values with equal?).}
  @defproc[((beginner-remove remove) [x any/c][l list?]) list?]{Constructs a list like the given one with
                                                                the first occurrence of the given item removed (comparing values with equal?).}
  @defproc[(reverse [l list?]) list]{Creates a reversed version of a list.}
  @defproc[(assq [x any/c][l list?]) (union false cons?)]{Determines whether some item is the first item of a pair in a list of pairs.})

 ("Posns"
  @defproc[(posn) signature]{Signature for posns.}
  @defproc[(make-posn [x any/c][y any/c]) posn]{Constructs a posn from two arbitrary values.}
  @defproc[(posn? [x any/c]) boolean?]{Determines if its input is a posn.}
  @defproc[(posn-x [p posn]) any]{Extracts the x component of a posn.}
  @defproc[(posn-y [p posn]) any]{Extracts the y component of a posn.})
 
 ("Characters"
  @defproc[(char? [x any/c]) boolean?]{Determines whether a value is a character.}
  @defproc[(char=? [c char][d char][e char] ...) boolean?]{Determines whether two characters are equal.}
  @defproc[(char<? [x char][d char][e char] ...) boolean?]{Determines whether a character precedes another.}
  @defproc[(char>? [c char][d char][e char] ...) boolean?]{Determines whether a character succeeds another.}
  @defproc[(char<=? [c char][d char][e char] ...) boolean?]{Determines whether a character precedes another  (or is equal to it).}
  @defproc[(char>=? [c char][d char][e char] ...) boolean?]{Determines whether a character succeeds another  (or is equal to it).}
  @defproc[(char-ci=? [c char][d char][e char] ...) boolean?]{Determines whether two characters are equal in a case-insensitive manner.}
  @defproc[(char-ci<? [c char][d char][e char] ...) boolean?]{Determines whether a character precedes another in a case-insensitive manner.}
  @defproc[(char-ci>? [c char][d char][e char] ...) boolean?]{Determines whether a character succeeds another in a case-insensitive manner.}
  @defproc[(char-ci<=? [c char][d char][e char] ...) boolean?]{Determines whether a character precedes another (or is equal to it) in a case-insensitive manner.}
  @defproc[(char-ci>=? [c char][d char][e char] ...) boolean?]{Determines whether a character succeeds another (or is equal to it) in a case-insensitive manner.}
  @defproc[(char-numeric? [c char]) boolean?]{Determines whether a character represents a digit.}
  @defproc[(char-alphabetic? [c char]) boolean?]{Determines whether a character represents an alphabetic character.}
  @defproc[(char-whitespace? [c char]) boolean?]{Determines whether a character represents space.}
  @defproc[(char-upper-case? [c char]) boolean?]{Determines whether a character is an upper-case character.}
  @defproc[(char-lower-case? [c char]) boolean?]{Determines whether a character is a lower-case character.}
  @defproc[(char-upcase [c char]) char]{Determines the equivalent upper-case character.}
  @defproc[(char-downcase [c char]) char]{Determines the equivalent lower-case character.}
  @defproc[(char->integer [c char]) integer]{Lookups the number that corresponds to the given character in the ASCII table (if any).})
 
 ("Strings"
  @defproc[(string? [x any/c]) boolean?]{Determines whether a value is a string.}
  @defproc[(string-length [s string]) nat]{Determines the length of a string.}
  @defproc[((beginner-string-ith string-ith) [s string][i natural-number]) string]{Extracts the ith 1-letter substring from the given one.}
  @defproc[((beginner-replicate replicate) [i natural-number][s string]) string]{Replicates the given string.}
  @defproc[((beginner-int->string int->string) [i integer]) string]{Converts an integer in [0,55295] or [57344 1114111] to a 1-letter string.}
  @defproc[((beginner-string->int string->int) [s string]) integer]{Converts a 1-letter string to an integer in [0,55295] or [57344, 1114111].}
  @defproc[((beginner-explode explode) [s string]) (listof string)]{Translates a string into a list of 1-letter strings.}
  @defproc[((beginner-implode implode) [l list?]) string]{Concatenates the list of 1-letter strings into one string.}
  @defproc[((beginner-string-numeric? string-numeric?) [s string]) boolean?]{Determines whether all 'letters' in the string are numeric.}
  @defproc[((beginner-string-alphabetic? string-alphabetic?) [s string]) boolean?]{Determines whether all 'letters' in the string are alphabetic.}
  @defproc[((beginner-string-whitespace? string-whitespace?) [s string]) boolean?]{Determines whether all 'letters' in the string are white space. }
  @defproc[((beginner-string-upper-case? string-upper-case?) [s string]) boolean?]{Determines whether all 'letters' in the string are upper case.}
  @defproc[((beginner-string-lower-case? string-lower-case?) [s string]) boolean?]{Determines whether all 'letters' in the string are lower case.}
  @defproc[((beginner-string-contains? string-contains?) [s string] [t string]) boolean?]{Determines whether the first string appears literally in the second one.}
  @defproc[(string [c char] ...) string?]{Builds a string of the given characters.}
  @defproc[(make-string [i natural-number][c char]) string]{Produces a string of given length from a single given character.}
  @defproc[(string-ref [s string][i natural-number]) char]{Extracts the i-the character from a string.}
  @defproc[(substring [s string][i natural-number][j natural-number]) string]{Extracts the substring starting at a 0-based index up to the second 0-based index (exclusive).}
  @defproc[(string-copy [s string]) string]{Copies a string.}
  @defproc[(string-append [s string] ...) string]{Juxtaposes the characters of several strings.}
  @defproc[(string=? [s string][t string][x string] ...) boolean?]{Compares two strings character-wise.}
  @defproc[(string<? [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically precedes another.}
  @defproc[(string>? [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically succeeds another.}
  @defproc[(string<=? [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically precedes another (or is equal to it).}
  @defproc[(string>=? [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically succeeds another (or is equal to it).}
  @defproc[(string-ci=?  [s string][t string][x string] ...) boolean?]{Compares two strings character-wise in a case-insensitive manner.}
  @defproc[(string-ci<?  [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically precedes another in a case-insensitive manner.}
  @defproc[(string-ci>?  [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically succeeds another in a case-insensitive manner.}
  @defproc[(string-ci<=? [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically precedes another (or is equal to it) in a case-insensitive manner.}
  @defproc[(string-ci>=? [s string][t string][x string] ...) boolean?]{Determines whether one string alphabetically succeeds another (or is equal to it) in a case-insensitive manner.}
  @defproc[(string->symbol [s string]) symbol]{Converts a string into a symbol.}
  @defproc[(string->number [s string]) (union number false)]{Converts a string into a number, produce false if impossible.}
  @defproc[(string->list [s string]) (listof char)]{Converts a string into a list of characters.}
  @defproc[(list->string [l list?]) string]{Converts a s list of characters into a string.}
  @defproc[(format [f string] [x any/c] ...) string]{Formats a string, possibly embedding values.})

 ("Images"
  @defproc[(image? [x any/c]) boolean?]{Determines whether a value is an image.}
  @defproc[(image=? [i image][j image]) boolean?]{Determines whether two images are equal.})
 
 ("Misc"
  @defproc[(identity [x any/c]) any]{Returns the argument unchanged.}
  @defproc[((beginner-error error) [x any/c] ...) void?]{Signals an error, combining the given values
                                                         into an error message. If any of the values' 
                                                         printed representations is too long, it is
                                                         truncated and ``...'' is put into the string.
                                                         If the first value is a symbol, it is treated
                                                         specially ; it is suffixed with a colon and a
                                                         space (the intention is that the symbol is the
                                                         name of the function signaling the error).}
  @defproc[((beginner-struct? struct?) [x any/c]) boolean?]{Determines whether some value is a structure.}
  @defproc[((beginner-equal? equal?) [x any/c][y any/c]) boolean?]{Determines whether two values are structurally equal where basic values are compared with the eqv? predicate.}
  @defproc[(eq? [x any/c][y any/c]) boolean?]{Determines whether two values are equivalent from the computer's perspective (intensional).}
  @defproc[(eqv? [x any/c][y any/c]) boolean?]{Determines whether two values are equivalent from the perspective of all functions that can be applied to it (extensional).}
  @defproc[((beginner-=~ =~) [x number][y number][z non-negative-real]) boolean?]{Checks whether two numbers are within some amount (the third argument) of either other.}
  @defproc[((beginner-equal~? equal~?) [x any/c][y any/c][z non-negative-real]) boolean?]{Compares like equal? on the first two arguments, except using =~ in the case of numbers.}
  @defproc[(eof) eof-object?]{The end-of-file value.}
  @defproc[(eof-object? [x any/c]) boolean?]{Determines whether some value is the end-of-file value.}
  @defproc[((beginner-exit exit)) void]{Exits the running program.}))
