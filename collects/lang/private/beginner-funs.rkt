(module beginner-funs scheme
  (require mzlib/etc mzlib/list mzlib/math syntax/docprovide)

  ;; Implements the procedures:
  (require "teachprims.rkt"
           "../posn.rkt"
           "../imageeq.rkt")

  ;; procedures with documentation:
  (provide-and-document
    procedures

    ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
      (number? (any -> boolean)
        "Determines whether some value is a number.")
      (=  (number number number ... -> boolean)
          "Compares numbers for equality.")
      (<  (real real real ... -> boolean)
          "Compares real numbers for less-than.")
      (>  (real real real ... -> boolean)
         "Compares real numbers for greater-than.")
      (<= (real real real ... -> boolean)
          "Compares real numbers for less-than or equality.")
      (>= (real real real ... -> boolean)
          "Compares real numbers for greater-than or equality.")
    
      ((beginner-+ +) (number number number ... -> number)
       "Evaluates the sum of the input numbers.")
      (- (number number ... -> number)
         "subtracts the second (and following) number(s) from the first; negate the number if there is only one argument.")
      ((beginner-* *) (number number number ... -> number)
       "Evaluates the product of all of the input numbers.")
      ((beginner-/ /) (number number number ... -> number)
       "Divides the first by the second (and all following) number(s); try (/ 3 4) and (/ 3 2 2)"
       " only the first number can be zero.")
      (max (real real ... -> real)
        "Determines the largest number.")
      (min (real real ... -> real)
        "Determines the smallest number.")       
      (quotient (integer integer -> integer)
        "Divides the first integer (exact or inexact) into the second; try (quotient 3 4) and (quotient 4 3).")
      (remainder (integer integer -> integer)
        "Determines the remainder of dividing the first by the second integer (exact or inexact).")
      (modulo (integer integer -> integer)
        "Finds the remainder of the division of the first number by the second; try (modulo 4 3) (modulo 4 -3).") 
      ((beginner-sqr sqr) (number -> number)
        "Evaluates the square of a number.")
      (sqrt (number -> number)
        "Evaluates the square root of a number.")
      (integer-sqrt (number -> integer)
        "Evaluates the integer (exact or inexact) square root of a number.")      
      (expt (number number -> number)
        "Evaluates the power of the first to the second number.")
      (abs (real -> real)
        "Evaluates the absolute value of a real number.")
      (sgn (real -> (union 1 #i1.0 0 #i0.0 -1 #i-1.0))
        "Evaluates the sign of a real number.")
    
      ;; fancy numeric 
      (exp (number -> number)
        "Evaluates e raised to a number.")
      (log (number -> number)
        "Evaluates the base-e logarithm of a number.")
    
      ;; trigonometry
      (sin (number -> number)
        "Evaluates the sine of a number (radians).")
      (cos (number -> number)
        "Evaluates the cosine of a number (radians).")
      (tan (number -> number)
        "Evaluates the tangent of a number (radians).")
      (asin (number -> number)
        "Evaluates the arcsine (inverse of sin) of a number.")
      (acos (number -> number)
        "Evaluates the arccosine (inverse of cos) of a number.")
      (atan (number [number] -> number)
        "Evaluates the arctan of the given number or the ratio of the two given numbers.")    
    
      (sinh (number -> number)
        "Evaluates the hyperbolic sine of a number.")
      (cosh (number -> number)
        "Evaluates the hyperbolic cosine of a number.")
    
      (exact? (number -> boolean)
        "Determines whether some number is exact.")
    
      (integer? (any -> boolean)
        "Determines whether some value is an integer (exact or inexact).")
    
      (zero? (number -> boolean)
        "Determines if some value is zero or not.") 
      (positive? (number -> boolean)
        "Determines if some value is strictly larger than zero.")
      (negative? (number -> boolean)
        "Determines if some value is strictly smaller than zero.")      
      (odd? (integer -> boolean)
        "Determines if some integer (exact or inexact) is odd or not.")
      (even? (integer -> boolean)
        "Determines if some integer (exact or inexact) is even or not.")

      (add1 (number -> number)
        "Evaluates a number one larger than a given number.")
      (sub1 (number -> number)
        "Evaluates a number one smaller than a given number.")

      (lcm (integer integer ... -> integer)
        "Evaluates the least common multiple of two integers (exact or inexact).")
    
      (gcd (integer integer ... -> integer)
        "Evaluates the greatest common divisior of two integers (exact or inexact).")
    
      (rational? (any -> boolean)
        "Determines whether some value is a rational number.")
    
      (numerator (rat -> integer)
        "Evaluates the numerator of a rational.")
    
      (denominator (rat -> integer)
        "Evaluates the denominator of a rational.")
    
      (inexact? (number -> boolean)
        "Determines whether some number is inexact.")
    
      (real? (any -> boolean)
        "Determines whether some value is a real number.")
    
      (floor (real -> integer)
        "Determines the closest integer (exact or inexact) below a real number.")
    
      (ceiling (real -> integer)
        "Determines the closest integer (exact or inexact) above a real number.")
    
      (round (real -> integer)
        "Rounds a real number to an integer (rounds to even to break ties).")
    
      (complex? (any -> boolean)
        "Determines whether some value is complex.")
    
      (make-polar (real real -> number)
        "Creates a complex from a magnitude and angle.")

      (make-rectangular (real real -> number)
        "Creates a complex from a real and an imaginary part.")
    
      (real-part (number -> real)
        "Extracts the real part from a complex number.")
    
      (imag-part (number -> real)
        "Extracts the imaginary part from a complex number.")
    
      (magnitude (number -> real)
        "Determines the magnitude of a complex number.")
    
      (angle (number -> real)
        "Extracts the angle from a complex number.")
    
      (conjugate (number -> number)
        "Evaluates the conjugate of a complex number.")
    
      (exact->inexact (number -> number)
        "Converts an exact number to an inexact one.")
    
      (inexact->exact (number -> number)
        "Approximates an inexact number by an exact one.")
    
                                        ;    "Odds and ends"
    
      (number->string (number -> string)
        "Converts a number to a string.")
    
      (integer->char (integer -> char)
        "Lookups the character that corresponds to the given integer (exact only!) in the ASCII table (if any).")
    
      ((beginner-random random) (integer -> integer)
        "Generates a random natural number less than some given integer (exact only!).")
    
      (current-seconds (-> integer)
        "Evaluates the current time in seconds elapsed"
        " (since a platform-specific starting date).")
    
      (e real
         "Euler's number.")
      (pi real
          "The ratio of a circle's circumference to its diameter."))
   
    ("Booleans" 
      (boolean? (any -> boolean)
        "Determines whether some value is a boolean.")
    
      (boolean=? (boolean boolean -> boolean)
        "Determines whether two booleans are equal.")

      (false? (any -> boolean)
        "Determines whether a value is false.")
    
      ((beginner-not not) (boolean -> boolean)
        "Evaluates the negation of a boolean value."))
   
    ("Symbols"
      (symbol? (any -> boolean)
        "Determines whether some value is a symbol.")
    
      (symbol=? (symbol symbol -> boolean)
        "Determines whether two symbols are equal.")

      (symbol->string (symbol -> string)
        "Converts a symbol to a string.")      )

    ("Lists"
      (cons? (any -> boolean)
        "Determines whether some value is a constructed list.")
      #;
      (pair? (any -> boolean)
        "Determines whether some value is a constructed list.")  
      (empty? (any -> boolean)
        "Determines whether some value is the empty list.")
      (null? (any -> boolean)
	"Determines whether some value is the empty list.")

      ((beginner-cons cons) (X (listof X) -> (listof X))
       "Constructs a list.")

      (null empty
	"The empty list.")

      ((beginner-first first) ( (cons Y (listof X)) -> Y )
	"Selects the first item of a non-empty list.")
      ((beginner-car car) ( (cons Y (listof X)) -> Y )
	"Selects the first item of a non-empty list.")       
      ((beginner-rest rest) ((cons Y (listof X)) -> (listof X))
	"Selects the rest of a non-empty list.")
      ((beginner-cdr cdr) ((cons Y (listof X)) -> (listof X))
	"Selects the rest of a non-empty list.")
    
      (second ( (cons Z (cons Y (listof X))) -> Y )
	"Selects the second item of a non-empty list.")
      (cadr ( (cons Z (cons Y (listof X))) -> Y )
	"Selects the second item of a non-empty list.")	  
      (cdar ( (cons (cons Z (listof Y)) (listof X)) -> (listof Y) )
	"Selects the rest of a non-empty list in a list.") 
      (caar ( (cons (cons Z (listof Y)) (listof X)) -> Z )
	"Selects the first item of the first list in a list.")
      (cddr ( (cons Z (cons Y (listof X))) -> (listof X) )
	"Selects the rest of the rest of a list.")
      (third ( (cons W (cons Z (cons Y (listof X)))) -> Y )
	"Selects the third item of a non-empty list.")
      (caddr ( (cons W (cons Z (cons Y (listof X)))) -> Y )
	"Selects the third item of a non-empty list.")	  
      (caadr ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z) )
	"Selects the rest of the first list in the first list of a list.")
      (caaar ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> W )
	"Selects the first item of the first list in the first list of a list.")
      (cdaar ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z) )
	"Selects the rest of the first list in the first list of a list.")
      (cdadr ( (cons W (cons (cons Z (listof Y)) (listof X))) -> (listof Y) )
	"Selects the rest of the first list in the rest of a list.")
      (cadar ( (cons (cons W (cons Z (listof Y))) (listof X)) -> Z )
	"Selects the second item of the first list of a list.")
      (cddar ( (cons (cons W (cons Z (listof Y))) (listof X)) -> (listof Y) )
	"Selects the rest of the rest of the first list of a list.")
      (cdddr ( (cons W (cons Z (cons Y (listof X)))) -> (listof X) )
	"Selects the rest of the rest of the rest of a list.")
      (fourth ( (listof Y)  -> Y ) ; domain: (cons V (cons W (cons Z (cons Y (listof X)))))
	"Selects the fourth item of a non-empty list.")
      (cadddr ( (listof Y)  -> Y ) ;  domain: (cons V (cons W (cons Z (cons Y (listof X)))))
	"Selects the fourth item of a non-empty list.")
      (fifth ( (listof Y) -> Y ) ; domain: (cons U (cons V (cons W (cons Z (cons Y (listof X))))))
	"Selects the fifth item of a non-empty list.")
      (sixth ( (listof Y) -> Y ) ;  domain: (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X)))))))
	"Selects the sixth item of a non-empty list.")
      (seventh ( (listof Y) -> Y ) ;  domain: (cons S (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X))))))))
	"Selects the seventh item of a non-empty list.")
      (eighth ( (listof Y) -> Y ) ;  domain: (cons R (cons S (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X)))))))))
	"Selects the eighth item of a non-empty list.")    
    
      (list-ref ((listof X) natural-number -> X )
	"Extracts the indexed item from the list.")
    
      (list (any ... -> (listof any)) "Constructs a list of its arguments.")
      
      (make-list (natural-number any -> (listof any)) 
        "Constructs a list of k (the first argument) copies of x (the second argument).")

      ((beginner-list* list*) (any ... (listof any) -> (listof any)) 
       "Constructs a list by adding multiple items to a list.")

      ((beginner-append append) ((listof any) (listof any) (listof any) ... -> (listof any))
       "Creates a single list from several, by juxtaposition of the items.")
      (length ((listof any) -> number)
	"Evaluates the number of items on a list.")
      (memq (any (listof any) -> (union false list))
	"Determines whether some value is on some list"
	" if so, it produces the suffix of the list that starts with x"
	" if not, it produces false."
	" (It compares values with the eq? predicate.)")
      (memv (any (listof any) -> (union false list))
	"Determines whether some value is on the list"
	" if so, it produces the suffix of the list that starts with x"
	" if not, it produces false."
	" (It compares values with the eqv? predicate.)")
      ((beginner-member? member?) (any (listof any) -> boolean)
	"Determines whether some value is on the list"
	" (comparing values with equal?).")
      ((beginner-member member) (any (listof any) -> boolean)
	"Determines whether some value is on the list"
	" (comparing values with equal?).")
      ((beginner-remove remove) (any (listof any) -> (listof any))
	"Constructs a list like the given one with the first occurrence of the given item removed"
	" (comparing values with equal?).")
      (reverse ((listof any) -> list)
	"Creates a reversed version of a list.")
      (assq (X (listof (cons X Y)) -> (union false (cons X Y)))
	"Determines whether some item is the first item of a pair"
	" in a list of pairs."))

    ("Posns"
      (posn signature "Signature for posns.")
      (make-posn (number number -> posn) "Constructs a posn.")
      (posn? (anything -> boolean) "Determines if its input is a posn.")
      (posn-x (posn -> number) "Extracts the x component of a posn.")
      (posn-y (posn -> number) "Extracts the y component of a posn."))

    ("Characters"
      (char? (any -> boolean)
	"Determines whether a value is a character.")
      (char=? (char char char ... -> boolean)
	"Determines whether two characters are equal.")
      (char<? (char char char ... -> boolean)
	"Determines whether a character precedes another.")
      (char>? (char char char ... -> boolean)
	"Determines whether a character succeeds another.")
      (char<=? (char char char ... -> boolean)
	"Determines whether a character precedes another"
	" (or is equal to it).")
      (char>=? (char char char ... -> boolean)
	"Determines whether a character succeeds another"
	" (or is equal to it).")
    
      (char-ci=? (char char char ... -> boolean)
	"Determines whether two characters are equal"
	" in a case-insensitive manner.")
      (char-ci<? (char char char ... -> boolean)
	"Determines whether a character precedes another"
	" in a case-insensitive manner.")
      (char-ci>? (char char char ... -> boolean)
	"Determines whether a character succeeds another"
	" in a case-insensitive manner.")
      (char-ci<=? (char char char ... -> boolean)
	"Determines whether a character precedes another"
	" (or is equal to it) in a case-insensitive manner.")
      (char-ci>=? (char char char ... -> boolean)
	"Determines whether a character succeeds another"
	" (or is equal to it) in a case-insensitive manner.")
    
      (char-numeric? (char -> boolean)
	"Determines whether a character represents a digit.")
      (char-alphabetic? (char -> boolean)
	"Determines whether a character represents"
	" an alphabetic character.")
      (char-whitespace? (char -> boolean)
	"Determines whether a character represents space.")
      (char-upper-case? (char -> boolean)
	"Determines whether a character is an"
	" upper-case character.")
      (char-lower-case? (char -> boolean)
	"Determines whether a character is a"
	" lower-case character.")
      (char-upcase (char -> char)
	"Determines the equivalent upper-case character.")
      (char-downcase (char -> char)
	"Determines the equivalent lower-case character.")
      (char->integer (char -> integer)
	"Lookups the number that corresponds to the"
	" given character in the ASCII table (if any)."))
    
    ("Strings"
      (string? (any -> boolean)
	"Determines whether a value is a string.")
      (string-length (string -> nat)
	"Determines the length of a string.")
      
      ((beginner-string-ith string-ith) (string nat -> string)
        "Extracts the ith 1-letter substring from the given one.")
      ((beginner-replicate replicate) (nat string -> string)
        "Replicates the given string.")
      ((beginner-int->string int->string) (integer -> string)
        "Converts an integer in [0,55295] or [57344 1114111] to a 1-letter string.")
      ((beginner-string->int string->int) (string -> integer)
        "Converts a 1-letter string to an integer in [0,55295] or [57344, 1114111].")
      ((beginner-explode explode) (string -> (listof string))
        "Translates a string into a list of 1-letter strings.")
      ((beginner-implode implode) ((listof string) -> string)
        "Concatenates the list of 1-letter strings into one string.")
      ((beginner-string-numeric? string-numeric?) (string -> boolean)
        "Determines whether all 'letters' in the string are numeric.")
      ((beginner-string-alphabetic? string-alphabetic?) (string -> boolean)
        "Determines whether all 'letters' in the string are alphabetic.")
      ((beginner-string-whitespace? string-whitespace?) (string -> boolean)
        "Determines whether all 'letters' in the string are white space.") 
      ((beginner-string-upper-case? string-upper-case?) (string -> boolean)
        "Determines whether all 'letters' in the string are upper case.")
      ((beginner-string-lower-case? string-lower-case?) (string -> boolean)
        "Determines whether all 'letters' in the string are lower case.")
 
      (string (char ... -> string)
	"Builds a string of the given characters.")
      (make-string (nat char -> string)
	"Produces a string of given length"
	" from a single given character.")
      (string-ref (string nat -> char)
	"Extracts the i-the character from a string.")
    
      (substring (string nat nat -> string)
	"Extracts the substring starting at a 0-based index"
	" up to the second 0-based index (exclusive).")
      (string-copy (string -> string)
	"Copies a string.")
      (string-append (string ... -> string)
	"Juxtaposes the characters of several strings.")
    
      (string=? (string string string ... -> boolean)
	"Compares two strings character-wise.")
      (string<? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" precedes another.")
      (string>? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" succeeds another.")
      (string<=? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" precedes another (or is equal to it).")
      (string>=? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" succeeds another (or is equal to it).")
    
      (string-ci=? (string string string ... -> boolean)
	"Compares two strings character-wise"
	" in a case-insensitive manner.")
      (string-ci<? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" precedes another in a case-insensitive manner.")
      (string-ci>? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" succeeds another in a case-insensitive manner.")
      (string-ci<=? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" precedes another (or is equal to it)"
	" in a case-insensitive manner.")
      (string-ci>=? (string string string ... -> boolean)
	"Determines whether one string alphabetically"
	" succeeds another (or is equal to it)"
	" in a case-insensitive manner.")
    
      (string->symbol (string -> symbol)
	"Converts a string into a symbol.")
      (string->number (string -> (union number false))
	"Converts a string into a number,"
	" produce false if impossible.")
      (string->list (string -> (listof char))
	"Converts a string into a list of characters.")
      (list->string ((listof char) -> string)
	"Converts a s list of characters into a string.")

      (format (string any ... -> string)
	"Formats a string, possibly embedding values."))

    ("Images"
     (image? (any -> boolean)
       "Determines whether a value is an image.")
     (image=? (image image -> boolean)
       "Determines whether two images are equal."))

    ("Misc"
      (identity (any -> any)
	"Returns the argument unchanged.")
      ((beginner-error error) (any ... -> void) "signals an error, combining the given values into an error message.\n\nIf any of the values' printed representations is too long, it is truncated and ``...'' is put into the string. If the first value is a symbol, it is treated specially; it is suffixed with a colon and a space (the intention is that the symbol is the name of the function signalling the error).")
      ((beginner-struct? struct?) (any -> boolean)
       "Determines whether some value is a structure.")
      ((beginner-equal? equal?) (any any -> boolean)
	"Determines whether two values are structurally equal"
	" where basic values are compared with the eqv? predicate.")
      (eq? (any any -> boolean)
	"Determines whether two values are equivalent from the"
	"  computer's perspective (intensional).")
      (eqv? (any any -> boolean)
	"Determines whether two values are equivalent from the"
	"  perspective of all functions that can be applied to it (extensional).")
      ((beginner-=~ =~) (number number non-negative-real -> boolean)
	"Checks whether two numbers are within some amount (the third argument) of either other.")
      ((beginner-equal~? equal~?) (any any non-negative-real -> boolean)
	"Compares like equal? on the first two arguments, except using =~ in the case of numbers.")
      (eof eof
	"The end-of-file value.")
      (eof-object? (any -> boolean)
	"Determines whether some value is the end-of-file value.")
      ((beginner-exit exit) ( -> void)
       "Exits the running program."))))
