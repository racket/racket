
(module beginner-funs scheme
  (require mzlib/etc mzlib/list mzlib/math syntax/docprovide)

  ;; Implements the procedures:
  (require "teachprims.ss"
	   "../posn.ss"
	   "../imageeq.ss")

  ;; procedures with documentation:
  (provide-and-document
    procedures

    ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
      (number? (any -> boolean)
	"to determine whether some value is a number")
      (= (number number number ... -> boolean)
	 "to compare numbers for equality")
      (< (real real real ... -> boolean)
	 "to compare real numbers for less-than")
      (> (real real real ... -> boolean)
	 "to compare real numbers for greater-than")
      (<= (real real real ... -> boolean)
	  "to compare real numbers for less-than or equality")
      (>= (real real ... -> boolean)
	  "to compare real numbers for greater-than or equality")
    
      ((beginner-+ +) (number number number ... -> number)
       "to compute the sum of the input numbers")
      (- (number number ... -> number)
	 "to subtract the second (and following) number(s) from the first; negate the number if there is only one argument")
      ((beginner-* *) (number number number ... -> number)
       "to compute the product of all of the input numbers")
      ((beginner-/ /) (number number number ... -> number)
       "to divide the first by the second (and all following) number(s); try (/ 3 4) and (/ 3 2 2)"
       " only the first number can be zero.")
      (max (real real ... -> real)
	"to determine the largest number")
      (min (real real ... -> real)
	"to determine the smallest number")       
      (quotient (integer integer -> integer)
	"to divide the first integer (exact or inexact) into the second; try (quotient 3 4) and (quotient 4 3)")
      (remainder (integer integer -> integer)
	"to determine the remainder of dividing the first by the second integer (exact or inexact)")
      (modulo (integer integer -> integer)
	"to find the remainder of the division of the first number by the second; try (modulo 4 3) (modulo 4 -3)") 
      ((beginner-sqr sqr) (number -> number)
	"to compute the square of a number")
      (sqrt (number -> number)
	"to compute the square root of a number")
      (integer-sqrt (number -> integer)
	"to compute the integer (exact or inexact) square root of a number")      
      (expt (number number -> number)
	"to compute the power of the first to the second number")
      (abs (real -> real)
	"to compute the absolute value of a real number")
      (sgn (real -> (union 1 #i1.0 0 #i0.0 -1 #i-1.0))
	"to compute the sign of a real number")
    
      ;; fancy numeric 
      (exp (number -> number)
	"to compute e raised to a number")
      (log (number -> number)
	"to compute the base-e logarithm of a number")
    
      ;; trigonometry
      (sin (number -> number)
	"to compute the sine of a number (radians)")
      (cos (number -> number)
	"to compute the cosine of a number (radians)")
      (tan (number -> number)
	"to compute the tangent of a number (radians)")
      (asin (number -> number)
	"to compute the arcsine (inverse of sin) of a number")
      (acos (number -> number)
	"to compute the arccosine (inverse of cos) of a number")
      (atan (number -> number)
	"to compute the arctan (inverse of tan) of a number")    
    
      (sinh (number -> number)
	"to compute the hyperbolic sine of a number")
      (cosh (number -> number)
	"to compute the hyperbolic cosine of a number")
    
      (exact? (number -> boolean)
	"to determine whether some number is exact")
    
      (integer? (any -> boolean)
	"to determine whether some value is an integer (exact or inexact)")
    
      (zero? (number -> boolean)
	"to determine if some value is zero or not") 
      (positive? (number -> boolean)
	"to determine if some value is strictly larger than zero")
      (negative? (number -> boolean)
	"to determine if some value is strictly smaller than zero")      
      (odd? (integer -> boolean)
	"to determine if some integer (exact or inexact) is odd or not")
      (even? (integer -> boolean)
	"to determine if some integer (exact or inexact) is even or not")

      (add1 (number -> number)
	"to compute a number one larger than a given number")
      (sub1 (number -> number)
	"to compute a number one smaller than a given number")

      (lcm (integer integer ... -> integer)
	"to compute the least common multiple of two integers (exact or inexact)")
    
      (gcd (integer integer ... -> integer)
	"to compute the greatest common divisior of two integers (exact or inexact)")
    
      (rational? (any -> boolean)
	"to determine whether some value is a rational number")
    
      (numerator (rat -> integer)
	"to compute the numerator of a rational")
    
      (denominator (rat -> integer)
	"to compute the denominator of a rational")
    
      (inexact? (number -> boolean)
	"to determine whether some number is inexact")
    
      (real? (any -> boolean)
	"to determine whether some value is a real number")
    
      (floor (real -> integer)
	"to determine the closest integer (exact or inexact) below a real number")
    
      (ceiling (real -> integer)
	"to determine the closest integer (exact or inexact) above a real number")
    
      (round (real -> integer)
	"to round a real number to an integer (rounds to even to break ties)")
    
      (complex? (any -> boolean)
	"to determine whether some value is complex")
    
      (make-polar (real real -> number)
	"to create a complex from a magnitude and angle")

      (make-rectangular (real real -> number)
	"to create a complex from a real and an imaginary part")
    
      (real-part (number -> real)
	"to extract the real part from a complex number")
    
      (imag-part (number -> real)
	"to extract the imaginary part from a complex number")
    
      (magnitude (number -> real)
	"to determine the magnitude of a complex number")
    
      (angle (number -> real)
	"to extract the angle from a complex number")
    
      (conjugate (number -> number)
	"to compute the conjugate of a complex number")
    
      (exact->inexact (number -> number)
	"to convert an exact number to an inexact one")
    
      (inexact->exact (number -> number)
	"to approximate an inexact number by an exact one")
    
					;    "Odds and ends"
    
      (number->string (number -> string)
	"to convert a number to a string")
    
      (integer->char (integer -> char)
	"to lookup the character that corresponds to the given integer (exact only!) in the ASCII table (if any)")
    
      ((beginner-random random) (integer -> integer)
	"to generate a random natural number less than some given integer (exact only!)")
    
      (current-seconds (-> integer)
	"to compute the current time in seconds elapsed"
	" (since a platform-specific starting date)")
    
      (e real
	 "Euler's number")
      (pi real
	  "the ratio of a circle's circumference to its diameter"))
   
    ("Booleans" 
      (boolean? (any -> boolean)
	"to determine whether some value is a boolean")
    
      (boolean=? (boolean boolean -> boolean)
	"to determine whether two booleans are equal")

      (false? (any -> boolean)
	"to determine whether a value is false")
    
      ((beginner-not not) (boolean -> boolean)
	"to compute the negation of a boolean value"))
   
    ("Symbols"
      (symbol? (any -> boolean)
	"to determine whether some value is a symbol")
    
      (symbol=? (symbol symbol -> boolean)
	"to determine whether two symbols are equal")

      (symbol->string (symbol -> string)
	"to convert a symbol to a string")      )

    ("Lists"
      (cons? (any -> boolean)
	"to determine whether some value is a constructed list")
      (pair? (any -> boolean)
	"to determine whether some value is a constructed list")	
      (empty? (any -> boolean)
	"to determine whether some value is the empty list")
      (null? (any -> boolean)
	"to determine whether some value is the empty list")

      ((beginner-cons cons) (X (listof X) -> (listof X))
       "to construct a list")

      (null empty
	"the empty list")

      ((beginner-first first) ( (cons Y (listof X)) -> Y )
	"to select the first item of a non-empty list")
      ((beginner-car car) ( (cons Y (listof X)) -> Y )
	"to select the first item of a non-empty list")       
      ((beginner-rest rest) ((cons Y (listof X)) -> (listof X))
	"to select the rest of a non-empty list")
      ((beginner-cdr cdr) ((cons Y (listof X)) -> (listof X))
	"to select the rest of a non-empty list")
    
      (second ( (cons Z (cons Y (listof X))) -> Y )
	"to select the second item of a non-empty list")
      (cadr ( (cons Z (cons Y (listof X))) -> Y )
	"to select the second item of a non-empty list")	  
      (cdar ( (cons (cons Z (listof Y)) (listof X)) -> (listof Y) )
	"to select the rest of a non-empty list in a list") 
      (caar ( (cons (cons Z (listof Y)) (listof X)) -> Z )
	"to select the first item of the first list in a list")
      (cddr ( (cons Z (cons Y (listof X))) -> (listof X) )
	"to select the rest of the rest of a list")
      (third ( (cons W (cons Z (cons Y (listof X)))) -> Y )
	"to select the third item of a non-empty list")
      (caddr ( (cons W (cons Z (cons Y (listof X)))) -> Y )
	"to select the third item of a non-empty list")	  
      (caadr ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z) )
	"to select the rest of the first list in the first list of a list")
      (caaar ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> W )
	"to select the first item of the first list in the first list of a list")
      (cdaar ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z) )
	"to select the rest of the first list in the first list of a list")
      (cdadr ( (cons W (cons (cons Z (listof Y)) (listof X))) -> (listof Y) )
	"to select the rest of the first list in the rest of a list")
      (cadar ( (cons (cons W (cons Z (listof Y))) (listof X)) -> Z )
	"to select the second item of the first list of a list")
      (cddar ( (cons (cons W (cons Z (listof Y))) (listof X)) -> (listof Y) )
	"to select the rest of the rest of the first list of a list")
      (cdddr ( (cons W (cons Z (cons Y (listof X)))) -> (listof X) )
	"to select the rest of the rest of the rest of a list")
      (fourth ( (listof Y)  -> Y ) ; domain: (cons V (cons W (cons Z (cons Y (listof X)))))
	"to select the fourth item of a non-empty list")
      (cadddr ( (listof Y)  -> Y ) ;  domain: (cons V (cons W (cons Z (cons Y (listof X)))))
	"to select the fourth item of a non-empty list")
      (fifth ( (listof Y) -> Y ) ; domain: (cons U (cons V (cons W (cons Z (cons Y (listof X))))))
	"to select the fifth item of a non-empty list")
      (sixth ( (listof Y) -> Y ) ;  domain: (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X)))))))
	"to select the sixth item of a non-empty list")
      (seventh ( (listof Y) -> Y ) ;  domain: (cons S (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X))))))))
	"to select the seventh item of a non-empty list")
      (eighth ( (listof Y) -> Y ) ;  domain: (cons R (cons S (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X)))))))))
	"to select the eighth item of a non-empty list")    
    
      (list-ref ((listof X) natural-number -> X )
	"to extract the indexed item from the list")
    
      (list (any ... -> (listof any)) "to construct a list of its arguments")
      
      (make-list (natural-number any -> (listof any)) "(make-list k x) constructs a list of k copies of x")

      ((beginner-list* list*) (any ... (listof any) -> (listof any)) 
       "to construct a list by adding multiple items to a list")

      ((beginner-append append) ((listof any) (listof any) (listof any) ... -> (listof any))
       "to create a single list from several, by juxtaposition of the items")
      (length ((listof any) -> number)
	"to compute the number of items on a list")
      (memq (any (listof any) -> (union false list))
	"to determine whether some value is on some list"
	" if so, it produces the suffix of the list that starts with x"
	" if not, it produces false."
	" (It compares values with the eq? predicate.)")
      (memv (any (listof any) -> (union false list))
	"to determine whether some value is on the list"
	" if so, it produces the suffix of the list that starts with x"
	" if not, it produces false."
	" (it compares values with the eqv? predicate.)")
      ((beginner-member? member?) (any (listof any) -> boolean)
	"to determine whether some value is on the list"
	" (comparing values with equal?)")
      ((beginner-member member) (any (listof any) -> boolean)
	"to determine whether some value is on the list"
	" (comparing values with equal?)")
      ((beginner-remove remove) (any (listof any) -> (listof any))
	"to construct a list like the given one with the first occurrence of the given item removed"
	" (comparing values with equal?)")
      (reverse ((listof any) -> list)
	"to create a reversed version of a list")
      (assq (X (listof (cons X Y)) -> (union false (cons X Y)))
	"to determine whether some item is the first item of a pair"
	" in a list of pairs"))

    ("Posns"
      (posn signature "signature for posns")
      (make-posn (number number -> posn) "to construct a posn")
      (posn? (anything -> boolean) "to determine if its input is a posn")
      (posn-x (posn -> number) "to extract the x component of a posn")
      (posn-y (posn -> number) "to extract the y component of a posn"))

    ("Characters"
      (char? (any -> boolean)
	"to determine whether a value is a character")
      (char=? (char char char ... -> boolean)
	"to determine whether two characters are equal")
      (char<? (char char char ... -> boolean)
	"to determine whether a character precedes another")
      (char>? (char char char ... -> boolean)
	"to determine whether a character succeeds another")
      (char<=? (char char char ... -> boolean)
	"to determine whether a character precedes another"
	" (or is equal to it)")
      (char>=? (char char char ... -> boolean)
	"to determine whether a character succeeds another"
	" (or is equal to it)")
    
      (char-ci=? (char char char ... -> boolean)
	"to determine whether two characters are equal"
	" in a case-insensitive manner")
      (char-ci<? (char char char ... -> boolean)
	"to determine whether a character precedes another"
	" in a case-insensitive manner")
      (char-ci>? (char char char ... -> boolean)
	"to determine whether a character succeeds another"
	" in a case-insensitive manner")
      (char-ci<=? (char char char ... -> boolean)
	"to determine whether a character precedes another"
	" (or is equal to it) in a case-insensitive manner")
      (char-ci>=? (char char char ... -> boolean)
	"to determine whether a character succeeds another"
	" (or is equal to it) in a case-insensitive manner")
    
      (char-numeric? (char -> boolean)
	"to determine whether a character represents a digit")
      (char-alphabetic? (char -> boolean)
	"to determine whether a character represents"
	" an alphabetic character")
      (char-whitespace? (char -> boolean)
	"to determine whether a character represents space")
      (char-upper-case? (char -> boolean)
	"to determine whether a character is an"
	" upper-case character")
      (char-lower-case? (char -> boolean)
	"to determine whether a character is a"
	" lower-case character")
      (char-upcase (char -> char)
	"to determine the equivalent upper-case character")
      (char-downcase (char -> char)
	"to determine the equivalent lower-case character")
      (char->integer (char -> integer)
	"to lookup the number that corresponds to the"
	" given character in the ASCII table (if any)"))
    
    ("Strings"
      (string? (any -> boolean)
	"to determine whether a value is a string")
      (string-length (string -> nat)
	"to determine the length of a string")
      
      ((beginner-string-ith string-ith) (string nat -> string)
        "to extract the ith 1-letter substring from the given one")
      ((beginner-replicate replicate) (nat string -> string)
        "to replicate the given string")
      ((beginner-int->string int->string) (integer -> string)
        "to convert an integer in [0,55295] or [57344 1114111] to a 1-letter string")
      ((beginner-string->int string->int) (string -> integer)
        "to convert a 1-letter string to an integer in [0,55295] or [57344, 1114111]")
      ((beginner-explode explode) (string -> (listof string))
        "to translate a string into a list of 1-letter strings")
      ((beginner-implode implode) ((listof string) -> string)
        "to concatenate the list of 1-letter strings into one string")
      ((beginner-string-numeric? string-numeric?) (string -> boolean)
        "to determine whether all 'letters' in the string are numeric")
      ((beginner-string-alphabetic? string-alphabetic?) (string -> boolean)
        "to determine whether all 'letters' in the string are alphabetic")
      ((beginner-string-whitespace? string-whitespace?) (string -> boolean)
        "to determine whether all 'letters' in the string are white space") 
      ((beginner-string-upper-case? string-upper-case?) (string -> boolean)
        "to determine whether all 'letters' in the string are upper case")
      ((beginner-string-lower-case? string-lower-case?) (string -> boolean)
        "to determine whether all 'letters' in the string are lower case")
 
      (string (char ... -> string)
	"(string c1 c2 ...) builds a string")
      (make-string (nat char -> string)
	"to produce a string of given length"
	" from a single given character")
      (string-ref (string nat -> char)
	"to extract the i-the character from a string")
    
      (substring (string nat nat -> string)
	"to extract the substring starting at a 0-based index"
	" up to the second 0-based index (exclusive)")
      (string-copy (string -> string)
	"to copy a string")
      (string-append (string ... -> string)
	"to juxtapose the characters of several strings")
    
      (string=? (string string string ... -> boolean)
	"to compare two strings character-wise")
      (string<? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" precedes another")
      (string>? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" succeeds another")
      (string<=? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" precedes another (or is equal to it)")
      (string>=? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" succeeds another (or is equal to it)")
    
      (string-ci=? (string string string ... -> boolean)
	"to compare two strings character-wise"
	" in a case-insensitive manner")
      (string-ci<? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" precedes another in a case-insensitive manner")
      (string-ci>? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" succeeds another in a case-insensitive manner")
      (string-ci<=? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" precedes another (or is equal to it)"
	" in a case-insensitive manner")
      (string-ci>=? (string string string ... -> boolean)
	"to determine whether one string alphabetically"
	" succeeds another (or is equal to it)"
	" in a case-insensitive manner")
    
      (string->symbol (string -> symbol)
	"to convert a string into a symbol")
      (string->number (string -> (union number false))
	"to convert a string into a number,"
	" produce false if impossible")
      (string->list (string -> (listof char))
	"to convert a string into a list of characters")
      (list->string ((listof char) -> string)
	"to convert a s list of characters into a string")

      (format (string any ... -> string)
	"to format a string, possibly embedding values"))

    ("Images"
     (image? (any -> boolean)
       "to determine whether a value is an image")
     (image=? (image image -> boolean)
       "to determine whether two images are equal"))

    ("Misc"
      (identity (any -> any)
	"to return the argument unchanged")
      ((beginner-error error) (any ... -> void) "to signal an error, combining the given values into an error message.\n\nIf any of the values' printed representations is too long, it is truncated and ``...'' is put into the string. If the first value is a symbol, it is treated specially; it is suffixed with a colon and a space (the intention is that the symbol is the name of the function signalling the error).")
      ((beginner-struct? struct?) (any -> boolean)
       "to determine whether some value is a structure")
      ((beginner-equal? equal?) (any any -> boolean)
	"to determine whether two values are structurally equal"
	" where basic values are compared with the eqv? predicate")
      (eq? (any any -> boolean)
	"to determine whether two values are equivalent from the"
	"  computer's perspective (intensional)")
      (eqv? (any any -> boolean)
	"to determine whether two values are equivalent from the"
	"  perspective of all functions that can be applied to it (extensional)")
      ((beginner-=~ =~) (number number non-negative-real -> boolean)
	"to check whether two numbers are within some amount (the third argument) of either other")
      ((beginner-equal~? equal~?) (any any non-negative-real -> boolean)
	"to compare like equal? on the first two arguments, except using =~ in the case of numbers")
      (eof eof
	"the end-of-file value")
      (eof-object? (any -> boolean)
	"to determine whether some value is the end-of-file value")
      ((beginner-exit exit) ( -> void)
       "to exit the running program"))))
