#lang scribble/manual
@(require (for-label racket/flonum
                     racket/fixnum
                     racket/unsafe/ops
                     racket/require
                     racket/base
                     (only-in typed/racket/base
                              Integer Exact-Rational Boolean Listof Natural U List
                              Positive-Integer)
                     math)
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          racket/sandbox
          racket/math
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))
@interaction-eval[#:eval untyped-eval (require racket/list
                                               racket/function)]

@(define math-style tt)

@title[#:tag "number-theory"]{Number Theory}
@(author-jens-axel)

@defmodule[math/number-theory]

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "congruences"]{Congruences and Modular Arithmetic}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Divisor"]{Divisor}}
@defproc[(divides? [m Integer] [n Integer]) Boolean]{
   Returns @racket[#t] if @racket[m] divides @racket[n], @racket[#f] otherwise.
           
   Formally, an integer @racket[m] divides an integer @racket[n] when there
   exists a unique integer @racket[k] such that @racket[(* m k) = n].

   @examples[#:eval untyped-eval
                    (divides? 2 9)
                    (divides? 2 8)]
   
   Note that @racket[0] cannot divide anything:
   @interaction[#:eval untyped-eval
                       (divides? 0 5)
                       (divides? 0 0)]
   
   Practically, if @racket[(divides? m n)] is @racket[#t], then @racket[(/ n m)] will return
   an integer and will not raise @racket[exn:fail:contract:divide-by-zero].
   
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/B%C3%A9zout's_identity"]{Bezout's Identity}}
@defproc[(bezout [a Integer] [b Integer] [c Integer] ...) (Listof Integer)]{
  Given integers @racket[a b c ...] returns a list of integers @racket[(list u v w ...)]
  such that @racket[(gcd a b c ...) = (+ (* a u) (* b v) (* c w) ...)].

  @examples[#:eval untyped-eval
                   (bezout 6 15)
                   (+ (* -2 6) (* 1 15))
                   (gcd 6 15)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Coprime"]{Coprime}}
@defproc[(coprime? [a Integer] [b Integer] ...) Boolean]{
  Returns @racket[#t] if the integers @racket[a b ...] are coprime.
  Formally, a set of integers is considered coprime (also called relatively prime)
  if their greatest common divisor is 1.

  @examples[#:eval untyped-eval
                   (coprime? 2 6 15)]
}
 
@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Pairwise_coprime"]{Pairwise Coprime}}
@defproc[(pairwise-coprime? [a Integer] [b Integer] ...) Boolean]{
  Returns @racket[#t] if the integers @racket[a b ...] are @italic{pairwise} coprime, meaning
  that each adjacent pair of integers is coprime.

The numbers 2, 6 and 15 are coprime, but not @italic{pairwise} coprime, because 2 and 6 share the
factor 3:
@interaction[#:eval untyped-eval
                    (pairwise-coprime? 2 6 15)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Chinese_remainder_theorem"]{Chinese Remainder Theorem}}
@defproc[(solve-chinese [as (Listof Integer)] [ns (Listof Integer)]) Natural]{
  Given a length-@racket[k] list of integers @racket[as] and a length-@racket[k] list of coprime
  moduli @racket[ns], @racket[(solve-chinese as ns)] returns the least natural number @racket[x]
  that is a solution to the equations

  @racketblock[x = #,(elem @racket[a] @subscript{1}) #,(elem "(mod " @racket[n] @subscript{1} ")")
                ...
               x = #,(elem @racket[a] @subscript{@racket[k]}) #,(elem "(mod " @racket[x] @subscript{@racket[k]} ")")]

  The solution @racket[x] is less than
  @racket[(* #,(elem @racket[n] @subscript{1}) ... #,(elem @racket[n] @subscript{@racket[k]}))].
  
  The moduli @racket[ns] must all be positive.
  
  What is the least number @racket[x] that when divided by 3 leaves 
  a remainder of 2, when divided by 5 leaves a remainder of 3, and 
  when divided by 7 leaves a remainder of 2? 
  @interaction[#:eval untyped-eval
                      (solve-chinese '(2 3 2) '(3 5 7))]  
}



@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Quadratic_residue"]{Quadratic Residue}}
@defproc[(quadratic-residue? [a Integer] [n Integer]) Boolean]{
Returns @racket[#t] if @racket[a] is a quadratic residue modulo @racket[n], otherwise @racket[#f].
The modulus @racket[n] must be positive, and @racket[a] must be nonnegative.

Formally, @racket[a] is a quadratic residue modulo @racket[n] if there
exists a number @racket[x] such that @racket[(* x x) = a] (mod @racket[n]).
In other words, @racket[(quadratic-residue? a n)] is @racket[#t] when
@racket[a] is a perfect square modulo @racket[n].

@examples[#:eval untyped-eval
                 (quadratic-residue? 0 4)
                 (quadratic-residue? 1 4)
                 (quadratic-residue? 2 4)
                 (quadratic-residue? 3 4)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Legendre_symbol"]{Legendre Symbol}}
@defproc[(quadratic-character [a Integer] [p Integer]) (U -1 0 1)]{
Returns the value of the quadratic character modulo the prime @racket[p].                                                              
That is, for a non-zero @racket[a] the number @racket[1] is returned when 
@racket[a] is a quadratic residue,
and @racket[-1] is returned when @racket[a] is a non-residue. 
If @racket[a] is zero, then @racket[0] is returned.

If @racket[a] is negative or @racket[p] is not positive, @racket[quadratic-character] raises an error.
If @racket[p] is not prime, @racket[(quadratic-character a p)] is indeterminate.
        
This function is also known as the @emph{Legendre symbol}.

  @interaction[#:eval untyped-eval
                      (quadratic-character 0 5)
                      (quadratic-character 1 5)
                      (quadratic-character 2 5)
                      (quadratic-character 3 5)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Modular_multiplicative_inverse"]{Multiplicative Inverse}}
@defproc[(modular-inverse [a Integer] [n Integer]) Natural]{
  Returns the inverse of @racket[a] modulo @racket[n] if @racket[a] and @racket[n] are coprime,
  otherwise raises an error. The modulus @racket[n] must be positive, and @racket[a] must be nonzero.
  
  Formally, if @racket[a] and @racket[n] are coprime, @racket[b = (modular-inverse a n)] is the
  unique natural number less than @racket[n] such that @racket[(* a b) = 1] (mod @racket[n]).
  
  @interaction[#:eval untyped-eval
                      (modular-inverse 2 5)
                      (modulo (* 2 3) 5)]
}

@defproc[(modular-expt [a Integer] [b Integer] [n Integer]) Natural]{
  Computes @racket[(modulo (expt a b) n)], but much more efficiently. The modulus @racket[n] must
  be positive, and the exponent @racket[b] must be nonnegative.
  
  @examples[#:eval untyped-eval
                   (modulo (expt -6 523) 19)
                   (modular-expt -6 523 19)
                   (modular-expt 9 158235208 19)
                   (eval:alts (code:line (code:comment "don't try this at home!")
                                         (modulo (expt 9 158235208) 19))
                              (eval:result @racketresultfont{4}))
                   ]
}

@subsection[#:tag "modular"]{Parameterized Modular Arithmetic}
@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Modular_arithmetic"]{Modular Arithmetic}}

The @racketmodname[math/number-theory] library supports modular arithmetic parameterized on a current
modulus. For example, the code

@racketblock[(with-modulus n
               ((modexpt a b) . mod= . c))]

corresponds with the mathematical statement
@italic{a}@superscript{@italic{b}} = @italic{c} (mod @italic{n}).

The current modulus is stored in a parameter that, for performance reasons, can only be
set using @racket[with-modulus]. (The basic modular operators cache parameter reads, and this
restriction guarantees that the cached values are current.)

@defform[(with-modulus n body ...)
         #:contracts ([n Integer])]{
Alters the current modulus within the dynamic extent of @racket[body].
The expression @racket[n] must evaluate to a positive integer.

By default, the current modulus is @racket[1], meaning that every modular arithmetic expression
that does not raise an error returns @racket[0].
}

@defproc[(current-modulus) Positive-Integer]{
Returns the current modulus.
@examples[#:eval untyped-eval
                 (current-modulus)
                 (with-modulus 5 (current-modulus))]
}

@defproc[(mod [x Exact-Rational]) Natural]{
Converts a rational number @racket[x] to a natural number less than the current modulus.

If @racket[x] is an integer, this is equivalent to @racket[(modulo x n)].
If @racket[x] is a fraction, an integer input is generated by multiplying its numerator
by its denominator's modular inverse.

@examples[#:eval untyped-eval
                 (with-modulus 7 (mod (* 218 7)))
                 (with-modulus 7 (mod 3/2))
                 (with-modulus 7 (mod/ 3 2))
                 (with-modulus 7 (mod 3/7))]
}

@deftogether[(@defproc[(mod+ [a Integer] ...) Natural]
              @defproc[(mod* [a Integer] ...) Natural])]{
Equivalent to @racket[(modulo (+ a ...) (current-modulus))] and
@racket[(modulo (* a ...) (current-modulus))], respectively, but generate smaller intermediate
values.
}

@deftogether[(@defproc[(modsqr [a Integer]) Natural]
              @defproc[(modexpt [a Integer] [b Integer]) Natural])]{
Equivalent to @racket[(mod* a a)] and @racket[(modular-expt a b (current-modulus))], respectively.
}

@defproc[(mod- [a Integer] [b Integer] ...) Natural]{
Equivalent to @racket[(modulo (- a b ...) (current-modulus))], but generates smaller intermediate
values. Note that @racket[(mod- a) = (mod (- a))].
}

@defproc[(mod/ [a Integer] [b Integer] ...) Natural]{
Divides @racket[a] by @racket[(* b ...)], by multiplying @racket[a] by the multiplicative inverse
of @racket[(* b ...)]. The one-argument variant returns the modular inverse of @racket[a].

Note that @racket[(mod/ a b ...)] is @bold{not} equivalent to
@racket[(modulo (/ a b ...) (current-modulus))]; see @racket[mod=] for a demonstration.
}

@deftogether[(@defproc[(mod= [a Integer] [b Integer] ...) Boolean]
              @defproc[(mod< [a Integer] [b Integer] ...) Boolean]
              @defproc[(mod<= [a Integer] [b Integer] ...) Boolean]
              @defproc[(mod> [a Integer] [b Integer] ...) Boolean]
              @defproc[(mod>= [a Integer] [b Integer] ...) Boolean])]{
Each of these is equivalent to @racket[(op (mod a) (mod b) ...)], where @racket[op] is the
corresponding numeric comparison function. Additionally, when given one argument, the inequality
tests always return @racket[#t].

Suppose we wanted to know why 17/4 = 8 (mod 15), but 51/12 (mod 15) is undefined, even though
normally 51/12 = 17/4. In code,
@interaction[#:eval untyped-eval
                    (with-modulus 15 (mod/ 17 4))
                    (/ 51 12)
                    (with-modulus 15 (mod/ 51 12))]
We could try to divide by brute force: find, modulo 15, all the numbers @racket[a]
for which @racket[(mod* a 4)] is @racket[17], then find all the numbers @racket[b] for which
@racket[(mod* a 12)] is @racket[51].
@interaction[#:eval untyped-eval
                    (with-modulus 15
                      (for/list ([a  (in-range 15)]
                                 #:when (mod= (mod* a 4) 17))
                        a))
                    (with-modulus 15
                      (for/list ([b  (in-range 15)]
                                 #:when (mod= (mod* b 12) 51))
                        b))]
So the problem isn't that @racket[b] doesn't exist, it's that @racket[b] isn't @italic{unique}.
}

@; ----------------------------------------
@section[#:tag "primes"]{Primes}


@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Prime_number"]{Prime Number}}
@defproc[(prime? [z Integer]) Boolean]{
Returns @racket[#t] if @racket[z] is a prime, @racket[#f] otherwise.

Formally, an integer @racket[z] is prime when the only positive divisors of @racket[z]
are @racket[1] and @racket[(abs z)].

The positive primes below 20 are:
  @interaction[#:eval untyped-eval
                      (filter prime? (range 1 21))]
The corresponding negative primes are:
  @interaction[#:eval untyped-eval
                      (filter prime? (range 1 -21 -1))]
}

@defproc[(odd-prime? [z Integer]) Boolean]{
Returns @racket[#t] if @racket[z] is a odd prime,
@racket[#f] otherwise.

@interaction[#:eval untyped-eval
                    (odd-prime? 2)
                    (odd-prime? 3)]
}

@defproc[(nth-prime [n Integer]) Natural]{
Returns the @racket[n]th positive prime; @racket[n] must be nonnegative.
@interaction[#:eval untyped-eval
                    (nth-prime 0)
                    (nth-prime 1)
                    (nth-prime 2)]
}

@defproc[(random-prime [n Integer]) Natural]{
Returns a random prime smaller than @racket[n], which must be greater than @racket[2].
                                    
The function @racket[random-prime] picks random numbers
below @racket[n] until a prime is found.

@interaction[#:eval untyped-eval
                    (random-prime 10)
                    (random-prime 10)
                    (random-prime 10)]
}

@defproc[(next-prime [z Integer]) Integer]{
Returns the first prime larger than @racket[z].

@interaction[#:eval untyped-eval
                    (next-prime 4)
                    (next-prime 5)]
}

@defproc[(prev-prime [z Integer]) Integer]{
Returns the first prime smaller than @racket[z].

@interaction[#:eval untyped-eval
                    (prev-prime 4)
                    (prev-prime 5)]
}

@defproc[(next-primes [z Integer] [n Integer]) (Listof Integer)]{
Returns list of the next @racket[n] primes larger than @racket[z]; @racket[n] must be nonnegative.

@interaction[#:eval untyped-eval
                    (next-primes 2 4)]
}

@defproc[(prev-primes [z Integer] [n Integer]) (Listof Integer)]{
Returns list of the next @racket[n] primes smaller than @racket[z]; @racket[n] must be nonnegative.

@interaction[#:eval untyped-eval
                    (prev-primes 13 4)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Integer_factorization"]{Integer Factorization}}
@defproc[(factorize [n Natural]) (Listof (List Natural Natural))]{
Returns the factorization of a natural number @racket[n].
The factorization consists of a list of corresponding 
primes and exponents. The primes will be in ascending order.

The prime factorization of 600 = 2^3 * 3^1 * 5^2:
@interaction[#:eval untyped-eval
                    (factorize 600)]
}

@defproc[(defactorize [f (Listof (List Natural Natural))]) Natural]{
Returns the natural number, whose factorization is given 
by @racket[f]. The factorization @racket[f] is represented
as described in @racket[factorize].

@interaction[#:eval untyped-eval
                    (defactorize '((2 3) (3 1) (5 2)))]
}

@defproc[(divisors [z Integer]) (Listof Natural)]{
Returns a list of all positive divisors of the integer @racket[z].
The divisors appear in ascending order.                                                       
                                                       
 @interaction[#:eval untyped-eval
                     (divisors 120)
                     (divisors -120)]
}

@defproc[(prime-divisors [z Natural]) (Listof Natural)]{
Returns a list of all positive prime divisors of the integer 
@racket[z]. The divisors appear in ascending order.                                                       
                                                       
 @interaction[#:eval untyped-eval
                     (prime-divisors 120)]
}

@defproc[(prime-exponents [z Natural]) (Listof Natural)]{
Returns a list of the exponents of in a factorization of the integer
@racket[z].                                                       

 @interaction[#:eval untyped-eval
                     (define z (* 2 2 2 3 5 5))
                     (prime-divisors z)
                     (prime-exponents z)]
}

@; ----------------------------------------
@section[#:tag "roots"]{Roots}

@defproc[(integer-root [n Natural] [m Natural]) Natural]{
Returns the @racket[m]th integer root of @racket[n].
This is the largest integer @racket[r] such that 
@racket[(expt r m) <= n].

 @interaction[#:eval untyped-eval
                     (integer-root (expt 3 4) 4)
                     (integer-root (+ (expt 3 4) 1) 4)]
}

@defproc[(integer-root/remainder [n Natural] [m Natural]) 
         (values Natural Natural)]{
Returns two values. The first, @racket[r], is the @racket[m]th 
integer root of @racket[n]. The second is @racket[n-r^m].
            
 @interaction[#:eval untyped-eval
                     (integer-root/remainder (expt 3 4) 4)
                     (integer-root/remainder (+ (expt 3 4) 1) 4)]
}

 
@; ----------------------------------------
@section[#:tag "powers"]{Powers}

@defproc[(max-dividing-power [a Integer] [b Integer]) Natural]{
Returns the largest exponent, @racket[n], of a power with 
base @racket[a] that divides @racket[b].

That is, @racket[(expt a n)] divides @racket[b] but @racket[(expt a (+ n 1))] does not divide
@racket[b].

 @interaction[#:eval untyped-eval
                     (max-dividing-power 3 (expt 3 4))
                     (max-dividing-power 3 5)]
}

@defproc[(perfect-power [m Integer]) 
         (U (List Natural Natural) #f)]{
If @racket[m] is a perfect power, a list with two elements 
@racket[b] and @racket[n] such that @racket[(expt b n) = m] 
is returned, otherwise @racket[#f] is returned.

 @interaction[#:eval untyped-eval
                     (perfect-power (expt 3 4))
                     (perfect-power (+ (expt 3 4) 1))]
}

 
@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Perfect_power"]{Perfect Power}}
@defproc[(perfect-power? [m Integer]) Boolean]{
Returns @racket[#t] if @racket[m] is a perfect power,
otherwise @racket[#f].        

  @interaction[#:eval untyped-eval
                      (perfect-power? (expt 3 4))
                      (perfect-power? (+ (expt 3 4) 1))]
}


@defproc[(prime-power [m Natural])
         (U (List Natural Natural) #f)]{
If @racket[m] is a power of the form @racket[(expt p n)]
where @racket[p] is prime, then a list with the
prime and the exponent is returned, otherwise
@racket[#f] is returned.

  @interaction[#:eval untyped-eval
                      (prime-power (expt 3 4))
                      (prime-power (expt 6 4))]
}

@defproc[(prime-power? [m Natural]) Boolean]{
Returns @racket[#t] if @racket[m] is a prime power,
otherwise @racket[#f].

  @interaction[#:eval untyped-eval
                      (prime-power? (expt 3 4))
                      (prime-power? (expt 6 4))
                      (prime-power? 1)
                      (prime-power? 0)]
}

@defproc[(odd-prime-power? [m Natural]) Boolean]{
Returns @racket[#t] if @racket[m] is a power of an odd prime,
otherwise @racket[#f].

  @interaction[#:eval untyped-eval
                      (odd-prime-power? (expt 2 4))
                      (odd-prime-power? (expt 3 4))
                      (odd-prime-power? (expt 15 4))]
}

@defproc[(as-power [m Positive-Integer]) 
         (values Natural Natural)]{
Returns two values @racket[b] and @racket[n]
such that @racket[m = (expt b n)] and @racket[n] is maximal.

  @interaction[#:eval untyped-eval
                      (as-power (* (expt 2 4) (expt 3 4)))
                      (expt 6 4)
                      (* (expt 2 4) (expt 3 4))
                      (as-power (* (expt 2 4) (expt 3 5)))]
}

@defproc[(perfect-square [m Natural]) 
         (U Natural #f)]{
Returns @racket[(sqrt m)] if @racket[m] is perfect 
square, otherwise @racket[#f].

@interaction[#:eval untyped-eval
                    (perfect-square 9)
                    (perfect-square 10)]
}

@; ----------------------------------------
@section[#:tag "multiplicative"]{Multiplicative Functions}

The functions in this section are @deftech{multiplicative}.
In number theory, a multiplicative function is a function @racket[f] such that
@racket[(f a b) = (* (f a) (f b))] for all coprime natural numbers @racket[a] and @racket[b].

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Euler%27s_totient_function"]{Euler's Totient}}
@defproc[(totient [n Natural]) Natural]{
Returns the number of integers from 1 to @racket[n]
that are coprime with @racket[n].

This function is known as Eulers totient or phi function.

@interaction[#:eval untyped-eval
                    (totient 9)
                    (length (filter (curry coprime? 9) (range 10)))]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/M%C3%B6bius_function"]{Moebius Function}}
@defproc[(moebius-mu [n Natural]) (U -1 0 1)]{
Returns:
@itemlist[@item{@racket[1]  if @racket[n] is a product of an even number of primes}
          @item{@racket[-1] if @racket[n] is a product of an odd number of primes}
          @item{@racket[0]  if @racket[n] has a multiple prime factor}]

@interaction[#:eval untyped-eval
                    (moebius-mu (* 2 3 5))
                    (moebius-mu (* 2 3 5 7))
                    (moebius-mu (* 2 2 3 5 7))]
}


@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Divisor_function"]{Divisor Function}}
@defproc[(divisor-sum [n Natural] [k Natural]) Natural]{
Returns sum of the @racket[k]th powers of 
all divisors of @racket[n].

@interaction[#:eval untyped-eval         
                    (divisor-sum 12 2)
                    (apply + (map sqr (divisors 12)))]
}

@margin-note{OEIS: @hyperlink["http://oeis.org/A001222"]{Big Omega}}
@defproc[(prime-omega [n Natural]) natural?]{
Counting multiplicities the number of prime factors of @racket[n] is returned.

Note: The function @racket[prime-omega] is multiplicative.

@interaction[#:eval untyped-eval         
                    (prime-omega (* 2 2 2 3 3 5))]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Von_Mangoldt_function"]{Von Mangoldt Function}}
@defproc[(mangoldt-lambda [n Natural]) Real]{
The von Mangoldt function. 
If @racket[n=p^k] for a prime @racket[p] and an integer @racket[k>=1] then @racket[(log n)] is returned.                                             
Otherwise 0 is returned.

@interaction[#:eval untyped-eval         
                    (mangoldt-lambda (* 3 3))
                    (log 3)]
}



@; ----------------------------------------
@section[#:tag "number-sequences"]{Number Sequences}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Bernoulli_number"]{Bernoulli Number}}
@defproc[(bernoulli-number [n Integer]) Exact-Rational]{
  Returns the @racket[n]th Bernoulli number; @racket[n] must be nonnegative.

  @interaction[#:eval untyped-eval
                      (map bernoulli-number (range 9))]
  
  Note that these are the @italic{first} Bernoulli numbers, since @racket[(bernoulli-number 1) = -1/2].
}

@margin-note{MathWorld: @hyperlink["http://mathworld.wolfram.com/EulerianNumber.html"]{Eulerian Number}}
@defproc[(eulerian-number [n Integer] [k Integer]) Natural]{
  Returns the Eulerian number @math-style{<n,k>}; both arguments must be nonnegative.

  @interaction[#:eval untyped-eval
                      (eulerian-number 5 2)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Fibonacci_number"]{Fibonacci Number}}
@defproc[(fibonacci [n Integer]) Natural]{
  Returns the @racket[n]th Fibonacci number; @racket[n] must be nonnegative.

  The ten first Fibonacci numbers.
  @interaction[#:eval untyped-eval
                      (map fibonacci (range 10))]
}

@defproc[(make-fibonacci [a Integer] [b Integer]) (Integer -> Integer)]{
Returns a function representing @italic{a} Fibonacci sequence with the first two numbers
@racket[a] and @racket[b]. The @racket[fibonacci] function is defined as
@racket[(make-fibonacci 0 1)].

@margin-note{Wikipedia: @hyperlink["http://wikipedia.org/wiki/Lucas_number"]{Lucas Number}}
The Lucas numbers are defined as a Fibonacci sequence starting with 2 and 1:
@interaction[#:eval untyped-eval
                    (map (make-fibonacci 2 1) (range 10))]
}

@defproc[(modular-fibonacci [n Integer] [m Integer]) Natural]{
  Returns the @racket[n]th Fibonacci number modulo @racket[m]; @racket[n] must be nonnegative
  and @racket[m] must be positive.

  The ten first Fibonacci numbers modulo 5.
  @interaction[#:eval untyped-eval
                      (map (λ (n) (modular-fibonacci n 5)) (range 10))]
}

@defproc[(make-modular-fibonacci [a Integer] [b Integer]) (Integer Integer -> Integer)]{
Like @racket[make-fibonacci], but makes a modular Fibonacci sequence.
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Farey_sequence"]{Farey Sequence}}
@defproc[(farey-sequence [n Integer]) (Listof Exact-Rational)]{
Returns a list of the numbers in the @racket[n]th Farey sequence; @racket[n] must be positive.

The @racket[n]th Farey sequence is the sequence of all 
completely reduced rational numbers from 0 to 1 which denominators
are less than or equal to @racket[n].
  @interaction[#:eval untyped-eval
                      (farey-sequence 1)
                      (farey-sequence 2)
                      (farey-sequence 3)]
}

@margin-note{MathWorld: @hyperlink["http://mathworld.wolfram.com/TangentNumber.html"]{Tangent Number}}
@defproc[(tangent-number [n Integer]) Integer]{
Returns the @racket[n]th tangent number; @racket[n] must be nonnegative.
            
  @interaction[#:eval untyped-eval
                      (tangent-number 1)
                      (tangent-number 2)
                      (tangent-number 3)]
}


@; ----------------------------------------
@section[#:tag "combinatorics"]{Combinatorics}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Factorial"]{Factorial}}
@defproc[(factorial [n Integer]) Natural]{
  Returns the factorial of @racket[n], which must be nonnegative.
  The factorial of @racket[n] is the number @racket[(* n (- n 1) (- n 2) ... 1)].
  @interaction[#:eval untyped-eval
                      (factorial 3)
                      (factorial 0)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Binomial_coefficient"]{Binomial Coefficient}}
@defproc[(binomial [n Integer] [k Integer]) Natural]{
  Returns the number of ways to choose a @italic{set} of @racket[k] items from a set of
  @racket[n] items; i.e. the order of the @racket[k] items is not significant.
  Both arguments must be nonnegative.
  
  When @racket[k > n], @racket[(binomial n k) = 0]. Otherwise, @racket[(binomial n k)] is
  equivalent to @racket[(/ (factorial n) (factorial k) (factorial (- n k)))], but computed more
  quickly.
  @interaction[#:eval untyped-eval
                      (binomial 5 3)]
}


@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Permutation#Permutations_in_combinatorics"]{
Permutations}}
@defproc[(permutations [n Integer] [k Integer]) Natural]{
  Returns the number of ways to choose a @italic{sequence} of @racket[k] items from a set of
  @racket[n] items; i.e. the order of the @racket[k] items is significant.
  Both arguments must be nonnegative.
  
  When @racket[k > n], @racket[(permutations n k) = 0]. Otherwise, @racket[(permutations n k)] is
  equivalent to @racket[(/ (factorial n) (factorial (- n k)))].
  @interaction[#:eval untyped-eval
                      (permutations 5 3)]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Multinomial_theorem#Multinomial_coefficients"]{Multinomial Coeffecient}}
@defproc[(multinomial [n Integer] [ks (Listof Integer)]) Natural]{
  A generalization of @racket[binomial] to multiple sets of choices; e.g.
  @racket[(multinomial n (list k0 k1 k2))] is the number of ways to choose a set of @racket[k0] items,
  a set of @racket[k1] items, and a set of @racket[k2] items from a set of @racket[n] items.
  All arguments must be nonnegative.
  
  When @racket[(apply + ks) = n], this is equivalent to
  @racket[(apply / (factorial n) (map factorial ks))]. Otherwise, @racket[multinomial] returns @racket[0].
  @interaction[#:eval untyped-eval
                      (multinomial 5 '(3 2))
                      (= (multinomial 8 '(5 3))
                         (binomial 8 5)
                         (binomial 8 3))
                      (multinomial 10 '(5 3 2))
                      (multinomial 0 '())
                      (multinomial 4 '(1 1))]
}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Partition_(number_theory)"]{Partition}}
@defproc[(partitions [n Integer]) Natural]{
  Returns the number of partitions of @racket[n], which must be nonnegative.
  A partition of a positive integer @racket[n] is a way 
  of writing @racket[n] as a sum of positive integers.
  The number 3 has the partitions @racket[(+ 1 1 1)], @racket[(+ 1 2)] and @racket[(+ 3)].
  @interaction[#:eval untyped-eval
                      (partitions 3)
                      (partitions 4)]
}


@; ----------------------------------------
@section[#:tag "special_numbers"]{Special Numbers}

@subsection{Polygonal Numbers}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Polygonal_number"]{Polygonal Number}}
@deftogether[
(@defproc[(triangle-number? [n Natural]) Boolean]
  @defproc[(square-number? [n Natural]) Boolean]
  @defproc[(pentagonal-number? [n Natural]) Boolean]
  @defproc[(hexagonal-number? [n Natural]) Boolean]
  @defproc[(heptagonal-number? [n Natural]) Boolean]
  @defproc[(octagonal-number? [n Natural]) Boolean])]{
These functions check whether the input is a polygonal number of the types
triangle, square, pentagonal, hexagonal, heptagonal and octogonal 
respectively.
}

@deftogether[
(@defproc[(triangle-number [n Natural]) Natural]
@defproc[(sqr [n Natural]) Natural]
@defproc[(pentagonal-number [n Natural]) Natural]
@defproc[(hexagonal-number [n Natural]) Natural]
@defproc[(heptagonal-number [n Natural]) Natural]
@defproc[(octagonal-number [n Natural]) Natural])]{
These functions return the @racket[n]th polygonal number 
of the corresponding type of polygonal number.
}


@; ----------------------------------------

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Mediant_(mathematics)"]{Mediant}}
@section[#:tag "fractions"]{Fractions}
@defproc[(mediant [x Exact-Rational] [y Exact-Rational]) Exact-Rational]{
Computes the @racket[mediant] of the numbers @racket[x] and @racket[y].
The mediant of two fractions @math-style{p/q} and @math-style{r/s} in their
lowest term is the number @math-style{(p+r)/(q+s)}.
  @interaction[#:eval untyped-eval
                      (mediant 1/2 5/6)]
}



@; ----------------------------------------
@section[#:tag "quadratics"]{The Quadratic Equation}

@defproc[(quadratic-solutions [a Real] [b Real] [c Real]) (Listof Real)]{
Returns a list of all real solutions to the equation @math-style{a x^2 + b x +c = 0}.
  @interaction[#:eval untyped-eval
                      (quadratic-solutions 1 0 -1)
                      (quadratic-solutions 1 2 1)
                      (quadratic-solutions 1 0 1)]  
}

@defproc[(quadratic-integer-solutions [a Real] [b Real] [c Real]) (Listof Integer)]{
Returns a list of all integer solutions to the equation @math-style{a x^2 + b x +c = 0}.
  @interaction[#:eval untyped-eval
                      (quadratic-integer-solutions 1 0 -1)
                      (quadratic-integer-solutions 1 0 -2)]  
}

@defproc[(quadratic-natural-solutions [a Real] [b Real] [c Real]) (Listof Natural)]{
Returns a list of all natural solutions to the equation @math-style{a x^2 + b x +c = 0}.
  @interaction[#:eval untyped-eval
                      (quadratic-natural-solutions 1 0 -1)
                      (quadratic-natural-solutions 1 0 -2)]  
}



@; ----------------------------------------
@section[#:tag "primitive_roots"]{The group Zn and Primitive Roots}

@margin-note{Wikipedia: @hyperlink["http://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n"]{
The Group Zn}}
The numbers @math-style{0, 1, ..., n-1} with addition and multiplication
modulo @racket[n] is a ring called @math-style{Zn}. 

The group of units in @math-style{Zn} with respect to multiplication 
modulo @racket[n] is called @math-style{Un}.

The order of an element @math-style{x} in @math-style{Un} 
is the least @math-style{k>0} such that @math-style{x^k=1 mod n}.

A generator the group @math-style{Un} is called a @emph{primitive root} modolo @racket[n].
Note that @math-style{g} is a primitive root if and only if @math-style{order(g)=phi(n)},
where @math-style{phi} is Eulers totient. A group with a generator is called @emph{cyclic}.


@defproc[(unit-group [n Integer]) (Listof Positive-Integer)]{
Returns a list of all elements of @math-style{Un}, the unit group modulo @racket[n]. The
modulus @racket[n] must be positive.
  @interaction[#:eval untyped-eval
                      (unit-group 5)
                      (unit-group 6)]  
}

@defproc[(unit-group-order [x Integer] [n Integer]) Positive-Integer]{
Returns the order of @racket[x] in the group @math-style{Un}; both arguments must be positive.
If @racket[x] and @racket[n] are not coprime, @racket[(unit-group-order x n)] raises an error.
  @interaction[#:eval untyped-eval
                      (unit-group-order 2 5)
                      (unit-group-order 2 6)]  
}

@defproc[(unit-group-orders [n Integer]) (Listf Positive-Integer)]{
Returns a list @racket[(list (unit-group-order x0 n) (unit-group-order x1 n) ...)] where
@racket[x0], @racket[x1], ... are the elements of @math-style{Un}.
The modulus @racket[n] must be positive.
  @interaction[#:eval untyped-eval
                      (unit-group-orders 5)
                      (map (curryr unit-group-order 5) (unit-group 5))]
}

@defproc[(primitive-root? [x Integer] [n Integer]) Boolean]{
Returns @racket[#t] if the element @racket[x] in @math-style{Un} is a primitive root modulo @racket[n],
otherwise @racket[#f] is returned. An error is signaled if @racket[x] is not a member of @math-style{Un}.
Both arguments must be positive.
  @interaction[#:eval untyped-eval
                      (primitive-root? 1 5)
                      (primitive-root? 2 5)
                      (primitive-root? 5 5)]
}

@defproc[(exists-primitive-root? [n Integer]) Boolean]{
Returns @racket[#t] if the group @math-style{Un} has a primitive root (i.e. it is cyclic),
otherwise @racket[#f] is returned.
In other words, @racket[#t] is returned if @racket[n] is one of 
@math-style{1, 2, 4, p^e, 2*p^e} where @math-style{p} is an odd prime,
and @racket[#f] otherwise.
The modulus @racket[n] must be positive.
  @interaction[#:eval untyped-eval
                      (exists-primitive-root? 5)
                      (exists-primitive-root? 6)
                      (exists-primitive-root? 12)]  
}

@defproc[(primitive-root [n Integer]) (Union Natural #f)]{
Returns a primitive root of @math-style{Un} if one exists,
otherwise @racket[#f] is returned. The modulus @racket[n] must be positive.
  @interaction[#:eval untyped-eval
                      (primitive-root 5)
                      (primitive-root 6)]
}

@defproc[(primitive-roots [n Integer]) (Listof Natural)]{
Returns a list of all primitive roots of @math-style{Un}. The modulus @racket[n] must be positive.
  @interaction[#:eval untyped-eval
                      (primitive-roots 3)
                      (primitive-roots 5)
                      (primitive-roots 6)]
}


@(close-eval untyped-eval)

