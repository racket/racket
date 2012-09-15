#lang scribble/doc
@(require (for-label racket/flonum
                     racket/fixnum
                     racket/unsafe/ops
                     racket/require
                     math)
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          racket/sandbox
          racket/math
          "utils.rkt")

@(define untyped-eval (make-untyped-math-eval))

@title[#:tag "number-theory" #:style '(toc)]{Number Theory}
@(author-jens-axel)

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "congruences"]{Congruences and Modular Arithmetic}

@defproc[(divides? [m Integer] [n Integer]) boolean?]{
   Returns @racket[#t] if @racket[m] divides @racket[n],
   @racket[#f] otherwise.

   Note: That a non-zero integer @racket[m] divides an integer @racket[n]
   means there exists an integer @racket[k] such that m*k=n.

   Test whether 2 divides 9:
   @interaction[#:eval untyped-eval
                       (require math)
                       (divides? 2 9)]
   
   @; http://en.wikipedia.org/wiki/Divisor
}   

@defproc[(bezout [a Integer] [b Integer] [c Integer] ...) (Listof Integer)]{
  Given integers @racket[a], @racket[b], @racket[c] ... 
  returns a list of integers @racket[u], @racket[v], @racket[q] ... 
  such that @racket[gcd](@racket[a],@racket[b],@racket[c],...) 
  = @racket[au + bv + cw + ...]

  The greatest common divisor of 6 and 15 is 3.
  @interaction[#:eval untyped-eval
                      (bezout 6 15)
                      (+ (* -2 6) (* 1 15))]
  @; http://en.wikipedia.org/wiki/B%C3%A9zout's_identity
}
  
@defproc[(bezout-binary [a Integer] [b Integer]) (Listof Integer)]{
  Same as @racket[bezout] but restricted to two arguments.
}

@defproc[(coprime? [a Integer] [b Integer] ...) boolean?]{
  Returns @racket[#t] if the integers @racket[a],@racket[b],... are coprime.

  Note: A set of integers are considered coprime (also called relatively prime)
  if their greatest common divisor is 1.

  The numbers 2, 6, and, 15 are coprime.
  @interaction[#:eval untyped-eval
                      (coprime? 2 6 15)]
  @; http://en.wikipedia.org/wiki/Coprime
}  

 
@defproc[(pairwise-coprime? [a Integer] [b Integer] ...) boolean?]{
  Returns @racket[#t] if the integers @racket[a],@racket[b],... are pairwise coprime.

  The numbers 2, 6, and, 15 are not pairwise coprime, since 2 and 6 share the factor 3.
  @interaction[#:eval untyped-eval
                      (pairwise-coprime? 2 6 15)]
  @; http://en.wikipedia.org/wiki/Pairwise_coprime
}  

@defproc[(inverse [a Integer] [n Integer]) natural?]{
  Returns the inverse of @racket[a] module @racket[n],
  if @racket[a] and @racket[n] are coprime,
  otherwise @racket[#f] is returned.
  
  Note: If @racket[a] and @racket[n] are coprime, then
  the inverse, @racket[b], is a number in the set @racket[{0,...,n-1}]
  such that @racket[ab=1 mod n].
  
  The number 3 is an inverse to 2 modulo 5.
  @interaction[#:eval untyped-eval
                      (inverse 2 5)
                      (modulo (* 2 3) 5)]
  The number 0 has no inverse modulo 5.
  @interaction[#:eval untyped-eval
                      (inverse 0 5)]
  @; http://en.wikipedia.org/wiki/Modular_multiplicative_inverse
}  



@defproc[(solve-chinese [as (Listof Integer)] [bs (Listof Integer)]) natural?]{
  Given a list of integers @racket[as] and a list of coprime moduli @racket[ns]
  the function @racket[solve-chinese] will return
  the single natural solution @racket[x] in @racket[{0,...,n-1}]
  to the equations

  @racket[x=a1  mod n1,  ...,  x=ak  mod nk]

  where @racket[a1], ... are the elements of @racket[as],  
  and   @racket[n1], ... are the elements of @racket[ns],  
  and   @racket[n=n1*...*nk].

  
  What is the least number @racket[x] that when divided by 3 leaves 
  a remainder of 2, when divided by 5 leaves a remainder of 3, and 
  when divided by 7 leaves a remainder of 2? 
  @interaction[#:eval untyped-eval
                      (solve-chinese '(2 3 2) '(3 5 7))]
  
  @; http://en.wikipedia.org/wiki/Chinese_remainder_theorem
}


@; ----------------------------------------
@section[#:tag "primes"]{Primes}


@defproc[(prime? [z Integer]) boolean?]{
Returns @racket[#t] if @racket[z] is a prime,
@racket[#f] otherwise.

Note: An integer @racket[z] is considered a prime, if the only 
positive divisors of @racket[z] are @racket[1] and @racket[|z|].

The positive primes below 20 are:
  @interaction[#:eval untyped-eval
                      (require racket/list)
                      (filter prime? (range 1 21))]
The corresponding negative primes are:
  @interaction[#:eval untyped-eval
                      (filter prime? (range 1 -21 -1))]
}

@defproc[(odd-prime? [z Integer]) boolean?]{
Returns @racket[#t] if @racket[z] is a odd prime,
@racket[#f] otherwise.

@interaction[#:eval untyped-eval
                    (odd-prime? 2)
                    (odd-prime? 3)]
}

@defproc[(nth-prime [n Natural]) natural?]{
Returns the n'th positive prime. 
@interaction[#:eval untyped-eval
                    (nth-prime 0)
                    (nth-prime 1)
                    (nth-prime 2)]
}

@defproc[(next-prime [z Integer]) prime?]{
Returns the first prime larger than @racket[z].

@interaction[#:eval untyped-eval
                    (untyped-next-prime 4)
                    (untyped-next-prime 5)]

TODO: Figure out how to export next-prime even though
TR can't make contract automatically.
}

@defproc[(prev-prime [z Integer]) prime?]{
Returns the first prime smaller than @racket[z].

@interaction[#:eval untyped-eval
                    (untyped-prev-prime 4)
                    (untyped-prev-prime 5)]

TODO: Figure out how to export prev-prime even though
TR can't make contract automatically.
}

@defproc[(next-primes [z Integer] [n Natural]) (Listof prime?)]{
Returns list of the next @racket[n] primes larger than @racket[z].

@interaction[#:eval untyped-eval
                    (next-primes 2 4)]
}

@defproc[(prev-primes [z Integer] [n Natural]) (Listof prime?)]{
Returns list of the next @racket[n] primes smaller than @racket[z].

@interaction[#:eval untyped-eval
                    (prev-primes 13 4)]
}

@defproc[(factorize [n Natural]) (Listof (List prime? natural?))]{
Returns the factorization of a natural number @racket[n].
The factorization consists of a list of corresponding 
primes and exponents. The primes will be in ascending order.

The prime factorization of 600 = 2^3 * 3^1 * 5^2:
@interaction[#:eval untyped-eval
                    (factorize 600)]
}

@defproc[(defactorize [f (Listof (List prime? natural?))]) natural?]{
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

@defproc[(prime-divisors [z Integer]) (Listof Natural)]{
Returns a list of all positive prime divisors of the integer 
@racket[z]. The divisors appear in ascending order.                                                       
                                                       
 @interaction[#:eval untyped-eval
                     (prime-divisors 120)]
}

@defproc[(prime-exponents [z Integer]) (Listof Natural)]{
Returns a list of the exponents of in a factorization of the integer
@racket[z].                                                       
                                                       
 @interaction[#:eval untyped-eval
                     (define z (* 2 2 2 3 5 5))
                     (prime-divisors z)
                     (prime-exponents z)]
}

@; ----------------------------------------
@section[#:tag "roots"]{Roots}


@defproc[(integer-root [n Natural] [m Natural]) natural?]{
Returns the @racket[m]'th integer root of @racket[n].
This is the largest number @racket[r] such that 
@racket[r^m<=n].
            
 @interaction[#:eval untyped-eval
                     (integer-root (expt 3 4) 4)
                     (integer-root (+ (expt 3 4) 1) 4)]
}
 
@defproc[(integer-root/remainder [n Natural] [m Natural]) 
         (values natural? natural?)]{
Returns two values. The first, @racket[r], is the @racket[m]'th 
integer root of @racket[n]. The second is @racket[n-r^m].
            
 @interaction[#:eval untyped-eval
                     (integer-root/remainder (expt 3 4) 4)
                     (integer-root/remainder (+ (expt 3 4) 1) 4)]
}

 
@; ----------------------------------------
@section[#:tag "powers"]{Powers}

@defproc[(max-dividing-power [a Integer] [b Integer]) natural?]{
Returns the largest exponent, @racket[n], of a power with 
base @racket[a] that divides @racket[b].

That is, @racket[a^n] divides @racket[b] but @racket[a^(n+1)] does not divide
@racket[b].
            
 @interaction[#:eval untyped-eval
                     (max-dividing-power 3 (expt 3 4))
                     (max-dividing-power 3 5)]
}

@defproc[(perfect-power [m Integer]) 
         (Union (List natural? natural?) #f)]{
If @racket[m] is a perfect power, a list with two elements 
@racket[b] and @racket[n] such that @racket[b^n = m] 
is returned, otherwise @racket[#f] is returned.
            
 @interaction[#:eval untyped-eval
                     (perfect-power (expt 3 4))
                     (perfect-power (+ (expt 3 4) 1))]
}
 
 
@defproc[(perfect-power? [m Integer]) boolean?]{
Returns @racket[#t] if @racket[m] is a perfect power,
otherwise @racket[#f].        

  @interaction[#:eval untyped-eval
                      (perfect-power? (expt 3 4))
                      (perfect-power? (+ (expt 3 4) 1))]
}

 
@defproc[(prime-power [m Natural])
         (Union (List prime? natural?) #f)]{
If @racket[m] is a power of the form @racket[p^n]
where @racket[p] is prime, then a list with the
prime and the exponent is returned, otherwise
@racket[#f] is returned.

  @interaction[#:eval untyped-eval
                      (prime-power (expt 3 4))
                      (prime-power (expt 6 4))]
}

@defproc[(prime-power? [m Natural]) boolean?]{
Returns @racket[#t] if @racket[m] is a prime power,
otherwise @racket[#f].

  @interaction[#:eval untyped-eval
                      (prime-power? (expt 3 4))
                      (prime-power? (expt 6 4))
                      (prime-power? 1)
                      (prime-power? 0)]
}

@defproc[(odd-prime-power? [m Natural]) boolean?]{
Returns @racket[#t] if @racket[m] is a power of an odd prime,
otherwise @racket[#f].

  @interaction[#:eval untyped-eval
                      (odd-prime-power? (expt 2 4))
                      (odd-prime-power? (expt 3 4))
                      (odd-prime-power? (expt 15 4))]
}

@defproc[(as-power [m Positive-Integer]) 
         (values natural? natural?)]{
Returns two values @racket[b] and @racket[n]
such that @racket[m=b^r] and @racket[n] is maximal.
                                     
  @interaction[#:eval untyped-eval
                      (as-power (* (expt 2 4) (expt 3 4)))
                      (expt 6 4)
                      (* (expt 2 4) (expt 3 4))
                      (as-power (* (expt 2 4) (expt 3 5)))]
}

@defproc[(perfect-square [m Natural]) 
         (Union natural? #f)]{
Returns @racket[sqrt(m)] if @racket[m] is perfect 
square, otherwise @racket[#f].        

@interaction[#:eval untyped-eval
                    (perfect-square 9)
                    (perfect-square 10)]
}

@; ----------------------------------------
@section[#:tag "multiplicative"]{Multiplicative Functions}

In number theory a multiplicative function is a 
function @racket[f] such that @racket[f(a b) = f(a) f(b)]
for all coprime natural numbers @racket[a] and @racket[b].

The functions @racket[totient], @racket[moebius-mu], and, 
@racket[divisor-sum] are multiplicative.

@defproc[(totient [n Natural]) natural?]{
Returns the number of integers from 1 to @racket[n]
that are coprime with @racket[n].

This function is known as Eulers totient or phi function.

Note: The function @racket[totient] is multiplicative.

@interaction[#:eval untyped-eval
                    (totient 9)
                    (length (filter (curry coprime? 9) (range 10)))]

@; http://en.wikipedia.org/wiki/Euler%27s_totient_function
}


@defproc[(moebius-mu [n Natural]) (Union -1 0 1)]{
Returns:

@racket[1]  if @racket[n] is a product of an even number of primes

@racket[-1] if @racket[n] is a product of an odd number of primes

@racket[0]  if @racket[n] has a multiple prime factor
 
Note: The function @racket[moebius-mu] is multiplicative.

@interaction[#:eval untyped-eval
                    (moebius-mu (* 2 3 5))
                    (moebius-mu (* 2 3 5 7))
                    (moebius-mu (* 2 2 3 5 7))]

@; http://en.wikipedia.org/wiki/M%C3%B6bius_function
}


@defproc[(divisor-sum [n Natural] [k Natural]) natural?]{
Returns sum of the @racket[k]th powers of 
all divisors of @racket[n].

Note: The function @racket[divisor-sum] is multiplicative.

@interaction[#:eval untyped-eval         
                    (divisor-sum 12 2)
                    (apply + (map sqr (divisors 12)))]

@; http://en.wikipedia.org/wiki/Divisor_function
}


@; ----------------------------------------
@section[#:tag "number-sequences"]{Number Sequences}

@defproc[(bernoulli [n Natural]) exact-rational?]{
  Returns the @racket[n]th Bernoulli number.
  Definition:
  @racket[http://en.wikipedia.org/wiki/Bernoulli_number].

  @interaction[#:eval untyped-eval
                      (map bernoulli (range 9))]
}

@; ----------------------------------------
@section[#:tag "quadratic-residues"]{Quadratic Residues}


@(close-eval untyped-eval)