#lang scribble/doc
@(require (for-label racket/math
                     racket/flonum
                     racket/fixnum
                     racket/unsafe/ops
                     racket/require)
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          racket/sandbox
          racket/math)

@(define math-eval 
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket )))
@;(interaction-eval #:eval math-eval (require math))

@title[#:tag "number-theory" #:style '(toc)]{Number Theory}

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "congruences"]{Congruences and Modular Arithmetic}

@defproc[(divides? [m Integer] [n Integer]) boolean?]{
   Returns @racket[#t] if @racket[m] divides @racket[n],
   @racket[#f] otherwise.

   Note: That a non-zero integer @racket[m] divides an integer @racket[n]
   means there exists an integer @racket[k] such that m*k=n.

   Test whether 2 divides 9:
   @interaction[(require math)
                (divides? 2 9)]
   
   @; http://en.wikipedia.org/wiki/Divisor
}   

@defproc[(bezout [a Integer] [b Integer] [c Integer] ...) (Listof Integer)]{
  Given integers @racket[a], @racket[b], @racket[c] ... 
  returns a list of integers @racket[u], @racket[v], @racket[q] ... 
  such that @racket[gcd](@racket[a],@racket[b],@racket[c],...) 
  = @racket[au + bv + cw + ...]

  The greatest common divisor of 6 and 15 is 3.
  @interaction[(require math)
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
  @interaction[(require math)
               (coprime? 2 6 15)]
  @; http://en.wikipedia.org/wiki/Coprime
}  

 
@defproc[(pairwise-coprime? [a Integer] [b Integer] ...) boolean?]{
  Returns @racket[#t] if the integers @racket[a],@racket[b],... are pairwise coprime.

  The numbers 2, 6, and, 15 are not pairwise coprime, since 2 and 6 share the factor 3.
  @interaction[(require math)
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
  @interaction[(require math)
               (inverse 2 5)
               (modulo (* 2 3) 5)]
  The number 0 has no inverse modulo 5.
  @interaction[(require math)
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
  @interaction[(require math)
               (solve-chinese '(2 3 2) '(3 5 7))]
  
  @; http://en.wikipedia.org/wiki/Chinese_remainder_theorem
}

