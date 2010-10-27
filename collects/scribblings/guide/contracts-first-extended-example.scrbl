#lang scribble/doc

@(require scribble/manual
	  scribble/core
          scribble/eval
          "guide-utils.ss"
          "contracts-utils.ss"
	  (only-in racket/list argmax)
          (for-label racket/contract))

@;(require "shared.rkt" (only-in racket/list argmax))

@title[#:tag "contracts-first"]{Contracts: A Thorough Example}

This section develops several different flavors of contracts for one and
 the same example: Racket's @racket[argmax] function. According to
 its Racket documentation, the function  consumes a procedure @racket[proc] and
 a non-empty list of values, @racket[lst]. It  
@nested[#:style 'inset]{
 returns the @emph{first} element in the list @racket[lst] that maximizes
 the result of @racket[proc].}  
 The emphasis on @emph{first} is ours. 

Examples: 
@interaction[#:eval ((make-eval-factory (list 'racket)))
(argmax add1 (list 1 2 3)) 
(argmax sqrt (list .4 .9 .16))
(argmax second '((a 2) (b 3) (c 4) (d 1) (e 4)))
]

Here is the simplest possible contract for this function: 
@racketmod[#:file @tt{version 1} 
racket

(define (argmax f lov) ...)

(provide/contract 
  [argmax (-> (-> any/c real?) (and/c pair? list?) anyc/)])
]
 This contract captures two essential conditions of the informal
 description of @scheme[argmax]: 
@itemlist[

@item{the given function must produce numbers that are comparable according
to @scheme[<]. In particular, the contract @scheme[(-> any/c number?)]
would not do, because @scheme[number?] also recognizes complex numbers in
Racket.}

@item{the given list must contain at least one item.}
]
 When combined with the name, the contract explains the behavior of 
 @scheme[argmax] at the same level as an ML function type in a
 module signature (except for the non-empty list aspect). 

Contracts may communicate significantly more than a type signature,
 however. Take a look at this second contract for @scheme[argmax]:
@racketmod[#:file @tt{version 2}
racket

(define (argmax f lov) ...)

(provide/contract
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) () 
         (_ (lambda (r)
	      (define f@r (f r))
	      (for/and ((v lov)) (>= f@r (f v))))))])
]
 It is a @emph{dependent} contract that names the two arguments and uses
 the names to impose a predicate on the result. This predicate computes
 @scheme[(f r)] -- where @racket[r] is the result of @scheme[argmax] -- and
 then validates that this value is greater than or equal to all values
 of @racket[f] on the items of @racket[lov].

Is it possible that @scheme[argmax] could cheat by returning a random value
 that accidentally maximizes @scheme[f] over all elements of @scheme[lov]? 
 With a contract, it is possible to rule out this possibility: 
@racketmod[#:file @tt{version 2 rev. a}
racket

(define (argmax f lov) ...)

(provide/contract
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) () 
         (_ (lambda (r)
	      (define f@r (f r))
              (and
		(memq r lov)
		(for/and ((v lov)) (>= f@r (f v)))))))])
]
 The @scheme[memq] function ensures that @scheme[r] is @emph{intensionally equal}
 @margin-note*{That is, "pointer equality" for those who prefer to think at
 the hardware level.} to one of the members of @scheme[lov]. Of course, a
 moment's worth of reflection shows that it is impossible to make up such a
 value. Functions are opaque values in Racket and without applying a
 function, it is impossible to determine whether some random input value
 produces an output value or triggers some exception. So we ignore this
 possibility from here on. 

Version 2 formulates the overall sentiment of @scheme[argmax]'s
 documentation, but it fails to bring across that the result is the
 @emph{first} element of the given list that maximizes the given function
 @racket[f]. Here is a version that communicates this second aspect of
 the informal documentation: 
@racketmod[#:file @tt{version 3} 
racket

(define (argmax f lov) ...)

(provide/contract
  [argmax
    (->i ([f (-> any/c real?)] [a (and/c cons? list?)]) ()
         (_ (lambda (r)
	      (define f@r (f r))
	      (and (for/and ((v lov)) (>= f@r (f v)))		   
		   (eq? (first (memf (lambda (v) (= (f v) f@r)) lov)) 
		        r)))))])
]
 That is, the @scheme[memf] function determines the first element of
 @racket[lov] whose value under @racket[f] is equal to @racket[r]'s value
 under @racket[f]. If this element is intensionally equal to @racket[r],
 the result of @scheme[argmax] is correct.  

This second refinement step introduces two problems. First, both conditions
 recompute the values of @racket[f] for all elements of @racket[lov]. Second,
 the contract is now quite difficult to read. Contracts should have a concise
 formulation that a client can comprehend with a simple scan. Let us
 eliminate the readability problem with two auxiliary functions that have
 reasonably meaningful names: 

@(define dominates1
  @multiarg-element['tt]{@list{
   @racket[f@r] is greater or equal to all @scheme[(f v)] for @racket[v] in @racket[lov]}})

@(define first?1
  @multiarg-element['tt]{
   @list{@racket[r] is @scheme[eq?] to the first element @racket[v] of @racket[lov] 
         for which @racket[(pred? v)]}})

@; ---------------------------------------------------------------------------------------------------
@racketmod[#:file @tt{version 3 rev. a} 
racket

(define (argmax f lov) ...)

(provide/contract
  [argmax
    (->i ([f (-> any/c real?)] [a (and/c cons? list?)]) ()
         (_ (lambda (r)
	      (define f@r (f r))
	      (and (is-first-max? r f@r f lov)
		   (dominates-all f@r f lov)))))])

@code:comment{where}

@code:comment{@#,dominates1}
(define (dominates-all f@r f lov)
  (for/and ((v lov)) (>= (f v) f@r)))

@code:comment{@#,first?1}
(define (is-first-max? r f@r f lov)
  (eq? (first (memf (lambda (v) (= (f v) f@r)) lov)) r))
]
 The names of the two predicates express their functionality and, in
 principle, render it unnecessary to read their definitions. 

This step leaves us with the problem of the newly introduced inefficiency.
 To avoid the recomputation of @racket[(f v)] for all @racket[v] on
 @racket[lov], we change the contract so that it computes these values and
 reuses them as needed:


@(define dominates2
  @multiarg-element['tt]{@list{
   @racket[f@r] is greater or equal to all @racket[f@v] in @racket[flov]}})

@(define first?2
  @multiarg-element['tt]{
   @list{@racket[r] is @racket[(second x)] for the first
         @racket[x] in @racket[flov+lov] s.t. @scheme[(= (first x) f@r)]}})

@racketmod[#:file @tt{version 3 rev. b} 
racket

(define (argmax f lov) ...)

(provide/contract
  [argmax
    (->i ([f (-> any/c real?)] [a (and/c cons? list?)]) ()
         (_ (lambda (r)
	      (define f@r (f r))
	      (define flov (map f lov))
	      (and (is-first-max? r f@r lov flov)
		   (dominates-all f@r flov)))))])

@code:comment{where}

@code:comment{@#,dominates2}
(define (dominates-all f@r lov)
  (for/and ((f@v flov)) (>= f@r f@v)))

@code:comment{@#,first?2}
(define (is-first-max? r f@r lov+flov)
  (define fst (first flov))
  (if (= (first fst) f@r)
      (eq? (second fst) r)
      (is-first-max? f@r r (rest lov+flov))))
]
 Now the predicate on the result once again computes all values of @racket[f]
 for elements of @racket[lov] once. 

@margin-note{The word "eager" comes from the literature on the linguistics
 of contracts.}

Version 3 may still be too eager when it comes to calling @racket[f]. While
 Racket's @scheme[argmax] always calls @racket[f] no matter how many items
 @racket[lov] contains, let us imagine for illustrative purposes that our
 own implementation first checks whether the list is a singleton.  If so,
 the first element would be the only element of @racket[lov] and in that
 case there would be no need to compute @racket[(f r)].  
@margin-note*{The @scheme[argmax] of Racket implicitly argues that it not
 only promises the first value that maximizes @scheme[f] over @scheme[lov]
 but also that @scheme[f] produces/produced a value for the result.}
 As a matter of fact, since @racket[f] may diverge or raise an exception
 for some inputs, @scheme[argmax] should avoid calling @racket[f] when
 possible.

The following contract demonstrates how a higher-order dependent contract
 needs to be adjusted so as to avoid being over-eager: 

@racketmod[#:file @tt{version 4} 
racket

(define (argmax f lov) 
  (if (empty? (rest lov))
      (first lov)
      ...))

(provide/contract
  [argmax
    (->i ([f (-> any/c real?)] [lov (and/c pair? list?)]) ()
         (_ (lambda (r)
	      (cond
		[(empty? (rest a)) (eq? (first a) r)]
		[else
		  (define f@r (f r))
		  (define flov (map f lov))
		  (and (is-first-max? r f@r lov flov)
		       (dominates-all f@r flov))]))))])

@code:comment{where}

@code:comment{@#,dominates2}
(define (dominates-all f@r lov) ...)

@code:comment{@#,first?2}
(define (is-first-max? r f@r lov+flov) ...)
]
 Note that such considerations don't apply to the world of first-order
 contracts. Only a higher-order (or lazy) language forces the programmer to
 express contracts with such precision.
 
The problem of diverging or exception-raising functions should alert the
 reader to the even more general problem of functions with side-effects. If
 the given function @racket[f] has visible effects -- say it logs its calls
 to a file -- then the clients of @scheme[argmax] will be able to observe
 two sets of logs for each call to @scheme[argmax]. To be precise, if the
 list of values contains more than one element, the log will contain two
 calls of @racket[f] per value on @racket[lov]. If @racket[f] is expensive
 to compute, doubling the calls imposes a high cost.

To avoid this cost and to signal problems with overly eager contracts, a
 contract system could record the i/o of contracted function arguments and
 use these hashtables in the dependency specification. This is a topic of
 on-going research in PLT. Stay tuned. 


@;{one could randomly check some element here, instead of all of them and
thus ensure 'correctness' at 1/(length a) probability}
