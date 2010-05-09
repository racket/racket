#lang scribble/manual

@begin[(require "utils.ss" (for-label typed/racket/base))]

@title[#:tag "varargs"]{Variable-Arity Functions: Programming with Rest Arguments}

Typed Racket can handle some uses of rest arguments.

@section{Uniform Variable-Arity Functions}

In Racket, one can write a function that takes an arbitrary
number of arguments as follows:

@racketmod[
racket
(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))

(sum)
(sum 1 2 3 4)
(sum 1 3)]

The arguments to the function that are in excess to the
non-rest arguments are converted to a list which is assigned
to the rest parameter.  So the examples above evaluate to
@racketresult[0], @racketresult[10], and @racketresult[4].

We can define such functions in Typed Racket as well:

@racketmod[
typed/racket
(: sum (Number * -> Number))
(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))]

This type can be assigned to the function when each element
of the rest parameter is used at the same type.

@section{Non-Uniform Variable-Arity Functions}

However, the rest argument may be used as a heterogeneous list.
Take this (simplified) definition of the Racket function @racket[map]:

@racketmod[
racket
(define (map f as . bss)
  (if (or (null? as)
          (ormap null? bss))
      null
      (cons (apply f (car as) (map car bss))
            (apply map f (cdr as) (map cdr bss)))))

(map add1 (list 1 2 3 4))
(map cons (list 1 2 3) (list (list 4) (list 5) (list 6)))
(map + (list 1 2 3) (list 2 3 4) (list 3 4 5) (list 4 5 6))]

Here the different lists that make up the rest argument @racket[bss]
can be of different types, but the type of each list in @racket[bss]
corresponds to the type of the corresponding argument of @racket[f].
We also know that, in order to avoid arity errors, the length of
@racket[bss] must be one less than the arity of @racket[f] (as
@racket[as] corresponds to the first argument of @racket[f]).
                                                            
The example uses of @racket[map] evaluate to @racketresult[(list 2 3 4 5)],
@racketresult[(list (list 1 4) (list 2 5) (list 3 6))], and
@racketresult[(list 10 14 18)].

In Typed Racket, we can define @racket[map] as follows:

@racketmod[
typed/racket
(: map 
   (All (C A B ...)
        ((A B ... B -> C) (Listof A) (Listof B) ... B
         ->
         (Listof C))))
(define (map f as . bss)
  (if (or (null? as)
          (ormap null? bss))
      null
      (cons (apply f (car as) (map car bss))
            (apply map f (cdr as) (map cdr bss)))))]

Note that the type variable @racket[B] is followed by an
ellipsis.  This denotes that B is a dotted type variable
which corresponds to a list of types, much as a rest
argument corresponds to a list of values.  When the type
of @racket[map] is instantiated at a list of types, then
each type @racket[t] which is bound by @racket[B] (notated by
the dotted pre-type @racket[t ... B]) is expanded to a number
of copies of @racket[t] equal to the length of the sequence
assigned to @racket[B].  Then @racket[B] in each copy is
replaced with the corresponding type from the sequence.

So the type of @racket[(inst map Integer Boolean String Number)]
is

@racket[((Boolean String Number -> Integer)
         (Listof Boolean) (Listof String) (Listof Number)
         ->
         (Listof Integer))].
