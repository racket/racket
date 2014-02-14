#lang scribble/manual

@begin[(require "../utils.rkt" (for-label typed/racket/base) (for-label (only-in rnrs/lists-6 fold-left)))]

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
(: sum (-> Number * Number))
(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))]

This type can be assigned to the function when each element
of the rest parameter is used at the same type.

@section{Non-Uniform Variable-Arity Functions}

However, the rest argument may be used as a heterogeneous list.
Take this (simplified) definition of the R6RS function @racket[fold-left]:

@racketmod[
racket
(define (fold-left f i as . bss)
  (if (or (null? as)
          (ormap null? bss))
      i
      (apply fold-left
             f
             (apply f i (car as) (map car bss))
             (cdr as)
             (map cdr bss))))

(fold-left + 0 (list 1 2 3 4) (list 5 6 7 8))
(fold-left + 0 (list 1 2 3) (list 2 3 4) (list 3 4 5) (list 4 5 6))
(fold-left (λ (i v n s) (string-append i (vector-ref v n) s))
           ""
           (list (vector "A cat" "A dog" "A mouse")
                 (vector "tuna" "steak" "cheese"))
           (list 0 2)
           (list " does not eat " "."))]

Here the different lists that make up the rest argument @racket[bss]
can be of different types, but the type of each list in @racket[bss]
corresponds to the type of the corresponding argument of @racket[f].
We also know that, in order to avoid arity errors, the length of
@racket[bss] must be two less than the arity of @racket[f].
The first argument to @racket[f] is the accumulator,
and @racket[as] corresponds to the second argument of @racket[f].

The example uses of @racket[fold-left] evaluate to @racketresult[36],
@racketresult[42], and @racketresult["A cat does not eat cheese."].

In Typed Racket, we can define @racket[fold-left] as follows:

@racketmod[
typed/racket
(: fold-left
   (All (C A B ...)
        (-> (-> C A B ... B C) C (Listof A) (Listof B) ... B
            C)))
(define (fold-left f i as . bss)
  (if (or (null? as)
          (ormap null? bss))
      i
      (apply fold-left
             f
             (apply f i (car as) (map car bss))
             (cdr as)
             (map cdr bss))))]

Note that the type variable @racket[B] is followed by an
ellipsis.  This denotes that B is a dotted type variable
which corresponds to a list of types, much as a rest
argument corresponds to a list of values.  When the type
of @racket[fold-left] is instantiated at a list of types, then
each type @racket[t] which is bound by @racket[B] (notated by
the dotted pre-type @racket[t ... B]) is expanded to a number
of copies of @racket[t] equal to the length of the sequence
assigned to @racket[B].  Then @racket[B] in each copy is
replaced with the corresponding type from the sequence.

So the type of @racket[(inst fold-left Integer Boolean String Number)]
is

@racket[(-> (-> Integer Boolean String Number Integer)
            Integer (Listof Boolean) (Listof String) (Listof Number)
            Integer)].
