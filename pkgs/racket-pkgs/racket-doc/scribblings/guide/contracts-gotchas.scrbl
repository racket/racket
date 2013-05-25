#lang scribble/doc
@(require scribble/manual scribble/eval racket/sandbox
          "guide-utils.rkt" "contracts-utils.rkt"
          (for-label racket/contract))

@title[#:tag "contracts-gotchas"]{Gotchas}

@ctc-section{Contracts and @racket[eq?]}

As a general rule, adding a contract to a program should
either leave the behavior of the program unchanged, or
should signal a contract violation. And this is almost true
for Racket contracts, with one exception: @racket[eq?].

The @racket[eq?] procedure is designed to be fast and does
not provide much in the way of guarantees, except that if it
returns true, it means that the two values behave
identically in all respects. Internally, this is implemented
as pointer equality at a low-level so it exposes information
about how Racket is implemented (and how contracts are
implemented). 

Contracts interact poorly with @racket[eq?] because function
contract checking is implemented internally as wrapper
functions. For example, consider this module:
@racketmod[
racket

(define (make-adder x)
  (if (= 1 x)
      add1
      (lambda (y) (+ x y))))
(provide (contract-out 
          [make-adder (-> number? (-> number? number?))]))
]

It exports the @racket[make-adder] function that is the usual curried
addition function, except that it returns Racket's @racket[add1] when
its input is @racket[1].

You might expect that
@racketblock[
(eq? (make-adder 1)
     (make-adder 1))
]

would return @racket[#t], but it does not. If the contract were
changed to @racket[any/c] (or even @racket[(-> number? any/c)]), then
the @racket[eq?] call would return @racket[#t].

Moral: Do not use @racket[eq?] on values that have contracts.

@ctc-section[#:tag "exists-gotcha"]{Exists Contracts and Predicates}

Much like the @racket[eq?] example above, @racket[#:∃] contracts
can change the behavior of a program.

Specifically,
the @racket[null?] predicate (and many other predicates) return @racket[#f]
for @racket[#:∃] contracts, and changing one of those contracts to @racket[any/c]
means that @racket[null?] might now return @racket[#t] instead, resulting in
arbitrarily different behavior depending on how this boolean might flow around
in the program.

@defmodulelang[racket/exists]

To work around the above problem, the 
@racketmodname[racket/exists] library behaves just like @racketmodname[racket],
but predicates signal errors when given @racket[#:∃] contracts.

Moral: Do not use predicates on @racket[#:∃] contracts, but if you're not sure, use
@racketmodname[racket/exists] to be safe.

@ctc-section{Defining Recursive Contracts}

When defining a self-referential contract, it is natural to use
@racket[define]. For example, one might try to write a contract on
streams like this:

@(define e (make-base-eval))
@(interaction-eval #:eval e (require racket/contract))
@interaction[
  #:eval e
(define stream/c
  (promise/c
   (or/c null?
         (cons/c number? stream/c))))
]
@close-eval[e]

Unfortunately, this does not work because the value of
@racket[stream/c] is needed before it is defined. Put another way, all
of the combinators evaluate their arguments eagerly, even though the
values that they accept do not.

Instead, use
@racketblock[
(define stream/c
  (promise/c
   (or/c
    null?
    (cons/c number? (recursive-contract stream/c)))))
]

The use of @racket[recursive-contract] delays the evaluation of the
identifier @racket[stream/c] until after the contract is first
checked, long enough to ensure that @racket[stream/c] is defined.

See also @ctc-link["lazy-contracts"].

@ctc-section{Mixing @racket[set!] and @racket[contract-out]}

The contract library assumes that variables exported via
@racket[contract-out] are not assigned to, but does not enforce
it. Accordingly, if you try to @racket[set!] those variables, you 
may be surprised. Consider the following example:

@interaction[
(module server racket
  (define (inc-x!) (set! x (+ x 1)))
  (define x 0)
  (provide (contract-out [inc-x! (-> void?)]
                         [x integer?])))

(module client racket
  (require 'server)

  (define (print-latest) (printf "x is ~s\n" x))

  (print-latest)
  (inc-x!)
  (print-latest))

(require 'client)
]

Both calls to @racket[print-latest] print @racket[0], even though the
value of @racket[x] has been incremented (and the change is visible
inside the module @racket[x]).

To work around this, export accessor functions, rather than
exporting the variable directly, like this:

@racketmod[
racket

(define (get-x) x)
(define (inc-x!) (set! x (+ x 1)))
(define x 0)
(provide (contract-out [inc-x! (-> void?)]
                       [get-x (-> integer?)]))
]

Moral: This is a bug that we will address in a future release.

