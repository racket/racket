#lang scribble/doc
@(require scribble/manual 
          scribble/eval
	  scheme/sandbox
          "guide-utils.ss"
          "contracts-utils.ss"
          (for-label scheme/contract))

@title[#:tag "contracts-gotchas"]{Gotchas}

@ctc-section{Contracts and @scheme[eq?]}

As a general rule, adding a contract to a program should
either leave the behavior of the program unchanged, or
should signal a contract violation. And this is almost true
for PLT Scheme contracts, with one exception: @scheme[eq?].

The @scheme[eq?] procedure is designed to be fast and does
not provide much in the way of guarantees, except that if it
returns true, it means that the two values behave
identically in all respects. Internally, this is implemented
as pointer equality at a low-level so it exposes information
about how PLT Scheme is implemented (and how contracts are
implemented). 

Contracts interact poorly with @scheme[eq?] because function
contract checking is implemented internally as wrapper
functions. For example, consider this module:
@schememod[
scheme

(define (make-adder x)
  (if (= 1 x)
      add1
      (lambda (y) (+ x 1))))
(provide/contract [make-adder (-> number? (-> number? number?))])
]

It exports the @scheme[make-adder] function that is the usual curried
addition function, except that it returns Scheme's @scheme[add1] when
its input is @scheme[1].

You might expect that
@schemeblock[
(eq? (make-adder 1)
     (make-adder 1))
]

would return @scheme[#t], but it does not. If the contract were
changed to @scheme[any/c] (or even @scheme[(-> number? any/c)]), then
the @scheme[eq?] call would return @scheme[#t].

Moral: do not use @scheme[eq?] on values that have contracts.

@ctc-section[#:tag "exists-gotcha"]{Exists contracts and predicates}

Much like the @scheme[eq?] example above, @scheme[#:∃] contracts
can change the behavior of a program.

Specifically,
the @scheme[null?] predicate (and many other predicates) return @scheme[#f]
for @scheme[#:∃] contracts, and changing one of those contracts to @scheme[any/c]
means that @scheme[null?] might now return @scheme[#t] instead, resulting in
arbitrarily different behavior depending on this boolean might flow around
in the program.

@defmodulelang[scheme/exists]

To work around the above problem, the 
@schememodname[scheme/exists] library behaves just like the @schememodname[scheme],
but where predicates signal errors when given @scheme[#:∃] contracts.

Moral: do not use predicates on @scheme[#:∃] contracts, but if you're not sure, use
@schememodname[scheme/exists] to be safe.

@ctc-section{Defining recursive contracts}

When defining a self-referential contract, it is natural to use
@scheme[define]. For example, one might try to write a contract on
streams like this:

@(define e (make-base-eval))
@(interaction-eval #:eval e (require scheme/contract))
@interaction[
  #:eval e
(define stream/c
 (promise/c
  (or/c
   null?
   (cons/c number? stream/c))))
]
@close-eval[e]
 
Unfortunately, this does not work because the value of
@scheme[stream/c] is needed before it is defined. Put another way, all
of the combinators evaluate their arguments eagerly, even thought the
values that they accept do not.

Instead, use 
@schemeblock[
(define stream/c
 (promise/c
  (or/c
   null?
   (cons/c 1
           (recursive-contract stream/c)))))
]

The use of @scheme[recursive-contract] delays the evaluation of the
identifier @scheme[stream/c] until after the contract is first
checked, long enough to ensure that @scheme[stream/c] is defined.

See also @ctc-link["lazy-contracts"].

@ctc-section{Using @scheme[set!] to Assign to Variables Provided via @scheme[provide/contract]}

The contract library assumes that variables exported via
@scheme[provide/contract] are not assigned to, but does not enforce
it. Accordingly, if you try to @scheme[set!] those variables, you 
may be surprised. Consider the following example:

@interaction[
(module server scheme
  (define (inc-x!) (set! x (+ x 1)))
  (define x 0)
  (provide/contract [inc-x! (-> void?)]
                    [x integer?]))

(module client scheme
  (require 'server)

  (define (print-latest) (printf "x is ~s\n" x))

  (print-latest)
  (inc-x!)
  (print-latest))

(require 'client)
]

Both calls to @scheme[print-latest] print @scheme[0], even though the
value of @scheme[x] has been incremented (and the change is visible
inside the module @scheme[x]).

To work around this, export accessor functions, rather than
exporting the variable directly, like this:

@schememod[
scheme

(define (get-x) x)
(define (inc-x!) (set! x (+ x 1)))
(define x 0)
(provide/contract [inc-x! (-> void?)]
                  [get-x (-> integer?)])
]

Moral: This is a bug we hope to address in a future release.

