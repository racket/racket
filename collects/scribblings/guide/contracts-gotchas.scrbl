#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          "contracts-utils.ss"
          (for-label scheme/contract))

@title[#:tag "contracts-gotchas"]{Gotchas}

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

This is a bug we hope to address in a future release.
@;{
@question{Contracts and @scheme[eq?]}

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

(define (make-adder ))
(provide make-adder)

(provide/contract [make-adder (-> number? (-> number? number?))])
]


}
