#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt" "contracts-utils.rkt"
          (for-label racket/contract))

@title[#:tag "contract-boundaries"]{Contracts and Boundaries}

Like a contract between two business partners, a software
contract is an agreement between two parties. The agreement
specifies obligations and guarantees for each ``product''
(or value) that is handed from one party to the other.

A contract thus establishes a boundary between the two parties. Whenever a
value crosses this boundary, the contract monitoring system performs contract
checks, making sure the partners abide by the established contract.

In this spirit, Racket encourages contracts mainly at module
boundaries. Specifically, programmers may attach contracts to
@racket[provide] clauses and thus impose constraints and promises on the use
of exported values. For example, the export specification 
@racketmod[
racket

(provide (contract-out [amount positive?]))

(define amount ...)
]

promises to all clients of the above module that the value of @racket[amount] will
always be a positive number. The contract system monitors
the module's obligation carefully. Every time a client
refers to @racket[amount], the monitor checks that the value
of @racket[amount] is indeed a positive number.

The contracts library is built into the Racket language, but
if you wish to use @racket[racket/base], you can explicitly
require the contracts library like this:

@racketmod[
racket/base
(require racket/contract) (code:comment "now we can write contracts")

(provide (contract-out [amount positive?]))

(define amount ...)
]

@ctc-section[#:tag "amount0"]{Contract Violations}

If we bind @racket[amount] to a number that is not positive,

@racketmod[
racket

(provide (contract-out [amount positive?]))

(define amount 0)]

then, when the module is required, the monitoring
system signals a violation of the contract and
blames the module for breaking its promises.

@; @ctc-section[#:tag "qamount"]{A Subtle Contract Violation}

An even bigger mistake would be to bind @racket[amount]
to a non-number value:

@racketmod[
racket

(provide (contract-out [amount positive?]))

(define amount 'amount)
]

In this case, the monitoring system will apply
@racket[positive?] to a symbol, but @racket[positive?]
reports an error, because its domain is only numbers. To
make the contract capture our intentions for all Racket
values, we can ensure that the value is both a number and is
positive, combining the two contracts with @racket[and/c]:

@racketblock[
(provide (contract-out [amount (and/c number? positive?)]))
]

@;{

==================================================

The section below discusses assigning to variables that are
provide/contract'd. This is currently buggy so this
discussion is elided. Here's the expansion of
the requiring module, just to give an idea:

(module m racket
  (require mzlib/contract)
  (provide/contract [x x-ctc]))

(module n racket (require m) (define (f) ... x ...))
==>
(module n racket
  (require (rename m x x-real))
  (define x (apply-contract x-real x-ctc ...))
  (define (f) ... x ...))

The intention is to only do the work of applying the
contract once (per variable reference to a
provide/contract'd variable). This is a significant
practical savings for the contract checker (this
optimization is motivated by my use of contracts while I was
implementing one of the software construction projects
(scrabble, I think ...))

Of course, this breaks assignment to the provided variable.

==================================================

<question title="Example" tag="example">

<table src="simple.rkt">
<tr><td bgcolor="e0e0fa">
<racket>
;; Language: Pretty Big
(module a racket
  (require mzlib/contract)

  (provide/contract
   [amount positive?])

  (provide
   ;; -> Void
   ;; effect: sets variable a
   do-it)
  
  (define amount 4)
  
  (define (do-it) <font color="red">(set! amount -4)</font>))

(module b racket 
  (require a)
  
  (printf "~s\n" amount)
  <font color="red">(do-it)</font>
  (printf "~s\n" amount))

(require b)
</racket>
<td bgcolor="beige" valign="top">
<pre>

the "server" module 
this allows us to write contracts 

export @racket[amount] with a contract 


export @racket[do-it] without contract 



set amount to 4, 
  which satisfies contract


the "client" module 
requires functionality from a

first reference to @racket[amount] (okay)
a call to @racket[do-it], 
second reference to @racket[amount] (fail)

</pre> </table>

<p><strong>Note:</strong> The above example is mostly self-explanatory. Take a
look at the lines in red, however. Even though the call to @racket[do-it]
sets @racket[amount] to -4, this action is <strong>not</strong> a contract
violation. The contract violation takes place only when the client module
(@racket[b]) refers to @racket[amount] again and the value flows across
the module boundary for a second time. 

</question>
}

@ctc-section{Experimenting with Contracts and Modules}

All of the contracts and modules in this chapter (excluding those just
following) are written using the standard @tt{#lang} syntax for
describing modules. Since modules serve as the boundary between
parties in a contract, examples involve multiple modules.

To experiment with multiple modules within a single module or within
DrRacket's @tech{definitions area}, use
Racket's submodules. For example, try the example earlier in
this section like this:

@racketmod[
racket

(module+ server
  (provide (contract-out [amount (and/c number? positive?)]))
  (define amount 150))
 
(module+ main
  (require (submod ".." server))
  (+ amount 10))
]

Each of the modules and their contracts are wrapped in parentheses
with the @racket[module+] keyword at the front. The first form after
@racket[module] is the name of the module to be used in a subsequent
@racket[require] statement (where each reference through a
@racket[require] prefixes the name with @racket[".."]).

@ctc-section[#:tag "intro-nested"]{Experimenting with Nested Contract Boundaries}

In many cases, it makes sense to attach contracts at module boundaries.
It is often convenient, however, to be able to use contracts at
a finer granularity than modules. The @racket[define/contract]
form enables this kind of use:

@racketmod[
racket

(define/contract amount
  (and/c number? positive?)
  150)

(+ amount 10)
]

In this example, the @racket[define/contract] form establishes a contract
boundary between the definition of @racket[amount] and its surrounding
context. In other words, the two parties here are the definition and
the module that contains it.

Forms that create these @emph{nested contract boundaries} can sometimes
be subtle to use because they may have unexpected performance implications
or blame a party that may seem unintuitive. These subtleties are explained
in @secref["simple-nested"] and @ctc-link["gotcha-nested"].
