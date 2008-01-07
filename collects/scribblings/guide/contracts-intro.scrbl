#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]
@require["contracts-utils.ss"]
@(require (for-label scheme/contract))

@title[#:tag "contract-boundaries"]{Contracts and Boundaries}

Like a contract between two business partners, a software
contract is an agreement between two parties. The agreement
specifies obligations and guarantees for each party.

A contract thus establishes a boundary between the two parties. Whenever a
value crosses this boundary, the contract monitoring system performs contract
checks, making sure the partners abide by the established contract.

In this spirit, PLT Scheme supports contracts only at module
boundaries. Specifically, programmers may attach contracts to
@scheme[provide] clauses and thus impose constraints and promises on the use
of exported values. For example, the export specification 
@schememod[
scheme

(provide/contract 
  [amount positive?])
(define amount ...)
]

promises to all clients of @scheme[a] that amount will always be a
positive number. The contract system monitors @scheme[a]'s obligation
carefully. Every time a client refers to @scheme[amount], the monitor checks
that the value of @scheme[amount] is indeed a positive number.

The contracts library is built into the Scheme language, but
if you wish to use @scheme[scheme/base], you can explicitly
require the contracts library like this:

@schememod[
scheme/base
(require scheme/contract) (code:comment "now we can write contracts")

(provide/contract 
  [amount positive?])
(define amount ...)
]

@question[#:tag "amount0"]{What happens if @scheme[a] sets @scheme[amount] to 0?}

Suppose the creator of @scheme[a] had written 
@schememod[
scheme

(provide/contract 
  [amount positive?])
  
(define amount 0)]

When module @scheme[a] is required, the monitoring
system will signal a violation of the contract and
blame @scheme[a] for breaking its promises.

@question[#:tag "qamount"]{What happens if @scheme[a] sets @scheme[amount] to @scheme['amount]?}

Suppose the creator of @scheme[a] had written 
@schememod[
scheme

(provide/contract 
  [amount positive?])
  
(define amount 'amount)
]

In that case, @scheme[positive?] will report an error, since
its domain is only numbers. To make the contract capture our
intentions for all Scheme values, we can ensure that the
value is both a number and is positive, using an
@scheme[and/c] contract:

@schemeblock[
(provide/contract 
  [amount (and/c number? positive?)])
]

@;{

==================================================

The section below discusses assigning to variables that are
provide/contract'd. This is currently buggy so this
discussion is elided. Here's the expansion of
the requiring module, just to give an idea:

(module m mzscheme 
  (require (lib "contract.ss"))
   (provide/contract [x x-ctc]))

(module n mzscheme (require m) (define (f) ... x ...))
==>
(module n mzscheme 
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

<table src="simple.ss">
<tr><td bgcolor="e0e0fa">
<scheme>
;; Language: Pretty Big
(module a mzscheme 
  (require (lib "contract.ss"))

  (provide/contract 
   [amount positive?])

  (provide 
   ;; -> Void
   ;; effect: sets variable a
   do-it)
  
  (define amount 4)
  
  (define (do-it) <font color="red">(set! amount -4)</font>))

(module b mzscheme 
  (require a)
  
  (printf "~s~n" amount)
  <font color="red">(do-it)</font>
  (printf "~s~n" amount))

(require b)
</scheme>
<td bgcolor="beige" valign="top">
<pre>

the "server" module 
this allows us to write contracts 

export @scheme[amount] with a contract 


export @scheme[do-it] without contract 



set amount to 4, 
  which satisfies contract


the "client" module 
requires functionality from a

first reference to @scheme[amount] (okay)
a call to @scheme[do-it], 
second reference to @scheme[amount] (fail)

</pre> </table>

<p><strong>Note:</strong> The above example is mostly self-explanatory. Take a
look at the lines in red, however. Even though the call to @scheme[do-it]
sets @scheme[amount] to -4, this action is <strong>not</strong> a contract
violation. The contract violation takes place only when the client module
(@scheme[b]) refers to @scheme[amount] again and the value flows across
the module boundary for a second time. 

</question>
}

@question[#:tag "obligations"]{How can a ``server'' module impose obligations on its client?}

On occasion, a module may want to enter a contract with
another module only if the other module abides by certain
rules. In other words, the module isn't just promising some
services, it also demands the client to deliver
something. This kind of thing happens when a module exports
a function, an object, a class or other values that enable
values to flow in both directions.
