#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]
@require["contracts-utils.ss"]
@(require (for-label scheme/contract))

@title[#:tag "contract-func"]{Simple Contracts on Functions}

When a module exports a function, it establishes two channels of
communication between itself and the client module that imports the
function. If the client module calls the function, it sends a value into the
"server" module. Conversely, if such a function call ends and the function
returns a value, the "server" module sends a value back to the "client" module. 

It is important to keep this picture in mind when you read the explanations
of the various ways of imposing contracts on functions. 

@question[#:tag "argcontract"]{How does a contract restrict the arguments of a function?}

Functions usually don't work on all possible Scheme values but only on a
select subset such as numbers, booleans, etc. Here is a module that may
represent a bank account: 

@schememod[
scheme/base
(require scheme/contract)

(provide/contract 
  [create  (-> string? number? any)]
  [deposit (-> number? any)])

(define amount 0)
(define (create name initial-deposit) ...)
(define (deposit a) (set! amount (+ amount a)))
]

It exports two functions: 
@itemize{

@item{@scheme[create]: The function's contract says that it consumes two
arguments, a string and a number, and it promises nothing about the return value. }}

@item{@scheme[deposit]: The function's contract demands from the client modules
that they apply it to numbers.  It promises nothing about the return value. }

If a "client" module that were to apply @scheme[deposit] to
@scheme['silly], it would violate the contract.  The contract monitoring
system would flag this violation and blame "client" for breaking the contract
with @scheme[myaccount]. 

@bold{Note:} Instead of @scheme[any] you could also use the
more specific contract @scheme[void?], which says that the function will
always return the @scheme[(void)] value. This contract, however, would require
the contract monitoring system to check the return value every time the function
is called, even though the "client" module can't do much with this value
anyway. In contrast, @scheme[any] tells the monitoring system @italic{not}
to check the return value. Additionally, it tells a potential client that the
"server" module @italic{makes no promises at all} about the function's return
value.

@question[#:tag "arrow"]{What does the arrow do?}

It is natural to use an arrow to say that an exported value is a
function. In decent high schools, you learn that a function has a domain
and a range, and that you write this fact down like this: 
@schemeblock[
f : A -> B
]
Here the @scheme[A] and @scheme[B] are sets; @scheme[A] is the
domain and @scheme[B] is the range. 

Functions in a programming language have domains and ranges, too. In
statically typed languages, you write down the names of types for each
argument and for the result. When all you have, however, is a Scheme name,
such as @scheme[create] or @scheme[deposit], you want to tell the
reader what the name represents (a function) and, if it is a function (or
some other complex value) what the pieces are supposed to be. This is why
we use a @scheme[->] to say "hey, expect this to be a function." 

So @scheme[->] says "this is contract for a function." What follows
in a function contracts are contracts (sub-contracts if you wish) that tell
the reader what kind of arguments to expect and what kind of a result the
function will produce. For example, 
@schemeblock[
(provide/contract
 [create (-> string? number? boolean? account?)])
]
says that @scheme[create] is a function of three arguments: a string, a
number, and a boolean. Its result is an account. 

In short, the arrow @scheme[->] is a @italic{contract
combinator}. Its purpose is to combine other contracts into a contract
that says "this is a function @italic{and} its arguments and its result are
like that."

@question[#:tag "dots"]{What are the dots in contracts about?}

If you are used to mathematics, you like the arrow in between the domain
  and the range of a function, not at the beginning. If you have read "How
  to Design Programs," you have seen this many times. Indeed, you may have
  seen contracts such as these in other people's code:

@schemeblock[
(provide/contract
  [create (string? number? boolean? . -> . account?)])
]

If a PLT Scheme S-expression contains two dots with a symbol in the middle,
the reader re-arranges the S-expression and place the symbol at the front. Thus, 
@schemeblock[
(string? number? boolean? . -> . account?)
]
is really just a short-hand for 
@schemeblock[
(-> string? number? boolean? account?)
]
Of course, placing the arrow to the left of the range follows not only
mathematical tradition but also that of typed functional languages. 

@question[#:tag "own"]{Can I make my own contracts for arguments?}

The @scheme[deposit] function adds the given number to the value of
@scheme[amount]. While the function's contract prevents clients from
applying it to non-numbers, the contract still allows them to apply the function
to complex numbers, negative numbers, or inexact numbers, all of which do not
represent amounts of money. 

To this end, the contract system allows programmers to define their own
contracts: 

@schememod[
scheme/base
(require scheme/contract)
  
(define (amount? a)
  (and (number? a) (integer? a) (exact? a) (>= a 0)))

(provide/contract
  (code:comment "an amount is a natural number of cents")
  (code:comment "is the given number an amount?")
  [deposit (-> amount? any)]
  [amount? (-> any/c boolean?)])
  
(define this 0)
(define (deposit a) (set! this (+ this a)))
]

The module introduces a
predicate, @scheme[amount?]. The @scheme[provide]
clause refers to this predicate, as a contract, for its
specification of the contract of
@scheme[deposit].

Of course it makes no sense to restrict a channel of
communication to values that the client doesn't
understand. Therefore the module also exports
the @scheme[amount?] predicate itself, with a contract
saying that it accepts an arbitrary value and returns a
boolean.

In this case, we could also have used @scheme[natural-number/c], which is
a contract defined in "contract.ss" that is equivalent to @scheme[amount]
(modulo the name): 

@schememod[
scheme/base

(require scheme/contract)

(provide/contract
  (code:comment "an amount is a natural number of cents")
  [deposit (-> natural-number/c any)])
  
(define this 0)
(define (deposit a) (set! this (+ this a)))
]

Lesson: look up the built-in contracts. 

@question[#:tag "and-or"]{Are and/c and or/c contract combinators? What of listof?}

The short answer is yes. Both @scheme[and/c] and @scheme[or/c]
ombine contracts and they do what you expect them to do. 

For example, if we didn't have @scheme[natural-number/c], the
@scheme[amount?] contract is a bit opaque. Instead, we would define it
as follows: 

@schememod[
scheme/base
(require scheme/contract)

(define amount 
  (and/c number? integer? exact? (or/c positive? zero?)))

(provide/contract
  (code:comment "an amount is a natural number of cents")
  (code:comment "is the given number an amount?")
  [deposit (-> amount any)])
  
(define this 0)
(define (deposit a) (set! this (+ this a)))
]

That is, amount is a contract that enforces the following conditions: the
value satisfies @scheme[number?] and @scheme[integer?] and
@scheme[exact?] and is either @scheme[positive?] or
@scheme[zero?].

Oh, we almost forgot. What do you think @scheme[(listof char?)]
means? Hint: it is a contract!

@question[#:tag "range"]{How does a contract restrict the range of a function?}

Consider a utility module for creating strings from banking records: 

@schememod[scheme/base
(require scheme/contract)
  
(provide/contract
  ...
  (code:comment "convert a random number to a string")
  [format-number (-> number? string?)]
  
  (code:comment "convert an amount into a dollar based string")
  [format-nat (-> natural-number/c 
                  (lambda (result)
                    (define L (string-length result))
                    (and (string? result)
                         (>= L 3)
                         (char=? #\. (string-ref result (- L 3))))))])
...
]
The contract of the exported function @scheme[format-number] specifies that
the function consumes a number and produces a string. 

The contract of the exported function @scheme[format-nat] is more
interesting than the one of @scheme[format-number].  It consumes only
natural numbers. Its range contract promises a string that has a dot "." in the
third position from the right. 

@bold{Exercise 1} Strengthen the promise of the range contract for
@scheme[format-nat] so that it admits only strings with digits and a single
dot. 

@question[#:tag "exercise1"]{Solution to Exercise 1}

@schememod[scheme/base
(require scheme/contract)

(define (digit-char? x) 
  (member x '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
  
(provide/contract
  ...
  (code:comment "convert a random number to a string")
  [format-number (-> number? string?)]
  
  (code:comment "convert an  amount (natural number) of cents")
  (code:comment "into a dollar based string")
  [format-nat (-> natural-number/c 
                  (lambda (result)
                    (define L (string-length result))
                    (and (string? result)
                         (andmap digit-char? 
                                 (string->list (substring result 0 (- L 3))))
                         (andmap digit-char?
                                 (string->list (substring result (- L 2) L)))
                         (char=? #\. (string-ref result (- L 3))))))])
]


