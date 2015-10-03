#lang scribble/doc
@(require scribble/manual scribble/eval
          scribble/core racket/list 
          scribble/racket
          "../guide-utils.rkt" "utils.rkt"
          (for-label racket/contract))

@title[#:tag "contract-func"]{Simple Contracts on Functions}

A mathematical function has a @deftech{domain} and a
@deftech{range}. The domain indicates the kind of values that the
function can accept as arguments, and the range indicates the kind of
values that it produces. The conventional notation for describing a
function with its domain and range is

@racketblock[
f : A -> B
]

where @racket[A] is the domain of the function and @racket[B] is the
range.

Functions in a programming language have domains and ranges, too, and
a contract can ensure that a function receives only values in its
domain and produces only values in its range. A @racket[->] creates
such a contract for a function. The forms after a @racket[->] specify
contracts for the domains and finally a contract for the range.

Here is a module that might represent a bank account:

@racketmod[
racket

(provide (contract-out
          [deposit (-> number? any)]
          [balance (-> number?)]))

(define amount 0)
(define (deposit a) (set! amount (+ amount a)))
(define (balance) amount)
]

The module exports two functions: 

@itemize[

@item{@racket[deposit], which accepts a number and returns some value
      that is not specified in the contract, and}

@item{@racket[balance], which returns a number indicating the current
      balance of the account.}

]

When a module exports a function, it establishes two channels of
communication between itself as a ``server'' and the ``client'' module
that imports the function. If the client module calls the function, it
sends a value into the server module. Conversely, if such a function
call ends and the function returns a value, the server module sends a
value back to the client module. This client--server distinction is
important, because when something goes wrong, one or the other of the
parties is to blame.

If a client module were to apply @racket[deposit] to @racket['millions],
it would violate the contract.  The contract-monitoring system would
catch this violation and blame the client for breaking the contract with
the above module. In contrast, if the @racket[balance] function were
to return @racket['broke], the contract-monitoring system
would blame the server module.

A @racket[->] by itself is not a contract; it is a @deftech{contract
combinator}, which combines other contracts to form a contract.

@; ------------------------------------------------------------------------

@section{Styles of @racket[->]}

If you are used to mathematical functions, you may prefer a contract
  arrow to appear between the domain and the range of a function, not
  at the beginning. If you have read @|HtDP|, you have seen this many
  times. Indeed, you may have seen contracts such as these in other
  people's code:

@racketblock[
(provide (contract-out
          [deposit (number? . -> . any)]))
]

If a Racket S-expression contains two dots with a symbol in the middle,
the reader re-arranges the S-expression and place the symbol at the front, 
as described in @secref["lists-and-syntax"].  Thus, 

@racketblock[
(number? . -> . any)
]

is just another way of writing

@racketblock[
(-> number? any)
]

@; ------------------------------------------------------------------------

@section[#:tag "simple-nested"]{Using @racket[define/contract] and @racket[->]}

The @racket[define/contract] form introduced in @ctc-link["intro-nested"] can
also be used to define functions that come with a contract. For example,

@racketblock[
(define/contract (deposit amount)
  (-> number? any)
  (code:comment "implementation goes here")
  ....)
]

which defines the @racket[deposit] function with the contract from earlier.
Note that this has two potentially important impacts on the use of
@racket[deposit]:

@itemlist[#:style 'ordered
  @item{Since the contract will always be checked on calls to @racket[deposit],
        even inside the module in which it is defined, this may increase
        the number of times the contract is checked. This could lead to
        a performance degradation. This is especially true if the function
        is called repeatedly in loops or using recursion.}
  @item{In some situations, a function may be written to accept a more
        lax set of inputs when called by other code in the same module.
        For such use cases, the contract boundary established by
        @racket[define/contract] is too strict.}
]

@; ----------------------------------------------------------------------
@section{@racket[any] and @racket[any/c]}

The @racket[any] contract used for @racket[deposit] matches any kind
of result, and it can only be used in the range position of a function
contract.  Instead of @racket[any] above, we could use the more
specific contract @racket[void?], which says that the function will
always return the @racket[(void)] value. The @racket[void?] contract,
however, would require the contract monitoring system to check the
return value every time the function is called, even though the
``client'' module can't do much with the value. In contrast,
@racket[any] tells the monitoring system @italic{not} to check the
return value, it tells a potential client that the ``server'' module
@italic{makes no promises at all} about the function's return value,
even whether it is a single value or multiple values.

The @racket[any/c] contract is similar to @racket[any], in that it
makes no demands on a value. Unlike @racket[any], @racket[any/c]
indicates a single value, and it is suitable for use as an argument
contract. Using @racket[any/c] as a range contract imposes a check
that the function produces a single value. That is,

@racketblock[(-> integer? any)]

describes a function that accepts an integer and returns any number of
values, while

@racketblock[(-> integer? any/c)]

describes a function that accepts an integer and produces a single
result (but does not say anything more about the result). The function

@racketblock[
(define (f x) (values (+ x 1) (- x 1)))
]

matches @racket[(-> integer? any)], but not @racket[(-> integer? any/c)].

Use @racket[any/c] as a result contract when it is particularly
important to promise a single result from a function. Use @racket[any]
when you want to promise as little as possible (and incur as little
checking as possible) for a function's result.

@; ------------------------------------------------------------------------

@ctc-section[#:tag "own"]{Rolling Your Own Contracts}

The @racket[deposit] function adds the given number to the value of
@racket[amount]. While the function's contract prevents clients from
applying it to non-numbers, the contract still allows them to apply
the function to complex numbers, negative numbers, or inexact numbers,
none of which sensibly represent amounts of money.

The contract system allows programmers to define their own contracts
as functions:

@racketmod[
racket
  
(define (amount? a)
  (and (number? a) (integer? a) (exact? a) (>= a 0)))

(provide (contract-out
          (code:comment "an amount is a natural number of cents")
          (code:comment "is the given number an amount?")
          [deposit (-> amount? any)]
          [amount? (-> any/c boolean?)]
          [balance (-> amount?)]))
  
(define amount 0)
(define (deposit a) (set! amount (+ amount a)))
(define (balance) amount)
]

This module defines an @racket[amount?] function and uses it as a
contract within @racket[->] contracts. When a client calls the
@racket[deposit] function as exported with the contract @racket[(->
amount? any)], it must supply an exact, nonnegative integer, otherwise
the @racket[amount?] function applied to the argument will return
@racket[#f], which will cause the contract-monitoring system to blame
the client. Similarly, the server module must provide an exact,
nonnegative integer as the result of @racket[balance] to remain
blameless.

Of course, it makes no sense to restrict a channel of communication to
values that the client doesn't understand. Therefore the module also
exports the @racket[amount?] predicate itself, with a contract saying
that it accepts an arbitrary value and returns a boolean.

In this case, we could also have used @racket[natural-number/c] in
place of @racket[amount?], since it implies exactly the same check:

@racketblock[
(provide (contract-out
          [deposit (-> natural-number/c any)]
          [balance (-> natural-number/c)]))
]

Every function that accepts one argument can be treated as a predicate
and thus used as a contract. For combining existing checks into a new
one, however, contract combinators such as @racket[and/c] and
@racket[or/c] are often useful. For example, here is yet another way
to write the contracts above:

@racketblock[
(define amount/c 
  (and/c number? integer? exact? (or/c positive? zero?)))

(provide (contract-out
          [deposit (-> amount/c any)]
          [balance (-> amount/c)]))
]

Other values also serve double duty as contracts.  For example, if a
function accepts a number or @racket[#f], @racket[(or/c number?  #f)]
suffices. Similarly, the @racket[amount/c] contract could have been
written with a @racket[0] in place of @racket[zero?]. If you use a
regular expression as a contract, the contract accepts strings and
byte strings that match the regular expression.

Naturally, you can mix your own contract-implementing functions with
combinators like @racket[and/c]. Here is a module for creating strings
from banking records:

@racketmod[
racket

(define (has-decimal? str)
  (define L (string-length str))
  (and (>= L 3)
       (char=? #\. (string-ref str (- L 3)))))

(provide (contract-out
          (code:comment "convert a random number to a string")
          [format-number (-> number? string?)]

          (code:comment "convert an amount into a string with a decimal")
          (code:comment "point, as in an amount of US currency")
          [format-nat (-> natural-number/c
                          (and/c string? has-decimal?))]))
]
The contract of the exported function @racket[format-number] specifies
that the function consumes a number and produces a string. The
contract of the exported function @racket[format-nat] is more
interesting than the one of @racket[format-number].  It consumes only
natural numbers. Its range contract promises a string that has a
@litchar{.} in the third position from the right.

If we want to strengthen the promise of the range contract for
@racket[format-nat] so that it admits only strings with digits and a single
dot, we could write it like this:

@racketmod[
racket

(define (digit-char? x) 
  (member x '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(define (has-decimal? str)
  (define L (string-length str))
  (and (>= L 3)
       (char=? #\. (string-ref str (- L 3)))))

(define (is-decimal-string? str)
  (define L (string-length str))
  (and (has-decimal? str)
       (andmap digit-char?
               (string->list (substring str 0 (- L 3))))
       (andmap digit-char?
               (string->list (substring str (- L 2) L)))))

....

(provide (contract-out
          ....
          (code:comment "convert an  amount (natural number) of cents")
          (code:comment "into a dollar-based string")
          [format-nat (-> natural-number/c 
                          (and/c string? 
                                 is-decimal-string?))]))
]

Alternately, in this case, we could use a regular expression as a
contract:

@racketmod[
racket

(provide 
 (contract-out
  ....
  (code:comment "convert an  amount (natural number) of cents")
  (code:comment "into a dollar-based string")
  [format-nat (-> natural-number/c
                  (and/c string? #rx"[0-9]*\\.[0-9][0-9]"))]))
]

@; ------------------------------------------------------------------------

@ctc-section{Contracts on Higher-order Functions}

Function contracts are not just restricted to having simple
predicates on their domains or ranges. Any of the contract
combinators discussed here, including function contracts
themselves, can be used as contracts on the arguments and
results of a function.

For example, 

@racketblock[(-> integer? (-> integer? integer?))]

is a contract that describes a curried function. It matches functions
that accept one argument and then return another function accepting a
second argument before finally returning an integer. If a server
exports a function @racket[make-adder] with this contract, and if
@racket[make-adder] returns a value other than a function, then the
server is to blame. If @racket[make-adder] does return a function, but
the resulting function is applied to a value other than an integer,
then the client is to blame.

Similarly, the contract

@racketblock[(-> (-> integer? integer?) integer?)]

describes functions that accept other functions as its input. If a
server exports a function @racket[twice] with this contract and the
@racket[twice] is applied to a value other than a function of one
argument, then the client is to blame. If @racket[twice] is applied to
a function of one argument and @racket[twice] calls the given function
on a value other than an integer, then the server is to blame.

@; ----------------------------------------------------------------------

@ctc-section[#:tag "flat-named-contracts"]{Contract Messages with ``tempN''}

You wrote your module. You added contracts. You put them into the interface
so that client programmers have all the information from interfaces. It's a
piece of art: 
@interaction[#:eval 
             contract-eval
             (module bank-server racket
               (provide
                (contract-out
                 [deposit (-> (λ (x)
                                (and (number? x) (integer? x) (>= x 0)))
                              any)]))
               
               (define total 0)
               (define (deposit a) (set! total (+ a total))))]

Several clients used your module. Others used their
modules in turn. And all of a sudden one of them sees this error
message:

@interaction[#:eval 
             contract-eval
             (require 'bank-server)
             (deposit -10)]

What is the @racketerror{temp7} doing there?  Wouldn't it be nice if
we had a name for this class of data much like we have string, number,
and so on?

For this situation, Racket provides @deftech{flat named
contracts}. The use of ``contract'' in this term shows that contracts
are first-class values. The ``flat'' means that the collection of data
is a subset of the built-in atomic classes of data; they are described
by a predicate that consumes all Racket values and produces a
boolean. The ``named'' part says what we want to do, which is to name
the contract so that error messages become intelligible:

@interaction[#:eval 
             contract-eval
             (module improved-bank-server racket
               (define (amount? x) (and (number? x) (integer? x) (>= x 0)))
               (define amount (flat-named-contract 'amount amount?))
  
               (provide (contract-out [deposit (amount . -> . any)]))
  
               (define total 0)
               (define (deposit a) (set! total (+ a total))))]

With this little change, the error message becomes quite readable:

@interaction[#:eval 
             contract-eval
             (require 'improved-bank-server)
             (deposit -10)]

@; not sure why, but if I define str directly to be the
@; expression below, then it gets evaluated before the 
@; expressions above it.
@(define str "huh?")

@(begin
   (set! str
         (with-handlers ((exn:fail? exn-message))
           (contract-eval '(deposit -10))))
   "")

@ctc-section[#:tag "dissecting-contract-errors"]{Dissecting a contract error message}

@(define (lines a b)
   (define lines (regexp-split #rx"\n" str))
   (table (style #f '())
          (map (λ (x) (list (paragraph error-color x)))
               (take (drop lines a) b))))

In general, each contract error message consists of six sections:
@itemize[@item{a name for the function or method associated with the contract
               and either the phrase ``contract violation'' or ``broke its contract''
               depending on whether the contract was violated by the client or the
               server; e.g. in the previous example: @lines[0 1]}
          @item{a description of the precise aspect of the contract that was violated, @lines[1 2]}
          @item{the complete contract plus a path into it showing which aspect was violated, @lines[3 2]}
          @item{the module where the contract was put (or, more generally, the boundary that the contract mediates), @lines[5 1]}
          @item{who was blamed, @lines[6 2]}
          @item{and the source location where the contract appears. @lines[8 1]}]
