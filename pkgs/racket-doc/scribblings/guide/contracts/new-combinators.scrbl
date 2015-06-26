#lang scribble/doc
@(require scribble/manual scribble/eval "utils.rkt"
          (for-label racket/contract racket/gui))

@(define ex-eval (make-base-eval))
@(ex-eval '(require racket/contract))

@title{Building New Contracts}

Contracts are represented internally as functions that
accept information about the contract (who is to blame,
source locations, @|etc|) and produce projections (in the
spirit of Dana Scott) that enforce the contract. A
projection is a function that accepts an arbitrary value,
and returns a value that satisfies the corresponding
contract. For example, a projection that accepts only
integers corresponds to the contract @racket[(flat-contract
integer?)], and can be written like this:

@racketblock[
(define int-proj
  (λ (x)
    (if (integer? x)
        x
        (signal-contract-violation))))
]

As a second example, a projection that accepts unary functions
on integers looks like this:

@racketblock[
(define int->int-proj
  (λ (f)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (λ (x) (int-proj (f (int-proj x))))
        (signal-contract-violation))))
]

Although these projections have the right error behavior,
they are not quite ready for use as contracts, because they
do not accommodate blame and do not provide good error
messages. In order to accommodate these, contracts do not
just use simple projections, but use functions that accept a
@deftech{blame object} encapsulating
the names of two parties that are the candidates for blame,
as well as a record of the source location where the
contract was established and the name of the contract. They
can then, in turn, pass that information
to @racket[raise-blame-error] to signal a good error
message.

Here is the first of those two projections, rewritten for
use in the contract system:
@racketblock[
(define (int-proj blame)
  (λ (x)
    (if (integer? x)
        x
        (raise-blame-error
         blame
         x
         '(expected: "<integer>" given: "~e")
         x))))
]
The new argument specifies who is to be blamed for
positive and negative contract violations.

Contracts, in this system, are always
established between two parties. One party, called the server, provides some
value according to the contract, and the other, the client, consumes the
value, also according to the contract. The server is called
the positive position and the client the negative position. So,
in the case of just the integer contract, the only thing
that can go wrong is that the value provided is not an
integer. Thus, only the positive party (the server) can ever accrue
blame.  The @racket[raise-blame-error] function always blames
the positive party.

Compare that to the projection for our function contract:

@racketblock[
(define (int->int-proj blame)
  (define dom (int-proj (blame-swap blame)))
  (define rng (int-proj blame))
  (λ (f)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (λ (x) (rng (f (dom x))))
        (raise-blame-error
         blame
         f
         '(expected "a procedure of one argument" given: "~e")
         f))))
]

In this case, the only explicit blame covers the situation
where either a non-procedure is supplied to the contract or
the procedure does not accept one argument. As with
the integer projection, the blame here also lies with the
producer of the value, which is
why @racket[raise-blame-error] is passed @racket[blame] unchanged.

The checking for the domain and range are delegated to
the @racket[int-proj] function, which is supplied its
arguments in the first two lines of
the @racket[int->int-proj] function. The trick here is that,
even though the @racket[int->int-proj] function always
blames what it sees as positive, we can swap the blame parties by
calling @racket[blame-swap] on the given @tech{blame object}, replacing
the positive party with the negative party and vice versa.

This technique is not merely a cheap trick to get the example to work,
however. The reversal of the positive and the negative is a
natural consequence of the way functions behave. That is,
imagine the flow of values in a program between two
modules. First, one module (the server) defines a function, and then that
module is required by another (the client). So far, the function itself
has to go from the original, providing module to the
requiring module. Now, imagine that the providing module
invokes the function, supplying it an argument. At this
point, the flow of values reverses. The argument is
traveling back from the requiring module to the providing
module! The client is ``serving'' the argument to the server,
and the server is receiving that value as a client.
And finally, when the function produces a result,
that result flows back in the original
direction from server to client.
Accordingly, the contract on the domain reverses
the positive and the negative blame parties, just like the flow
of values reverses.

We can use this insight to generalize the function contracts
and build a function that accepts any two contracts and
returns a contract for functions between them.

This projection also goes further and uses
@racket[blame-add-context] to improve the error messages
when a contract violation is detected.

@racketblock[
(define (make-simple-function-contract dom-proj range-proj)
  (λ (blame)
    (define dom (dom-proj (blame-add-context blame
                                             "the argument of"
                                             #:swap? #t)))
    (define rng (range-proj (blame-add-context blame
                                               "the range of")))
    (λ (f)
      (if (and (procedure? f)
               (procedure-arity-includes? f 1))
          (λ (x) (rng (f (dom x))))
          (raise-blame-error
           blame
           f
           '(expected "a procedure of one argument" given: "~e")
           f)))))
]

While these projections are supported by the contract library
and can be used to build new contracts, the contract library
also supports a different API for projections that can be more
efficient. Specifically, a @deftech{val first projection} accepts
a blame object without the negative blame information and then
returns a function that accepts the value to be contracted, and
then finally accepts the name of the negative party to the contract
before returning the value with the contract. Rewriting @racket[int->int-proj]
to use this API looks like this:
@interaction/no-prompt[#:eval ex-eval
(define (int->int-proj blame)
  (define dom-blame (blame-add-context blame
                                       "the argument of"
                                       #:swap? #t))
  (define rng-blame (blame-add-context blame "the range of"))
  (define (check-int v to-blame neg-party)
    (unless (integer? v)
      (raise-blame-error
       to-blame #:missing-party neg-party
       v
       '(expected "an integer" given: "~e")
       v)))
  (λ (f)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (λ (neg-party)
          (λ (x)
            (check-int x dom-blame neg-party)
            (define ans (f x))
            (check-int ans rng-blame neg-party)
            ans))
        (λ (neg-party)
          (raise-blame-error
           blame #:missing-party neg-party
           f
           '(expected "a procedure of one argument" given: "~e")
           f)))))]
The advantage of this style of contract is that the @racket[_blame]
and @racket[_v] arguments can be supplied on the server side of the
contract boundary and the result can be used for every different
client. With the simpler situation, a new blame object has to be
created for each client.

Projections like the ones described above, but suited to
other, new kinds of value you might make, can be used with
the contract library primitives. Specifically, we can use 
@racket[make-chaperone-contract] to build it:
@interaction/no-prompt[#:eval ex-eval
 (define int->int-contract
   (make-contract
    #:name 'int->int
    #:val-first-projection int->int-proj))]
and then combine it with a value and get some contract
checking.
@def+int[#:eval 
         ex-eval
         (define/contract (f x)
           int->int-contract
           "not an int")
         (f #f)
         (f 1)]

@section{Contract Struct Properties}

The @racket[make-chaperone-contract] function is okay for one-off contracts,
but often you want to make many different contracts that differ only
in some pieces. The best way to do that is to use a @racket[struct]
with either @racket[prop:contract], @racket[prop:chaperone-contract], or
@racket[prop:flat-contract]. 

For example, lets say we wanted to make a simple form of the @racket[->]
contract that accepts one contract for the range and one for the domain. 
We should define a struct with two fields and use 
@racket[build-chaperone-contract-property] to construct the chaperone contract
property we need.
@interaction/no-prompt[#:eval ex-eval
                              (struct simple-arrow (dom rng)
                                #:property prop:chaperone-contract
                                (build-chaperone-contract-property
                                 #:name
                                 (λ (arr) (simple-arrow-name arr))
                                 #:val-first-projection
                                 (λ (arr) (simple-arrow-val-first-proj arr))))]

To do the automatic coercion of values like @racket[integer?] and @racket[#f]
into contracts, we need to call @racket[coerce-chaperone-contract]
(note that this rejects impersonator contracts and does not insist
on flat contracts; to do either of those things, call @racket[coerce-contract]
or @racket[coerce-flat-contract] instead).
@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-contract dom rng)
                                (simple-arrow (coerce-contract 'simple-arrow-contract dom)
                                              (coerce-contract 'simple-arrow-contract rng)))]

To define @racket[_simple-arrow-name] is straight-forward; it needs to return
an s-expression representing the contract:
@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-name arr)
                                `(-> ,(contract-name (simple-arrow-dom arr))
                                     ,(contract-name (simple-arrow-rng arr))))]
And we can define the projection using a generalization of the 
projection we defined earlier, this time using 
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{chaperones}:
@interaction/no-prompt[#:eval
                       ex-eval
                       (define (simple-arrow-val-first-proj arr)
                         (define dom-ctc (get/build-val-first-projection (simple-arrow-dom arr)))
                         (define rng-ctc (get/build-val-first-projection (simple-arrow-rng arr)))
                         (λ (blame)
                           (define dom+blame (dom-ctc (blame-add-context blame
                                                                         "the argument of"
                                                                         #:swap? #t)))
                           (define rng+blame (rng-ctc (blame-add-context blame "the range of")))
                           (λ (f)
                             (if (and (procedure? f)
                                      (procedure-arity-includes? f 1))
                                 (λ (neg-party)
                                   (chaperone-procedure
                                    f
                                    (λ (arg) 
                                      (values 
                                       (λ (result) ((rng+blame result) neg-party))
                                       ((dom+blame arg) neg-party)))))
                                 (λ (neg-party)
                                   (raise-blame-error
                                    blame #:missing-party neg-party
                                    f
                                    '(expected "a procedure of one argument" given: "~e")
                                    f))))))]

@def+int[#:eval 
         ex-eval
         (define/contract (f x)
           (simple-arrow-contract integer? boolean?)
           "not a boolean")
         (f #f)
         (f 1)]

@section{With all the Bells and Whistles}

There are a number of optional pieces to a contract that 
@racket[simple-arrow-contract] did not add. In this section,
we walk through all of them to show examples of how they can
be implemented.

The first is a first-order check. This is used by @racket[or/c]
in order to determine which of the higher-order argument contracts
to use when it sees a value. Here's the function for 
our simple arrow contract.
@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-first-order ctc)
                                (λ (v) (and (procedure? v) 
                                            (procedure-arity-includes? v 1))))]
It accepts a value and returns @racket[#f] if the value is guaranteed not
to satisfy the contract, and @racket[#t] if, as far as we can tell, 
the value satisfies the contract, just be inspecting first-order
properties of the value.

The next is random generation. Random generation in the contract
library consists of two pieces: the ability to randomly generate
values satisfying the contract and the ability to exercise values
that match the contract that are given, in the hopes of finding bugs
in them (and also to try to get them to produce interesting values to
be used elsewhere during generation).

To exercise contracts, we need to implement a function that
is given a @racket[arrow-contract] struct and some fuel. It should return
two values: a function that accepts values of the contract
and exercises them, plus a list of values that the exercising
process will always produce. In the case of our simple
contract, we know that we can always produce values of the range,
as long as we can generate values of the domain (since we can just
call the function). So, here's a function that matches the 
@racket[_exercise] argument of @racket[build-chaperone-contract-property]'s
contract:
@interaction/no-prompt[#:eval
                       ex-eval
                       (define (simple-arrow-contract-exercise arr)
                         (define env (contract-random-generate-get-current-environment))
                         (λ (fuel)
                           (define dom-generate 
                             (contract-random-generate/choose (simple-arrow-dom arr) fuel))
                           (cond
                             [dom-generate
                              (values 
                               (λ (f) (contract-random-generate-stash
                                       env
                                       (simple-arrow-rng arr)
                                       (f (dom-generate))))
                               (list (simple-arrow-rng arr)))]
                             [else
                              (values void '())])))]
If the domain contract can be generated, then we know we can do some good via exercising. 
In that case, we return a procedure that calls @racket[_f] (the function matching
the contract) with something that we generated from the domain, and we stash the result
value in the environment too. We also return @racket[(simple-arrow-rng arr)]
to indicate that exercising will always produce something of that contract.

If we cannot, then we simply return a function that
does no exercising (@racket[void]) and the empty list (indicating that we won't generate
any values).

Then, to generate values matching the contract, we define a function
that when given the contract and some fuel, makes up a random function.
To help make it a more effective testing function, we can exercise
any arguments it receives, and also stash them into the generation
environment, but only if we can generate values of the range contract.
@interaction/no-prompt[#:eval
                       ex-eval
                       (define (simple-arrow-contract-generate arr)
                         (λ (fuel)
                           (define env (contract-random-generate-get-current-environment))
                           (define rng-generate 
                             (contract-random-generate/choose (simple-arrow-rng arr) fuel))
                           (cond
                             [rng-generate
                              (λ ()
                                (λ (arg)
                                  (contract-random-generate-stash env (simple-arrow-dom arr) arg)
                                  (rng-generate)))]
                             [else
                              #f])))]

When the random generation pulls something out of the environment,
it needs to be able to tell if a value that has been passed to
@racket[contract-random-generate-stash] is a candidate for
the contract it is trying to generate. Of course, it the contract
passed to @racket[contract-random-generate-stash] is an exact
match, then it can use it. But it can also use the value if the
contract is stronger (in the sense that it accepts fewer values).

To provide that functionality, we implement this function:
@interaction/no-prompt[#:eval ex-eval
                              (define (simple-arrow-first-stronger? this that)
                                (and (simple-arrow? that)
                                     (contract-stronger? (simple-arrow-dom that)
                                                         (simple-arrow-dom this))
                                     (contract-stronger? (simple-arrow-rng this)
                                                         (simple-arrow-rng that))))]
This function accepts @racket[_this] and @racket[_that], two contracts. It is
guaranteed that @racket[_this] will be one of our simple arrow contracts,
since we're supplying this function together with the simple arrow implementation.
But the @racket[_that] argument might be any contract. This function
checks to see if @racket[_that] is also a simple arrow contract and, if so
compares the domain and range. Of course, there are other contracts that we
could also check for (e.g., contracts built using @racket[->] or @racket[->*]),
but we do not need to. The stronger function is allowed to return @racket[#f]
if it doesn't know the answer but if it returns @racket[#t], then the contract
really must be stronger.

Now that we have all of the pieces implemented, we need to pass them
to @racket[build-chaperone-contract-property] so the contract system
starts using them:
@interaction/no-prompt[#:eval ex-eval
                              (struct simple-arrow (dom rng)
                                #:property prop:custom-write contract-custom-write-property-proc
                                #:property prop:chaperone-contract
                                (build-chaperone-contract-property
                                 #:name
                                 (λ (arr) (simple-arrow-name arr))
                                 #:val-first-projection
                                 (λ (arr) (simple-arrow-val-first-proj arr))
                                 #:first-order simple-arrow-first-order
                                 #:stronger simple-arrow-first-stronger?
                                 #:generate simple-arrow-contract-generate
                                 #:exercise simple-arrow-contract-exercise))
                              
                              (define (simple-arrow-contract dom rng)
                                (simple-arrow (coerce-contract 'simple-arrow-contract dom)
                                              (coerce-contract 'simple-arrow-contract rng)))]
We also add a @racket[prop:custom-write] property so
that the contracts print properly, e.g.:
@interaction[#:eval ex-eval (simple-arrow-contract integer? integer?)]
(We use @racket[prop:custom-write] because the contract library
can not depend on @racketmod[racket/generic] but yet still wants
to provide some help to make it easy to use the right printer.)

Now that that's done, we can use the new functionality. Here's a random function, 
generated by the contract library, using our @racket[simple-arrow-contract-generate]
function:
@def+int[#:eval 
         ex-eval
         (define a-random-function
           (contract-random-generate 
            (simple-arrow-contract integer? integer?)))
         (a-random-function 0)
         (a-random-function 1)]

Here's how the contract system can now automatically find bugs in functions
that consume simple arrow contracts:
@def+int[#:eval 
         ex-eval
         (define/contract (misbehaved-f f)
           (-> (simple-arrow-contract integer? boolean?) any)
           (f "not an integer"))
         (contract-exercise misbehaved-f)]

And if we hadn't implemented @racket[simple-arrow-first-order], then
@racket[or/c] would not be able to tell which branch of the @racket[or/c]
to use in this program:
@def+int[#:eval
         ex-eval
         (define/contract (maybe-accepts-a-function f)
           (or/c (simple-arrow-contract real? real?)
                 (-> real? real? real?)
                 real?)
           (if (procedure? f)
               (if (procedure-arity-includes f 1)
                   (f 1132)
                   (f 11 2))
               f))
         (maybe-accepts-a-function sqrt)
         (maybe-accepts-a-function 123)]


@(close-eval ex-eval)
