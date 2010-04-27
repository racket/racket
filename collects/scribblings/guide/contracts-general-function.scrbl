#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          "contracts-utils.ss"
	  (for-label framework/framework)
          (for-label racket/contract) 
          (for-label racket/gui))

@title[#:tag "contracts-general-functions"]{Contracts on Functions in General}

@ctc-section[#:tag "flat-named-contracts"]{Contract Error Messages that Contain ``???''}

You wrote your module. You added contracts. You put them into the interface
so that client programmers have all the information from interfaces. It's a
piece of art: 
@racketmod[
racket

(provide/contract
 [deposit (-> (lambda (x)
                (and (number? x) (integer? x) (>= x 0)))
              any)])
  
(define this 0)
(define (deposit a) ...)
]

Several clients used your module. Others used their
modules in turn. And all of a sudden one of them sees this error
message:

@inset-flow{@racketerror{bank-client broke the contract (-> ??? any)
it had with myaccount on deposit; expected <???>, given: -10}}

Clearly, @racket[bank-client] is a module that uses @racket[myaccount]
but what is the @racketerror{???} doing there?  Wouldn't it be nice if
we had a name for this class of data much like we have string, number,
and so on?

For this situation, Racket provides @deftech{flat named
contracts}. The use of ``contract'' in this term shows that contracts
are first-class values. The ``flat'' means that the collection of data
is a subset of the built-in atomic classes of data; they are described
by a predicate that consumes all Racket values and produces a
boolean. The ``named'' part says what we want to do, which is to name
the contract so that error messages become intelligible:

@racketmod[
racket

(define (amount? x) (and (number? x) (integer? x) (>= x 0)))
(define amount (flat-named-contract 'amount amount?))
  
(provide/contract
 [deposit (amount . -> . any)])
  
(define this 0)
(define (deposit a) ...)
]

With this little change, the error message becomes all of the
sudden quite readable:

@inset-flow{@racketerror{bank-client broke the contract (-> amount
any) it had with myaccount on deposit; expected <amount>, given: -10}}

@ctc-section[#:tag "optional"]{Optional Arguments}

Take a look at this excerpt from a string-processing module, inspired by the
@link["http://racketcookbook.org"]{Racket cookbook}: 

@racketmod[
racket

(provide/contract
 (code:comment @#,t{pad the given str left and right with})
 (code:comment @#,t{the (optional) char so that it is centered})
 [string-pad-center (->* (string? natural-number/c)
                         (char?) 
                         string?)])

(define (string-pad-center str width [pad #\space])
  (define field-width (min width (string-length str)))
  (define rmargin (ceiling (/ (- width field-width) 2)))
  (define lmargin (floor (/ (- width field-width) 2)))
  (string-append (build-string lmargin (λ (x) pad))
                 str
                 (build-string rmargin (λ (x) pad))))
]

 The module exports @racket[string-pad-center], a function
 that creates a string of a given @racket[width] with the
 given string in the center. The default fill character is
 @racket[#\space]; if the client module wishes to use a
 different character, it may call @racket[string-pad-center]
 with a third argument, a @racket[char], overwriting the
 default.

The function definition uses optional arguments, which is
appropriate for this kind of functionality. The interesting
point here is the formulation of the contract for the
@racket[string-pad-center].


The contract combinator @racket[->*], demands several groups of contracts: 
@itemize[
@item{The first one is a parenthesized group of contracts for all required
arguments. In this example, we see two: @racket[string?] and
@racket[natural-number/c]. }

@item{The second one is a parenthesized group of contracts for all optional
arguments: @racket[char?]. }

@item{The last one is a single contract: the result of the function.}
]

 Note if a default value does not satisfy a contract, you
 won't get a contract error for this interface. In contrast
 to type systems, we do trust you; if you can't trust
 yourself, you need to communicate across boundaries for
 everything you write.

@ctc-section[#:tag "rest-args"]{Rest Arguments}

We all know that @racket[+] in Beginner Racket is a function
  that consumes at least two numbers but, in principle,
  arbitrarily many more. Defining the function is easy:
@racketblock[
(define (plus fst snd . rst)
  (foldr + (+ fst snd) rst))
]
Describing this function via a contract is difficult because of the rest
argument (@racket[rst]). 

Here is the contract:
@racketblock[
(provide/contract 
 [plus (->* (number? number?) () #:rest (listof number?) number?)])
]
The @racket[->*] contract combinator empowers you to specify
  functions that consume a variable number of arguments or functions like
  @racket[plus], which consume ``at least this number'' of arguments but
  an arbitrary number of additional arguments. 

The contracts for the required arguments are enclosed in the first
pair of parentheses: 
@racketblock[
(number? number?)
]

For @racket[plus] they demand two numbers. The empty pair of
parenthesis indicates that there are no optional arguments
(not counting the rest arguments) and the contract for the
rest argument follows @racket[#:rest]
@racketblock[
(listof number?)
]
  Since the remainder of the actual arguments are collected
  in a list for a rest parameter such as @racket[rst], the
  contract demands a list of values; in this specific
  examples, these values must be numbers.

@ctc-section[#:tag "keywords"]{Keyword Arguments}

Sometimes, a function accepts many arguments and remembering
their order can be a nightmare. To help with such functions,
Racket has @seclink["lambda-keywords"]{keyword} arguments. 

For example, consider this function that creates a simple
GUI and asks the user a yes-or-no question:
@racketmod[
racket/gui

(define (ask-yes-or-no-question #:question question 
                                #:default answer
                                #:title title
                                #:width w
                                #:height h)
  (define d (new dialog% [label title] [width w] [height h]))
  (define msg (new message% [label question] [parent d]))
  (define (yes) (set! answer #t) (send d show #f))
  (define (no) (set! answer #f) (send d show #f))
  (define yes-b (new button% 
                     [label "Yes"] [parent d] 
                     [callback (λ (x y) (yes))]
                     [style (if answer '(border) '())]))
  (define no-b (new button% 
                    [label "No"] [parent d] 
                    [callback (λ (x y) (no))]
                    [style (if answer '() '(border))]))
  (send d show #t)
  answer)

(provide/contract
 [ask-yes-or-no-question
  (-> #:question string?
      #:default boolean?
      #:title string?
      #:width exact-integer?
      #:height exact-integer?
      boolean?)])
]
@margin-note{Note that if you really want to ask a yes-or-no
question via a GUI, you should use
@racket[message-box/custom] (and generally speaking,
avoiding the responses ``yes'' and ``no'' in your dialog is a
good idea, too ...).}

The contract for @racket[ask-yes-or-no-question] uses our
old friend the @racket[->] contract combinator. Just like
@racket[lambda] (or @racket[define]-based functions) use
keywords for specifying keyword arguments, it uses keywords
for specifying contracts on keyword arguments. In this case,
it says that @racket[ask-yes-or-no-question] must receive
five keyword arguments, one for each of the keywords
@racket[#:question],
@racket[#:default],
@racket[#:title],
@racket[#:width], and
@racket[#:height]. 
Also, just like in a function definition, the keywords in
the @racket[->] may appear in any order.

@ctc-section[#:tag "optional-keywords"]{Optional Keyword Arguments}

Of course, many of the parameters in
@racket[ask-yes-or-no-question] (from the previous question)
have reasonable defaults, and should be made optional:
@racketblock[
(define (ask-yes-or-no-question #:question question 
                                #:default answer
                                #:title [title "Yes or No?"]
                                #:width [w 400]
                                #:height [h 200])
  ...)
]

To specify this function's contract, we need to use
@racket[->*]. It too supports keywords just as you might
expect, in both the optional and mandatory argument
sections. In this case, we have mandatory keywords
@racket[#:question] and @racket[#:default], and optional keywords 
@racket[#:title],
@racket[#:width], and
@racket[#:height]. So, we write the contract like this:
@racketblock[
(provide/contract
 [ask-yes-or-no-question
  (->* (#:question string?
        #:default boolean?)

       (#:title string?
        #:width exact-integer?
        #:height exact-integer?)

       boolean?)])
]
putting the mandatory keywords in the first section and the
optional ones in the second section.

@ctc-section[#:tag "arrow-d"]{When a Function's Result Depends on its Arguments}

Here is an excerpt from an imaginary (pardon the pun) numerics module:

@racketmod[
racket
(provide/contract 
 [sqrt.v1 (->d ([argument (>=/c 1)])
               ()
               [result (<=/c argument)])])
...
]

The contract for the exported function @racket[sqrt.v1] uses the
@racket[->d] rather than @racket[->] function contract. The ``d''
stands for @italic{dependent} contract, meaning the contract for the
function range depends on the value of the argument. 

In this particular case, the argument of @racket[sqrt.v1] is greater
or equal to 1. Hence a very basic correctness check is that the result is
smaller than the argument. (Naturally, if this function is critical, one
could strengthen this check with additional clauses.)

In general, a dependent function contract looks just like
the more general @racket[->*] contract, but with names added
that can be used elsewhere in the contract.

Yes, there are many other contract combinators such as @racket[<=/c]
and @racket[>=/c], and it pays off to look them up in the contract
section of the reference manual. They simplify contracts tremendously
and make them more accessible to potential clients. 

@ctc-section[#:tag "arrow-d-args"]{When Contract Arguments Depend on Each Other}

Eventually bank customers want their money back. Hence, a module that
implements a bank account must include a method for withdrawing money. Of
course, ordinary accounts don't let customers withdraw an arbitrary amount of
money but only as much as they have in the account. 

Suppose the account module provides the following two functions:
@racketblock[
balance : (-> account amount)
withdraw : (-> account amount account)
]
Then, informally, the proper precondition for @racket[withdraw] is that 
``the balance of the given account is greater than or equal to the given (withdrawal) amount.''
The postcondition is similar to the one for
@ctc-link["flat-named-contracts"]{@racket[deposit]}:
``the balance of the resulting account is larger than (or equal to) the one of the
given account.''
You could of course also formulate a full-fledged correctness condition, namely,
that the balance of the resulting account is equal to the balance of the given
one, plus the given amount.

The following module implements accounts imperatively and specifies the
conditions we just discussed:
@racketmod[
racket

(code:comment "section 1: the contract definitions")
(define-struct account (balance) #:mutable)
(define amount natural-number/c)

(define msg> "account a with balance larger than ~a expected")
(define msg< "account a with balance less than ~a expected")

(define (mk-account-contract acc amt op msg)
  (define balance0 (balance acc))
  (define (ctr a)
    (and (account? a) (op balance0 (balance a))))
  (flat-named-contract (format msg balance0) ctr))

(code:comment "section 2: the exports")
(provide/contract
 [create   (amount . -> . account?)]
 [balance  (account? . -> . amount)]
 [withdraw (->d ([acc account?]
                 [amt (and/c amount (<=/c (balance acc)))])
                ()
                [result (mk-account-contract acc amt >= msg>)])]
 [deposit  (->d ([acc account?]
                 [amt amount])
                ()
                [result (mk-account-contract acc amt <= msg<)])])

(code:comment "section 3: the function definitions")
(define balance account-balance)

(define (create amt) (make-account amt))

(define (withdraw acc amt)
  (set-account-balance! acc (- (balance acc) amt))
  acc)

(define (deposit acc amt)
  (set-account-balance! acc (+ (balance acc) amt))
  acc)
]

The second section is the export interface: @itemize[
@item{@racket[create] consumes an initial deposit and
produces an account. This kind of contract is just like a
type in a statically typed language, except that statically
typed languages usually don't support the type ``natural
numbers'' (as a full-fledged subtype of numbers). }

@item{@racket[balance] consumes an account and computes its current balance.}

@item{@racket[withdraw] consumes an account, named @racket[acc], and an
amount, @racket[amt]. In addition to being an @racket[amount], the
latter must also be less than @racket[(balance acc)], i.e., the balance of
the given account. That is, the contract for @racket[amt] depends on the
value of @racket[acc], which is what the @racket[->d] 
contract combinator expresses.

The result contract is formed on the fly: 
@racket[(mk-account-contract acc amt > msg>)]. 
It is an application of a contract-producing function that
consumes an account, an amount, a comparison operator, and an error message (a
format string). The result is a contract. 
}

@item{@racket[deposit]'s contract has been reformulated using the
@racket[->d] combinator. }
]

The code in the first section defines all those pieces that
are needed for the formulation of the export contracts:
@racket[account?], @racket[amount], error messages (format
strings), and @racket[mk-account-contract]. The latter is a
function that extracts the current balance from the given
account and then returns a named contract, whose error
message (contract name) is a string that refers to this
balance. The resulting contract checks whether an account
has a balance that is larger or smaller, depending on the
given comparison operator, than the original balance.

@ctc-section[#:tag "arrow-d-eval-order"]{Ensuring that a Function Properly Modifies State}

The @racket[->d] contract combinator can also ensure that a
function only modifies state according to certain
constraints. For example, consider this contract
(it is a slightly simplified from the function
@racket[preferences:add-panel] in the framework):
@racketblock[
(->d ([parent (is-a?/c area-container-window<%>)])
      ()
      [_
       (let ([old-children (send parent get-children)])
         (λ (child)
           (andmap eq?
                   (append old-children (list child))
                   (send parent get-children))))])
]
It says that the function accepts a single argument, named
@racket[parent], and that @racket[parent] must be
an object matching the interface @racket[area-container-window<%>].

The range contract ensures that the function only modifies
the children of @racket[parent] by adding a new child to the
front of the list. It accomplishes this by using the
@racket[_] instead of a normal identifier, which tells the
contract library that the range contract does not depend on
the values of any of the results, and thus the contract
library evaluates the expression following the @racket[_]
when the function is called, instead of when it
returns. Therefore the call to the @racket[get-children] method
happens before the function under the contract is called.
When the function under contract returns, its result is
passed in as @racket[child], and the contract ensures that
the children after the function return are the same as the
children before the function called, but with one more
child, at the front of the list.

To see the difference in a toy example that focuses
on this point, consider this program
@racketmod[
racket
(define x '())
(define (get-x) x)
(define (f) (set! x (cons 'f x)))
(provide/contract 
 [f (->d () () [_ (begin (set! x (cons 'ctc x)) any/c)])]
 [get-x (-> (listof symbol?))])
]
If you were to require this module, call @racket[f], then
the result of @racket[get-x] would be @racket['(f ctc)]. In
contrast, if the contract for @racket[f] were
@racketblock[(->d () () [res (begin (set! x (cons 'ctc x)) any/c)])]
(only changing the underscore to @racket[res]), then
the result of @racket[get-x] would be @racket['(ctc f)].

@ctc-section[#:tag "case-lambda"]{Contracts for @racket[case-lambda]}

Dybvig, in Chapter 5 of the
 @link["http://www.racket.com/csug/"]{Chez Racket User's Guide},
explains the meaning and pragmatics of
@racket[case-lambda] with the following example (among
others):

@racketblock[
(define substring1
  (case-lambda
    [(s) (substring1 s 0 (string-length s))]
    [(s start) (substring1 s start (string-length s))]
    [(s start end) (substring s start end)]))
]
 This version of @racket[substring] has one of the following signature:
@itemize[
@item{just a string, in which case it copies the string;}
@item{a string and an index into the string, in which case it extracts the
 suffix of the string starting at the index; or }
@item{a string a start index and an end index, in which case it extracts the
 fragment of the string between the two indices. }
]

The contract for such a function is formed with the @racket[case->]
 combinator, which combines as many functional contracts as needed: 
@racketblock[
(provide/contract 
  [substring1 
   (case->
    (string? . -> . string?)
    (string? natural-number/c . -> . string?)
    (string? natural-number/c natural-number/c . -> . string?))])
]
 As you can see, the contract for @racket[substring1] combines three
 function contracts, just as many clauses as the explanation of its
 functionality required.

@;{
This isn't supported anymore (yet...?). -robby

In the case of @racket[substring1], we also know that the indices
  that it consumes ought to be natural numbers less than the length of the
  given string. Since @racket[case->] just combines arrow contracts,
  adding such constraints is just a matter of strengthening the individual
  contracts: 
<racket>
(provide/contract 
  [substring1 (case->
               (string? . -> . string?)
               (->r ([s string?]
                     [_ (and/c natural-number/c (</c (string-length s)))])
                 string?)
               (->r ([s string?]
                     [a (and/c natural-number/c (</c (string-length s)))]
                     [o (and/c natural-number/c
                               (>=/c a)
                               (</c (string-length s)))])
                  string?))])
</racket>
  Here we used @racket[->r] to name the parameters and express the
  numeric constraints on them. 
}

@ctc-section[#:tag "multiple"]{Multiple Result Values}

The function @racket[split] consumes a list of @racket[char]s
  and delivers the string that occurs before the first occurrence of
  @racket[#\newline] (if any) and the rest of the list: 
@racketblock[
(define (split l)
  (define (split l w)
    (cond
      [(null? l) (values (list->string (reverse w)) '())]
      [(char=? #\newline (car l))
       (values (list->string (reverse w)) (cdr l))]
      [else (split (cdr l) (cons (car l) w))]))
  (split l '()))
]
  It is a typical multiple-value function, returning two values by
  traversing a single list.

The contract for such a function can use the ordinary
function arrow @racket[->], since it
treats @racket[values] specially, when it appears as the
last result:
@racketblock[
(provide/contract 
 [split (-> (listof char?)
            (values string? (listof char?)))])
]

The contract for such a function can also be written
using @racket[->*], just like @racket[plus]: 
@racketblock[
(provide/contract 
 [split (->* ((listof char?))
             ()
             (values string? (listof char?)))])
]
 As before the contract for the argument is wrapped in an
 extra pair of parentheses (and must always be wrapped like
 that) and the empty pair of parentheses indicates that
 there are no optoinal arguments. The contracts for the
 results are inside @racket[values]: a string and a list of
 characters.

Now suppose we also want to ensure that the first result of
 @racket[split] is a prefix of the given word in list format. In that
 case, we need to use the @racket[->d] contract combinator:
@racketblock[
(define (substring-of? s)
  (flat-named-contract
    (format "substring of ~s" s)
    (lambda (s2)
      (and (string? s2)
           (<= (string-length s2) s)
           (equal? (substring s 0 (string-length s2)) s2)))))

(provide/contract 
 [split (->d ([fl (listof char?)])
             ()
             (values [s (substring-of (list->string fl))]
                     [c (listof char?)]))])
]
 Like @racket[->*], the @racket[->d] combinator uses a function over the
 argument to create the range contracts. Yes, it doesn't just return one
 contract but as many as the function produces values: one contract per
 value.  In this case, the second contract is the same as before, ensuring
 that the second result is a list of @racket[char]s. In contrast, the
 first contract strengthens the old one so that the result is a prefix of
 the given word. 

This contract is expensive to check of course. Here is a slightly
  cheaper version: 
@racketblock[
(provide/contract 
 [split (->d ([fl (listof char?)])
             ()
             (values [s (string-len/c (length fl))]
                     [c (listof char?)]))])
]
  Click on @racket[string-len/c] to see what it does.

@ctc-section[#:tag "no-domain"]{Procedures of Some Fixed, but Statically Unknown Arity}

Imagine yourself writing a contract for a function that accepts some other
function and a list of numbers that eventually applies the former to the
latter. Unless the arity of the given function matches the length of the
given list, your procedure is in trouble. 

Consider this @racket[n-step] function:
@racketblock[
(code:comment "(number ... -> (union #f number?)) (listof number) -> void")
(define (n-step proc inits)
  (let ([inc (apply proc inits)])
    (when inc
      (n-step proc (map (λ (x) (+ x inc)) inits)))))
]

The argument of @racket[n-step] is @racket[proc], a function
@racket[proc] whose results are either numbers or false, and a list. It
then applies @racket[proc] to the list @racket[inits]. As long as
@racket[proc] returns a number, @racket[n-step] treats that number
as an increment for each of the numbers in @racket[inits] and
recurs. When @racket[proc] returns @racket[false], the loop stops.
  
Here are two uses: 
@racketblock[
(code:comment "nat -> nat") 
(define (f x)
  (printf "~s\n" x)
  (if (= x 0) #f -1))
(n-step f '(2))

(code:comment "nat nat -> nat") 
(define (g x y)
  (define z (+ x y))
  (printf "~s\n" (list x y z))
  (if (= z 0) #f -1))
  
(n-step g '(1 1))
]

A contract for @racket[n-step] must specify two aspects of
@racket[proc]'s behavior: its arity must include the number of elements
in @racket[inits], and it must return either a number or
@racket[#f]. The latter is easy, the former is difficult. At first
glance, this appears to suggest a contract that assigns a
@italic{variable-arity} to @racket[proc]: 
@racketblock[
(->* () 
     (listof any/c)
     (or/c number? false/c))
]
This contract, however, says that the function must accept @emph{any}
number of arguments, not a @emph{specific} but
@emph{undetermined} number. Thus, applying @racket[n-step] to
@racket[(lambda (x) x)] and @racket[(list 1)] breaks the contract
because the given function accepts only one argument. 

 The correct contract uses the @racket[unconstrained-domain->]
 combinator, which specifies only the range of a function, not its
 domain. It is then possible to combine this contract with an arity test to
 specify the correct @racket[n-step]'s contract:
@racketblock[
(provide/contract
 [n-step
  (->d ([proc 
         (and/c (unconstrained-domain-> 
                 (or/c false/c number?))
                (λ (f) (procedure-arity-includes? 
                        f 
                        (length inits))))]
        [inits (listof number?)])
       ()
       any)])
]

