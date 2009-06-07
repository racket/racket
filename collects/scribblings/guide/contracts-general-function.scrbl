#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          "contracts-utils.ss"
	  (for-label framework/framework)
          (for-label scheme/contract) 
          (for-label scheme/gui))

@title[#:tag "contracts-general-functions"]{Contracts on Functions in General}

@ctc-section[#:tag "flat-named-contracts"]{Contract Error Messages that Contain ``???''}

You wrote your module. You added contracts. You put them into the interface
so that client programmers have all the information from interfaces. It's a
piece of art: 
@schememod[
scheme

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

@inset-flow{@schemeerror{bank-client broke the contract (-> ??? any)
it had with myaccount on deposit; expected <???>, given: -10}}

Clearly, @scheme[bank-client] is a module that uses @scheme[myaccount]
but what is the @schemeerror{???} doing there?  Wouldn't it be nice if
we had a name for this class of data much like we have string, number,
and so on?

For this situation, PLT Scheme provides @deftech{flat named
contracts}. The use of ``contract'' in this term shows that contracts
are first-class values. The ``flat'' means that the collection of data
is a subset of the built-in atomic classes of data; they are described
by a predicate that consumes all Scheme values and produces a
boolean. The ``named'' part says what we want to do, which is to name
the contract so that error messages become intelligible:

@schememod[
scheme

(define (amount? x) (and (number? x) (integer? x) (>= x 0)))
(define amount (flat-named-contract 'amount amount?))
  
(provide/contract
 [deposit (amount . -> . any)])
  
(define this 0)
(define (deposit a) ...)
]

With this little change, the error message becomes all of the
sudden quite readable:

@inset-flow{@schemeerror{bank-client broke the contract (-> amount
any) it had with myaccount on deposit; expected <amount>, given: -10}}

@ctc-section[#:tag "optional"]{Optional Arguments}

Take a look at this excerpt from a string-processing module, inspired by the
@link["http://schemecookbook.org"]{Scheme cookbook}: 

@schememod[
scheme

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

 The module exports @scheme[string-pad-center], a function
 that creates a string of a given @scheme[width] with the
 given string in the center. The default fill character is
 @scheme[#\space]; if the client module wishes to use a
 different character, it may call @scheme[string-pad-center]
 with a third argument, a @scheme[char], overwriting the
 default.

The function definition uses optional arguments, which is
appropriate for this kind of functionality. The interesting
point here is the formulation of the contract for the
@scheme[string-pad-center].


The contract combinator @scheme[->*], demands several groups of contracts: 
@itemize[
@item{The first one is a parenthesized group of contracts for all required
arguments. In this example, we see two: @scheme[string?] and
@scheme[natural-number/c]. }

@item{The second one is a parenthesized group of contracts for all optional
arguments: @scheme[char?]. }

@item{The last one is a single contract: the result of the function.}
]

 Note if a default value does not satisfy a contract, you
 won't get a contract error for this interface. In contrast
 to type systems, we do trust you; if you can't trust
 yourself, you need to communicate across boundaries for
 everything you write.

@ctc-section[#:tag "rest-args"]{Rest Arguments}

We all know that @scheme[+] in Beginner Scheme is a function
  that consumes at least two numbers but, in principle,
  arbitrarily many more. Defining the function is easy:
@schemeblock[
(define (plus fst snd . rst)
  (foldr + (+ fst snd) rst))
]
Describing this function via a contract is difficult because of the rest
argument (@scheme[rst]). 

Here is the contract:
@schemeblock[
(provide/contract 
 [plus (->* (number? number?) () #:rest (listof number?) number?)])
]
The @scheme[->*] contract combinator empowers you to specify
  functions that consume a variable number of arguments or functions like
  @scheme[plus], which consume ``at least this number'' of arguments but
  an arbitrary number of additional arguments. 

The contracts for the required arguments are enclosed in the first
pair of parentheses: 
@schemeblock[
(number? number?)
]

For @scheme[plus] they demand two numbers. The empty pair of
parenthesis indicates that there are no optional arguments
(not counting the rest arguments) and the contract for the
rest argument follows @scheme[#:rest]
@schemeblock[
(listof number?)
]
  Since the remainder of the actual arguments are collected
  in a list for a rest parameter such as @scheme[rst], the
  contract demands a list of values; in this specific
  examples, these values must be numbers.

@ctc-section[#:tag "keywords"]{Keyword Arguments}

Sometimes, a function accepts many arguments and remembering
their order can be a nightmare. To help with such functions,
PLT Scheme has @seclink["lambda-keywords"]{keyword} arguments. 

For example, consider this function that creates a simple
GUI and asks the user a yes-or-no question:
@schememod[
scheme/gui

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
@scheme[message-box/custom] (and generally speaking,
avoiding the responses ``yes'' and ``no'' in your dialog is a
good idea, too ...).}

The contract for @scheme[ask-yes-or-no-question] uses our
old friend the @scheme[->] contract combinator. Just like
@scheme[lambda] (or @scheme[define]-based functions) use
keywords for specifying keyword arguments, it uses keywords
for specifying contracts on keyword arguments. In this case,
it says that @scheme[ask-yes-or-no-question] must receive
five keyword arguments, one for each of the keywords
@scheme[#:question],
@scheme[#:default],
@scheme[#:title],
@scheme[#:width], and
@scheme[#:height]. 
Also, just like in a function definition, the keywords in
the @scheme[->] may appear in any order.

@ctc-section[#:tag "optional-keywords"]{Optional Keyword Arguments}

Of course, many of the parameters in
@scheme[ask-yes-or-no-question] (from the previous question)
have reasonable defaults, and should be made optional:
@schemeblock[
(define (ask-yes-or-no-question #:question question 
                                #:default answer
                                #:title [title "Yes or No?"]
                                #:width [w 400]
                                #:height [h 200])
  ...)
]

To specify this function's contract, we need to use
@scheme[->*]. It too supports keywords just as you might
expect, in both the optional and mandatory argument
sections. In this case, we have mandatory keywords
@scheme[#:question] and @scheme[#:default], and optional keywords 
@scheme[#:title],
@scheme[#:width], and
@scheme[#:height]. So, we write the contract like this:
@schemeblock[
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

@schememod[
scheme
(provide/contract 
 [sqrt.v1 (->d ([argument (>=/c 1)])
               ()
               [result (<=/c argument)])])
...
]

The contract for the exported function @scheme[sqrt.v1] uses the
@scheme[->d] rather than @scheme[->] function contract. The ``d''
stands for @italic{dependent} contract, meaning the contract for the
function range depends on the value of the argument. 

In this particular case, the argument of @scheme[sqrt.v1] is greater
or equal to 1. Hence a very basic correctness check is that the result is
smaller than the argument. (Naturally, if this function is critical, one
could strengthen this check with additional clauses.)

In general, a dependent function contract looks just like
the more general @scheme[->*] contract, but with names added
that can be used elsewhere in the contract.

Yes, there are many other contract combinators such as @scheme[<=/c]
and @scheme[>=/c], and it pays off to look them up in the contract
section of the reference manual. They simplify contracts tremendously
and make them more accessible to potential clients. 

@ctc-section[#:tag "arrow-d-args"]{When Contract Arguments Depend on Each Other}

Eventually bank customers want their money back. Hence, a module that
implements a bank account must include a method for withdrawing money. Of
course, ordinary accounts don't let customers withdraw an arbitrary amount of
money but only as much as they have in the account. 

Suppose the account module provides the following two functions:
@schemeblock[
balance : (-> account amount)
withdraw : (-> account amount account)
]
Then, informally, the proper precondition for @scheme[withdraw] is that 
``the balance of the given account is greater than or equal to the given (withdrawal) amount.''
The postcondition is similar to the one for
@ctc-link["flat-named-contracts"]{@scheme[deposit]}:
``the balance of the resulting account is larger than (or equal to) the one of the
given account.''
You could of course also formulate a full-fledged correctness condition, namely,
that the balance of the resulting account is equal to the balance of the given
one, plus the given amount.

The following module implements accounts imperatively and specifies the
conditions we just discussed:
@schememod[
scheme

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
@item{@scheme[create] consumes an initial deposit and
produces an account. This kind of contract is just like a
type in a statically typed language, except that statically
typed languages usually don't support the type ``natural
numbers'' (as a full-fledged subtype of numbers). }

@item{@scheme[balance] consumes an account and computes its current balance.}

@item{@scheme[withdraw] consumes an account, named @scheme[acc], and an
amount, @scheme[amt]. In addition to being an @scheme[amount], the
latter must also be less than @scheme[(balance acc)], i.e., the balance of
the given account. That is, the contract for @scheme[amt] depends on the
value of @scheme[acc], which is what the @scheme[->d] 
contract combinator expresses.

The result contract is formed on the fly: 
@scheme[(mk-account-contract acc amt > msg>)]. 
It is an application of a contract-producing function that
consumes an account, an amount, a comparison operator, and an error message (a
format string). The result is a contract. 
}

@item{@scheme[deposit]'s contract has been reformulated using the
@scheme[->d] combinator. }
]

The code in the first section defines all those pieces that
are needed for the formulation of the export contracts:
@scheme[account?], @scheme[amount], error messages (format
strings), and @scheme[mk-account-contract]. The latter is a
function that extracts the current balance from the given
account and then returns a named contract, whose error
message (contract name) is a string that refers to this
balance. The resulting contract checks whether an account
has a balance that is larger or smaller, depending on the
given comparison operator, than the original balance.

@ctc-section[#:tag "arrow-d-eval-order"]{Ensuring that a Function Properly Modifies State}

The @scheme[->d] contract combinator can also ensure that a
function only modifies state according to certain
constraints. For example, consider this contract
(it is a slightly simplified from the function
@scheme[preferences:add-panel] in the framework):
@schemeblock[
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
@scheme[parent], and that @scheme[parent] must be
an object matching the interface @scheme[area-container-window<%>].

The range contract ensures that the function only modifies
the children of @scheme[parent] by adding a new child to the
front of the list. It accomplishes this by using the
@scheme[_] instead of a normal identifier, which tells the
contract library that the range contract does not depend on
the values of any of the results, and thus the contract
library evaluates the expression following the @scheme[_]
when the function is called, instead of when it
returns. Therefore the call to the @scheme[get-children] method
happens before the function under the contract is called.
When the function under contract returns, its result is
passed in as @scheme[child], and the contract ensures that
the children after the function return are the same as the
children before the function called, but with one more
child, at the front of the list.

To see the difference in a toy example that focuses
on this point, consider this program
@schememod[
scheme
(define x '())
(define (get-x) x)
(define (f) (set! x (cons 'f x)))
(provide/contract 
 [f (->d () () [_ (begin (set! x (cons 'ctc x)) any/c)])]
 [get-x (-> (listof symbol?))])
]
If you were to require this module, call @scheme[f], then
the result of @scheme[get-x] would be @scheme['(f ctc)]. In
contrast, if the contract for @scheme[f] were
@schemeblock[(->d () () [res (begin (set! x (cons 'ctc x)) any/c)])]
(only changing the underscore to @scheme[res]), then
the result of @scheme[get-x] would be @scheme['(ctc f)].

@ctc-section[#:tag "case-lambda"]{Contracts for @scheme[case-lambda]}

Dybvig, in Chapter 5 of the
 @link["http://www.scheme.com/csug/"]{Chez Scheme User's Guide},
explains the meaning and pragmatics of
@scheme[case-lambda] with the following example (among
others):

@schemeblock[
(define substring1
  (case-lambda
    [(s) (substring1 s 0 (string-length s))]
    [(s start) (substring1 s start (string-length s))]
    [(s start end) (substring s start end)]))
]
 This version of @scheme[substring] has one of the following signature:
@itemize[
@item{just a string, in which case it copies the string;}
@item{a string and an index into the string, in which case it extracts the
 suffix of the string starting at the index; or }
@item{a string a start index and an end index, in which case it extracts the
 fragment of the string between the two indices. }
]

The contract for such a function is formed with the @scheme[case->]
 combinator, which combines as many functional contracts as needed: 
@schemeblock[
(provide/contract 
  [substring1 
   (case->
    (string? . -> . string?)
    (string? natural-number/c . -> . string?)
    (string? natural-number/c natural-number/c . -> . string?))])
]
 As you can see, the contract for @scheme[substring1] combines three
 function contracts, just as many clauses as the explanation of its
 functionality required.

@;{
This isn't supported anymore (yet...?). -robby

In the case of @scheme[substring1], we also know that the indices
  that it consumes ought to be natural numbers less than the length of the
  given string. Since @scheme[case->] just combines arrow contracts,
  adding such constraints is just a matter of strengthening the individual
  contracts: 
<scheme>
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
</scheme>
  Here we used @scheme[->r] to name the parameters and express the
  numeric constraints on them. 
}

@ctc-section[#:tag "multiple"]{Multiple Result Values}

The function @scheme[split] consumes a list of @scheme[char]s
  and delivers the string that occurs before the first occurrence of
  @scheme[#\newline] (if any) and the rest of the list: 
@schemeblock[
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
function arrow @scheme[->], since it
treats @scheme[values] specially, when it appears as the
last result:
@schemeblock[
(provide/contract 
 [split (-> (listof char?)
            (values string? (listof char?)))])
]

The contract for such a function can also be written
using @scheme[->*], just like @scheme[plus]: 
@schemeblock[
(provide/contract 
 [split (->* ((listof char?))
             ()
             (values string? (listof char?)))])
]
 As before the contract for the argument is wrapped in an
 extra pair of parentheses (and must always be wrapped like
 that) and the empty pair of parentheses indicates that
 there are no optoinal arguments. The contracts for the
 results are inside @scheme[values]: a string and a list of
 characters.

Now suppose we also want to ensure that the first result of
 @scheme[split] is a prefix of the given word in list format. In that
 case, we need to use the @scheme[->d] contract combinator:
@schemeblock[
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
 Like @scheme[->*], the @scheme[->d] combinator uses a function over the
 argument to create the range contracts. Yes, it doesn't just return one
 contract but as many as the function produces values: one contract per
 value.  In this case, the second contract is the same as before, ensuring
 that the second result is a list of @scheme[char]s. In contrast, the
 first contract strengthens the old one so that the result is a prefix of
 the given word. 

This contract is expensive to check of course. Here is a slightly
  cheaper version: 
@schemeblock[
(provide/contract 
 [split (->d ([fl (listof char?)])
             ()
             (values [s (string-len/c (length fl))]
                     [c (listof char?)]))])
]
  Click on @scheme[string-len/c] to see what it does.

@ctc-section[#:tag "no-domain"]{Procedures of Some Fixed, but Statically Unknown Arity}

Imagine yourself writing a contract for a function that accepts some other
function and a list of numbers that eventually applies the former to the
latter. Unless the arity of the given function matches the length of the
given list, your procedure is in trouble. 

Consider this @scheme[n-step] function:
@schemeblock[
(code:comment "(number ... -> (union #f number?)) (listof number) -> void")
(define (n-step proc inits)
  (let ([inc (apply proc inits)])
    (when inc
      (n-step proc (map (λ (x) (+ x inc)) inits)))))
]

The argument of @scheme[n-step] is @scheme[proc], a function
@scheme[proc] whose results are either numbers or false, and a list. It
then applies @scheme[proc] to the list @scheme[inits]. As long as
@scheme[proc] returns a number, @scheme[n-step] treats that number
as an increment for each of the numbers in @scheme[inits] and
recurs. When @scheme[proc] returns @scheme[false], the loop stops.
  
Here are two uses: 
@schemeblock[
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

A contract for @scheme[n-step] must specify two aspects of
@scheme[proc]'s behavior: its arity must include the number of elements
in @scheme[inits], and it must return either a number or
@scheme[#f]. The latter is easy, the former is difficult. At first
glance, this appears to suggest a contract that assigns a
@italic{variable-arity} to @scheme[proc]: 
@schemeblock[
(->* () 
     (listof any/c)
     (or/c number? false/c))
]
This contract, however, says that the function must accept @emph{any}
number of arguments, not a @emph{specific} but
@emph{undetermined} number. Thus, applying @scheme[n-step] to
@scheme[(lambda (x) x)] and @scheme[(list 1)] breaks the contract
because the given function accepts only one argument. 

 The correct contract uses the @scheme[unconstrained-domain->]
 combinator, which specifies only the range of a function, not its
 domain. It is then possible to combine this contract with an arity test to
 specify the correct @scheme[n-step]'s contract:
@schemeblock[
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

