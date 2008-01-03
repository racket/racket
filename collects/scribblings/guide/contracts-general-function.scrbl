#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]
@require["contracts-utils.ss"]
@(require (for-label scheme/contract))

<section title="Contracts on Functions in General" tag="genfunc" />

<question title="Why do the error messages contain "..."?" tag="flat-named-contract">

<p>You wrote your module. You added contracts. You put them into the interface
so that client programmers have all the information from interfaces. It's a
piece of art: 
<scheme>
(module myaccount mzscheme
  (require (lib "contract.ss"))

  (provide/contract
   [deposit (-> (lambda (x)
		  (and (number? x) (integer? x) (>= x 0)))
		any)])
  
  (define this 0)
  (define (deposit a) ...))
</scheme>

Several clients used your module. Others used their modules. And all of a sudden
one of them sees this error message: 

<blockquote>
<pre><font color="red">
bank-client broke the contract (-> ??? any)
it had with myaccount on deposit; 
expected &lt;???>, given: -10
</font></pre>
</blockquote>

Clearly, @scheme[bank-client] is a module that uses @scheme[myaccount]
but what are the '?' doing there? Wouldn't it be nice if we had a name for this
class of data much like we have string, number, and so on? 

<p>For this situation, PLT Scheme provides "flat named contracts". The use of
"contract" shows that contracts are first-class values. The "flat" means that
the collection of data is a subset of the built-in atomic classes of data; they
are described by a predicate that consumes all Scheme values and produces a
boolean. The "named" part says what we want to do: name the contract so that
error messages become intelligible: 

<scheme>
(module myaccount mzscheme
  (require (lib "contract.ss"))

  (define (amount? x) (and (number? x) (integer? x) (>= x 0)))
  (define amount (flat-named-contract 'amount amount?))
  
  (provide/contract
   [deposit (amount . -> . any)])
  
  (define this 0)
  (define (deposit a) ...))
</scheme>

With this little change, the error message becomes all of sudden quite readable: 

<blockquote>
<pre><font color="red">
bank-client broke the contract (-> amount any)
it had with myaccount on deposit; 
expected &lt;amount>, given: -10
</font></pre>
</blockquote>

</question>

<question title="Can a contract specify that the result depends on the arguments?" tag="arrow-d">

<p>Here is an excerpt from an imaginary (pardon the pun) <a
name="numerics-module">numerics module</a>:

<scheme>
(module numerics mzscheme 
  (require (lib "contract.ss"))

  (provide/contract 
   [sqrt.v1 (->d (lambda (x) (>= x 1))
                 (lambda (argument) 
                   (lambda (result)
                     (<= result argument))))]
    ...)
 ...)  
</scheme>

The contract for the exported function @scheme[sqrt.v1] uses the
@scheme[->d] rather than @scheme[->] function contract. The "d"
stands for <em>dependent</em> contract, meaning the contract for the
function range depends on the value of the argument. 

<p>In this particular case, the argument of @scheme[sqrt.v1] is greater
or equal to 1. Hence a very basic correctness check is that the result is
smaller than the argument. [Naturally, if this function is critical, one
could strengthen this check with additional clauses.]

<p>In general, a dependent function contract checks the argument, then
applies the function in the range position to the argument, and uses the
result of this application (usually a closure) as the range check. 

<p>Let's look at a second example to see how this closure creating business
works so well here. Take a look at the following module, which exports
(among other things) a @scheme[deposit] function for a bank
account. Whether the programmer implements this bank account imperatively
or functionally, the balance of the account should increase when a customer
deposits money: 

<a name="deposit" />
<scheme>
(module account mzscheme 
  (require (lib "contract.ss"))

  (define-struct account (balance))
  (define amount natural-number/c)

  (provide/contract
   ;; create an account with the given initial deposit 
   [create (amount . -> . account?)]

   ;; what is the current balance of the account?
   [balance (account . -> . amount)]

   ;; deposit the given amount into the given account 
   ;; account? amount -> account?
   [deposit (->d account? 
                 amount
                 (lambda (account amount)
                   (let ([balance0 (balance account)])
                     (lambda (result)
                       (and (account? result) 
                            (<= balance0 (balance result)))))))]
    ... )
  
  ...
  )
</scheme>

<p>The module implements the creation of accounts, balance checks, account
deposits, and many other functions presumably. In principle, the
@scheme[deposit] function consumes an account and an amount, adds the
amount to the account, and returns the (possibly modified) account. 

<p>To ensure the increase in the account's balance, the contract is a
dependent contract. The function on the right of the @scheme[->d]
contract combinator consumes the two argument values: an account and an
amount. It then returns a function that checks @scheme[deposit]'s
result. In this case, it checks whether the result is an account and
whether @scheme[balance0] is smaller than the account's balance. Note
that @scheme[balance0] is the value that @scheme[balance] extracted
from the given account <em>before</em> the range-checking function was
produced. 

</question>

<question title="See: currying would help with contracts!" tag="curry">

<p>Exactly!

<p>The contract for @scheme[sqrt.v1] suggests that curried versions of
the numeric comparison operators would come in handy for defining contracts
and combining them with contract combinators such as @scheme[->d]. 

<p>Here is a revision of <a href="#numerics-module">the
@scheme[numerics] module</a> that exploits these special contract
combinators: 

<scheme>
(module numerics mzscheme 
  (require (lib "contract.ss"))

  (provide/contract 
   [sqrt.2 (->d (>=/c 1.0) <=/c)]
   ...)
 ...)  
</scheme>

The combinator @scheme[>=/c] consumes a number @scheme[x] and
produces a contract. This contract also consumes a second number, say
@scheme[y], and makes sure that @scheme[(>= y x)] holds. The
contract combinator @scheme[<=/c] works in an analogous fashion. 

<p>Thus in the above example, the contract for @scheme[sqrt.v2] makes
sure that its argument is greater or equal to 1.0 and that its result is
less than or equal to its argument. 

<p>Yes, there are many other contract combinators such as @scheme[<=/c]
and @scheme[>=/c], and it pays off to look them up in the contract
report section (of the mzlib manual). They simplify contracts tremendously
and make them more accessible to potential clients. 

</question>

<question title="Can a contract specify that arguments depend on each other?" tag="arrow-r">

<p>Eventually bank customers want their money back. Hence, a module that
implements a bank account must include a method for withdrawing money. Of
course, ordinary accounts don't let customers withdraw an arbitrary amount of
money but only as much as they have in the account. 

<p>Suppose the account module provides the following two functions:
<scheme>
;; balance : Account -> Amount
;; withdraw : Account Amount -> Account 
</scheme>
Then, informally, the proper precondition for @scheme[withdraw] is that 
<blockquote>
"the balance of the given account is greater than or equal to the given (withdrawal) amount."
</blockquote>
The postcondition is similar to the one for <a
href="#deposit">@scheme[deposit]</a>: 
<blockquote>
"the balance of the resulting account is larger than (or equal to) than the one of the
given account." 
</blockquote>
You could of course also formulate a full-fledged correctness condition, namely,
that the balance of the resulting account is equal to the balance of the given
one, plus the given amount. 

<p>The following module implements accounts imperatively and specifies the
conditions we just discussed: 
<table>
<tr><td>
<scheme>
(module account mzscheme 
  (require (lib "contract.ss"))
<font color="deeppurple">  
  (define-struct account (balance))
  (define amount natural-number/c)
  
  (define msg> 
    "account a with balance larger than ~a expected")
  (define msg< 
    "account a with balance less than ~a expected")

  (define (mk-account-contract acc amt op msg)
    (define balance0 (balance acc))
    (define (ctr a)
      (and (account? a) (op balance0 (balance a))))
    (flat-named-contract (format msg balance0) ctr))</font>
<font color="purple">  
  (provide/contract 
   [create   (amount . -> . account?)]
   [balance  (account? . -> . amount)]
   [withdraw (->r ([acc account?] 
                   [amt (and/c amount (&lt;/c (balance acc)))])
               (mk-account-contract acc amt > msg>))]
   [deposit  (->r ([acc account?] 
                   [amt amount])
                (mk-account-contract acc amt < msg<))])
</font>                     
  (define balance account-balance)
  
  (define (create amt) (make-account amt))
  
  (define (withdraw acc amt)
    (set-account-balance! acc (- (balance acc) amt))
    acc)
  
  (define (deposit acc amt)
    (set-account-balance! acc (+ (balance acc) amt))
    acc))
</scheme>
<td bgcolor="beige" valign="top">
<pre>


the contract definitions 













the exports 










the function definitions 

</pre>
</table>

The purple part is the export interface: 
<ol>
<li>@scheme[create] consumes an initial deposit and produces an
account. This kind of contract is just like a type in a statically typed
language, except that statically typed languages usually don't support the type
"natural numbers" (as a full-fledged subtype of numbers). 
</li>

<li>@scheme[balance] consumes an account and computes its current balance. 
</li>

<li>@scheme[withdraw] consumes an account, named @scheme[acc], and an
amount, @scheme[amt]. In addition to being an @scheme[amount], the
latter must also be less than @scheme[(balance acc)], i.e., the balance of
the given account. That is, the contract for @scheme[amt] depends on the
value of @scheme[acc], which is what the @scheme[->r] (r for recursive)
contract combinator expresses.

<p>The result contract is formed on the fly: @scheme[(mk-account-contract acc amt
> msg>)]. It is an application of a contract-producing function that
consumes an account, an amount, a comparison operator, and an error message (a
format string). The result is a contract. 
</li>

<li>@scheme[deposit]'s contract has been reformulated using the
@scheme[->r] combinator. Strictly speaking, this isn't necessary and the use
of @scheme[->d] would suffice, because the contracts for the arguments do
not depend on each other. 
</li>
</ol>

The code in deep purple defines all those pieces that are needed for the
formulation of the export contracts: @scheme[account?], @scheme[amount],
error messages (format strings), and @scheme[mk-account-contract]. The
latter is a function that extracts the current balance from the given account
and then returns a named contract, whose error message (contract name) is a
string that refers to this balance. The resulting contract checks whether an
account has a balance that is larger or smaller, depending on the given
comparison operator, than the original balance. 

</question>

<question title="What about rest arguments?" tag="rest-args">

<p>We all know that @scheme[+] in Beginner Scheme is a function that
  consumes at least two numbers but, in principle, arbitrary
  manner. Defining the function is easy: 
<scheme>
(define (plus fst snd . rst)
  (foldr + (+ fst snd) rst))
</scheme>
  Describing this function via a contract is difficult because of the rest
  argument (@scheme[rst]). 

<p>Here is the contract:
<scheme>
(provide/contract 
 [plus (->* (number? number?) (listof number?) (number?))])
</scheme>
  The @scheme[->*] contract combinator empowers you to specify
  functions that consume a variable number of arguments or functions like
  @scheme[plus], which consume "at least this number" of arguments but
  an arbitrary number of additional arguments. 

<p>The contracts for the required arguments are enclosed in an additional
  pair of parentheses: 
<scheme>
(number? number?)
</scheme>
  For @scheme[plus] they demand two numbers. The contract for the
  rest argument follows: 
<scheme>
(listof number?)
</scheme>
  Since the remainder of the actual arguments are collected in a list for
  a @scheme[rest] parameter such as @scheme[rst], the contract
  demands a list of values; in this specific examples, these values must be
  number. 

<p>Finally, you may have noticed that the contract for the function range
  of @scheme[plus] is also wrapped in a pair of parentheses. The reason
  for those is that functions can return multiple values not just one, and
  this is our next topic in this guide. 
</question>

<question title="What about case-lambda?" tag="case-lambda">

<p><a href="http://www.scheme.com/csug/binding.html">Dybvig</a> explains
  the meaning and pragmatics of @scheme[case-lambda] with the following
  example (among others): 

<scheme>
(define substring1
  (case-lambda
    [(s) (substring1 s 0 (string-length s))]
    [(s start) (substring1 s start (string-length s))]
    [(s start end) (substring s start end)]))
</scheme>
 This version of @scheme[substring] has one of the following signature:
<ol>
 <li>just a string, in which case it copies the string; 
 <li>a string and an index into the string, in which case it extracts the
 suffix of the string starting at the index; or 
 <li>a string a start index and an end index, in which case it extracts the
 fragment of the string between the two indices. 
</ol> 

<p>The contract for such a function is formed with the @scheme[case->]
 combinator, which combines as many functional contracts as needed: 
<scheme>
(provide/contract 
  [substring1 (case->
               (string? . -> . string?)
               (string? natural-number/c . -> . string?)
               (string? natural-number/c natural-number/c . -> . string?))])
</scheme>
 As you can see, the contract for @scheme[substring1] combines three
 function contracts, just as many clauses as the explanation of its
 functionality required.

<p>In the case of @scheme[substring1], we also know that the indices
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
</question>


<question title="What about multiple values?" tag="multiple">

<p>The function @scheme[split] consumes a list of @scheme[char]s
  and delivers the string that occurs before the first occurrence of
  @scheme[#\newline] (if any) and the rest of the list: 
<scheme>
(define (split l)
  (define (split l w)
    (cond
      [(null? l) (values (list->string (reverse w)) '())]
      [(char=? #\newline (car l)) (values (list->string (reverse w)) (cdr l))]
      [else (split (cdr l) (cons (car l) w))]))
  (split l '()))
</scheme>
  It is a typical multiple-value function, returning two values by
  traversing a single list.

<p>
The contract for such a function can use the ordinary
function arrow @scheme[->], since it
treats @scheme[values] specially, when it appears as the
last result:
<scheme>
(provide/contract 
 [split (-> (listof char?)
            (values string? (listof char?)))])
</scheme>
</p>

<p>The contract for such a function can also be written
using @scheme[->*], just like
 @scheme[plus]: 
<scheme>
(provide/contract 
 [split (->* ((listof char?))
             (string? (listof char?)))])
</scheme>
 As before the contract for the single argument is wrapped
 in an extra pair of parentheses (and must always be wrapped
 like that); so are the contracts for the results: a string
 and a list of characters.
</p>

<p>Now suppose we also want to ensure that the first result of
 @scheme[split] is a prefix of the given word in list format. In that
 case, we need to combine the @scheme[->*] contract combinator with the
 @scheme[->d] combinator, which is of course the @scheme[->d*]
 combinator: 
<scheme>
(provide/contract 
 [splitp (->d* ((listof char?)) 
               (lambda (fl) 
                 (define wd (list->string fl))
                 (values (and/c string? (lambda (w) (string<=? w wd)))
                         (listof char?))))])
</scheme>
 Like @scheme[->d], the new combinator uses a function over the
 argument to create the range contracts. Yes, it doesn't just return one
 contract but as many as the function produces values: one contract per
 value.  In this case, the second contract is the same as before, ensuring
 that the second result is a list of @scheme[char]s. In contrast, the
 first contract strengthens the old one so that the result is a prefix of
 the given word. 

<p>This contract is expensive to check of course. Here is a slightly
  cheaper version: 
<scheme>
(provide/contract 
 [splitl (->d* ((listof char?)) 
               (lambda (fl) 
                 (values (and/c string? (string/len (add1 (length fl))))
                         (listof char?))))])
</scheme>
  Check the help desk for an explanation of @scheme[string/len]. 

</question>

<question title="What about procedures of any specific arity?" tag="no-domain">
<p>
Imagine yourself writing a contract for a function that accepts some other
function and a list of numbers that eventually applies the former to the
latter. Unless the arity of the given function matches the length of the
given list, your procedure is in trouble. 
</p>


<p>Consider this @scheme[n-step] function:

<scheme>
;; (Number ... -> (union #f number?)) (listof Number) -> Void
(define (n-step proc inits)
  (let ([inc (apply proc inits)])
    (when inc
      (n-step proc (map (λ (x) (+ x inc)) inits)))))
</scheme>

The argument of @scheme[n-step] is @scheme[proc], a function
@scheme[proc] whose results are either numbers or false, and a list. It
then applies @scheme[proc] to the list @scheme[inits]. As long as
@scheme[proc] returns a number, @scheme[n-step] treats that number
as an increment for each of the numbers in @scheme[inits] and
recurs. When @scheme[proc] returns @scheme[false], the loop stops.
</p>
  
Here are two uses: 
<table>
<tr>
<td width="20" />
<td valign="top">
<pre>@scheme[
;; Nat -> Nat 
(define (f x)
  (printf "~s \n" x)
  (if (= x 0) #f -1))
  
(n-step f '(2))
]</pre></td>
<td width="150" />
<td valign="top" width="150"><pre>@scheme[
;; Nat Nat -> Nat 
(define (g x y)
  (define z (+ x y))
  (printf "~s\n" (list x y z))
  (if (= z 0) #f -1))
  
(n-step g '(1 1))
]</pre></td></tr>
</table>
</center>

<p>A contract for @scheme[n-step] must specify two aspects of
@scheme[proc]'s behavior: its arity must include the number of elements
in @scheme[inits], and it must return either a number of
@scheme[#f]. The latter is easy, the former is difficult. At first
glance, this appears to suggest a contract that assigns a
<em>variable-arity</em> to @scheme[proc]: 
@scheme[
(->* () 
     (listof any/c)
     (or/c number? false/c))
]
This contract, however, says that the function must accept <em>any</em>
number of arguments, not a <em>specific</em> but
<em>undetermined</em> number. Thus, applying @scheme[n-step] to
@scheme[(lambda (x) x)] and @scheme[(list 1)] breaks the contract
because the given function accepts only one argument. 
</p>

<p>
 The correct contract uses the @scheme[unconstrained-domain->]
 combinator, which specifies only the range of a function, not its
 domain. It is then possible to combine this contract with an arity test to
 specify the correct @scheme[n-step]'s contract:
<scheme>
(provide/contract
 [n-step
  (->r ([proc (and/c (unconstrained-domain-> (or/c false/c number?))
                     (λ (f) (procedure-arity-includes? f (length inits))))]
        [inits (listof number?)])
       any)])
</scheme>
</p>

</question>

<question title="What about opt-lambda?" tag="opt-lambda">

<p>Take a look at this excerpt from a string-processing module, inspired by the
Scheme cookbook: 
<scheme>
(module string-pad mzscheme 
  (require (lib "contract.ss") (lib "etc.ss") (lib "string.ss" "srfi" "13"))
  
  (provide/contract
    ;; pad the given str left and right with
    ;; the (optional) char so that it is centered
    [string-pad-center (opt-> (string? natural-number/c) (char?) 
                              string?)])

  (define string-pad-center
    (opt-lambda (str width [pad #\space])
      (define field-width (min width (string-length str)))
      (define rmargin (- width (floor (/ (- width field-width) 2))))
      (string-pad (string-pad-right str rmargin pad) width pad))))
</scheme>

 The module exports @scheme[string-pad-center], a function that creates a
 string of a given @scheme[width] with the given string in the center. The
 default fill character is @scheme[#\space]; if the client module requires
 different character, it may call @scheme[string-pad-center] with a third
 argument, a @scheme[char], overwriting the default. 

<p>The function definition uses @scheme[opt-lambda], which is appropriate
for this kind of functionality. The interesting point here is the formulation of
the contract for @scheme[opt-lambda]. Like the contract combinator for
@scheme[->*], i.e., rest arguments, @scheme[opt->] demands several
groups of contracts: 
<ol>
<li>The first one is a parenthesized group of contracts for all required
arguments. In this example, we see two: @scheme[string?] and
@scheme[natural-number/c]. 

<li>The second one is a parenthesized group of contracts for all optional
arguments: @scheme[char?]. 

<li>The last one is a single contract: the result of the function. 
</ol>
 Note if a default value does not satisfy a contract, you won't get a contract
 error for this interface. We do trust <em>you</em>; if you can't trust
 yourself, you need to communicate across boundaries for everything you write. 

The contract library does not have built-in combinators to
specify richer contracts for functions that have optional
arguments, like functions that have optional arguments where
the arguments depend on each other.

To specify such contracts combine @scheme[case->] with
the other function contract combinators, like we did in
the @scheme[substring1] function above.

