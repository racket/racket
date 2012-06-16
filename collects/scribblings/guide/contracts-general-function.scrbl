#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt" "contracts-utils.rkt"
          (for-label framework/framework racket/contract racket/gui))

@title[#:tag "contracts-general-functions"]{Contracts on Functions in General}

The @racket[->] contract constructor works for functions that take a
fixed number of arguments and where the result contract is independent
of the input arguments. To support other kinds of functions, Racket
supplies additional contract constructors, notably @racket[->*] and 
@racket[->i].

@ctc-section[#:tag "optional"]{Optional Arguments}

Take a look at this excerpt from a string-processing module, inspired by the
@link["http://schemecookbook.org"]{Scheme cookbook}: 

@racketmod[
racket

(provide
 (contract-out
  (code:comment @#,t{pad the given str left and right with})
  (code:comment @#,t{the (optional) char so that it is centered})
  [string-pad-center (->* (string? natural-number/c)
                          (char?) 
                          string?)]))

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

 Note if a default value does not satisfy a contract, you won't get a
 contract error for this interface. If you can't trust yourself to get
 the initial value right, you need to communicate the initial value
 across a boundary.

@ctc-section[#:tag "rest-args"]{Rest Arguments}

The @racket[max] operator consumes at least one real number, but it
 accepts any number of additional arguments. You can write other such
 functions using a ``rest'' argument, such as in @racket[max-abs]:

@margin-note{See @secref["rest-args"] for an introduction to rest
arguments.}

@racketblock[
(define (max-abs n . rst)
  (foldr (lambda (n m) (max (abs n) m)) (abs n) rst))
]

Describing this function through a contract requires a further
extension of @racket[->*]: a @racket[#:rest] keyword specifies a
contract on a list of arguments after the required and optional
arguments:

@racketblock[
(provide
 (contract-out
  [max-abs (->* (real?) () #:rest (listof real?) real?)]))
]

As always for @racket[->*], the contracts for the required arguments
are enclosed in the first pair of parentheses, which in this case is a
single real number. The empty pair of parenthesis indicates that there
are no optional arguments (not counting the rest arguments). The
contract for the rest argument follows @racket[#:rest]; since all
additional arguments must be real numbers, the list of rest arguments
must satisfy the contract @racket[(listof real?)].


@ctc-section[#:tag "keywords"]{Keyword Arguments}

It turns out that the @racket[->] contract constructor also contains
support for keyword arguments. For example, consider this function,
which creates a simple GUI and asks the user a yes-or-no question:

@margin-note{See @secref["lambda-keywords"] for an introduction to
keyword arguments.}

@racketmod[
racket/gui

(define (ask-yes-or-no-question question 
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

(provide (contract-out
          [ask-yes-or-no-question
           (-> string?
               #:default boolean?
               #:title string?
               #:width exact-integer?
               #:height exact-integer?
               boolean?)]))
]

@margin-note{If you really want to ask a yes-or-no question
via a GUI, you should use @racket[message-box/custom]. For that
matter, it's usually better to provide buttons with more specific
answers than ``yes'' and ``no.''}

The contract for @racket[ask-yes-or-no-question] uses @racket[->], and
in the same way that @racket[lambda] (or @racket[define]-based
functions) allows a keyword to precede a functions formal argument,
@racket[->] allows a keyword to precede a function contract's argument
contract. In this case,
the contract says that @racket[ask-yes-or-no-question] must receive four keyword
arguments, one for each of the keywords
@racket[#:default],
@racket[#:title],
@racket[#:width], and
@racket[#:height]. 
As in a function definition, the order of the keywords in @racket[->]
relative to each other does not matter for clients of the function;
only the relative order of argument contracts without keywords
matters.

@ctc-section[#:tag "optional-keywords"]{Optional Keyword Arguments}

Of course, many of the parameters in
@racket[ask-yes-or-no-question] (from the previous question)
have reasonable defaults and should be made optional:

@racketblock[
(define (ask-yes-or-no-question question 
                                #:default answer
                                #:title [title "Yes or No?"]
                                #:width [w 400]
                                #:height [h 200])
  ...)
]

To specify this function's contract, we need to use
@racket[->*] again. It supports keywords just as you might
expect in both the optional and mandatory argument
sections. In this case, we have the mandatory keyword
@racket[#:default] and optional keywords
@racket[#:title],
@racket[#:width], and
@racket[#:height]. So, we write the contract like this:

@racketblock[
(provide (contract-out
          [ask-yes-or-no-question
           (->* (string?
                 #:default boolean?)
                (#:title string?
                 #:width exact-integer?
                 #:height exact-integer?)

                boolean?)]))
]

That is, we put the mandatory keywords in the first section, and we
put the optional ones in the second section.


@ctc-section[#:tag "case-lambda"]{Contracts for @racket[case-lambda]}

A function defined with @racket[case-lambda] might impose different
constraints on its arguments depending on how many are provided. For
example, a @racket[report-cost] function might convert either a pair
of numbers or a string into a new string:

@margin-note{See @secref["case-lambda"] for an introduction to
@racket[case-lambda].}

@def+int[
(define report-cost
  (case-lambda
    [(lo hi) (format "between $~a and $~a" lo hi)]
    [(desc) (format "~a of dollars" desc)]))
(report-cost 5 8)
(report-cost "millions")
]

The contract for such a function is formed with the @racket[case->]
 combinator, which combines as many functional contracts as needed: 
@racketblock[
(provide (contract-out
          [report-cost
           (case->
            (integer? integer? . -> . string?)
            (string? . -> . string?))]))
]
 As you can see, the contract for @racket[report-cost] combines two
 function contracts, which is just as many clauses as the explanation
 of its functionality required.

@;{
This isn't supported anymore (yet...?). -robby

In the case of @racket[substring1], we also know that the indices
  that it consumes ought to be natural numbers less than the length of the
  given string. Since @racket[case->] just combines arrow contracts,
  adding such constraints is just a matter of strengthening the individual
  contracts: 
<racket>
(provide
 (contract-out
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
                  string?))]))
</racket>
  Here we used @racket[->r] to name the parameters and express the
  numeric constraints on them. 
}

@ctc-section[#:tag "arrow-d"]{Argument and Result Dependencies}

The following is an excerpt from an imaginary numerics module:

@racketblock[
(provide
 (contract-out
  [real-sqrt (->i ([argument (>=/c 1)])
                  [result (argument) (<=/c argument)])]))
]
 
@margin-note{The word ``indy'' was originally chosen in response
 to two existing words in the research literature: ``lax'' and
 ``picky'' that check contracts slightly differently; ``indy'' 
 is meant to suggest that blame may be assigned to the contract itself;
 e.g. the contract is an independent operator and can itself be blamed.}
The contract for the exported function @racket[real-sqrt] uses the
@racket[->i] rather than @racket[->*] function contract. The ``i''
stands for an @italic{indy dependent} contract, meaning the contract for the
function range depends on the value of the argument. The appearance
of @racket[argument] in the line for @racket[result]'s contract means
that the result depends on the argument. In this
particular case, the argument of @racket[real-sqrt] is greater or
equal to 1, so a very basic correctness check is that the result is
smaller than the argument.

In general, a dependent function contract looks just like
the more general @racket[->*] contract, but with names added
that can be used elsewhere in the contract.

@;{
Yes, there are many other contract combinators such as @racket[<=/c]
and @racket[>=/c], and it pays off to look them up in the contract
section of the reference manual. They simplify contracts tremendously
and make them more accessible to potential clients. 
}

Going back to the bank-account example, suppose that we generalize the
module to support multiple accounts and that we also include a
withdrawal operation. The improved bank-account module includes an
@racket[account] structure type and the following functions:

@racketblock[
(provide (contract-out
          [balance (-> account? amount/c)]
          [withdraw (-> account? amount/c account?)]
          [deposit (-> account? amount/c account?)]))
]

Besides requiring that a client provide a valid amount for a
withdrawal, however, the amount should be less than or equal to the specified
account's balance, and the resulting account will have less money than
it started with. Similarly, the module might promise that a deposit
produces an account with money added to the account. The following
implementation enforces those constraints and guarantees through
contracts:

@racketmod[
racket

(code:comment "section 1: the contract definitions")
(struct account (balance))
(define amount/c natural-number/c)

(code:comment "section 2: the exports")
(provide
 (contract-out
  [create   (amount/c . -> . account?)]
  [balance  (account? . -> . amount/c)]
  [withdraw (->i ([acc account?]
                  [amt (acc) (and/c amount/c (<=/c (balance acc)))])
                 [result (acc amt)
                         (and/c account? 
                                (lambda (res)
                                  (>= (balance res) 
                                      (- (balance acc) amt))))])]
  [deposit  (->i ([acc account?]
                  [amt amount/c])
                 [result (acc amt)
                         (and/c account? 
                                (lambda (res)
                                  (>= (balance res) 
                                      (+ (balance acc) amt))))])]))

(code:comment "section 3: the function definitions")
(define balance account-balance)

(define (create amt) (account amt))

(define (withdraw a amt)
  (account (- (account-balance a) amt)))

(define (deposit a amt)
  (account (+ (account-balance a) amt)))
]

The contracts in section 2 provide typical type-like guarantees for
@racket[create] and @racket[balance]. For @racket[withdraw] and
@racket[deposit], however, the contracts check and guarantee the more
complicated constraints on @racket[balance] and @racket[deposit].  The
contract on the second argument to @racket[withdraw] uses
@racket[(balance acc)] to check whether the supplied withdrawal amount
is small enough, where @racket[acc] is the name given within
@racket[->i] to the function's first argument. The contract on the
result of @racket[withdraw] uses both @racket[acc] and @racket[amt] to
guarantee that no more than that requested amount was withdrawn. The
contract on @racket[deposit] similarly uses @racket[acc] and
@racket[amount] in the result contract to guarantee that at least as
much money as provided was deposited into the account.

As written above, when a contract check fails, the error message is
not great. The following revision uses @racket[flat-named-contract]
within a helper function @racket[mk-account-contract] to provide
better error messages.

@racketmod[
racket

(code:comment "section 1: the contract definitions")
(struct account (balance))
(define amount/c natural-number/c)

(define msg> "account a with balance larger than ~a expected")
(define msg< "account a with balance less than ~a expected")

(define (mk-account-contract acc amt op msg)
  (define balance0 (balance acc))
  (define (ctr a)
    (and (account? a) (op balance0 (balance a))))
  (flat-named-contract (format msg balance0) ctr))

(code:comment "section 2: the exports")
(provide
 (contract-out
  [create   (amount/c . -> . account?)]
  [balance  (account? . -> . amount/c)]
  [withdraw (->i ([acc account?]
                  [amt (acc) (and/c amount/c (<=/c (balance acc)))])
                 [result (acc amt) (mk-account-contract acc amt >= msg>)])]
  [deposit  (->i ([acc account?]
                  [amt amount/c])
                 [result (acc amt) 
                         (mk-account-contract acc amt <= msg<)])]))

(code:comment "section 3: the function definitions")
(define balance account-balance)

(define (create amt) (account amt))

(define (withdraw a amt)
  (account (- (account-balance a) amt)))

(define (deposit a amt)
  (account (+ (account-balance a) amt)))
]

@ctc-section[#:tag "arrow-d-eval-order"]{Checking State Changes}

The @racket[->i] contract combinator can also ensure that a
function only modifies state according to certain
constraints. For example, consider this contract
(it is a slightly simplified from the function
@racket[preferences:add-panel] in the framework):
@racketblock[
(->i ([parent (is-a?/c area-container-window<%>)])
      [_ (parent)
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
(provide
 (contract-out
  [f (->i () [_ (begin (set! x (cons 'ctc x)) any/c)])]
  [get-x (-> (listof symbol?))]))
]
If you were to require this module, call @racket[f], then
the result of @racket[get-x] would be @racket['(f ctc)]. In
contrast, if the contract for @racket[f] were
@racketblock[(->i () [res (begin (set! x (cons 'ctc x)) any/c)])]
(only changing the underscore to @racket[res]), then
the result of @racket[get-x] would be @racket['(ctc f)].

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
function arrow @racket[->], since @racket[->]
treats @racket[values] specially when it appears as the
last result:
@racketblock[
(provide (contract-out
          [split (-> (listof char?)
                     (values string? (listof char?)))]))
]

The contract for such a function can also be written
using @racket[->*]:
@racketblock[
(provide (contract-out
          [split (->* ((listof char?))
                      ()
                      (values string? (listof char?)))]))
]
 As before, the contract for the argument with @racket[->*] is wrapped in an
 extra pair of parentheses (and must always be wrapped like
 that) and the empty pair of parentheses indicates that
 there are no optional arguments. The contracts for the
 results are inside @racket[values]: a string and a list of
 characters.

Now, suppose that we also want to ensure that the first result of
 @racket[split] is a prefix of the given word in list format. In that
 case, we need to use the @racket[->i] contract combinator:
@racketblock[
(define (substring-of? s)
  (flat-named-contract
    (format "substring of ~s" s)
    (lambda (s2)
      (and (string? s2)
           (<= (string-length s2) s)
           (equal? (substring s 0 (string-length s2)) s2)))))

(provide
 (contract-out
  [split (->i ([fl (listof char?)])
              (values [s (fl) (substring-of (list->string fl))]
                      [c (listof char?)]))]))
]
 Like @racket[->*], the @racket[->i] combinator uses a function over the
 argument to create the range contracts. Yes, it doesn't just return one
 contract but as many as the function produces values: one contract per
 value.  In this case, the second contract is the same as before, ensuring
 that the second result is a list of @racket[char]s. In contrast, the
 first contract strengthens the old one so that the result is a prefix of
 the given word. 

This contract is expensive to check, of course. Here is a slightly
  cheaper version: 
@racketblock[
(provide
 (contract-out
  [split (->i ([fl (listof char?)])
              (values [s (fl) (string-len/c (length fl))]
                      [c (listof char?)]))]))
]


@ctc-section[#:tag "no-domain"]{Fixed but Statically Unknown Arities}

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
(provide
 (contract-out
  [n-step
   (->i ([proc (inits)
          (and/c (unconstrained-domain-> 
                  (or/c false/c number?))
                 (λ (f) (procedure-arity-includes? 
                         f 
                         (length inits))))]
         [inits (listof number?)])
        ()
        any)]))
]

