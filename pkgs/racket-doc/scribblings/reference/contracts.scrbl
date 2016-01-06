#lang scribble/doc
@(require "mz.rkt")
@(require (for-label syntax/modcollapse
                     racket/stxparam))

@(define contract-eval
   (lambda ()
     (let ([the-eval (make-base-eval)])
       (the-eval '(require racket/contract racket/contract/parametric racket/list))
       the-eval)))

@(define blame-object
   @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{blame object})
@(define blame-objects
   @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{blame objects})

@title[#:tag "contracts" #:style 'toc]{Contracts}

@guideintro["contracts"]{contracts}

The contract system guards one part of a program from
another. Programmers specify the behavior of a module's exports via
@racket[(provide (contract-out ....))], and the contract system enforces those
constraints.

@(define-syntax-rule
   (add-use-sources (x y ...))
   (x y ...
      #:use-sources
      (racket/contract/private/base
       racket/contract/private/misc
       racket/contract/private/provide
       racket/contract/private/guts
       racket/contract/private/prop
       racket/contract/private/blame
       racket/contract/private/ds
       racket/contract/private/opt
       racket/contract/private/basic-opters

       racket/contract/private/box
       racket/contract/private/hash
       racket/contract/private/vector
       racket/contract/private/struct-dc)))

@(define-syntax-rule
   (declare-exporting-ctc mod)
   (add-use-sources (declare-exporting mod racket/contract racket)))

@(add-use-sources @note-lib[racket/contract])

@deftech{Contracts} come in two forms: those constructed by the
various operations listed in this section of the manual, and various
ordinary Racket values that double as contracts, including
@itemize[
@item{@tech{symbols}, @tech{booleans}, @tech{characters}, @tech{keywords}, and
@racket[null], which are treated as contracts that recognize
themselves, using @racket[eq?], }

@item{@tech{strings}, @tech{byte strings}, @racket[+nan.0], and
       @racket[+nan.f], which are treated as contracts
that recognize themselves using @racket[equal?], }

@item{@tech{numbers} (except @racket[+nan.0] and
       @racket[+nan.f]), which are treated as contracts
that recognize themselves using @racket[=],}

@item{@tech{regular expressions}, which are treated as contracts
       that recognize @tech{byte strings} and @tech{strings} that
       match the regular expression, and }

@item{predicates: any procedure of arity 1 is treated as a
predicate. During contract checking, it is applied to the values that
appear and should return @racket[#f] to indicate that the contract
failed, and anything else to indicate it passed.}

]

@deftech{Contract combinators} are functions such as @racket[->] and
@racket[listof] that take contracts and produce other contracts.

Contracts in Racket are subdivided into three different categories:
@;
@itemlist[@item{@deftech{Flat contract}s can be fully checked immediately for
                 a given value. These kinds of contracts are essentially
                 predicate functions. Using @racket[flat-contract-predicate],
                 you can extract the predicate from an arbitrary flat contract; some
                 flat contracts can be applied like functions, in which case
                 they accept a single argument and return @racket[#t] or
                 @racket[#f] to indicate if the given value would be accepted
                 by the contract. All of the flat contracts returned by functions
                 in this library can be used directly as predicates, but ordinary
                 Racket values that double as flat contracts (e.g., numbers or symbols)
                 cannot.
                 
                 The function @racket[flat-contract?] recognizes a flat contract.}
          @item{@deftech{Chaperone contracts} are not always immediately
                 checkable, but are guaranteed to not change any properties
                 of any values that they check. That is, they may wrap
                 a value in such a way that it signals contract violations
                 later, as the value is used (e.g., a function contract
                 checks the inputs and outputs to the function only when
                 the function is called and returned), but any properties
                 that the value had before being wrapped by the contract
                 are preserved by the contract wrapper. 
                 
                 All flat contracts are also chaperone contracts (but
                 not vice-versa).}
         @item{@deftech{Impersonator contracts} do not provide any 
                guarantees about values they check. Impersonator contracts
                may hide properties of values, or even make them completely
                opaque (e.g, @racket[new-∀/c]).
                
                All contracts are impersonator contracts.}]

For more about this hierarchy, see @tech{chaperones} and
a research paper on chaperones, impersonators, and how they can be used to 
implement contracts @cite{Strickland12}.

@history[#:changed "6.1.1.8" @list{Changed @racket[+nan.0] and @racket[+nan.f] to
                                           be @racket[equal?]-based contracts.}]

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "data-structure-contracts"]{Data-structure Contracts}
@declare-exporting-ctc[racket/contract/base]

@defproc[(flat-named-contract [name any/c]
                              [flat-contract flat-contract?]
                              [generator (or/c #f (-> contract (-> int? any))) #f])
         flat-contract?]{
Produces a contract like @racket[flat-contract], but with the name @racket[name].

For example,
@racketblock[(define/contract i
               (flat-named-contract
                'odd-integer
                (lambda (x) (and (integer? x) (odd? x))))
               2)]

The generator argument adds a generator for the flat-named-contract. See
@racket[contract-generate] for more information.
}

@defthing[any/c flat-contract?]{

A flat contract that accepts any value.

When using this contract as the result portion of a function contract,
consider using @racket[any] instead; using @racket[any] leads to
better memory performance, but it also allows multiple results.}


@defthing[none/c flat-contract?]{

A @tech{flat contract} that accepts no values.}


@defproc[(or/c [contract contract?] ...)
         contract?]{

Takes any number of contracts and returns
a contract that accepts any value that any one of the contracts
accepts individually.

The @racket[or/c] result tests any value by applying the contracts in
order, from left to right, with the exception that it always moves the
non-@tech{flat contracts} (if any) to the end, checking them
last. Thus, a contract such as @racket[(or/c (not/c real?)
positive?)] is guaranteed to only invoke the @racket[positive?]
predicate on real numbers.

If all of the arguments are procedures or @tech{flat contracts}, the
result is a @tech{flat contract}. If only one of the arguments is a
higher-order contract, the result is a contract that just checks the
flat contracts and, if they don't pass, applies the higher-order
contract.

If there are multiple higher-order contracts, @racket[or/c] uses
@racket[contract-first-order-passes?] to distinguish between
them. More precisely, when an @racket[or/c] is checked, it first
checks all of the @tech{flat contracts}. If none of them pass, it
calls @racket[contract-first-order-passes?] with each of the
higher-order contracts. If only one returns true, @racket[or/c] uses
that contract. If none of them return true, it signals a contract
violation. If more than one returns true, it also signals a contract
violation.
For example, this contract
@racketblock[
(or/c (-> number? number?)
      (-> string? string? string?))
]
does not accept a function like this one: @racket[(lambda args ...)]
since it cannot tell which of the two arrow contracts should be used
with the function.

If all of its arguments are @racket[list-contract?]s, then @racket[or/c]
returns a @racket[list-contract?].
}

@defproc[(first-or/c [contract contract?] ...)
         contract?]{

 Takes any number of contracts and returns a contract that
 accepts any value that any one of the contracts accepts
 individually.

 The @racket[first-or/c] result tests any value by applying the
 contracts in order from left to right. Thus, a contract
 such as @racket[(first-or/c (not/c real?) positive?)]
 is guaranteed to only invoke the 
 @racket[positive?] predicate on real numbers.

 If all of the arguments are procedures or @tech{flat
  contracts}, the result is a @tech{flat contract} and
 similarly if all of the arguments are @tech{chaperone
  contracts} the result is too. Otherwise, the result is an
 @tech{impersonator contract}.

 If there are multiple higher-order contracts, 
 @racket[first-or/c] uses @racket[contract-first-order-passes?]
 to distinguish between them. More precisely, when an 
 @racket[first-or/c] is checked, it checks the first order passes
 of the first contract against the value. If it succeeds,
 then it uses only that contract. If it fails, then it moves
 to the second contract, continuing until it finds one of
 the contracts where the first order check succeeds. If none
 of them do, a contract violation is signaled.

 For example, this contract
 @racketblock[
 (first-or/c (-> number? number?)
        (-> string? string? string?))]
 accepts the function @racket[(λ args 0)],
 applying the @racket[(->number? number?)] contract to the function
 because it comes first, even though
 @racket[(-> string? string? string?)] also applies.

 If all of its arguments are @racket[list-contract?]s, then @racket[first-or/c]
 returns a @racket[list-contract?].
}

@defproc[(and/c [contract contract?] ...) contract?]{

Takes any number of contracts and returns a contract that
accepts any value that satisfies all of the contracts simultaneously.

If all of the arguments are procedures or @tech{flat contracts},
the result is a @tech{flat contract}.

The contract produced by @racket[and/c] tests any value by applying
the contracts in order, from left to right.}


@defproc[(not/c [flat-contract flat-contract?]) flat-contract?]{

Accepts a flat contract or a predicate and returns a flat contract
that checks the inverse of the argument.}


@defproc[(=/c [z real?]) flat-contract?]{

Returns a flat contract that requires the input to be a number and
@racket[=] to @racket[z].}


@defproc[(</c [n real?]) flat-contract?]{

Returns a flat contract that requires the input to be a number and
@racket[<] than @racket[n].}


@defproc[(>/c [n real?]) flat-contract?]{
Like @racket[</c], but for @racket[>].}


@defproc[(<=/c [n real?]) flat-contract?]{
Like @racket[</c], but for @racket[<=].}


@defproc[(>=/c [n real?]) flat-contract?]{
Like @racket[</c], but for @racket[>=].}

@defproc[(between/c [n real?] [m real?])
flat-contract?]{ Returns a flat contract that requires the
input to be a real number between @racket[n] and @racket[m] or equal to
one of them.}

@defproc[(real-in [n real?] [m real?]) flat-contract?]{
An alias for @racket[between/c].}

@defproc[(integer-in [j exact-integer?] [k exact-integer?]) flat-contract?]{

Returns a flat contract that requires the input to be an exact integer
between @racket[j] and @racket[k], inclusive.}

@defproc[(char-in [a char?] [b char?]) flat-contract?]{

Returns a flat contract that requires the input to be a character whose
code point number is between the code point numbers of @racket[a] and
@racket[b], inclusive.}


@defthing[natural-number/c flat-contract?]{

A flat contract that requires the input to be an exact non-negative integer.}


@defproc[(string-len/c [len real?]) flat-contract?]{

Returns a flat contract that recognizes strings that have fewer than
@racket[len] characters.}


@defthing[false/c flat-contract?]{

An alias @racket[#f] for backwards compatibility.}


@defthing[printable/c flat-contract?]{

A flat contract that recognizes values that can be written out and
read back in with @racket[write] and @racket[read].}


@defproc[(one-of/c [v any/c] ...+) flat-contract?]{

Accepts any number of atomic values and returns a flat contract that
recognizes those values, using @racket[eqv?]  as the comparison
predicate.  For the purposes of @racket[one-of/c], atomic values are
defined to be: @tech{characters}, @tech{symbols}, @tech{booleans},
@racket[null], @tech{keywords}, @tech{numbers},
@|void-const|, and @|undefined-const|.

This is a backwards compatibility contract constructor. If
neither @|void-const| nor @|undefined-const| are arguments,
it simply passes its arguments to @racket[or/c].
}


@defproc[(symbols [sym symbol?] ...+) flat-contract?]{

Accepts any number of symbols and returns a flat contract that
recognizes those symbols.

This is a backwards compatibility constructor; it merely
passes its arguments to @racket[or/c].
}

@defproc[(vectorof [c contract?]
                   [#:immutable immutable (or/c #t #f 'dont-care) 'dont-care]
                   [#:flat? flat? boolean? #f])
         contract?]{
Returns a contract that recognizes vectors. The elements of the vector must
match @racket[c].

If the @racket[flat?] argument is @racket[#t], then the resulting contract is
a flat contract, and the @racket[c] argument must also be a flat contract.  Such
flat contracts will be unsound if applied to mutable vectors, as they will not
check future operations on the vector.

If the @racket[immutable] argument is @racket[#t] and the @racket[c] argument is
a flat contract, the result will be a flat contract.  If the @racket[c] argument
is a chaperone contract, then the result will be a chaperone contract.

When a higher-order @racket[vectorof] contract is applied to a vector, the result
is not @racket[eq?] to the input.  The result will be a copy for immutable vectors
and a @tech{chaperone} or @tech{impersonator} of the input for mutable vectors,
unless the @racket[c] argument is a flat contract and the vector is immutable,
in which case the result is the original vector.

@history[#:changed "6.3.0.5" @list{Changed flat vector contracts to not copy
           immutable vectors.}]}


@defproc[(vector-immutableof [c contract?]) contract?]{

Returns the same contract as @racket[(vectorof c #:immutable #t)]. This form exists for
backwards compatibility.}

@defproc[(vector/c [c contract?] ...
                   [#:immutable immutable (or/c #t #f 'dont-care) 'dont-care]
                   [#:flat? flat? boolean? #f])
         contract?]{
Returns a contract that recognizes vectors whose lengths match the number of
contracts given. Each element of the vector must match its corresponding contract.

If the @racket[flat?] argument is @racket[#t], then the resulting contract is
a flat contract, and the @racket[c] arguments must also be flat contracts.  Such
flat contracts will be unsound if applied to mutable vectors, as they will not
check future operations on the vector.

If the @racket[immutable] argument is @racket[#t] and the @racket[c] arguments are
flat contracts, the result will be a flat contract.  If the @racket[c] arguments
are chaperone contracts, then the result will be a chaperone contract.

When a higher-order @racket[vector/c] contract is applied to a vector, the result
is not @racket[eq?] to the input.  The result will be a copy for immutable vectors
and a @tech{chaperone} or @tech{impersonator} of the input for mutable vectors.}


@defproc[(vector-immutable/c [c contract?] ...) contract?]{

Returns the same contract as @racket[(vector/c c ... #:immutable #t)]. This form exists for
reasons of backwards compatibility.}


@defproc[(box/c [in-c contract?]
                [c contract? in-c]
                [#:immutable immutable (or/c #t #f 'dont-care) 'dont-care]
                [#:flat? flat? boolean? #f])
         contract?]{
Returns a contract that recognizes boxes. The content of the box must match @racket[c],
and mutations on mutable boxes must match @racket[in-c].

If the @racket[flat?] argument is @racket[#t], then the resulting contract is
a flat contract, and the @racket[out] argument must also be a flat contract.  Such
flat contracts will be unsound if applied to mutable boxes, as they will not check
future operations on the box.

If the @racket[immutable] argument is @racket[#t] and the @racket[c] argument is
a flat contract, the result will be a flat contract.  If the @racket[c] argument is
a chaperone contract, then the result will be a chaperone contract.

When a higher-order @racket[box/c] contract is applied to a box, the result
is not @racket[eq?] to the input.  The result will be a copy for immutable boxes
and either a @tech{chaperone} or @tech{impersonator} of the input for mutable boxes.}


@defproc[(box-immutable/c [c contract?]) contract?]{

Returns the same contract as @racket[(box/c c #:immutable #t)]. This form exists for
reasons of backwards compatibility.}


@defproc[(listof [c contract?]) list-contract?]{

Returns a contract that recognizes a list whose every element matches
the contract @racket[c]. Beware that when this contract is applied to
a value, the result is not necessarily @racket[eq?] to the input.

@examples[#:eval (contract-eval) #:once
                 (define/contract some-numbers
                   (listof number?)
                   (list 1 2 3))
                 (eval:error
                  (define/contract just-one-number
                    (listof number?)
                    11))]

}


@defproc[(non-empty-listof [c contract?]) list-contract?]{

Returns a contract that recognizes non-empty lists whose elements match
the contract @racket[c]. Beware that when this contract is applied to
a value, the result is not necessarily @racket[eq?] to the input.

@examples[#:eval (contract-eval) #:once
                 (define/contract some-numbers
                   (non-empty-listof number?)
                   (list 1 2 3))
                                
                 (eval:error
                  (define/contract not-enough-numbers
                    (non-empty-listof number?)
                    (list)))]
}

@defproc[(list*of [c contract?]) contract?]{

Returns a contract that recognizes improper lists whose elements match
the contract @racket[c]. If an improper list is created with @racket[cons],
then its @racket[car] position is expected to match @racket[c] and
its @racket[cdr] position is expected to be @racket[(list*of c)]. Otherwise,
it is expected to match @racket[c]. Beware that when this contract is applied to
a value, the result is not necessarily @racket[eq?] to the input.

@examples[#:eval (contract-eval) #:once
                 (define/contract improper-numbers
                   (list*of number?)
                   (cons 1 (cons 2 3)))
                                
                 (eval:error
                  (define/contract not-improper-numbers
                    (list*of number?)
                    (list 1 2 3)))]

@history[#:added "6.1.1.1"]
}


@defproc[(cons/c [car-c contract?] [cdr-c contract?]) contract?]{

Produces a contract that recognizes pairs whose first and second elements
match @racket[car-c] and @racket[cdr-c], respectively. Beware that
when this contract is applied to a value, the result is not
necessarily @racket[eq?] to the input.

If the @racket[cdr-c] contract is a @racket[list-contract?], then
@racket[cons/c] returns a @racket[list-contract?].

@examples[#:eval (contract-eval) #:once
                 (define/contract a-pair-of-numbers
                   (cons/c number? number?)
                   (cons 1 2))
                                
                 (eval:error
                  (define/contract not-a-pair-of-numbers
                    (cons/c number? number?)
                    (cons #f #t)))]

@history[#:changed "6.0.1.13" @list{Added the @racket[list-contract?] propagating behavior.}]
}

@defform*[[(cons/dc [car-id contract-expr] [cdr-id (car-id) contract-expr] cons/dc-option)
           (cons/dc [car-id (cdr-id) contract-expr] [cdr-id contract-expr] cons/dc-option)]
          #:grammar ([cons/dc-option (code:line)
                                     #:flat
                                     #:chaperone
                                     #:impersonator])]{

Produces a contract that recognizes pairs whose first and second elements
match the expressions after @racket[car-id] and @racket[cdr-id], respectively.

In the first case, the contract on the @racket[cdr-id] portion of the contract
may depend on the value in the @racket[car-id] portion of the pair and in
the second case, the reverse is true.

@examples[#:eval (contract-eval) #:once
                 (define/contract an-ordered-pair-of-reals
                   (cons/dc [hd real?] [tl (hd) (>=/c hd)])
                   (cons 1 2))
                                
                 (eval:error
                  (define/contract not-an-ordered-pair-of-reals
                    (cons/dc [hd real?] [tl (hd) (>=/c hd)])
                    (cons 2 1)))]

@history[#:added "6.1.1.6"]
}

@defproc[(list/c [c contract?] ...) list-contract?]{

Produces a contract for a list. The number of elements in the list
must match the number of arguments supplied to @racket[list/c], and
each element of the list must match the corresponding contract. Beware
that when this contract is applied to a value, the result is not
necessarily @racket[eq?] to the input.}


@defproc[(syntax/c [c flat-contract?]) flat-contract?]{

Produces a flat contract that recognizes syntax objects whose
@racket[syntax-e] content matches @racket[c].}


@defform[(struct/c struct-id contract-expr ...)]{
Produces a contract that recognizes instances of the structure
type named by @racket[struct-id], and whose field values match the
contracts produced by the @racket[contract-expr]s.

Contracts for immutable fields must be either flat or chaperone contracts.
Contracts for mutable fields may be impersonator contracts.
If all fields are immutable and the @racket[contract-expr]s evaluate
to flat contracts, a flat contract is produced.  If all the
@racket[contract-expr]s are chaperone contracts, a chaperone contract is
produced.  Otherwise, an impersonator contract is produced.
}


@defform/subs[(struct/dc struct-id field-spec ... maybe-inv)
              ([field-spec [field-name maybe-lazy contract-expr]
                           [field-name (dep-field-name ...)
                                       maybe-lazy
                                       maybe-contract-type
                                       maybe-dep-state
                                       contract-expr]]
               [field-name field-id
                           (#:selector selector-id)
                           (field-id #:parent struct-id)]
               [maybe-lazy (code:line) #:lazy]
               [maybe-contract-type (code:line) #:flat #:chaperone #:impersonator]
               [maybe-dep-state (code:line) #:depends-on-state]
               [maybe-inv (code:line)
                          (code:line #:inv (dep-field-name ...) invariant-expr)])]{
Produces a contract that recognizes instances of the structure
type named by @racket[struct-id], and whose field values match the
contracts produced by the @racket[field-spec]s.

If the @racket[field-spec] lists the names of other fields,
then the contract depends on values in those fields, and the @racket[contract-expr]
expression is evaluated each time a selector is applied, building a new contract
for the fields based on the values of the @racket[dep-field-name] fields (the
@racket[dep-field-name] syntax is the same as the @racket[field-name] syntax).
If the field is a dependent field and no @racket[contract-type] annotation
appears, then it is assumed that the contract is
a chaperone, but not always a flat contract (and thus the entire @racket[struct/dc]
contract is not a flat contract).
If this is not the case, and the contract is
always flat then the field must be annotated with
the @racket[#:flat], or the field must be annotated with
@racket[#:impersonator] (in which case, it must be a mutable field).

A @racket[field-name] is either an identifier naming a field in the first
case, an identifier naming a selector in the second case indicated
by the @racket[#:selector] keyword, or
a field id for a struct that is a parent of @racket[struct-id], indicated
by the @racket[#:parent] keyword.

If the @racket[#:lazy] keyword appears, then the contract
on the field is checked lazily (only when a selector is applied);
@racket[#:lazy] contracts cannot be put on mutable fields.

If a dependent contract depends on some mutable state, then use the
@racket[#:depends-on-state] keyword argument (if a field's dependent contract
depends on a mutable field, this keyword is automatically inferred).
The presence of this keyword means that the contract expression is evaluated
each time the corresponding field is accessed (or mutated, if it is a mutable
field). Otherwise, the contract expression for a dependent field contract
is evaluated when the contract is applied to a value.

If the @racket[#:inv] clause appears, then the invariant expression is 
evaluated (and must return a non-@racket[#f] value) when the contract
is applied to a struct.

Contracts for immutable fields must be either flat or chaperone contracts.
Contracts for mutable fields may be impersonator contracts.
If all fields are immutable and the @racket[contract-expr]s evaluate
to flat contracts, a flat contract is produced.  If all the
@racket[contract-expr]s are chaperone contracts, a chaperone contract is
produced.  Otherwise, an impersonator contract is produced.

As an example, the function @racket[bst/c] below
returns a contract for binary search trees whose values
are all between @racket[lo] and @racket[hi].
The lazy annotations ensure that this contract does not
change the running time of operations that do not
inspect the entire tree.

@racketblock[(struct bt (val left right))
             (define (bst/c lo hi)
               (or/c #f
                     (struct/dc bt
                                [val (between/c lo hi)]
                                [left (val) #:lazy (bst lo val)]
                                [right (val) #:lazy (bst val hi)])))]

@history[#:changed "6.0.1.6" @elem{Added @racket[#:inv].}]
}

@defproc[(parameter/c [in contract?] [out contract? in])
         contract?]{

Produces a contract on parameters whose values must match
@racket[_out]. When the value in the contracted parameter
is set, it must match @racket[_in].

@examples[#:eval (contract-eval) #:once
(define/contract current-snack
  (parameter/c string?)
  (make-parameter "potato-chip"))
(define baked/c
  (flat-named-contract 'baked/c (λ (s) (regexp-match #rx"baked" s))))
(define/contract current-dinner
  (parameter/c string? baked/c)
  (make-parameter "turkey" (λ (s) (string-append "roasted " s))))

(eval:error (current-snack 'not-a-snack))
(eval:error
 (parameterize ([current-dinner "tofurkey"])
   (current-dinner)))
]}


@defproc[(procedure-arity-includes/c [n exact-nonnegative-integer?]) flat-contract?]{

Produces a contract for procedures that accept @racket[n] argument
(i.e,. the @racket[procedure?] contract is implied).}


@defproc[(hash/c [key chaperone-contract?]
                 [val contract?]
                 [#:immutable immutable (or/c #t #f 'dont-care) 'dont-care]
                 [#:flat? flat? boolean? #f])
         contract?]{
Produces a contract that recognizes @racket[hash] tables with keys and values
as specified by the @racket[key] and @racket[val] arguments.

@examples[#:eval (contract-eval) #:once
          (define/contract good-hash
            (hash/c integer? boolean?)
            (hash 1 #t
                  2 #f
                  3 #t))
          (eval:error
           (define/contract bad-hash
             (hash/c integer? boolean?)
             (hash 1 "elephant"
                   2 "monkey"
                   3 "manatee")))]

There are a number of technicalities that control how @racket[hash/c] contracts
behave.
@itemlist[@item{
 If the @racket[flat?] argument is @racket[#t], then the resulting contract is
 a flat contract, and the @racket[key] and @racket[val] arguments must also be flat
 contracts. 

@examples[#:eval (contract-eval) #:once
          (flat-contract? (hash/c integer? boolean?))
          (flat-contract? (hash/c integer? boolean? #:flat? #t))
          (eval:error (hash/c integer? (-> integer? integer?) #:flat? #t))]
 
 Such flat contracts will be unsound if applied to mutable hash tables,
 as they will not check future mutations to the hash table.

@examples[#:eval (contract-eval) #:once
          (define original-h (make-hasheq))
          (define/contract ctc-h 
            (hash/c integer? boolean? #:flat? #t)
            original-h)
          (hash-set! original-h 1 "not a boolean")
          (hash-ref ctc-h 1)]}
           @item{
If the @racket[immutable] argument is @racket[#t] and the @racket[key] and
@racket[val] arguments are @racket[flat-contract?]s, the result will be a 
@racket[flat-contract?].

@examples[#:eval (contract-eval) #:once
          (flat-contract? (hash/c integer? boolean? #:immutable #t))]
          
If either the domain or the range is a @racket[chaperone-contract?], then the result will
be a @racket[chaperone-contract?].

@examples[#:eval (contract-eval) #:once
          (flat-contract? (hash/c (-> integer? integer?) boolean?
                                  #:immutable #t))
          (chaperone-contract? (hash/c (-> integer? integer?) boolean?
                                       #:immutable #t))]
}

           @item{
If the @racket[key] argument is a @racket[chaperone-contract?] but not a
@racket[flat-contract?], then the resulting contract
can be applied only to @racket[equal?]-based hash tables.
@examples[#:eval (contract-eval) #:once
          (eval:error
           (define/contract h
             (hash/c (-> integer? integer?) any/c)
             (make-hasheq)))]
Also, when such a @racket[hash/c] contract is applied to a hash table, the result is not 
@racket[eq?]
to the input. The result of applying the contract will be a copy for immutable hash tables,
and either a @tech{chaperone} or @tech{impersonator} of the original hash table
for mutable hash tables.
}]}

@defform[(hash/dc [key-id key-contract-expr] [value-id (key-id) value-contract-expr]
                  hash/dc-option)
         #:grammar ([hash/dc-option (code:line)
                                    (code:line #:immutable immutable?-expr hash/dc-option)
                                    (code:line #:kind kind-expr hash/dc-option)])]{
 Creates a contract for @racket[hash?] tables with keys matching @racket[key-contract-expr]
 and where the contract on the values can depend on the key itself, since
 @racket[key-id] will be bound to the corresponding key before evaluating
 the @racket[values-contract-expr].
 
 If @racket[immutable?-expr] is @racket[#t], then only @racket[immutable?] hashes
 are accepted. If it is @racket[#f] then @racket[immutable?] hashes are always
 rejected. It defaults to @racket['dont-care], in which case both mutable and
 immutable hashes are accepted.
 
 If @racket[kind-expr] evaluates to @racket['flat], then @racket[key-contract-expr]
 and @racket[value-contract-expr] are expected to evaluate to @racket[flat-contract?]s.
 If it is @racket['chaperone], then they are expected to be @racket[chaperone-contract?]s,
 and it may also be @racket['impersonator], in which case they may be any @racket[contract?]s.
 The default is @racket['chaperone].
 
 @examples[#:eval (contract-eval) #:once
            (define/contract h
              (hash/dc [k real?] [v (k) (>=/c k)])
              (hash 1 3
                    2 4))
            (eval:error
             (define/contract h
               (hash/dc [k real?] [v (k) (>=/c k)])
               (hash 3 1
                     4 2)))]
 
 
}

@defproc[(channel/c [val contract?])
          contract?]{
Produces a contract that recognizes @tech{channel}s that communicate
values as specified by the @racket[val] argument.

If the @racket[val] argument is a chaperone contract, then the resulting contract
is a chaperone contract. Otherwise, the resulting contract is an impersonator
contract. When a channel contract is applied to a channel, the resulting channel
is not @racket[eq?] to the input.

@examples[#:eval (contract-eval) #:once
  (define/contract chan
    (channel/c string?)
    (make-channel))
  (thread (λ () (channel-get chan)))
  (eval:error (channel-put chan 'not-a-string))
]}


@defform/subs[#:literals (values)
  (prompt-tag/c contract ... maybe-call/cc)
  ([maybe-call/cc (code:line)
                  (code:line #:call/cc contract)
                  (code:line #:call/cc (values contract ...))])
   #:contracts ([contract contract?])]{
Takes any number of contracts and returns a contract that recognizes
continuation prompt tags and will check any aborts or prompt handlers that
use the contracted prompt tag.

Each @racket[contract] will check the corresponding value passed to
an @racket[abort-current-continuation] and handled by the handler of a
call to @racket[call-with-continuation-prompt].

If all of the @racket[contract]s are chaperone contracts, the resulting
contract will also be a @tech{chaperone} contract. Otherwise, the contract is
an @tech{impersonator} contract.

If @racket[maybe-call/cc] is provided, then the provided contracts
are used to check the return values from a continuation captured with
@racket[call-with-current-continuation].

@examples[#:eval (contract-eval) #:once
  (define/contract tag
    (prompt-tag/c (-> number? string?))
    (make-continuation-prompt-tag))

  (eval:error
   (call-with-continuation-prompt
     (lambda ()
       (number->string
         (call-with-composable-continuation
           (lambda (k)
             (abort-current-continuation tag k)))))
     tag
     (lambda (k) (k "not a number"))))
]
}


@defproc[(continuation-mark-key/c [contract contract?]) contract?]{
Takes a single contract and returns a contract that recognizes
continuation marks and will check any mappings of marks to values
or any accesses of the mark value.

If the argument @racket[contract] is a chaperone contract, the resulting
contract will also be a @tech{chaperone} contract. Otherwise, the contract is
an @tech{impersonator} contract.

@examples[#:eval (contract-eval) #:once
  (define/contract mark-key
    (continuation-mark-key/c (-> symbol? (listof symbol?)))
    (make-continuation-mark-key))

  (eval:error
   (with-continuation-mark
     mark-key
     (lambda (s) (append s '(truffle fudge ganache)))
     (let ([mark-value (continuation-mark-set-first
                        (current-continuation-marks) mark-key)])
       (mark-value "chocolate-bar"))))
]
}

@defproc[(evt/c [contract chaperone-contract?] ...) chaperone-contract?]{
Returns a contract that recognizes @tech{synchronizable event}s whose
@tech{synchronization result}s are checked by the given
@racket[contract]s.

The resulting contract is always a @tech{chaperone} contract and its
arguments must all be chaperone contracts.

@examples[#:eval (contract-eval) #:once
  (define/contract my-evt
    (evt/c evt?)
    always-evt)
  (define/contract failing-evt
    (evt/c number? number?)
    (alarm-evt (+ (current-inexact-milliseconds) 50)))
  (sync my-evt)
  (eval:error (sync failing-evt))
]
}


@defform[(flat-rec-contract id flat-contract-expr ...)]{

Constructs a recursive @tech{flat contract}. A
@racket[flat-contract-expr] can refer to @racket[id] to refer
recursively to the generated contract.

For example, the contract

@racketblock[
   (flat-rec-contract sexp
     (cons/c sexp sexp)
     number?
     symbol?)
]

is a flat contract that checks for (a limited form of)
S-expressions. It says that a @racket[sexp] is either two
@racket[sexp]s combined with @racket[cons], or a number, or a symbol.

Note that if the contract is applied to a circular value, contract
checking will not terminate.}


@defform[(flat-murec-contract ([id flat-contract-expr ...] ...) body ...+)]{

A generalization of @racket[flat-rec-contract] for defining several
mutually recursive flat contracts simultaneously. Each @racket[id] is
visible in the entire @racket[flat-murec-contract] form, and the
result of the final @racket[body] is the result of the entire form.}


@defidform[any]{

Represents a contract that is always satisfied. In particular, it can accept
multiple values.  It can only be used in a result position of contracts like
@racket[->]. Using @racket[any] elsewhere is a syntax error.}

@defproc[(promise/c [c contract?]) contract?]{

Constructs a contract on a promise. The contract does not force the
promise, but when the promise is forced, the contract checks that the
result value meets the contract @racket[c].}

@defproc[(flat-contract [predicate (-> any/c any/c)]) flat-contract?]{

Constructs a @tech{flat contract} from @racket[predicate]. A value
satisfies the contract if the predicate returns a true value.

This function is a holdover from before predicates could be used
directly as flat contracts. It exists today for backwards compatibility.
}


@defproc[(flat-contract-predicate [v flat-contract?])
         (-> any/c any/c)]{

Extracts the predicate from a flat contract.

This function is a holdover from before flat contracts could be used
directly as predicates. It exists today for backwards compatibility.
}


@; ------------------------------------------------------------------------

@section[#:tag "function-contracts"]{Function Contracts}
@declare-exporting-ctc[racket/contract/base]

A @deftech{function contract} wraps a procedure to delay
checks for its arguments and results. There are three
primary function contract combinators that have increasing
amounts of expressiveness and increasing additional
overheads. The first @racket[->] is the cheapest. It
generates wrapper functions that can call the original
function directly. Contracts built with @racket[->*] require
packaging up arguments as lists in the wrapper function and
then using either @racket[keyword-apply] or
@racket[apply]. Finally, @racket[->i]
is the most expensive (along with @racket[->d]),
because it requires delaying the evaluation of the contract
expressions for the domain and range until the function
itself is called or returns.

The @racket[case->] contract is a specialized contract,
designed to match @racket[case-lambda] and
@racket[unconstrained-domain->] allows range checking
without requiring that the domain have any particular shape
(see below for an example use).

@defform*/subs[#:literals (any values)
               [(-> dom ... range)]
               ([dom dom-expr (code:line keyword dom-expr)]
                [range range-expr (values range-expr ...) any])]{

Produces a contract for a function that accepts a fixed
number of arguments and returns either a fixed number of
results or completely unspecified results (the latter when
@racket[any] is specified).

Each @racket[dom-expr] is a contract on an argument to a
function, and each @racket[range-expr] is a contract on a
result of the function.

@margin-note{Using a @racket[->] between two whitespace-delimited
@racketparenfont{.}s is the same as putting the @racket[->] right
after the enclosing opening parenthesis. See
@guidesecref["lists-and-syntax"] or @secref["parse-pair"] for more
information.}

For example,

@racketblock[(integer? boolean? . -> . integer?)]

produces a contract on functions of two arguments. The first argument
must be an integer, and the second argument must be a boolean. The
function must produce an integer.

A domain specification may include a keyword. If so, the function must
accept corresponding (mandatory) keyword arguments, and the values for
the keyword arguments must match the corresponding contracts. For
example:

@racketblock[(integer? #:x boolean? . -> . integer?)]

is a contract on a function that accepts a by-position argument that
is an integer and a @racket[#:x] argument that is a boolean.

If @racket[any] is used as the last sub-form for @racket[->], no
contract checking is performed on the result of the function, and
thus any number of values is legal (even different numbers on different
invocations of the function).

If @racket[(values range-expr ...)] is used as the last sub-form of
@racket[->], the function must produce a result for each contract, and
each value must match its respective contract.}


@defform*/subs[#:literals (any values)
          [(->* (mandatory-dom ...) optional-doms rest pre range post)]
          ([mandatory-dom dom-expr (code:line keyword dom-expr)]
           [optional-doms (code:line) (optional-dom ...)]
           [optional-dom dom-expr (code:line keyword dom-expr)]
           [rest (code:line) (code:line #:rest rest-expr)]
           [pre (code:line) 
                (code:line #:pre pre-cond-expr)
                (code:line #:pre/desc pre-cond-expr)]
           [range range-expr (values range-expr ...) any]
           [post (code:line) 
                 (code:line #:post post-cond-expr)
                 (code:line #:post/desc post-cond-expr)])]{

The @racket[->*] contract combinator produces contracts for functions
that accept optional arguments (either keyword or positional) and/or
arbitrarily many arguments. The first clause of a @racket[->*]
contract describes the mandatory arguments, and is similar to the
argument description of a @racket[->] contract. The second clause
describes the optional arguments. The range of description can either
be @racket[any] or a sequence of contracts, indicating that the
function must return multiple values.

If present, the
@racket[rest-expr] contract governs the arguments in the rest
parameter. Note that the @racket[rest-expr] contract governs only
the arguments in the rest parameter, not those in mandatory arguments.
For example, this contract:
@racketblock[(->* () #:rest (cons/c integer? (listof integer?)) any)]
does not match the function
@racketblock[(λ (x . rest) x)]
because the contract insists that the function accept zero arguments
(because there are no mandatory arguments listed in the contract). The
@racket[->*] contract does not know that the contract on the rest argument is
going to end up disallowing empty argument lists.

The @racket[pre-cond-expr] and @racket[post-cond-expr]
expressions are checked as the function is called and returns,
respectively, and allow checking of the environment without an
explicit connection to an argument (or a result). If the @racket[#:pre]
or @racket[#:post] keywords are used, then a @racket[#f] result is
treated as a failure and any other result is treated as success.
If the @racket[#:pre/desc] or @racket[#:post/desc] keyword is used,
the result of the expression must be either a boolean, a string, or a
list of strings, where @racket[#t] means success and any of the other
results mean failure. If the result is a string or a list of strings,
the strings are expected to have at exactly one space after each
newline and multiple are used as lines in the error message; the contract
itself adds single space of indentation to each of the strings in that case.
The formatting requirements are not checked but they
match the recommendations in @secref["err-msg-conventions"].

As an example, the contract
@racketblock[(->* () (boolean? #:x integer?) #:rest (listof symbol?) symbol?)]
matches functions that optionally accept a boolean, an
integer keyword argument @racket[#:x] and arbitrarily more
symbols, and that return a symbol.
}

@defform*/subs[#:literals (any values)
[(->i maybe-chaperone
      (mandatory-dependent-dom ...)
      dependent-rest
      pre-condition
      dependent-range
      post-condition)
 (->i maybe-chaperone
      (mandatory-dependent-dom ...)
      (optional-dependent-dom ...)
      dependent-rest
      pre-condition
      dependent-range
      post-condition)]
([maybe-chaperone #:chaperone (code:line)]
 [mandatory-dependent-dom id+ctc
                          (code:line keyword id+ctc)]
 [optional-dependent-dom id+ctc
                         (code:line keyword id+ctc)]
 [dependent-rest (code:line) (code:line #:rest id+ctc)]
 [pre-condition (code:line)
                (code:line #:pre (id ...) 
                           boolean-expr pre-condition)
                (code:line #:pre/desc (id ...) 
                           expr pre-condition)
                (code:line #:pre/name (id ...)
                           string boolean-expr pre-condition)]
 [dependent-range any
                  id+ctc
                  un+ctc
                  (values id+ctc ...)
                  (values un+ctc ...)]
 [post-condition (code:line)
                 (code:line #:post (id ...)
                            boolean-expr post-condition)
                 (code:line #:post/desc (id ...)
                            expr post-condition)
                 (code:line #:post/name (id ...)
                            string boolean-expr post-condition)]
 [id+ctc [id contract-expr]
         [id (id ...) contract-expr]]
 [un+ctc [_ contract-expr]
         [_ (id ...) contract-expr]]
)]{

The @racket[->i] contract combinator differs from the @racket[->*]
combinator in that each argument and result is named and these names can
be used in the subcontracts and in the pre-/post-condition clauses.
In other words, @racket[->i] expresses dependencies among arguments and results.

The optional first keyword argument to @racket[->i] indicates if the result
contract will be a chaperone. If it is @racket[#:chaperone], all of the contract for the arguments
and results must be chaperone contracts and the result of @racket[->i] will be
a chaperone contract. If it is not present, then the result
contract will not be a chaperone contract.

The first sub-form of a @racket[->i] contract covers the mandatory and the
second sub-form covers the optional arguments. Following that is an optional
rest-args contract, and an optional pre-condition. The pre-condition is
introduced with the @racket[#:pre] keyword followed by the list of names on
which it depends. If the @racket[#:pre/name] keyword is used, the string
supplied is used as part of the error message; similarly with @racket[#:post/name].
If @racket[#:pre/desc] or @racket[#:post/desc] is used, the the result of
the expression is treated the same way as @racket[->*].

The @racket[dependent-range] non-terminal specifies the possible result
contracts. If it is @racket[any], then any value is allowed. Otherwise, the
result contract pairs a name and a contract or a multiple values return
with names and contracts. In the last two cases, the range contract may be
optionally followed by a post-condition; the post-condition expression is
not allowed if the range contract is @racket[any]. Like the pre-condition,
the post-condition must specify the variables on which it depends.

Consider this sample contract:
@racketblock[(->i ([x number?]
                   [y (x) (>=/c x)])
                  [result (x y) (and/c number? (>=/c (+ x y)))])]
It specifies a function of two arguments, both numbers. The contract on the
second argument (@racket[y]) demands that it is greater than the first
argument. The result contract promises a number that is greater than the
sum of the two arguments. While the dependency specification for @racket[y]
signals that the argument contract depends on the value of the first
argument, the dependency sequence for @racket[result] indicates that the
contract depends on both argument values. @margin-note*{In general, an
empty sequence is (nearly) equivalent to not adding
a sequence at all except that the former is more expensive than the latter.}
Since the contract for @racket[x] does not depend on anything else, it does
not come with any dependency sequence, not even @racket[()].

This example is like the previous one, except the @racket[x] and @racket[y]
arguments are now optional keyword arguments, instead of mandatory, by-position
arguments:
@racketblock[(->i ()
                  (#:x [x number?]
                   #:y [y (x) (>=/c x)])
                  [result (x y) (and/c number? (>=/c (+ x y)))])]

The contract expressions are not always evaluated in
order. First, if there is no dependency for a given contract expression,
the contract expression is evaluated at the time that the @racket[->i]
expression is evaluated rather than the time when the function is called or
returns.  These dependency-free contract expressions are evaluated in the
order in which they are listed.
@;
Second, the dependent contract sub-expressions are evaluated when the
contracted function is called or returns in some order that satisfies the
dependencies. That is, if a contract for an argument depends on the value
of some other contract, the former is evaluated first (so that the
argument, with its contract checked, is available for the other).  When
there is no dependency between two arguments (or the result and an
argument), then the contract that appears earlier in the source text is
evaluated first.

Finally, if all of the identifier positions of the range
contract are @racket[_]s (underscores), then the range contract expressions
are evaluated when the function is called and the underscore is not bound
in the range, after the argument contracts are evaluated and
checked. Otherwise, the range expressions are evaluated when the function
returns.

If there are optional arguments that are not supplied, then
the corresponding variables will be bound to a special value
called @racket[the-unsupplied-arg] value.
}

@defform*/subs[#:literals (any values)
[(->d (mandatory-dependent-dom ...)
      dependent-rest
      pre-condition
      dependent-range
      post-condition)
 (->d (mandatory-dependent-dom ...)
      (optional-dependent-dom ...)
      dependent-rest
      pre-condition
      dependent-range
      post-condition)]
([mandatory-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [optional-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [dependent-rest (code:line) (code:line #:rest id rest-expr)]
 [pre-condition (code:line) (code:line #:pre boolean-expr) (code:line #:pre-cond boolean-expr)]
 [dependent-range any
                  [_ range-expr]
                  (values [_ range-expr] ...)
                  [id range-expr]
                  (values [id range-expr] ...)]
 [post-condition (code:line) (code:line #:post-cond boolean-expr)]
)]{

This contract is here for backwards compatibility; any new code should
use @racket[->i] instead.

This contract is similar to @racket[->i], but is ``lax'', meaning
that it does not enforce contracts internally. For example, using
this contract
@racketblock[(->d ([f (-> integer? integer?)])
                  #:pre
                  (zero? (f #f))
                  any)]
will allow @racket[f] to be called with @racket[#f], trigger whatever bad
behavior the author of @racket[f] was trying to prohibit by insisting that
@racket[f]'s contract accept only integers.

The @racket[#:pre-cond] and @racket[#:post-cond] keywords are aliases for
@racket[#:pre] and @racket[#:post] and are provided for backwards compatibility.

}

@defform*/subs[#:literals (any values ->)
                          [(case-> (-> dom-expr ... rest range) ...)]
                          ([rest (code:line) (code:line #:rest rest-expr)]
                           [range range-expr (values range-expr ...) any])]{
  This contract form is designed to match
  @racket[case-lambda]. Each argument to @racket[case->] is a
  contract that governs a clause in the
  @racket[case-lambda]. If the @racket[#:rest] keyword is
  present, the corresponding clause must accept an arbitrary
  number of arguments. The @racket[range] specification is
  just like that for @racket[->] and @racket[->*].
  
  For example, this contract matches a function with two
  cases, one that accepts an integer, returning void, and one
  that accepts no arguments and returns an integer.
  @racketblock[(case-> (-> integer? void?)
                       (-> integer?))]
  Such a contract could be used to guard a function that controls
  access to a single shared integer.
}

@defproc[(dynamic->* 
          [#:mandatory-domain-contracts mandatory-domain-contracts (listof contract?) '()]
          [#:optional-domain-contracts optional-domain-contracts (listof contract?) '()]
          [#:mandatory-keywords mandatory-keywords (listof keyword?) '()]
          [#:mandatory-keyword-contracts mandatory-keyword-contracts (listof contract?) '()]
          [#:optional-keywords optional-keywords (listof keyword?) '()]
          [#:optional-keyword-contracts optional-keyword-contracts (listof contract?) '()]
          [#:rest-contract rest-contract (or/c #f contract?) #f]
          [#:range-contracts range-contracts (or/c #f (listof contract?))])
         contract?]{
  Like @racket[->*], except the number of arguments and results can be computed
  at runtime, instead of being fixed at compile-time. Passing @racket[#f] as the
  @racket[#:range-contracts] argument produces a contract like one where @racket[any]
  is used with @racket[->] or @racket[->*].
  
  For many uses, @racket[dynamic->*]'s result is slower than @racket[->*] (or @racket[->]), 
  but for some it has comparable speed. The name of the contract returned by
  @racket[dynamic->*] uses the @racket[->] or @racket[->*] syntax.
}

@defform[(unconstrained-domain-> range-expr ...)]{

Constructs a contract that accepts a function, but makes no constraint
on the function's domain. The @racket[range-expr]s determine the number
of results and the contract for each result.

Generally, this contract must be combined with another contract to
ensure that the domain is actually known to be able to safely call the
function itself.

For example, the contract

@racketblock[
(provide
 (contract-out
  [f (->d ([size natural-number/c]
           [proc (and/c (unconstrained-domain-> number?)
                        (lambda (p)
                          (procedure-arity-includes? p size)))])
          ()
          [_ number?])]))
]

says that the function @racket[f] accepts a natural number
and a function. The domain of the function that @racket[f]
accepts must include a case for @racket[size] arguments,
meaning that @racket[f] can safely supply @racket[size]
arguments to its input.

For example, the following is a definition of @racket[f] that cannot
be blamed using the above contract:

@racketblock[
(define (f i g)
  (apply g (build-list i add1)))
]}

@defthing[predicate/c contract?]{
  Use this contract to indicate that some function
  is a predicate. It is semantically equivalent to
  @racket[(-> any/c boolean?)].

  This contract also includes an optimization so that functions returning
  @racket[#t] from @racket[struct-predicate-procedure?] are just returned directly, without
  being wrapped. This contract is used by @racket[provide/contract]'s
  @racket[struct] subform so that struct predicates end up not being wrapped.
}

@defthing[the-unsupplied-arg unsupplied-arg?]{
  Used by @racket[->i] (and @racket[->d]) to bind
  optional arguments that are not supplied by a call site.
}

@defproc[(unsupplied-arg? [v any/c]) boolean?]{
  A predicate to determine whether @racket[v] is
  @racket[the-unsupplied-arg].
}


@section[#:tag "parametric-contracts"]{Parametric Contracts}
@defmodule*/no-declare[(racket/contract/parametric)]
@declare-exporting-ctc[racket/contract/parametric]

The most convenient way to use parametric contract is to use
@racket[contract-out]'s @racket[#:exists] keyword.
The @racketmodname[racket/contract/parametric] provides a few more,
general-purpose parametric contracts.

@defform[(parametric->/c (x ...) c)]{

Creates a contract for parametric polymorphic functions.  Each function is
protected by @racket[c], where each @racket[x] is bound in @racket[c] and refers
to a polymorphic type that is instantiated each time the function is applied.

At each application of a function, the @racket[parametric->/c] contract constructs
a new opaque wrapper for each @racket[x]; values flowing into the polymorphic
function (i.e. values protected by some @racket[x] in negative position with
respect to @racket[parametric->/c]) are wrapped in the corresponding opaque
wrapper.  Values flowing out of the polymorphic function (i.e. values protected
by some @racket[x] in positive position with respect to @racket[parametric->/c])
are checked for the appropriate wrapper.  If they have it, they are unwrapped;
if they do not, a contract violation is signaled.

@examples[#:eval (contract-eval) #:once
(define/contract (check x y)
  (parametric->/c [X] (boolean? X . -> . X))
  (if (or (not x) (equal? y 'surprise))
      'invalid
      y))
(check #t 'ok)
(eval:error (check #f 'ignored))
(check #t 'surprise)
]
}

@defproc[(new-∀/c [name (or/c symbol? #f) #f]) contract?]{
  Constructs a new universal contract.

  Universal contracts accept all values when in negative positions (e.g., function
  inputs) and wrap them in an opaque struct, hiding the precise value.
  In positive positions (e.g. function returns),
  a universal contract accepts only values that were previously accepted
  in negative positions (by checking for the wrappers).

  The name is used to identify the contract in error messages and defaults
  to a name based on the lexical context of @racket[new-∀/c].

  For example, this contract:
  @racketblock[(let ([a (new-∀/c 'a)])
                 (-> a a))]
  describes the identity function (or a non-terminating function).
  That is, the first use of the @racket[a] appears in a
  negative position and thus inputs to that function are wrapped with an opaque struct.
  Then, when the function returns, it is checked to determine whether the result is wrapped, since
  the second @racket[a] appears in a positive position.

  The @racket[new-∀/c] contract constructor is dual to @racket[new-∃/c].
}

@defproc[(new-∃/c [name (or/c symbol? #f) #f]) contract?]{
  Constructs a new existential contract.

  Existential contracts accept all values when in positive positions (e.g., function
  returns) and wrap them in an opaque struct, hiding the precise value.
  In negative positions (e.g. function inputs),
  they accepts only values that were previously accepted in positive positions (by checking
  for the wrappers).

  The name is used to identify the contract in error messages and defaults
  to a name based on the lexical context of @racket[new-∀/c].

  For example, this contract:
  @racketblock[(let ([a (new-∃/c 'a)])
                 (-> (-> a a)
                     any/c))]
  describes a function that accepts the identity function (or a non-terminating function)
  and returns an arbitrary value. That is, the first use of the @racket[a] appears in a
  positive position and thus inputs to that function are wrapped with an opaque struct.
  Then, when the function returns, it is checked to see if the result is wrapped, since
  the second @racket[a] appears in a negative position.

  The @racket[new-∃/c] construct constructor is dual to @racket[new-∀/c].
}




@; ------------------------------------------------------------------------

@section{Lazy Data-structure Contracts}

@defform[(contract-struct id (field-id ...))]{

  @deprecated[@racket[struct]]{Lazy struct contracts no longer require a separate
              struct declaration; instead @racket[struct/dc]
              and @racket[struct/c] work directly with
              @racket[struct] and @racket[define-struct].
  }

Like @racket[struct], but with two differences:
they do not
define field mutators, and they define two contract constructors:
@racket[id]@racketidfont{/c} and @racket[id]@racketidfont{/dc}. The
first is a procedure that accepts as many arguments as there are
fields and returns a contract for struct values whose fields match the
arguments. The second is a syntactic form that also produces contracts
on the structs, but the contracts on later fields may depend on the
values of earlier fields.

The generated contract combinators are @italic{lazy}: they only verify
the contract holds for the portion of some data structure that is
actually inspected. More precisely, a lazy data structure contract is
not checked until a selector extracts a field of a struct.

@specsubform/subs[
(#,(elem (racket id) (racketidfont "/dc")) field-spec ...)

([field-spec
  [field-id contract-expr]
  [field-id (field-id ...) contract-expr]])
]{

In each @racket[field-spec] case, the first @racket[field-id]
specifies which field the contract applies to; the fields must be
specified in the same order as the original
@racket[contract-struct]. The first case is for when the
contract on the field does not depend on the value of any other
field. The second case is for when the contract on the field does
depend on some other fields, and the parenthesized @racket[field-id]s
indicate which fields it depends on; these dependencies can only be to
earlier fields.}}

@defform[(define-contract-struct id (field-id ...))]{
  @deprecated[@racket[struct]]{Lazy struct contracts no longer require a separate
              struct declaration; instead @racket[struct/dc]
              and @racket[struct/c] work directly with
              @racket[struct] and @racket[define-struct].
  }

  Like @racket[contract-struct], but where the constructor's name is
  @racketidfont["make-"]@racket[id], much like @racket[define-struct].
}

@; ------------------------------------------------------------------------

@include-section["contracts-struct-prop.scrbl"]

@; ------------------------------------------------------------------------

@section[#:tag "attaching-contracts-to-values"]{Attaching Contracts to Values}
@declare-exporting-ctc[racket/contract/base]

@defform/subs[
#:literals (struct rename)
(contract-out p/c-item ...)
([p/c-item
  (struct id/super ((id contract-expr) ...)
    struct-option)
  (rename orig-id id contract-expr)
  (id contract-expr)
  (code:line #:∃ poly-variables)
  (code:line #:exists poly-variables)
  (code:line #:∀ poly-variables)
  (code:line #:forall poly-variables)]
 [poly-variables id (id ...)]
 [id/super id
           (id super-id)]
 [struct-option (code:line)
                #:omit-constructor])]{

A @racket[_provide-spec] for use in @racket[provide] (currently only for
the same @tech{phase level} as the @racket[provide] form; for example,
@racket[contract-out] cannot be nested within @racket[for-syntax]). Each @racket[id]
is provided from the module. In
addition, clients of the module must live up to the contract specified
by @racket[contract-expr] for each export.

The @racket[contract-out] form treats modules as units of
blame. The module that defines the provided variable is expected to
meet the positive (co-variant) positions of the contract. Each module
that imports the provided variable must obey the negative
(contra-variant) positions of the contract. Each @racket[contract-expr]
in a @racket[contract-out] form is effectively moved to the end of the
enclosing module, so a @racket[contract-expr] can refer to variables
that are defined later in the same module.

Only uses of the contracted variable outside the module are
checked. Inside the module, no contract checking occurs.

The @racket[rename] form of @racket[contract-out] exports the
first variable (the internal name) with the name specified by the
second variable (the external name).

The @racket[struct] form of @racket[contract-out]
provides a structure-type definition, and each field has a contract
that dictates the contents of the fields. The structure-type
definition must appear before the @racket[provide] clause within the
enclosing module. If the structure type has a parent, the second
@racket[struct] form (above) must be used, with the first name
referring to the structure type to export and the second name
referring to the parent structure type. Unlike a @racket[struct]
definition, however, all of the fields (and their contracts) must be
listed. The contract on the fields that the sub-struct shares with its
parent are only used in the contract for the sub-struct's constructor, and
the selector or mutators for the super-struct are not provided. The
exported structure-type name always doubles as a constructor, even if
the original structure-type name does not act as a constructor.
If the @racket[#:omit-constructor] option is present, the constructor
is not provided.

The @racket[#:∃], @racket[#:exists], @racket[#:∀], and @racket[#:forall]
clauses define new abstract contracts. The variables are bound in the
remainder of the @racket[contract-out] form to new contracts that hide
the values they accept and ensure that the exported functions are treated
parametrically. See @racket[new-∃/c] and @racket[new-∀/c] for details
on how the clauses hide the values.

The implementation of @racket[contract-out] uses
@racket[syntax-property] to attach properties to the code it generates
that records the syntax of the contracts in the fully expanded program.
Specifically, the symbol @racket['provide/contract-original-contract]
is bound to vectors of two elements, the exported identifier and a
syntax object for the expression that produces the contract controlling
the export.
}

@defform[(recontract-out id ...)]{
   A @racket[_provide-spec] for use in @racket[provide] (currently,
     just like @racket[contract-out], only for
     the same @tech{phase level} as the @racket[provide] form).
     
   It re-exports @racket[id], but with positive blame associated
   to the module containing @racket[recontract-out] instead of the
   location of the original site of @racket[id].
   
   This can be useful when a public module wants to export an
   identifier from a private module but where any contract violations
   should be reported in terms of the public module instead of the
   private one.
   
   @examples[#:eval (contract-eval) #:once
             (module private-implementation racket/base
               (require racket/contract)
               (define (recip x) (/ 1 x))
               (define (non-zero? x) (not (= x 0)))
               (provide/contract [recip (-> (and/c real? non-zero?)
                                            (between/c -1 1))]))
             (module public racket/base
               (require racket/contract
                        'private-implementation)
               (provide (recontract-out recip)))
             
             (require 'public)
             (eval:error (recip +nan.0))]
   
   Replacing the use of @racket[recontract-out] with just
   @racket[recip] would result in a contract violation blaming
   the private module.                              
}

@defform[(provide/contract p/c-item ...)]{

A legacy shorthand for @racket[(provide (contract-out p/c-item ...))],
except that a @racket[_contract-expr] within @racket[provide/contract]
is evaluated at the position of the @racket[provide/contract] form
instead of at the end of the enclosing module.}

@subsection{Nested Contract Boundaries}
@defmodule*/no-declare[(racket/contract/region)]
@declare-exporting-ctc[racket/contract/region]

@defform*/subs[
 [(with-contract blame-id (wc-export ...) free-var-list ... body ...+)
  (with-contract blame-id results-spec free-var-list ... body ...+)]
 ([wc-export
   (id contract-expr)]
  [result-spec
   (code:line #:result contract-expr)
   (code:line #:results (contract-expr ...))]
  [free-var-list
   (code:line)
   (code:line #:freevar id contract-expr)
   (code:line #:freevars ([id contract-expr] ...))])]{
Generates a local contract boundary.

The first @racket[with-contract] form cannot appear in expression position.
All names defined within the first @racket[with-contract] form are
visible externally, but those names listed in the @racket[wc-export]
list are protected with the corresponding contract.  The @racket[body] of
the form allows definition/expression interleaving if its context does.

The second @racket[with-contract] form must appear in expression position.
The final @racket[body] expression should return the same number of values
as the number of contracts listed in the @racket[result-spec], and each
returned value is contracted with its respective contract.  The sequence
of @racket[body] forms is treated as for @racket[let].

The @racket[blame-id] is used for the positive positions of
contracts paired with exported @racket[id]s.  Contracts broken
within the @racket[with-contract] @racket[body] will use the
@racket[blame-id] for their negative position.

If a free-var-list is given, then any uses of the free variables
inside the @racket[body] will be protected with contracts that
blame the context of the @racket[with-contract] form for the positive
positions and the @racket[with-contract] form for the negative ones.}

@(define furlongs->feet-eval (contract-eval))
@defform*[[(define/contract id contract-expr free-var-list init-value-expr)
 (define/contract (head args) contract-expr free-var-list body ...+)]]{
Works like @racket[define], except that the contract
@racket[contract-expr] is attached to the bound value.  For the
definition of @racket[head] and @racket[args], see @racket[define].
For the definition of @racket[free-var-list], see @racket[with-contract].

@examples[#:eval furlongs->feet-eval
  (define/contract distance (>=/c 0) 43.52)
  (define/contract (furlongs->feet fr)
    (-> real? real?)
    (* 660 fr))
  (code:comment "a contract violation expected here:")
  (eval:error (furlongs->feet "not a furlong"))
]

The @racket[define/contract] form treats the individual definition as
a contract region. The definition itself is responsible for positive
(co-variant) positions of the contract, and references to
@racket[id] outside of the definition must meet the negative
positions of the contract. Since the contract boundary is
between the definition and the surrounding context, references to
@racket[id] inside the @racket[define/contract] form are not checked.

@examples[#:eval (contract-eval) #:once
  (code:comment "an unsual predicate that prints when called")
  (define (printing-int? x)
    (displayln "I was called")
    (exact-integer? x))
  (define/contract (fact n)
    (-> printing-int? printing-int?)
    (if (zero? n)
        1
        (* n (fact (sub1 n)))))
  (code:line (fact 5) (code:comment "only prints twice, not for each recursive call"))
]

If a free-var-list is given, then any uses of the free variables
inside the @racket[body] will be protected with contracts that
blame the context of the @racket[define/contract] form for the positive
positions and the @racket[define/contract] form for the negative ones.

@examples[#:eval (contract-eval) #:once
  (define (integer->binary-string n)
    (number->string n 2))
  (define/contract (numbers->strings lst)
    (-> (listof number?) (listof string?))
    #:freevar integer->binary-string (-> exact-integer? string?)
    (code:comment "mistake, lst might contain inexact numbers")
    (map integer->binary-string lst))
  (eval:error (numbers->strings '(4.0 3.3 5.8)))
]}

@defform*[[(define-struct/contract struct-id ([field contract-expr] ...)
                                   struct-option ...)
           (define-struct/contract (struct-id super-struct-id)
                                   ([field contract-expr] ...)
                                   struct-option ...)]]{
Works like @racket[define-struct], except that the arguments to the constructor,
accessors, and mutators are protected by contracts.  For the definitions of
@racket[field] and @racket[struct-option], see @racket[define-struct].

The @racket[define-struct/contract] form only allows a subset of the
@racket[struct-option] keywords: @racket[#:mutable], @racket[#:transparent],
@racket[#:auto-value], @racket[#:omit-define-syntaxes], @racket[#:property] and
@racket[#:omit-define-values].

@examples[#:eval (contract-eval) #:once
(define-struct/contract fish ([color number?]))
(make-fish 5)
(eval:error (make-fish #f))

(define-struct/contract (salmon fish) ([ocean symbol?]))
(make-salmon 5 'atlantic)
(eval:error (make-salmon 5 #f))
(eval:error (make-salmon #f 'pacific))
]}

@defform[(invariant-assertion invariant-expr expr)]{
  Establishes an invariant of @racket[expr], determined by @racket[invariant-expr].
                   
  Unlike the specification of a contract, an
  @racket[invariant-assertion] does not establish a boundary
  between two parties. Instead, it simply attaches a logical assertion 
  to the value. Because the form uses contract machinery to check the
  assertion, the surrounding module is treated as the party to be blamed
  for any violations of the assertion. 
  
  This means, for example, that the assertion is checked on
  recursive calls, when an invariant is used on the right-hand
  side of a definition:
                              
  @examples[#:eval
            furlongs->feet-eval
            (define furlongss->feets
              (invariant-assertion
               (-> (listof real?) (listof real?))
               (λ (l)
                 (cond
                   [(empty? l) empty]
                   [else
                    (if (= 327 (car l))
                        (furlongss->feets (list "wha?"))
                        (cons (furlongs->feet (first l))
                              (furlongss->feets (rest l))))]))))
            
            (furlongss->feets (list 1 2 3))
            
            (eval:error (furlongss->feets (list 1 327 3)))]
  
  @history[#:added "6.0.1.11"]
  
}

@defidform[current-contract-region]{
  Bound by @racket[define-syntax-parameter], this contains
  information about the current contract region, used by
  the above forms to determine the candidates for blame
  assignment.
}

@subsection{Low-level Contract Boundaries}
@declare-exporting-ctc[racket/contract/base]

@defform[(define-module-boundary-contract id 
           orig-id 
           contract-expr
           pos-blame-party
           source-loc)
         #:grammar ([pos-blame-party (code:line)
                                     (code:line #:pos-source pos-source-expr)]
                    [source-loc (code:line)
                                (code:line #:srcloc srcloc-expr)])]{
  Defines @racket[id] to be @racket[orig-id], but with the contract
  @racket[contract-expr].
  
  The identifier @racket[id] is defined as a macro transformer that
  consults the context of its use to determine the name for negative
  blame assignment (using the entire module where a reference appears
  as the negative party).
  
  The positive party defaults to the module containing the use of
  @racket[define-module-boundary-contract], but can be specified explicitly
  via the @racket[#:pos-source] keyword.
  
  The source location used in the blame error messages for the location
  of the place where the contract was put on the value defaults to the
  source location of the use of @racket[define-module-boundary-contract],
  but can be specified via the @racket[#:srcloc] argument, in which case
  it can be any of the things that the third argument to @racket[datum->syntax]
  can be.
  
  @examples[#:eval (contract-eval) #:once
            (module server racket/base
              (require racket/contract/base)
              (define (f x) #f)
              (define-module-boundary-contract g f (-> integer? integer?))
              (provide g))
            (module client racket/base
              (require 'server)
              (define (clients-fault) (g #f))
              (define (servers-fault) (g 1))
              (provide servers-fault clients-fault))
            (require 'client)
            (eval:error (clients-fault))
            (eval:error (servers-fault))]

}

@defform*[[(contract contract-expr to-protect-expr
                     positive-blame-expr negative-blame-expr)
           (contract contract-expr to-protect-expr
                     positive-blame-expr negative-blame-expr
                     value-name-expr source-location-expr)]]{

The primitive mechanism for attaching a contract to a value. The
purpose of @racket[contract] is as a target for the expansion of some
higher-level contract specifying form.

The @racket[contract] expression adds the contract specified by
@racket[contract-expr] to the value produced by
@racket[to-protect-expr]. The result of a @racket[contract] expression
is the result of the @racket[to-protect-expr] expression, but with the
contract specified by @racket[contract-expr] enforced on
@racket[to-protect-expr].

The values of @racket[positive-blame-expr] and @racket[negative-blame-expr]
indicate how to assign blame for positive and negative positions of the contract
specified by @racket[contract-expr].  They may be any value, and are formatted
as by @racket[display] for purposes of contract violation error messages.

If specified, @racket[value-name-expr] indicates a name for the protected value
to be used in error messages.  If not supplied, or if @racket[value-name-expr]
produces @racket[#f], no name is printed.  Otherwise, it is also formatted as by
@racket[display]. More precisely, the @racket[value-name-expr] ends up in the
@racket[blame-name] field of the blame record, which is used as the first portion
of the error message.
@examples[#:eval (contract-eval) #:once
          (eval:error (contract integer? #f 'pos 'neg 'timothy #f))
          (eval:error (contract integer? #f 'pos 'neg #f #f))]

If specified, @racket[source-location-expr] indicates the source location
reported by contract violations.  The expression must produce a @racket[srcloc]
structure, @tech{syntax object}, @racket[#f], or a list or vector in the format
accepted by the third argument to @racket[datum->syntax].

}

@; ------------------------------------------------------------------------

@section{Building New Contract Combinators}

@defmodule*/no-declare[(racket/contract/combinator)]
@declare-exporting-ctc[racket/contract/combinator]


@deftogether[(
@defproc[(make-contract
          [#:name name any/c 'anonymous-contract]
          [#:first-order test (-> any/c any/c) (λ (x) #t)]
          [#:late-neg-projection
           late-neg-proj
           (or/c #f (-> blame? (-> any/c any/c any/c)))
           #f]
          [#:val-first-projection
           val-first-proj
           (or/c #f (-> blame? (-> any/c (-> any/c any/c))))
           #f]
          [#:projection proj (-> blame? (-> any/c any/c))
           (λ (b)
             (λ (x)
               (if (test x)
                 x
                 (raise-blame-error
                  b x
                  '(expected: "~a" given: "~e")
                  name x))))]
          [#:stronger stronger
                      (or/c #f (-> contract? contract? boolean?))
                      #f]
          [#:list-contract? is-list-contract? boolean? #f])
         contract?]
@defproc[(make-chaperone-contract
          [#:name name any/c 'anonymous-chaperone-contract]
          [#:first-order test (-> any/c any/c) (λ (x) #t)]
          [#:late-neg-projection
           late-neg-proj
           (or/c #f (-> blame? (-> any/c any/c any/c)))
           #f]
          [#:val-first-projection 
           val-first-proj
           (or/c #f (-> blame? (-> any/c (-> any/c any/c))))
           #f]
          [#:projection proj (-> blame? (-> any/c any/c))
           (λ (b)
             (λ (x)
               (if (test x)
                 x
                 (raise-blame-error
                  b x
                  '(expected: "~a" given: "~e")
                  name x))))]
          [#:stronger stronger
                      (or/c #f (-> contract? contract? boolean?))
                      #f]
          [#:list-contract? is-list-contract? boolean? #f])
         chaperone-contract?]
@defproc[(make-flat-contract
          [#:name name any/c 'anonymous-flat-contract]
          [#:first-order test (-> any/c any/c) (λ (x) #t)]
          [#:late-neg-projection
           late-neg-proj
           (or/c #f (-> blame? (-> any/c any/c any/c)))
           #f]
          [#:val-first-projection 
           val-first-proj
           (or/c #f (-> blame? (-> any/c (-> any/c any/c))))
           #f]
          [#:projection proj (-> blame? (-> any/c any/c))
           (λ (b)
             (λ (x)
               (if (test x)
                 x
                 (raise-blame-error
                  b x
                  '(expected: "~a" given: "~e")
                  name x))))]
          [#:stronger stronger
                      (or/c #f (-> contract? contract? boolean?))
                      #f]
          [#:list-contract? is-list-contract? boolean? #f])
         flat-contract?]
)]{

These functions build simple higher-order contracts, chaperone contracts, and flat contracts,
respectively.  They both take the same set of three optional arguments: a name,
a first-order predicate, and a blame-tracking projection.

The @racket[name] argument is any value to be rendered using @racket[display] to
describe the contract when a violation occurs.  The default name for simple
higher-order contracts is @racketresult[anonymous-contract], for chaperone
contracts is @racketresult[anonymous-chaperone-contract], and for flat
contracts is @racketresult[anonymous-flat-contract].

The first-order predicate @racket[test] can be used to determine which values
the contract applies to; this must be the set of values for which the
contract fails immediately without any higher-order wrapping.  This test is used
by @racket[contract-first-order-passes?], and indirectly by @racket[or/c]
and @racket[from-or/c] to determine which higher-order contract to wrap a
value with when there are multiple higher-order contracts to choose from.
The default test accepts any value. The predicate should be influenced by
the value of @racket[(contract-first-order-okay-to-give-up?)] (see it's documentation
for more explanation).

The @racket[late-neg-proj] defines the behavior of applying the contract. If it is
supplied, it accepts a blame object that does not have a value for
 the @racket[blame-negative] field. Then it must return a function that accepts
 both the value that is getting the contract and the name of the blame party, in
 that order. The result must either be the value (perhaps suitably wrapped
 with a @tech{chaperone} or @tech{impersonator} to enforce the contract), or
 signal a contract violation using @racket[raise-blame-error]. The default is
 @racket[#f].
 
The projection @racket[proj] and @racket[val-first-proj] are older mechanisms for
 defining the behavior of applying the contract.  The @racket[proj] argument
is a curried function of two arguments: the first application accepts a blame
object, and the second accepts a value to protect with the contract.  The
projection must either produce the value, suitably wrapped to enforce any
higher-order aspects of the contract, or signal a contract violation using
@racket[raise-blame-error].  The default projection produces an error when the
first-order test fails, and produces the value unchanged otherwise.
The @racket[val-first-proj] is like @racket[late-neg-proj], except with
an extra layer of currying.

At least one of the @racket[late-neg-proj], @racket[proj],
 @racket[val-first-proj], or @racket[first-order] must be non-@racket[#f].

The projection arguments (@racket[late-neg-proj], @racket[proj], and
 @racket[val-first-proj]) must be in sync with the @racket[test] argument.
 In particular, if the test argument returns @racket[#f] for some value,
 then the projections must raise a blame error for that value.

Projections for chaperone contracts must produce a value that passes
@racket[chaperone-of?] when compared with the original, uncontracted value.
Projections for flat contracts must fail precisely when the first-order test
does, and must produce the input value unchanged otherwise.  Applying a flat
contract may result in either an application of the predicate, or the
projection, or both; therefore, the two must be consistent.  The existence of a
separate projection only serves to provide more specific error messages.  Most
flat contracts do not need to supply an explicit projection.

The @racket[stronger] argument is used to implement @racket[contract-stronger?]. The
first argument is always the contract itself and the second argument is whatever
was passed as the second argument to @racket[contract-stronger?]. If no
@racket[stronger] argument is supplied, then a default that compares its arguments
with @racket[equal?] is used.

The @racket[is-list-contract?] argument is used by the @racket[list-contract?] predicate
to determine if this is a contract that accepts only @racket[list?] values.

@examples[#:eval (contract-eval) #:once
(define int/c
  (make-flat-contract #:name 'int/c #:first-order integer?))
(contract int/c 1 'positive 'negative)
(eval:error (contract int/c "not one" 'positive 'negative))
(int/c 1)
(int/c "not one")
(define int->int/c
  (make-contract
   #:name 'int->int/c
   #:first-order
   (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
   #:projection
   (λ (b)
     (let ([domain ((contract-projection int/c) (blame-swap b))]
           [range ((contract-projection int/c) b)])
       (λ (f)
         (if (and (procedure? f) (procedure-arity-includes? f 1))
           (λ (x) (range (f (domain x))))
           (raise-blame-error
            b f
            '(expected "a function of one argument" given: "~e")
            f)))))))
(eval:error (contract int->int/c "not fun" 'positive 'negative))
(define halve
  (contract int->int/c (λ (x) (/ x 2)) 'positive 'negative))
(halve 2)
(eval:error (halve 1/2))
(eval:error (halve 1))
]

@history[#:changed "6.0.1.13" @list{Added the @racket[#:list-contract?] argument.}]
}

@defproc[(build-compound-type-name [c/s any/c] ...) any]{

Produces an S-expression to be used as a name
for a contract. The arguments should be either contracts or
symbols. It wraps parentheses around its arguments and
extracts the names from any contracts it is supplied with.}

@defproc[(coerce-contract [id symbol?] [x any/c]) contract?]{

Converts a regular Racket value into an instance of a contract struct,
converting it according to the description of @tech{contracts}.

If @racket[x] is not one of the coercible values,
@racket[coerce-contract] signals an error, using the first argument in
the error message.}

@defproc[(coerce-contracts [id symbol?] [xs (listof any/c)]) (listof contract?)]{

Coerces all of the arguments in 'xs' into contracts (via
@racket[coerce-contract/f]) and signals an error if any of them are not
contracts.  The error messages assume that the function named by
@racket[id] got @racket[xs] as its entire argument list.
}

@defproc[(coerce-chaperone-contract [id symbol?] [x any/c]) chaperone-contract?]{
  Like @racket[coerce-contract], but requires the result
  to be a chaperone contract, not an arbitrary contract.
}

@defproc[(coerce-chaperone-contracts [id symbol?] [x (listof any/c)])
         (listof chaperone-contract?)]{
  Like @racket[coerce-contracts], but requires the results
  to be chaperone contracts, not arbitrary contracts.
}

@defproc[(coerce-flat-contract [id symbol?] [x any/c]) flat-contract?]{
  Like @racket[coerce-contract], but requires the result
  to be a flat contract, not an arbitrary contract.
}

@defproc[(coerce-flat-contracts [id symbol?] [x (listof any/c)]) (listof flat-contract?)]{
  Like @racket[coerce-contracts], but requires the results
  to be flat contracts, not arbitrary contracts.
}

@defproc[(coerce-contract/f [x any/c]) (or/c contract? #f)]{
  Like @racket[coerce-contract], but returns @racket[#f] if
  the value cannot be coerced to a contract.
}

@defproc[(get/build-val-first-projection [c contract?])
         (-> contract? blame? (-> any/c (-> any/c any/c)))]{
  Returns the @racket[_val-first] projection for @racket[c].
              
  See @racket[make-contract] for more details.
  
@history[#:added "6.1.1.5"]
}

@defproc[(get/build-late-neg-projection [c contract?])
         (-> contract? blame? (-> any/c any/c any/c))]{
 Returns the @racket[_late-neg] projection for @racket[c].
              
 If @racket[c] does not have a @racket[_late-neg] contract,
 then this function uses the original projection for it
 and logs a warning to the @racket['racket/contract] logger.
 
 See @racket[make-contract] for more details.
  
 @history[#:added "6.2.900.11"]
}

@defparam[skip-projection-wrapper? wrap? boolean? #:value #f]{
 The functions @racket[make-chaperone-contract] and 
 @racket[build-chaperone-contract-property] wrap their
 arguments to ensure that the result of the projections
 are chaperones of the input. This layer of wrapping can,
 in some cases, introduce unwanted overhead into contract
 checking. If this parameter's value is @racket[#t]
 during the dynamic extent of the call to either of those
 functions, the wrapping (and thus the checks) are skipped.
}

@subsection{Blame Objects}

@defproc[(blame? [x any/c]) boolean?]{
 This predicate recognizes @|blame-objects|.
}

@defproc[(raise-blame-error [b blame?]
                            [x any/c]
                            [fmt (or/c string?
                                       (listof (or/c string?
                                                     'given 'given:
                                                     'expected 'expected:)))]
                            [v any/c] ...)
         none/c]{

Signals a contract violation.  The first argument, @racket[b], records the
current blame information, including positive and negative parties, the name of
the contract, the name of the value, and the source location of the contract
application.  The second argument, @racket[x], is the value that failed to
satisfy the contract.

The remaining arguments are a format string,
@racket[fmt], and its arguments, @racket[v ...], specifying an error message
specific to the precise violation.

If @racket[fmt] is a list, then the elements are concatenated together
(with spaces added, unless there are already spaces at the ends of the strings),
after first replacing symbols with either their string counterparts, or
replacing @racket['given] with @racket["produced"] and
@racket['expected] with @racket["promised"], depending on whether or not
the @racket[b] argument has been swapped or not (see @racket[blame-swap]).

If @racket[fmt] contains the symbols @racket['given:] or @racket['expected:],
they are replaced like @racket['given:] and @racket['expected:] are, but
the replacements are prefixed with the string @racket["\n  "] to conform
to the error message guidelines in @secref["err-msg-conventions"].

}

@defproc[(blame-add-context [blame blame?]
                            [context (or/c string? #f)]
                            [#:important important (or/c string? #f) #f]
                            [#:swap? swap? boolean? #f])
         blame?]{
  Adds some context information to blame error messages
  that explicates which portion of the contract failed
  (and that gets rendered by @racket[raise-blame-error]).

  The @racket[context] argument describes one layer of the
  portion of the contract, typically of the form @racket["the 1st argument of"]
  (in the case of a function contract)
  or @racket["a conjunct of"] (in the case of an @racket[and/c] contract).

  For example, consider this contract violation:
  @examples[#:label #f #:eval (contract-eval) #:once
(define/contract f
  (list/c (-> integer? integer?))
  (list (λ (x) x)))

(eval:error ((car f) #f))
]
It shows that the portion of the contract being violated is the first
occurrence of @racket[integer?], because the @racket[->] and
the @racket[list/c] combinators each internally called
@racket[blame-add-context] to add the two lines following
``in'' in the error message.

The @racket[important] argument is used to build the beginning part
of the contract violation. The last @racket[important] argument that
gets added to a @|blame-object| is used. The @racket[class/c] contract
adds an important argument, as does the @racket[->] contract (when
@racket[->] knows the name of the function getting the contract).

The @racket[swap?] argument has the effect of calling @racket[blame-swap]
while adding the layer of context, but without creating an extra
@|blame-object|.

The context information recorded in blame structs keeps track of
combinators that do not add information, and add the string @racket["..."]
for them, so programmers at least see that there was some context
they are missing in the error messages. Accordingly, since there are
combinators that should not add any context (e.g., @racket[recursive-contract]),
passing @racket[#f] as the context string argument avoids adding the
@racket["..."] string.
}

@deftogether[(
@defproc[(blame-positive [b blame?]) any/c]
@defproc[(blame-negative [b blame?]) any/c]
)]{
These functions produce printable descriptions of the current positive and
negative parties of a @|blame-object|.
}

@defproc[(blame-contract [b blame?]) any/c]{
This function produces a description of the contract associated with a blame
object (the result of @racket[contract-name]).
}

@defproc[(blame-value [b blame?]) any/c]{
This function produces the name of the value to which the contract was applied,
or @racket[#f] if no name was provided.
}

@defproc[(blame-source [b blame?]) srcloc?]{
This function produces the source location associated with a contract.  If no
source location was provided, all fields of the structure will contain
@racket[#f].
}

@defproc[(blame-swap [b blame?]) blame?]{
This function swaps the positive and negative parties of a @|blame-object|.
(See also @racket[blame-add-context].)
}

@deftogether[(
@defproc[(blame-original? [b blame?]) boolean?]
@defproc[(blame-swapped? [b blame?]) boolean?]
)]{

These functions report whether the current blame of a given @|blame-object| is the
same as in the original contract invocation (possibly of a compound contract
containing the current one), or swapped, respectively.  Each is the negation of
the other; both are provided for convenience and clarity.

}

@defproc[(blame-replace-negative [b blame?] [neg any/c]) blame?]{
  Produces a @racket[blame?] object just like @racket[b] except
             that it uses @racket[neg] instead of the negative
             position @racket[b] has.
}

@defproc[(blame-update [b blame?] [pos any/c] [neg any/c]) blame?]{
  Produces a @racket[blame?] object just like @racket[b] except
             that it adds @racket[pos] and @racket[neg] to the positive
             and negative parties of @racket[b] respectively.
}

@defproc[(blame-missing-party? [b blame?]) boolean?]{
 Returns @racket[#t] when @racket[b] does not have both parties.
}

@defproc[(blame-add-missing-party [b (and/c blame? blame-missing-party?)]
                                  [missing-party any/c])
         (and/c blame? (not/c blame-missing-party?))]{
 Produces a new blame object like @racket[b], except that the missing
 party is replaced with @racket[missing-party].
}


@defstruct[(exn:fail:contract:blame exn:fail:contract) ([object blame?])]{
  This exception is raised to signal a contract error. The @racket[object]
  field contains a @|blame-object| associated with a contract violation.
}

@defparam[current-blame-format
          proc
          (-> blame? any/c string? string?)]{

A @tech{parameter} that is used when constructing a
contract violation error. Its value is procedure that
accepts three arguments:
@itemize[
@item{the @|blame-object| for the violation,}
@item{the value that the contract applies to, and}
@item{a message indicating the kind of violation.}]
The procedure then
returns a string that is put into the contract error
message. Note that the value is often already included in
the message that indicates the violation.

@examples[#:eval (contract-eval) #:once
(define (show-blame-error blame value message)
  (string-append
   "Contract Violation!\n"
   (format "Guilty Party: ~a\n" (blame-positive blame))
   (format "Innocent Party: ~a\n" (blame-negative blame))
   (format "Contracted Value Name: ~a\n" (blame-value blame))
   (format "Contract Location: ~s\n" (blame-source blame))
   (format "Contract Name: ~a\n" (blame-contract blame))
   (format "Offending Value: ~s\n" value)
   (format "Offense: ~a\n" message)))
(current-blame-format show-blame-error)
(define/contract (f x)
  (-> integer? integer?)
  (/ x 2))
(f 2)
(eval:error (f 1))
(eval:error (f 1/2))
]

}

@subsection{Contracts as structs}

@para{
The property @racket[prop:contract] allows arbitrary structures to act as
contracts.  The property @racket[prop:chaperone-contract] allows arbitrary
structures to act as chaperone contracts; @racket[prop:chaperone-contract]
inherits @racket[prop:contract], so chaperone contract structures may also act
as general contracts.  The property @racket[prop:flat-contract] allows arbitrary structures
to act as flat contracts; @racket[prop:flat-contract] inherits both
@racket[prop:chaperone-contract] and @racket[prop:procedure], so flat contract structures
may also act as chaperone contracts, as general contracts, and as predicate procedures.
}

@deftogether[(
@defthing[prop:contract struct-type-property?]
@defthing[prop:chaperone-contract struct-type-property?]
@defthing[prop:flat-contract struct-type-property?]
)]{
These properties declare structures to be contracts or flat contracts,
respectively.  The value for @racket[prop:contract] must be a @tech{contract
property} constructed by @racket[build-contract-property]; likewise, the value
for @racket[prop:chaperone-contract] must be a @tech{chaperone contract property}
constructed by @racket[build-chaperone-contract-property] and the value
for @racket[prop:flat-contract] must be a @tech{flat contract property}
constructed by @racket[build-flat-contract-property].
}

@deftogether[(
@defthing[prop:contracted struct-type-property?]
@defthing[impersonator-prop:contracted impersonator-property?]
)]{
These properties attach a contract value to the protected structure,
chaperone, or impersonator value.  The function @racket[has-contract?]
returns @racket[#t] for values that have one of these properties, and
@racket[value-contract] extracts the value from the property (which
is expected to be the contract on the value).
}

@deftogether[(
@defthing[prop:blame struct-type-property?]
@defthing[impersonator-prop:blame impersonator-property?]
)]{
These properties attach a blame information to the protected structure,
chaperone, or impersonator value.  The function @racket[has-blame?]
returns @racket[#t] for values that have one of these properties, and
@racket[value-blame] extracts the value from the property.

The value is expected to be the blame record for the contract on the value or
a @racket[cons]-pair of a blame record with a missing party and the missing
party. The @racket[value-blame] function reassembles the arguments of the pair
into a complete blame record using @racket[blame-add-missing-party]. If
the value has one of the properties, but the value is not a blame object
or a pair whose @racket[car] position is a blame object, then @racket[has-blame?]
returns @racket[#f] but @racket[value-blame] returns @racket[#f].
}

@deftogether[(
@defproc[(build-flat-contract-property
          [#:name
           get-name
           (-> contract? any/c)
           (λ (c) 'anonymous-flat-contract)]
          [#:first-order
           get-first-order
           (-> contract? (-> any/c boolean?))
           (λ (c) (λ (x) #t))]
          [#:late-neg-projection
           late-neg-proj
           (or/c #f (-> contract? (-> blame? (-> any/c any/c any/c))))
           #f]
          [#:val-first-projection 
           val-first-proj
           (or/c #f (-> contract? blame? (-> any/c (-> any/c any/c))))
           #f]
          [#:projection
           get-projection
           (-> contract? (-> blame? (-> any/c any/c)))
           (λ (c)
             (λ (b)
               (λ (x)
                 (if ((get-first-order c) x)
                     x
                     (raise-blame-error
                      b x '(expected: "~a" given: "~e") 
                      (get-name c) x)))))]
          [#:stronger
           stronger
           (or/c (-> contract? contract? boolean?) #f)
           #f]
          [#:generate
           generate
           (->i ([c contract?])
                ([generator
                  (c)
                  (-> (and/c positive? real?)
                      (or/c (-> (or/c contract-random-generate-fail? c))
                            #f))]))
           (λ (c) (λ (fuel) #f))]
          [#:exercise
           exercise
           (->i ([c contract?])
                ([result
                  (c)
                  (-> (and/c positive? real?)
                      (values
                       (-> c void?)
                       (listof contract?)))]))
           (λ (c) (λ (fuel) (values void '())))]
          [#:list-contract? is-list-contract? (-> contract? boolean?) (λ (c) #f)])
         flat-contract-property?]
@defproc[(build-chaperone-contract-property
          [#:name
           get-name
           (-> contract? any/c)
           (λ (c) 'anonymous-chaperone-contract)]
          [#:first-order
           get-first-order
           (-> contract? (-> any/c boolean?))
           (λ (c) (λ (x) #t))]
          [#:late-neg-projection
           late-neg-proj
           (or/c #f (-> contract? blame? (-> any/c any/c any/c)))
           #f]
          [#:val-first-projection 
           val-first-proj
           (or/c #f (-> contract? blame? (-> any/c (-> any/c any/c))))
           #f]
          [#:projection
           get-projection
           (-> contract? (-> blame? (-> any/c any/c)))
           (λ (c)
             (λ (b)
               (λ (x)
                 (if ((get-first-order c) x)
                     x
                     (raise-blame-error
                      b x '(expected: "~a" given: "~e") 
                      (get-name c) x)))))]
          [#:stronger
           stronger
           (or/c (-> contract? contract? boolean?) #f)
           #f]
          [#:generate
           generate
           (->i ([c contract?])
                ([generator
                  (c)
                  (-> (and/c positive? real?)
                      (or/c (-> (or/c contract-random-generate-fail? c))
                            #f))]))
           (λ (c) (λ (fuel) #f))]
          [#:exercise
           exercise
           (->i ([c contract?])
                ([result
                  (c)
                  (-> (and/c positive? real?)
                      (values
                       (-> c void?)
                       (listof contract?)))]))
           (λ (c) (λ (fuel) (values void '())))]
          [#:list-contract? is-list-contract? (-> contract? boolean?) (λ (c) #f)])
         chaperone-contract-property?]
@defproc[(build-contract-property
          [#:name
           get-name
           (-> contract? any/c)
           (λ (c) 'anonymous-contract)]
          [#:first-order
           get-first-order
           (-> contract? (-> any/c boolean?))
           (λ (c) (λ (x) #t))]
          [#:late-neg-projection
           late-neg-proj
           (or/c #f (-> contract? blame? (-> any/c any/c any/c)))
           #f]
          [#:val-first-projection 
           val-first-proj
           (or/c #f (-> contract? blame? (-> any/c (-> any/c any/c))))
           #f]
          [#:projection
           get-projection
           (-> contract? (-> blame? (-> any/c any/c)))
           (λ (c)
             (λ (b)
               (λ (x)
                 (if ((get-first-order c) x)
                     x
                     (raise-blame-error
                      b x '(expected: "~a" given: "~e") 
                      (get-name c) x)))))]
          [#:stronger
           stronger
           (or/c (-> contract? contract? boolean?) #f)
           #f]
          [#:generate
           generate
           (->i ([c contract?])
                ([generator
                  (c)
                  (-> (and/c positive? real?)
                      (or/c (-> (or/c contract-random-generate-fail? c))
                            #f))]))
           (λ (c) (λ (fuel) #f))]
          [#:exercise
           exercise
           (->i ([c contract?])
                ([result
                  (c)
                  (-> (and/c positive? real?)
                      (values
                       (-> c void?)
                       (listof contract?)))]))
           (λ (c) (λ (fuel) (values void '())))]
          [#:list-contract? is-list-contract? (-> contract? boolean?) (λ (c) #f)])
         contract-property?])]{
   
These functions build the arguments for @racket[prop:contract],
@racket[prop:chaperone-contract], and @racket[prop:flat-contract], respectively.

A @deftech{contract property} specifies the behavior of a structure when used as
a contract.  It is specified in terms of seven properties:
@itemlist[
  @item{@racket[get-name] which produces a description to @racket[write] as part
   of a contract violation;}
  @item{@racket[get-first-order], which produces a first-order predicate to be
   used by @racket[contract-first-order-passes?];}
  @item{@racket[get-late-neg-projection], which produces a blame-tracking projection
   defining the behavior of the contract (The @racket[get-projection]
   and @racket[get-val-first-projection] arguments also specify the projection,
   but using a different signature. They are here for backwards compatibility.);}
  @item{@racket[stronger], a predicate that determines whether this
   contract (passed in the first argument) is stronger than some other
   contract (passed in the second argument) and whose default always
   returns @racket[#f];}
  @item{@racket[generate], which returns a thunk that generates random values
   matching the contract (using @racket[contract-random-generate-fail])
   to indicate failure) or @racket[#f] to indicate that random
   generation for this contract isn't supported;}
  @item{@racket[exercise], which returns a function that exercises values
   matching the contract (e.g., if it is a function contract, it may call
   the function) and a list of contracts whose values will be generated
   by this process;}
  @item{and @racket[is-list-contract?], which is used by @racket[flat-contract?]
   to determine if this contract accepts only @racket[list?]s.}
]

At least one of the @racket[late-neg-proj], @racket[proj],
@racket[val-first-proj], or @racket[first-order] must be non-@racket[#f].

These accessors are passed as (optional) keyword arguments to
@racket[build-contract-property], and are applied to instances of the
appropriate structure type by the contract system.  Their results are used
analogously to the arguments of @racket[make-contract].

A @deftech{chaperone contract property} specifies the behavior of a structure
when used as a chaperone contract.  It is specified using
@racket[build-chaperone-contract-property], and accepts exactly the same set of
arguments as @racket[build-contract-property].  The only difference is that the
projection accessor must return a value that passes @racket[chaperone-of?] when
compared with the original, uncontracted value.

A @deftech{flat contract property} specifies the behavior of a structure when
used as a flat contract.  It is specified using
@racket[build-flat-contract-property], and accepts exactly the same set of
arguments as @racket[build-contract-property].  The only difference is that the
projection accessor is expected not to wrap its argument in a higher-order
fashion, analogous to the constraint on projections in
@racket[make-flat-contract].

@history[#:changed "6.0.1.13" @list{Added the @racket[#:list-contract?] argument.}
         #:changed "6.1.1.4" 
         @list{Allow @racket[generate] to return @racket[contract-random-generate-fail]}]
}

@deftogether[(
@defproc[(contract-property? [x any/c]) boolean?]
@defproc[(chaperone-contract-property? [x any/c]) boolean?]
@defproc[(flat-contract-property? [x any/c]) boolean?]
)]{
These predicates detect whether a value is a @tech{contract property},
@tech{chaperone contract property}, or a
@tech{flat contract property}, respectively.
}

@subsection{Obligation Information in Check Syntax}

@seclink[#:doc '(lib "scribblings/drracket/drracket.scrbl")
"buttons" #:indirect? #t]{Check Syntax} in DrRacket shows obligation information for
contracts according to @racket[syntax-property]s that the contract combinators
leave in the expanded form of the program. These properties indicate
where contracts appear in the source and where the positive and negative
positions of the contracts appear.

To make Check Syntax show obligation information for your new contract
combinators, use the following properties (some helper macros and functions
are below):

@itemize[@item{@racketblock0['racket/contract:contract :
                             (vector/c symbol? (listof syntax?) (listof syntax?))]
                This property should be attached to the result of a transformer
                that implements a contract combinator. It signals to Check Syntax
                that this is where a contract begins.

                The first element in the
                vector should be a unique (in the sense of @racket[eq?]) value
                that Check Syntax can use a tag to match up this contract with
                its subpieces (specified by the two following syntax properties).

                The second and third elements of the vector are syntax objects
                from pieces of the contract, and Check Syntax will color them.
                The first list should contain subparts that are the responsibility
                of parties (typically modules) that provide implementations of the contract.
                The second list should contain subparts that are the
                responsibility of clients.

                For example, in @racket[(->* () #:pre #t any/c #:post #t)],
                the @racket[->*] and the @racket[#:post] should be in the first
                list and @racket[#:pre] in the second list.}

          @item{@racketblock0['racket/contract:negative-position : symbol?]
                 This property should be attached to sub-expressions of
                 a contract combinator that are expected to be other contracts.
                 The value of the property should be the key (the first element from
                 the vector for the @racket['racket/contract:contract] property)
                 indicating which contract this is.

                 This property should be used when the expression's value is a contract
                 that clients are responsible for. }

          @item{@racketblock0['racket/contract:positive-position : symbol?]
                 This form is just like @racket['racket/contract:negative-position],
                 except that it should be used when the expression's value is
                 a contract that the original party should be responsible for.
                 }

          @item{@racketblock0['racket/contract:contract-on-boundary : symbol?]
                 The presence of this property tells Check Syntax that it
                 should start coloring from this point. It expects the expression
                 to be a contract
                 (and, thus, to have the @racket['racket/contract:contract] property);
                 this property indicates that this contract is on a (module) boundary.

                 (The value of the property is not used.)
                 }

          @item{@racketblock0['racket/contract:internal-contract : symbol?]

                 Like @racket['racket/contract:contract-on-boundary], the presence
                 of this property triggers coloring, but this is meant for use
                 when the party (module) containing the contract (regardless of whether
                 or not this module exports anything matching the contract)
                 can be blamed for violating the contract. This comes into play
                 for @racket[->i] contracts, since the contract itself has
                 access to values under contract via the dependency.
                 }
         ]

@defform/subs[(define/final-prop header body ...)
              ([header main-id
                       (main-id id ...)
                       (main-id id ... . id)])]{
  The same as @racket[(define header body ...)], except that uses of
              @racket[main-id] in the header are annotated
              with the @racket['racket/contract:contract] property
              (as above).
}

@defform/subs[(define/subexpression-pos-prop header body ...)
              ([header main-id
                       (main-id id ...)
                       (main-id id ... . id)])]{
  The same as @racket[(define header body ...)], except that uses of
              @racket[main-id] in the header are annotated
              with the @racket['racket/contract:contract] property
              (as above) and arguments are annotated with the
              @racket['racket/contract:positive-position] property.
}


@; ------------------------------------------------------------------------

@subsection{Utilities for Building New Combinators}

@defproc[(contract-stronger? [x contract?] [y contract?]) boolean?]{
  Returns @racket[#t] if the contract @racket[x] accepts either fewer
  or the same number of values as @racket[y] does.

  Contracts that are the same (i.e., where @racket[x] is @racket[equal?]
  to @racket[y]) are considered to always be stronger than each other.
  
  This function is conservative, so it may return @racket[#f] when
  @racket[x] does, in fact, accept fewer values.

@examples[#:eval (contract-eval) #:once
                 (contract-stronger? integer? integer?)
                 (contract-stronger? (between/c 25 75) (between/c 0 100))
                 (contract-stronger? (between/c 0 100) (between/c 25 75))
                 (contract-stronger? (between/c -10 0) (between/c 0 10))

                 (contract-stronger? (λ (x) (and (real? x) (<= x 0)))
                                     (λ (x) (and (real? x) (<= x 100))))]


}

@defproc[(contract-first-order-passes? [contract contract?]
                                       [v any/c])
         boolean?]{

Returns a boolean indicating whether the first-order tests
of @racket[contract] pass for @racket[v].

If it returns @racket[#f], the contract is guaranteed not to
hold for that value; if it returns @racket[#t], the contract
may or may not hold. If the contract is a first-order
contract, a result of @racket[#t] guarantees that the
contract holds.

See also @racket[contract-first-order-okay-to-give-up?] and
@racket[contract-first-order-try-less-hard].
}

@defproc[(contract-first-order [c contract?]) (-> any/c boolean?)]{
Produces the first-order test used by @racket[or/c] to match values to
higher-order contracts.
}

@section[#:tag "contract-utilities"]{Contract Utilities}

@declare-exporting-ctc[racket/contract/base]

@defproc[(contract? [v any/c]) boolean?]{

Returns @racket[#t] if its argument is a contract (i.e., constructed
with one of the combinators described in this section or a value that
can be used as a contract) and @racket[#f] otherwise.}

@defproc[(chaperone-contract? [v any/c]) boolean?]{

Returns @racket[#t] if its argument is a contract that guarantees that
it returns a value which passes @racket[chaperone-of?] when compared to
the original, uncontracted value.}

@defproc[(impersonator-contract? [v any/c]) boolean?]{

Returns @racket[#t] if its argument is a contract that is not a chaperone
contract nor a flat contract.}

@defproc[(flat-contract? [v any/c]) boolean?]{

Returns @racket[#t] when its argument is a contract that can be
checked immediately (unlike, say, a function contract).

For example,
@racket[flat-contract] constructs flat contracts from predicates, and
symbols, booleans, numbers, and other ordinary Racket values
(that are defined as @tech{contracts}) are also
flat contracts.}

@defproc[(list-contract? [v any/c]) boolean?]{
  Recognizes certain @racket[contract?] values that accept @racket[list?]s.
  
  A list contract is one that insists that its argument
  is a @racket[list?], meaning that the value cannot be cyclic
  and must either be the empty list or a pair constructed
  with @racket[cons] and another list.
  
  @history[#:added "6.0.1.13"]
}

@defproc[(contract-name [c contract?]) any/c]{
Produces the name used to describe the contract in error messages.
}

@defproc[(value-contract [v has-contract?]) (or/c contract? #f)]{
  Returns the contract attached to @racket[v], if recorded.
  Otherwise it returns @racket[#f].

  To support @racket[value-contract] and @racket[value-contract]
  in your own contract combinators, use @racket[prop:contracted] or
  @racket[impersonator-prop:contracted].
}

@defproc[(has-contract? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a value that
  has a recorded contract attached to it.
}

@defproc[(value-blame [v has-blame?]) (or/c blame? #f)]{
  Returns the @|blame-object| for the contract attached
  to @racket[v], if recorded. Otherwise it returns @racket[#f].
  
  To support @racket[value-contract] and @racket[value-blame]
  in your own contract combinators, use @racket[prop:blame] or
  @racket[impersonator-prop:blame].
  
  @history[#:added "6.0.1.12"]
}

@defproc[(has-blame? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a value that
  has a contract with blame information attached to it.

  @history[#:added "6.0.1.12"]
}

@defproc[(contract-late-neg-projection [c contract?]) (-> blame? (-> any/c (or/c #f any/c) any/c))]{
  Produces the projection defining a contract's behavior.

  The first argument, @racket[blame?] object encapsulates information about
  the contract checking, mostly used to create a meaningful error message if
  a contract violation is detected. The resulting function's first argument
  is the value that should have the contract and its second argument is
  a ``missing party'' from the blame object, to be passed to @racket[raise-contract-error].

  If possible, use this function instead of @racket[contract-val-first-projection] or
  @racket[contract-projection].
}


@defproc[(contract-projection [c contract?]) (-> blame? (-> any/c any/c))]{
  Produces the projection defining a contract's behavior. See also
  @racket[contract-late-neg-projection].
}

@defproc[(contract-val-first-projection [c contract?]) (-> blame? (-> any/c (-> any/c any/c)))]{
  Produces the projection defining a contract's behavior.
  See also @racket[contract-late-neg-projection].
}

@defproc[(make-none/c [sexp-name any/c]) contract?]{

Makes a contract that accepts no values, and reports the
name @racket[sexp-name] when signaling a contract violation.}

@defform*[[(recursive-contract contract-expr)
           (recursive-contract contract-expr #:list-contract?)
           (recursive-contract contract-expr type)
           (recursive-contract contract-expr type #:list-contract?)]]{

Delays the evaluation of its argument until the contract is checked,
making recursive contracts possible.  If @racket[type] is given, it
describes the expected type of contract and must be one of the keywords
@racket[#:impersonator], @racket[#:chaperone], or @racket[#:flat].  If
@racket[type] is not given, an impersonator contract is created.

If @racket[#:list-contract?] is returned, then the result is a
@racket[list-contract?] and the @racket[contract-expr] must evaluate
to a @racket[list-contract?].

@history[#:changed "6.0.1.13" @list{Added the @racket[#:list-contract?] argument.}]
}


@defform/subs[(opt/c contract-expr maybe-name)
              ([maybe-name (code:line)
                           (code:line #:error-name id)])]{

This optimizes its argument contract expression by
traversing its syntax and, for known contract combinators,
fuses them into a single contract combinator that avoids as
much allocation overhead as possible. The result is a
contract that should behave identically to its argument,
except faster.

If the @racket[#:error-name] argument is present, and
@racket[contract-expr] evaluates to a non-contract
expression, then @racket[opt/c] raises an error using
@racket[id] as the name of the primitive, instead of using
the name @racket[opt/c].

@examples[#:eval (contract-eval) #:once
                 (eval:error
                  (define/contract (f x)
                    (opt/c '(not-a-contract))
                    x))
                 (eval:error
                  (define/contract (f x)
                    (opt/c '(not-a-contract) #:error-name define/contract)
                    x))]
}


@defform[(define-opt/c (id id ...) expr)]{

This defines a recursive contract and simultaneously
optimizes it. Semantically, it behaves just as if
the @racket[-opt/c] were not present, defining a function on
contracts (except that the body expression must return a
contract). But, it also optimizes that contract definition,
avoiding extra allocation, much like @racket[opt/c] does.

For example,

@racketblock[
(define-contract-struct bt (val left right))

(define-opt/c (bst-between/c lo hi)
  (or/c null?
        (bt/c [val (real-in lo hi)]
              [left (val) (bst-between/c lo val)]
              [right (val) (bst-between/c val hi)])))

(define bst/c (bst-between/c -inf.0 +inf.0))
]

defines the @racket[bst/c] contract that checks the binary
search tree invariant. Removing the @racket[-opt/c] also
makes a binary search tree contract, but one that is
(approximately) 20 times slower.}

@defthing[contract-continuation-mark-key continuation-mark-key?]{
Key used by continuation marks that are present during contract checking.
The value of these marks are the @|blame-objects| that correspond to the contract
currently being checked.
}

@defproc[(contract-custom-write-property-proc [c contract?] 
                                              [p output-port?]
                                              [mode (or/c #f #t 0 1)])
         void?]{
  Prints @racket[c] to @racket[p] using the contract's name.

  @history[#:added "6.1.1.5"]
}

@defproc[(rename-contract [contract contract?]
                          [name any/c])
         contract?]{
  Produces a contract that acts like @racket[contract] but with the name
  @racket[name].

  The resulting contract is a flat contract if @racket[contract] is a
  flat contract.

  @history[#:added "6.3"]
}

@defform[(contract-first-order-okay-to-give-up?)]{
 This form returns a boolean that controls the result
 of first-order contact checks. More specifically, if
 it returns @racket[#t], then a first-order check may
 return @racket[#t] even when the entire first-order
 checks have not happened. If it returns @racket[#f]
 then the first order checks must continue until a
 definitive answer is returned.

 This will only return @racket[#t] in the dynamic
 extent of @racket[or/c] or @racket[first-or/c]'s
 checking to determine which branch to use.
 
 @history[#:added "6.3.0.9"]
}
@defform[(contract-first-order-try-less-hard e)]{
 Encourages first-order checks that happen in the
 dynamic-extent of @racket[e] to be more likely to
 give up. That is, makes it more likely that
 @racket[contract-first-order-okay-to-give-up?] might
 return @racket[#t].

 If not in the dynamic-extent of @racket[or/c]'s or
 @racket[first-or/c]'s checking to determine the branch,
 then this form has no effect.

 @history[#:added "6.3.0.9"]
}

@defproc[(if/c [predicate (-> any/c any/c)]
               [then-contract contract?]
               [else-contract contract?])
         contract?]{
  Produces a contract that, when applied to a value, first tests the
  value with @racket[predicate]; if @racket[predicate] returns true, the
  @racket[then-contract] is applied; otherwise, the
  @racket[else-contract] is applied. The resulting contract is a flat
  contract if both @racket[then-contract] and @racket[else-contract] are
  flat contracts.

  For example, the following contract enforces that if a value is a
  procedure, it is a thunk; otherwise it can be any (non-procedure)
  value:
    @racketblock[(if/c procedure? (-> any) any/c)]
  Note that the following contract is @bold{not} equivalent:
    @racketblock[(or/c (-> any) any/c) (code:comment "wrong!")]
  The last contract is the same as @racket[any/c] because
  @racket[or/c] tries flat contracts before higher-order contracts.

  @history[#:added "6.3"]
}

@defthing[failure-result/c contract?]{
  A contract that describes the failure result arguments of procedures
  such as @racket[hash-ref].

  Equivalent to @racket[(if/c procedure? (-> any) any/c)].

  @history[#:added "6.3"]
}




@section{@racketmodname[racket/contract/base]}

@defmodule[racket/contract/base]

The @racketmodname[racket/contract/base] module provides a subset
of the exports of @racketmodname[racket/contract] module. In
particular, it contains everything in the
@itemize[@item{@secref["data-structure-contracts"]}
         @item{@secref["function-contracts"]}
         @item{@secref["attaching-contracts-to-values"] and}
         @item{@secref["contract-utilities"] sections.}]

Unfortunately, using @racketmodname[racket/contract/base] does not
yield a significantly smaller memory footprint than
@racketmodname[racket/contract], but it can still be useful to
add contracts to libraries that @racketmodname[racket/contract]
uses to implement some of the more sophisticated
parts of the contract system.

@section{Legacy Contracts}

@defproc[(make-proj-contract [name any/c]
                             [proj
                              (or/c (-> any/c
                                        any/c
                                        (list/c any/c any/c)
                                        contact?
                                        (-> any/c any/c))
                                    (-> any/c
                                        any/c
                                        (list/c any/c any/c)
                                        contact?
                                        boolean?
                                        (-> any/c any/c)))]
                             [first-order (-> any/c boolean?)])
         contract?]{
  Builds a contract using an old interface.

  Modulo errors, it is equivalent to:
  @racketblock[(make-contract
                #:name name
                #:first-order first-order
                #:projection
                (cond
                  [(procedure-arity-includes? proj 5)
                   (lambda (blame)
                     (proj (blame-positive blame)
                           (blame-negative blame)
                           (list (blame-source blame) (blame-value blame))
                           (blame-contract blame)
                           (not (blame-swapped? blame))))]
                  [(procedure-arity-includes? proj 4)
                   (lambda (blame)
                     (proj (blame-positive blame)
                           (blame-negative blame)
                           (list (blame-source blame) (blame-value blame))
                           (blame-contract blame)))]))]
}

@defproc[(raise-contract-error [val any/c] [src any/c]
                               [pos any/c] [name any/c]
                               [fmt string?] [arg any/c] ...)
         any/c]{
  Calls @racket[raise-blame-error] after building a @racket[blame] struct from
  the @racket[val], @racket[src], @racket[pos], and @racket[name] arguments.
  The @racket[fmt] string and following arguments are passed to
  @racket[format] and used as the string in the error message.
}

@defproc[(contract-proc [c contract?])
         (->* (symbol? symbol? (or/c syntax? (list/c any/c any/c)))
              (boolean?)
              (-> any/c any))]{
  Constructs an old-style projection from a contract.

  The resulting function accepts the information that is in a @racket[blame]
  struct and returns a projection function that checks the contract.

}
@section{Random generation}

@defproc[(contract-random-generate [ctc contract?]
                                   [fuel 5 exact-nonnegative-integer?]
                                   [fail (or/c #f (-> any) (-> boolean? any)) #f])
         any/c]{
Attempts to randomly generate a value which will match the contract. The fuel
argument limits how hard the generator tries to generate a value matching the
contract and is a rough limit of the size of the resulting value.

The generator may fail to generate a value, either because some contracts
do not have corresponding generators (for example, not all predicates have
generators) or because there is not enough fuel. In either case, the
function @racket[fail] is invoked. If @racket[fail] accepts an argument,
it is called with @racket[#t] when there is no generator for @racket[ctc]
and called with @racket[#f] when there is a generator, but the generator
ended up returning @racket[contract-random-generate-fail].

@history[#:changed "6.1.1.5" @list{Allow @racket[fail] to accept a boolean.}]

}

@defproc[(contract-exercise [val any/c] ...+) void?]{
  Attempts to get the @racket[val]s to break their contracts (if any).
                  
  Uses @racket[value-contract] to determine if any of the @racket[val]s have a
  contract and, for those that do, uses information about the contract's shape
  to poke and prod at the value. For example, if the value is function, it will
  use the contract to tell it what arguments to supply to the value. 
}

@defproc[(contract-random-generate/choose [c contract?] [fuel exact-nonnegative-integer?])
         (or/c #f (-> c))]{
  This function is like @racket[contract-random-generate], but it is intended to
  be used with combinators that generate values based on sub-contracts
  they have. It cannot be called, except during contract
  generation. It will never fail, but it might escape back to an enclosing
  call or to the original call to @racket[contract-random-generate].
 
  It chooses one of several possible generation strategies, and thus it may not
  actually use the generator associated with @racket[c], but might instead
  use a stashed value that matches @racket[c] that it knows about via
  @racket[contract-random-generate-stash].

@history[#:added "6.1.1.5"]
}

@defthing[contract-random-generate-fail contract-random-generate-fail?]{
  An atomic value that is used to indicate that a generator
  failed to generate a value.

@history[#:added "6.1.1.5"]
}

@defproc[(contract-random-generate-fail? [v any/c]) boolean?]{
  A predicate to recognize @racket[contract-random-generate-fail].

@history[#:added "6.1.1.5"]
}

@defproc[(contract-random-generate-env? [v any/c]) boolean?]{
  Recognizes contract generation environments.

@history[#:added "6.1.1.5"]
}

@defproc[(contract-random-generate-stash [env contract-random-generate-env?]
                                         [c contract?]
                                         [v c]) void?]{
  This should be called with values that the program under
  test supplies during contract generation. For example, when
  @racket[(-> (-> integer? integer?) integer?)] is generated,
  it may call its argument function. That argument function may
  return an integer and, if so, that integer should be saved by
  calling @racket[contract-random-generate-stash], so it can
  be used by other integer generators.

@history[#:added "6.1.1.5"]
}

@defproc[(contract-random-generate-get-current-environment) contract-random-generate-env?]{
  Returns the environment currently being for generation. This function
  can be called only during the dynamic extent of contract generation.
  It is intended to be grabbed during the construction of a contract
  generator and then used with @racket[contract-random-generate-stash]
  while generation is happening.

@history[#:added "6.1.1.5"]
}
