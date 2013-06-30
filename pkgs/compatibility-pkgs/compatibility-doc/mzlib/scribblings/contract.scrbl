#lang scribble/doc
@(require "common.rkt"
          scribble/struct
          "contract-label.rkt"
          (for-label mzlib/contract)
          (for-label (prefix-in r: racket/contract)))

@(define-syntax-rule (twocolumns id ...)
   (*twocolumns (list (racket id) ...)))
@(define (*twocolumns uneven-l)
   (let* ([l (if (zero? (modulo (length uneven-l) 2)) uneven-l (append uneven-l (list #f)))]
          [len (length l)]
          [half (quotient len 2)]
          [a (for/list ([i (in-range half)]
                        [e l])
               e)]
          [b (list-tail l half)]
          [spacer (hspace 2)]
          [to-flow (compose make-flow list make-paragraph list)])
     (make-table #f
                 (map (lambda (a b)
                        (list (to-flow spacer)
                              (to-flow a)
                              (to-flow spacer)
                              (to-flow (or b ""))))
                      a b))))

@mzlib[#:mode title contract]

@deprecated[@racketmodname[racket/contract]]{
  This library is designed as a backwards compatible library
  for old uses of contracts. It should not be used for new
  libraries.
}

The main differences: the function contract syntax is more
regular and function contracts now support keywords, and
@tt{union} is now @racket[or/c].

The @racketmodname[mzlib/contract] library re-exports many bindings
from @racketmodname[racket/contract]:

@twocolumns[
 </c
 <=/c
 =/c
 >/c
 >=/c
 and/c
 any
 any/c
 between/c
 box-immutable/c
 build-compound-type-name
 coerce-contract
 cons/c
 contract
 contract-first-order-passes?
 contract-violation->string
 contract?
 define-contract-struct
 false/c
 flat-contract
 flat-contract-predicate
 flat-contract?
 flat-murec-contract
 flat-named-contract
 flat-rec-contract
 guilty-party
 integer-in
 list/c
 listof
 make-none/c
 make-proj-contract
 natural-number/c
 none/c
 not/c
 one-of/c
 or/c
 parameter/c
 printable/c
 promise/c
 provide/contract
 raise-contract-error
 real-in
 recursive-contract
 string/len
 symbols
 syntax/c
 vector-immutable/c
 vector-immutableof]

It also provides the old version of the following contracts:

@defform[(define/contract id contract-expr init-value-expr)]{

Attaches the contract @racket[contract-expr] to
@racket[init-value-expr] and binds that to @racket[id].

The @racket[define/contract] form treats individual definitions as
units of blame. The definition itself is responsible for positive
(co-variant) positions of the contract and each reference to
@racket[id] (including those in the initial value expression) must
meet the negative positions of the contract.

Error messages with @racket[define/contract] are not as clear as those
provided by @racket[provide/contract], because
@racket[define/contract] cannot detect the name of the definition
where the reference to the defined variable occurs. Instead, it uses
the source location of the reference to the variable as the name of
that definition.}

@defproc[(box/c [c flat-contract?]) flat-contract?]{

Returns a flat contract that recognizes boxes. The content of the box
must match @racket[c].}

@defproc[(vectorof [c flat-contract?]) flat-contract?]{

Accepts a flat contract and returns a flat contract
that checks for vectors whose elements match the original contract.}

@defproc[(vector/c [c flat-contract?] ...) flat-contract?]{

Accepts any number of flat contracts and returns a
flat contract that recognizes vectors. The number of elements in the
vector must match the number of arguments supplied to
@racket[vector/c], and each element of the vector must match the
corresponding flat contract.}

@defform[(struct/c struct-id flat-contract-expr ...)]{

Produces a flat contract that recognizes instances of the structure
type named by @racket[struct-id], and whose field values match the
flat contracts produced by the @racket[flat-contract-expr]s.}

@defproc[(build-flat-contract [name symbol?] [predicate (-> any/c any)]) flat-contract?]{
  Builds a flat contract out of @racket[predicate], giving it the name
  @racket[name]. Nowadays, just using @racket[predicate] directly is preferred.
}

@defform*[((-> contract-dom-expr ... any)
           (-> contract-dom-expr ... contract-rng-expr))]{
This is a restricted form of @racketmodname[racket/contract]'s
                             @r:-> contract that does not
                             handle keyword arguments or multiple
                             value results.

}

@defform*/subs[((->* (contract-dom-expr ...) ->*rng)
                (->* (contract-dom-expr ...) contract-rest-expr ->*rng)) 
               ([->*rng (contract-rng-expr ...)
                        any])]{
  The @racket[->*] form matches up to
  @racketmodname[racket/contract]'s @r:-> and @r:->*, according
  to the following rules; each equation on the
  left refers to a @racketmodname[mzlib/contract] combinator;
  on the right are the @racketmodname[racket/contract] equivalents.
  @racketblock[(->* (contract-dom-expr ...) any) =
               (#,r:-> contract-dom-expr ... any)]
  @racketblock[(->* (contract-dom-expr ...) (contract-rng-expr ...)) =
               (#,r:-> contract-dom-expr ... (values contract-rng-expr))]
  @racketblock[(->* (contract-expr ...) contract-rest-expr any) =
               (#,r:->* (contract-expr ...) #:rest contract-rest-expr any)]
  @racketblock[(->* (contract-expr ...) contract-rest-expr (contract-rng-expr ...)) =
               (#,r:->* (contract-expr ...)
                        #:rest contract-rest-expr 
                        (values contract-rng-expr ...))]

}

@defform*[((opt-> (contract-req-expr ...) (contact-opt-expr ...) any)
           (opt-> (contract-req-expr ...) (contact-opt-expr ...) contract-rng-expr))]{

  The @racket[opt->] form is a simplified verison of @racketmodname[racket/contract]'s
      @|r:->*| and appearances of @racket[opt->] can be simply replaced with @|r:->*|.

}

@defform*[((opt->* (contract-req-expr ...) (contact-opt-expr ...) any)
           (opt->* (contract-req-expr ...) (contact-opt-expr ...) (contract-rng-expr ...)))]{

  The @racket[opt->*] form   matches up to
      @racketmodname[racket/contract]'s @r:->*, according
  to the following rules; each equation on the
  left refers to a @racketmodname[mzlib/contract] combinator;
  on the right are the @racketmodname[racket/contract] equivalents.

  @racketblock[(opt->* (contract-req-expr ...) (contract-opt-expr ...) any) =
               (#,r:->* (contract-req-expr ...) (contract-opt-expr ...) any)]

  @racketblock[(opt->* (contract-req-expr ...)
                       (contract-opt-expr ...)
                       (contract-rng-expr ...)) =
               (#,r:->* (contract-req-expr ...)
                        (contract-opt-expr ...)
                        (values contract-rng-expr ...))]
}

@defform[(->d contract-dom-expr ... contract-rng-fun-expr)]{
  The @racket[->d] contract constructor is just like @racket[->],
  except that the range position is expected to be a function
  that accepts the actual arguments passed to the function,
  and returns a contract for the range. For example, this
  is one contract for @racket[sqrt]:
  @racketblock[(->d real?
                    (λ (in)
                      (and/c real?
                             (λ (out)
                               (< (abs (- (sqr out) in))
                                  0.01)))))]
  It says that the input must be a real number, and so must the
  result, and that the square of the result is within
  @racket[0.01] of input.

}

@defform*[((->d* (contract-dom-expr ...) contract-rng-fun-expr)
           (->d* (contract-dom-expr ...) contract-rest-expr contract-rng-fun-expr))]{
  The @racket[->d*] contract constructor is a generalization of 
      @racket[->d] to support multiple values and rest arguments.
      
      In the two sub-expression case, the first sequence of contracts
      are contracts on the domain of the function and the second
      subexpression is expected to evaluate to a function that accepts
      as many arguments as there are expressions in the first position.
      It should return multiple values: one contract for each result
      of the function.
      
      In the three sub-expression case, the first and last subexpressions
      are just like the sub-expressions in the two sub-expression case;
      the middle sub-expression si expected to evaluate to a contract on
      the rest argument.
      
}

@defform*/subs[((->r ([dom-x contract-dom-expr] ...) rng)
                (->r ([dom-x contract-dom-expr] ...) rest-x contract-rest-expr rng))
               ((rng any
                     (values contract-expr ...)
                     contract-expr))]{

  The @racket[->r] form is a simplified version of @racketmodname[racket/contract]'s @|r:->i|, where
  each @racket[contract-dom-expr] is parameterized over all of the @racket[dom-x] variables
  (and does lax checking; see @r:->d for details).

}

@defform*[((->pp ([dom-x contract-dom-expr] ...) pre-cond-expr any)
           (->pp ([dom-x contract-dom-expr] ...)
                 pre-cond-expr
                 (values [rng-x contract-rng-expr] ...)
                 post-cond-expr)
           (->pp ([dom-x contract-dom-expr] ...)
                 pre-cond-expr
                 contract-rng-expr
                 rng-x
                 post-cond-expr))]{

  The @racket[->pp] form, like @racket[->r] is a simplified version of @racketmodname[racket/contract]'s @|r:->i|, where
  each @racket[contract-dom-expr] is parameterized over all of the @racket[dom-x] variables
  (and does lax checking; see @racketmodname[racket/contract]'s @r:->d for details). Unlike @racket[->r], it also has pre- and post-condition
  expressions; these expressions are also implicitly parameterized over all of the @racket[dom-x]
  variables and the post-condition is also paramterized over @racket[rng-x], which is bound to the result
  of the function.

}

@defform*[((->pp-rest ([dom-x contract-dom-expr] ...) rest-x rest-contract-expr pre-cond-expr any)
           (->pp-rest ([dom-x contract-dom-expr] ...)
                      rest-x rest-contract-expr
                      pre-cond-expr
                      (values [rng-x contract-rng-expr] ...)
                      post-cond-expr)
           (->pp-rest ([dom-x contract-dom-expr] ...)
                      rest-x rest-contract-expr
                      pre-cond-expr
                      contract-rng-expr
                      rng-x
                      post-cond-expr))]{
  Like @racket[->pp], but with an additional contract for the rest arguments of the function.
}

@defform[(case-> mzlib/contract-arrow-contract-expr ...)]{
   Builds a contract analogous to @racket[case-lambda],
   where each case comes from one of the contract expression arguments
   (tried in order).
}

@defform[(object-contract [id mzlib/contract-arrow-contract-expr] ...)]{
   Builds a contract for objects where each @racket[id] is expected to be 
   a method on the object living up to the corresponding contract
}
