#lang scribble/doc
@(require "common.rkt"
          scribble/struct
          (for-label mzlib/contract))

@(define-syntax-rule (twocolumns id ...)
   (*twocolumns (list (scheme id) ...)))
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

This library is designed as a backwards compatible library
for old uses of contracts. It should not be used for new
libraries; use @schememodname[scheme/contract] instead.

The main differences: the function contract syntax is more
regular and function contracts now support keywords, and
@tt{union} is now @scheme[or/c].

The @schememodname[mzlib/contract] library re-exports many bindings
from @schememodname[scheme/contract]:

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

It also provides the old version of the following forms:

@defform[(define/contract id contract-expr init-value-expr)]{

Attaches the contract @scheme[contract-expr] to
@scheme[init-value-expr] and binds that to @scheme[id].

The @scheme[define/contract] form treats individual definitions as
units of blame. The definition itself is responsible for positive
(co-variant) positions of the contract and each reference to
@scheme[id] (including those in the initial value expression) must
meet the negative positions of the contract.

Error messages with @scheme[define/contract] are not as clear as those
provided by @scheme[provide/contract], because
@scheme[define/contract] cannot detect the name of the definition
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
