#lang scribble/doc
@(require "common.ss"
          scribble/struct
          (for-label mzlib/contract))

@(define-syntax-rule (twocolumns id ...)
   (*twocolumns (list (scheme id) ...)))
@(define (*twocolumns l)
   (let* ([len (length l)]
          [l (if (odd? len) (append l (list #f)) l)]
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
                        (append (list (to-flow spacer)
                                      (to-flow a))
                                (if b
                                    (list (to-flow spacer)
                                          (to-flow  b))
                                    null)))
                      a b))))

@mzlib[#:mode title contract]

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
 box/c
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
 flat-contract/predicate?
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
 struct/c
 symbols
 syntax/c
 vector-immutable/c
 vector-immutableof
 vector/c
 vectorof]

It also provides the old version of @scheme[define/contract]:

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
