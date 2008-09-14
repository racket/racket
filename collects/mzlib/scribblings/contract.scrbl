#lang scribble/doc
@(require "common.ss"
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
 box/c
 build-compound-type-name
 coerce-contract
 cons/c
 contract
 contract-first-order-passes?
 contract-violation->string
 contract?
 define-contract-struct
 define/contract
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
 struct/c
 symbols
 syntax/c
 vector-immutable/c
 vector-immutableof
 vector/c
 vectorof]

