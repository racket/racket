#lang racket/base

(require (submod "custom-hash.rkt" test)
         (submod "alist.rkt" test)
         (submod "from-docs.rkt" test)
         (submod "stream.rkt" test)
         (submod "iterator.rkt" test)
         (submod "struct-form.rkt" test)
         (submod "equal+hash.rkt" test)
         (submod "custom-write.rkt" test)
         (submod "defaults.rkt" test)
         "syntax-errors.rkt"
         "base-interfaces.rkt"
         "contract.rkt"
         "from-unstable.rkt"
         "poly-contracts.rkt"
         "empty-interface.rkt")
