#lang racket/base

(require "machine.rkt"
         racket/local
         racket/match
         (for-syntax racket/base
                     syntax/parse
                     syntax/id-table
                     racket/dict
                     unstable/sequence))

(define-syntax (dfa stx)
  (syntax-parse
   stx
   [(_ start:id
       (end:id ...)
       [state:id ([evt:expr next-state:id]
                  ...)]
       ...)
    
    (define end? (make-bound-id-table))
    (for ([e (in-syntax #'(end ...))])
      (dict-set! end? e #t))
    
    (with-syntax
        ([(state-constructor ...)
          (for/list ([st (in-syntax #'(state ...))])
            (if (dict-ref end? st #f)
                #'machine-accepting
                #'machine))])
      (syntax/loc stx
        (local
          [; state : input -> next-state
           (define state
             (state-constructor
              '(dfa [evt next-state]
                    ...)
              (match-lambda
                [evt next-state]
                ...)))
           ...]
          start)))]))

(provide dfa)
