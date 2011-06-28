#lang racket/base
(require "nfa.rkt"
         (for-syntax syntax/parse
                     racket/syntax
                     unstable/syntax
                     syntax/id-table
                     racket/dict
                     racket/list
                     racket/base))

(define-syntax (epsilon stx) (raise-syntax-error 'epsilon "Outside nfa/ep" stx))
(define-syntax (nfa/ep stx)
  (syntax-parse
   stx
   #:literals (epsilon)
   [(_ (start:id ...)
       (end:id ...)
       [state:id ([epsilon (epsilon-state:id ...)]
                  ...
                  [evt:expr (next-state:id ...)]
                  ...)]
       ...)
    (define state->epsilon (make-bound-id-table))
    (for ([stx (in-list (syntax->list #'([state epsilon-state ... ...] ...)))])
      (syntax-case stx ()
        [[state . es]
         (bound-id-table-set! state->epsilon #'state (syntax->list #'es))]))
    (define seen? (make-parameter (make-immutable-bound-id-table)))
    (define (state->epsilons state)
      (if (dict-has-key? (seen?) state)
          empty
          (parameterize ([seen? (bound-id-table-set (seen?) state #t)])
            (define es (bound-id-table-ref state->epsilon state empty))
            (list* state (append-map state->epsilons es)))))
    (with-syntax*
        ([((start* ...) ...)
          (syntax-map state->epsilons #'(start ...))]
         [((((next-state* ...) ...) ...) ...)
          (syntax-map (λ (ns*)
                        (syntax-map (λ (ns)
                                      (syntax-map state->epsilons ns))
                                    ns*))
                      #'(((next-state ...) ...) ...))])
      (syntax/loc stx
        (nfa (start* ... ...)
             (end ...)
             [state ([evt (next-state* ... ...)]
                     ...)]
             ...)))]))

(provide epsilon
         nfa/ep)
