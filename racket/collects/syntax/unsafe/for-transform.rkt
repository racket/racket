#lang racket/base

(require (for-template
          (only-in racket/base
                   begin
                   void)
          (only-in racket/private/for
                   [expand-for-clause orig:expand-for-clause])))

(provide expand-for-clause
         expand-for-clause*)

(define (expand-for-clause orig-stx clause)
  (define new-stx (orig:expand-for-clause orig-stx clause))
  (syntax-case new-stx ()
    [(outer-bind
      outer-check
      loop-bind
      pos-guard
      inner-bind
      inner-check
      pre-guard
      post-guard
      loop-args)
     (if (let loop ([inner-check #'inner-check])
           (syntax-case inner-check (begin void)
             [(begin e ...) (for/and ([e (in-list (syntax->list #'(e ...)))])
                              (loop e))]
             [(void) #t]
             [_ #f]))
         #'(outer-bind
            outer-check
            loop-bind
            pos-guard
            inner-bind
            pre-guard
            post-guard
            loop-args)
         (raise-syntax-error 'expand-for-clause
                             "expansion includes unrecognized inner-check form"
                             orig-stx
                             #'inner-check))]))

(define (expand-for-clause* orig-stx clause)
  (orig:expand-for-clause orig-stx clause))
