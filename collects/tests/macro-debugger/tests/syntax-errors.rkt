#lang racket/base
(require "../gentest-framework.rkt")
(provide proto:errors)

(define-tests proto:errors "Bad syntax"
  [#:suite
   "Atomic expressions"
   (testKE (#%top a b c)
           #:error-step)
   (testKE (#%top . 5)
           #:error-step)
   (testKE (quote)
           #:error-step)
   (testKE (quote a b)
           #:error-step)
   (testKE (#%require . x)
           #:error-step)
   (testKE (#%require 5)
           #:error-step)
   (testKE (#%require (prefix mzlib/list))
           #:error-step)
   (testKE (#%require (prefix 5 mzlib/list))
           #:error-step)]
  [#:suite
   "Definitions"
   (testKE (define-values x 'a)
           #:error-step)
   (testKE (define-values (x))
           #:error-step)
   (testKE (define-values (x) 'a 'b)
           #:error-step)
   (testKE (define-values (x) . 1)
           #:error-step)
   (testKE (define-values (x x) 1)
           #:error-step)
   (testKE (define-syntaxes x 1)
           #:error-step)
   (testKE (define-syntaxes (x))
           #:error-step)
   (testKE (define-syntaxes (x) 1 2)
           #:error-step)
   (testKE (define-syntaxes (x) . 3)
           #:error-step)
   (testKE (define-syntaxes (x x) 1)
           #:error-step)]
  
  ;; "Simple expressions"
  [#:suite
   "if misapplied"
   (testKE (if)
           #:error-step)
   (testKE (if 1)
           #:error-step)
   (testKE (if 'a 'b)
           #:error-step)
   (testKE (if 1 2 3 4)
           #:error-step)
   (testKE (if . x)
           #:error-step)
   (testKE (if 1 . x)
           #:error-step)
   (testKE (if 1 2 . x)
           #:error-step)
   (testKE (if 1 2 3 . x)
           #:error-step)]
  [#:suite
   "wcm misapplied"
   (testKE (with-continuation-mark)
           #:error-step)
   (testKE (with-continuation-mark 1)
           #:error-step)
   (testKE (with-continuation-mark 1 2 3 4)
           #:error-step)
   (testKE (with-continuation-mark . x)
           #:error-step)
   (testKE (with-continuation-mark 1 . x)
           #:error-step)
   (testKE (with-continuation-mark 1 2 . x)
           #:error-step)
   (testKE (with-continuation-mark 1 2 3 . x)
           #:error-step)]
  [#:suite
   "set! misapplied"
   (testKE (set!)
           #:error-step)
   (testKE (set! x)
           #:error-step)
   (testKE (set! x . 3)
           #:error-step)
   (testKE (set! x 1 2)
           #:error-step)
   (testKE (set! 1)
           #:error-step)
   (testKE (set! 1 2)
           #:error-step)]
  
  ;; "Sequence-containing expressions"
  [#:suite
   "begin misapplied"
   (testKE (#%expression (begin))
           #:error-step)
   (testKE (begin . 1)
           #:error-step)
   (testKE (begin 'a . 2)
           #:error-step)]
  [#:suite
   "begin0 misapplied"
   (testKE (begin0)
           #:error-step)
   (testKE (begin0 . 1)
           #:error-step)
   (testKE (begin0 'a . 2)
           #:error-step)
   (testKE (begin0 'a 'b . 3)
           #:error-step)]
  [#:suite 
   "#%app (implicit) misapplied"
   (testKE (+ . 1)
           [#:steps (tag-app (#%app + . 1))
                    error])
   (testKE (+ 1 . 2)
           [#:steps (tag-app (#%app + 1 . 2))
                    error])
   (testKE (+ 1 2 . 3)
           [#:steps (tag-app (#%app + 1 2 . 3))
                    error])]
  [#:suite
   "#%app (explicit) misapplied"
   (testKE (#%app . +)
           #:error-step)
   (testKE (#%app + . 1)
           #:error-step)
   (testKE (#%app + 1 . 2)
           #:error-step)
   (testKE (#%app + 1 2 . 3)
           #:error-step)]
  
  ;; "Binding forms"
  [#:suite
   "lambda misapplied"
   (testKE (lambda)
           #:error-step)
   (testKE (lambda args)
           #:error-step)
   (testKE (lambda #(a b) 1)
           #:error-step)
   (testKE (lambda args . 1)
           #:error-step)
   (testKE (lambda 1 2)
           #:error-step)
   (testKE (lambda (1) 2)
           #:error-step)
   (testKE (lambda (x . 1) 2)
           #:error-step)
   (testKE (lambda (x x) 1)
           #:error-step)
   (testKE (lambda (x y x) 1)
           #:error-step)]
  [#:suite 
   "letrec-values misapplied"
   (testKE (letrec-values)
           #:error-step)
   (testKE (letrec-values x)
           #:error-step)
   (testKE (letrec-values x 1)
           #:error-step)
   (testKE (letrec-values (x) 2)
           #:error-step)
   (testKE (letrec-values (x 1) 2)
           #:error-step)
   (testKE (letrec-values ([x 1]) 2)
           #:error-step)
   (testKE (letrec-values ([(x . y) 1]) 2)
           #:error-step)
   (testKE (letrec-values ([(x) 1 2]) 2)
           #:error-step)
   (testKE (letrec-values ([(x) 1] x) 2)
           #:error-step)
   (testKE (letrec-values ([(x) 1] [y 2]) 3)
           #:error-step)
   (testKE (letrec-values ([(x x) 1]) 2)
           #:error-step)
   (testKE (letrec-values ([(x) 1] [(x) 2]) 3)
           #:error-step)]

  [#:suite
   "Internal definitions"
   [#:suite
    "Basic internal definitions"
    (testKE (lambda () . 1) ;; FIXME
            #:error-step)
    (testKE (lambda () (begin))
            [#:steps (rename-lambda (lambda () (begin)))
                     (splice-block (lambda ()))
                     error])
    (testKE (lambda () (define-values (x) 1))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x) 1) . 2)
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (begin (define-values (x) 1) . 2))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (begin (define-values (x) 1) . 2) 3)
            [#:rename+error-step rename-lambda])
    (testKE (lambda ()
              (define-values (x) 1)
              (define-values (x) 2)
              3)
            [#:rename+error-step rename-lambda])]
   [#:suite
    "#%stratified-body"
    (testKE (#%stratified-body
             (define-values (x) 'a)
             'b
             (define-values (y) 'c)
             'd)
            [#:steps (block->letrec (#%stratified-body
                                     (letrec-values ([(x) 'a])
                                       (#%stratified-body
                                        'b
                                        (define-values (y) 'c)
                                        'd))))
                     (rename-letrec-values (#%stratified-body
                                            (letrec-values ([(x) 'a])
                                              (#%stratified-body
                                               'b
                                               (define-values (y) 'c)
                                               'd))))
                     error])
    (testKE (#%stratified-body (define-values (x) 'a))
            [#:steps error])]
   [#:suite
    "bad internal begin"
    (testKE (lambda () (begin . 1))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (begin 1 . 2))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x) 1) (begin . 2))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x) 1) (begin 1 . 2))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x) 1) (begin . 2) 3)
            [#:rename+error-step rename-lambda])]
   [#:suite
    "bad definition forms"
    (testKE (lambda () (define-values))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values x))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values x 1))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x . y) 1))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x) . 1))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x) 1 2))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values (x x) 1))
            [#:rename+error-step rename-lambda])]]
  [#:suite
   "Errors in primitive contexts"
   [#:suite 
    "Definitions"
    (testKE (define-syntaxes (x) (lambda))
            #:error-step)
    (testKE (define-values (x) (wrong))
            #:error-step)]
   [#:suite
    "Simple expressions"
    (testKE (if (wrong) 'b 'c)
            #:error-step)
    (testKE (if 'a (wrong) 'c)
            #:error-step)
    (testKE (if 'a 'b (wrong))
            #:error-step)
    (testKE (if (wrong) 'b)
            #:error-step)
    (testKE (if 'a (wrong))
            #:error-step)
    (testKE (with-continuation-mark (wrong) 'b 'c)
            #:error-step)
    (testKE (with-continuation-mark 'a (wrong) 'c)
            #:error-step)
    (testKE (with-continuation-mark 'a 'b (wrong))
            #:error-step)
    (testKE (set! x (wrong))
            #:error-step)]
   [#:suite
    "Sequence-containing expressions"
    (testKE (begin (wrong))
            #:error-step)
    (testKE (begin 'a (wrong))
            #:error-step)
    (testKE (begin0 (wrong))
            #:error-step)
    (testKE (begin0 'a (wrong))
            #:error-step)
    (testKE (#%app (wrong))
            #:error-step)
    (testKE (#%app + (wrong))
            #:error-step)]
   [#:suite
    "Binding forms"
    (testKE (lambda (x) (begin0 (wrong)))
            [#:rename+error-step rename-lambda])
    (testKE (letrec-values ([(x) (wrong)]) 1)
            [#:rename+error-step rename-letrec-values])
    (testKE (letrec-values ([(x) 'a]) (begin0 (wrong)))
            [#:rename+error-step rename-letrec-values])]
   [#:suite
    "Internal definitions"
    (testKE (lambda () (wrong))
            [#:rename+error-step rename-lambda])
    (testKE (lambda () (define-values () (wrong)) 1)
            [#:steps 
             (rename-lambda (lambda () (define-values () (wrong)) 1))
             (block->letrec (lambda () (letrec-values ([() (wrong)]) 1)))
             (rename-letrec-values (lambda () (letrec-values ([() (wrong)]) 1)))
             error])
    (testKE (lambda () (define-values (x) 1) (wrong))
            [#:rename+error-step rename-lambda])]])
